#'Get distance
#'
#'This function is to calculate the distane between two coordinates based on how a crow flies.
#'@param lon1  longitude of point 1
#'@param lat1 latitude of point 1
#'@param lon2 longitude of point 2
#'@param lat2 latitude of point 2
#'@return the distance between two points. If lat1, lon1, lat2, and lon2 were supplied as a vector, it will return a vector of distances.

simple_distance<-function(lon1,lat1,lon2,lat2){
  elements<-length(lon1)
  distances<-geosphere::distHaversine(cbind(lon1, lat1), cbind(lon2, lat2))
  return(distances)}

#' Get fishing coordinates
#'
#' This function uses Spot Trace data to determine fishing coordinates, interpolate depth, and secondary bathymetric information.
#' @param df the ifish dataframe; must contain tables: ifish_tracker, ifish_boat, ifish_findmespot
#' @param depthraster the depth raster file, for example, GEBCO file
#' @return original data frame with fishing coordinates, depth, and secondary bathymetric information added.
#' @export


fishing_grounds<- function(df, depthraster){
depth<- as.data.frame(rasterToPoints(depthraster)) %>%
  dplyr::rename(depth=gebco) %>%
  ## notice that depth is -99999 for land, we need to change that
  mutate(depth=ifelse(depth==-99999,10000,depth))
# turn it spatial
coordinates(depth)<-c("x","y")
# at start they are just long-lat observations
proj4string(depth) <-CRS("+proj=longlat +datum=WGS84")

# now we need to interpolate (this finds depth at each ping)
#this takes quite a long time!
interpolated_depths<-akima::interpp(x=coordinates(depth)[,1],
                                    y=coordinates(depth)[,2],
                                    z = depth$depth,
                                    xo = df$longitude,
                                    yo = df$latitude,
                                    duplicate="strip"
)

#add to data frame
df$depth <- interpolated_depths$z
#change date time format
df$datetime<-lubridate::ymd(df$date_time)

### replace -99999 for lat and long with missing values
df <-
  df %>%
  setNames(make.unique(names(.))) %>%
  dplyr::rename(boatname = boat_name) %>%
  dplyr::filter(longitude !=-99999) %>%
  dplyr::filter(latitude !=-99999) %>%
  dplyr::arrange(boatname,datetime)

df <-
  df%>%
  ungroup() %>%
  #group by boat name, this makes sure that we compute difftime only when it's between the same boat
  dplyr::group_by(boatname) %>%
  dplyr::mutate(time_diff=difftime(`datetime`,
                                   lag(`datetime`,order_by=`datetime`),
                                   unit="hours"))
# ### this is the burst as defined by the original script, we use it only briefly
df <-
  df %>%
  ungroup() %>%
  #begin of burst is flagged here as either a completely new boat or a week has passed
  mutate(BeginOfBurst = ifelse(is.na(time_diff) | time_diff > 7*24*60*60 ,1,0))  %>%
  mutate(burst = cumsum(BeginOfBurst)) %>%
  dplyr::select(-BeginOfBurst)

df<-
  df %>% ungroup() %>%
  group_by(burst) %>%
  distinct(`datetime`,.keep_all = TRUE) %>%
  ungroup()

## let's try to build distances by hand without using new libraries
df<-
  df %>%
  group_by(burst) %>%
  # compute distance
  mutate(distance = simple_distance(longitude,latitude,lag(longitude),lag(latitude))) %>%
  # distance in terms of km!
  mutate(distance= distance/1000)

#now let's compute speed in km/h
df<-
  df %>%
  mutate(speed = distance/as.numeric(time_diff))

#tag bad observations:
df<-
  df %>%
  group_by(burst) %>%
  mutate(quality = ifelse(
    (speed < 30 | is.na(speed)) & (lag(speed)<30 | is.na(lag(speed))),1,0))

## simply drop all observations without lat and long
df<-
  df %>%
  filter(!is.na(longitude)) %>%
  filter(!is.na(latitude))

#Find other bathymetry properties based on the depthraster
slope <- terrain(depthraster, opt='slope', unit='degrees')
aspect <- terrain(depthraster, opt='aspect', unit='degrees')
BRI <- terrain(depthraster, opt='TRI', unit='degrees')
BPI <- terrain(depthraster, opt='TPI', unit='degrees')
roughness <- terrain(depthraster, opt='roughness', unit='degrees')

#Change all this to dataframe
slope <- as.data.frame(rasterToPoints(slope))
aspect <- as.data.frame(rasterToPoints(aspect))
BRI <- as.data.frame(rasterToPoints(BRI))
BPI <- as.data.frame(rasterToPoints(BPI))
roughness <- as.data.frame(rasterToPoints(roughness))

#Interpolate slope
interpolated_slopes <- akima::interpp(x=coordinates(slope)[,1],
                                      y=coordinates(slope)[,2],
                                      z = slope$slope,
                                      xo = df$longitude,
                                      yo = df$latitude,
                                      duplicate="strip"
)

#Interpolated aspect
interpolated_aspect <- akima::interpp(x=coordinates(aspect)[,1],
                                      y=coordinates(aspect)[,2],
                                      z = aspect$aspect,
                                      xo = df$longitude,
                                      yo = df$latitude,
                                      duplicate="strip"
)

#Interpolated BRI
interpolated_BRI <- akima::interpp(x=coordinates(BRI)[,1],
                                   y=coordinates(BRI)[,2],
                                   z = BRI$tri,
                                   xo = df$longitude,
                                   yo = df$latitude,
                                   duplicate="strip"
)

#Interpolated BPI
interpolated_BPI <- akima::interpp(x=coordinates(BPI)[,1],
                                   y=coordinates(BPI)[,2],
                                   z = BPI$tpi,
                                   xo = df$longitude,
                                   yo = df$latitude,
                                   duplicate="strip"
)

#Interpolate roughness
interpolated_roughness <- akima::interpp(x=coordinates(roughness)[,1],
                                         y=coordinates(roughness)[,2],
                                         z = roughness$roughness,
                                         xo = df$longitude,
                                         yo = df$latitude,
                                         duplicate="strip"
)

#add that to the original data frame
df$Aspect= interpolated_aspect$z
df$Slope =interpolated_slopes$z
df$BRI= interpolated_BRI$z
df$BPI=interpolated_BPI$z
df$Roughness=interpolated_roughness$z

return(df)}

