#' Match catch species with fishing coordinates and bathymetry
#'
#' @param dffish ifish postgresQL data frame containing fish species, length data, and boat/landing information.
#' @param dftracker ifish postgresQL data frame containing tracker information, including latitude, longitude, and date/time.
#' @param depthraster bathymetry raster file
#' @return data frame that combines catch data from CODRS photographs and fishing coordinates
#'         from Spot Trace. Fishing ground was matched with photographs based on first_codrs_photo_date and date of fishing coordinates.
#'         Fishing trips with no corresponding fishing coordinates are grouped by boat name and assigned its mean latitude and mean longitude.
#'

catch_location <- function(dffish, dftracker, depthraster){
#filter for snapper only
dffish <- dffish %>%setNames(make.unique(names(.))) %>%
  dplyr::filter(program_type=='Snapper') %>%
  unite(fishname, fish_genus, fish_species, sep=" ", remove= FALSE) %>%
  dplyr::mutate(weight=(var_a *(cm^var_b)/1000))

#Remove data with no fishname
dffish <- dffish[!(is.na(dffish$fishname) | dffish$fishname==" "), ] #remove NAs and blank fishnames
dftracker <- dftracker %>%setNames(make.unique(names(.)))

####Load fishing locations
fishinglocations <- fishing_grounds(df=dftracker, depthraster = depthraster)
fishinglocations$datetime <- as.Date(fishinglocations$datetime)
#df containing all the environmental predictors for the top species
mergedlandings <- merge(x=dffish, y=fishinglocations, by.x=c('first_codrs_picture_date','boat_name'), by.y=c('datetime','boatname'), all=FALSE)
mergedlandings <- mergedlandings %>% dplyr::select(landing_id,fishing_gear.x, latitude, longitude, depth,
                                                   gt_estimate.x, first_codrs_picture_date, Aspect, Slope, BPI, BRI, Roughness) %>%
  dplyr::distinct(landing_id, .keep_all=TRUE)
#But not all landings have matching envrionmental variables
#Total landings=4613
#Landing with matching spot trace pings=4435
#unmatched= 1964
###
#For the remainging landing_id, use average location and depth based on each boat_name
#find landing_id that has not been matched and extract that from the df2top data frame
matchedlandingid <- mergedlandings %>% distinct(landing_id)
#Filter df2top
unmatchedlandings <- anti_join(dffish, matchedlandingid) %>% distinct(landing_id, .keep_all=TRUE)
#Get boat names
unmatchedboat <- unmatchedlandings %>% distinct(boat_name)
#Filter out boats that have matched landings
fishinglocationsunmatched <- semi_join(fishinglocations, unmatchedboat, by=c('boatname'='boat_name'))
#Calculate average lat, long, and depth by boat
meanstats <- fishinglocationsunmatched %>% group_by(boatname) %>%
  mutate(meanlat=mean(latitude), meanlong=mean(longitude), meandepth=mean(depth), meanasp=mean(Aspect), meanslope=mean(Slope), meanBPI=mean(BPI),
         meanBRI=mean(BRI), meanrough=mean(Roughness)) %>%
  dplyr::select(boatname, meanlat, meanlong, meandepth, gt_estimate, fishing_gear, datetime, meanasp, meanslope, meanBPI, meanBRI, meanrough) %>%
  distinct(boatname, .keep_all=TRUE)
#Add mean stats to the unmatchedlanding data frame #####
unmatchedlandings$boat_name <- as.factor(unmatchedlandings$boat_name)
unmatchedstats <-left_join(x=unmatchedlandings, y=meanstats, by=c('boat_name'='boatname'))
unmatchedstats <- unmatchedstats%>%
  dplyr::select(landing_id, fishing_gear.x, gt_estimate.x, meanlat, meanlong, meandepth,datetime,meanasp, meanslope, meanBPI, meanBRI, meanrough)
na_boats <- unmatchedstats %>% filter_all(any_vars(is.na(.))) %>% #filter for any rows WITH NAs
  dplyr::select(landing_id) #this is landing_id that has no fishing info
checkboatname <- semi_join(dffish, na_boats) %>% distinct(boat_name, registration_port, gt_estimate)
#These boats do not have corresponding fishing locations, so removed from community data and environmental data
#Remove NAs from environmental
unmatchedstats <- na.omit(unmatchedstats)
#Create column order so we can reorder the column names so that the two dataframes will match
col_order <- c('landing_id','fishing_gear.x','meanlat','meanlong','meandepth','gt_estimate.x','datetime','meanasp','meanslope','meanBPI','meanBRI','meanrough' )
#ensure that the number of columns in col_order are the same as the number of columns in the data frame
if(length(col_order) != length(colnames(mergedlandings))){
  stop('matched and unmatched data frame are different lengths, check columns')
}
#reorder by column index so that the two dataframes will match
unmatchedstats <- unmatchedstats[col_order]
#Combine mergedlandings data with unmacthedstats data
#Just stack em on top of each other essentially
colnames(unmatchedstats) <- colnames(mergedlandings)
environmental <- rbind(mergedlandings, unmatchedstats)
environmental$fishing_gear.x <- as.factor(environmental$fishing_gear.x)
#Make sure community data and environmental data are in the same order

catch_and_location <- inner_join(dffish, environmental, by=c('landing_id'="landing_id"))

return(catch_and_location)}
