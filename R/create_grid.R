#' Create a grid in the Indonesian EEZ
#'
#' @param eez sf object for Indonesian EEZ shapefile
#' @param type type of grid. Default type='polygon'. Other options are 'centers'. Argument in function must be a string.
#' @export


create_grid <- function(eez, type='polygons'){
#Create grid in the Indonesian eez area
eezbox <- st_bbox(eez)
xrange <- eezbox$xmax - eezbox$xmin
yrange <- eezbox$ymax - eezbox$ymin
# size of squares
grid_spacing <-1
# number of polygons necessary
rozmery <- c(xrange/grid_spacing , yrange/grid_spacing) %>%
  ceiling() # rounded up to nearest integer
#Create grid
grid <- st_make_grid(eez, square = T, n = rozmery, what=type) %>% # the grid, covering bounding box
  st_difference(idn)
area <- st_area(grid)
grid <- st_as_sf(data.frame(ID=c(1:length(area)), area=area, geometry=grid))
return(grid)}

