library (spatialEco)
library (sp)
library (rgdal)
library (tmap)

poly.shp = file.choose() # select polygon shapefile (this will be the basemap layer)
poly.shp <- readOGR(poly.shp)

point.shp = file.choose() # select points shapefile (if you only have an excel file with X&Y data see below)
point.shp <- readOGR(point.shp)

#additional layers can be loaded in the same way just copy the code above(lines 10 & 11) and change 'point.shp' to an appropriate name

#plot map with polygons and points layers above

#note - this will only work if all layers have the same projection - if not see below

p1<-tm_shape(poly.shp)+tm_fill()+tm_scale_bar(breaks = NULL, width = NA, size = 0.8, text.color = NA,
                                         color.dark = "black", color.light = "white", lwd = 1, position = NA,
                                         just = NA) + tm_compass(north = 0, type = NA, fontsize = 0.8, size = NA,
                                                                 show.labels = 1, cardinal.directions = c("N", "E", "S", "W"),
                                                                 text.color = NA, color.dark = NA, color.light = NA, lwd = 1,
                                                                 position = c("RIGHT", "TOP"), just = NA)+tm_shape(point.shp)+tm_dots()

p1

# we can also make the map interactive, this allows us to click on points to view their attributes

tmap_mode("view")
p1


#reading points from Excel workbook

library(xlsx)
library(openxlsx)

stfile = file.choose() #select excel file
filename = strsplit(stfile,'\\',fixed=TRUE)[[1]][length(strsplit(stfile,'\\',fixed=TRUE)[[1]])]
dir = substr(stfile, 1, nchar(stfile)-nchar(filename))

# Read and inspect data sheet names
stsheet <- getSheetNames(stfile)
stsheet

# load data from the desired sheet in workbook

point.data <- read.xlsx(stfile, sheet = stsheet[1], #change the stsheet number accordingly or enter the sheetname between " " 
          detectDates = TRUE,
          colNames = TRUE)

#define the coordinates
coordinates(point.data) <- c("East", "North") # change "East" and "North" to whatever the columnames containing the coordinates are

#and set projection (make sure the correct projection is set - some common ones below)

# for transverse mercator projected latlong & OSBG36 datum (if you coordinates are as X&Y or Easting and Northing then this will probably work)

proj4string(point.data) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36") #


# for latlong & OSBG36 datum (for coordinates in decimal degrees)

proj4string(point.data) <- CRS("+proj=longlat +datum=OSGB36") 

#if the above don't work try changing '+datum=OSGB36' to '+datum=WGS84'
#for other projects change the '+proj=' argument appropriately (try Googling 'proj4string R' for help) )


#plot it
tmap_mode("plot")

p2<-tm_shape(poly.shp)+tm_fill()+tm_scale_bar(breaks = NULL, width = NA, size = 0.8, text.color = NA,
                                          color.dark = "black", color.light = "white", lwd = 1, position = NA,
                                          just = NA) + tm_compass(north = 0, type = NA, fontsize = 0.8, size = NA,
                                                                  show.labels = 1, cardinal.directions = c("N", "E", "S", "W"),
                                                                  text.color = NA, color.dark = NA, color.light = NA, lwd = 1,
                                                                  position = c("RIGHT", "TOP"), just = NA)+tm_shape(point.data)+tm_dots()

p2


#if layers have different projections then we can simply transform projections so they match 

#e.g. let's suppose the 'poly.shp' and 'point.data' have different projections

#take the projection information from 'poly.shp' and transform 'point.data' to match

point.data <- spTransform(point.data, CRS=proj4string(poly.shp))










