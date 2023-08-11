#This script replicates the 'is closest to' method of joining points to polygons in ArcGIS 



library (rgeos)
library (sp)
library (rgdal)
library(spatialEco)

# Step 1 - Load the data

# Method 1 - perform points to polygons join using two shapefiles 

#load the polygon shapefile
poly.shp = file.choose() # select polygon shapefile 
poly.shp <- readOGR(poly.shp)

#load the points shapefile - If you only have the point data as XY data in an Excel workbook jump to Method 2. 
point.shp = file.choose() # select points shapefile (if you only have an excel file with X&Y data see below)
dir= substr(point.shp, 1, nchar(point.shp)-nchar(filename))
point.shp <- readOGR(point.shp)


#Method 2 

#reading points from Excel workbook

library(xlsx)
library(openxlsx)

stfile = file.choose() #select excel file
filename = strsplit(stfile,'\\',fixed=TRUE)[[1]][length(strsplit(stfile,'\\',fixed=TRUE)[[1]])]
dir2 = substr(stfile, 1, nchar(stfile)-nchar(filename))

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

# for transverse mercator projected latlong & OSBG36 datum (if you coordinates are as X&Y or Easting and Northing and came from WIMS then this will work)

proj4string(point.data) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36") #

point.shp <- point.data


# Step 2 -  You can quickly check the points align the to polygons by plotting it: 

library(tmap)

tm_shape(poly.shp)+tm_fill()+tm_shape(point.shp)+tm_dots()


#Step 2 - computing distances between points and polygons

# First we need to trasnform to a planar projection (UTM zone 30 here - which is good for England)
# If we kept the geographical projection (latlong) then the distortion would likely result in big inaccuracties when computing distances 

CRS <- "+proj=utm +zone=30 +datum=OSGB36 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
poly.shp <- spTransform(poly.shp, CRS)
point.shp <- spTransform(point.shp, CRS)

#Set up container for results
n <- length(point.shp)
nearest_polygon <- integer(n)

# For each point, find name of nearest polygon - if your polygon shapefile doesn't have an 'OBJECTID' then edit 'OBJECTID' after '$' operator so that is references an field in the shapefile attributes that is unique for all polygons (if you're  not sure what the field names are type 'colnames(poly.shp@data)' into the console)
#depending on the number of points you want to join this loop process may take a while

for (i in seq_along(nearest_polygon)) {
  nearest_polygon[i] <- poly.shp@data$OBJECTID [which.min(gDistance(point.shp[i,], poly.shp, byid=TRUE))]
}


#Which reurtns a list of values which references the nearest Polygon OBJECTID for each point

nearest_polygon

#so now we need to add these OBJECTIDs data slot of the points shapefile 

point.shp@data$OBJECTID <- nearest_polygon

# then join the point data to the polygon data merging by OBJECTID


join <- merge(poly.shp@data, point.shp@data, by= "OBJECTID", all.y=T) # creates a dataframe with all the joined attribute data

#we can add this back to the point shapefile 

point.shp@data <- join

#and export this as a shapefile saving it to the same folder as the original points file

#change the 'dsn= to dir1' if you started with a shapefile for the points data

writeOGR(point.shp, dsn=dir2, layer="join_output", driver="ESRI Shapefile")

#or if you simply want the data as a worksheet save to Excel file (i.e. the .dbf file of an ESRI shapefile)

write.xlsx(point.shp,file=dir2, sheetName = "join_output", col.names = T)


## If you only want to join points that strictly fall within a polygon boundary 


#first let's ensure each polygon has a unique ID 

poly.shp@data$poly.ids <- 1:nrow(poly.shp) 

#The point.in.poly function in the spatialEco package returns a SpatialPointsDataFrame object of the points that intersect an sp polygon object 

points_to_polygons <- spatialEco::point.in.poly(points.shp, poly.shp, poly.id = "poly.ids")


