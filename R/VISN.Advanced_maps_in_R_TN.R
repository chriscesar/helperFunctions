#load the following libraries

library(readr)  # For reading in files
library(dplyr)  # For formatting and cleaning data
library(rgdal)  # For manipulating map data
library(raster)  # For clipping shapefile polygons
library(ggplot2)  # For drawing plots
library(maps)  # For making maps
library(mapdata)  # For supplying map data
library(maptools) # For reading map data
library(ggmap)  # For plotting map data, downloading map tiles from online sources
library(ggsn)
library(tmap) # For reading and plotting spatial data - useful for interactive maps 
library(openxlsx) # for opening Excel files
library(xlsx)
#Creating a map using ggplot2 and maps

#create a map showing occurrence records of 2 species of bird. 
#Rueppell's Vulture (Gyps rueppellii) feeds on large mammalian carrion and
#the African Penguin (Spheniscus demersus) feeds on small marine fish, 
#We will use species occurence data from the Global Biodiversity Information Facility (GBIF)

#read the data in from the GBIF repository 

vulture <- read.csv("https://raw.githubusercontent.com/ourcodingclub/CC-6-Maps/master/Gyps_rueppellii_GBIF.csv", sep="\t")

penguin <- read.csv("https://raw.githubusercontent.com/ourcodingclub/CC-6-Maps/master/Spheniscus_dermersus_GBIF.csv", sep="\t")

#take a moment to explore the structure of these data

str(vulture)
str(penguin)

View(vulture)
View(penguin)



#Now let's clean up the data using dplyr

# Keep only the columns we need
vars <- c("gbifid", "scientificname", "locality", "decimallongitude",
          "decimallatitude", "coordinateuncertaintyinmeters")

vulture_trim <- vulture %>% dplyr::select(one_of(vars))
penguin_trim <- penguin %>% dplyr::select(one_of(vars))
# `one_of()` is part of `select()` and selects all columns specified in `vars`

# Combine the dataframes
pc_trim <- bind_rows(vulture_trim, penguin_trim)

# Check column names and content
str(pc_trim)

# Check that species names are consistent
unique(pc_trim$scientificname)
# Needs cleaning up

# Clean up "scientificname" to make names consistent
pc_trim$scientificname <- pc_trim$scientificname %>%
  recode("Gyps rueppellii (A. E. Brehm, 1852)" = "Gyps rueppellii",
         "Gyps rueppellii subsp. erlangeri Salvadori, 1908" = "Gyps rueppellii",
         "Gyps rueppelli rueppelli" = "Gyps rueppellii",
         "Spheniscus demersus (Linnaeus, 1758)" = "Spheniscus demersus")

# Checking names
unique(pc_trim$scientificname)
# Done

#Now we can make a preliminary plot to make sure the data looks right. Remember, a map is just a graph with longitude and latitude as the x and y axes:
  
ggplot(pc_trim, aes(x = decimallongitude, y = decimallatitude, colour = scientificname)) +
geom_point()

#If you squint, you might be able to see the southern African cape, with lots of penguins on it. It looks like some of the penguin populations might be from zoos in U.S cities, but we only want to plot natural populations, so let's remove those entries:

pc_trim_us <- pc_trim %>% filter(decimallongitude > -50)

ggplot(pc_trim_us, aes(x = decimallongitude, y = decimallatitude, colour = scientificname)) +
  geom_point()

#Now we can add some simple map data from the maps package, which integrates nicely with ggplot2.

#First we need to pull some map data: # see the maps package documentation for other basemaps
  
  map_world <- borders("world", fill = "grey90", colour = "black")
  
#Then you can plot map_world by simply adding it to your ggplot2 call and designating the ggplot() as a map using coord_quickmap():
    
    ggplot() +
    map_world +  # Add world map
    geom_point(data = pc_trim_us,  # Add and plot species data
               aes(x = decimallongitude, y = decimallatitude, colour = scientificname)) +
    coord_quickmap() +  # Define aspect ratio of the map, so it doesn't get stretched when resizing
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") + 
    guides(colour=guide_legend(title="Species"))
    
#You can also subset the contents of world_map, to only plot a particular country or set of countries. Say we wanted to only plot the distribution of vultures and penguins in southern Africa, in the countries of South Africa, Namibia, Botswana, Zimbabwe. We can set the region argument of borders():
    
# Make a vector of country names
    
    saf_countries <- c("South Africa", "Namibia", "Botswana", "Zimbabwe")
    
# Call the vector in `borders()`
    
    map_saf <- borders("world", regions = saf_countries, fill = "grey90", colour = "black")
  
#Then define the x and y axis limits in ggplot() using xlim() and ylim() with a bit of trial and error:
      
      ggplot() +
      map_saf +  # Add map
      geom_point(data = pc_trim_us,  # Add and plot speices data
                 aes(x = decimallongitude, y = decimallatitude, colour = scientificname)) +
      coord_quickmap() +  # Define aspect ratio of the map, so it doesn't get stretched when resizing
      xlim(8, 35) +  # Set x axis limits, xlim(min, max)
      ylim(-35, -15) +  # Set y axis limits
      theme_classic() +  # Remove ugly grey background
      theme(legend.position = "top") +  # Position the legend at the top of the plot
      xlab("Longitude") +
      ylab("Latitude") + 
      guides(colour=guide_legend(title="Species"))
      

      
      
##########################################################################

#Creating a map using ggplot2 + ggmap
      
#The ggmap package also offers decent options for plotting maps. ggmap pulls map tiles as image files from various online sources, including Google maps and Open Street Maps.
      
#First make a bounding box (bbox), to tell ggmap what region of the world to download map tiles for. The bounding box must be in the form of a vector, with decimal latitude and longitude values in this order c(lowerleftlongitude, lowerleftlatitude, upperrightlongitude, upperrightlatitude). We can set the bounding box to the size of our data using the following:      

bbox <- c(min(pc_trim_us$decimallongitude) - 2,
                min(pc_trim_us$decimallatitude) - 2,
                max(pc_trim_us$decimallongitude) + 2,
                max(pc_trim_us$decimallatitude) + 2
      )
      
# the +2 -2 values are added to make the edges of the map slightly larger than the limits of the data, purely for aesthetic reasons.
      
#Now to download the map data from ggmap, using bbox in the location argument:
        
    map_penguin <- get_map(location = bbox, source = "stamen", maptype = "toner-lite")
      
#We can check that the map is correct by plotting the map_penguin object:
      
      ggmap(map_penguin)
      
#To add the data, use ggplot2 syntax but define the base plot as a ggmap() instead of a ggplot():
      
      ggmap(map_penguin) +
        geom_point(data = pc_trim_us,
                   aes(x = decimallongitude,
                       y = decimallatitude,
                       colour = scientificname),
                   alpha = 0.6,                     # `alpha=` sets the transparency of `geom_point()`, from 0 (transparent) to 1 (opaque)
                   size = 2) +                      # `size=` sets the diameter of `geom_point()`
        xlab(expression("Decimal Longitude ("*degree*")")) +  # Wrapping the label in `expression()` and using *degree* lets us add a degree symbol
        ylab(expression("Decimal Latitude ("*degree*")"))
      
#ggmap can access a whole load of different map types. 
#Have a go at re-plotting the map above with some alternative map types by replacing the source = and maptype = arguments in get_map().
#But be warned, not every maptype is available for the entire world: -see the documentation
    
      
?get_map()   
      
# e.g.
      
map_penguin <- get_map(location = bbox, source = "google", maptype = "terrain")
      

ggmap(map_penguin) +
  geom_point(data = pc_trim_us,
             aes(x = decimallongitude,
                 y = decimallatitude,
                 colour = scientificname),
             alpha = 0.6,                     # `alpha=` sets the transparency of `geom_point()`, from 0 (transparent) to 1 (opaque)
             size = 2) +                      # `size=` sets the diameter of `geom_point()`
  xlab(expression("Decimal Longitude ("*degree*")")) +  # Wrapping the label in `expression()` and using *degree* lets us add a degree symbol
  ylab(expression("Decimal Latitude ("*degree*")"))


###########################################################################

#Using shapefiles

#shapefiles can be loaded directly in R using the 'rgdal' and 'sp' packages

# Let's load in the WFD TW & CW waterbodies shapefile 

WFD_wbs <- readOGR("G:\\N_Marine\\04 A & R\\Key GIS Data\\TraC Shapefile latest\\Cycle2TRAC_for_Chris(with_names).shp")

#note that shapefiles are automatically recognised as the appropriate spatial object
#in this case a SpatialPolygonsDataFrame - an S4 object - with the various slots that make up the spatial object 

str(WFD_wbs) # view structure

#note the SPDF is built up of many 'slots' with the polygon information '@polygons', attribute data '@data', spatial information '@coords' & '@bbox', projection information '@proj4string'. 

# we can quickly plot shapefiles using the generic plot function 

plot(WFD_wbs)

#sometimes we may want to alter the data of shapefile. This is not easy to do with spatial objects. 
#But we can decontruct the spatial object and run the usual data cleaning, summarisation procedcures etc. on the decontructed data.frames 

WFD_wbs.df<- WFD_wbs@data
WFD_wbs.df[] <- lapply(WFD_wbs.df,as.character)#converts factors to charactors


#Now let's use this shapefile as a basemap and add some data to it

# For this example let's use the 2018 DIN interim classifications 

DIN_2018 <- read.xlsx ("G:\\N_Marine\\01 Dir_ Conv_ Nat_l\\WFD\\Class_n Reporting\\WFD Interim Assessment 2018\\DIN\\DIN_(Alerts)_results_2018-03-14.xlsx")

#note this data has no spatial information - but there are many common fields between this and our shapefile with unique IDs   e.g. WB.ID - but slightly different column headings

#first rename the columns to match 

colnames(WFD_wbs.df)[2] <- "WB.ID"

#now join the two based on WB.ID

DIN_2018.sp <- merge(WFD_wbs.df, DIN_2018, by= "WB.ID", all=TRUE)

# we can now write this information back into the data slot of the shapefile to associate it with the spatial iformation

WFD_wbs@data <- DIN_2018.sp



#now lets plot this 

tmap_mode("plot")
p1<-tm_shape(WFD_wbs)+tm_fill("Reporting.Classification", textNA="Insufficient data for classification", title="2018 DIN classification")+tm_scale_bar(breaks = NULL, width = NA, size = 0.8, text.color = NA,
                                              color.dark = "black", color.light = "white", lwd = 1, position = NA,
                                              just = NA) + tm_compass(north = 0, type = NA, fontsize = 0.8, size = NA,
                                                                      show.labels = 1, cardinal.directions = c("N", "E", "S", "W"),
                                                                      text.color = NA, color.dark = NA, color.light = NA, lwd = 1,
                                                                      position = c("RIGHT", "TOP"), just = NA)

p1

#the polygon colours can be changed by setting the palette see  tmaptools::palette_explorer() for options


# now we can interactively explore this new shapefile and can view the classification info by clicking the polygons

tmap_mode("view")


tm_shape(WFD_wbs)+tm_fill()

#or view the previous plot interactively 

p1








