### exploration_phytoSizeShape.R ###
## examining size and shape changes in marine phyto data

# set metadata and load packages ####
library(tidyverse)

# load data ####
df0 <- openxlsx::read.xlsx(paste0("data/","PhytoChl_2000-2020 (WB+RegionalSeas).xlsx"),
                                  sheet = "Abundance2000_2020USE")
# This data is in LONG format and contains abundance

# initial data tidy ####
df_yr <- df0 %>% 
  dplyr::select(.,-c(Index,Taxon,aphiaID,TorN,Date,DoY,Month,Time,
                     Distance.from.WB.edge,Sample.Id,Easting,Northing,
                     Latitude,Longitude,`Depth.(min)`,`Depth.(max)`,Size.Class)) %>% 
  group_by(Regional.Sea,Waterbody,Eurohab.area,Year,TaxonUSE) %>% 
  summarise(Abundance = mean(Abundance),.groups = "drop") %>% 
  pivot_wider(.,names_from = TaxonUSE, values_from = Abundance,
              values_fill=list(Abundance = 0))
