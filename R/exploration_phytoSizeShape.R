### exploration_phytoSizeShape.R ####
## examining size and shape changes in marine phyto data

### approach ####
# Import phytoplankton abundance data
# Summarise abundances of individual taxa by year
# Extract taxa with values for each year(?)
## (Consider) Where there is a missing value for otherwise common spp, consider interpolating
## abundance values
# For each taxon, scale abundances so that each year's abundance is a proportion
# of the maximum count for that spp. (all counts range from 0.0 to 1.0)
# Calculate between year growth rates
# Across all taxa, calculate pairwise growth rates
# For each pairwise comparison, calculate the cosine of observed growth rates
# against a 1-to-1 relationship

### setting up ####
#### set metadata and load packages ####
library(tidyverse)

#### load data ####
df0 <- openxlsx::read.xlsx(paste0("data/","PhytoChl_2000-2020 (WB+RegionalSeas).xlsx"),
                                  sheet = "Abundance2000_2020USE")

# This data is in LONG format and contains abundances of >500 taxa across 112 WBs

# initial data tidy ####
df_yr <- df0 %>% 
  dplyr::select(.,-c(Index,Taxon,aphiaID,TorN,Date,DoY,Month,Time,# remove un-needed cols
                     Distance.from.WB.edge,Sample.Id,Easting,Northing,
                     Latitude,Longitude,`Depth.(min)`,`Depth.(max)`,Size.Class)) %>% 
  group_by(Regional.Sea,Waterbody,Eurohab.area,Year,TaxonUSE) %>% # group columns for summarising
  summarise(Abundance = mean(Abundance),.groups = "drop") %>% # calc mean abundances for each year
  pivot_wider(.,names_from = TaxonUSE, values_from = Abundance, #widen data to 1 row per WB/year
              values_fill=list(Abundance = 0))


## how many years do we have per WB?
df_yr %>% 
  group_by(Waterbody) %>% 
  count() -> tmp

hist(tmp$n) ### big range here.  Little to be gained by retaining poorly-represented WBs

accept <- 14 ### how many years of data is acceptable?
tmp$kp <- ifelse(tmp$n >= accept, TRUE, FALSE)
kp <- tmp$Waterbody[tmp$kp==TRUE]  

### trim data accordingly  
df_yr_trm <- df_yr[df_yr$Waterbody %in% kp,] ## retain WBs with sufficient data
df_yr_trm <- df_yr_trm %>% ## remove 'empty' species columns
  select(-where(~is.numeric(.) && sum(.) == 0))
