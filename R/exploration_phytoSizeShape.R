### exploration_phytoSizeShape.R ####
## examining size and shape changes in marine phyto data

### overall idea is based on a very simplified version of the proposed approach
### of Spencer (2015) http://dx.doi.org/10.1016/j.jtbi.2015.01.002

### approach ####
# Import phytoplankton abundance data ==DONE==
# Summarise abundances of individual taxa by year ==DONE==
# For each taxon, scale abundances so that each year's abundance is a proportion
#   of the maximum count for that spp. for that WB (all counts range from 0.0 to 1.0) ==DONE==
# Select appropriate taxa to continue analyses
## Extract taxa with values for each year(?)
## (Consider) Where there is a missing value for otherwise common spp, consider interpolating
##  abundance values
# Calculate between year growth rates
# Across all taxa, calculate pairwise growth rates
# For each pairwise comparison, calculate the cosine of observed growth rates
#   against a 1-to-1 relationship

### setting up ####
#### set metadata and load packages ####
library(tidyverse)

# Import phytoplankton abundance data ####
#### load data ####
df0 <- as_tibble(openxlsx::read.xlsx(paste0("data/in/","PhytoChl_2000-2020 (WB+RegionalSeas).xlsx"),
                                  sheet = "Abundance2000_2020USE"))

# This data is in LONG format and contains abundances of >500 taxa across 112 WBs

# Summarise abundances of individual taxa by year ####
#### initial data tidy ####
df_yr <- df0 %>% 
  # remove un-needed cols
  dplyr::select(.,-c(Index,Taxon,aphiaID,TorN,Date,DoY,Month,Time,
                     Distance.from.WB.edge,Sample.Id,Easting,Northing,
                     Latitude,Longitude,`Depth.(min)`,
                     `Depth.(max)`,Size.Class)) %>%
  # group columns for summarising
  group_by(Regional.Sea,Waterbody,Eurohab.area,Year,TaxonUSE) %>%
  # calc mean abundances for each year
  summarise(Abundance = mean(Abundance),.groups = "drop") %>%
  # widen data to 1 row per WB/year
  pivot_wider(.,names_from = TaxonUSE, values_from = Abundance,
              values_fill=list(Abundance = 0))

#### how many years do we have per WB? ####
df_yr %>% 
  group_by(Waterbody) %>% 
  count() -> tmp

# hist(tmp$n, breaks = 20) ### big range here.  Little to be gained by retaining poorly-represented WBs

accept <- 14 ### how many years of data is acceptable?
tmp$kp <- ifelse(tmp$n >= accept, TRUE, FALSE) # flag WBs meeting acceptable data amount
kp <- tmp$Waterbody[tmp$kp==TRUE]  # keep only those flagged
rm(accept, tmp)

#### trim data to retain WBs with sufficient data ####
df_yr_trm <- df_yr[df_yr$Waterbody %in% kp,] ## retain WBs with sufficient data
rm(kp)
df_yr_trm <- df_yr_trm %>% ## remove 'empty' species columns
  select(-where(~is.numeric(.) && sum(.) == 0))

write.csv(df_yr_trm, file="data/out/df_yr_trm.csv",row.names = FALSE)

# For each taxon, scale abundances so that each year's abundance is a proportion ####
### does NOT calculate by WB
# df_yr_trm_norm <- df_yr_trm %>%
# # explicitly name which cols to normalise
# mutate(across(.cols = Bacillariophyceae:Surirella,
#                 .fns = ~ . / max(., na.rm = TRUE)))

# Assuming your data frame is called filtered_data
### DOES calculate by WB
df_yr_norm <- df_yr_trm %>%
  group_by(Waterbody) %>%
  mutate(across(.cols = Bacillariophyceae:Surirella, 
                .fns = ~ . / max(., na.rm = TRUE))) %>%
  ungroup()

write.csv(df_yr_norm, file="data/out/df_yr_norm.csv",row.names = FALSE)

#### remove rare spp ####
### normalised values are returned as NAs when a taxon doesn't appear at all in a given WB
### remove these taxa
na_threshold <- 0 #maximum number of NAs allowed
## remove columns with more than maximum
df_yr_norm_trm <- df_yr_norm %>%
  select(-which(colSums(is.na(.)) > na_threshold))
rm(na_threshold)
write.csv(df_yr_norm_trm, file="data/out/df_yr_norm_trm.csv",
          row.names = FALSE)## write data

### tidy up ####
# remove data
rm(list = ls(pattern = "^df"))
# unload packages
detach("package:tidyverse", unload = TRUE)
