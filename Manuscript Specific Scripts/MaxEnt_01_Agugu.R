# Load necessary libraries
##Run Java locally before loading library
Sys.setenv(JAVA_HOME = "C:/Program Files/Eclipse Adoptium/jdk-21.0.10.7-hotspot")
library(rJava)
library(dismo)  
library(raster) 
library(maps)   
library(rJava)
library(terra)
library(sf)
library(dismo)
library(randomForest)
library(gbm)
library(caret)

source("functions.R")



#Load occurence data
lav_data <- read.csv("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/Wet Season Data_Ibadan/lav_coords_bsw1.csv")

# lav_data_t <- read.csv("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/lav_dataset_wet.csv") %>% 
#   dplyr::select("Ward.Name", 
# "X_Breeding.site.coordinates_latitude",
# "X_Breeding.site.coordinates_longitude",
# "Anopheles_Caught")
# 
# lav_data_ta <- lav_data_t %>% 
#   dplyr::filter(Ward.Name == "Agugu")

lav_data0 <- read.csv("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/Wet Season Data_Ibadan/lav_coords_bsw1.csv")
lav_data1 <- read.csv("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/lav_coords_bs1.csv")

lav_data0$season <- "Wet"
lav_data1$season <- "Dry"

##Rename column names before merging
lav_data0 <- lav_data0 %>%
  rename(
    bs_label = bs_labelw,
    anoph    = anophw
  )

lav_data1 <- lav_data1 %>%
  rename(
    anoph = anophd
  )

lav_data <- rbind(lav_data0, lav_data1)

anopheles_sites <- lav_data %>% 
  dplyr::filter(anoph == "Yes")

# Keep only latitude and longitude columns
occurrences_in_agugu <- anopheles_sites[, 2:3]
colnames(occurrences_in_agugu) <- c("lon", "lat")


# Ensure they're numeric
occurrences_in_agugu$lon <- as.numeric(occurrences_in_agugu$lon)
occurrences_in_agugu$lat <- as.numeric(occurrences_in_agugu$lat)

##Ensure points fall within agugu ward
# Step 1: Convert to sf object 
occurrences_a_sf <- st_as_sf(occurrences_in_agugu, coords = c("lon", "lat"), crs = 4326)

# occurrences_proj <- st_transform(occurrences_proj, 4326)
# df_ib_a <- st_transform(df_ib_a, 4326)

# Step 2: Reproject to match the CRS of df_ib_a
occurrences_proj <- st_transform(occurrences_a_sf, crs = st_crs(df_ib_a))

# Plot to verify visually
plot(st_geometry(df_ib_a), col = "lightblue", main = "Occurrence Points within Agugu")
plot(st_geometry(occurrences_a_sf), col = "red", pch = 20, add = TRUE)


# Plot using ggplot with geom_sf for both layers
ggplot() +
  geom_sf(data = df_ib_a, fill = NA, color = "black") +
  geom_sf(data = occurrences_proj, color = "red", size = 2, alpha = 0.8) +
  labs(
    title = "Positive Breeding Sites in Agugu",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  coord_sf()
  