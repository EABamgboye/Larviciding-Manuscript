# Load necessary libraries
library(dismo)  # For MaxEnt modeling
library(raster) # For working with spatial raster data
library(maps)   # For map visualization
library(terra)
library(sf)
library(dismo)
library(randomForest)
library(gbm)
library(caret)

Sys.setenv(JAVA_HOME = "C:/Program Files/Eclipse Adoptium/jdk-21.0.8.9-hotspot")

library(rJava)

source("functions.R")

##Read in Challenge Data
lav_chal_df <- read.csv("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/Wet Season Data_Ibadan/lav_chal_bsw.csv")

occurrences_in_chal <- lav_chal_df %>% 
  dplyr::filter(Anophp == "Yes")

##Challenge
occurrences_cl <- occurrences_in_chal %>%
    dplyr::select(X, Y) %>%
  mutate(
    lon = X,
    lat = Y
  ) %>%
  dplyr::select(lon, lat)
# 
# # Ensure they're numeric
# occurrences$lon <- as.numeric(occurrences$lon)
# occurrences$lat <- as.numeric(occurrences$lat)

##Plot sites
# Step 1: Convert the 'occurrences' dataframe to an sf object, assigning its *current* CRS (UTM Zone 31N)
#occurrences_cl_sf <- st_as_sf(occurrences_cl, coords = c("lon", "lat"), crs = 32631)  # Replace with correct EPSG if different

occurrences_cl_sf <- st_as_sf(occurrences_in_chal, coords = c("Y", "X"), crs = 4326)  # Replace with correct EPSG if different


occurrences_cl_sf <- st_transform(occurrences_cl_sf, st_crs(df_ib_c))

# Step 2: Transform to match the CRS of df_ib_a (WGS84)
occurrences_cl_wgs84 <- st_transform(occurrences_cl_sf, crs = st_crs(df_ib_c))

# Step 3: (Optional) Filter points that fall inside df_ib_a (Agugu ward polygon)
#occurrences_in_agugu <- occurrences_wgs84[st_within(occurrences_wgs84, df_ib_a, sparse = FALSE), ]
#occurrences_in_agugu_proj <- occurrences_proj[st_within(occurrences_proj, df_ib_a, sparse = FALSE), ]

#Write to GeoJSON
#st_write(occurrences_in_agugu, "occurrences_in_agugu.geojson")


# Step 3b: (Optional) Filter points that fall inside df_ib_c (Challenge ward polygon)
occurrences_in_cl <- occurrences_cl_wgs84[st_within(occurrences_cl_wgs84, df_ib_c, sparse = FALSE), ]
#occurrences_in_cl_proj <- occurrences_proj[st_within(occurrences_proj, df_ib_c, sparse = FALSE), ]

# Step 4: Plot to verify visually
plot(st_geometry(df_ib_c), col = "lightblue", main = "Occurrence Points within Challenge")
plot(st_geometry(occurrences_in_cl), col = "red", pch = 20, add = TRUE)


##Fix Outlier point
# Get coordinates as matrix
coords_cl <- st_coordinates(occurrences_in_cl)

# Identify the outlier row (here: lowest latitude)
outlier_id <- which.min(coords_cl[, "Y"])

# Compute median of the other points
med_lon <- median(coords_cl[-outlier_id, "X"])
med_lat <- median(coords_cl[-outlier_id, "Y"])

# Replace geometry of the outlier
st_geometry(occurrences_in_cl)[outlier_id] <- st_sfc(
  st_point(c(med_lon, med_lat)),
  crs = st_crs(occurrences_in_cl)
)

# Step 3: Plot using ggplot with geom_sf for both layers
ggplot() +
  geom_sf(data = df_ib_c, fill = NA, color = "black") +
  geom_sf(data = occurrences_in_cl, color = "red", size = 2, alpha = 0.8) +
  labs(
    title = "Positive Breeding Sites in Challenge",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  coord_sf()

##Write file to disk
ccoords <- sf::st_coordinates(occurrences_in_cl)

occurrences_in_cl_df <- cbind(sf::st_drop_geometry(occurrences_in_cl), ccoords)

write.csv(occurrences_in_cl_df, "occurrences_in_cl.csv", row.names = FALSE)

