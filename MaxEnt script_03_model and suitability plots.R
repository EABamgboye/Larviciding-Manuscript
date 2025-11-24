##Desktop 
Sys.setenv(JAVA_HOME="C:/Program Files/Eclipse Adoptium/jdk-21.0.7.6-hotspot")
library(rJava)  # try loading rJava manually

##My Laptop
Sys.setenv(JAVA_HOME="C:/Program Files/Eclipse Adoptium/jdk-21.0.8.9-hotspot")

library(rJava)  # try loading rJava manually

#------------------------------------------------------------------------------
#1. Convert SpatRaster to RasterStack for MaxEnt
predictors_r <- raster::stack(all_env_vars)

# 2. Make occurrence points a matrix of coordinates

#occ_coords <- st_coordinates(occurrences)  # gives X = lon, Y = lat


# Convert sf to matrix in raster CRS
occ_coords <- st_coordinates(st_transform(occurrences_in_agugu, crs(predictors_r)))

# Get raster extent
ext(predictors_r)
# Or, using terra:
terra::ext(predictors_r)

# Extract values for all layers
occ_vals <- terra::extract(predictors_r, vect(st_transform(occurrences, crs(predictors_r))))
occ_vals

##My laptop
library(terra)

# Option 1: Use EPSG code
occ_vect <- vect(occurrences, geom = c("lon", "lat"), crs = "EPSG:4326")

# If your raster is not in WGS84, project the points to match raster CRS
if (!compareCRS(occ_vect, predictors_r)) {
  occ_vect <- project(occ_vect, predictors_r)
}

# Convert RasterStack to SpatRaster
predictors_r_terra <- rast(predictors_r)

# Extract raster values
occ_vals <- raster::extract(predictors_r_terra, occ_vect)


# Remove rows where any predictor is NA
valid_idx <- complete.cases(occurrences[, -1])  # exclude ID column
occ_coords_valid <- occurrences[valid_idx, ]

# Crop raster to extent of occurrences + buffer
ext_occ <- ext(occ_vect)
predictors_crop <- crop(predictors_r, ext_occ)


# 3. Run MaxEnt
maxent_model <- maxent(predictors_r, occ_coords_valid)

# 4. View summary
print(maxent_model)


# Predict across study area
suitability <- predict(predictors_r, maxent_model)

crs(suitability) <- "EPSG:32631"

# Plot results
plot(suitability, main="Habitat Suitability")
points(occurrences_sf, col="red", pch=20)
points(absent_sites_in_agugu, col="blue", pch=20)



plot(ward_vect_mi, border="blue", add=TRUE)


library(ggplot2)
library(sf)
library(raster)
library(terra) # optional, for newer raster handling

# Convert raster to dataframe for ggplot
suitability_df <- as.data.frame(rasterToPoints(suitability))
colnames(suitability_df) <- c("x", "y", "suitability")

# Make sure your points are sf objects
# occurrences_sf -> presence points
# absent_sites_in_agugu -> absence points

ggplot() +
  geom_raster(data = suitability_df, aes(x = x, y = y, fill = suitability)) +
  #geom_sf(data = df_ib_a, fill = NA, color = "white")+
  scale_fill_viridis_c(name = "Suitability") +  # nicer color scale
  geom_sf(data = occurrences_sf, color = "red", size = 2) +
  geom_sf(data = absent_sites_in_agugu, color = "blue", size = 2) +
  labs(title = "Habitat Suitability") +
  theme_minimal() +
  coord_sf()


# Reproject shapefile and points to match raster
df_ib_a_utm <- st_transform(df_ib_a, crs(suitability))
occurrences_utm <- st_transform(occurrences_sf, crs(suitability))
absent_sites_utm <- st_transform(absent_sites_in_agugu, crs(suitability))

# Plot again
ggplot() +
  geom_raster(data = suitability_df, aes(x = x, y = y, fill = suitability)) +
  scale_fill_viridis_c(name = "Suitability") +
  geom_sf(data = df_ib_a_utm, fill = NA, color = "white", size = 0.6) +
  geom_sf(data = occurrences_utm, color = "red", size = 2) +
  geom_sf(data = absent_sites_utm, color = "blue", size = 2) +
  labs(title = "Habitat Suitability") +
  theme_minimal() +
  coord_sf()


##Plot only within ward shape file
# Make sure raster and ward have the same CRS
ward_vect_utm <- project(ward_vect, suitability)
library(terra)

# Convert RasterLayer to SpatRaster
suitability_terra <- rast(suitability)

# Mask with SpatVector
suitability_mask <- mask(suitability_terra, ward_vect_utm)

# Convert to dataframe for ggplot
suit_df <- as.data.frame(suitability_mask, xy = TRUE)
names(suit_df)[3] <- "Suitability"

# Plot clipped map
ggplot() +
  geom_raster(data = suit_df, aes(x = x, y = y, fill = Suitability)) +
  scale_fill_viridis_c(option = "plasma") +
  geom_sf(data = st_as_sf(ward_vect_utm), fill = NA, color = "black", size = 0.5) +
  geom_sf(data = occurrences_sf, color = "green", size = 2.5) +
  #geom_sf(data = hh_pos_a_df, color = "aliceblue", size = 1.5) +
  theme_manuscript() +
  labs(title = "Habitat Suitability", fill = "Suitability")


##Adding Household suitability

# Add a column to each point layer to indicate the type
occurrences_sf$Type <- "Larva present"
hh_pos_a_df$Type <- "Malaria Positive Households"

hh_unique <- hh_pos_a_df %>%
  distinct(Serial.Number, .keep_all = TRUE)

# Combine both into one sf object
points_combined <- rbind(occurrences_sf[, "Type"], hh_unique[, "Type"])



# Plot
ggplot() +
  geom_raster(data = suit_df, aes(x = x, y = y, fill = Suitability)) +
  scale_fill_viridis_c(option = "plasma", name = "Suitability") +
  geom_sf(data = st_as_sf(ward_vect_utm), fill = NA, color = "black", size = 0.5) +
  geom_sf(data = points_combined, aes(color = Type), size = 2) +
  scale_color_manual(values = c("Larva present" = "green2", "Malaria Positive Households" = "snow2")) +
  theme_manuscript() +
  labs(title = "Habitat Suitability", color = "Point Type")


##Evaluation of MaxEnt Model
# 1. Make your presence points a SpatVector (since they are in lon/lat-like coords)
occurrences1 <- vect(occurrences, 
                    geom = c("lon", "lat"), 
                    crs = "EPSG:4326")

# Convert raster::RasterLayer to terra::SpatRaster
suitability_terra <- rast(suitability)


# Reproject points to raster CRS
occurrences_utm <- terra::project(occurrences1, crs(suitability_terra))

# Extract raster values at reprojected points
presence_values <- terra::extract(suitability_terra, occurrences_utm)

# Extract suitability values at presence locations
presence_values <- terra::extract(suitability_terra, occurrences1)







# Use training/testing split for evaluation
presence_values <- extract(suitability, occurrences)
background_points <- randomPoints(predictors, 500)  # Generate background points
background_values <- extract(suitability, background_points)

# Combine predictions
labels <- c(rep(1, length(presence_values)), rep(0, length(background_values)))
predictions <- c(presence_values, background_values)

# Evaluate model performance using AUC
library(pROC)
roc_curve <- roc(labels, predictions)
plot(roc_curve, main = "ROC Curve for MaxEnt Model")
auc(roc_curve)  # Calculate AUC value
