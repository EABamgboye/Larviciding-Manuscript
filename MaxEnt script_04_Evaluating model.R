# ===============================
# MaxEnt Modeling with Presence and Absence Points
# Using terra and dismo
# ===============================

# Load libraries
library(dismo)
library(terra)
library(sp)
library(caret)

# -------------------------------
# 1. Load data
# -------------------------------

# Raster predictors (RasterStack or SpatRaster)
predictors_r <- predictors_r

# Presence points 
presence_data <- occurrences_in_agugu

##Convert to lon, lat
presence_data<- presence_data %>%
  dplyr::select(geometry) %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) %>%
  st_drop_geometry() %>%
  dplyr::select(lon, lat)

# Ensure they're numeric
presence_data$lon <- as.numeric(presence_data$lon)
presence_data$lat <- as.numeric(presence_data$lat)

# Convert sf to SpatialPoints
presence_sp <- as_Spatial(occurrences_in_agugu)

# Absence/background points (data.frame with lon, lat)
absence_data <- absent_sites_in_agugu

##Convert to lon, lat
absence_data<- absence_data %>%
  dplyr::select(geometry) %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) %>%
  st_drop_geometry() %>%
  dplyr::select(lon, lat)

# Ensure they're numeric
absence_data$lon <- as.numeric(absence_data$lon)
absence_data$lat <- as.numeric(absence_data$lat)

# Convert to SpatialPoints
presence_sp <- SpatialPoints(presence_data[, c("lon","lat")],
                             proj4string = CRS("+proj=longlat +datum=WGS84"))

absence_sp <- SpatialPoints(
  as.matrix(absence_data[, c("lon", "lat")]),
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# ------------------------------
# 4. Align CRS
# ------------------------------
# Transform presence and absence points to raster CRS
presence_sp_utm <- spTransform(presence_sp, CRS(proj4string(predictors_r)))
absence_sp_utm  <- spTransform(absence_sp, CRS(proj4string(predictors_r)))



###3.  
# Convert SpatialPoints to sf
presence_sf <- st_as_sf(presence_sp_utm)

# Create 500 m buffer
presence_buf <- st_buffer(presence_sf, dist = 500)

# Convert raster to terra SpatRaster if not already
predictors_terra <- rast(predictors_r)

# Crop raster to buffer extent
predictors_crop <- crop(predictors_terra, vect(presence_buf))
predictors_crop <- mask(predictors_crop, vect(presence_buf))

#3COnvert back to RasterStack
predictors_raster <- raster::stack(predictors_crop)

# ----------------------------
# 2. Fit MaxEnt with replicates
# ----------------------------
maxent_model <- maxent(
  x = predictors_raster,
  p = presence_sp_utm,
  a = absence_sp_utm,
  args = c("replicates=5", "replicatetype=crossvalidate")
)

# ----------------------------
# 3. Predict each replicate and average
# ----------------------------
pred_full <- raster::predict(predictors_raster, maxent_model@models[[1]])

pred_list <- list()
for (i in seq_along(maxent_model@models)) {
  # Predict each replicate
  pred_i <- raster::predict(predictors_raster, maxent_model@models[[i]])
  
  # Force as RasterLayer
  if (!inherits(pred_i, "RasterLayer")) stop("Prediction is not RasterLayer")
  
  pred_list[[i]] <- pred_i
}

# Remove NULLs or invalid layers
pred_list <- Filter(function(x) inherits(x, "RasterLayer"), pred_list)

# Stack
pred_stack <- stack(pred_list)


suitability_mean <- raster::stackApply(pred_stack,
                                       indices = rep(1, nlayers(pred_stack)),
                                       fun = base::mean)


threshold_val <- 0.5
suitability_binary <- suitability_mean > threshold_val

par(mfrow=c(1,2))
plot(suitability_mean, main="Mean MaxEnt Suitability")
plot(suitability_binary, main="Binary Presence/Absence Map")

# Convert raster to dataframe for ggplot
suitability_dfm <- as.data.frame(rasterToPoints(suitability_mean))
colnames(suitability_dfm) <- c("x", "y", "suitability")

# Reproject shapefile and points to match raster
df_ib_a_utm <- st_transform(df_ib_a, crs(suitability_mean))
occurrences_utm <- st_transform(occurrences_sf, crs(suitability_mean))
absent_sites_utm <- st_transform(absent_sites_in_agugu, crs(suitability_mean))

# Plot again
ggplot() +
  geom_raster(data = suitability_dfm, aes(x = x, y = y, fill = suitability)) +
  scale_fill_viridis_c(name = "Suitability") +
  geom_sf(data = df_ib_a_utm, fill = NA, color = "white", size = 0.6) +
  geom_sf(data = occurrences_utm, color = "red", size = 2) +
  geom_sf(data = absent_sites_utm, color = "blue", size = 2) +
  labs(title = "Habitat Suitability") +
  theme_minimal() +
  coord_sf()



##Model Evaluation using ROC
# Use training/testing split for evaluation
presence_values <- raster::extract(suitability, occurrences)
background_points <- randomPoints(predictors_r, 500)  # Generate background points
background_values <- raster::extract(suitability, background_points)

# Combine predictions
labels <- c(rep(1, length(presence_values)), rep(0, length(background_values)))
predictions <- c(presence_values, background_values)

# Evaluate model performance using AUC
library(pROC)
roc_curvea <- roc(labels, predictions)
plot(roc_curvea, main = "ROC Curve for MaxEnt Model Agugu")
auc_valuea <- auc(roc_curvea)  # Calculate AUC value
legend("bottomright", legend = paste("AUC =", round(auc_valuea, 3)), bty = "n")

# Convert ROC curve to a data frame
roc_df <- data.frame(
  specificity = rev(roc_curvea$specificities),
  sensitivity = rev(roc_curvea$sensitivities)
)

# Extract AUC
auc_value <- auc(roc_curvea)

# Plot with ggplot2
agroc <- ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "#1f78b4", size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey40") +
  labs(title = "ROC Curve for MaxEnt Model (Agugu)",
       subtitle = paste("AUC =", round(auc_value, 3)),
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/", 'Agugu ROC curve.pdf'), agroc, width = 11, height = 10)



# Perform k-fold cross-validation
library(sp)
presence_sp <- SpatialPoints(occurrences[, c("lon", "lat")], 
                             proj4string = CRS("+proj=longlat +datum=WGS84"))

maxent_model_cv <- dismo::maxent(x = predictors_r, p = presence_sp, args="replicates=5")

# Predict across study area
# Create a raster stack to store predictions
pred_stack <- stack()

for(i in 1:length(maxent_model_cv@models)) {
  pred_stack <- stack(pred_stack, predict(predictors_r, maxent_model_cv@models[[i]]))
}

pred_stack_terra <- rast(pred_stack) 

# Average across replicates
suitability_cv <- app(pred_stack_terra, fun = base::mean)

plot(suitability_cv)


##Evaluate with AIC
library(ENMeval)
library(raster)

library(ENMeval)

# Occurrences as matrix or data.frame
occ_matrix <- as.data.frame(occurrences[, c("lon", "lat")])

# Background points (optional, v2 can generate automatically)
bg_points <- NULL  # let ENMeval sample background if not provided

# Run ENMevaluate
eval <- ENMevaluate(
  occ = occ_matrix,      # presence points
  env = predictors_r,    # raster stack (RasterStack or SpatRaster)
  method = "block",      # spatial partitioning
  algorithm = "maxnet",  # algorithm
  parallel = TRUE        # enable parallel processing
)

eval <- ENMevaluate(
  occs = occ_matrix,        # presence points
  env = predictors_r,       # RasterStack or SpatRaster
  bg.coords = NULL,         # optional background points; NULL lets ENMeval generate them
  algorithm = "maxnet",     # model algorithm
  partition.type = "block", # <- replaces 'method'
  parallel = TRUE           # enable parallel processing
)

# View results including AICc
eval@results


# Check AIC results
eval@results  # Contains AICc, deltaAICc, and other metrics


# View AICc for each model
eval@results$AICc


##Evaluating with Uncertainties and confidence limits
# install.packages(c("ENMeval","terra","sf","maxnet"))  # maxnet backend is used by ENMeval v2+
library(ENMeval)
library(terra)
library(sf)

# Convert your RasterStack to SpatRaster if needed
predictors_r <- terra::rast(predictors_r)   # ensure it's a SpatRaster

# Convert land use to factor
predictors_r$landuse_code <- as.factor(predictors_r$landuse_code)

# Make sure occurrences are in matrix form
occs_mat <- sf::st_coordinates(occurrences_in_agugu)

# Convert occurrence points to an sf object
occs_sf <- st_as_sf(data.frame(occs_mat), coords = c("X","Y"), crs = 4326)  # WGS84

# Project to UTM zone 31N (same as your raster)
occs_utm <- st_transform(occs_sf, crs(predictors_r))

# Convert back to matrix for ENMevaluate
occs_clean <- st_coordinates(occs_utm)


# Create a background matrix (all raster cells)
bg_cells <- terra::as.data.frame(predictors_r, xy = TRUE, cells = FALSE)
bg_coords <- bg_cells[, c("x","y")]

# Rename bg columns to match occs
colnames(bg_coords) <- colnames(occs_mat)


library(terra)

# Extract predictor values for occurrences
# occ_vals <- terra::extract(predictors_r, occs_mat)
# keep_occ <- complete.cases(occ_vals)
# occs_clean <- occs_mat[keep_occ, ]

# Extract predictor values for background
bg_vals <- terra::extract(predictors_r, bg_coords)
keep_bg <- complete.cases(bg_vals)
bg_clean <- bg_coords[keep_bg, ]

library(ENMeval)

en_a <- ENMevaluate(
  occs = occs_clean,
  env = predictors_r,
  bg = bg_clean,
  algorithm = "maxnet",
  partitions = "randomkfold",
  categoricals = "landuse_code",
  parallel = FALSE,
  tune.args = list(
    fc = c("L","LQ","H","LQH"),
    rm = c(0.5, 1, 2)
  )
)

#Explore the file(en_a)
en_a@results

# Check which model had the best AICc
en_a@results[which.min(en_a@results$AICc), ]

plot(en_a, "jackknife")


# ENMevaluate output object contains predictions for each run.
# Extract raster predictions: en@results or en@predictions depending on version
# For ENMeval v2, use en@predictions (list of rasters)
pred_list <- en_a@predictions  # may be a list of SpatRaster objects or Raster*; check structure

# If predictions are returned as a list of SpatRaster or RasterStack per run, convert to terra SpatRaster list
library(terra)

# If you want a list with one raster
rep_rasters <- list(pred_list[[1]])

# # Flatten into a list of SpatRasters (one per replicate)
# rep_rasters <- list()
# i <- 1
# for (p in pred_list) {
#   # p might be a RasterLayerStack with single layer or a list; convert safely:
#   pr <- rast(p)            # safe conversion from Raster* or list
#   # If pr has multiple layers (e.g. one per model variant), pick the layer you want.
#   # Here we assume a single layer per replicate.
#   rep_rasters[[i]] <- pr[[1]]
#   i <- i + 1
# }

# Stack them
s <- rast(rep_rasters)     # SpatRaster with layers = replicates

# Define your own mean function
my_mean <- function(x) {
  mean(x, na.rm = TRUE)   # na.rm = TRUE ignores NA values
}

# Compute mean and SD per cell
mean_r <- app(pred_list, fun = my_mean, cores = 1)
sd_r   <- app(pred_list, fun = sd,   cores = 1)

# 95% CI by mean ± 1.96*sd (approximate)
ci_lower_approx <- mean_r - 1.96 * sd_r
ci_upper_approx <- mean_r + 1.96 * sd_r

# Define a function that returns the lower 2.5% quantile for a single cell across layers
ci_lower_fun <- function(x) {
  quantile(x, probs = 0.025, na.rm = TRUE)[[1]]  # select the first element
}

ci_upper_fun <- function(x) {
  quantile(x, probs = 0.975, na.rm = TRUE)[[1]]  # select the first element
}

# Better: percentile-based CI (empirical 2.5% and 97.5%)
ci_lower <- app(pred_list, fun = ci_lower_fun)

ci_upper <- app(pred_list, fun = ci_upper_fun)

# Save outputs
writeRaster(mean_r, "maxent_mean.tif", overwrite = TRUE)
writeRaster(sd_r,   "maxent_sd.tif",   overwrite = TRUE)
writeRaster(ci_lower, "maxent_CI025.tif", overwrite = TRUE)
writeRaster(ci_upper, "maxent_CI975.tif", overwrite = TRUE)

# Quick plots
plot(mean_r, main = "Mean suitability")
plot(sd_r, main = "SD of suitability (replicates)")
plot(ci_upper, main = "97.5% percentile (upper)")
plot(ci_lower, main = "25.0% percentile (lower)")

library(terra)

# Stack rasters
r_stack <- c(mean_r, sd_r, ci_upper, ci_lower)
names(r_stack) <- c("Mean", "SD", "Upper 97.5%", "Lower 2.5%")

# Plot all in one figure
plot(r_stack)


library(terra)
library(ggplot2)
library(gridExtra)

# Convert raster to data.frame
r_to_df <- function(r) as.data.frame(r, xy = TRUE)

df_mean <- r_to_df(mean_r)
df_sd <- r_to_df(sd_r)
df_upper <- r_to_df(ci_upper)
df_lower <- r_to_df(ci_lower)

# Make ggplots
p1 <- ggplot(df_mean, aes(x=x, y=y, fill=lyr.1)) + geom_raster() + coord_fixed() + labs(title="Mean")
p2 <- ggplot(df_sd, aes(x=x, y=y, fill=sd)) + geom_raster() + coord_fixed() + labs(title="SD")
p3 <- ggplot(df_upper, aes(x=x, y=y, fill=lyr.1)) + geom_raster() + coord_fixed() + labs(title="97.5%")
p4 <- ggplot(df_lower, aes(x=x, y=y, fill=lyr.1)) + geom_raster() + coord_fixed() + labs(title="2.5%")

# Arrange plots
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol=2)

# Save
ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/Agugu_Suitability_plots.pdf"), combined_plot, width = 11, height = 10)

library(ggplot2)
library(gridExtra)

# Define a color palette
pal <- c("#f7fcf5","#c7e9c0","#41ab5d","#006d2c")  # light to dark green
pal_sd <- c("#f7f7f7","#cccccc","#969696","#252525") # grey scale for SD
# New color palette: purple to yellow
pal <- c("#440154", "#3b528b", "#21918c", "#5ec962", "#fde725")  
pal <- c("#fee0b6", "#fdae6b", "#e6550d", "#9e0168")
pal <- c("#1b9e77", "#66c2a5", "#fbb4b9", "#d53e4f")




# Plot Mean suitability
p1 <- ggplot(df_mean, aes(x=x, y=y, fill=lyr.1)) +
  geom_raster(color="black", size=0.1) +   # black outlines
  scale_fill_gradientn(colors = pal, na.value = "transparent") +
  coord_fixed() +
  labs(title="Mean suitability") +
  theme_minimal() +
  theme(axis.text=element_blank(), axis.title=element_blank())

# Plot SD of suitability
p2 <- ggplot(df_sd, aes(x=x, y=y, fill= sd)) +
  geom_raster(color="black", size=0.1) +
  scale_fill_gradientn(colors = pal_sd, na.value = "transparent") +
  coord_fixed() +
  labs(title="SD of suitability") +
  theme_minimal() +
  theme(axis.text=element_blank(), axis.title=element_blank())

# Plot 97.5% percentile
p3 <- ggplot(df_upper, aes(x=x, y=y, fill=lyr.1)) +
  geom_raster(color="black", size=0.1) +
  scale_fill_gradientn(colors = pal, na.value = "transparent") +
  coord_fixed() +
  labs(title="97.5% percentile") +
  theme_minimal() +
  theme(axis.text=element_blank(), axis.title=element_blank())

# Plot 2.5% percentile
p4 <- ggplot(df_lower, aes(x=x, y=y, fill=lyr.1)) +
  geom_raster(color="black", size=0.1) +
  scale_fill_gradientn(colors = pal, na.value = "transparent") +
  coord_fixed() +
  labs(title="2.5% percentile") +
  theme_minimal() +
  theme(axis.text=element_blank(), axis.title=element_blank())

# Arrange plots side by side (2x2)
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol=2)

# Save to PDF
ggsave("Agugu_Suitability_plots.pdf", combined_plot, width=11, height=10)

ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/", "Agugu_Suitability_plots.pdf"), combined_plot, width = 11, height = 10)
