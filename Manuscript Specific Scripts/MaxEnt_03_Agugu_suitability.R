source("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/functions.R")
Sys.setenv(JAVA_HOME="C:/Program Files/Eclipse Adoptium/jdk-21.0.10.7-hotspot")
Sys.setenv(JAVA_HOME="C:/Program Files/Eclipse Adoptium/jdk-21.0.7.6-hotspot")
library(rJava)  # try loading rJava manually
library(raster)
library(dismo)
library(ENMeval)
library(ENMeval)
library(terra)
library(pROC)

##Read in environmental rasters
predictors_a_subset <- rast("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/predictors_a_subset.tif")


#1. Convert SpatRaster to RasterStack for MaxEnt
predictors_r <- raster::stack(predictors_a_subset)

# 2. Make occurrence points a matrix of coordinates
occ_coords <- st_coordinates(occurrences_a_sf)  # gives X = lon, Y = lat


# Convert sf to matrix in raster CRS
occ_coords <- st_coordinates(st_transform(occurrences_a_sf, crs(predictors_r)))

# Get raster extent
# ext(predictors_r)
# # Or, using terra:
# terra::ext(predictors_r)

# Convert RasterStack to SpatRaster
predictors_r_terra <- rast(predictors_r)

# Reproject occurrences to same CRS as raster
occ_reproj <- st_transform(occurrences_a_sf, crs(predictors_r_terra))

# Convert occurrences to SpatVector
occ_vect <- vect(occ_reproj)

# Extract values
occ_vals <- terra::extract(predictors_r_terra, occ_vect)

##Remove ID column
occ_vals <- subset(occ_vals, select = -1)

head(occ_vals)

##Bring raster names from script 02
vars_keep_vec <- read.csv(file.path(LuOneDir, "variable_names.csv"))
vars_keep_vec <- read.csv("C:/Users/ebamgboye/OneDrive - Loyola University Chicago/Documents/variable_names.csv")

##Remove ID column
vars_keep_vec <- subset(vars_keep_vec, select = -1)

predictor_a_names <- as.character(vars_keep_vec[[1]])

names(occ_vals) <- predictor_a_names
names(predictors_r) <- predictor_a_names


##Replace rows with NA
# For lyr.1 use mean
occ_vals$lyr.1[is.na(occ_vals$lyr.1)] <- 
  mean(occ_vals$lyr.1, na.rm = TRUE)

##For "landuse" use mode
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

occ_vals$landuse_code[is.na(occ_vals$landuse_code)] <- 
  get_mode(occ_vals$landuse_code)

# Remove rows where any predictor is NA
valid_idx <- complete.cases(occ_vals)  # exclude ID column

occ_coords_valid <- occ_coords[valid_idx, ]


##Fix NA values in raster
occ_vals_check <- raster::extract(predictors_r, occ_coords_valid)

bad_idx <- which(!complete.cases(occ_vals_check))
bad_xy  <- occ_coords_valid[bad_idx, ]

cells_bad <- raster::cellFromXY(predictors_r, bad_xy)
cells_bad

# mean of lyr.1 (ignoring NAs)
lyr1      <- predictors_r[["lyr.1"]]
mean_lyr1 <- raster::cellStats(lyr1, stat = "mean", na.rm = TRUE)

# mode of landuse_code (ignoring NAs)
landuse   <- predictors_r[["landuse_code"]]
lu_vals   <- raster::getValues(landuse)
lu_vals   <- lu_vals[!is.na(lu_vals)]

mode_landuse <- as.numeric(names(sort(table(lu_vals), decreasing = TRUE)[1]))
mode_landuse

# fill in lyr.1 with its mean where NA at those cells
lyr1[cells_bad[is.na(lyr1[cells_bad])]] <- mean_lyr1

# fill in landuse_code with its mode where NA at those cells
landuse[cells_bad[is.na(landuse[cells_bad])]] <- mode_landuse

# put layers back into the stack
predictors_r[["lyr.1"]]        <- lyr1
predictors_r[["landuse_code"]] <- landuse



##MOVE TO VERSION 2

# occ_vals_check2 <- raster::extract(predictors_r, occ_vals2)
# which(!complete.cases(occ_vals_check2))   # should be integer(0)


##Wet and Dry season
maxent_modelj <- maxent(
  x       = predictors_r,
  p       = occ_coords_valid,
  factors = "landuse_code"
)

print(maxent_modelj)


# 3. Run MaxEnt for specific seasons
##Split rasters into wet and dry
# Step 1: Identify all layers
layer_names <- names(predictors_r)

# Step 2: Create a data frame with type and year
layers_df <- data.frame(
  index = seq_along(layer_names),
  layer_name = layer_names,
  type = ifelse(grepl("^EVI", layer_names), "EVI",
                ifelse(grepl("^NDWI", layer_names), "NDWI",
                       ifelse(grepl("^NDMI", layer_names), "NDMI", "other"))),
  year = as.numeric(sub(".*_(\\d{4})_.*", "\\1", layer_names))
)
layers_df$year[is.na(layers_df$year)] <- NA  # mark non-vegetation layers

# Step 3: Get the vegetation layers for each season
dry_veg <- layers_df$layer_name[layers_df$year == 2023 & layers_df$type %in% c("EVI","NDWI","NDMI")]
wet_veg <- layers_df$layer_name[layers_df$year == 2024 & layers_df$type %in% c("EVI","NDWI","NDMI")]

# Step 4: Get the non-vegetation layers (to keep in both)
other_layers <- layers_df$layer_name[layers_df$type == "other"]

# Step 5: Create the seasonal stacks
dry_stack <- subset(predictors_r, c(dry_veg, other_layers))
wet_stack <- subset(predictors_r, c(wet_veg, other_layers))

# Step 6: Check
dry_stack
wet_stack


##Wet Season alone
# wet season: first 15 points
occ_wet <- occ_coords_valid[1:15, ]

maxent_wet <- maxent(
  x       = wet_stack,
  p       = occ_wet,
  factors = "landuse_code"
)

# 4. View summary
print(maxent_wet)

# 
# 


# Run MaxEnt with bootstrap replicates

set.seed(123)

maxent_wet <- maxent(
  x       = wet_stack,
  p       = occ_wet,
  factors = "landuse_code",
  args    = c("replicates=10", "replicatetype=bootstrap")
)

# Predictions (average across replicates)
predict_map <- predict(maxent_wet, wet_stack)
plot(predict_map)
print(maxent_wet)
maxent_wet@results

# Example: assuming 5 replicates
contrib_matrix <- maxent_wet@results[grep("contribution", rownames(maxent_wet@results)), ]
avg_contrib <- rowMeans(contrib_matrix)
sd_contrib  <- apply(contrib_matrix, 1, sd)

# Print average contributions
avg_contrib
sd_contrib

# Create a data frame for plotting
plot_maxxwet <- data.frame(
  variable = rownames(contrib_matrix),
  mean     = avg_contrib,
  sd       = sd_contrib
)

# Sort by mean, descending
plot_maxxwet <- plot_maxxwet[order(-plot_maxxwet$mean), ]

# Make variable a factor, ordered by mean contribution
plot_maxxwet$variable <- factor(plot_maxxwet$variable, levels = plot_maxxwet$variable)

# Bar plot with error bars
ggplot(plot_maxxwet, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "red") +
  coord_flip() +
  labs(
    title = "MaxEnt Variable Contributions (Mean ± SD)",
    x = "Environmental Variable",
    y = "Percent Contribution"
  ) +
  theme_minimal(base_size = 14)


##Dry Season alone
# dry season: last 2 points
occ_dry <- occ_coords_valid[16:17, ]

maxent_dry <- maxent(
  x = dry_stack, 
  p = occ_dry,
  factors = "landuse_code")

# 4. View summary
print(maxent_dry)

pred_all <- predict(predictors_r, maxent_modelj)
pred_wet <- predict(wet_stack, maxent_wet)
pred_dry <- predict(dry_stack, maxent_dry)

plot(pred_wet)
plot(pred_dry)
plot(pred_all)


# ---------------------------
# 4. Jackknife test for variable importance
# ---------------------------
# Convert SpatRaster to RasterStack
predictors_r_raster <- raster::stack(predictors_r)
predictors_w_raster <- raster::stack(wet_stack)
predictors_d_raster <- raster::stack(dry_stack)

# Re-run MaxEnt with jackknife
##all seasons
##Rename variable names in raster
names(predictors_r_raster) <- c(
  "EVI_Feb.2023",          # EVI_2023_02
  "EVI_May.2024",          # EVI_2024_05
  "EVI_June.2024",         # EVI_2024_06
  "EVI_July.2024",         # EVI_2024_07
  "NDWI_Jan.2023",         # NDWI_2023_01
  "NDWI_Feb.2023",         # NDWI_2023_02
  "NDWI_May.2024",         # NDWI_2024_05
  "NDWI_June.2024",        # NDWI_2024_06
  "NDWI_July.2024",        # NDWI_2024_07
  "NDMI_Jan.2023",         # NDMI_2023_01
  "NDMI_May.2024",         # NDMI_2024_05
  "NDMI_June.2024",        # NDMI_2024_06
  "NDMI_July.2024",        # NDMI_2024_07
  "Avg.Rad from NTL",      # avg_rad
  "population_density",    # gpw_v4_population_density_rev11_2020_1_deg
  "distance_to_water bodies", # distance2water_30arcsec
  "land surface temperature", # lyr.1
  "landuse"                # landuse_code
)

names(predictors_r_raster)


##complete Jackknife analysis here(17th March, 2026)
jka <- dismo::maxent(wet_stack, occ_wet, args = c("jackknife=true"))
# Base R jackknife plot
plot(jka, type = "jackknife")
# View results
print(jka)


##Wet season
##Rename variable names
names(predictors_w_raster) <- c(
 "EVI_May.2024", "EVI_June.2024", "EVI_July.2024",
 "NDWI_May.2024", "NDWI_June.2024","NDWI_July.2024",  
 "NDMI_May.2024", "NDMI_June.2024","NDMI_July.2024",
 "Avg_Rad. from NTL", "population_density", "Distance2water_bodies",
 "land surface temp.", "landuse" 
)

set.seed(123)

jkaw <- dismo::maxent(
  x = predictors_w_raster, 
  p = occ_wet, 
 args    = c(
             "jackknife=true",          # keep jackknife plots
             "replicates=5",            # number of bootstrap replicates
             "replicatetype=bootstrap"  # type of replicate resampling
           )
  )


# Extract all rows containing contributions
jkaw_contrib_matrix <- jkaw@results[grep("contribution", rownames(jkaw@results)), ]

# Compute mean and SD across replicates
jkaw_mean_contrib <- rowMeans(jkaw_contrib_matrix)
jkaw_sd_contrib   <- apply(jkaw_contrib_matrix, 1, sd)

# Create data frame for plotting
jkaw_plot_data <- data.frame(
  variable = rownames(jkaw_contrib_matrix),
  mean     = jkaw_mean_contrib,
  sd       = jkaw_sd_contrib
)

# Order variables by mean contribution
jkaw_plot_data$variable <- factor(jkaw_plot_data$variable, levels = jkaw_plot_data$variable[order(jkaw_plot_data$mean, decreasing = TRUE)])

pdf("Agugu bootstraped VariableContribution plot.pdf", width = 12, height = 6)

ggplot(jkaw_plot_data, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "red") +
  coord_flip() +
  labs(title = "MaxEnt Variable Contributions (Bootstrap Mean ± SD)",
       x = "Variable",
       y = "Percent Contribution") +
  theme_minimal(base_size = 14)

dev.off()



# Base R jackknife plot
plot(jkaw, type = "jackknife")
# View results
print(jkaw)


##Dry season
##Rename variables in raster
names(predictors_d_raster) <- c(
  "EVI_Feb.2023",
  "NDWI_Jan.2023",
  "NDWI_Feb.2023",
  "NDMI_Jan.2023",
  "Avg.Rad from NTL",
  "population_density",
  "distance_to_water bodies",
  "land surface temperature",
  "landuse"
)

jkad <- dismo::maxent(predictors_d_raster, occ_dry, args = c("jackknife=true"))
# Base R jackknife plot
plot(jkad, type = "jackknife")
# View results
print(jkad)


##Evaluating Direction of effect 
# Open PDF device
pdf("Agugu jackknife_responses.pdf", width = 12, height = 6)

par(mar = c(4, 4, 2, 1))  # bottom, left, top, right
response(jka)

response(jkaw)
response(jkaw@models[[1]])

response(jkad)

dev.off()

##Plot Variable contribution into pdf
pdf("Agugu jackknife_variable contribution.pdf", width = 12, height = 8)

par(mar = c(4, 4, 2, 1))  # bottom, left, top, right

plot(jka, type = "jackknife")
plot(jkaw, type = "jackknife")
plot(jkad, type = "jackknife")

dev.off()

##Combine all in a single pdf
pdf("Agugu jackknife_analysis result.pdf", width = 12, height = 7)
par(mar = c(4, 4, 2, 1))  # bottom, left, top, right

plot(jka, type = "jackknife")
response(jka)
plot(jkaw, type = "jackknife")
response(jkaw)
plot(jkad, type = "jackknife")
response(jkad)

dev.off()



# Extract numeric results
jk_resultsa <- jkaw@results
head(jk_resultsa)

jk_dfa <- jk_resultsa[grep("jackknife", rownames(jk_resultsa)), , drop = FALSE]

# Convert results into a dataframe
resa <- as.data.frame(jk_resultsa)

# Extract variable names (they appear in the result names)
varsa <- gsub("\\.contribution.*", "", grep("\\.contribution", rownames(resa), value = TRUE))

# Build a tidy dataframe of jackknife results
jk_dfa <- lapply(varsa, function(v) {
  data.frame(
    variable = v,
    with_only = as.numeric(resa[paste0("Training.gain.with.only.", v), 1]),
    without   = as.numeric(resa[paste0("Training.gain.without.", v), 1])
  )
}) %>%
  bind_rows()

# Add the "with all variables" value (same for all)
jk_dfa <- jk_dfa %>%
  mutate(all_varsa = as.numeric(resa["Regularized.training.gain", 1]))

#Rename variable names
jk_dfa <- jk_dfa %>%
  mutate(variable = recode(variable,
                           "EVI.2"  = "EVI(June 2024)",
                           "EVI.3"  = "EVI(July 2024)",
                           "NDMI.1" = "NDMI (May 2024)",
                           "NDMI.2" = "NDMI (June 2024)",
                           "NDMI.3" = "NDMI (July 2024)",
                           "NDWI.1" = "NDWI (May 2024)",
                           "NDWI.2" = "NDWI (June 2024)",
                           "NDWI.3" = "NDWI (July 2024)",
                           "angle_mean" = "Mean Angle",
                           "avg_rad" = "Avg. Radiance from NTL",
                           "distance2water_30arcsec" = "Distance to Water",
                           "gpw_v4_population_density_rev11_2020_1_deg" = "Population Density",
                           "landuse_code" = "Land Use",
                           "log_area" = "Building Area",
                           "lyr.1" = "Land Surface Temperature",
                           "nndist_mean" = "Mean Nearest Neighbor Distance",
                           "shape_mean" = "Mean Shape"))

# Reshape to long format for ggplot
jk_longa <- jk_dfa %>%
  pivot_longer(cols = c("with_only", "without", "all_varsa"),
               names_to = "condition",
               values_to = "gain")

# Plot jackknife bar chart
# ggplot(jk_longa, aes(x = variable, y = gain, fill = condition)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   scale_fill_manual(values = c("with_only" = "#1f78b4",   # blue
#                                "without"  = "#e31a1c",   # red
#                                "all_varsa" = "#33a02c")) + # green
#   labs(title = "MaxEnt Jackknife Test of Variable Importance",
#        x = "Environmental Variable",
#        y = "Training Gain",
#        fill = "Condition") +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 10))

# Plot
ggplot(jk_longa, aes(x = gain, y = variable, fill = condition)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("without" = "skyblue",
                               "with_only" = "blue",
                               "all_varsa" = "red")) +
  labs(x = "regularized training gain",
       y = "Environmental Variable",
       title = "Jackknife of regularized training gain for species") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right",
        axis.text.y = element_text(size = 10))





##Another Plot
library(ggplot2)
library(dplyr)
library(tidyr)

# Assume jk_long is your long-format jackknife results with columns:
# Variable, Condition (with_only / without / all_vars), Gain

# Separate out "all_vars"
jk_sepa <- jk_longa %>% 
  filter(condition != "all_vars")

jk_alla <- jk_longa %>% 
  filter(condition == "all_vars")

# Plot
jk_agugu <- ggplot(jk_sepa, aes(x = gain, y = variable, fill = condition)) +
  geom_col(position = "dodge") +  # side-by-side bars
  geom_col(data = jk_alla, aes(x = gain, y = variable), 
           fill = "aliceblue", width = 0.5, inherit.aes = FALSE) +
  labs(title = "MaxEnt Jackknife Test of Variable Importance",
       x = "Training Gain", y = "Environmental Variable") +
  scale_fill_manual(values = c("with_only" = "lightgreen",
                               "without" = "red",
                               "all_varsa" = "aliceblue")) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/", 'Jack n Knife Test for Agugu.pdf'), jk_agugu , width = 11, height = 10)


#Replot for Manuscript
# Reorder variables by 'with_only' gain
jk_sepa <- jk_sepa %>% filter(condition !="all_varsa") %>% 
  group_by(variable) %>%
  mutate(max_with_only = ifelse(condition == "with_only", gain, NA)) %>%
  ungroup() %>%
  group_by(variable) %>%
  fill(max_with_only, .direction = "downup") %>%
  ungroup() %>%
  mutate(variable = fct_reorder(variable, max_with_only, .desc = TRUE))

# Then plot
jk_agugu <- ggplot(jk_sepa, aes(x = gain, y = variable, fill = condition)) +
  geom_col(position = "dodge") +
  #geom_col(data = jk_alla, aes(x = gain, y = variable),  
          # fill = "aliceblue", width = 0.5, inherit.aes = FALSE) +
  labs(title = "MaxEnt Jackknife Test of Variable Importance (Agugu)",
       x = "Training Gain", y = "Environmental Variable") +
  scale_fill_manual(values = c("with_only" = "lightgreen",
                               "without" = "plum")) +
  theme_manuscript() +
  theme(legend.position = "bottom")


ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/", 'Jack n Knife Test for Agugu.pdf'), jk_agugu , width =8, height = 11)

##Evaluating Direction of effect 
# Open PDF device
pdf("Agugu jackknife_responses.pdf", width = 11, height = 16)

response(jkaw, variables = "Distance to Water")

response(jkaw, variables = "Land Surface Temperature")

response(jkaw, variables = "NDWI.3")

response(jkad, variables = "avg_rad")


# Close PDF device
dev.off()

#Open PDF automatically
shell.exec("Agugu jackknife_responses.pdf")



















##------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

###-----------------------------------------------------------------------------
# Predict across study area
###-----------------------------------------------------------------------------

suitability <- predict(predictors_r, maxent_model)

crs(suitability) <- "EPSG:32631"

# Plot results
plot(suitability, main="Habitat Suitability")
points(occurrences_in_agugu, col="red", pch=20)
#plot(ward_vect, border="blue", add=TRUE)


##Plot using ggplot2

# Convert raster to dataframe for ggplot
suitability_df <- as.data.frame(rasterToPoints(suitability))
colnames(suitability_df) <- c("x", "y", "suitability")

# Make sure your points are sf objects
# occurrences_sf -> presence points
# absent_sites_in_agugu -> absence points

# ggplot() +
#   #geom_sf(data = df_ib_a, fill = NA, color = "white")+
#   geom_raster(data = suitability_dfc, aes(x = x, y = y, fill = suitability)) +
#   scale_fill_viridis_c(name = "Suitability") +  # nicer color scale
#   geom_sf(data = occurrences_sf, color = "red", size = 2) +
#   geom_sf(data = absent_sites_in_agugu, color = "blue", size = 2) +
#   labs(title = "Habitat Suitability") +
#   theme_minimal() +
#   coord_sf()


# Reproject shapefile and points to match raster
df_ib_a_utm <- st_transform(df_ib_a, crs(suitability))
occurrences_utm <- st_transform(occurrences_a_sf, crs(suitability))

#absent_sites_utm <- st_transform(absent_sites_in_agugu, crs(suitability))

# Plot again
ggplot() +
  geom_raster(data = suitability_df, aes(x = x, y = y, fill = suitability)) +
  scale_fill_viridis_c(name = "Suitability") +
  geom_sf(data = df_ib_a_utm, fill = NA, color = "grey", size = 0.6) +
  geom_sf(data = occurrences_utm, color = "red", size = 2) +
  #geom_sf(data = absent_sites_utm, color = "blue", size = 2) +
  labs(title = " Agugu Habitat Suitability") +
  theme_minimal() +
  coord_sf()


##Plot only within ward shape file
# Make sure raster and ward have the same CRS

raster_crs <- st_crs(suitability)

ward_vect_utm <- st_transform(df_ib_a, crs = raster_crs)
library(terra)

# Convert RasterLayer to SpatRaster
suitability_terra <- rast(suitability)

# Mask with SpatVector
suitability_mask <- mask(suitability_terra, ward_vect_utm)

# Convert to dataframe for ggplot
suit_df <- as.data.frame(suitability_mask, xy = TRUE)
names(suit_df)[3] <- "Suitability"

# Plot clipped map
habsuita <- ggplot() +
  geom_raster(data = suit_df, aes(x = x, y = y, fill = Suitability)) +
  scale_fill_viridis_c(option = "plasma") +
  geom_sf(data = st_as_sf(ward_vect_utm), fill = NA, color = "black", size = 0.5) +
  #geom_sf(data = occurrences_utm, color = "green", size = 2.5) +
  #geom_sf(data = hh_pos_a_df, color = "black", size = 1) +
  theme_manuscript() +
  labs(title = "Habitat Suitability Agugu", fill = "Suitability")

ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/", 'Habitat Suitability for Challenge.pdf'), habsuita, width = 11, height = 10)


#Convert suitability plot to categories

# Example: load raster
suitability_rasta <- suitability

# Create categories
library(classInt)
fisher_breaksa <- classIntervals(suit_df$Suitability, n = 4, style = "fisher")$brks
fisher_breaksa

ma <- matrix(c(-Inf, fisher_breaksa[2], 0,
              fisher_breaksa[2], fisher_breaksa[3], 1,
              fisher_breaksa[3], fisher_breaksa[4], 2,
              fisher_breaksa[4], Inf, 3),
            ncol = 3, byrow = TRUE)


suitability_cat <- reclassify(suitability_rasta, ma)


# # Define colors for categories
# cat_colors <- c("lightgreen","lightblue", "yellow", "red")  # very low, low, medium, high
# 
# # Plot raster with specified colors
# plot(suitability_catc, 
#      col = cat_colors,
#      legend = TRUE,
#      main = "Suitability Categories (0 = Very Low,1 = Low, 2 = Medium, 3 = High)")

#Replot raster with ggplot
# 1. Crop/mask raster to shapefile extent
suitability_masked <- mask(suitability_cat, df_ib_a_utm)

# 2. Convert raster to dataframe
suit_dfcat <- as.data.frame(suitability_masked, xy = TRUE)
colnames(suit_dfcat)[3] <- "class"

# 3. Attach class labels (0–3 → categorical names)
suit_dfcat$class <- factor(suit_dfcat$class,
                            levels = 0:3,
                            labels = c("Very Low", "Low", "Medium", "High"))

suit_dfcat <- suit_dfcat %>%
  filter(!is.na(class))

# 4. Plot with ggplot2
habsuitcata <- ggplot() +
  geom_raster(data =suit_dfcat, aes(x = x, y = y, fill = class)) +
  geom_sf(data = st_as_sf(ward_vect_utm), fill = NA, color = "black", size = 0.5) +
  coord_sf () +
  scale_fill_manual(values = c(
    "Very Low" = "#d9f0a3",  # light green
    "Low" = "#addd8e",       # medium green
    "Medium" = "#fee08b",    # yellow
    "High" = "#fc8d59"       # orange-red
  )) +
  labs(title = "Suitability Categories",
       fill = "Suitability") +
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/", 'Habitat Suitability Categories for Agugu.pdf'), habsuitcata, width = 11, height = 10)


#Suitability boxplot and co
# Extract values from the raster
suit_valuesa <- raster::extract(suitability_rasta, 1:ncell(suitability_rasta))
suit_valuesa <- data.frame(value = suit_valuesa)


# Remove NAs first
suit_valuesa_clean <- suit_valuesa %>%
  filter(!is.na(value))

quantile(suit_valuesa_clean$value, probs = c(0.25, 0.5, 0.75, 0.8, 0.9), na.rm = TRUE)


# Categorize suitability
suit_summarya <- suit_valuesa_clean %>%
  mutate(
    suitability_category = ifelse(value > 0.80, "Highly suitable", "Not highly suitable")
  ) %>%
  summarise(
    total = n(),
    highly_suitable = sum(suitability_category == "Highly suitable"),
    proportion_highly_suitable = highly_suitable / total
  )

suit_summarya
