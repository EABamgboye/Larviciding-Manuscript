Sys.setenv(JAVA_HOME="C:/Program Files/Eclipse Adoptium/jdk-21.0.7.6-hotspot")
library(rJava)  # try loading rJava manually


#1. Convert SpatRaster to RasterStack for MaxEnt
predictors_r <- raster::stack(all_env_vars)

# 2. Make occurrence points a matrix of coordinates
occ_coords <- st_coordinates(occurrences_in_agugu)  # gives X = lon, Y = lat


# Convert sf to matrix in raster CRS
occ_coords <- st_coordinates(st_transform(occurrences_in_agugu, crs(predictors_r)))

# Get raster extent
ext(predictors_r)
# Or, using terra:
terra::ext(predictors_r)

# Convert RasterStack to SpatRaster
predictors_r_terra <- rast(predictors_r)

# Reproject occurrences to same CRS as raster
occ_reproj <- st_transform(occurrences_in_agugu, crs(predictors_r_terra))

# Convert occurrences to SpatVector
occ_vect <- vect(occ_reproj)

# Extract values
occ_vals <- terra::extract(predictors_r_terra, occ_vect)

head(occ_vals)


# Remove rows where any predictor is NA
valid_idx <- complete.cases(occ_vals[, -1])  # exclude ID column
occ_coords_valid <- occ_coords[valid_idx, ]


# 3. Run MaxEnt
maxent_model <- maxent(
  x = predictors_r, 
  p = occ_coords_valid,
  factors = "landuse_code")

# 4. View summary
print(maxent_model)

# ---------------------------
# 4. Jackknife test for variable importance
# ---------------------------
# Convert SpatRaster to RasterStack
predictors_r_raster <- raster::stack(predictors_r)

# Re-run MaxEnt with jackknife
jka <- dismo::maxent(predictors_r_raster, occ_coords_valid, args = c("jackknife=true"))

# View results
print(jka)

# # Re-run MaxEnt with jackknife enabled
# jka <- maxent(predictors_r, occ_coords_valid, args = c("jackknife=true"))
# 
# # Base R jackknife plot
# plot(jka, type = "jackknife")

# Extract numeric results
jk_resultsa <- jka@results
head(jk_resultsa)

jk_dfa <- jka[grep("jackknife", rownames(jka)), , drop = FALSE]

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

response(jka, var = "distance2water_30arcsec")

response(jka, var = "NDWI.2")

response(jka, var = "NDWI.3")

response(jka, var = "avg_rad")


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
occurrences_utm <- st_transform(occurrences_in_agugu, crs(suitability))

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
