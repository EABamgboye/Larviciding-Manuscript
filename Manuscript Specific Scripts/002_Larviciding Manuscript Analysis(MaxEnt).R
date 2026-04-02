#loadpath
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
shapefileDir <- "C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan"
Entodir <- "C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento"
Lavplotsdir <- "C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/projects/Manuscripts/ongoing/Larviciding Manuscript/New Manuscript Sections"

##load packages and themes
source("functions.R")

##load extra packages
library(dismo) 
library(raster)
library(maps)  
library(terra)
library(sf)
library(dismo)
library(randomForest)
library(gbm)
library(caret)


##Need to install Eclipse Adoptium to run MaxEnt modelling
Sys.setenv(JAVA_HOME = (file.path(Entodir, "Eclipse Adoptium", "jdk-21.0.10.7-hotspot")))
library(rJava)


##MaxEnt 01- Retrieve occurence points, visualize and ensure they fit into shapefile
#Load occurence data
agugu_lav_data_wet <- read.csv(file.path(Entodir, "Wet Season Data_Ibadan", "lav_coords_bsw1.csv"))
agugu_lav_data_dry <- read.csv(file.path(Entodir, "lav_coords_bs1.csv"))

##Add season column
agugu_lav_data_wet$season <- "Wet"
agugu_lav_data_dry$season <- "Dry"

##Rename column names before merging
agugu_lav_data_wet <- agugu_lav_data_wet %>%
  rename(
    bs_label = bs_labelw,
    anoph    = anophw
  )

agugu_lav_data_dry <- agugu_lav_data_dry %>%
  rename(
    anoph = anophd
  )

agugu_lav_data <- rbind(agugu_lav_data_wet, agugu_lav_data_dry)

ag_anopheles_sites <- agugu_lav_data %>% 
  dplyr::filter(anoph == "Yes")

# Keep only latitude and longitude columns and rename
occurrences_in_agugu <- ag_anopheles_sites[, 2:3]
colnames(occurrences_in_agugu) <- c("lon", "lat")


# Ensure they're numeric
occurrences_in_agugu$lon <- as.numeric(occurrences_in_agugu$lon)
occurrences_in_agugu$lat <- as.numeric(occurrences_in_agugu$lat)

##Ensure points fall within agugu ward
# Step 1: Convert to sf object 
occurrences_a_sf <- st_as_sf(occurrences_in_agugu, coords = c("lon", "lat"), crs = 4326)

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
    title = "Positive Breeding Sites in Agugu(Dry and Wet Season",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  coord_sf()


##MaxEnt 02-Environmental Variable extractions
##Here is the output environmental stack of predictor variables
##This is run in the script titled: "MaxEnt script_02_Env_covariates_Agugu.R"
predictors_a_subset <- rast("C:/Users/ebamgboye/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/predictors_a_subset.tif")

##Bring raster names from script 02
vars_keep_vec <- read.csv(file.path(Entodir, "variable_names.csv"))

##Remove ID column
vars_keep_vec <- subset(vars_keep_vec, select = -1)

predictor_a_names <- as.character(vars_keep_vec[[1]])

names(predictors_a_subset) <- predictor_a_names

##Visualize some layers of raster
ag_vect <- vect(df_ib_a)  # convert sf → SpatVector
ag_vect_utm <- project(ag_vect, predictors_a_subset)

##Plot in panels
panel_par <- par(mfrow = c(2, 2))
layers <- c(8, 9, 17, 18)
for (i in layers) {
  plot(predictors_a_subset[[i]],
       col = hcl.colors(100, "Viridis"),
       main = names(predictors_a_subset)[i])
  
  plot(ag_vect_utm, add = TRUE, border = "red", lwd = 2)
}
par(panel_par)

##MaxEnt_03: MaxEnt modelling of environmental predictors
#1. Convert SpatRaster to RasterStack for MaxEnt
predictors_r <- raster::stack(predictors_a_subset)

# 2. Make occurrence points a matrix of coordinates
occ_coords <- st_coordinates(occurrences_a_sf)  

# Convert occurrence point sf to matrix in raster CRS
occ_coords <- st_coordinates(st_transform(occurrences_a_sf, crs(predictors_r)))

# # Confirm raster extent
terra::ext(predictors_r)
 
# # Convert RasterStack to SpatRaster
predictors_r_terra <- rast(predictors_r)
 
# # Reproject occurrences to same CRS as raster
occ_reproj <- st_transform(occurrences_a_sf, crs(predictors_r_terra))
# 
# # Convert occurrences to SpatVector
occ_vect <- vect(occ_reproj)
 
# # Extract values
occ_vals <- terra::extract(predictors_r_terra, occ_vect)
 
 ##Remove ID column
occ_vals <- subset(occ_vals, select = -1)
 
head(occ_vals)

##Add raster layer names to occurence values
names(occ_vals) <- predictor_a_names

##This might not be necessary. Raster isn't used at this point##
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

##Visualize layers of raster again
#Convert predictor layers to terra to make plot extent smooth
# r <- rast(predictors_r)
# panel_par <- par(mfrow = c(2, 2), mar = c(3, 3, 2, 1))
# layers <- c(6, 10, 18, 19)
# for (i in layers) {
#   plot(r[[i]], col = hcl.colors(100, "Viridis"), main = names(r[[i]]))
#   plot(ag_vect_utm, add = TRUE, border = "red", lwd = 2)
# }
# par(panel_par)


##Perform manipulations on the raster to summarize vegetation indices for Wet season here
#Dry season and other sensitivity analysis in a separate script##
# -------------------------------
# Extract vegetation index layers
# -------------------------------
layer_names <- names(predictors_r)

veg_types <- c("EVI", "NDWI", "NDMI")

# create a data.frame with layer info
layers_df <- data.frame(
  layer_name = layer_names,
  type = ifelse(grepl("^EVI", layer_names), "EVI",
                ifelse(grepl("^NDWI", layer_names), "NDWI",
                       ifelse(grepl("^NDMI", layer_names), "NDMI","other"))),
  year = as.numeric(sub(".*_(\\d{4})_.*", "\\1", layer_names)),
  stringsAsFactors = FALSE
)

# Extract wet season: May-July 2024 
wet_layers <- layers_df$layer_name[layers_df$type %in% veg_types & layers_df$year == 2024]

wet_evi  <- wet_layers[grepl("EVI", wet_layers)]
wet_ndwi <- wet_layers[grepl("NDWI", wet_layers)]
wet_ndmi <- wet_layers[grepl("NDMI", wet_layers)]

# -------------------------------
# Compute seasonal-average rasters
# -------------------------------
EVI_wet_r  <- if(length(wet_evi) > 1) calc(predictors_r[[wet_evi]], mean, na.rm=TRUE) else predictors_r[[wet_evi]]
NDWI_wet_r <- if(length(wet_ndwi) > 1) calc(predictors_r[[wet_ndwi]], mean, na.rm=TRUE) else predictors_r[[wet_ndwi]]
NDMI_wet_r <- if(length(wet_ndmi) > 1) calc(predictors_r[[wet_ndmi]], mean, na.rm=TRUE) else predictors_r[[wet_ndmi]]

# -------------------------------
# Identify non-vegetation layers (to keep for merging later)
# -------------------------------
other_layers <- layers_df$layer_name[layers_df$type == "other"]
other_stack  <- predictors_r[[other_layers]]

# -------------------------------
# Create final wet seaso stack
# -------------------------------
wet_stack <- stack(EVI_wet_r, NDWI_wet_r, NDMI_wet_r, other_stack)
names(wet_stack) <- c("EVI_wet","NDWI_wet","NDMI_wet", other_layers)

# Check for NAs/missing values anywhere in the stack
anyNA(values(wet_stack))

##Fix NAs in columns
# --- 1. Continuous rasters: replace NA by layer mean ---
cont_names <- c("EVI_wet", "NDWI_wet", "NDMI_wet",
                "avg_rad", "gpw_v4_population_density_rev11_2020_1_deg",
                "distance2water_30arcsec", "lyr.1",
                "nndist_mean", "area_mean", "angle_mean", "shape_mean")

cont_names <- cont_names[cont_names %in% names(wet_stack)]

for (nm in cont_names) {
  r <- wet_stack[[nm]]
  m <- cellStats(r, stat = "mean", na.rm = TRUE)
  r[is.na(r)] <- m
  wet_stack[[nm]] <- r
}

# --- 2. Categorical raster: replace NA by mode (most frequent class) ---
cat_name <- "landuse_code"  # adjust to your actual landuse layer name
if (cat_name %in% names(wet_stack)) {
  r <- wet_stack[[cat_name]]
  fr <- freq(r, useNA = "no")
  mode_val <- fr[which.max(fr[,"count"]), "value"]
  r[is.na(r)] <- mode_val
  wet_stack[[cat_name]] <- r
}

# -------------------------------
# Extract raster values at wet season occurrence points 
# # -------------------------------
occ_wet <- occ_coords[1:15, ]
occ_vals_wet <- raster::extract(wet_stack, occ_wet)

##Rename Wet Stack Raster
names(wet_stack) <- c(
  "Mean EVI(May-July.2024)",
  "Mean NDWI(May_July.2024)",
  "Mean NDMI(May_July.2024)",
  "Avg.Rad from NTL",
  "population_density",
  "distance_to_water bodies",
  "land surface temperature",
  "landuse",
  "nndist_mean",
  "area_mean",                                 
  "angle_mean",                                
  "shape_mean"
)

##Fit MaxEnt Wet Season Model
maxent_wet <- maxent(
  x       = wet_stack,
  p       = occ_wet,
  factors = "landuse",
  args    = c("replicates=10", "replicatetype=bootstrap")
)

##Run Jack Knife Analysis
set.seed(123)
jkaw <- dismo::maxent(
  x = wet_stack, 
  p = occ_wet, 
  args    = c(
    "jackknife=true",          
    "replicates=5",            
    "replicatetype=bootstrap",
    "randomseed=true" 
  )
)

##Make visualizations of variable contribution
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

pdf("Agugu bootstraped VariableContribution_finalplot.pdf", width = 12, height = 6)

ggplot(jkaw_plot_data, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "red") +
  coord_flip() +
  labs(title = "MaxEnt Variable Contributions (Bootstrap Mean ± SD)",
       x = "Variable",
       y = "Percent Contribution") +
  theme_manuscript()

dev.off()

##Permutation importance visualization

# Extract all rows containing permutation importance
jkaw_permutation_matrix <- jkaw@results[grep("permutation.importance", rownames(jkaw@results)), ]

# Compute mean and SD across replicates
jkaw_mean_permutation <- rowMeans(jkaw_permutation_matrix)
jkaw_sd_permutation   <- apply(jkaw_permutation_matrix, 1, sd)

# Create data frame for plotting
jkaw_pplot_data <- data.frame(
  variable = rownames(jkaw_permutation_matrix),
  mean     = jkaw_mean_permutation,
  sd       = jkaw_sd_permutation
)

# Order variables by mean permutation
jkaw_pplot_data$variable <- factor(jkaw_pplot_data$variable, levels = jkaw_pplot_data$variable[order(jkaw_pplot_data$mean, decreasing = TRUE)])

pdf("Agugu bootstraped Permutationimportance_finalplot.pdf", width = 12, height = 6)

ggplot(jkaw_pplot_data, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "red") +
  coord_flip() +
  labs(title = "MaxEnt Variable permutation importance (Bootstrap Mean ± SD)",
       x = "Variable",
       y = "Percent permutation") +
  theme_manuscript()

dev.off()


##MaxEnt04_Evaluation of model using AUC 
# -------------------------------
# 1. Prepare occurrence data
# -------------------------------
occ_wet_mat <- as.matrix(occ_wet)

# Set cross-validation folds
k <- 2
foldsw <- createFolds(1:nrow(occ_wet_mat), k = k)

# Storage objects
roc_list <- list()
auc_valuesw <- numeric(length(foldsw))

# -------------------------------
# 2. Cross-validation loop
# -------------------------------
for(i in seq_along(foldsw)){
  
  train_idx <- setdiff(1:nrow(occ_wet_mat), foldsw[[i]])
  test_idx  <- foldsw[[i]]
  
  train_pts <- occ_wet_mat[train_idx, , drop = FALSE]
  test_pts  <- occ_wet_mat[test_idx, , drop = FALSE]
  
  # Fit MaxEnt
  modelw <- maxent(
    x = wet_stack,
    p = train_pts,
    factors = "landuse"
  )
  
  # Generate background points
  bg_points <- randomPoints(wet_stack, n = 10000)
  
  # Evaluate model
  eval_objw <- evaluate(
    p = test_pts,
    a = bg_points,
    model = modelw,
    x = wet_stack
  )
  
  # Store AUC
  auc_valuesw[i] <- eval_objw@auc
  
  # Store ROC data
  roc_list[[i]] <- data.frame(
    FPR = eval_objw@FPR,
    TPR = eval_objw@TPR,
    Fold = i
  )
}

# Combine ROC curves
roc_df <- bind_rows(roc_list)

# -------------------------------
# 3. Interpolate ROC curves
# -------------------------------

fpr_grid <- seq(0, 1, length.out = 200)

tpr_matrix <- sapply(unique(roc_df$Fold), function(f){
  
  fold_data <- roc_df %>% filter(Fold == f)
  
  approx(
    x = fold_data$FPR,
    y = fold_data$TPR,
    xout = fpr_grid,
    rule = 2
  )$y
})

# -------------------------------
# 4. Compute mean ROC + CI
# -------------------------------

tpr_mean  <- rowMeans(tpr_matrix)

tpr_lower <- apply(tpr_matrix, 1, quantile, 0.025)

tpr_upper <- apply(tpr_matrix, 1, quantile, 0.975)

roc_summary <- data.frame(
  FPR = fpr_grid,
  TPR_mean = tpr_mean,
  TPR_lower = tpr_lower,
  TPR_upper = tpr_upper
)

# -------------------------------
# 5. Plot publication-quality ROC
# -------------------------------

pdf("Agugu bootstraped ROC plot.pdf", width = 12, height = 6)

ggplot() +
  geom_ribbon(
    data = roc_summary,
    aes(x = FPR, ymin = TPR_lower, ymax = TPR_upper),
    fill = "steelblue",
    alpha = 0.25
  ) +
  geom_line(
    data = roc_summary,
    aes(x = FPR, y = TPR_mean),
    color = "steelblue",
    size = 1.3
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "grey40"
  ) +
  labs(
    title = paste0("MaxEnt ROC Curve (Mean AUC = ", round(mean(auc_valuesw),3), ")"),
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  coord_equal() +
  theme_manuscript()

dev.off()



###----------------------------------------------------------------------------###
########----------------CHALLENGE(FORMAL)--------------------------------#########
###----------------------------------------------------------------------------###

##MaxEnt 01- Retrieve occurence points, visualize and ensure they fit into shapefile
##Read in Challenge Data
lav_chal_df <- read.csv(file.path(Entodir, "Wet Season Data_Ibadan", "lav_chal_bsw.csv"))

occurrences_in_chal <- lav_chal_df %>% 
  dplyr::filter(Anophp == "Yes")

##Extract coordinates
occurrences_cl <- occurrences_in_chal %>%
  dplyr::select(X, Y) %>%
  mutate(
    lon = X,
    lat = Y
  ) %>%
  dplyr::select(lon, lat)
# 
# # Ensure they're numeric
occurrences_cl$lon <- as.numeric(occurrences_cl$lon)
occurrences_cl$lat <- as.numeric(occurrences_cl$lat)

##Plot sites
# Step 1: Convert the 'occurrences' dataframe to an sf object, assigning its *current* CRS (UTM Zone 31N)
occurrences_cl_sf <- st_as_sf(occurrences_in_chal, coords = c("Y", "X"), crs = 4326)  # Replace with correct EPSG if different


# Step 2: Transform to match the CRS of df_ib_c (WGS84)
occurrences_cl_wgs84 <- st_transform(occurrences_cl_sf, crs = st_crs(df_ib_c))

# Step 3: Filter points that fall inside df_ib_c (Challenge ward polygon)
occurrences_in_cl <- occurrences_cl_wgs84[st_within(occurrences_cl_wgs84, df_ib_c, sparse = FALSE), ]

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

# Plot using ggplot with geom_sf for both layers
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


##MaxEnt 02-Environmental Variable extractions
##Here is the output environmental stack of predictor variables
##This is run in the script titled: "MaxEnt_02_Chalenge_Env.R"
# Environmental Predictor data set
predictors_c_subset <- rast("C:/Users/ebamgboye/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/predictors_c_subset.tif")


##Visualize some layers of raster
ch_vect <- vect(df_ib_c)  # convert sf → SpatVector
ch_vect_utm <- project(ch_vect, predictors_c_subset)

##Add meaningful names
names(predictors_c_subset) <- c(
  "EVI.June_2024",
  "EVI.July_2024",
  "NDWI.May_2024",
  "NDWI.June_2024",
  "NDWI.JUly_2024",
  "NDMI.May_2024",
  "NDMI.June_2024",
  "NDMI.July_2024",
  "Avg_rad from NTL",
  "land surface temperature",
  "landuse",
  "population_density_2020",
  "distance2water_bodies",
  "nndist_mean",
  "log_area",
  "angle_mean",
  "shape_mean"
)

##Visualize raster plot in panels
panel_par <- par(mfrow = c(2, 2))
layers <- c(8, 11, 17, 14)
for (i in layers) {
  plot(predictors_c_subset[[i]],
       col = hcl.colors(100, "Viridis"),
       main = names(predictors_c_subset)[i])
  
  plot(ch_vect_utm, add = TRUE, border = "red", lwd = 2)
}
par(panel_par)


##MaxEnt_03: MaxEnt modelling of environmental predictors
# # -------------------------------
# # Vegetation layer manipulation to compute average (wet season)
# # -------------------------------
clayer_names <- names(predictors_c_subset)

names(clayer_names) <- c(
  "EVI.June_2024",
  "EVI.July_2024",
  "NDWI.May_2024",
  "NDWI.June_2024",
  "NDWI.JUly_2024",
  "NDMI.May_2024",
  "NDMI.June_2024",
  "NDMI.July_2024",
  "Avg_rad from NTL",
  "land surface temperature",
  "landuse",
  "population_density_2020",
  "distance2water_bodies",
  "nndist_mean",
  "log_area",
  "angle_mean",
  "shape_mean"
)


veg_types <- c("EVI", "NDWI", "NDMI")

# create a data.frame with layer info
clayers_df <- data.frame(
  clayer_name = clayer_names,
  type = ifelse(grepl("^EVI", clayer_names), "EVI",
                ifelse(grepl("^NDWI", clayer_names), "NDWI",
                       ifelse(grepl("^NDMI", clayer_names), "NDMI","other"))),
  year = as.numeric(sub(".*_(\\d{4})_.*", "\\1", clayer_names)),
  stringsAsFactors = FALSE
)

# Wet season: May-July 2024 
cwet_layers <- clayers_df$clayer_name[clayers_df$type %in% veg_types]

# -------------------------------
# Split by index for averaging
# -------------------------------
wet_evic  <- cwet_layers[grepl("EVI", cwet_layers)]
wet_ndwic <- cwet_layers[grepl("NDWI", cwet_layers)]
wet_ndmic <- cwet_layers[grepl("NDMI", cwet_layers)]

# -------------------------------
# Compute wet season average raster
# -------------------------------
EVI_wet_rc  <- if(length(wet_evic) > 1) app(predictors_c_subset[[wet_evic]], mean, na.rm=TRUE) else predictors_c_subset[[wet_evic]]
NDWI_wet_rc <- if(length(wet_ndwic) > 1) app(predictors_c_subset[[wet_ndwic]], mean, na.rm=TRUE) else predictors_c_subset[[wet_ndwic]]
NDMI_wet_rc <- if(length(wet_ndmic) > 1) app(predictors_c_subset[[wet_ndmic]], mean, na.rm=TRUE) else predictors_c_subset[[wet_ndmic]]

# -------------------------------
# Identify non-vegetation layers to merge later
# -------------------------------
cother_layers <- clayers_df$clayer_name[clayers_df$type == "other"]
cother_stack  <- predictors_c_subset[[cother_layers]]

# -------------------------------
# Create wet season stack
# -------------------------------
cwet_stack <- c(EVI_wet_rc, NDWI_wet_rc, NDMI_wet_rc, cother_stack)
names(cwet_stack) <- c("EVI_wet","NDWI_wet","NDMI_wet", cother_layers)


# # -------------------------------
# Extract raster values at occurrence points
# # -------------------------------
# Create sf points
occurrences_cl_sf <- st_as_sf(
  occurrences_in_cl,
  coords = c("X", "Y"),
  crs = 4326
)

# 2) Reproject points to match cwet_stack CRS
occurrences_cl_sf <- st_transform(occurrences_cl_sf, crs = crs(cwet_stack))

# 3) Extract values directly 
occ_vals_wetc <- terra::extract(cwet_stack, terra::vect(occurrences_cl_sf))

# Optional: if you still need numeric coordinate matrix in raster CRS
coords_cl <- st_coordinates(occurrences_cl_sf)


# Fit MaxEnt for wet season
##Rename Wet Stack Raster
names(cwet_stack) <- c(
  "Mean EVI(May-July.2024)",
  "Mean NDWI(May_July.2024)",
  "Mean NDMI(May_July.2024)",
  "Avg.Rad from NTL",
  "land surface temperature",
  "landuse",
  "population_density",
  "distance_to_water bodies",
  "nndist_mean",
  "log_area",
  "angle_mean",
  "shape_mean"
)

##Some data manipulation before running MaxEnt
# predictors: SpatRaster -> RasterStack
cwet_stack_r <- raster::stack(cwet_stack)
occ_wetc <- occurrences_cl_sf

## 2. Reproject to the CRS of the raster stack (UTM 31N)
occ_wetc_utm <- st_transform(occ_wetc, crs = st_crs(cwet_stack_r))

## 3. Convert sf -> SpatialPointsDataFrame for maxent()
occ_wetc_utm$presence <- 1
occ_wet_sp_utm <- as(occ_wetc_utm, "Spatial")

## 4. check NA predictors
vals <- raster::extract(cwet_stack_r, occ_wet_sp_utm)
keep <- !apply(is.na(vals), 1, any)
occ_wet_sp_utm_clean <- occ_wet_sp_utm[keep, ]

## 5. Run Maxent
set.seed(123) 
maxent_wetc <- maxent(
  x       = cwet_stack_r,
  p       = occ_wet_sp_utm_clean,
  factors = "landuse" ,
  args    = c("replicates=10", "replicatetype=bootstrap")
)

##Run Jack Knife Analysis
set.seed(123)
jkawc <- dismo::maxent(
  x = cwet_stack_r, 
  p = occ_wet_sp_utm_clean, 
  args    = c(
    "jackknife=true",          # keep jackknife plots
    "replicates=5",            # number of bootstrap replicates
    "replicatetype=bootstrap",
    "randomseed=true"
  )
)


# Extract all rows containing contributions
jkawc_contrib_matrix <- jkawc@results[grep("contribution", rownames(jkawc@results)), ]

# Compute mean and SD across replicates
jkawc_mean_contrib <- rowMeans(jkawc_contrib_matrix)
jkawc_sd_contrib   <- apply(jkawc_contrib_matrix, 1, sd)

# Create data frame for plotting
jkawc_plot_data <- data.frame(
  variable = rownames(jkawc_contrib_matrix),
  mean     = jkawc_mean_contrib,
  sd       = jkawc_sd_contrib
)

# Order variables by mean contribution
jkawc_plot_data$variable <- factor(jkawc_plot_data$variable, levels = jkawc_plot_data$variable[order(jkawc_plot_data$mean, decreasing = TRUE)])

pdf("Challenge bootstraped VariableContribution plot.pdf", width = 12, height = 6)

ggplot(jkawc_plot_data, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "red") +
  coord_flip() +
  labs(title = "MaxEnt Variable Contributions (Bootstrap Mean ± SD)",
       x = "Variable",
       y = "Percent Contribution") +
  theme_manuscript()

dev.off()

##Plot Permutation importance
# Extract all rows containing permutation importance
jkawc_permutation_matrix <- jkawc@results[grep("permutation.importance", rownames(jkawc@results)), ]

# Compute mean and SD across replicates
jkawc_mean_permutation <- rowMeans(jkawc_permutation_matrix)
jkawc_sd_permutation   <- apply(jkawc_permutation_matrix, 1, sd)

# Create data frame for plotting
jkawc_pplot_data <- data.frame(
  variable = rownames(jkawc_permutation_matrix),
  mean     = jkawc_mean_permutation,
  sd       = jkawc_sd_permutation
)

# Order variables by mean permutation
jkawc_pplot_data$variable <- factor(jkawc_pplot_data$variable, levels = jkawc_pplot_data$variable[order(jkawc_pplot_data$mean, decreasing = TRUE)])

pdf("Challenge bootstraped Permutationimportance plot.pdf", width = 12, height = 6)

ggplot(jkawc_pplot_data, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "red") +
  coord_flip() +
  labs(title = "MaxEnt Variable permutation importance (Bootstrap Mean ± SD)",
       x = "Variable",
       y = "Percent permutationution") +
  theme_manuscript()

dev.off()

##MaxEnt04_Evaluation of model using AUC 
# -------------------------------
# Extract point coordinates
# -------------------------------
occ_mat <- coordinates(occ_wet_sp_utm) 

occ_chal_mat <- occ_mat

# Set the cross-validation level
k <- 2
foldsc <- createFolds(1:nrow(occ_chal_mat), k = k)

# Storage objects
roc_listc <- list()
auc_valuesc <- numeric(length(foldsc))

# -------------------------------
# Cross-validation loop
# -------------------------------

for(i in seq_along(foldsc)){
  
  train_idx_c <- setdiff(1:nrow(occ_chal_mat), foldsc[[i]])
  test_idx_c  <- foldsc[[i]]
  
  train_pts_c<- occ_chal_mat[train_idx_c, , drop = FALSE]
  test_pts_c  <- occ_chal_mat[test_idx_c, , drop = FALSE]
  
  # Fit MaxEnt
  modelc <- maxent(
    x = cwet_stack_r,
    p = train_pts_c,
    factors = "landuse"
  )
  
  # Generate background points
  bg_pointsc <- randomPoints(cwet_stack_r, n = 10000)
  
  # Evaluate model
  eval_objc <- evaluate(
    p = test_pts_c,
    a = bg_pointsc,
    model = modelc,
    x = cwet_stack_r
  )
  
  # Store AUC
  auc_valuesc[i] <- eval_objc@auc
  
  # Store ROC data
  roc_listc[[i]] <- data.frame(
    FPR = eval_objc@FPR,
    TPR = eval_objc@TPR,
    Fold = i
  )
}

# Combine ROC curves
roc_dfc <- bind_rows(roc_listc)

# -------------------------------
# Interpolate ROC curves
# -------------------------------

fpr_gridc <- seq(0, 1, length.out = 200)

tpr_matrixc <- sapply(unique(roc_dfc$Fold), function(f){
  
  fold_datac <- roc_dfc %>% filter(Fold == f)
  
  approx(
    x = fold_datac$FPR,
    y = fold_datac$TPR,
    xout = fpr_gridc,
    rule = 2
  )$y
})

# -------------------------------
# 4. Compute mean ROC + CI
# -------------------------------

tpr_meanc  <- rowMeans(tpr_matrixc)

tpr_lowerc <- apply(tpr_matrixc, 1, quantile, 0.025)

tpr_upperc <- apply(tpr_matrixc, 1, quantile, 0.975)

roc_summaryc <- data.frame(
  FPR = fpr_gridc,
  TPR_mean = tpr_meanc,
  TPR_lower = tpr_lowerc,
  TPR_upper = tpr_upperc
)

# -------------------------------
# 5. Plot publication-quality ROC
# -------------------------------

pdf("Challenge bootstraped ROC plot.pdf", width = 12, height = 6)

ggplot() +
  geom_ribbon(
    data = roc_summaryc,
    aes(x = FPR, ymin = TPR_lower, ymax = TPR_upper),
    fill = "steelblue",
    alpha = 0.25
  ) +
 geom_line(
    data = roc_summary,
    aes(x = FPR, y = TPR_mean),
    color = "steelblue",
    size = 1.3
  ) +
 geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "grey40"
  ) +
 labs(
    title = paste0("MaxEnt ROC Curve Challenge(Mean AUC = ", round(mean(auc_valuesc),3), ")"),
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
 coord_equal() +
 theme_manuscript()

dev.off ()



