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
library(ENMeval)
library(ENMeval)
library(terra)
library(pROC)

##Need to install Eclipse Adoptium to run MaxEnt modelling
Sys.setenv(JAVA_HOME = (file.path(Entodir, "Eclipse Adoptium", "jdk-21.0.10.7-hotspot")))
library(rJava)

####---------------------------------------------------------------------------#####
##---------------------------------Agugu------------------------------------------##
####---------------------------------------------------------------------------#####
##Read in environmental variables raster file
predictors_a_subset <- rast(file.path(Entodir, "predictors_a_subset.tif"))

##Bring raster names from script 02
vars_keep_vec <- read.csv(file.path(Entodir, "variable_names.csv"))

##Remove ID column
vars_keep_vec <- subset(vars_keep_vec, select = -1)

predictor_a_names <- as.character(vars_keep_vec[[1]])

names(predictors_a_subset) <- predictor_a_names


#Convert SpatRaster to RasterStack for MaxEnt
predictors_r <- raster::stack(predictors_a_subset)


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

# -------------------------------
# Separate dry vs wet layers
# -------------------------------
# Dry season: Jan-Mar 2023
dry_layers <- layers_df$layer_name[layers_df$type %in% veg_types & layers_df$year == 2023]

# Wet season: May -Jul 2024 
wet_layers <- layers_df$layer_name[layers_df$type %in% veg_types & layers_df$year == 2024]

# -------------------------------
# Split by index for averaging
# -------------------------------
dry_evi  <- dry_layers[grepl("EVI", dry_layers)]
dry_ndwi <- dry_layers[grepl("NDWI", dry_layers)]
dry_ndmi <- dry_layers[grepl("NDMI", dry_layers)]

wet_evi  <- wet_layers[grepl("EVI", wet_layers)]
wet_ndwi <- wet_layers[grepl("NDWI", wet_layers)]
wet_ndmi <- wet_layers[grepl("NDMI", wet_layers)]

# -------------------------------
# Compute seasonal-average rasters
# -------------------------------
EVI_dry_r  <- if(length(dry_evi) > 1) calc(predictors_r[[dry_evi]], mean, na.rm=TRUE) else predictors_r[[dry_evi]]
NDWI_dry_r <- if(length(dry_ndwi) > 1) calc(predictors_r[[dry_ndwi]], mean, na.rm=TRUE) else predictors_r[[dry_ndwi]]
NDMI_dry_r <- if(length(dry_ndmi) > 1) calc(predictors_r[[dry_ndmi]], mean, na.rm=TRUE) else predictors_r[[dry_ndmi]]

EVI_wet_r  <- if(length(wet_evi) > 1) calc(predictors_r[[wet_evi]], mean, na.rm=TRUE) else predictors_r[[wet_evi]]
NDWI_wet_r <- if(length(wet_ndwi) > 1) calc(predictors_r[[wet_ndwi]], mean, na.rm=TRUE) else predictors_r[[wet_ndwi]]
NDMI_wet_r <- if(length(wet_ndmi) > 1) calc(predictors_r[[wet_ndmi]], mean, na.rm=TRUE) else predictors_r[[wet_ndmi]]

# -------------------------------
# Identify non-vegetation layers (to keep in both layers)
# -------------------------------
other_layers <- layers_df$layer_name[layers_df$type == "other"]
other_stack  <- predictors_r[[other_layers]]

# -------------------------------
# Create final seasonal stacks
# -------------------------------
dry_stack <- stack(EVI_dry_r, NDWI_dry_r, NDMI_dry_r, other_stack)
names(dry_stack) <- c("EVI_dry","NDWI_dry","NDMI_dry", other_layers)

wet_stack <- stack(EVI_wet_r, NDWI_wet_r, NDMI_wet_r, other_stack)
names(wet_stack) <- c("EVI_wet","NDWI_wet","NDMI_wet", other_layers)


##Combine the dry and wet stack
combined_stack <- stack(EVI_dry_r, NDWI_dry_r, NDMI_dry_r, 
                        EVI_wet_r, NDWI_wet_r, NDMI_wet_r,  other_stack)
names(combined_stack) <- c("EVI_dry","NDWI_dry","NDMI_dry","EVI_wet","NDWI_wet","NDMI_wet", other_layers)

##Fix NAs in columns
# --- 1. Continuous rasters: replace NA by layer mean ---
cont_names <- c("EVI_dry", "NDWI_dry","NDMI_dry","EVI_wet", "NDWI_wet", "NDMI_wet",
                "avg_rad", "gpw_v4_population_density_rev11_2020_1_deg",
                "distance2water_30arcsec", "lyr.1",
                "nndist_mean", "area_mean", "angle_mean", "shape_mean")

cont_names <- cont_names[cont_names %in% names(combined_stack)]

for (nm in cont_names) {
  r <- combined_stack[[nm]]
  m <- cellStats(r, stat = "mean", na.rm = TRUE)
  r[is.na(r)] <- m
  combined_stack[[nm]] <- r
}

# --- 2. Categorical raster: replace NA by mode (most frequent class) ---

cat_name <- "landuse_code"  

if (cat_name %in% names(combined_stack)) {
  r <- combined_stack[[cat_name]]
  fr <- freq(r, useNA = "no")
  mode_val <- fr[which.max(fr[,"count"]), "value"]
  r[is.na(r)] <- mode_val
  combined_stack[[cat_name]] <- r
}

# -------------------------------
# # Extract raster values at occurrence points(both wet and dry season)
# # -------------------------------
##Occurrence points(From the 002_Larviciding Manuscript Analysis(MaxEnt)Script)
occ_coords <- st_coordinates(occurrences_a_sf)  

occ_vals_combined <- raster::extract(combined_stack, occ_coords)

#Combined Wet and Dry Season using Mean of Vegetation indices
##Rename Combined Wet aand Dry Season Raster
names(combined_stack) <- c(
  "EVI(Feb.2023)",
  "Mean NDWI(Jan_Feb.2023)",
  "Mean NDMI(Jan_Feb.2023)",
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

# ##Extract Wet season coordinates
# occ_wet <- occ_coords_valid[1:15, ]
maxent_wet_com <- maxent(
  x       = combined_stack,
  p       = occ_coords,
  factors = "landuse"
)

##Run Jack Knife Analysis
set.seed(123)
jka_com <- dismo::maxent(
  x = combined_stack, 
  p = occ_coords, 
  args    = c(
    "jackknife=true",          
    "replicates=5",            
    "replicatetype=bootstrap",
    "randomseed=true" 
  )
)


# Extract all rows containing contributions
jkaw_contrib_matrix_com <- jka_com@results[grep("contribution", rownames(jka_com@results)), ]

# Compute mean and SD across replicates
jkaw_mean_contrib_com <- rowMeans(jkaw_contrib_matrix_com)
jkaw_sd_contrib_com   <- apply(jkaw_contrib_matrix_com, 1, sd)

# Create data frame for plotting
jkaw_plot_data_com <- data.frame(
  variable = rownames(jkaw_contrib_matrix_com),
  mean     = jkaw_mean_contrib_com,
  sd       = jkaw_sd_contrib_com
)

# Order variables by mean contribution
jkaw_plot_data_com$variable <- factor(jkaw_plot_data_com$variable, levels = jkaw_plot_data_com$variable[order(jkaw_plot_data_com$mean, decreasing = TRUE)])

pdf("Agugu bootstraped VariableContribution_finalplot_combined.pdf", width = 12, height = 6)

ggplot(jkaw_plot_data_com, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "red") +
  coord_flip() +
  labs(title = "MaxEnt Variable Contributions (Bootstrap Mean ± SD)",
       x = "Variable",
       y = "Percent Contribution") +
  theme_manuscript()

dev.off()

# print(jkaw)

##Plot Permutation importance
# Extract all rows containing permutation importance
jkaw_permutation_matrix_com <- jka_com@results[grep("permutation.importance", rownames(jka_com@results)), ]

# Compute mean and SD across replicates
jkaw_mean_permutation_com <- rowMeans(jkaw_permutation_matrix_com)
jkaw_sd_permutation_com   <- apply(jkaw_permutation_matrix_com, 1, sd)

# Create data frame for plotting
jkaw_pplot_data_com <- data.frame(
  variable = rownames(jkaw_permutation_matrix_com),
  mean     = jkaw_mean_permutation_com,
  sd       = jkaw_sd_permutation_com
)

# Order variables by mean permutation
jkaw_pplot_data_com$variable <- factor(jkaw_pplot_data_com$variable, levels = jkaw_pplot_data_com$variable[order(jkaw_pplot_data_com$mean, decreasing = TRUE)])

pdf("Agugu bootstraped Permutationimportance_finalplot_combined.pdf", width = 12, height = 6)

ggplot(jkaw_pplot_data_com, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "red") +
  coord_flip() +
  labs(title = "MaxEnt Variable permutation importance (Bootstrap Mean ± SD)",
       x = "Variable",
       y = "Percent permutation") +
  theme_manuscript()

dev.off()



##----------------------------------------------------------------------------##
##---create wet stack with only veg layers of month of collection------#
###---------------------------------------------------------------------------##
wet_evi <- "EVI_2024_07"
wet_ndwi <- "NDWI_2024_07"
wet_ndmi <- "NDMI_2024_07"

EVI_wet_r  <- if(length(wet_evi) > 1) calc(predictors_r[[wet_evi]], mean, na.rm=TRUE) else predictors_r[[wet_evi]]
NDWI_wet_r <- if(length(wet_ndwi) > 1) calc(predictors_r[[wet_ndwi]], mean, na.rm=TRUE) else predictors_r[[wet_ndwi]]
NDMI_wet_r <- if(length(wet_ndmi) > 1) calc(predictors_r[[wet_ndmi]], mean, na.rm=TRUE) else predictors_r[[wet_ndmi]]

wet_stack2 <- stack(EVI_wet_r, NDWI_wet_r, NDMI_wet_r, other_stack)
names(wet_stack2) <- c("EVI_wet","NDWI_wet","NDMI_wet", other_layers)

##Extract values from raster at occurence points
occ_wet <- occ_coords[1:15, ]
occ_vals_wet2 <- raster::extract(wet_stack2, occ_wet)

# Fit MaxEnt for wet season raster with one layer of vegetation index
##Rename Wet Stack2 Raster
names(wet_stack2) <- c(
  "EVI(July.2024)",
  "NDWI(July.2024)",
  "NDMI(July.2024)",
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

##Extract Wet season coordinates
occ_wet <- occ_coords_valid[1:15, ]
maxent_wet2 <- maxent(
  x       = wet_stack2,
  p       = occ_wet,
  factors = "landuse"
)

##Run Jack Knife Analysis
set.seed(123)
jkaw2 <- dismo::maxent(
  x = wet_stack2, 
  p = occ_wet, 
  args    = c(
    "jackknife=true",          # keep jackknife plots
    "replicates=5",            # number of bootstrap replicates
    "replicatetype=bootstrap",
    "randomseed=true"
  )
)


# Extract all rows containing contributions
jkaw_contrib_matrix2 <- jkaw2@results[grep("contribution", rownames(jkaw2@results)), ]

# Compute mean and SD across replicates
jkaw_mean_contrib2 <- rowMeans(jkaw_contrib_matrix2)
jkaw_sd_contrib2   <- apply(jkaw_contrib_matrix2, 1, sd)

# Create data frame for plotting
jkaw_plot_data2 <- data.frame(
  variable = rownames(jkaw_contrib_matrix2),
  mean     = jkaw_mean_contrib2,
  sd       = jkaw_sd_contrib2
)

# Order variables by mean contribution
jkaw_plot_data2$variable <- factor(jkaw_plot_data2$variable, levels = jkaw_plot_data2$variable[order(jkaw_plot_data2$mean, decreasing = TRUE)])

pdf("Agugu bootstraped VariableContribution plot2.pdf", width = 12, height = 6)

ggplot(jkaw_plot_data2, aes(x = variable, y = mean)) +
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
jkaw_permutation_matrix2 <- jkaw2@results[grep("permutation.importance", rownames(jkaw2@results)), ]

# Compute mean and SD across replicates
jkaw_mean_permutation2 <- rowMeans(jkaw_permutation_matrix2)
jkaw_sd_permutation2   <- apply(jkaw_permutation_matrix2, 1, sd)

# Create data frame for plotting
jkaw_pplot_data2 <- data.frame(
  variable = rownames(jkaw_permutation_matrix2),
  mean     = jkaw_mean_permutation2,
  sd       = jkaw_sd_permutation2
)

# Order variables by mean permutation
jkaw_pplot_data2$variable <- factor(jkaw_pplot_data2$variable, levels = jkaw_pplot_data2$variable[order(jkaw_pplot_data2$mean, decreasing = TRUE)])

pdf("Agugu bootstraped Permutationimportance plot2.pdf", width = 12, height = 6)

ggplot(jkaw_pplot_data2, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "red") +
  coord_flip() +
  labs(title = "MaxEnt Variable permutation importance (Bootstrap Mean ± SD)",
       x = "Variable",
       y = "Percent permutationution") +
  theme_manuscript()

dev.off()



#----------------------------------------------------------------------------##
##---create another wet stack with veg layers averaged 2 month before data collection------#
###---------------------------------------------------------------------------##
wet_evi3 <- c("EVI_2024_06","EVI_2024_07")
wet_ndwi3 <- c("NDWI_2024_06","NDWI_2024_07")
wet_ndmi3 <- c("NDMI_2024_06","NDMI_2024_07")

EVI_wet_r3  <- if(length(wet_evi3) > 1) calc(predictors_r[[wet_evi3]], mean, na.rm=TRUE) else predictors_r[[wet_evi3]]
NDWI_wet_r3 <- if(length(wet_ndwi3) > 1) calc(predictors_r[[wet_ndwi3]], mean, na.rm=TRUE) else predictors_r[[wet_ndwi3]]
NDMI_wet_r3 <- if(length(wet_ndmi3) > 1) calc(predictors_r[[wet_ndmi3]], mean, na.rm=TRUE) else predictors_r[[wet_ndmi3]]

wet_stack3 <- stack(EVI_wet_r3, NDWI_wet_r3, NDMI_wet_r3, other_stack)
names(wet_stack3) <- c("EVI_wet","NDWI_wet","NDMI_wet", other_layers)

##Extract values at occurence point for wet raster with two months averaged vegetation layers
occ_vals_wet3 <- raster::extract(wet_stack3, occ_wet)

# Fit MaxEnt for wet season with one two months averaged vegetation index
##Rename Wet Stack 3 Raster
names(wet_stack3) <- c(
  "Mean EVI(Jun-July.2024)",
  "Mean NDWI(Jun-July.2024)",
  "Mean NDMI(Jun-July.2024)",
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

##Extract Wet season coordinates
occ_wet <- occ_coords[1:15, ]
maxent_wet3 <- maxent(
  x       = wet_stack3,
  p       = occ_wet,
  factors = "landuse"
)

##Run Jack Knife Analysis
set.seed(123)
jkaw3 <- dismo::maxent(
  x = wet_stack3, 
  p = occ_wet, 
  args    = c(
    "jackknife=true",          # keep jackknife plots
    "replicates=5",            # number of bootstrap replicates
    "replicatetype=bootstrap",
    "randomseed=true"
  )
)


# Extract all rows containing contributions
jkaw_contrib_matrix3 <- jkaw3@results[grep("contribution", rownames(jkaw3@results)), ]

# Compute mean and SD across replicates
jkaw_mean_contrib3 <- rowMeans(jkaw_contrib_matrix3)
jkaw_sd_contrib3   <- apply(jkaw_contrib_matrix3, 1, sd)

# Create data frame for plotting
jkaw_plot_data3 <- data.frame(
  variable = rownames(jkaw_contrib_matrix3),
  mean     = jkaw_mean_contrib3,
  sd       = jkaw_sd_contrib3
)

# Order variables by mean contribution
jkaw_plot_data3$variable <- factor(jkaw_plot_data3$variable, levels = jkaw_plot_data3$variable[order(jkaw_plot_data3$mean, decreasing = TRUE)])

pdf("Agugu bootstraped VariableContribution plot3.pdf", width = 12, height = 6)

ggplot(jkaw_plot_data3, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "red") +
  coord_flip() +
  labs(title = "MaxEnt Variable Contributions (Bootstrap Mean ± SD)",
       x = "Variable",
       y = "Percent Contribution") +
  theme_manuscript()

dev.off()

# print(jkaw)

##Plot Permutation importance
# Extract all rows containing permutation importance
jkaw_permutation_matrix3 <- jkaw3@results[grep("permutation.importance", rownames(jkaw3@results)), ]

# Compute mean and SD across replicates
jkaw_mean_permutation3 <- rowMeans(jkaw_permutation_matrix3)
jkaw_sd_permutation3 <- apply(jkaw_permutation_matrix3, 1, sd)

# Create data frame for plotting
jkaw_pplot_data3 <- data.frame(
  variable = rownames(jkaw_permutation_matrix3),
  mean     = jkaw_mean_permutation3,
  sd       = jkaw_sd_permutation3
)

# Order variables by mean permutation
jkaw_pplot_data3$variable <- factor(jkaw_pplot_data3$variable, levels = jkaw_pplot_data3$variable[order(jkaw_pplot_data3$mean, decreasing = TRUE)])

pdf("Agugu bootstraped Permutationimportance plot3.pdf", width = 12, height = 6)

ggplot(jkaw_pplot_data3, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "red") +
  coord_flip() +
  labs(title = "MaxEnt Variable permutation importance (Bootstrap Mean ± SD)",
       x = "Variable",
       y = "Percent permutationution") +
  theme_manuscript()

dev.off()



####---------------------------------------------------------------------------####
##---------------------------Challenge-----------------------------------------##
####--------------------------------------------------------------------------####

##----------------------------------------------------------------------------##
##---create another wet stack with only veg layers of month of collection------#
###---------------------------------------------------------------------------##
##Read in extracted environmental rasters
predictors_c_subset <- rast(file.path(Entodir, "predictors_c_subset.tif"))

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

#Create dataframe for layer names
clayer_names <- names(predictors_c_subset)

# create a data.frame with layer info
clayers_df <- data.frame(
  clayer_name = clayer_names,
  type = ifelse(grepl("^EVI", clayer_names), "EVI",
                ifelse(grepl("^NDWI", clayer_names), "NDWI",
                       ifelse(grepl("^NDMI", clayer_names), "NDMI","other"))),
  year = as.numeric(sub(".*_(\\d{4})_.*", "\\1", clayer_names)),
  stringsAsFactors = FALSE
)

#Create Vegetation indices hold
veg_types <- c("EVI", "NDWI", "NDMI")

# # Extract Wet season: May-Jul 2024
# cwet_layers <- clayers_df$clayer_name[clayers_df$type %in% veg_types]

##Extract other layers to merge later
cother_layers <- clayers_df$clayer_name[clayers_df$type == "other"]
cother_stack  <- predictors_c_subset[[cother_layers]]


#Assign variables
wet_evic2 <- "EVI.July_2024"
wet_ndwic2 <- "NDWI.JUly_2024"
wet_ndmic2 <- "NDMI.July_2024"

EVI_wet_rc2  <- if(length(wet_evic2) > 1) app(predictors_c_subset[[wet_evic2]], mean, na.rm=TRUE) else predictors_c_subset[[wet_evic2]]
NDWI_wet_rc2 <- if(length(wet_ndwic2) > 1) app(predictors_c_subset[[wet_ndwic2]], mean, na.rm=TRUE) else predictors_c_subset[[wet_ndwic2]]
NDMI_wet_rc2 <- if(length(wet_ndmic2) > 1) app(predictors_c_subset[[wet_ndmic2]], mean, na.rm=TRUE) else predictors_c_subset[[wet_ndmic2]]

# EVI_wet_rc2  <- if(length(wet_evic2) > 1) calc(cwet_stack_r[[wet_evic2]], mean, na.rm=TRUE) else cwet_stack_r[[wet_evic2]]
# NDWI_wet_rc2 <- if(length(wet_ndwic2) > 1) calc(cwet_stack_r[[wet_ndwic2]], mean, na.rm=TRUE) else cwet_stack_r[[wet_ndwic2]]
# NDMI_wet_rc2 <- if(length(wet_ndmic2) > 1) calc(cwet_stack_r[[wet_ndmic2]], mean, na.rm=TRUE) else cwet_stack_r[[wet_ndmic2]]

cwet_stack_r2 <- c(EVI_wet_rc2, NDWI_wet_rc2, NDMI_wet_rc2, cother_stack)
names(cwet_stack_r2) <- c("EVI_wet","NDWI_wet","NDMI_wet", cother_layers)

##occ_vals_wet2c2 <- raster::extract(cwet_stack_r2, occ_wet_sp_utm)

# Fit MaxEnt for wet season with only one vegetation layer
##Rename Wet Stack2 Raster
names(cwet_stack_r2) <- c(
  "EVI(July.2024)",
  "NDWI(July.2024)",
  "NDMI(July.2024)",
  "Avg.Rad from NTL",
  "land surface temperature",
  "landuse",
  "population_density",
  "distance_to_water bodies",
  "nndist_mean",
  "area_mean",                                 
  "angle_mean",                                
  "shape_mean"
)

##Ensure object types are compatible for MaxEnt
##Must have run the first model for challenge in 002_Larviciding Manuscript(MaxEnt) script
occ_wetc_utmc <- st_coordinates(occ_wetc_utm)
cwet_stack_r2 <- raster::stack(cwet_stack_r2)
maxent_c2 <- maxent(
  x       = cwet_stack_r2,
  p       = occ_wetc_utmc,
  factors = "landuse", 
  args    = c("replicates=10", "replicatetype=bootstrap")
)

##Run Jack Knife Analysis
set.seed(123)
jkac2 <- dismo::maxent(
  x = cwet_stack_r2, 
  p = occ_wetc_utmc, 
  args    = c(
    "jackknife=true",          # keep jackknife plots
    "replicates=5",            # number of bootstrap replicates
    "replicatetype=bootstrap",
    "randomseed=true"
  )
)


# Extract all rows containing contributions
jkac_contrib_matrix2 <- jkac2@results[grep("contribution", rownames(jkac2@results)), ]

# Compute mean and SD across replicates
jkac_mean_contrib2 <- rowMeans(jkac_contrib_matrix2)
jkac_sd_contrib2   <- apply(jkac_contrib_matrix2, 1, sd)

# Create data frame for plotting
jkac_plot_data2 <- data.frame(
  variable = rownames(jkac_contrib_matrix2),
  mean     = jkac_mean_contrib2,
  sd       = jkac_sd_contrib2
)

# Order variables by mean contribution
jkac_plot_data2$variable <- factor(jkac_plot_data2$variable, levels = jkac_plot_data2$variable[order(jkac_plot_data2$mean, decreasing = TRUE)])

pdf("Challenge bootstraped VariableContributionplot2.pdf", width = 12, height = 6)

ggplot(jkac_plot_data2, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "red") +
  coord_flip() +
  labs(title = "MaxEnt Variable Contributions (Bootstrap Mean ± SD), (0mth) ",
       x = "Variable",
       y = "Percent Contribution") +
  theme_manuscript()

dev.off()

##Plot Permutation importance
# Extract all rows containing permutation importance
jkac_permutation_matrix2 <- jkac2@results[grep("permutation.importance", rownames(jkac2@results)), ]

# Compute mean and SD across replicates
jkac_mean_permutation2 <- rowMeans(jkac_permutation_matrix2)
jkac_sd_permutation2   <- apply(jkac_permutation_matrix2, 1, sd)

# Create data frame for plotting
jkac_pplot_data2 <- data.frame(
  variable = rownames(jkac_permutation_matrix2),
  mean     = jkac_mean_permutation2,
  sd       = jkac_sd_permutation2
)

# Order variables by mean permutation
jkac_pplot_data2$variable <- factor(jkac_pplot_data2$variable, levels = jkac_pplot_data2$variable[order(jkac_pplot_data2$mean, decreasing = TRUE)])

pdf("Challenge bootstraped Permutationimportance plot2.pdf", width = 12, height = 6)

ggplot(jkac_pplot_data2, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "red") +
  coord_flip() +
  labs(title = "MaxEnt Variable permutation importance (Bootstrap Mean ± SD)- 0mth",
       x = "Variable",
       y = "Percent permutationution") +
  theme_manuscript()

dev.off()



#----------------------------------------------------------------------------##
##---create wet stack with veg layers averaged 2 month before collection------#
###---------------------------------------------------------------------------##
wet_evic3 <- c("EVI.Jun_2024", "EVI.July_2024")
wet_ndwic3 <- c("NDWI.June_2024", "NDWI.JUly_2024")
wet_ndmic3 <- c("NDMI.June_2024", "NDMI.July_2024")

EVI_wet_rc3  <- if(length(wet_evic3) > 1) app(predictors_c_subset[[wet_evic3]], mean, na.rm=TRUE) else predictors_c_subset[[wet_evic3]]
NDWI_wet_rc3 <- if(length(wet_ndwic3) > 1) app(predictors_c_subset[[wet_ndwic3]], mean, na.rm=TRUE) else predictors_c_subset[[wet_ndwic3]]
NDMI_wet_rc3 <- if(length(wet_ndmic3) > 1) app(predictors_c_subset[[wet_ndmic3]], mean, na.rm=TRUE) else predictors_c_subset[[wet_ndmic3]]

cwet_stack_r3 <- c(EVI_wet_rc3, NDWI_wet_rc3, NDMI_wet_rc3, cother_stack)
names(cwet_stack_r3) <- c("EVI_wet","NDWI_wet","NDMI_wet", cother_layers)

#occ_vals_wet2c3 <- raster::extract(cwet_stack_r3, occ_wet_sp_utm)

# Fit MaxEnt for wet season for vegetation layers averaged of 2 months
##Rename Wet Stack 3 Raster
names(cwet_stack_r3) <- c(
  "EVI(Jun-July.2024)",
  "NDWI(Jun-July.2024)",
  "NDMI(Jun-July.2024)",
  "Avg.Rad from NTL",
  "land surface temperature",
  "landuse",
  "population_density",
  "distance_to_water bodies",
  "nndist_mean",
  "area_mean",                                 
  "angle_mean",                                
  "shape_mean"
)

##Ensure object types are compatible for MaxEnt
occ_wetc_utmc <- st_coordinates(occ_wetc_utm)
cwet_stack_r3 <- raster::stack(cwet_stack_r3)
maxent_c3 <- maxent(
  x       = cwet_stack_r3,
  p       = occ_wetc_utmc,
  factors = "landuse", 
  args    = c("replicates=10", "replicatetype=bootstrap")
)

##Run Jack Knife Analysis
set.seed(123)
jkac3 <- dismo::maxent(
  x = cwet_stack_r3, 
  p = occ_wetc_utmc, 
  args    = c(
    "jackknife=true",          # keep jackknife plots
    "replicates=5",            # number of bootstrap replicates
    "replicatetype=bootstrap",
    "randomseed=true"
  )
)


# Extract all rows containing contributions
jkac_contrib_matrix3 <- jkac3@results[grep("contribution", rownames(jkac3@results)), ]

# Compute mean and SD across replicates
jkac_mean_contrib3 <- rowMeans(jkac_contrib_matrix3)
jkac_sd_contrib3   <- apply(jkac_contrib_matrix3, 1, sd)

# Create data frame for plotting
jkac_plot_data3 <- data.frame(
  variable = rownames(jkac_contrib_matrix3),
  mean     = jkac_mean_contrib3,
  sd       = jkac_sd_contrib3
)

# Order variables by mean contribution
jkac_plot_data3$variable <- factor(jkac_plot_data3$variable, levels = jkac_plot_data3$variable[order(jkac_plot_data3$mean, decreasing = TRUE)])

pdf("Challenge bootstraped VariableContributionplot3.pdf", width = 12, height = 6)

ggplot(jkac_plot_data3, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "red") +
  coord_flip() +
  labs(title = "MaxEnt Variable Contributions (Bootstrap Mean ± SD), (2mth) ",
       x = "Variable",
       y = "Percent Contribution") +
  theme_manuscript()

dev.off()


##Plot Permutation importance
# Extract all rows containing permutation importance
jkac_permutation_matrix3 <- jkac3@results[grep("permutation.importance", rownames(jkac3@results)), ]

# Compute mean and SD across replicates
jkac_mean_permutation3 <- rowMeans(jkac_permutation_matrix3)
jkac_sd_permutation3   <- apply(jkac_permutation_matrix3, 1, sd)

# Create data frame for plotting
jkac_pplot_data3 <- data.frame(
  variable = rownames(jkac_permutation_matrix3),
  mean     = jkac_mean_permutation3,
  sd       = jkac_sd_permutation3
)

# Order variables by mean permutation
jkac_pplot_data3$variable <- factor(jkac_pplot_data3$variable, levels = jkac_pplot_data3$variable[order(jkac_pplot_data3$mean, decreasing = TRUE)])

pdf("Challenge bootstraped Permutationimportance plot3.pdf", width = 12, height = 6)

ggplot(jkac_pplot_data3, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "red") +
  coord_flip() +
  labs(title = "MaxEnt Variable permutation importance (Bootstrap Mean ± SD)- 2mth",
       x = "Variable",
       y = "Percent permutationution") +
  theme_manuscript()

dev.off()
