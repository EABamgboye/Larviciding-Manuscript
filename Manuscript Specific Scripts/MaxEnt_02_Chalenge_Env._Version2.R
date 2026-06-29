library(raster)
library(dismo)
library(ENMeval)
library(ENMeval)
library(terra)
library(pROC)
library(sp)
library(raster)

# -------------------------------
# 1️⃣ Inputs
# -------------------------------
# Environmental Predictor data set
predictors_c_subset <- rast("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/predictors_c_subset.tif")

# Occurence points
occurrences_in_cl <- read.csv("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/occurrences_in_cl.csv") %>% 
  dplyr::select(c(X,Y))

occurrences_cl_sf <- st_as_sf(occurrences_in_cl, coords = c("X", "Y"), crs = 4326)  # Replace with correct EPSG if different
occurrences_cl_sf <- st_transform(occurrences_cl_sf, st_crs(df_ib_c))


# predictors_r already contains monthly layers like "EVI_2023_02", "NDWI_2024_06", etc.

# -------------------------------
# 2️⃣ Detect vegetation index layers
# -------------------------------


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

# Wet season: Jul-Aug 2024 (or include months in 2024)
cwet_layers <- clayers_df$clayer_name[clayers_df$type %in% veg_types]

# -------------------------------
# 4️⃣ Split by index for averaging
# -------------------------------
wet_evic  <- cwet_layers[grepl("EVI", cwet_layers)]
wet_ndwic <- cwet_layers[grepl("NDWI", cwet_layers)]
wet_ndmic <- cwet_layers[grepl("NDMI", cwet_layers)]

# -------------------------------
# 5️⃣ Compute seasonal-average rasters
# -------------------------------
EVI_wet_rc  <- if(length(wet_evic) > 1) app(predictors_c_subset[[wet_evic]], mean, na.rm=TRUE) else predictors_c_subset[[wet_evic]]
NDWI_wet_rc <- if(length(wet_ndwic) > 1) app(predictors_c_subset[[wet_ndwic]], mean, na.rm=TRUE) else predictors_c_subset[[wet_ndwic]]
NDMI_wet_rc <- if(length(wet_ndmic) > 1) app(predictors_c_subset[[wet_ndmic]], mean, na.rm=TRUE) else predictors_c_subset[[wet_ndmic]]

# -------------------------------
# 6️⃣ Identify non-vegetation layers (to keep in both)
# -------------------------------
cother_layers <- clayers_df$clayer_name[clayers_df$type == "other"]
cother_stack  <- predictors_c_subset[[cother_layers]]

# -------------------------------
# 7️⃣ Create final seasonal stacks
# -------------------------------
cwet_stack <- c(EVI_wet_rc, NDWI_wet_rc, NDMI_wet_rc, cother_stack)
names(cwet_stack) <- c("EVI_wet","NDWI_wet","NDMI_wet", cother_layers)


# # -------------------------------
# # 8️⃣ Extract raster values at occurrence points
# # -------------------------------
# occ_vals_dry <- raster::extract(dry_stack, occ_coords_valid)
occ_vals_wetc <- raster::extract(cwet_stack, occurrences_in_cl)
# 
# # -------------------------------
# # 9️⃣ Combine with coordinates for MaxEnt (optional)
# # -------------------------------
# occ_vals_dry_df <- cbind(occ_coords_valid, occ_vals_dry)
# occ_vals_wet_df <- cbind(occ_coords_valid, occ_vals_wet)

# # -------------------------------
# # 🔟 Fit MaxEnt for dry season
# # -------------------------------
# occ_dry <- occ_coords_valid[16:17, ]
# maxent_dry <- maxent(
#   x       = dry_stack,
#   p       = occ_dry,
#   factors = "landuse_code"
# )

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

##Some data manipulation before running MAxEnt
# predictors: SpatRaster -> RasterStack
cwet_stack_r <- raster::stack(cwet_stack)

occ_wetc <- occurrences_cl_sf

## 1. Start from your sf object in WGS84
occ_wetc <- occurrences_cl_sf         # already sf


## 2. Reproject to the CRS of the raster stack (UTM 31N)
occ_wetc_utm <- st_transform(occ_wetc, crs = st_crs(cwet_stack_r))
#occ_wetc_utm <- sf::st_transform(occ_wetc, crs = raster::crs(cwet_stack_r))
## 3. Convert sf -> SpatialPointsDataFrame for maxent()
occ_wetc_utm$presence <- 1
occ_wet_sp_utm <- as(occ_wetc_utm, "Spatial")

## 4. (Optional) check NA predictors
vals <- raster::extract(cwet_stack_r, occ_wet_sp_utm)
keep <- !apply(is.na(vals), 1, any)
occ_wet_sp_utm_clean <- occ_wet_sp_utm[keep, ]

## 5. Run Maxent
names(cwet_stack_r) # check the exact landuse layer name
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

# print(jkaw)

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


##Extract and plot response curves

m1c <- jkawc@models[[1]]

# variables used in the MaxEnt model
names(m1c@data)          # often works
# if that is NULL, also try:
colnames(m1c@presence)

vars_of_interest <- c("Mean.NDWI.May_July.2024.",
                      "Avg.Rad.from.NTL",
                      "angle_mean")   # replace with your actual names


library(dismo)

bootstrap_response_mean <- function(models, var, n_grid = 200) {
  # 1) response for each replicate
  resp_list <- lapply(models, function(m) response(m, var = var))
  
  # use first replicate's x grid as reference
  x_ref <- resp_list[[1]][, 1]
  
  # 2) interpolate all replicates onto the same x grid (if needed)
  y_mat <- sapply(resp_list, function(r) {
    approx(r[, 1], r[, 2], xout = x_ref, rule = 2)$y
  })
  
  # 3) mean (and optional sd) across replicates
  y_mean <- rowMeans(y_mat, na.rm = TRUE)
  y_sd   <- apply(y_mat, 1, sd, na.rm = TRUE)
  
  data.frame(x = x_ref, mean = y_mean, sd = y_sd)
}

models <- jkawc@models

par(mfrow = c(1, 3))

for (v in vars_of_interest) {
  resp_df <- bootstrap_response_mean(models, var = v)
  
  plot(resp_df$x, resp_df$mean, type = "l",
       xlab = v, ylab = "Suitability (bootstrap mean)",
       main = v)
  
  # optional: add ±1 SD as shaded band
  lines(resp_df$x, resp_df$mean + resp_df$sd, lty = 2, col = "grey50")
  lines(resp_df$x, resp_df$mean - resp_df$sd, lty = 2, col = "grey50")
}

par(mfrow = c(1, 1))

##Response for all variables
library(dismo)

library(dismo)
library(raster)

modelsc <- jkawc@models
m1c <- modelsc[[1]]

# 1) variables used in the MaxEnt model
vars_all <- colnames(m1c@presence)
if (is.null(vars_all) || length(vars_all) == 0) {
  vars_all <- colnames(m1c@presence)
}

# remove accidental duplicates
vars_to_plot <- unique(vars_all)
vars_to_plot <- vars_to_plot[!is.na(vars_to_plot) & nzchar(vars_to_plot)]

bootstrap_response_mean <- function(models, var) {
  resp_list <- lapply(models, function(m) response(m, var = var))
  
  x_ref <- resp_list[[1]][, 1]
  
  if (is.numeric(x_ref)) {
    y_mat <- sapply(resp_list, function(r) {
      approx(r[, 1], r[, 2], xout = x_ref, rule = 2)$y
    })
  } else {
    # categorical factor: match by category labels
    y_mat <- sapply(resp_list, function(r) {
      idx <- match(x_ref, r[, 1])
      r[, 2][idx]
    })
  }
  
  y_mean <- rowMeans(y_mat, na.rm = TRUE)
  y_sd   <- apply(y_mat, 1, sd, na.rm = TRUE)
  
  data.frame(x = x_ref, mean = y_mean, sd = y_sd)
}

# 2) one panel per variable (paginate for many variables)
plot_bootstrap_response_panels <- function(vars, models,
                                           ncol = 3, nrow = 3,
                                           out_pdf = "maxent_response_curves_all_vars_challenge.pdf") {
  per_page <- ncol * nrow
  k <- length(vars)
  n_pages <- ceiling(k / per_page)
  
  pdf(out_pdf, width = 8.5, height = 11)
  
  for (p in seq_len(n_pages)) {
    idx <- ((p - 1) * per_page + 1):min(p * per_page, k)
    vars_page <- vars[idx]
    
    par(mfrow = c(nrow, ncol), mar = c(4, 4, 2, 1))
    
    for (v in vars_page) {
      resp_df <- bootstrap_response_mean(models, v)
      
      if (is.numeric(resp_df$x)) {
        plot(resp_df$x, resp_df$mean, type = "l",
             xlab = v, ylab = "Suitability",
             main = v)
        lines(resp_df$x, resp_df$mean + resp_df$sd, lty = 2, col = "grey50")
        lines(resp_df$x, resp_df$mean - resp_df$sd, lty = 2, col = "grey50")
      } else {
        x_num <- seq_along(resp_df$x)
        plot(x_num, resp_df$mean, type = "b",
             xaxt = "n", xlab = v, ylab = "Suitability",
             main = v)
        axis(1, at = x_num, labels = resp_df$x, las = 2, cex.axis = 0.7)
        lines(x_num, resp_df$mean + resp_df$sd, lty = 2, col = "grey50")
        lines(x_num, resp_df$mean - resp_df$sd, lty = 2, col = "grey50")
      }
    }
    
    # fill remaining panels (if last page not full)
    if (length(vars_page) < per_page) {
      for (i in (length(vars_page) + 1):per_page) plot.new()
    }
  }
  
  dev.off()
}

plot_bootstrap_response_panels(vars_to_plot, modelsc, ncol = 3, nrow = 3)



##----------------------------------------------------------------------------##
##---create another wet stack with only veg layers of month of collection------#
###---------------------------------------------------------------------------##

#wet_evi  <- wet_layers[grepl("EVI", wet_layers)]
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

occ_vals_wet2c2 <- raster::extract(cwet_stack_r2, occ_wet_sp_utm)

# Fit MaxEnt for wet season
##Rename Wet Stack Raster
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

# print(jkaw)

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
##---create another wet stack with veg layers averaged 2 month before collection------#
###---------------------------------------------------------------------------##

#wet_evi  <- wet_layers[grepl("EVI", wet_layers)]
wet_evic3 <- c("EVI.Jun_2024", "EVI.July_2024")
wet_ndwic3 <- c("NDWI.June_2024", "NDWI.JUly_2024")
wet_ndmic3 <- c("NDMI.June_2024", "NDMI.July_2024")

EVI_wet_rc3  <- if(length(wet_evic3) > 1) app(predictors_c_subset[[wet_evic3]], mean, na.rm=TRUE) else predictors_c_subset[[wet_evic3]]
NDWI_wet_rc3 <- if(length(wet_ndwic3) > 1) app(predictors_c_subset[[wet_ndwic3]], mean, na.rm=TRUE) else predictors_c_subset[[wet_ndwic3]]
NDMI_wet_rc3 <- if(length(wet_ndmic3) > 1) app(predictors_c_subset[[wet_ndmic3]], mean, na.rm=TRUE) else predictors_c_subset[[wet_ndmic3]]

# EVI_wet_rc2  <- if(length(wet_evic2) > 1) calc(cwet_stack_r[[wet_evic2]], mean, na.rm=TRUE) else cwet_stack_r[[wet_evic2]]
# NDWI_wet_rc2 <- if(length(wet_ndwic2) > 1) calc(cwet_stack_r[[wet_ndwic2]], mean, na.rm=TRUE) else cwet_stack_r[[wet_ndwic2]]
# NDMI_wet_rc2 <- if(length(wet_ndmic2) > 1) calc(cwet_stack_r[[wet_ndmic2]], mean, na.rm=TRUE) else cwet_stack_r[[wet_ndmic2]]

cwet_stack_r3 <- c(EVI_wet_rc3, NDWI_wet_rc3, NDMI_wet_rc3, cother_stack)
names(cwet_stack_r3) <- c("EVI_wet","NDWI_wet","NDMI_wet", cother_layers)

occ_vals_wet2c3 <- raster::extract(cwet_stack_r3, occ_wet_sp_utm)

# Fit MaxEnt for wet season
##Rename Wet Stack Raster
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

pdf("Challenge bootstraped VariableContributionplot2.pdf", width = 12, height = 6)

ggplot(jkac_plot_data3, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "red") +
  coord_flip() +
  labs(title = "MaxEnt Variable Contributions (Bootstrap Mean ± SD), (2mth) ",
       x = "Variable",
       y = "Percent Contribution") +
  theme_manuscript()

dev.off()

# print(jkaw)

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






























##Evaluate the Wet Season MaxEnt model
# Generate background points (10,000 random points in study area)
bg_pointsw <- randomPoints(wet_stack, n = 10000)

# Evaluate MaxEnt model
eval_wet <- evaluate(p = occ_wet, a = bg_pointsw, model = maxent_wet, x = wet_stack)

# Access AUC
eval_wet@auc

#Evaluate using Cross validation
wet_stack_terra <- terra::rast(wet_stack)

eval <- ENMevaluate(
  occs       = occ_wet,
  envs       = wet_stack_terra,
  algorithm  = "maxent.jar",   # or "maxnet", etc., as appropriate
  partitions = "block",
  tune.args  = list(
    fc = c("L", "LQ", "H"),           # feature classes to test
    rm = seq(0.5, 4, 0.5)             # regularization multipliers
  ),
  parallel   = TRUE,
  numCores   = 4
)

# View results
eval@results

##Summarize Evaluations and Make visualizations

# 1. Take the results table and identify the AICc-best model
res <- eval@results
best_row <- res[which.min(res$delta.AICc), ]          # row 11 in your printout
best_id  <- as.character(best_row$tune.args)          # e.g. "fc.LQ_rm.2"

best_id

# 2. Get the prediction raster and data points for that model
pred_r   <- eval.predictions(eval)[[best_id]]         # SpatRaster of predictions
occ_pts  <- eval.occs(eval)                           # presence points (SpatVector)
bg_pts   <- eval.bg(eval)                             # background points (SpatVector)

# 2b. Keep ONLY the coordinate columns (here named X, Y)
occ_pts <- occ_pts[, c("X", "Y")]
bg_pts  <- bg_pts[,  c("X", "Y")]

# 3. Extract predicted values at presences and background
occ_vals <- terra::extract(pred_r, occ_pts)[, 1]
bg_vals  <- terra::extract(pred_r, bg_pts)[, 1]

# 4. Build ROC object
labels <- c(rep(1, length(occ_vals)), rep(0, length(bg_vals)))  # 1 = presence, 0 = background
scores <- c(occ_vals, bg_vals)

roc_obj <- pROC::roc(response = labels, predictor = scores, quiet = TRUE)

# 5. Plot ROC curve and print AUC
plot(roc_obj,
     col  = "blue",
     lwd  = 2,
     main = paste0("ROC curve - ", best_id))

abline(a = 0, b = 1, lty = 2, col = "grey")
pROC::auc(roc_obj)


##Using k-fold Cross validation
# Ensure occ_wet is numeric matrix
occ_wet_mat <- as.matrix(occ_wet)

foldsw <- createFolds(1:nrow(occ_wet_mat), k = 2)  # small example with 2 points

auc_valuesw <- numeric(length(foldsw))

for(i in 1:length(foldsw)){
  # Row indices
  train_idxw <- setdiff(1:nrow(occ_wet_mat), foldsw[[i]])
  test_idxw  <- foldsw[[i]]
  
  # Subset as matrices
  train_points_matw <- occ_wet_mat[train_idxw, , drop = FALSE]  # keep 2D structure
  test_points_matw  <- occ_wet_mat[test_idxw, , drop = FALSE]
  
  # Fit MaxEnt
  modelw <- maxent(x = wet_stack, p = train_points_matw, factors = "landuse")
  
  # Background points as matrix
  bg_pointsw <- randomPoints(wet_stack, n = 10000)
  
  # Evaluate model on test points
  eval_objw <- evaluate(p = test_points_matw, a = bg_pointsw, model = modelw, x = wet_stack)
  
  auc_valuesw[i] <- eval_objw@auc
}

mean(auc_valuesw)

# Simple ROC plot with AUC
plot(eval_objw, 'ROC', main = paste0("ROC Curve (AUC = ", round(eval_objw@auc, 3), ")"))








# -------------------------------
# Optional: Predict habitat suitability
# -------------------------------
dry_pred <- predict(maxent_dry, dry_stack)
wet_pred <- predict(maxent_wet, wet_stack)

plot(dry_pred, main="Dry Season Suitability")
plot(wet_pred, main="Wet Season Suitability")


jkatw <- dismo::maxent(wet_stack, occ_wet, args = c("jackknife=true"))
# Base R jackknife plot
plot(jkatw, type = "jackknife")
# View results
print(jkatw)

jkatd <- dismo::maxent(dry_stack, occ_dry, args = c("jackknife=true"))
# Base R jackknife plot
plot(jkatd, type = "jackknife")
# View results
print(jkatd)

jkatc <- dismo::maxent(combined_stack, occ_coords_valid, args = c("jackknife=true"))
# Base R jackknife plot
plot(jkatc, type = "jackknife")
# View results
print(jkatc)






























##Evaluate the MaxEnt models
library(dismo)

# Generate background points (10,000 random points in study area)
bg_pointsw <- randomPoints(wet_stack, n = 10000)
bg_pointsd <- randomPoints(dry_stack, n = 10000)

# Evaluate MaxEnt model
eval_dry <- evaluate(p = occ_dry, a = bg_pointsw, model = maxent_dry, x = dry_stack)
eval_wet <- evaluate(p = occ_wet, a = bg_pointsd, model = maxent_wet, x = wet_stack)

# Access AUC
eval_dry@auc
eval_wet@auc


##Using Cross-validation/K-fold
library(dismo)

options(maxent = "C:/Users/ebamgboye/Downloads/maxent/maxent/maxent.jar")

maxent_dry_cv <- maxent(x = dry_stack, p = occ_dry, 
                        factors = "landuse_code", 
                        args = c("replicates=5","replicatetype=crossvalidate"))


library(caret)
library(dismo)
set.seed(123)

# Ensure occ_dry is numeric matrix
occ_dry_mat <- as.matrix(occ_dry)

# Create folds (still works on matrix)
library(caret)
set.seed(123)
folds <- createFolds(1:nrow(occ_dry_mat), k = 2)  # small example with 2 points

auc_valuesd <- numeric(length(folds))

for(i in 1:length(folds)){
  # Row indices
  train_idx <- setdiff(1:nrow(occ_dry_mat), folds[[i]])
  test_idx  <- folds[[i]]
  
  # Subset as matrices
  train_points_mat <- occ_dry_mat[train_idx, , drop = FALSE]  # keep 2D structure
  test_points_mat  <- occ_dry_mat[test_idx, , drop = FALSE]
  
  # Fit MaxEnt
  model <- maxent(x = dry_stack, p = train_points_mat, factors = "landuse_code")
  
  # Background points as matrix
  bg_points <- randomPoints(dry_stack, n = 10000)
  
  # Evaluate model on test points
  eval_obj <- evaluate(p = test_points_mat, a = bg_points, model = model, x = dry_stack)
  
  auc_values[i] <- eval_obj@auc
}

mean(auc_valuesd)

library(caret)
set.seed(123)
# Ensure occ_wet is numeric matrix
occ_wet_mat <- as.matrix(occ_wet)

foldsw <- createFolds(1:nrow(occ_wet_mat), k = 2)  # small example with 2 points

auc_valuesw <- numeric(length(foldsw))

for(i in 1:length(foldsw)){
  # Row indices
  train_idxw <- setdiff(1:nrow(occ_wet_mat), foldsw[[i]])
  test_idxw  <- foldsw[[i]]
  
  # Subset as matrices
  train_points_matw <- occ_wet_mat[train_idxw, , drop = FALSE]  # keep 2D structure
  test_points_matw  <- occ_wet_mat[test_idxw, , drop = FALSE]
  
  # Fit MaxEnt
  modelw <- maxent(x = wet_stack, p = train_points_matw, factors = "landuse_code")
  
  # Background points as matrix
  bg_pointsw <- randomPoints(wet_stack, n = 10000)
  
  # Evaluate model on test points
  eval_objw <- evaluate(p = test_points_matw, a = bg_pointsw, model = modelw, x = wet_stack)
  
  auc_valuesw[i] <- eval_objw@auc
}

mean(auc_valuesw)

# Simple ROC plot with AUC
plot(eval_objw, 'ROC', main = paste0("ROC Curve (AUC = ", round(eval_objw@auc, 3), ")"))


##Improve on ROC Curve to show folds. 
roc_list <- list()  # create empty list

for(i in 1:length(foldsw)) {
  # Row indices
  train_idxw <- setdiff(1:nrow(occ_wet_mat), foldsw[[i]])
  test_idxw  <- foldsw[[i]]
  
  train_points_matw <- occ_wet_mat[train_idxw, , drop = FALSE]
  test_points_matw  <- occ_wet_mat[test_idxw, , drop = FALSE]
  
  modelw <- maxent(x = wet_stack, p = train_points_matw, factors = "landuse")
  bg_pointsw <- randomPoints(wet_stack, n = 10000)
  
  eval_objw <- evaluate(p = test_points_matw, a = bg_pointsw, model = modelw, x = wet_stack)
  
  # Create ROC object for this fold
  response <- c(rep(1, length(eval_objw@presence)), rep(0, length(eval_objw@absence)))
  predictor <- c(eval_objw@presence, eval_objw@absence)
  
  roc_list[[i]] <- roc(response = response, predictor = predictor)
}

# Now your roc_list exists, you can create roc_df_all
roc_df_all <- do.call(rbind, lapply(1:length(roc_list), function(i) {
  data.frame(
    FPR = 1 - roc_list[[i]]$specificities,  # FPR = 1 - specificity
    TPR = roc_list[[i]]$sensitivities,
    Fold = paste0("Fold ", i)
  )
}))


ggplot(roc_df_all, aes(x = FPR, y = TPR, color = Fold)) +
  geom_step(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(
    title = "Cross-validated ROC Curves",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))





mean_auc <- mean(auc_valuesw, na.rm = TRUE)
sd_auc   <- sd(auc_valuesw, na.rm = TRUE)

# Plot ROC curves for each fold
plot(roc_list[[1]], main = "Cross-validated ROC (k=2)")
if (length(roc_list) > 1) {
  for (i in 2:length(roc_list)) plot(roc_list[[i]], add = TRUE, col = i)
}

legend("bottomright",
       legend = sprintf("Mean AUC = %.3f ± %.3f (SD)", mean_auc, sd_auc),
       bty = "n")





























library(pROC)

response <- c(rep(1, length(eval_objw@presence)), rep(0, length(eval_objw@absence)))
predictor <- c(eval_objw@presence, eval_objw@absence)

roc_obj <- roc(response = response, predictor = predictor)

plot.roc(roc_obj, main = paste0("ROC Curve (AUC = ", round(auc(roc_obj),3), ")"))



library(dismo)
library(ggplot2)
library(dplyr)

# eval_objw = your ModelEvaluation object from MaxEnt (dry or wet)
# Convert to a data frame for ggplot2
roc_df <- data.frame(
  FPR = eval_objw@FPR,  # 1 - specificity = false positive rate
  TPR = eval_objw@TPR       # true positive rate
)

ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(
    title = paste0("ROC Curve (AUC = ", round(eval_objw@auc, 3), ")"),
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal(base_size = 14)



library(pROC)

roc_list <- list()  # create empty list

for(i in 1:length(foldsw)) {
  # Row indices
  train_idxw <- setdiff(1:nrow(occ_wet_mat), foldsw[[i]])
  test_idxw  <- foldsw[[i]]
  
  train_points_matw <- occ_wet_mat[train_idxw, , drop = FALSE]
  test_points_matw  <- occ_wet_mat[test_idxw, , drop = FALSE]
  
  modelw <- maxent(x = wet_stack, p = train_points_matw, factors = "landuse_code")
  bg_pointsw <- randomPoints(wet_stack, n = 10000)
  
  eval_objw <- evaluate(p = test_points_matw, a = bg_pointsw, model = modelw, x = wet_stack)
  
  # Create ROC object for this fold
  response <- c(rep(1, length(eval_objw@presence)), rep(0, length(eval_objw@absence)))
  predictor <- c(eval_objw@presence, eval_objw@absence)
  
  roc_list[[i]] <- roc(response = response, predictor = predictor)
}

# Now your roc_list exists, you can create roc_df_all
roc_df_all <- do.call(rbind, lapply(1:length(roc_list), function(i) {
  data.frame(
    FPR = 1 - roc_list[[i]]$specificities,  # FPR = 1 - specificity
    TPR = roc_list[[i]]$sensitivities,
    Fold = paste0("Fold ", i)
  )
}))


ggplot(roc_df_all, aes(x = FPR, y = TPR, color = Fold)) +
  geom_step(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(
    title = "Cross-validated ROC Curves",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
