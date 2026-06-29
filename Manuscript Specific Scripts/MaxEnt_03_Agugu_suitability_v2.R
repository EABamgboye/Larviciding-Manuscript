library(raster)
library(dismo)
library(ENMeval)
library(ENMeval)
library(terra)
library(pROC)
source("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/functions.R")

# -------------------------------
# 1️⃣ Inputs
# -------------------------------
# predictors_r : your RasterStack with monthly layers + other predictors
# occ_coords_valid : matrix/data.frame of occurrence points (x/y)
# categorical layers : e.g., landuse_code

# Example:
# predictors_r already contains monthly layers like "EVI_2023_02", "NDWI_2024_06", etc.

# -------------------------------
# 2️⃣ Detect vegetation index layers
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
# 3️⃣ Separate dry vs wet layers
# -------------------------------
# Dry season: Jan-Mar 2023
dry_layers <- layers_df$layer_name[layers_df$type %in% veg_types & layers_df$year == 2023]

# Wet season: Jul-Aug 2024 (or include months in 2024)
wet_layers <- layers_df$layer_name[layers_df$type %in% veg_types & layers_df$year == 2024]

# -------------------------------
# 4️⃣ Split by index for averaging
# -------------------------------
dry_evi  <- dry_layers[grepl("EVI", dry_layers)]
dry_ndwi <- dry_layers[grepl("NDWI", dry_layers)]
dry_ndmi <- dry_layers[grepl("NDMI", dry_layers)]

wet_evi  <- wet_layers[grepl("EVI", wet_layers)]
wet_ndwi <- wet_layers[grepl("NDWI", wet_layers)]
wet_ndmi <- wet_layers[grepl("NDMI", wet_layers)]

# -------------------------------
# 5️⃣ Compute seasonal-average rasters
# -------------------------------
EVI_dry_r  <- if(length(dry_evi) > 1) calc(predictors_r[[dry_evi]], mean, na.rm=TRUE) else predictors_r[[dry_evi]]
NDWI_dry_r <- if(length(dry_ndwi) > 1) calc(predictors_r[[dry_ndwi]], mean, na.rm=TRUE) else predictors_r[[dry_ndwi]]
NDMI_dry_r <- if(length(dry_ndmi) > 1) calc(predictors_r[[dry_ndmi]], mean, na.rm=TRUE) else predictors_r[[dry_ndmi]]

EVI_wet_r  <- if(length(wet_evi) > 1) calc(predictors_r[[wet_evi]], mean, na.rm=TRUE) else predictors_r[[wet_evi]]
NDWI_wet_r <- if(length(wet_ndwi) > 1) calc(predictors_r[[wet_ndwi]], mean, na.rm=TRUE) else predictors_r[[wet_ndwi]]
NDMI_wet_r <- if(length(wet_ndmi) > 1) calc(predictors_r[[wet_ndmi]], mean, na.rm=TRUE) else predictors_r[[wet_ndmi]]

# -------------------------------
# 6️⃣ Identify non-vegetation layers (to keep in both)
# -------------------------------
other_layers <- layers_df$layer_name[layers_df$type == "other"]
other_stack  <- predictors_r[[other_layers]]

# -------------------------------
# 7️⃣ Create final seasonal stacks
# -------------------------------
dry_stack <- stack(EVI_dry_r, NDWI_dry_r, NDMI_dry_r, other_stack)
names(dry_stack) <- c("EVI_dry","NDWI_dry","NDMI_dry", other_layers)

wet_stack <- stack(EVI_wet_r, NDWI_wet_r, NDMI_wet_r, other_stack)
names(wet_stack) <- c("EVI_wet","NDWI_wet","NDMI_wet", other_layers)


combined_stack <- stack(EVI_dry_r, NDWI_dry_r, NDMI_dry_r, 
                        EVI_wet_r, NDWI_wet_r, NDMI_wet_r,  other_stack)
names(combined_stack) <- c("EVI_dry","NDWI_dry","NDMI_dry","EVI_wet","NDWI_wet","NDMI_wet", other_layers)



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
# # 8️⃣ Extract raster values at occurrence points
# # -------------------------------
# occ_vals_dry <- raster::extract(dry_stack, occ_coords_valid)
occ_vals_wet <- raster::extract(wet_stack, occ_wet)
occ_vals_combined <- raster::extract(combined_stack, occ_coords_valid)

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

##Extract Wet season coordinates
occ_wet <- occ_coords_valid[1:15, ]
maxent_wet <- maxent(
  x       = wet_stack,
  p       = occ_wet,
  factors = "landuse"
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

# print(jkaw)

##Plot Permutation importance
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


##Extract and plot response curves

m1a <- jkaw@models[[1]]

# variables used in the MaxEnt model
names(m1a@data)          # often works
# if that is NULL, also try:
colnames(m1a@presence)

vars_of_interesta <- c("distance_to_water.bodies",
                       "Mean.NDWI.May_July.2024.",
                      "land.surface.temperature"
                )# replace with your actual names


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

modelsa <- jkaw@models

par(mfrow = c(1, 3))

for (v in vars_of_interesta) {
  resp_df <- bootstrap_response_mean(modelsa, var = v)
  
  plot(resp_df$x, resp_df$mean, type = "l",
       xlab = v, ylab = "Suitability (bootstrap mean)",
       main = v)
  
  # optional: add ±1 SD as shaded band
  lines(resp_df$x, resp_df$mean + resp_df$sd, lty = 2, col = "grey50")
  lines(resp_df$x, resp_df$mean - resp_df$sd, lty = 2, col = "grey50")
}

par(mfrow = c(1, 1))


##All variables
library(dismo)

library(dismo)
library(raster)

modelsa <- jkaw@models
m1a <- modelsa[[1]]

# 1) variables used in the MaxEnt model
vars_all <- colnames(m1a@presence)
if (is.null(vars_all) || length(vars_all) == 0) {
  vars_all <- colnames(m1a@presence)
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
                                           out_pdf = "maxent_response_curves_all_vars.pdf") {
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

plot_bootstrap_response_panels(vars_to_plot, modelsa, ncol = 3, nrow = 3)


library(dplyr)
library(ggplot2)

# Build full dataset
response_all <- bind_rows(
  lapply(vars_of_interesta, function(v) {
    
    df <- bootstrap_response_mean(modelsa, var = v)
    
    df$Variable <- v
    return(df)
  })
)


# Ensure variable is a factor and reorder
response_all$Variable <- factor(
  response_all$Variable,
  levels = c("distance_to_water.bodies",
    "Mean.NDWI.May_July.2024.",
    "land.surface.temperature" 
  )
)


respa <- ggplot(response_all, aes(x = x, y = mean)) +
  
  # Confidence ribbon
  geom_ribbon(aes(ymin = mean - sd,
                  ymax = mean + sd),
              fill = "steelblue",
              alpha = 0.2) +
  
  # Mean response line
  geom_line(color = "steelblue",
            linewidth = 1.2) +
  
  facet_wrap(~ Variable, scales = "free_x") +
  
  labs(
    x = "Predictor Value",
    y = "Predicted Suitability (Bootstrap Mean)",
    title = "MaxEnt Bootstrap Response Curves"
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

respa

ggsave(paste0(LuDataDir,"/", Sys.Date(), "/", 'Boostrapped Response curves(Agugu).pdf'), respa, width = 8, height = 6)







##----------------------------------------------------------------------------##
##---create another wet stack with only veg layers of month of collection------#
###---------------------------------------------------------------------------##

#wet_evi  <- wet_layers[grepl("EVI", wet_layers)]
wet_evi <- "EVI_2024_07"
wet_ndwi <- "NDWI_2024_07"
wet_ndmi <- "NDMI_2024_07"

EVI_wet_r  <- if(length(wet_evi) > 1) calc(predictors_r[[wet_evi]], mean, na.rm=TRUE) else predictors_r[[wet_evi]]
NDWI_wet_r <- if(length(wet_ndwi) > 1) calc(predictors_r[[wet_ndwi]], mean, na.rm=TRUE) else predictors_r[[wet_ndwi]]
NDMI_wet_r <- if(length(wet_ndmi) > 1) calc(predictors_r[[wet_ndmi]], mean, na.rm=TRUE) else predictors_r[[wet_ndmi]]

wet_stack2 <- stack(EVI_wet_r, NDWI_wet_r, NDMI_wet_r, other_stack)
names(wet_stack2) <- c("EVI_wet","NDWI_wet","NDMI_wet", other_layers)

occ_vals_wet2 <- raster::extract(wet_stack2, occ_wet)

# Fit MaxEnt for wet season
##Rename Wet Stack Raster
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

# print(jkaw)

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
##---create another wet stack with veg layers averaged 2 month before collection------#
###---------------------------------------------------------------------------##

#wet_evi  <- wet_layers[grepl("EVI", wet_layers)]
wet_evi3 <- c("EVI_2024_06","EVI_2024_07")
wet_ndwi3 <- c("NDWI_2024_06","NDWI_2024_07")
wet_ndmi3 <- c("NDMI_2024_06","NDMI_2024_07")

EVI_wet_r3  <- if(length(wet_evi3) > 1) calc(predictors_r[[wet_evi3]], mean, na.rm=TRUE) else predictors_r[[wet_evi3]]
NDWI_wet_r3 <- if(length(wet_ndwi3) > 1) calc(predictors_r[[wet_ndwi3]], mean, na.rm=TRUE) else predictors_r[[wet_ndwi3]]
NDMI_wet_r3 <- if(length(wet_ndmi3) > 1) calc(predictors_r[[wet_ndmi3]], mean, na.rm=TRUE) else predictors_r[[wet_ndmi3]]

wet_stack3 <- stack(EVI_wet_r3, NDWI_wet_r3, NDMI_wet_r3, other_stack)
names(wet_stack3) <- c("EVI_wet","NDWI_wet","NDMI_wet", other_layers)

occ_vals_wet3 <- raster::extract(wet_stack3, occ_wet)

# Fit MaxEnt for wet season
##Rename Wet Stack Raster
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
occ_wet <- occ_coords_valid[1:15, ]
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


##Combined Wet aand Dry Season using Mean of Vegetation indices
# Fit MaxEnt for wet season
##Rename Wet Stack Raster
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
  p       = occ_coords_valid,
  factors = "landuse"
)

##Run Jack Knife Analysis
set.seed(123)
jka_com <- dismo::maxent(
  x = combined_stack, 
  p = occ_coords_valid, 
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
