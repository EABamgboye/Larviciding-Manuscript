# =========================================
# MaxEnt for Tiny Dataset (17 points total)
# =========================================

library(raster)
library(dismo)
library(ggplot2)
library(dplyr)
library(tidyr)

# ------------------------------
# 1. Assume wet & dry stacks are loaded
# ------------------------------
# wet_stack, dry_stack are RasterStacks
# Example: wet_stack <- stack("EVI_wet.tif", "NDWI_wet.tif", ...)
#          dry_stack <- stack("EVI_dry.tif", "NDWI_dry.tif", ...)

# ------------------------------
# 2. Add season as categorical layer
# ------------------------------
library(raster)

# Create a new raster for season (same extent/resolution as wet_stack)
season_wet <- raster(wet_stack)       # copy structure of wet_stack
season_wet[] <- 1                     # 1 = wet

season_dry <- raster(dry_stack)       # copy structure of dry_stack
season_dry[] <- 0                     # 0 = dry

# Add as new layer to stacks
wet_stack <- addLayer(wet_stack, season_wet)
dry_stack <- addLayer(dry_stack, season_dry)

# Rename layer
names(wet_stack)[nlayers(wet_stack)] <- "season"
names(dry_stack)[nlayers(dry_stack)] <- "season"

#Identify duplicate names excluding 'season'
dup_names <- setdiff(intersect(names(wet_stack), names(dry_stack)), "season")

# Drop duplicates from dry_stack but keep 'season'
dry_unique <- dropLayer(dry_stack, which(names(dry_stack) %in% dup_names))

# Combine stacks
combined_stack <- stack(wet_stack, dry_unique)
# ------------------------------
# 3. Combine stacks
# ------------------------------
names(combined_stack) <- make.names(names(combined_stack))

# ------------------------------
# 4. Combine occurrence points
# ------------------------------
# occ_wet and occ_dry: data.frames with X,Y coordinates
# Ensure occurrence points are data.frames
occ_wet <- as.data.frame(occ_wet)
occ_dry <- as.data.frame(occ_dry)

occ_wet$season <- 1
occ_dry$season <- 0
occ_combined <- rbind(occ_wet, occ_dry)
occ_coords <- as.matrix(occ_combined[, c("X", "Y")])

# ------------------------------
# 5. Define categorical variables
# ------------------------------
categorical_vars <- c("season.1", "season.2", "landuse_code")  # include season + any categorical layers

# ------------------------------
# 6. Fit MaxEnt model
# ------------------------------
model_full <- maxent(
  x = wet_stack,
  p = occ_wet,
  factors = "landuse"
)

cat("MaxEnt fit successfully.\n")

# ------------------------------
# 7. Compute permutation importance ± simple error bars
# ------------------------------
# With tiny n, bootstrapping presences is unstable, so we approximate via random subsets of background
n_boot <- 20  # small number due to tiny dataset
var_names <- names(wet_stack)
perm_importance_boot <- matrix(NA, nrow = n_boot, ncol = length(var_names))
colnames(perm_importance_boot) <- var_names

for(b in 1:n_boot){
  bg_points <- randomPoints(wet_stack, n = 1000)
  eval_obj <- evaluate(p = occ_wet, a = bg_points, model = maxent_wet, x = wet_stack)
  
  for(v in var_names){
    colname <- paste0(v, ".permutation.importance")
    if(colname %in% rownames(maxent_wet@results)){
      perm_importance_boot[b, v] <- maxent_wet@results[colname, 1]
    }
  }
}

perm_summary <- data.frame(
  Variable = var_names,
  Perm_Mean = colMeans(perm_importance_boot, na.rm=TRUE),
  Perm_Lower = apply(perm_importance_boot, 2, function(x) quantile(x, 0.025, na.rm=TRUE)),
  Perm_Upper = apply(perm_importance_boot, 2, function(x) quantile(x, 0.975, na.rm=TRUE))
)

# Select top 5 variables by mean permutation importance
top5_vars <- perm_summary %>% arrange(desc(Perm_Mean)) %>% slice(1:5) %>% pull(Variable)

# ------------------------------
# 8. Generate response curves for top 5 variables
# ------------------------------
response_data <- data.frame()
for(var in top5_vars){
  if(var %in% categorical_vars){
    vals <- unique(wet_stack[[var]][])
    vals <- vals[!is.na(vals)]
  } else {
    vals <- seq(cellStats(wet_stack[[var]], min, na.rm=TRUE),
                cellStats(wet_stack[[var]], max, na.rm=TRUE),
                length.out = 50)
  }
  
  for(v in vals){
    # Create mean raster values for other variables
    newdata <- as.data.frame(t(cellStats(wet_stack, stat='mean', na.rm=TRUE)))
    colnames(newdata) <- names(wet_stack)
    newdata[, var] <- v
    
    pred <- predict(maxent_wet, newdata)
    response_data <- rbind(response_data,
                           data.frame(Variable = var, Value = v, Predicted = mean(pred, na.rm=TRUE)))
  }
}

# ------------------------------
# 9. Plot response curves
# ------------------------------
response_data$Variable <- factor(response_data$Variable, levels = top5_vars)

ggplot() +
  # Numeric variables
  geom_line(data = response_data %>% filter(!Variable %in% categorical_vars),
            aes(x = Value, y = Predicted),
            color = "steelblue", size = 1) +
  geom_point(data = response_data %>% filter(!Variable %in% categorical_vars),
             aes(x = Value, y = Predicted),
             color = "steelblue", size = 1) +
  # Categorical variables (season/landuse)
  geom_point(data = response_data %>% filter(Variable %in% categorical_vars),
             aes(x = factor(Value), y = Predicted),
             color = "darkred", size = 3) +
  facet_wrap(~Variable, scales = "free_x") +
  labs(
    title = "MaxEnt Response Curves (Top 5 Variables, Combined Wet/Dry)",
    x = "Variable Value",
    y = "Predicted Probability of Presence"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

# ------------------------------
# 10. Optional: Plot permutation importance ± CI
# ------------------------------
library(ggplot2)
library(dplyr)

# Define categorical variables
categorical_vars <- c("landuse_code", "season.1", "season.2")

# Ensure categorical variables are factors
response_data <- response_data %>%
  mutate(
    Value = ifelse(Variable %in% categorical_vars, as.factor(Value), as.numeric(Value))
  )
# Split numeric and categorical data first
response_numeric <- response_data %>% filter(!Variable %in% categorical_vars) %>%
  mutate(Value = as.numeric(Value))

response_categorical <- response_data %>% filter(Variable %in% categorical_vars) %>%
  mutate(Value = factor(Value))

response_numeric <- response_data %>%
  filter(!Variable %in% categorical_vars)

response_categorical <- response_data %>%
  filter(Variable %in% categorical_vars)
# Plot
ggplot() +
  # Numeric variables: lines + points
  geom_line(data = response_numeric, aes(x = Value, y = Predicted), color = "steelblue", size = 1) +
  geom_point(data = response_numeric, aes(x = Value, y = Predicted), color = "steelblue", size = 1) +
  # Categorical variables: points only
  geom_point(data = response_categorical, aes(x = Value, y = Predicted), color = "darkred", size = 3) +
  facet_wrap(~Variable, scales = "free_x") +
  labs(
    title = "MaxEnt Response Curves (Top Variables, Combined Wet/Dry)",
    x = "Variable Value",
    y = "Predicted Probability of Presence"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )
