# 5/5/2026
## Continuous distance-decay exposure model using density kernel
## Assumption: all sampled aquatic habitats are potential Anopheles breeding sites

library(sf)
library(dplyr)
library(purrr)
library(ggplot2)
library(units)
library(purrr)
library(ggplot2)
library(units)

##Read in datasets
hh_all_data_ag <- read_csv("hh_all_data_ag.csv")

library(stringr)  # or use base R below

hh_all_data_ag$season_clean <- str_extract(hh_all_data_ag$season, "dry|wet")

# optional: stable factor for tables
hh_all_data_ag$season_clean <- factor(
  hh_all_data_ag$season_clean,
  levels = c("dry", "wet")
)

table(hh_all_data_ag$season_clean, hh_all_data_ag$Malaria_Positive_HH)
## ------------------------------------------------------------
## 1. Prepare spatial data
## ------------------------------------------------------------
lav_all_data_ag <- bind_rows(lav_dry_std_ag, lav_wet_std_ag)

# Project to UTM Zone 31N so distances are in meters
hh_m <- st_transform(hh_all_data_ag, 32631)
larva_m <- st_transform(lav_all_data_ag, 32631)
lav_dry_std_ag <- st_transform(lav_dry_std_ag, 32631)
lav_wet_std_ag <- st_transform(lav_wet_std_ag, 32631)


# Create binary malaria outcome
hh_m <- hh_m %>%
  mutate(
    malaria_bin = ifelse(Malaria_Positive_HH == "Positive", 1, 0)
  )

# Household attribute table
hh_data <- hh_m %>%
  st_drop_geometry() %>%
  dplyr::select(sn, malaria_bin)

## ------------------------------------------------------------
## 2. Kernel exposure function
## ------------------------------------------------------------

kernel_exposure <- function(lambda, dist_mat, exposure_type = "sum") {
  
  weights <- exp(-dist_mat / lambda)
  
  if (exposure_type == "sum") {
    exposure <- rowSums(weights)
  }
  
  if (exposure_type == "mean") {
    exposure <- rowMeans(weights)
  }
  
  return(exposure)
}


## ------------------------------------------------------------
## 3. Function to fit kernel model for one season
## ------------------------------------------------------------

fit_kernel_by_season <- function(hh_sf,
                                 larva_sf,
                                 season_name,
                                 lambda_grid = seq(2, 500, by = 2),
                                 exposure_type = "mean") {
  
  # Distance matrix: rows = households, columns = sampled larval habitats
  dist_mat <- st_distance(hh_sf, larva_sf)
  dist_mat <- units::drop_units(dist_mat)
  
  # Household data
  model_base <- hh_sf %>%
    st_drop_geometry() %>%
    dplyr::select(sn, malaria_bin)
  
  fit_one_lambda <- function(lambda) {
    
    exposure_raw <- kernel_exposure(
      lambda = lambda,
      dist_mat = dist_mat,
      exposure_type = exposure_type
    )
    
    model_data <- model_base %>%
      mutate(
        exposure_raw = exposure_raw,
        exposure = as.numeric(scale(exposure_raw))
      )
    
    fit <- glm(
      malaria_bin ~ exposure,
      data = model_data,
      family = binomial()
    )
    
    fit_summary <- summary(fit)
    
    beta <- coef(fit)[["exposure"]]
    se <- fit_summary$coefficients["exposure", "Std. Error"]
    p_value <- fit_summary$coefficients["exposure", "Pr(>|z|)"]
    
    data.frame(
      season = season_name,
      lambda = lambda,
      logLik = as.numeric(logLik(fit)),
      AIC = AIC(fit),
      beta = beta,
      SE = se,
      OR = exp(beta),
      OR_low95 = exp(beta - 1.96 * se),
      OR_high95 = exp(beta + 1.96 * se),
      p_value = p_value,
      n_households = nrow(model_data),
      n_habitats = ncol(dist_mat)
    )
  }
  
  results <- map_dfr(lambda_grid, fit_one_lambda)
  
  best <- results %>%
    slice_min(AIC, n = 1, with_ties = FALSE)
  
  return(
    list(
      results = results,
      best = best
    )
  )
}


## ------------------------------------------------------------
## 4. Split larval habitats by season
## ------------------------------------------------------------

larva_wet <- lav_wet_std_ag 
# %>%
#   filter(season == "Wet",
#          Anopheles_Caught == "Yes")

larva_dry <- lav_dry_std_ag
# %>%
#   filter(season == "Dry",
#          Anopheles_Caught == "Yes")


hh_wet <- hh_m %>%
  filter(season == "wet")

hh_dry <- hh_m %>%
  filter(season == "dry")

larva_all <- larva_m 
# %>%
#   filter(Anopheles_Caught == "Yes")

## ------------------------------------------------------------
## 5. Fit season-specific models
## ------------------------------------------------------------

lambda_grid <- seq(2, 500, by = 2)

wet_model <- fit_kernel_by_season(
  hh_sf = hh_wet,
  larva_sf = larva_wet,
  season_name = "Wet",
  lambda_grid = lambda_grid,
  exposure_type = "mean"
)

dry_model <- fit_kernel_by_season(
  hh_sf = hh_dry,
  larva_sf = larva_dry,
  season_name = "Dry",
  lambda_grid = lambda_grid,
  exposure_type = "mean"
)

all_model <- fit_kernel_by_season(
  hh_sf = hh_m,
  larva_sf = larva_all,
  season_name = "All",
  lambda_grid = lambda_grid,
  exposure_type = "mean"
)

kernel_results <- bind_rows(
  wet_model$results,
  dry_model$results,
  all_model$results
)

best_lambdas <- bind_rows(
  wet_model$best,
  dry_model$best,
  all_model$best
)

best_lambdas
best_lambdas_a <- best_lambdas %>%
  mutate(
    study_source = case_when(
      season == "Wet" ~ "Agugu_wet",
      season == "Dry" ~ "Agugu_dry",
      season == "All" ~ "Agugu_combined",
      TRUE ~ NA_character_
    )
  )

## ------------------------------------------------------------
## 6. Plot AIC by season
## ------------------------------------------------------------

ggplot(kernel_results, aes(x = lambda, y = AIC, color = season)) +
  geom_line(linewidth = 1.2) +
  geom_vline(
    data = best_lambdas,
    aes(xintercept = lambda, color = season),
    linetype = "dashed"
  ) +
  theme_manuscript() +
  labs(
    x = "Distance-decay scale, lambda (meters)",
    y = "AIC",
    title = "Season-specific kernel distance-decay model fit"
  )


##Create Dataset for combined Fig 5B
kernel_results_a <- kernel_results %>%
  mutate(
    study_source = case_when(
      season == "Wet" ~ "Agugu_wet",
      season == "Dry" ~ "Agugu_dry",
      season == "All" ~ "Agugu_combined",
      TRUE ~ NA_character_
    )
  )

## ------------------------------------------------------------
## 7. Plot odds ratio by season
## ------------------------------------------------------------

ggplot(kernel_results_a, aes(x = lambda, y = OR, color = study_source, fill = study_source)) +
  geom_ribbon(
    aes(ymin = OR_low95, ymax = OR_high95),
    alpha = 0.18,
    color = NA
  ) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(
    data = best_lambdas_a,
    aes(xintercept = lambda, color = study_source),
    linetype = "dashed"
  ) +
  coord_cartesian(ylim = c(0.95, 1.5)) +
  theme_manuscript() +
  labs(
    x = "Distance-decay scale, lambda (meters)",
    y = "Odds ratio per 1 SD increase in exposure",
    title = "Kernel-based potential larval habitat exposure and malaria risk"
  )

##write to file
write.csv(kernel_results_a, file.path(Lavplotsdir, "kernel_results_agugu.csv"))
write.csv(best_lambdas_a, file.path(Lavplotsdir, "best_lambdas_agugu.csv"))

kernel_results_a <- read.csv(file.path(Lavplotsdir, "kernel_results_agugu.csv"))
## ------------------------------------------------------------
## 8. Combined model with season adjustment
## ------------------------------------------------------------

chosen_lambda <- wet_model$best$lambda[1]

make_season_exposure <- function(hh_sf, larva_sf, season_name, lambda, exposure_type = "mean") {
  
  dist_mat <- st_distance(hh_sf, larva_sf)
  dist_mat <- units::drop_units(dist_mat)
  
  exposure_raw <- kernel_exposure(
    lambda = lambda,
    dist_mat = dist_mat,
    exposure_type = exposure_type
  )
  
  hh_sf %>%
    st_drop_geometry() %>%
    dplyr::select(sn, malaria_bin) %>%
    mutate(
      season = season_name,
      exposure_raw = exposure_raw
    )
}

combined_data <- bind_rows(
  make_season_exposure(hh_wet, larva_wet, "Wet", chosen_lambda, "mean"),
  make_season_exposure(hh_dry, larva_dry, "Dry", chosen_lambda, "mean"),
  make_season_exposure(hh_m, larva_m, "All", chosen_lambda, "mean")
) %>%
  mutate(
    exposure = as.numeric(scale(exposure_raw)),
    season = factor(season)
  )

combined_fit <- glm(
  malaria_bin ~ exposure + season,
  data = combined_data,
  family = binomial()
)

summary(combined_fit)

exp(coef(combined_fit))


combined_fit_interaction <- glm(
  malaria_bin ~ exposure * season,
  data = combined_data,
  family = binomial()
)

summary(combined_fit_interaction)

anova(combined_fit, combined_fit_interaction, test = "Chisq")




best_lambdas_plot <- best_lambdas_a %>%
  mutate(
    study_source = factor(study_source),
    label = paste0(
      "lambda = ", lambda, " m; ",
      "n = ", n_households, "; habitats = ", n_habitats
    )
  )

ggplot(best_lambdas_plot, aes(x = OR, y = study_source, color = study_source)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey35") +
  geom_errorbarh(
    aes(xmin = OR_low95, xmax = OR_high95),
    height = 0.18,
    linewidth = 0.9
  ) +
  geom_point(size = 3) +
  scale_x_log10() +
  theme_manuscript() +
  labs(
    x = "Odds ratio per 1 SD increase in exposure, log scale",
    y = NULL,
    title = "Association at best-fitting distance-decay scale"
  )


ggplot(kernel_results_a, aes(x = lambda, y = OR, color = study_source)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey35") +
  geom_vline(
    data = best_lambdas_a,
    aes(xintercept = lambda, color = study_source),
    linetype = "dashed"
  ) +
  coord_cartesian(ylim = c(0.85, 1.35)) +
  theme_manuscript() +
  labs(
    x = "Distance-decay scale, lambda (meters)",
    y = "Odds ratio per 1 SD increase in exposure",
    title = "Kernel exposure and malaria risk across distance-decay scales"
  )
