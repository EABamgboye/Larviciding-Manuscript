source("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/functions.R")

library(haven)

##Import dry season household data
ib_hh_dfdry <- read_dta("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Ibadan Dry Season data_latest_Nov24/Ibadan Dry season survey data/IB dry season hhold data_edited_131124.dta")

ib_mal_hh_dfdry <- read_dta("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Ibadan Dry Season data_latest_Nov24/Ibadan Dry season survey data/IB dry season hhold list wt RDT_131124.dta")

##Merge Malaria Screening data
ib_all_drydata <- right_join(ib_mal_hh_dfdry, ib_hh_dfdry, by = "sn")

##Create dataset for analysis
household_sum_dfdry <- ib_all_drydata %>%
  group_by(sn) %>%
  summarise(
    longitude = first(bi7_long),
    latitude  = first(bi7_lat),
    ward = first(bi2),
    n_tested = n(),  # number of people in household
    n_positive = sum(q302 == 1, na.rm = TRUE),  # number of positive cases
    test_positivity_rate = (n_positive / n_tested) * 100,  # positivity rate (%)
    Malaria_Positive_HH = ifelse(n_positive > 0, "Positive", "Negative")  # household status
  )


household_sum_dfdry <- household_sum_dfdry  %>% 
  filter(!is.na(latitude) & !is.na(longitude))

household_sum_dfdry <-    st_as_sf(household_sum_dfdry, coords = c("longitude", "latitude"), crs = 4326)

st_crs(df_ib)
st_crs(household_sum_dfdry)

st_crs(df_ib) <- 4326
st_crs(household_sum_dfdry) <- 4326

st_crs(df_ib) <- 4326  
household_sum_dfdry <- st_transform(household_sum_dfdry, st_crs(df_ib))

household_sum_dfdry_int <- st_intersection(household_sum_dfdry, df_ib)

##Extract for Olopomewa alone
household_sum_dfdry_int_o <- st_intersection(household_sum_dfdry, df_ib_o)

##Read in Olopomewa breeding sites
lav_df_hh_int_o <-  st_read("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/Olopomewa_Lavsites_Dry.shp")

##Create convex hull around Agugu breeding sites
lav_df_hh_int_o   <- st_transform(lav_df_hh_int_o, 32631)
household_sum_dfdry_int_o <- st_transform(household_sum_dfdry_int_o, 32631)

ol_breeding_hull <- lav_df_hh_int_o %>%
  st_union() %>%
  st_convex_hull()

##Align CRS
ol_breeding_hull <- st_transform(ol_breeding_hull, st_crs(household_sum_dfdry_int_o))


##Extract households within hull of Agugu breeding sites
households_in_hull_ol <- household_sum_dfdry_int_o[st_within(household_sum_dfdry_int_o,
                                                ol_breeding_hull, sparse = FALSE), ]



write.csv(households_in_hull_ol, file.path(Lavplotsdir,"households_in_hull_ol.csv"))
          
##Plot convex hull extent and households
##Fix geometry
ol_breeding_hull <- st_transform(ol_breeding_hull, st_crs(df_ib_o))
households_in_hull_ol <- st_transform(households_in_hull_ol, st_crs(df_ib_o))
lav_df_hh_int_o <- st_transform(lav_df_hh_int_o, st_crs(df_ib_o))

#Basic R plot
plot(st_geometry(df_ib_o), col = "lightgrey", main = "Hull Coverage")
plot(st_geometry(ol_breeding_hull),
     col = adjustcolor("blue", alpha.f = 0.4),
     border = "blue",
     add = TRUE)
plot(st_geometry(households_in_hull_ol),
     col = "brown",
     pch = 20,
     add = TRUE)
plot(st_geometry(lav_df_hh_int_o),
     col = "yellow",
     pch = 20,
     add = TRUE)


## Continuous distance-decay exposure model
## Dry season only
## Exposure = distance-decayed proximity to Anopheles-positive larval habitats

library(sf)
library(dplyr)
library(purrr)
library(ggplot2)
library(units)

## ------------------------------------------------------------
## 1. Prepare spatial data
## ------------------------------------------------------------

# Project to UTM Zone 31N so distances are in meters
hh_mo <- st_transform(households_in_hull_ol, 32631)
larva_moo <- st_transform(lav_df_hh_int_o, 32631)

# Create binary malaria outcome
hh_mo <- hh_mo %>%
  mutate(
    malaria_bin = ifelse(Malaria_Positive_HH == "Positive", 1, 0)
  )

# Keep only Anopheles-positive larval habitats
larva_dry_o <- larva_moo %>%
  filter(Anphl_C == "Yes")

# Dry-season households
hh_dry_o <- hh_mo

# Check sample sizes
nrow(hh_dry_o)
nrow(larva_dry_o)

## ------------------------------------------------------------
## 2. Kernel exposure function
## ------------------------------------------------------------

kernel_exposure <- function(lambda, dist_mat, exposure_type = "mean") {
  
  weights <- exp(-dist_mat / lambda)
  
  if (exposure_type == "sum") {
    exposure <- rowSums(weights)
  } else if (exposure_type == "mean") {
    exposure <- rowMeans(weights)
  } else {
    stop("exposure_type must be either 'sum' or 'mean'")
  }
  
  return(exposure)
}

## ------------------------------------------------------------
## 3. Fit dry-season kernel model
## ------------------------------------------------------------

fit_kernel_dry <- function(hh_sf,
                           larva_sf,
                           lambda_grid = seq(2, 500, by = 2),
                           exposure_type = "mean",
                           season_name = "Dry") {
  
  # Distance matrix: rows = households, columns = larval habitats
  dist_mat <- st_distance(hh_sf, larva_sf)
  dist_mat <- units::drop_units(dist_mat)
  
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
  
  list(
    results = results,
    best = best
  )
}


## ------------------------------------------------------------
## 4. Run dry-season model
## ------------------------------------------------------------

lambda_grid <- seq(2, 500, by = 2)

dry_model_o <- fit_kernel_dry(
  hh_sf = hh_dry_o,
  larva_sf = larva_dry_o,
  lambda_grid = lambda_grid,
  exposure_type = "mean"
)

kernel_results_o <- dry_model_o$results
best_lambda_o <- dry_model_o$best

best_lambda_o


##Create Dataset for combined Fig 5B
kernel_results_o <- kernel_results_o %>%
  mutate(
    study_source = case_when(
      season == "Dry" ~ "Olopomewa_dry",
      TRUE ~ NA_character_
    )
  )


best_lambdas_o <- best_lambda_o %>%
  mutate(
    study_source = case_when(
      season == "Dry" ~ "Olopomewa_dry",
      TRUE ~ NA_character_
    )
  )

##write to file
write.csv(kernel_results_o, file.path(Lavplotsdir, "kernel_results_olopomewa.csv"))
write.csv(best_lambdas_o, file.path(Lavplotsdir, "best_lambdas_olopomewa.csv"))


#Read in files
kernel_results_o <- read_csv(file.path(Lavplotsdir, "kernel_results_olopomewa.csv"))
best_lambdas_o <- read_csv(file.path(Lavplotsdir, "best_lambdas_olopomewa.csv"))


## ------------------------------------------------------------
## 5. Plot AIC
## ------------------------------------------------------------

ggplot(kernel_results_o, aes(x = lambda, y = AIC)) +
  geom_line(linewidth = 1.2, color = "firebrick") +
  geom_vline(
    data = best_lambdas_o,
    aes(xintercept = lambda),
    linetype = "dashed",
    color = "firebrick"
  ) +
  theme_manuscript() +
  labs(
    x = "Distance-decay scale, lambda (meters)",
    y = "AIC",
    title = "Dry-season kernel distance-decay model fit"
  )

## ------------------------------------------------------------
## 6. Plot odds ratio
## ------------------------------------------------------------

ggplot(kernel_results_o, aes(x = lambda, y = OR)) +
  geom_line(linewidth = 1.2, color = "firebrick") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(
    data = best_lambdas_o,
    aes(xintercept = lambda),
    linetype = "dashed",
    color = "firebrick"
  ) +
  coord_cartesian(ylim = c(1.0, 1.5)) +
  theme_manuscript() +
  labs(
    x = "Distance-decay scale, lambda (meters)",
    y = "Odds ratio per 1 SD increase in exposure",
    title = "Dry-season larval habitat exposure and malaria risk"
  )

kernel_results_o <- kernel_results_o %>%
  mutate(
    OR_raw = exp(beta),
    OR_1SD = exp(beta * exposure_sd),
    OR_1SD_low95 = exp((beta - 1.96 * SE) * exposure_sd),
    OR_1SD_high95 = exp((beta + 1.96 * SE) * exposure_sd)
  )
