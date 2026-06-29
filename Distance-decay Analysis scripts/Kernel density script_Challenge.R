source("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/functions.R")

library(haven)

##Read in Challenge breeding sites
##Wet
lav_df_hh_int_c <-  st_read("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/Challenge_wetseason arval data.gpkg")

##Dry
lav_dfdry_hh_int_c <- st_read("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/Challenge dryseason larval data.shp")

##Challenge  analysis

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

##Extract for Challenge alone
household_sum_dfdry_int_c <- st_intersection(household_sum_dfdry, df_ib_c)


##Plot location of households
ggplot(df_ib_c) +
  geom_sf(fill= "NA")+
  geom_sf(data = household_sum_dfdry_int_c, aes(color = Malaria_Positive_HH), size = 1, alpha = 0.5)+
  scale_color_manual(values = c(Negative = "seagreen", Positive = "red"))+
  # scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
  # geom_text_repel(
  #   data = df_ib,
  #   aes(label =  `WardName`, geometry = geometry),color ='black',
  #   stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  # guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Household malaria status")+
  coord_sf()


st_write(household_sum_dfdry_int_c, file.path(Lavplotsdir, "household_sum_dfdry_int_c.shp"))
          

#Read in Ibadan Household data
ib_hh_df <- read_dta(file.path(LuPDir , "IB Wet season household data_edited.dta"))

ib_mal_hh_df <- read_dta(file.path(LuPDir , "IB Wet season household malaria screening.dta"))

##Merge Malaria Screening data
ib_all_wetdata <- right_join(ib_mal_hh_df, ib_hh_df, by = "sn")

##Create dataset for analysis
household_sum_df <- ib_all_wetdata %>%
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

household_sum_df <- household_sum_df  %>% 
  filter(!is.na(latitude) & !is.na(longitude))

household_sum_df <-    st_as_sf(household_sum_df, coords = c("longitude", "latitude"), crs = 4326)

st_crs(df_ib) <- 4326
st_crs(household_sum_df) <- 4326

household_sum_df <- st_transform(household_sum_df, st_crs(df_ib))

##Extract for Challenge alone
household_sum_df_int_c <- st_intersection(household_sum_df, df_ib_c)

#Label household data for Challenge
household_sum_dfdry_int_c$season <- "dry"

household_sum_df_int_c$season <- "wet"

#Merge all Challenge data
# library(dplyr)
# 
# household_sum_dfdry_int_c <- household_sum_dfdry_int_c %>%
#   rename(
#     n_tested = n_testd,
#     n_positive = n_postv,
#     test_positivity_rate = tst_ps_,
#     Malaria_Positive_HH = Ml_P_HH,
#     StateCode = StateCd,
#     WardCode = WardCod,
#     WardName = WardNam,
#     Timestamp = Timstmp,
#     GlobalID = GloblID,
#     AMAPCODE = AMAPCOD
#   )


hh_sum_chal <- bind_rows(household_sum_dfdry_int_c, household_sum_df_int_c)


##Create convex hull around Challenge breeding sites
##Wet
lav_df_hh_int_c   <- st_transform(lav_df_hh_int_c, 32631)
household_sum_df_int_c <- st_transform(household_sum_df_int_c, 32631)

ch_breeding_hull <- lav_df_hh_int_c %>%
  st_union() %>%
  st_convex_hull()

##Align CRS
ch_breeding_hull <- st_transform(ch_breeding_hull, st_crs(household_sum_df_int_c))


##Extract households within hull of Challenge breeding sites
households_in_hull_ch <- household_sum_df_int_c[st_within(household_sum_df_int_c,
                                                             ch_breeding_hull, sparse = FALSE), ]

st_write(households_in_hull_ch, file.path(Lavplotsdir, "households_in_hull_ch.shp"))


##Dry
##Read in necessary files
household_sum_dfdry_int_c <- st_read("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/projects/Manuscripts/ongoing/Larviciding Manuscript/New Manuscript Sections/household_sum_dfdry_int_c.shp")

lav_dfdry_hh_int_c   <- st_transform(lav_dfdry_hh_int_c, 32631)
household_sum_dfdry_int_c <- st_transform(household_sum_dfdry_int_c, 32631)

ch_breeding_hull_d <- lav_dfdry_hh_int_c %>%
  st_union() %>%
  st_convex_hull()

##Align CRS
ch_breeding_hull_d <- st_transform(ch_breeding_hull_d, st_crs(household_sum_dfdry_int_c))


##Extract households within hull of Challenge breeding sites
households_in_hull_ch_d <- household_sum_dfdry_int_c[st_within(household_sum_dfdry_int_c,
                                                          ch_breeding_hull_d, sparse = FALSE), ]

st_write(households_in_hull_ch_d, file.path(Lavplotsdir, "households_in_hull_ch_dry.shp"))


##Plot convex hull extent and households
##Fix geometry
ch_breeding_hull <- st_transform(ch_breeding_hull, st_crs(df_ib_c))
households_in_hull_ch <- st_transform(households_in_hull_ch, st_crs(df_ib_c))
lav_df_hh_int_c <- st_transform(lav_df_hh_int_c, st_crs(df_ib_c))

ch_breeding_hull_d <- st_transform(ch_breeding_hull_d, st_crs(df_ib_c))
households_in_hull_ch_d <- st_transform(households_in_hull_ch_d, st_crs(df_ib_c))
lav_dfdry_hh_int_c <- st_transform(lav_dfdry_hh_int_c, st_crs(df_ib_c))

#Basic R plot
plot(st_geometry(df_ib_c), col = "lightgrey", main = "Hull Coverage")
plot(st_geometry(ch_breeding_hull),
     col = adjustcolor("blue", alpha.f = 0.4),
     border = "blue",
     add = TRUE)
plot(st_geometry(households_in_hull_ch),
     col = "brown",
     pch = 20,
     add = TRUE)
plot(st_geometry(lav_df_hh_int_c),
     col = "yellow",
     pch = 20,
     add = TRUE)
plot(st_geometry(ch_breeding_hull_d),
     col = adjustcolor("lightblue", alpha.f = 0.4),
     border = "black",
     add = TRUE)
plot(st_geometry(households_in_hull_ch_d),
     col = "tomato",
     pch = 20,
     add = TRUE)
plot(st_geometry(lav_dfdry_hh_int_c),
     col = "green",
     pch = 20,
     add = TRUE)



## Continuous distance-decay exposure model
## Wet season only****
## Exposure = distance-decayed proximity to Anopheles-positive larval habitats

library(sf)
library(dplyr)
library(purrr)
library(ggplot2)
library(units)

## ------------------------------------------------------------
## 1. Prepare spatial data
## ------------------------------------------------------------
#Read in necessary files
households_in_hull_ch <- st_read("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/projects/Manuscripts/ongoing/Larviciding Manuscript/New Manuscript Sections/households_in_hull_ch.shp")
lav_df_hh_int_c <-  st_read("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/Challenge_wetseason arval data.gpkg")

# Project to UTM Zone 31N so distances are in meters
hh_mc <- st_transform(households_in_hull_ch, 32631)
larva_mc <- st_transform(lav_df_hh_int_c, 32631)

# Create binary malaria outcome
hh_mc <- hh_mc %>%
  mutate(
    malaria_bin = ifelse(Ml_P_HH == "Positive", 1, 0)
  )

# Keep only Anopheles-positive larval habitats
larva_wet_c <- larva_mc 
# %>%
#   filter(Anopheles_Caught == "Yes")

# Wet-season households
hh_wet_c <- hh_mc

# Check sample sizes
nrow(hh_wet_c)
nrow(larva_wet_c)

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
## 3. Fit wet-season kernel model
## ------------------------------------------------------------

fit_kernel_wet <- function(hh_sf,
                           larva_sf,
                           lambda_grid = seq(2, 500, by = 2),
                           exposure_type = "mean",
                           season_name = "Wet") {
  
  # Distance matrix: rows = households, columns = larval habitats
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
  
  list(
    results = results,
    best = best
  )
}



## ------------------------------------------------------------
## 4. Run wet-season model
## ------------------------------------------------------------

lambda_grid <- seq(2, 500, by = 2)

wet_model_c <- fit_kernel_wet(
  hh_sf = hh_wet_c,
  larva_sf = larva_wet_c,
  lambda_grid = lambda_grid,
  exposure_type = "mean"
)

kernel_results_c <- wet_model_c$results

##Create Dataset for combined Fig 5B
kernel_results_c <- kernel_results_c %>%
  mutate(
    study_source = case_when(
      season == "Wet" ~ "Challenge_wet",
      TRUE ~ NA_character_
    )
  )
best_lambda_c <- wet_model_c$best

best_lambda_c

best_lambdas_c <- best_lambda_c %>%
  mutate(
    study_source = case_when(
      season == "Wet" ~ "Challenge_wet",
      TRUE ~ NA_character_
    )
  )

##write to file
write.csv(kernel_results_c, file.path(Lavplotsdir, "kernel_results_challenge.csv"))
write.csv(best_lambdas_c, file.path(Lavplotsdir, "best_lambdas_challenge.csv"))

kernel_results_c <- read.csv(file.path(Lavplotsdir, "kernel_results_challenge.csv"))
best_lambdas_c <- read.csv(file.path(Lavplotsdir, "best_lambdas_challenge.csv"))
## ------------------------------------------------------------
## 5. Plot AIC
## ------------------------------------------------------------

ggplot(kernel_results_c, aes(x = lambda, y = AIC)) +
  geom_line(linewidth = 1.2, color = "firebrick") +
  geom_vline(
    data = best_lambdas_c,
    aes(xintercept = lambda),
    linetype = "dashed",
    color = "firebrick"
  ) +
  theme_manuscript() +
  labs(
    x = "Distance-decay scale, lambda (meters)",
    y = "AIC",
    title = "Wet-season kernel distance-decay model fit"
  )

## ------------------------------------------------------------
## 6. Plot odds ratio
## ------------------------------------------------------------

ggplot(kernel_results_c, aes(x = lambda, y = OR)) +
  geom_line(linewidth = 1.2, color = "firebrick") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(
    data = best_lambdas_c,
    aes(xintercept = lambda),
    linetype = "dashed",
    color = "firebrick"
  ) +
  coord_cartesian(ylim = c(1.0, 1.7)) +
  theme_manuscript() +
  labs(
    x = "Distance-decay scale, lambda (meters)",
    y = "Odds ratio per 1 SD increase in exposure",
    title = "Wet-season larval habitat exposure and malaria risk"
  )



## Continuous distance-decay exposure model
## Dry season only****
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
hh_mc_d <- st_transform(households_in_hull_ch_d, 32631)
larva_mc_d <- st_transform(lav_dfdry_hh_int_c, 32631)

# Create binary malaria outcome
hh_mc_d <- hh_mc_d %>%
  mutate(
    malaria_bin = ifelse(Ml_P_HH == "Positive", 1, 0)
  )

# Keep only Anopheles-positive larval habitats
larva_dry_c <- larva_mc_d 
# %>%
#   filter(Anopheles_Caught == "Yes")

# dry-season households
hh_dry_c <- hh_mc_d

# Check sample sizes
nrow(hh_dry_c)
nrow(larva_dry_c)

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
                           season_name = "dry") {
  
  # Distance matrix: rows = households, columns = larval habitats
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
  
  list(
    results = results,
    best = best
  )
}



## ------------------------------------------------------------
## 4. Run dry-season model
## ------------------------------------------------------------

lambda_grid <- seq(2, 500, by = 2)

dry_model_c <- fit_kernel_dry(
  hh_sf = hh_dry_c,
  larva_sf = larva_dry_c,
  lambda_grid = lambda_grid,
  exposure_type = "mean"
)

kernel_results_c_d <- dry_model_c$results

##Create Dataset for combined Fig 5B
kernel_results_c_d <- kernel_results_c_d %>%
  mutate(
    study_source = case_when(
      season == "dry" ~ "Challenge_dry",
      TRUE ~ NA_character_
    )
  )
best_lambda_c_d <- dry_model_c$best

best_lambda_c_d

best_lambdas_c_d <- best_lambda_c_d %>%
  mutate(
    study_source = case_when(
      season == "dry" ~ "Challenge_dry",
      TRUE ~ NA_character_
    )
  )

##write to file
write.csv(kernel_results_c_d, file.path(Lavplotsdir, "kernel_results_drychallenge.csv"))
write.csv(best_lambdas_c_d, file.path(Lavplotsdir, "best_lambdas_drychallenge.csv"))


## ------------------------------------------------------------
## 5. Plot AIC
## ------------------------------------------------------------

ggplot(kernel_results_c_d, aes(x = lambda, y = AIC)) +
  geom_line(linewidth = 1.2, color = "firebrick") +
  geom_vline(
    data = best_lambda_c_d,
    aes(xintercept = lambda),
    linetype = "dashed",
    color = "firebrick"
  ) +
  theme_manuscript() +
  labs(
    x = "Distance-decay scale, lambda (meters)",
    y = "AIC",
    title = "dry-season kernel distance-decay model fit"
  )

## ------------------------------------------------------------
## 6. Plot odds ratio
## ------------------------------------------------------------

ggplot(kernel_results_c_d, aes(x = lambda, y = OR)) +
  geom_line(linewidth = 1.2, color = "firebrick") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(
    data = best_lambdas_c_d,
    aes(xintercept = lambda),
    linetype = "dashed",
    color = "firebrick"
  ) +
  coord_cartesian(ylim = c(1.0, 1.7)) +
  theme_manuscript() +
  labs(
    x = "Distance-decay scale, lambda (meters)",
    y = "Odds ratio per 1 SD increase in exposure",
    title = "dry-season larval habitat exposure and malaria risk"
  )
