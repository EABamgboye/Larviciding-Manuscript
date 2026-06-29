#Incorporating Wet and Dry season into Objective 3 of larviciding manuscript
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[//]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
shapefileDir <- "C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan"
Entodir <- "C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento"
Lavplotsdir <- "C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/projects/Manuscripts/ongoing/Larviciding Manuscript/New Manuscript Sections"


library(sf)
library(dplyr)
library(purrr)
library(haven)

##load packages and themes
source("functions.R")


##Refer to script "Breeding site and Household Malaria Status"

##Agugu (Slum) analysis
#Label household data for Agugu
household_sum_dfdry_int_a$season <- "dry"

household_sum_df_int_a$season <- "wet"

#Merge all Agugu data
hh_sum_agugu <- rbind(household_sum_dfdry_int_a, household_sum_df_int_a)

##Plot location of households
ggplot(df_ib_a) +
  geom_sf(fill= "NA")+
  geom_sf(data = hh_sum_agugu, aes(color = Malaria_Positive_HH), size = 1, alpha = 0.5)+
  scale_color_manual(values = c(Negative = "seagreen", Positive = "red"))+
  scale_shape_manual(values = c(dry = 16,  wet= 17))+
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



##Refer to 001 script and bring in larval sites for Agugu
## Dataframe: lav_df_hh_int_a

##Visualize agugu breeding sites
ggplot(df_ib_a) +
  geom_sf(fill= "NA")+
  geom_sf(data = lav_df_hh_int_a_dry, aes(color = Anopheles_Caught), size = 1, alpha = 0.5)+
  scale_color_manual(values = c(No = "lightgreen", Yes = "tomato"))+
  #scale_shape_manual(values = c(dry = 16,  wet= 17))+
  # geom_text_repel(
  #   data = df_ib,
  #   aes(label =  `WardName`, geometry = geometry),color ='black',
  #   stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  # guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Breeding site status")+
  coord_sf()

##Plot location of households incorporating larval habitats
ggplot(df_ib_a) +
  geom_sf(fill= "NA")+
  geom_sf(data = hh_sum_agugu, aes(color = Malaria_Positive_HH, size = 'test_positivity_rate'), alpha = 0.4)+
  scale_color_manual(values = c(Negative = "seagreen", Positive = "tomato"))+
  geom_sf(data = lav_df_hh_int_a_wet,
          aes(shape = Anopheles_Caught, fill = Anopheles_Caught),
          size = 2, color = "black") +
  scale_shape_manual(name = "Presence of Anopheles",
                     values = c(No = 21, Yes = 24)) + 
  geom_sf(data = lav_df_hh_int_a_dry,
          aes(shape = Anopheles_Caught, fill = Anopheles_Caught),
          size = 2, color = "black") +
  scale_shape_manual(name = "Presence of Anopheles",
                     values = c(No = 21, Yes = 24)) +# 21 = filled circle, 24 = triangle
  scale_fill_manual(name = "Presence of Anopheles",
                    values = c(No = "yellow", Yes = "blue")) +
  # #geom_sf_text(data = Ag_gripshp, aes(label = FID), size = 3, color = "black") +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Household malaria and larval breeding site status")+
  coord_sf()


##Create convex hull around Agugu breeding sites
lav_df_hh_int_a_wet   <- st_transform(lav_df_hh_int_a_wet, 32631)
household_sum_df_int_a <- st_transform(household_sum_df_int_a, 32631)

ag_breeding_hull_w <- lav_df_hh_int_a_wet %>%
  st_union() %>%
  st_convex_hull()


ag_breeding_hull_d <- lav_df_hh_int_a_dry %>%
  st_union() %>%
  st_convex_hull()

##Align CRS
ag_breeding_hull_d <- st_transform(ag_breeding_hull_d, st_crs(hh_sum_agugu))
ag_breeding_hull_w <- st_transform(ag_breeding_hull_w, st_crs(hh_sum_agugu))



##Extract households within hull of Agugu breeding sites
households_in_hull_ag_d <- hh_sum_agugu[st_within(hh_sum_agugu,
                                                  ag_breeding_hull_d, sparse = FALSE), ]

write.csv(households_in_hull_ag_d, file = "households_in_hull_ag_d.csv")

households_in_hull_ag_w <- hh_sum_agugu[st_within(hh_sum_agugu,
                                                  ag_breeding_hull_w, sparse = FALSE), ]

write.csv(households_in_hull_ag_w, file = "households_in_hull_ag_w.csv")


##Plot convex hull extent and households
##Fix geometry
ag_breeding_hull_d <- st_transform(ag_breeding_hull_d, st_crs(df_ib_a))
ag_breeding_hull_w <- st_transform(ag_breeding_hull_w, st_crs(df_ib_a))
households_in_hull_ag_d <- st_transform(households_in_hull_ag_d, st_crs(df_ib_a))
households_in_hull_ag_w <- st_transform(households_in_hull_ag_w, st_crs(df_ib_a))
lav_df_hh_int_a_dry <- st_transform(lav_df_hh_int_a_dry, st_crs(df_ib_a))
lav_df_hh_int_a_wet <- st_transform(lav_df_hh_int_a_wet, st_crs(df_ib_a))

#Basic R plot
plot(st_geometry(df_ib_a), col = "lightgrey", main = "Hull Coverage")
plot(st_geometry(ag_breeding_hull_d),
     col = adjustcolor("blue", alpha.f = 0.4),
     border = "blue",
     add = TRUE)
plot(st_geometry(ag_breeding_hull_w),
     col = adjustcolor("lightblue", alpha.f = 0.4),
     border = "lightblue",
     add = TRUE)
plot(st_geometry(households_in_hull_ag_d),
     col = "red",
     pch = 20,
     add = TRUE)
plot(st_geometry(households_in_hull_ag_w),
     col = "brown",
     pch = 20,
     add = TRUE)
plot(st_geometry(lav_df_hh_int_a_dry),
     col = "green",
     pch = 20,
     add = TRUE)
plot(st_geometry(lav_df_hh_int_a_wet),
     col = "yellow",
     pch = 20,
     add = TRUE)


#Merge dataframes for kernel based analysis
##Larval data
lav_wet_std_ag <- lav_df_hh_int_a_wet %>%
  transmute(
    X,
    State,
    Locality = WardName,
    Settlement.Type,
    Breeding_Site_Recode,
    Breeding_Site_Recode2,
    Anopheles_Caught,
    season,
    geom
  )

lav_dry_std_ag <- lav_df_hh_int_a_dry %>%
  dplyr::select(
    X,
    State,
    LGA,
    Community,
    Settlement.Type,
    Breeding_Site_Recode,
    Breeding_Site_Recode2,
    Anopheles_Caught,
    season,
    geom
  )

lav_all_data_ag <- bind_rows(lav_dry_std_ag, lav_wet_std_ag)

write.csv(lav_all_data_ag, file = "lav_all_data_ag.csv")


households_in_hull_ag_w$season <- factor("wet")

#Merge Households in convexhull
hh_all_data_ag <- bind_rows(households_in_hull_ag_d, households_in_hull_ag_w)

hh_all_data_ag_export <- hh_all_data_ag %>%
  mutate(across(where(is.labelled), as_factor))

write.csv(hh_all_data_ag_export, file = "hh_all_data_ag.csv", row.names = FALSE)
write.csv(hh_all_data_ag, file = "hh_all_data_ag.csv")


##Characteristics of individuals tested in surrounding households
##Dry Season
#households_in_hull_ag_d <- read.csv(file = "households_in_hull_ag_d.csv")

hh_members_ag_dry <- ib_all_drydata %>%
  # keep only rows whose sn exists in hh_sum_agugu
  semi_join(households_in_hull_ag_d %>% st_drop_geometry() %>% dplyr::select(sn),
            by = "sn") %>%
  # dplyr::select(sn, hl4, hl5)
##Maintain convex hull geomtry
left_join(
  households_in_hull_ag_d %>%
    dplyr::select(sn, geometry),
  by = "sn"
) %>%
  sf::st_as_sf()


tab_hl4_codes <- table(as.numeric(hh_members_ag_dry$hl4))
prop.table(tab_hl4_codes)

tab_hl5_codes <- table(as.numeric(hh_members_ag_dry$hl5))
tab_hl5_codes

library(dplyr)
library(haven)
library(vctrs)

hh_members_ag_dry <- hh_members_ag_dry %>%
  mutate(
    age_num = vec_data(hl5),  # get underlying numeric age from haven_labelled
    age_5cat = cut(
      age_num,
      breaks = c(-Inf, 4, 14, 24, 49, Inf),
      labels = c("0-4", "5-14", "15-24", "25-49", "50+"),
      right = TRUE
    )
  )

## Check the distribution of the new 5-category age variable
table(hh_members_ag_dry$age_5cat, useNA = "ifany")

hh_members_ag_dry_sum <- read.csv("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/hh_members_ag_dry")

hh_members_ag_wet_sum <- read.csv("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/hh_members_ag_wet.csv")

# Drop sf + extra list columns, keep only age_5cat
age_df <- hh_members_ag_dry %>%
  st_drop_geometry() %>% 
  as.data.frame() %>%          # strip sf/tibble classes
  dplyr::select(age_5cat)

age_dist <- age_df %>%
  group_by(age_5cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    prop    = n / sum(n),
    percent = 100 * prop
  )

age_dist

##Wet Season
hh_members_ag_wet <- ib_all_wetdata %>%
  # keep only rows whose sn exists in hh_sum_agugu
  semi_join(households_in_hull_ag_w %>% st_drop_geometry() %>% dplyr::select(sn),
            by = "sn") %>%
  # # keep just the columns you want
  # dplyr::select(sn, hl4, hl5)
  ##Maintain convex hull geomtry
  left_join(
    households_in_hull_ag_w %>%
      dplyr::select(sn, geometry),
    by = "sn"
  ) %>%
  st_as_sf()


tab_hl4_codesw <- table(as.numeric(hh_members_ag_wet$hl4))
prop.table(tab_hl4_codesw)

tab_hl5_codesw <- table(as.numeric(hh_members_ag_wet$hl5))
tab_hl5_codesw


library(dplyr)
library(haven)
library(vctrs)

hh_members_ag_wet <- hh_members_ag_wet %>%
  mutate(
    age_num = vec_data(hl5),  # get underlying numeric age from haven_labelled
    age_5cat = cut(
      age_num,
      breaks = c(-Inf, 4, 14, 24, 49, Inf),
      labels = c("0-4", "5-14", "15-24", "25-49", "50+"),
      right = TRUE
    )
  )

## Check the distribution of the new 5-category age variable
table(hh_members_ag_wet$age_5cat, useNA = "ifany")

# Drop sf + extra list columns, keep only age_5cat
age_dfw <- hh_members_ag_wet %>%
  st_drop_geometry() %>% 
  as.data.frame() %>%          # strip sf/tibble classes
  dplyr::select(age_5cat)

age_distw <- age_dfw %>%
  group_by(age_5cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    prop    = n / sum(n),
    percent = 100 * prop
  )

age_distw

##Combine wet and dry information
hh_members_ag_all <- rbind(hh_members_ag_wet, hh_members_ag_dry)

tab_hl4_codesa <- table(as.numeric(hh_members_ag_all$hl4))
prop.table(tab_hl4_codesa)

age_dfa <- hh_members_ag_all %>%
  st_drop_geometry() %>% 
  as.data.frame() %>%          # strip sf/tibble classes
  dplyr::select(age_5cat)

age_dista <- age_dfa %>%
  group_by(age_5cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    prop    = n / sum(n),
    percent = 100 * prop
  )

age_dista


##Plot location of households within larval habitats prospected
ggplot(Ag_gripshp) +
  geom_sf(fill= "NA")+
  geom_sf(data = households_in_hull_ag, aes(color = Malaria_Positive_HH, size = 'test_positivity_rate'), alpha = 0.4)+
  scale_color_manual(values = c(Negative = "seagreen", Positive = "tomato"))+
  geom_sf(data = lav_df_hh_int_a,
          aes(shape = Anopheles_Caught, fill = Anopheles_Caught),
          size = 2, color = "black") +
  scale_shape_manual(name = "Presence of Anopheles",
                     values = c(No = 21, Yes = 24)) +  # 21 = filled circle, 24 = triangle
  scale_fill_manual(name = "Presence of Anopheles",
                    values = c(No = "yellow", Yes = "blue")) +
  # #geom_sf_text(data = Ag_gripshp, aes(label = FID), size = 3, color = "black") +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Household malaria and larval breeding site status")+
  coord_sf()




##Household summaries (17th June)
hh_members_ag_dry_sum <- read.csv("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/hh_members_ag_dry")

hh_members_ag_wet_sum <- read.csv("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/hh_members_ag_wet.csv")

hh_members_ch_dry_sum <- read.csv("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/hh_members_ch_dry.csv")

hh_members_ch_wet_sum <- read.csv("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/hh_members_ch_wet.csv") %>%
  dplyr:: select(net_ownership_and_ch_v_3, age_num, age_5cat) %>% 
  rename(hl4  = net_ownership_and_ch_v_3)

hh_members_ol_dry_sum <- read.csv("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/hh_members_ol_dry")

range(hh_members_ag_dry_sum$hl5, na.rm = TRUE)
range(hh_members_ag_wet_sum$hl5, na.rm = TRUE)
range(hh_members_ch_dry_sum$hl5, na.rm = TRUE)
range(hh_members_ch_wet_sum$hl4, na.rm = TRUE)
range(hh_members_ol_dry_sum$hl5, na.rm = TRUE)

table(hh_members_ag_dry_sum$age_5cat)
table(hh_members_ag_wet_sum$age_5cat)
table(hh_members_ch_dry_sum$age_5cat)
table(hh_members_ch_wet_sum$age_5cat)
table(hh_members_ol_dry_sum$age_5cat)


##Continuous distance-decay exposure model using density kernel
# Make sure both layers are in meters
hh_m    <- st_transform(households_in_hull_ag, 32631)
larva_m <- st_transform(lav_df_hh_int_a, 32631)

# Keep only Anopheles-positive habitats
# #larva_pos <- larva_m %>%
#   filter(Anopheles_Caught == "Yes")

# Extract coordinates
hh_coords    <- st_coordinates(hh_m)
larva_coords <- st_coordinates(larva_m)

# Household malaria status
hh_m <- hh_m %>%
  mutate(
    malaria_bin = ifelse(Malaria_Positive_HH == "Positive", 1, 0)
  )

hh_data <- hh_m %>%
  st_drop_geometry() %>%
  dplyr::select(sn, malaria_bin)   # sn = household ID

#Compute household–larval distance matrix
dist_mat <- st_distance(hh_m, larva_m)  # rows = households, cols = larval sites
dist_mat <- units::drop_units(dist_mat)  # convert to numeric meters

##Define dispersal kernel
kernel_exposure <- function(lambda, dist_mat){
  # Exponential decay kernel
  weights <- exp(-dist_mat / lambda)
  
  # Sum across all breeding sites for each household
  exposure <- rowSums(weights)
  
  return(exposure)
}


#Fit malaria model
fit_kernel_model <- function(lambda){
  
  hh_data$exposure <- kernel_exposure(lambda, dist_mat)
  
  fit <- glm(
    malaria_bin ~ exposure,
    data = hh_data,
    family = binomial()
  )
  
  data.frame(
    lambda = lambda,
    logLik = as.numeric(logLik(fit)),
    AIC = AIC(fit),
    beta = coef(fit)["exposure"],
    OR = exp(coef(fit)["exposure"])
  )
}


#Estimate flight range
lambda_grid <- seq(2, 500, by = 2)

kernel_results <- map_dfr(lambda_grid, fit_kernel_model)

# Best fitting lambda
best_lambda <- kernel_results %>%
  filter(AIC == min(AIC))


#Plot risk distance curve
k <- ggplot(kernel_results, aes(x = lambda, y = OR)) +
  geom_line(linewidth = 1.5, color = "steelblue") +
  geom_vline(xintercept = best_lambda$lambda, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_manuscript() +
  labs(
    x = "Mosquito dispersal scale (λ, meters)",
    y = "Odds ratio for malaria",
    title = "Estimated mosquito flight range from malaria risk"
  )


print(k)

ggsave(paste0(LuDir,"/", Sys.Date(), "/", 'kernel plot of bs ands hhmalpos.pdf'), k, width = 8, height = 6)


##Dry
##Continuous distance-decay exposure model using density kernel
# Make sure both layers are in meters
hh_m    <- st_transform(households_in_hull_ag_w, 32631)
larva_m <- st_transform(lav_df_hh_int_a_wet, 32631)

# Keep only Anopheles-positive habitats
# #larva_pos <- larva_m %>%
#   filter(Anopheles_Caught == "Yes")

# Extract coordinates
hh_coords    <- st_coordinates(hh_m)
larva_coords <- st_coordinates(larva_m)

# Household malaria status
hh_m <- hh_m %>%
  mutate(
    malaria_bin = ifelse(Malaria_Positive_HH == "Positive", 1, 0)
  )

hh_data <- hh_m %>%
  st_drop_geometry() %>%
  dplyr::select(sn, malaria_bin)   # sn = household ID

#Compute household–larval distance matrix
dist_mat <- st_distance(hh_m, larva_m)  # rows = households, cols = larval sites
dist_mat <- units::drop_units(dist_mat)  # convert to numeric meters

##Define dispersal kernel
kernel_exposure <- function(lambda, dist_mat){
  # Exponential decay kernel
  weights <- exp(-dist_mat / lambda)
  
  # Sum across all breeding sites for each household
  exposure <- rowSums(weights)
  
  return(exposure)
}


#Fit malaria model
fit_kernel_model <- function(lambda){
  
  hh_data$exposure <- kernel_exposure(lambda, dist_mat)
  
  fit <- glm(
    malaria_bin ~ exposure,
    data = hh_data,
    family = binomial()
  )
  
  data.frame(
    lambda = lambda,
    logLik = as.numeric(logLik(fit)),
    AIC = AIC(fit),
    beta = coef(fit)["exposure"],
    OR = exp(coef(fit)["exposure"])
  )
}


#Estimate flight range
lambda_grid <- seq(2, 500, by = 2)

kernel_results <- map_dfr(lambda_grid, fit_kernel_model)

# Best fitting lambda
best_lambda <- kernel_results %>%
  filter(AIC == min(AIC))


#Plot risk distance curve
k <- ggplot(kernel_results, aes(x = lambda, y = OR)) +
  geom_line(linewidth = 1.5, color = "steelblue") +
  geom_vline(xintercept = best_lambda$lambda, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_manuscript() +
  labs(
    x = "Mosquito dispersal scale (λ, meters)",
    y = "Odds ratio for malaria",
    title = "Estimated mosquito flight range from malaria risk"
  )


print(k)

ggsave(paste0(LuDir,"/", Sys.Date(), "/", 'kernel plot of bs ands hhmalpos.pdf'), k, width = 8, height = 6)


##Challenge (Formal) analysis
#Label household data for Challenge
household_sum_dfdry_int_c$season <- "dry"

household_sum_df_int_c$season <- "wet"

#Merge all Challenge data
hh_sum_chal <- rbind(household_sum_dfdry_int_c, household_sum_df_int_c)


##Characteristics of individuals tested in surrounding households(Challenge)
##Dry Season
hh_members_ch_dry <- ib_all_drydata %>%
  # keep only rows whose sn exists in hh_sum_agugu
  semi_join(households_in_hull_ch_d %>% st_drop_geometry() %>% dplyr::select(sn),
            by = "sn") %>%
  # keep just the columns you want
  #dplyr::select(sn, hl4, hl5)
##Maintain convex hull geometry
left_join(
  households_in_hull_ch_d %>%
    dplyr::select(sn, geometry),
  by = "sn"
) %>%
  st_as_sf()

# tab_hl4_codes <- table(as.numeric(hh_members_ag_dry$hl4))
# prop.table(tab_hl4_codes)
# 
# tab_hl5_codes <- table(as.numeric(hh_members_ag_dry$hl5))
# tab_hl5_codes
# 
# library(dplyr)
# library(haven)
# library(vctrs)

hh_members_ch_dry <- hh_members_ch_dry %>%
  mutate(
    age_num = vec_data(hl5),  # get underlying numeric age from haven_labelled
    age_5cat = cut(
      age_num,
      breaks = c(-Inf, 4, 14, 24, 49, Inf),
      labels = c("0-4", "5-14", "15-24", "25-49", "50+"),
      right = TRUE
    )
  )

# ## Check the distribution of the new 5-category age variable
# table(hh_members_ag_dry$age_5cat, useNA = "ifany")
# 
# # Drop sf + extra list columns, keep only age_5cat
# age_df <- hh_members_ag_dry %>%
#   st_drop_geometry() %>% 
#   as.data.frame() %>%          # strip sf/tibble classes
#   dplyr::select(age_5cat)
# 
# age_dist <- age_df %>%
#   group_by(age_5cat) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   mutate(
#     prop    = n / sum(n),
#     percent = 100 * prop
#   )
# 
# age_dist

##Wet Season
hh_members_ch_wet <- ib_all_wetdata %>%
  # keep only rows whose sn exists in hh_sum_agugu
  semi_join(households_in_hull_ch %>% st_drop_geometry() %>% dplyr::select(sn),
            by = "sn") %>%
  # keep just the columns you want
 # dplyr::select(sn, hl4, hl5)
##Maintain convex hull geomtry
left_join(
  households_in_hull_ch %>%
    dplyr::select(sn, geometry),
  by = "sn"
) %>%
  st_as_sf()

write_dta(hh_members_ch_wet, "hh_members_ch_wet.dta")

# tab_hl4_codesw <- table(as.numeric(hh_members_ag_wet$hl4))
# prop.table(tab_hl4_codesw)
# 
# tab_hl5_codesw <- table(as.numeric(hh_members_ag_wet$hl5))
# tab_hl5_codesw
# 
# library(dplyr)
# library(haven)
# library(vctrs)

hh_members_ch_wet <- hh_members_ch_wet %>%
  mutate(
    age_num = vec_data(hl5),  # get underlying numeric age from haven_labelled
    age_5cat = cut(
      age_num,
      breaks = c(-Inf, 4, 14, 24, 49, Inf),
      labels = c("0-4", "5-14", "15-24", "25-49", "50+"),
      right = TRUE
    )
  )

# ## Check the distribution of the new 5-category age variable
# table(hh_members_ag_wet$age_5cat, useNA = "ifany")
# 
# # Drop sf + extra list columns, keep only age_5cat
# age_dfw <- hh_members_ag_wet %>%
#   st_drop_geometry() %>% 
#   as.data.frame() %>%          # strip sf/tibble classes
#   dplyr::select(age_5cat)
# 
# age_distw <- age_dfw %>%
#   group_by(age_5cat) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   mutate(
#     prop    = n / sum(n),
#     percent = 100 * prop
#   )
# 
# age_distw

##Combine wet and dry information
hh_members_ch_all <- rbind(hh_members_ch_wet, hh_members_ch_dry)

tab_hl4_codesc <- table(as.numeric(hh_members_ch_all$hl4))
prop.table(tab_hl4_codesc)

age_dfc <- hh_members_ch_all %>%
  st_drop_geometry() %>% 
  as.data.frame() %>%          # strip sf/tibble classes
  dplyr::select(age_5cat)

age_distc <- age_dfc %>%
  group_by(age_5cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    prop    = n / sum(n),
    percent = 100 * prop
  )

age_distc

##Plot location of households
ggplot(df_ib_c) +
  geom_sf(fill= "NA")+
  geom_sf(data = hh_sum_chal, aes(color = Malaria_Positive_HH), size = 1, alpha = 0.5)+
  scale_color_manual(values = c(Negative = "seagreen", Positive = "red"))+
  scale_shape_manual(values = c(dry = 16,  wet= 17))+
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


##Refer to 001 script and bring in larval sites for Challenge
## Dataframe: lav_df_hh_int_c

##Visualize challenge breeding sites
ggplot(df_ib_c) +
  geom_sf(fill= "NA")+
  geom_sf(data = lav_df_hh_int_c, aes(color = Anopheles_Caught), size = 1, alpha = 0.5)+
  scale_color_manual(values = c(No = "lightgreen", Yes = "tomato"))+
  #scale_shape_manual(values = c(dry = 16,  wet= 17))+
  # geom_text_repel(
  #   data = df_ib,
  #   aes(label =  `WardName`, geometry = geometry),color ='black',
  #   stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  # guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Breeding site status")+
  coord_sf()

##Plot location of households incorporating larval habitats
ggplot(Ch_gripshp) +
  geom_sf(fill= "NA")+
  geom_sf(data = hh_sum_chal, aes(color = Malaria_Positive_HH, size = 'test_positivity_rate'), alpha = 0.4)+
  scale_color_manual(values = c(Negative = "seagreen", Positive = "tomato"))+
  geom_sf(data = lav_df_hh_int_c,
          aes(shape = Anopheles_Caught, fill = Anopheles_Caught),
          size = 2, color = "black") +
  scale_shape_manual(name = "Presence of Anopheles",
                     values = c(No = 21, Yes = 24)) +  # 21 = filled circle, 24 = triangle
  scale_fill_manual(name = "Presence of Anopheles",
                    values = c(No = "yellow", Yes = "blue")) +
  # #geom_sf_text(data = Ag_gripshp, aes(label = FID), size = 3, color = "black") +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Household malaria and larval breeding site status")+
  coord_sf()


##Create convex hull of around Agugu breeding sites
lav_df_hh_int_c_h   <- st_transform(lav_df_hh_int_c, 32631)
hh_sum_chal <- st_transform(hh_sum_chal, 32631)


ch_breeding_hull <- lav_df_hh_int_c_h %>%
  st_union() %>%
  st_convex_hull()

##Extract households within hull of Agugu breeding sites
households_in_hull_chl <- hh_sum_chal[st_within(hh_sum_chal,
                                                ch_breeding_hull, sparse = FALSE), ]



##Plot location of households within larval habitats prospected
ggplot(Ch_gripshp) +
  geom_sf(fill= "NA")+
  geom_sf(data = households_in_hull_chl, aes(color = Malaria_Positive_HH, size = 'test_positivity_rate'), alpha = 0.4)+
  scale_color_manual(values = c(Negative = "seagreen", Positive = "tomato"))+
  geom_sf(data = lav_df_hh_int_c,
          aes(shape = Anopheles_Caught, fill = Anopheles_Caught),
          size = 2, color = "black") +
  scale_shape_manual(name = "Presence of Anopheles",
                     values = c(No = 21, Yes = 24)) +  # 21 = filled circle, 24 = triangle
  scale_fill_manual(name = "Presence of Anopheles",
                    values = c(No = "yellow", Yes = "blue")) +
  # #geom_sf_text(data = Ag_gripshp, aes(label = FID), size = 3, color = "black") +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Household malaria and larval breeding site status")+
  coord_sf()


##Continuous distance-decay exposure model using density kernel

# Make sure both layers are in meters
hh_mc    <- st_transform(households_in_hull_chl, 32631)
larva_mc <- st_transform(lav_df_hh_int_c, 32631)

# Keep only Anopheles-positive habitats
# #larva_pos <- larva_m %>%
#   filter(Anopheles_Caught == "Yes")

# Extract coordinates
hhc_coords    <- st_coordinates(hh_mc)
larvac_coords <- st_coordinates(larva_mc)

# Household malaria status
hh_mc <- hh_mc %>%
  dplyr::mutate(
    malaria_bin = ifelse(Malaria_Positive_HH == "Positive", 1, 0)
  )

hh_datac <- hh_mc %>%
  st_drop_geometry() %>%
  dplyr::select(sn, malaria_bin)   # sn = household ID

#Compute household–larval distance matrix
dist_matc <- st_distance(hh_mc, larva_mc)  # rows = households, cols = larval sites
dist_matc <- units::drop_units(dist_matc)  # convert to numeric meters

##Define dispersal kernel
kernel_exposurec <- function(lambda, dist_matc){
  # Exponential decay kernel
  weightsc <- exp(-dist_matc / lambda)
  
  # Sum across all breeding sites for each household
  exposurec <- rowSums(weightsc)
  
  return(exposurec)
}


#Fit malaria model
fit_kernel_modelc <- function(lambda){
  
  hh_datac$exposurec <- kernel_exposurec(lambda, dist_matc)
  
  fit <- glm(
    malaria_bin ~ exposurec,
    data = hh_datac,
    family = binomial()
  )
  
  data.frame(
    lambda = lambda,
    logLik = as.numeric(logLik(fit)),
    AIC = AIC(fit),
    beta = coef(fit)["exposurec"],
    OR = exp(coef(fit)["exposurec"])
  )
}


#Estimate flight range
lambda_grid <- seq(2, 500, by = 2)

kernel_resultsc <- map_dfr(lambda_grid, fit_kernel_modelc)

# Best fitting lambda
best_lambdac <- kernel_resultsc %>%
  filter(AIC == min(AIC))


#Plot risk distanct curve
kc <- ggplot(kernel_resultsc, aes(x = lambda, y = OR)) +
  geom_line(linewidth = 1.5, color = "steelblue") +
  geom_vline(xintercept = best_lambdac$lambda, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_manuscript() +
  labs(
    x = "Mosquito dispersal scale (λ, meters)",
    y = "Odds ratio for malaria",
    title = "Estimated mosquito flight range from malaria risk"
  )

ggsave(paste0(LuDir,"/", Sys.Date(), "/", 'kernel plot of bs ands hhmalpos(chal).pdf'), k, width = 8, height = 6)


##Making plots for manuscript
##Kernel density curve
library(dplyr)
library(ggplot2)

kernel_combined <- bind_rows(
  kernel_resultsc %>% mutate(settlement = "Formal"),
  kernel_results  %>% mutate(settlement = "Slum")
)

# Peak lambdas ungrouped
peak_lambdas <- kernel_combined %>%
  group_by(settlement) %>%
  slice_max(OR, n = 1) %>%
  dplyr::select(settlement, lambda_peak = lambda, OR_peak = OR) %>%
  ungroup()

# Filter to start from peak
kernel_filtered <- kernel_combined %>%
  left_join(peak_lambdas, by = "settlement") %>%
  filter(lambda >= lambda_peak)

# Plot
kp <- ggplot(kernel_filtered, aes(x = lambda, y = OR, color = settlement, linetype = settlement)) +
  
  # Main OR lines
  geom_line(linewidth = 1.5) +
  
  # Colors for settlements
  scale_color_manual(values = c("Formal" = "#fe9c8f", "Slum" = "#f9caa7")) +
  
  # Vertical dashed lines at peak OR
  geom_vline(
    data = peak_lambdas,
    aes(xintercept = lambda_peak, color = "red"),
    linetype = "dashed",
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  
  # Label the λ values on x-axis at the peak
  geom_text(
    data = peak_lambdas,
    aes(x = lambda_peak, y = 1.0, label = lambda_peak, color = settlement),
    vjust = 1.5,
    hjust = -0.5,
    inherit.aes = FALSE
  ) +
  
  # Horizontal line for OR = 1
  geom_hline(yintercept = 1, linetype = "dashed", color = "steelblue") +
  
  # Label OR = 1 line (fix with inherit.aes = FALSE)
  geom_text(
    data = data.frame(
      x = max(kernel_filtered$lambda) + 5, 
      y = 1
    ),
    aes(x = x, y = y, label = paste0("OR = ", y)),
    color = "steelblue",
    hjust = 0,
    vjust = 1.2,
    inherit.aes = FALSE
  ) +
  
  # Theme and labels
  theme_manuscript() +
  labs(
    x = "Assumed Mosquito dispersal scale (λ, meters)",
    y = "Odds ratio for malaria",
    linetype = "Model",
    title = "Estimated mosquito flight range from malaria risk"
  )

ggsave(paste0(Lavplotsdir,"/", Sys.Date(), "/", 'kernel decay by buffer distance.pdf'), kp, width = 8, height = 6)


write.csv(kernel_filtered, file.path(Entodir, "kernel_filtered.csv"))
