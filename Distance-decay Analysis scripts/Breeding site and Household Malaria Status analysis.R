user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[//]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
LuDir <- file.path(Drive, "Documents")
LuPDir <- file.path(Drive, "Downloads")


library(readxl)
library(haven)

# ##Read in Ibadan Household data
# ib_hh_df <- read_dta(file.path(LuPDir , "IB Wet season household data_edited.dta"))
# 
# ib_mal_hh_df <- read_dta(file.path(LuPDir , "IB Wet season household malaria screening.dta"))
# 
# ##Merge Malaria Screening data
# ib_all_wetdata <- right_join(ib_mal_hh_df, ib_hh_df, by = "sn")
# 

##Read in new data
##Home
ib_wetdata_long <- read.csv("C:/Users/ebamg/Downloads/ibadan_long_wetseason_household_members_with_ind_netsupdated.csv")

##Work
ib_wetdata_long <- read.csv("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/Combined Working Data/Ibadan/Wet Season Data/Long Data/ibadan_long_wetseason_household_members_with_ind_netsupdated.csv")


##Select only households in EAs where larval prospection was conducted(Agugu & Challenge)
# ib_all_wetdata_lav <- ib_wetdata_long %>% 
#   dplyr::filter(bi5 %in% c('CHALLENGE_041/13', 'CHALLENGE_021/18', 'AGUGU_031/21')
#   )
#     
#     # 
#     # 'AGUGU_024/37', 'AGUGU_012/26', 'AGUGU_030/8','AGUGU_026/3','AGUGU_027/1',
#     #                        'CHALLENGE_021/18', 'CHALLENGE _21/18', 'CHALLENGE_18/021',
#     #                        'CHALLENGE _18/021', 'CHALLENGE _021/18', 'CHALLENGE_021/018', 
#     #                        'CHALLENGE_21/18', 'CHALLENGE_041/013', 'CHALLENGE_041/13', 
#     #                        'CHALLENGE _041/13'))
# 
# ##Check Challenge Housheolds

##Create dataset for analysis
household_sum_df <- ib_wetdata_long %>%
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


##Dry 


# ib_all_wetdata_df <- ib_all_wetdata %>%
#   group_by(sn) %>%
#   summarise(
#     longitude = first(bi7_long),
#     latitude  = first(bi7_lat),
#     ward = first(bi2),
#     n_tested = n(),  # number of people in household
#     n_positive = sum(q302 == 1, na.rm = TRUE),  # number of positive cases
#     test_positivity_rate = (n_positive / n_tested) * 100,  # positivity rate (%)
#     Malaria_Positive_HH = ifelse(n_positive > 0, "Positive", "Negative")  # household status
#   )


# ib_all_wetdata_df <- ib_all_wetdata_df  %>% 
#   filter(!is.na(latitude) & !is.na(longitude))
# 
# ib_all_wetdata_df <-    st_as_sf(ib_all_wetdata_df, coords = c("longitude", "latitude"), crs = 4326)
# 
# ####Troubleshooting
# ib_all_wetdata_dfc <- ib_all_wetdata_c %>%
#   group_by(sn) %>%
#   summarise(
#     longitude = first(bi7_long),
#     latitude  = first(bi7_lat),
#     ward = first(bi2),
#     n_tested = n(),  # number of people in household
#     n_positive = sum(q302 == 1, na.rm = TRUE),  # number of positive cases
#     test_positivity_rate = (n_positive / n_tested) * 100,  # positivity rate (%)
#     Malaria_Positive_HH = ifelse(n_positive > 0, "Positive", "Negative")  # household status
#   )


# ib_all_wetdata_lav <- ib_all_wetdata_dfc  %>% 
#   filter(!is.na(latitude) & !is.na(longitude))

household_sum_df <-    st_as_sf(household_sum_df, coords = c("longitude", "latitude"), crs = 4326)



st_crs(df_ib)
st_crs(household_sum_df)
#st_crs(ib_all_wetdata_dfc)

st_crs(df_ib) <- 4326
st_crs(household_sum_df) <- 4326
#st_crs(ib_all_wetdata_dfc) <- 4326

st_crs(df_ib) <- 4326  # if not already set, or transform to CRS of other data
household_sum_df <- st_transform(household_sum_df, st_crs(df_ib))
#ib_all_wetdata_df <- st_transform(ib_all_wetdata_df, st_crs(df_ib))
#ib_all_wetdata_dfc <- st_transform(ib_all_wetdata_dfc, st_crs(df_ib))

household_sum_df_int <- st_intersection(household_sum_df, df_ib)
#ib_all_wetdata_df_int <- st_intersection(ib_all_wetdata_df, df_ib)
#ib_all_wetdata_dfc_int <- st_intersection(ib_all_wetdata_dfc, df_ib_c)

##Extract for Agugu alone
household_sum_df_int_a <- st_intersection(household_sum_df, df_ib_a)

sum(household_sum_df_int_a_h$n_tested, na.rm = TRUE)
#Extract for Challenge 
# # Keep only grids with FID > 108
# grids_keep <- Ch_gripshp[Ch_gripshp$FID > 108, ]

household_sum_df_int_c <- st_intersection(household_sum_df, df_ib_c)

household_sum_df_int_o <- st_intersection(household_sum_df, df_ib_o)

##Plot location of households
ggplot(df_ib_a) +
  geom_sf(fill= "NA")+
  geom_sf(data = household_sum_df_int_a, aes(color = Malaria_Positive_HH), size = 1, alpha = 0.5)+
  scale_color_manual(values = c(Negative = "seagreen", Positive = "red"))+
  # scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
  # geom_text_repel(
  #   data = household_sum_df_int_c,
  #    aes(label =  `sn`, geometry = geometry),color ='black',
  #    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Household malaria status")+
  coord_sf()


ggplot(df_ib_c) +
  geom_sf(fill= "NA")+
  geom_sf(data = household_sum_df_int_c, aes(color = Malaria_Positive_HH), size = 1, alpha = 0.5)+
  scale_color_manual(values = c(Negative = "seagreen", Positive = "red"))+
  # scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
  # geom_text_repel(
  #   data = household_sum_df_int_c,
  #    aes(label =  `sn`, geometry = geometry),color ='black',
  #    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Household malaria status")+
  coord_sf()

ggplot(df_ib_o) +
  geom_sf(fill= "NA")+
  geom_sf(data = household_sum_df_int_o, aes(color = Malaria_Positive_HH), size = 1, alpha = 0.5)+
  #geom_sf(data = lav_df_hh_int_o, aes(color = Anphl_C), size = 1.5, alpha = 0.5)+
  scale_color_manual(values = c(Negative = "seagreen", Positive = "red"))+
  # scale_shape_manual(values = c(Formal = 16,  Informal= 17, Slum = 14))+
  # geom_text_repel(
  #   data = lav_df_hh_int_o,
  #    aes(label =  `sn`, geometry = geometry),color ='black',
  #    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Household malaria status")+
  coord_sf()




##Read in dry season larva dataset
lav_df_jf <- read_excel(file.path(LuDir ,"Osun-excel", "Larva prospection January and Feb updated April 2023.xlsx"))

lav_df_m <- read_excel(file.path(LuDir ,"Osun-excel", "MARCH LARVA IBADAN AND KANO.xlsx"))

lav_df_dry <- rbind(lav_df_jf, lav_df_m) %>% 
  dplyr::filter(State == "Oyo")

lav_df_dry[44, 27] <- "No"

lav_df_dry <- lav_df_dry %>% 
  mutate(Breeding_Site_Recode2 = recode(`Breeding site`,
                                        "Artificial Containers" = "Artificial",
                                        "Dug Well" = "Artificial",
                                        "Open Drain/Puddles" = "Artificial",
                                        "Open Tank" = "Artificial",
                                        "Tyre tracks" = "Artificial",
                                        "Tyres" = "Artificial",
                                        "Refuse /Sewage" = "Artificial", 
                                        "Drainage/Gutter/Ditch" = "Permanent",
                                        "Canal" = "Permanent"))


##Read in wet season larval dataset
lav_df_wet <- read_excel(file.path(LuPDir , "WET_SEASON_ENTO_COLLECTION_LARVAL_PROSPECTION_-_all_versions_-_labels_-_2024-08-12-21-21-06.xlsx"))

lav_df_wet  <- lav_df_wet  %>% 
  mutate(`Household Code/Number` = 1:272)

lav_df_wet  <- slice(lav_df_wet , -(1:2))

lav_df_wet  <- slice(lav_df_wet , -(6))

lav_df_wet  <- lav_df_wet  %>% 
  mutate(Anopheles_Caught = ifelse(`Number of Anopheles` > 0, "Yes", "No"))

##Recode Breeding site type for wetseason
lav_df_wet <- lav_df_wet %>% 
  mutate(`Breeding site` = recode(`Type of breeding site`,
                                       "Drainage" = "Drainage/Gutter/Ditch",
                                       "Gutter" = "Drainage/Gutter/Ditch",
                                       "Ditch" = "Drainage/Gutter/Ditch",
                                       "Earthen Pot" = "Artificial Containers",
                                       "Abandoned Well" = "Dug Well",
                                       "Protected Well" = "Dug Well",
                                       "Unprotected Well" = "Dug Well",
                                       "Tunnel" = "Canal",
                                       "Puddles" = "Open Drain/Puddles",
                                       "Pit" = "Open Drain/Puddles",
                                       "Plastic Bowls" = "Artificial Containers",
                                       "Tyre" = "Tyres",
                                       "Sewage" = "Refuse /Sewage"))

lav_df_wet <- lav_df_wet %>% 
  mutate(Breeding_Site_Recode2 = recode(`Breeding site`,
                                        "Artificial Containers" = "Artificial",
                                        "Dug Well" = "Artificial",
                                        "Open Drain/Puddles" = "Artificial",
                                        "Open Tank" = "Artificial",
                                        "Tyre tracks" = "Artificial",
                                        "Tyres" = "Artificial",
                                        "Refuse /Sewage" = "Artificial", 
                                        "Drainage/Gutter/Ditch" = "Permanent",
                                        "Canal" = "Permanent"))
#Extract needed variables
lav_df_dry_hh <- lav_df_dry %>% 
  dplyr::select(Locality, `Settlement Type`, Latitude, Longitude, `Breeding site`, 
                Breeding_Site_Recode2, Anopheles_Caught)

lav_df_wet_hh <- lav_df_wet %>% 
  dplyr::select(`Ward Name`, `Settlement Type`, `_Breeding site coordinates_latitude`, 
                `_Breeding site coordinates_longitude`,`Breeding site`,
                Breeding_Site_Recode2, Anopheles_Caught)

colnames(lav_df_dry_hh) [1] <- "Ward"
colnames(lav_df_dry_hh) [2] <- "Settlement"
colnames(lav_df_wet_hh) [1] <- "Ward"
colnames(lav_df_wet_hh) [2] <- "Settlement"
colnames(lav_df_wet_hh) [3] <- "Latitude"
colnames(lav_df_wet_hh) [4] <- "Longitude"


##Combine dry and wetseason
lav_df_hh <- rbind(lav_df_dry_hh, lav_df_wet_hh)

#Finalize breeding site Recode 
lav_df_hh <- lav_df_hh %>%
  mutate(Breeding_Site_Recode2 = case_when(
    Breeding_Site_Recode2 %in% c("Abandoned well", "OpenDrain/Puddle", "Tank") ~ "Artificial",
    Breeding_Site_Recode2 == "Stream" ~ "Permanent",
    TRUE ~ Breeding_Site_Recode2
  ))

##Convert to sf object for ploting
lav_df_hh <-    st_as_sf(lav_df_hh, coords = c("Longitude", "Latitude"), crs = 4326)

st_crs(df_ib) <- 4326 
lav_df_hh <- st_transform(lav_df_hh, st_crs(df_ib))

lav_df_hh <- st_intersection(lav_df_hh, df_ib)

##Extract for Agugu alone
lav_df_hh_int_a <- st_intersection(lav_df_hh, df_ib_a)

##Extract for Challenge alone
lav_df_hh_int_c <- st_intersection(lav_df_hh, df_ib_c)

## ---- Spatial outlier detection and correction -----------------------------

##Clean out outlier coordinates
lav_chall_coords <- st_coordinates(lav_df_hh_int_c)


# Input: coordinate matrix
coords <- as.data.frame(lav_chall_coords)
colnames(coords) <- c("lon", "lat")

# Step 1: Robust spatial center (median)
center <- c(
  lon = median(coords$lon),
  lat = median(coords$lat)
)

# Step 2: Euclidean distance from center (robust outlier detection)
coords$dist <- sqrt(
  (coords$lon - center["lon"])^2 +
    (coords$lat - center["lat"])^2
)

# Step 3: Identify extreme outlier(s) using 99th percentile threshold
outlier_idx <- which(coords$dist > quantile(coords$dist, 0.99))

# Step 4: Compute corrected coordinate using non-outlier points
corrected_coord <- coords[-outlier_idx, ] |>
  transform(dist = NULL) |>
  summarise(
    lon = median(lon),
    lat = median(lat)
  )

# Step 5: Replace outlier(s)
coords_corrected <- coords
coords_corrected[outlier_idx, c("lon", "lat")] <- corrected_coord

# Step 6: Remove distance column
coords_corrected$dist <- NULL

# Output corrected coordinates
coords_corrected

plot(coords$lon, coords$lat, pch = 16, col = "grey",
     xlab = "Longitude", ylab = "Latitude")
points(coords[outlier_idx, "lon"], coords[outlier_idx, "lat"],
       col = "red", pch = 16)
points(corrected_coord$lon, corrected_coord$lat,
       col = "blue", pch = 17, cex = 1.4)

legend("topright",
       legend = c("Original points", "Outlier", "Corrected point"),
       col = c("grey", "red", "blue"),
       pch = c(16, 16, 17))

## Step 7: Write corrected coordinates back to sf geometry
st_geometry(lav_df_hh_int_c) <- st_sfc(
  lapply(seq_len(nrow(coords_corrected)), function(i) {
    st_point(as.numeric(coords_corrected[i, c("lon", "lat")]))
  }),
  crs = st_crs(lav_df_hh_int_c)
)

##Updated Analysis April 27th

##Read in updated larval sites
lav_df_hh_int_a_wet <- st_read("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/Agugu_Larvalsites_wet.gpkg")
lav_df_hh_int_a_dry <- st_read("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/Agugu_Larvalsites_dry.gpkg")

lav_df_hh_int_o <-  st_read("C:/Users/ebamgboye/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/Olopomewa_Lavsites_Dry.shp")
##Some data wrangling to fit larval prospection area(using convex hull) and household survey points

#Agugu(Wet)
lav_df_hh_int_a_h   <- st_transform(lav_df_hh_int_a, 32631)
household_sum_df_int_a_h <- st_transform(household_sum_df_int_a, 32631)


a_breeding_hull <- lav_df_hh_int_a_h %>%
  st_union() %>%
  st_convex_hull()

households_in_hull_a <- household_sum_df_int_a_h[st_within(household_sum_df_int_a_h,
                                                           a_breeding_hull, sparse = FALSE), ]

sum(households_in_hull_a$n_tested, na.rm = TRUE)

#Challenge
lav_df_hh_int_c_h   <- st_transform(lav_df_hh_int_c, 32631)
household_sum_df_int_c_h <- st_transform(household_sum_df_int_c, 32631)


c_breeding_hull <- lav_df_hh_int_c_h %>%
  st_union() %>%
  st_convex_hull()

households_in_hull_c <- household_sum_df_int_c_h[st_within(household_sum_df_int_c_h,
                                                           c_breeding_hull, sparse = FALSE), ]
#Olopomewa
lav_df_hh_int_o_h   <- st_transform(lav_df_hh_int_o, 32631)
household_sum_df_int_o_h <- st_transform(household_sum_df_int_o, 32631)


o_breeding_hull <- lav_df_hh_int_o_h %>%
  st_union() %>%
  st_convex_hull()

households_in_hull_o <- household_sum_df_int_o_h[st_within(household_sum_df_int_o_h,
                                                           o_breeding_hull, sparse = FALSE), ]



##Read in gridded shape file
Ag_gripshp <- st_read ("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/new_gridded_00/Agugu/Agugu.shp")

Ch_gripshp <- st_read ("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/new_gridded_00/Challenge/Challenge.shp")

##Plot location of households incorporating larval habitats
lhc <- ggplot(df_ib_c) +
  geom_sf(fill= "NA")+
  geom_sf(data = households_in_hull_c, aes(color = Malaria_Positive_HH, size = 2), alpha = 0.4)+
  scale_color_manual(values = c(Negative = "seagreen", Positive = "red"))+
  geom_sf(data = lav_df_hh_int_c_h,
          aes(shape = Anopheles_Caught, fill = Anopheles_Caught),
          size = 3.5, color = "black") +
  scale_shape_manual(name = "Presence of Anopheles",
                     values = c(No = 21, Yes = 24)) +  # 21 = filled circle, 24 = triangle
  scale_fill_manual(name = "Presence of Anopheles",
                    values = c(No = "yellow", Yes = "blue")) +
 # geom_sf_text(data = Ag_gripshp, aes(label = FID), size = 1.5, color = "black") +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Household malaria and larval breeding site status")+
  coord_sf()

ggsave(paste0(LuDir,"/", Sys.Date(), "/", 'Household malaria and larval breeding site(chal).pdf'), lhc, width = 8, height = 6)


##Plot location of households incorporating larval habitats
lha <- ggplot(Ag_gripshp) +
  geom_sf(fill= "NA")+
  geom_sf(data = households_in_hull_a, aes(color = Malaria_Positive_HH, size = 1.5), alpha = 0.4)+
  scale_color_manual(values = c(Negative = "seagreen", Positive = "red"))+
  geom_sf(data = lav_df_hh_int_a_h,
          aes(shape = Anopheles_Caught, fill = Anopheles_Caught),
          size = 3.5, color = "black") +
  scale_shape_manual(name = "Presence of Anopheles",
                     values = c(No = 21, Yes = 24)) +  # 21 = filled circle, 24 = triangle
  scale_fill_manual(name = "Presence of Anopheles",
                    values = c(No = "yellow", Yes = "blue")) +
  # geom_sf_text(data = Ag_gripshp, aes(label = FID), size = 1.5, color = "black") +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Household malaria and larval breeding site status")+
  coord_sf()

ggsave(paste0(LuDir,"/", Sys.Date(), "/", 'Household malaria and larval breeding site(Agugu).pdf'), lha, width = 8, height = 6)


#Olopomewa
lho <- ggplot(df_ib_o) +
  geom_sf(fill= "NA")+
  geom_sf(data = household_sum_df_int_o, aes(color = Malaria_Positive_HH, size = 1.5), alpha = 0.4)+
  scale_color_manual(values = c(Negative = "seagreen", Positive = "red"))+
  geom_sf(data = lav_df_hh_int_o_h,
          aes(shape = Anphl_C, fill = Anphl_C),
          size = 3.5, color = "black") +
  scale_shape_manual(name = "Presence of Anopheles",
                     values = c(No = 21, Yes = 24)) +  # 21 = filled circle, 24 = triangle
  scale_fill_manual(name = "Presence of Anopheles",
                    values = c(No = "yellow", Yes = "blue")) +
  # geom_sf_text(data = Ag_gripshp, aes(label = FID), size = 1.5, color = "black") +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Household malaria and larval breeding site status")+
  coord_sf()

ggsave(paste0(LuDir,"/", Sys.Date(), "/", 'Household malaria and larval breeding site(Agugu).pdf'), lha, width = 8, height = 6)

#Compute distances and assess relationships
# Transform to UTM Zone 31N 
household_sum_df_int_a <- st_transform(household_sum_df_int_a, 32631)
lav_df_hh_int_a <- st_transform(lav_df_hh_int_a, 32631)

# # 1️⃣ Filter larval sites to those positive for Anopheles
# anopheles_sites <- lav_df_hh_int_a %>%
#   filter(Anopheles_Caught == "Yes")
# 
# # 2️⃣ Transform both datasets to a projected CRS in meters if needed (e.g., UTM Zone 31N for Ibadan)
# # (Only run if your CRS is still in longlat)
# household_sum_df_int_a <- st_transform(household_sum_df_int_a, 32631)
# anopheles_sites <- st_transform(anopheles_sites, 32631)
# 
# # 3️⃣ Create a2300-meter buffer around each household
# household_buffers <- st_buffer(household_sum_df_int_a, dist = 200)
# larva_buffer <- st_buffer(anopheles_sites, dist = 50)
# 
# # 4️⃣ Count number of positive larval habitats within each buffer
# intersections <- st_intersects(household_buffers, anopheles_sites)
# 
# # Add count of intersecting breeding sites to the original household data
# household_sum_df_int_a <- household_sum_df_int_a %>%
#   mutate(larvae_count_300m = lengths(intersections))
# 
# # 5️⃣ Summarize mean/median counts by malaria positivity
# summary_counts <- household_sum_df_int_a %>%
#   group_by(Malaria_Positive_HH) %>%
#   summarise(
#     mean_count = mean(larvae_count_300m),
#     median_count = median(larvae_count_300m),
#     n = n()
#   )
# 
# print(summary_counts)
# 
# # 6️⃣ Fit logistic regression: household malaria positivity ~ number of nearby larval habitats
# # (Convert outcome to binary numeric)
# household_sum_df_int_a <- household_sum_df_int_a %>%
#   mutate(Malaria_Positive_HH_bin = ifelse(Malaria_Positive_HH == "Positive", 1, 0))
# 
# buffer_model <- glm(Malaria_Positive_HH_bin ~ larvae_count_300m,
#                     family = binomial,
#                     data = household_sum_df_int_a)
# 
# summary(buffer_model)
# 
# # 7️⃣ Optional: Plot positivity rate by number of nearby breeding sites
# plot_data <- household_sum_df_int_a %>%
#   group_by(larvae_count_300m) %>%
#   summarise(
#     n_households = n(),
#     positivity_rate = mean(Malaria_Positive_HH_bin) * 100
#   )
# 
# ggplot(plot_data, aes(x = larvae_count_300m, y = positivity_rate)) +
#   geom_col(fill = "darkorange") +
#   theme_minimal() +
#   labs(x = "Number of Anopheles-positive breeding sites within 300m",
#        y = "Household Malaria Positivity (%)",
#        title = "Malaria Positivity by Proximity to Anopheles-positive Larval Habitats") +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
# 
# 
# ##Stratified by breeding site type
# 
# # 1️⃣ Filter positive larval habitats
# anopheles_sites <- lav_df_hh_int_a %>%
#   filter(Anopheles_Caught == "Yes")
# 
# # 2️⃣ Project to meters CRS if not already
# household_sum_df_int_a <- st_transform(household_sum_df_int_a, 32631)
# anopheles_sites <- st_transform(anopheles_sites, 32631)
# 
# # 3️⃣ Define buffer radius 200m here)
# buffer_radius <- 200
# household_buffers <- st_buffer(household_sum_df_int_a, dist = buffer_radius)
# 
# # 4️⃣ Get unique site types present
# site_types <- unique(anopheles_sites$Breeding.site)
# 
# # 5️⃣ Initialize a data frame to hold counts per site type
# counts_df <- household_sum_df_int_a %>%
#   st_set_geometry(NULL) %>%
#   dplyr::select(sn)  # or your household ID column
# 
# # 6️⃣ For each site type, count number of positive sites within buffer
# for(site in site_types) {
#   # Filter larval sites by site type
#   sites_sub <- anopheles_sites %>%
#     filter(Breeding.site == site)
#   
#   # Find intersections with household buffers
#   intersections <- st_intersects(household_buffers, sites_sub)
#   
#   # Count sites within buffer
#   counts <- lengths(intersections)
#   
#   # Add counts to dataframe, naming column by site type (make names R-friendly)
#   col_name <- paste0("count_", gsub("[^[:alnum:]_]", "", tolower(site)))
#   counts_df[[col_name]] <- counts
# }
# 
# # 7️⃣ Join counts back to households data
# household_sum_df_int_a <- household_sum_df_int_a %>%
#   left_join(counts_df, by = "sn")
# 
# # 8️⃣ Create binary outcome for modeling
# household_sum_df_int_a <- household_sum_df_int_a %>%
#   mutate(Malaria_Positive_HH_bin = ifelse(Malaria_Positive_HH == "Positive", 1, 0))
# 
# # 9️⃣ Fit logistic regression including counts for each breeding site type
# # Example: all site types together
# formula_str <- paste("Malaria_Positive_HH_bin ~", paste(names(counts_df)[-1], collapse = " + "))
# model_stratified <- glm(as.formula(formula_str), data = household_sum_df_int_a, family = binomial)
# 
# summary(model_stratified)




##Larval habitat as buffer center
# 1️⃣ Filter malaria-positive households
mal_pos_hh <- household_sum_df_int_a %>%
  filter(Malaria_Positive_HH == "Positive")

# 2️⃣ Filter Anopheles-positive breeding sites
anopheles_sites <- lav_df_hh_int_a %>%
  filter(Anopheles_Caught == "Yes")

# 3️⃣ Define buffer distances (in meters)
buffer_distances <- c(50, 150, 200, 250)

# 4️⃣ Loop through buffer distances, compute intersects + classify high-risk sites
for (dist in buffer_distances) {
  
  # Create buffer around each Anopheles-positive larval site
  buffer <- st_buffer(anopheles_sites, dist = dist)
  
  # Count number of positive households within each buffer
  intersects <- st_intersects(buffer, mal_pos_hh)
  
  # Add intersect counts and high-risk status to anopheles_sites dataframe
  anopheles_sites[[paste0("pos_hh_count_", dist, "m")]] <- lengths(intersects)
  anopheles_sites[[paste0("high_risk_site_", dist, "m")]] <- ifelse(lengths(intersects) >= 1, 1, 0)
  
  # Print counts for quick inspection
  print(paste0("At ", dist, "m buffer:"))
  print(table(anopheles_sites[[paste0("high_risk_site_", dist, "m")]]))
}

# 5️⃣ Summarize frequency by site type for each buffer distance
for (dist in buffer_distances) {
  cat("/nSummary by breeding site type at ", dist, "m buffer:/n")
  
  summary_table <- anopheles_sites %>%
    group_by(Breeding_Site_Recode2) %>%
    summarise(
      total_sites = n(),
      n_high_risk = sum(get(paste0("high_risk_site_", dist, "m"))),
      pct_high_risk = (n_high_risk / total_sites) * 100
    )
  
  print(summary_table)
}

# 6️⃣ Optional: Plot distribution of number of positive households around breeding sites (for500m as example)
ggplot(anopheles_sites, aes(x = pos_hh_count_50m)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(
    x = "Number of malaria-positive households within 300m",
    y = "Number of Anopheles-positive breeding sites",
    title = "Distribution of Positive Households near Larval Habitats (300m Buffer)"
  )

library(dplyr)
library(broom)

buffer_distances <- c(50, 150, 200, 250)

breeding_sites <- unique(anopheles_sites$Breeding_Site_Recode2)

for (dist in buffer_distances) {
  cat("/n=== Buffer:", dist, "m ===/n")
  outcome_var <- paste0("high_risk_site_", dist, "m")
  
  if (!outcome_var %in% names(anopheles_sites)) {
    cat("Variable", outcome_var, "not found. Skipping./n")
    next
  }
  
  for (site in breeding_sites) {
    cat("/nBreeding site:", site, "/n")
    
    df_sub <- anopheles_sites %>%
      filter(Breeding.site == site)
    
    # Check if there's variation in outcome; if all 0 or all 1, glm won't run properly
    if(length(unique(df_sub[[outcome_var]])) < 2) {
      cat("Not enough variation in outcome for this group. Skipping./n")
      next
    }
    
    # Fit intercept-only logistic regression (estimate prevalence of high risk in that site type)
    model <- glm(as.formula(paste(outcome_var, "~ 1")), data = df_sub, family = binomial)
    
    # Extract coefficients and transform intercept to probability (expit)
    tidy_res <- tidy(model)
    intercept_logit <- tidy_res$estimate[1]
    intercept_se <- tidy_res$std.error[1]
    
    # Calculate probability and 95% CI on response scale
    prob <- plogis(intercept_logit)
    lower_ci <- plogis(intercept_logit - 1.96 * intercept_se)
    upper_ci <- plogis(intercept_logit + 1.96 * intercept_se)
    
    cat(sprintf("Estimated probability of high risk: %.3f (95%% CI: %.3f - %.3f)/n", prob, lower_ci, upper_ci))
  }
}


##Chi square function
buffer_distances <- c(50, 100, 150, 200)
for (dist in buffer_distances) {
  cat("/nChi-square test for buffer distance:", dist, "m/n")
  buffer_col <- paste0("high_risk_site_", dist, "m")
  if (!buffer_col %in% names(anopheles_sites)) {
    cat("Column", buffer_col, "not found./n")
    next
  }
  cont_table <- table(anopheles_sites[[buffer_col]], anopheles_sites$Breeding_Site_Recode2)
  print(cont_table)
  # Check if any expected counts <5
  if(any(chisq.test(cont_table)$expected < 5)) {
    cat("Expected counts < 5, using Fisher's exact test/n")
    print(fisher.test(cont_table))
  } else {
    print(chisq.test(cont_table))
  }
}



###Another attempt based on data sparsity
# -------------------------------
# 1. Compute nearest distance to positive breeding site
# -------------------------------
# Compute distance matrix (households × anopheles_sites)
dist_matrix <- st_distance(household_sum_df_int_a, anopheles_sites)

# Get the nearest distance for each household
household_sum_df_int_a <- household_sum_df_int_a %>%
  mutate(dist_to_pos_site = apply(dist_matrix, 1, min))


# -------------------------------
# 2. Create proximity bands
# -------------------------------
household_sum_df_int_a <- household_sum_df_int_a %>%
  mutate(proximity_band = case_when(
    dist_to_pos_site < 25 ~ "<25m",
    dist_to_pos_site >= 25 & dist_to_pos_site <= 50 ~ "25-50",
    dist_to_pos_site >= 50 & dist_to_pos_site <= 75 ~ "50-75",
    TRUE ~ ">75m"
  ))


##Combine datasets for ease of analysis
st_crs(household_sum_df_int_a)
st_crs(anopheles_sites)

anopheles_sites <- st_transform(anopheles_sites, st_crs(household_sum_df_int_a))

hh_wlav_data_ag <- st_join(household_sum_df_int_a, anopheles_sites, join = st_nearest_feature)


# -------------------------------
# 3. Summarise malaria prevalence by proximity band
# -------------------------------
prevalence_summary <- hh_wlav_data_ag %>%
  group_by(proximity_band, Breeding.site) %>%
  summarise(
    n_households = n(),
    malaria_positive = sum(Malaria_Positive_HH == "Positive"),
    prevalence_percent = malaria_positive / n_households * 100
  )


##Recode breeding site into two categories
prevalence_summary <- prevalence_summary %>% 
  mutate(Breedingsite_grp = recode(Breeding.site,
                                     "Artificial Containers" = "Artificial",
                                     "Dug Well" = "Artificial",
                                     "Open Drain/Puddles" = "Artificial",
                                     "Open Tank" = "Artificial",
                                     "Tyre tracks" = "Artificial",
                                     "Tyres" = "Artificial",
                                     "Refuse /Sewage" = "Artificial", 
                                     "Drainage/Gutter/Ditch" = "Permanent",
                                     "Canal" = "Permanent"))
# View summary
print(prevalence_summary)

# -------------------------------
# 4. Plot malaria prevalence by proximity band
# -------------------------------
ggplot(prevalence_summary, aes(x = Breeding.site, y = prevalence_percent)) +
  facet_wrap(~proximity_band)+
  geom_col(fill = "firebrick") +
  ylab("Malaria prevalence (%)") +
  xlab("Distance to nearest Anopheles-positive breeding site")+
  theme_manuscript()

##Filter out those above 75m
household_sum_df_int_a_lg <- household_sum_df_int_a %>%
  dplyr::filter(proximity_band %in% c('<25m', '25-50m', '50-75'))

# -------------------------------
# 5. Logistic regression: Malaria risk by proximity band (if sufficient counts)
# -------------------------------
# Convert Malaria_Positive_HH to factor with "Negative" as reference
household_sum_df_int_a$Malaria_Positive_HH <- factor(household_sum_df_int_a$Malaria_Positive_HH, levels = c("Negative", "Positive"))

# Fit logistic regression model (add other covariates if available)
model <- glm(Malaria_Positive_HH ~ proximity_band, 
             family = binomial, data = household_sum_df_int_a_lg)

# View model results
summary(model)

# Fit logistic regression with continuous distance
model_continuous <- glm(Malaria_Positive_HH ~ dist_to_pos_site, 
                        family = binomial, 
                        data = household_sum_df_int_a)

# Check results
summary(model_continuous)



#View contingency table of malaria positivity by proximity band
malaria_table <- table(household_sum_df_int_a$Malaria_Positive_HH, 
                       household_sum_df_int_a$proximity_band)

# Print contingency table
print(malaria_table)

# Run Chi-square test
chi_test <- chisq.test(malaria_table)

# Print Chi-square test results
print(chi_test)

# Check expected cell counts to validate Chi-square assumptions
print(chi_test$expected)

# If any expected count is <5, run Fisher's Exact Test instead
if(any(chi_test$expected < 5)) {
  fisher_test <- fisher.test(malaria_table)
  print(fisher_test)
}


##More analysis
library(sf)
library(dplyr)

# Identify index of nearest positive breeding site for each household
nearest_site_index <- st_nearest_feature(household_sum_df_int_a, anopheles_sites)

# Retrieve the corresponding breeding site recode type
household_sum_df_int_a <- household_sum_df_int_a %>%
  mutate(Breeding_Site_Recode = anopheles_sites$Breeding.site[nearest_site_index])

# household_sum_df_int_a <- household_sum_df_int_a %>%
#   left_join(malaria_df, by = "Household_ID")


# List unique breeding site types
unique(household_sum_df_int_a$Breeding_Site_Recode)

# Loop through each breeding site type and run chi-square
site_types <- unique(household_sum_df_int_a$Breeding_Site_Recode)

for (site in site_types) {
  
  cat("/n==========/n")
  cat("Breeding Site Type:", site, "/n")
  cat("==========/n")
  
  # Filter data for the site type
  df_site <- household_sum_df_int_a %>%
    filter(Breeding_Site_Recode == site,
           proximity_band %in% c('<50m', '100'))
  
  # Create contingency table
  malaria_table <- table(df_site$Malaria_Positive_HH, df_site$proximity_band)
  print(malaria_table)
  
  # Check if enough observations to run chi-square
  if(all(dim(malaria_table) == c(2,2))) {
    
    chi_test <- chisq.test(malaria_table)
    print(chi_test)
    
    print("Expected cell counts:")
    print(chi_test$expected)
    
    # If any expected count is <5, run Fisher's test
    if(any(chi_test$expected < 5)) {
      fisher_test <- fisher.test(malaria_table)
      print("Fisher's Exact Test result:")
      print(fisher_test)
    }
    
  } else {
    print("Not enough data in one or more categories to run chi-square.")
  }
}

anopheles_sites

st_write(anopheles_sites, "anopheles_sites.shp")

test <- st_read("anopheles_sites.shp")
print(test)

st_read("C:/Users/ebamgboye/Downloads/nigeria.geojsonl/nigeria.geojsonl")
