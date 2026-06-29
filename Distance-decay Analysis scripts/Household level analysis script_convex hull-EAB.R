source("functions.R")

##Characteristics of individuals tested in surrounding households
##Dry Season
hh_members_ag_dry <- ib_all_drydata %>%
  # keep only rows whose sn exists in hh_sum_agugu
  semi_join(households_in_hull_ag_d %>% st_drop_geometry() %>% dplyr::select(sn),
            by = "sn") %>%
   mutate(season = "dry") %>%
  # keep just the columns you want
  dplyr::select(sn, hl4, hl5, season)


#hh_members_ag_dry <- households_in_hull_ag_d

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

write.csv(hh_members_ag_dry, file = "hh_members_ag_dry"
)


##Wet Season
hh_members_ag_wet <- ib_all_wetdata %>%
  # keep only rows whose sn exists in hh_sum_agugu
  semi_join(households_in_hull_ag_w %>% st_drop_geometry() %>% dplyr::select(sn),
            by = "sn") %>%
  mutate(season = "wet") %>%
  # keep just the columns you want
  dplyr::select(sn, hl4, hl5,season)



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

write.csv(hh_members_ag_wet, file = "hh_members_ag_wet.csv")

##Combine wet and dry information
hh_members_ag_all <- rbind(hh_members_ag_wet, hh_members_ag_dry)

write.csv(hh_members_ag_all, file = "hh_members_ag_all.csv")

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
ggplot(df_ib_a) +
  geom_sf(fill= "NA")+
  geom_sf(data = households_in_hull_ag_d, aes(color = Malaria_Positive_HH, size = 'test_positivity_rate'), alpha = 0.4)+
  geom_sf(data = households_in_hull_ag_w, aes(color = Malaria_Positive_HH, size = 'test_positivity_rate'), alpha = 0.4)+
  scale_color_manual(values = c(Negative = "seagreen", Positive = "tomato"))+
  # geom_sf(data = lav_df_hh_int_a,
  #         aes(shape = Anopheles_Caught, fill = Anopheles_Caught),
  #         size = 2, color = "black") +
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


##--------------------------------------REPITION-----------------------##
# ##Dry Season
# hh_members_ch_dry <- ib_all_drydata %>%
#   # keep only rows whose sn exists in hh_sum_agugu
#   semi_join(households_in_hull_ag_d %>% st_drop_geometry() %>% dplyr::select(sn),
#             by = "sn") %>%
#   # keep just the columns you want
#   dplyr::select(sn, hl4, hl5)
# 
# 
# tab_hl4_codes <- table(as.numeric(hh_members_ag_dry$hl4))
# prop.table(tab_hl4_codes)
# 
# tab_hl5_codes <- table(as.numeric(hh_members_ag_dry$hl5))
# tab_hl5_codes
# 
# library(dplyr)
# library(haven)
# library(vctrs)
# 
# hh_members_ag_dry <- hh_members_ag_dry %>%
#   mutate(
#     age_num = vec_data(hl5),  # get underlying numeric age from haven_labelled
#     age_5cat = cut(
#       age_num,
#       breaks = c(-Inf, 4, 14, 24, 49, Inf),
#       labels = c("0-4", "5-14", "15-24", "25-49", "50+"),
#       right = TRUE
#     )
#   )
# 
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
# 
# write.csv(hh_members_ag_dry, file = "hh_members_ag_dry"
# )
# 
# 
# 
# 
# ##Wet Season
# hh_members_ag_wet <- ib_all_wetdata %>%
#   # keep only rows whose sn exists in hh_sum_agugu
#   semi_join(households_in_hull_ag_w %>% st_drop_geometry() %>% dplyr::select(sn),
#             by = "sn") %>%
#   # keep just the columns you want
#   dplyr::select(sn, hl4, hl5)
# 
# 
# 
# tab_hl4_codesw <- table(as.numeric(hh_members_ag_wet$hl4))
# prop.table(tab_hl4_codesw)
# 
# tab_hl5_codesw <- table(as.numeric(hh_members_ag_wet$hl5))
# tab_hl5_codesw
# 
# 
# library(dplyr)
# library(haven)
# library(vctrs)
# 
# hh_members_ag_wet <- hh_members_ag_wet %>%
#   mutate(
#     age_num = vec_data(hl5),  # get underlying numeric age from haven_labelled
#     age_5cat = cut(
#       age_num,
#       breaks = c(-Inf, 4, 14, 24, 49, Inf),
#       labels = c("0-4", "5-14", "15-24", "25-49", "50+"),
#       right = TRUE
#     )
#   )
# 
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
# 
# write.csv(hh_members_ag_wet, file = "hh_members_ag_wet.csv")
# 
# ##Combine wet and dry information
# hh_members_ag_all <- rbind(hh_members_ag_wet, hh_members_ag_dry)
# 
# write.csv(hh_members_ag_all, file = "hh_members_ag_all.csv")

hh_members_ag_all <- read.csv("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/hh_members_ag_all.csv")

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








##Challenge
library(dplyr)
library(stringr)
library(sf)

###Challenge
#Wet
households_in_hull_ch <- st_read(file.path(Lavplotsdir, "households_in_hull_ch.shp"))

#Dry
household_sum_dfdry_int_c <- st_read(file.path(Lavplotsdir, "household_sum_dfdry_int_c.shp"))

# households_in_hull_ch_sf <- households_in_hull_ch %>%
#   mutate(
#     coord_text = paste(geometry, season),
#     nums = str_extract_all(coord_text, "-?//d+//.?//d*"),
#     lon = as.numeric(sapply(nums, `[`, 1)),
#     lat = as.numeric(sapply(nums, `[`, 2)),
#     season = str_extract(coord_text, "wet|dry")
#   ) %>%
#   dplyr::filter(!is.na(lon), !is.na(lat)) %>%
#   dplyr::select(-geometry, -coord_text, -nums) %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)



##Plot location of households within larval habitats prospected
ggplot(df_ib_c) +
  geom_sf(fill= "NA")+
  geom_sf(data = households_in_hull_ch_d, aes(color = Malaria_Positive_HH, size = test_positivity_rate), alpha = 0.4)+
  geom_sf(data = households_in_hull_ch, aes(color = Malaria_Positive_HH, size = test_positivity_rate), alpha = 0.4)+
  scale_color_manual(values = c(Negative = "seagreen", Positive = "tomato"))+
  # geom_sf(data = lav_df_hh_int_a,
  #         aes(shape = Anopheles_Caught, fill = Anopheles_Caught),
  #         size = 2, color = "black") +
  # scale_shape_manual(name = "Presence of Anopheles",
  #                    values = c(No = 21, Yes = 24)) +  # 21 = filled circle, 24 = triangle
  # scale_fill_manual(name = "Presence of Anopheles",
  #                   values = c(No = "yellow", Yes = "blue")) +
  # # #geom_sf_text(data = Ag_gripshp, aes(label = FID), size = 3, color = "black") +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Household malaria and larval breeding site status")+
  coord_sf()


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

tab_chl4_codesw <- table(as.numeric(hh_members_ch_wet$hl4))
prop.table(tab_chl4_codesw)

tab_chl5_codesw <- table(as.numeric(hh_members_ch_wet$hl5))
tab_chl5_codesw


library(dplyr)
library(haven)
library(vctrs)

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

## Check the distribution of the new 5-category age variable
table(hh_members_ch_wet$age_5cat, useNA = "ifany")

# Drop sf + extra list columns, keep only age_5cat
chage_dfw <- hh_members_ch_wet %>%
  st_drop_geometry() %>% 
  as.data.frame() %>%          # strip sf/tibble classes
  dplyr::select(age_5cat)

chage_distw <- chage_dfw %>%
  group_by(age_5cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    prop    = n / sum(n),
    percent = 100 * prop
  )

chage_distw

write.csv(hh_members_ch_wet, file = "hh_members_ch_wet.csv")

##Dry season

#households_in_hull_ol <- read_csv(file.path(Lavplotsdir, "households_in_hull_ol.csv"))

##Dry Season
hh_members_ch_dry <- ib_all_drydata %>%
  # keep only rows whose sn exists in hh_sum_agugu
  semi_join(households_in_hull_ch_d %>% st_drop_geometry() %>% dplyr::select(sn),
            by = "sn") %>%
  # keep just the columns you want
  dplyr::select(sn, hl4, hl5)



tab_chl4_codesd <- table(as.numeric(hh_members_ch_dry$hl4))
prop.table(tab_chl4_codesd)

tab_chl5_codesd <- table(as.numeric(hh_members_ch_dry$hl5))
tab_chl5_codesd


library(dplyr)
library(haven)
library(vctrs)

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

## Check the distribution of the new 5-category age variable
table(hh_members_ch_dry$age_5cat, useNA = "ifany")

# Drop sf + extra list columns, keep only age_5cat
chage_dfd <- hh_members_ch_dry %>%
  st_drop_geometry() %>% 
  as.data.frame() %>%          # strip sf/tibble classes
  dplyr::select(age_5cat)

chage_distd <- chage_dfd %>%
  group_by(age_5cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    prop    = n / sum(n),
    percent = 100 * prop
  )

chage_distd

write.csv(hh_members_ch_dry, file = "hh_members_ch_dry.csv")


##Olopomewa
#Dry
households_in_hull_ol <- read_csv(file.path(Lavplotsdir, "households_in_hull_ol.csv"))

##Dry Season
hh_members_ol_dry <- ib_all_drydata %>%
  # keep only rows whose sn exists in hh_sum_agugu
  semi_join(households_in_hull_ol %>% st_drop_geometry() %>% dplyr::select(sn),
            by = "sn") %>%
  # keep just the columns you want
  #dplyr::select(sn, hl4, hl5)
##Maintain convex hull geomtry
left_join(
  households_in_hull_ol %>%
    dplyr::select(sn, geometry),
  by = "sn"
) %>%
  st_as_sf()


tab_hl4_codes <- table(as.numeric(hh_members_ol_dry$hl4))
prop.table(tab_hl4_codes)

tab_hl5_codes <- table(as.numeric(hh_members_ol_dry$hl5))
tab_hl5_codes

library(dplyr)
library(haven)
library(vctrs)

hh_members_ol_dry <- hh_members_ol_dry %>%
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
table(hh_members_ol_dry$age_5cat, useNA = "ifany")

# Drop sf + extra list columns, keep only age_5cat
olage_df <- hh_members_ol_dry %>%
  st_drop_geometry() %>% 
  as.data.frame() %>%          # strip sf/tibble classes
  dplyr::select(age_5cat)

olage_dist <- olage_df %>%
  group_by(age_5cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    prop    = n / sum(n),
    percent = 100 * prop
  )

olage_dist

write.csv(hh_members_ol_dry, file = "hh_members_ol_dry"
)


##Overall plot
library(dplyr)
library(ggplot2)
households_in_hull_ag_w <- read.csv("households_in_hull_ag_w.csv")
households_in_hull_ol <- read_csv(file.path(Lavplotsdir, "households_in_hull_ol.csv"))

##Rename columns to facilitate merge
households_in_hull_ch_d <- households_in_hull_ch_d %>% 
  rename(Malaria_Positive_HH = Ml_P_HH)

households_in_hull_ch <- households_in_hull_ch %>% 
  rename(Malaria_Positive_HH = Ml_P_HH)

households_in_hull_ag_d <- households_in_hull_ag_d %>% 
  dplyr::select(-Malaria_Positive_HH) %>% 
  rename(Malaria_Positive_HH = test_positivity_rate)

households_in_hull_ag_w <- households_in_hull_ag_w %>% 
  dplyr::select(-Malaria_Positive_HH) %>% 
    rename(Malaria_Positive_HH = test_positivity_rate)

malaria_hh_plot_data <- bind_rows(
  households_in_hull_ch_d %>%
    st_drop_geometry() %>%
    dplyr::transmute(group = "CH dry", Malaria_Positive_HH),
  
  households_in_hull_ch %>%
    st_drop_geometry() %>%
    dplyr::transmute(group = "CH wet", Malaria_Positive_HH),
  
  households_in_hull_ag_d %>%
    st_drop_geometry() %>%
    dplyr::transmute(group = "AG dry", Malaria_Positive_HH),
  
  households_in_hull_ag_w %>%
    st_drop_geometry() %>%
    dplyr::transmute(group = "AG wet", Malaria_Positive_HH),
  
  households_in_hull_ol %>%
    st_drop_geometry() %>%
    dplyr::transmute(group = "Ol_dry", Malaria_Positive_HH)
) %>%
  dplyr::count(group, Malaria_Positive_HH) %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(percent = n / sum(n) * 100) %>%
  dplyr::ungroup()



ggplot(malaria_hh_plot_data,
       aes(x = group, y = percent, fill = Malaria_Positive_HH)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4
  ) +
  labs(
    x = NULL,
    y = "Households (%)",
    fill = "Household status"
  ) +
  theme_minimal()

malaria_positive_rate <- malaria_hh_plot_data %>%
  filter(Malaria_Positive_HH == "Positive")

ggplot(malaria_positive_rate,
       aes(x = group, y = percent)) +
  geom_col(fill = "#C0392B", width = 0.65) +
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),
    vjust = -0.4,
    size = 4
  ) +
  labs(
    x = NULL,
    y = "Malaria-positive households (%)"
  ) +
  ylim(0, max(malaria_positive_rate$percent) + 10) +
  theme_minimal()


hh_mal <- ggplot(malaria_hh_plot_data,
       aes(x = group, y = percent, fill = Malaria_Positive_HH)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c(Negative = "lightgreen", Positive = "tomato")) +
  geom_text(
    aes(label = paste0(round(percent, 1), "%\n(n=", n, ")")),
    position = position_stack(vjust = 0.9),
    color = "black",
    size = 3.8
  ) +
  labs(
    x = NULL,
    y = "Households (%)",
    fill = "Household status"
  ) +
  theme_manuscript()

ggsave(paste0(Lavplotsdir, "/Household malaria status.pdf"), hh_mal , width = 8, height = 6)
