#loadpath
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
shapefileDir <- "C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan"
Entodir <- "C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento"
Lavplotsdir <- "C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/projects/Manuscripts/ongoing/Larviciding Manuscript/New Manuscript Sections"

##load packages and themes
source("functions.R")

##load extra packages
library(readxl)
library(corrplot)
library(ggspatial)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggspatial)
library(sf)
library(scales)
library(broom)
library(tibble)
library(rnaturalearth)
library(rnaturalearthdata)


#Read in shapefile
## Read ibadan ward shape files
df_ib <- st_read(file.path(shapefileDir, "kano_ibadan_shape_files", "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>%
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName))

#Plot location of wards visited
pd <- ggplot(df_ib) +
  geom_sf(aes(fill = WardName), color = "black") +
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  scale_fill_manual(values = c(
    "Agugu" = "plum",  # Replace with actual ward names and desired colors
    "Challenge" = "coral",
    "Olopomewa" = "lightgreen"
  ), na.value = "white")+
  map_theme()+ 
  labs(title= "Wards in Ibadan visited for entomology study ")+
  coord_sf()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/", 'ibadan ento study wards.pdf'), pd, width = 8, height = 6)

##Split Ibadan shape file into working wards
df_ib_c <- df_ib %>%
  dplyr::filter(WardName == 'Challenge')

df_ib_a <- df_ib %>%
  dplyr::filter(WardName == 'Agugu')

df_ib_o <- df_ib %>%
  dplyr::filter(WardName == 'Olopomewa')


##Read in dry season larva dataset
lav_df_jf <- read_excel(file.path(Entodir ,"Osun-excel", "Larva prospection January and Feb updated April 2023.xlsx"))

lav_df_m <- read_excel(file.path(Entodir ,"Osun-excel", "MARCH LARVA IBADAN AND KANO.xlsx"))

##Merge January February with March
lav_df_dry <- rbind(lav_df_jf, lav_df_m) %>% 
  dplyr::filter(State == "Oyo")

#Some data cleaning
lav_df_dry[44, 27] <- "No"


##Read in wet season larval dataset
lav_df_wet <- read_excel(file.path(Entodir, "Wet Season Data_Ibadan", "WET_SEASON_ENTO_COLLECTION_LARVAL_PROSPECTION_-_all_versions_-_labels_-_2024-08-12-21-21-06.xlsx"))

#Some data cleaning
lav_df_wet  <- lav_df_wet  %>% 
  mutate(`Household Code/Number` = 1:272)

lav_df_wet  <- slice(lav_df_wet , -(1:2))

lav_df_wet  <- slice(lav_df_wet , -(6))

lav_df_wet  <- lav_df_wet  %>% 
  mutate(Anopheles_Caught = ifelse(`Number of Anopheles` > 0, "Yes", "No"))



##Breeding site manipulations
##Dry Season
##Recode and clean breeding site names 
lav_df_dry <- lav_df_dry %>% 
  mutate(Breeding_Site_Recode = recode(`Breeding site`,
                                       "Abandoned well" = "Dug Well",
                                       "OpenDrain/Puddle" = "Open Drain/Puddles",
                                       "Tank" = "Open Tank",
                                       "Stream" = "Canal"))


##Recode breeding sites to two groups(Permanent/Artificial)
lav_df_dry <- lav_df_dry %>% 
  mutate(Breeding_Site_Recode2 = recode(Breeding_Site_Recode,
                                        "Artificial Containers" = "Artificial",
                                        "Dug Well" = "Artificial",
                                        "Open Drain/Puddles" = "Artificial",
                                        "Open Tank" = "Artificial",
                                        "Tyre tracks" = "Artificial",
                                        "Tyres" = "Artificial",
                                        "Refuse /Sewage" = "Artificial", 
                                        "Drainage/Gutter/Ditch" = "Permanent",
                                        "Canal" = "Permanent"))



##Wet Season
##Fill missing entry
if (is.na(lav_df_wet[35, 8]) || lav_df_wet[35, 8] == "") {
  lav_df_wet[35, 8] <- "Puddles"
}

if (is.na(lav_df_wet[157, 8]) || lav_df_wet[157, 8] == "") {
  lav_df_wet[157, 8] <- "Gutter"
}

if (is.na(lav_df_wet[184, 6]) || lav_df_wet[184, 6] == "") {
  lav_df_wet[184, 6] <- "Slum"
}

lav_df_wet[163, 6] <- "Slum"

##Recode breeding sites to macth dry season
lav_df_wet <- lav_df_wet %>% 
  mutate(Breeding_Site_Recode = recode(`Type of breeding site`,
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

##Recode breeding sites to two groups(Permanent/Artificial)
lav_df_wet <- lav_df_wet %>% 
  mutate(Breeding_Site_Recode2 = recode(Breeding_Site_Recode,
                                        "Artificial Containers" = "Artificial",
                                        "Dug Well" = "Artificial",
                                        "Open Drain/Puddles" = "Artificial",
                                        "Open Tank" = "Artificial",
                                        "Tyre tracks" = "Artificial",
                                        "Tyres" = "Artificial",
                                        "Refuse /Sewage" = "Artificial", 
                                        "Drainage/Gutter/Ditch" = "Permanent",
                                        "Canal" = "Permanent"))
##Label datasets
lav_df_dry$season <- "Dry"

lav_df_wet$season <- "Wet"

#Write to folder
#Dry Season 
 write.csv(lav_df_dry, file.path(Entodir, "lav_dataset_dry.csv"))
#wet Season
 write.csv(lav_df_wet, file.path(Entodir, "lav_dataset_wet.csv"))

 
 
##EXtract variables for breeding site analysis
lav_dfd <- lav_df_dry %>% 
  dplyr::select(`Settlement Type`, Anopheles_Caught,
                season, Breeding_Site_Recode, Breeding_Site_Recode2)

lav_dfw <- lav_df_wet %>% 
  dplyr::select(`Settlement Type`, Anopheles_Caught,
                season, Breeding_Site_Recode, Breeding_Site_Recode2)

lav_overall <- rbind(lav_dfd, lav_dfw)

write.csv(lav_overall, file.path(Entodir, "lav_breedingsite_dataset.csv"))


##Larval Density Analysis
##Dry Season
lav_ib_dry <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo")

subset_lav <- lav_ib_dry[lav_ib_dry$`Anopheles` > 0, ]

lav_den_sum <- subset_lav %>% 
  mutate(Larva_Density = `Anopheles`/`No of dips`)

# Recode Site Codes for better understanding
lav_den_sum <- lav_den_sum %>% 
  mutate(`Site Code` = case_when(
    `Site Code` ==  "1" ~ "1",
    `Site Code` == "6" ~ "6",
    `Site Code` == "IB/AG/14" ~ "14",
    `Site Code` == "IB/OL/10" ~ "10",
    `Site Code` == "IB/OL/20" ~ "20"
  ))

lav_den_sum$season <- "Dry"

##Estimating total anopheles
lav_den_sum %>%
  summarise(Total_Anopheles = sum(Anopheles, na.rm = TRUE))

##Wet Season
subset_lav_wet <- lav_df_wet[lav_df_wet$`Number of Anopheles` > 0, ]

lav_den_sum_wet <- subset_lav_wet %>% 
  mutate(Larva_Density = `Number of Anopheles`/`Number of Dips`)

lav_den_sum_wet$season <- "Wet"

##Compute Av. Larval density
#Dry
lav_den_sum_dry <- lav_den_sum %>% 
  group_by(`Settlement Type`, `Breeding_Site_Recode`) %>%  # Group by breeding site type
  summarize(
    AvgLD = mean(`Larva_Density`, na.rm = TRUE)  # Average number of Anopheles caught per site
  )
lav_den_sum_dry$season <- "Dry"

#Wet
lav_den_sum_wett <- lav_den_sum_wet %>% 
  group_by(`Settlement Type`, `Breeding_Site_Recode`) %>%  # Group by breeding site type
  summarize(
    AvgLD = mean(`Larva_Density`, na.rm = TRUE)  # Average number of Anopheles caught per site
  )

lav_den_sum_wett$season <- "Wet"

##Combine Larval Density Data
lav_den_sum_all <- rbind(lav_den_sum_dry, lav_den_sum_wett)

write.csv(lav_den_sum_all, file.path(Entodir, "lav_density_dataset.csv"))

##---End Data Preparation------------------------------------------------------##



##Manuscript figures
##Figure 1: Larval habitat locations

##Figure 2: Pictures of some of the prospected habitats

#Figure 3 (Prospected breeding sites)

##Read in breeding site data set
lav_overall <- read.csv(file.path(Entodir, "lav_breedingsite_dataset.csv"))

##Figure 3A: Types of breeding sites prospected
# Summarize counts
donut_data <- lav_overall %>%
  count(Breeding_Site_Recode2) %>%
  mutate(
    prop = n / sum(n),
    label = paste0(Breeding_Site_Recode2, "\n", round(prop*100, 2), "%")
  )

# Custom colors 
custom_cols <- c(
  "Permanent" = "#ff7f00",
  "Artificial" = "#6a3d9a"
)

# Make donut plot
Fig3a <- ggplot(donut_data, aes(x = 2, y = prop, fill = Breeding_Site_Recode2)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y") +
  
  # Create the actual hole
  annotate("rect", xmin = 0, xmax = 1.3, ymin = 0, ymax = 1,
           fill = "white", color = NA) +
  
  scale_fill_manual(values = custom_cols) +
  
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), size = 4) +
  
  xlim(0.5, 2.5) +
  labs(
    title = "Distribution of Breeding Site Types",
    fill = "Breeding Site Type"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), "/", 'Distribution of Breeding Site Types.pdf'), Fig3a, width = 8, height = 9)

#Figure 3B: Percentage of breeding site type by season
# Summarize counts by Season, and Breeding site type
lav_overall_sum <- lav_overall %>% 
  group_by(Breeding_Site_Recode2, Season) %>%
  summarise(Count = n(), .groups = "drop") %>%
  # calculate total per Season for percentage
  group_by(Breeding_Site_Recode2) %>%
  mutate(Total = sum(Count),
         Percent = Count / Total * 100) %>%
  ungroup()

# Facet plot with stacked bars
Fig3b <- ggplot(lav_overall_sum , 
              aes(x = Breeding_Site_Recode2, y = Percent, fill = Season)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("Dry" = "bisque1", "Wet" = "lightblue")) +
  labs(
    title = "Proportion of Breeding Sites by Season and Type",
    x = "Breeding Site Type",
    y = "Proportion (%)",
    fill = "Season"
  ) +
  theme_manuscript()

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), "/", 'Proportion of Breeding Sites by Season and Type.pdf'), Fig3b, width = 8, height = 9)


#Figure 3C: Percentage of breeding site type by settlement type and seaason

# Summarise counts per Settlement, Season, and Breeding Site Type
lav_plot_data <- lav_overall %>%
  group_by(Settlement, Breeding_Site_Recode2, Season) %>%
  summarise(Count = n(), .groups = "drop")

##Compute proportion per category
lav_plot_data_prop <- lav_plot_data %>%
  # group by Settlement and Breeding Site Type
  group_by(Settlement, Season) %>%
  # calculate total sites of this type per Settlement
  mutate(TotalType = sum(Count)) %>%
  # proportion of each season within that type
  mutate(Proportion = (Count / TotalType) * 100) %>%
  ungroup()

# Facet plot with stacked bars
Fig3c <- ggplot(lav_plot_data_prop , 
              aes(x = Settlement, y = Proportion, fill = Breeding_Site_Recode2)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Proportion, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("Artificial" = "bisque1", "Permanent" = "#b2df8a")) +
  labs(
    title = "Proportion of Breeding Sites by Season and Type",
    x = "Breeding Site Type",
    y = "Proportion (%)",
    fill = "Season"
  ) +
  facet_wrap(~ Season) +
  theme_manuscript()

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), "/", 'Proportion of Breeding Sites by Season, Sett and Type.pdf'), Fig3c, width = 8, height = 9)


##Breeding site physico-chemical characteristics
##Update analysis for physico chemical characteristics of all breeding sites on 1st Dec. 

#Read in data set
#Dry Season 
lav_df_dry <- read.csv(file.path(Entodir, "lav_dataset_dry.csv"))

#wet Season
lav_df_wet <- read.csv(file.path(Entodir, "lav_dataset_wet.csv"))

##Dry Season data wrangling
lav_physico_drydf <- lav_df_dry %>% 
  dplyr::select(`Settlement Type`, `Origin of water`, `Water nature`, `Water Characteristics`, Temp, 
                pH, `Sunlight exposure`, Vegetation, Anopheles_Caught, Breeding_Site_Recode, 
                Breeding_Site_Recode2, season)

#Wet Season
lav_physico_wetdf <- lav_df_wet %>% 
  dplyr::select(
    `Settlement Type`,
    `Origin of Water`,
    `Water Nature`,
    `Water Characteristics`,
    `Temperature(Celcius)`,
    pH,
    `IIs the breeding site exposed to sunlight?`,
    `Presence of Vegetation`,
    Anopheles_Caught,
    Breeding_Site_Recode,
    Breeding_Site_Recode2,
    season
  ) %>%
  # Rename columns to match lav_physico_drydf exactly
  dplyr::rename(
    `Origin of water` = `Origin of Water`,
    `Water nature` = `Water Nature`,
    Temp = `Temperature(Celcius)`,
    `Sunlight exposure` = `IIs the breeding site exposed to sunlight?`,
    Vegetation = `Presence of Vegetation`
  )


lav_physicodf_dry_wet <- dplyr::bind_rows(lav_physico_drydf, lav_physico_wetdf)

##Clean up Extra 3 rows to remove data issues
lav_physicodf_dry_wet <- lav_physicodf_dry_wet %>%
  filter(
    !(
      `Settlement Type` == "Slum" &
        `Origin of water` == "drinage" &
        origin_clean == "Other/Unknown"
    )
  )


##Figure 3D: Percentage of breeding site by water nature
##Recode and clean variable names
#Water Origin
lav_physicodf_dry_wet <- lav_physicodf_dry_wet %>%
  mutate(
    origin_clean = case_when(
      # Rain categories
      str_detect(`Origin of water`, regex("^rain", ignore_case = TRUE)) ~ "Rain",
      
      # River
      str_detect(`Origin of water`, regex("river", ignore_case = TRUE)) ~ "River/Stream",
      
      # Drainage / gutter / ditch
      str_detect(`Origin of water`, regex("drain|ditch|gutter", ignore_case = TRUE)) ~ "Drainage/Gutter",
      
      # Waste water / sewage
      str_detect(`Origin of water`, regex("waste|sewage", ignore_case = TRUE)) ~ "Waste water/Sewage",
      
      # Domestic / Household
      str_detect(`Origin of water`, regex("domestic|household", ignore_case = TRUE)) ~ "Household/Domestic",
      
      # Man-made containers
      str_detect(`Origin of water`, regex("man made|manmade", ignore_case = TRUE)) ~ "Man-made",
      
      # Borehole
      str_detect(`Origin of water`, regex("borehole", ignore_case = TRUE)) ~ "Borehole",
      
      # Well
      str_detect(`Origin of water`, regex("well", ignore_case = TRUE)) ~ "Well",
      
      # Default
      TRUE ~ "Other/Unknown"
    )
  )


lav_physicodf_dry_wet <- lav_physicodf_dry_wet %>%
  mutate(waterorigin4 = case_when(
    origin_clean == "Rain" ~ "Rainwater",
    
    origin_clean %in% c("Borehole", "Well") ~ "Groundwater",
    
    origin_clean == "River/Stream" ~ "Surface water",
    
    origin_clean %in% c("Drainage/Gutter",
                        "Household/Domestic",
                        "Man-made",
                        "Waste water/Sewage",
                        "Other/Unknown") ~ "Anthropogenic/Domestic",
    
    TRUE ~ "Other"
  ))


# Calculate counts and percentages within each Settlement Type(by season)
lav_physicodf_plot <- lav_physicodf_dry_wet %>%
  group_by(`Settlement Type`, season, waterorigin4) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(`Settlement Type`, season) %>%
  mutate(
    percent = count / sum(count) * 100,
    waterorigin4_ordered = fct_reorder(waterorigin4, desc(count))
  ) %>%
  ungroup()


##Ensure order is preserved
lav_physicodf_plot$waterorigin4_ordered <- factor(
  lav_physicodf_plot$waterorigin4_ordered,
  levels = c(
    "Rainwater",
    "Groundwater",
    "Anthropogenic/Domestic",
    "Surface water"
  )
)

# Make Plot
Fig3d <- ggplot(lav_physicodf_plot, aes(x = `Settlement Type`, y = percent, fill = waterorigin4_ordered)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            position = position_stack(vjust = 0.5),
            size = 3.5) +
  facet_grid(~season) +
  scale_y_continuous(labels = percent_format(scale = 1)) +  
  scale_fill_brewer(palette = "Set3") +
  labs(
    x = "Settlement Type",
    y = "Percentage of Water Sources",
    fill = "Water Origin",
    title = "Distribution of Water Origins by Settlement Type"
  ) +
  theme_manuscript()

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), "/", 'Water Origin of breeding sites.pdf'), Fig3d, width = 8, height = 6)


#Figure 3E: Percentage of breeding sites by water nature
##Water Nature
table(lav_physicodf_dry_wet$waternature)

lav_physicodf_dry_wet <- lav_physicodf_dry_wet %>%
  mutate(waternature = case_when(
    `Water nature` == "Clear" ~ "Clean",
    
    `Water nature` %in% c("Clean", "clean") ~ "Clear",
    
    `Water nature` %in% c("polluted",
                          "Polluted") ~ "Polluted"
  ))

# Calculate counts and percentages within each Settlement Type
lav_physicodf_plot2 <- lav_physicodf_dry_wet %>%
  group_by(`Settlement Type`, season, waternature) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(`Settlement Type`, season) %>%
  mutate(
    percent = count / sum(count) * 100,
    waternature_ordered = fct_reorder(waternature, desc(count))
  ) %>%
  ungroup()

#Make Plot by Season and Settlement Type
Fig3e <- ggplot(
  lav_physicodf_plot2,
  aes(x = `Settlement Type`, y = percent, fill = waternature_ordered)
) +
  geom_col(width = 0.7, color = "black") +
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3.5,
    color = "black"
  ) +
  facet_wrap(~season) +
  scale_fill_manual(
    values = c("Clear" = "#a6cee3", "Polluted" = "#d2b48c")
  ) +
  labs(
    title = "Nature of Water by Settlement Type and Season",
    x = "Season",
    y = "Count",
    fill = "Water Nature"
  ) +
  theme_manuscript() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold")
  )


ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), "/", 'Nature of Water by Settlement Type and Season.pdf'), Fig3e, width = 8, height = 6)




##Sunlight
table(lav_physicodf_dry_wet$sunlight)

lav_physicodf_dry_wet <- lav_physicodf_dry_wet %>%
  mutate(sunlight = case_when(
    `Sunlight exposure` %in% c("Yes", 
                               "sunlit",
                               "Sunlit",
                               "sunnlit") ~ "Yes",
    
    `Sunlight exposure` %in% c("No",
                               "Shaded") ~ "No"
  ))


#Figure 3F: Percentage of breeding site type by vegetation
##Vegetation
table(lav_physicodf_dry_wet$Vegetation)

lav_physicodf_dry_wet <- lav_physicodf_dry_wet %>%
  mutate(Vegetation = case_when(
    `Vegetation` %in% c("Yes", 
                        "yes",
                        "submerge",
                        "Submerge") ~ "Yes",
    
    `Vegetation` %in% c("No",
                        "no") ~ "No"
  ))


# Calculate counts and percentages within each Settlement Type
lav_physicodf_plot3 <- lav_physicodf_dry_wet %>%
  group_by(season, `Settlement Type`, Vegetation) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(season, `Settlement Type`) %>%
  mutate(
    percent = count / sum(count) * 100,
    Vegetation_ordered = fct_reorder(Vegetation, desc(count))
  ) %>%
  ungroup()

lav_physicodf_plot3 <- lav_physicodf_plot3 %>%
  mutate(`n (%)` = paste0(count, " (", round(percent, 1), "%)"))


##Ensure order is preserved
lav_physicodf_plot3$Vegetation_ordered <- factor(
  lav_physicodf_plot3$Vegetation_ordered,
  levels = c("Yes", "No")
)

Fig3f <- ggplot(lav_physicodf_plot3, 
       aes(x = as.factor(`Settlement Type`), 
           y = percent, 
           fill = Vegetation_ordered)) +
  geom_bar(stat = "identity",
           position = position_stack(reverse = TRUE)) +   # <-- enforce stack order
  geom_text(aes(label = `n (%)`),
            position = position_stack(vjust = 0.5, reverse = TRUE),  # match bar
            size = 3) +
  scale_fill_manual(values = c("Yes" = "#b2df8a", "No" = "cornsilk")) +
  labs(
    title = "Proportion of Breeding Sites covered with Vegetation",
    x = "Settlement Type",
    y = "Proportion (%)",
    fill = "Vegetation Presence"
  ) +
  facet_wrap(~ season) +
  theme_manuscript()

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), "/", 'Vegetation Cover by Settlement Type.pdf'), Fig3f, width = 8, height = 9)



#Figure 4 (Habitat positivity and Larval densities)

##Figure 4A: Seasonal distribution of larval habitats positive for Anopheles larvae by type

#Extract data
# Main pie chart: Wet vs Dry
main_data <- lav_overall %>%
  mutate(
    Anopheles_Caught = unlist(Anopheles_Caught),
    Season = unlist(Season)
  ) %>%
  filter(Anopheles_Caught == "Yes") %>%
  group_by(Season) %>%
  summarize(value = n(), .groups = "drop") %>%
  dplyr::select(Season, value)
  
# Inset pie charts: Temporary vs Permanent per season
inset_wet <- lav_overall %>%
  mutate(
    Anopheles_Caught = unlist(Anopheles_Caught),
    Season = unlist(Season)
  ) %>%
  filter(Anopheles_Caught == "Yes") %>%
  filter(Season == "Wet") %>% 
  group_by(Breeding_Site_Recode2) %>%
  summarize(value = n(), .groups = "drop") %>%
  dplyr::select(Breeding_Site_Recode2, value)

inset_dry <- lav_overall %>%
  mutate(
    Anopheles_Caught = unlist(Anopheles_Caught),
    Season = unlist(Season)
  ) %>%
  filter(Anopheles_Caught == "Yes") %>%
  filter(Season == "Dry") %>% 
  group_by(Breeding_Site_Recode2) %>%
  summarize(value = n(), .groups = "drop") %>%
  dplyr::select(Breeding_Site_Recode2, value)


# Add percentages for labeling
main_data <- main_data %>%
  mutate(label = paste0(Season, "\n", value, " (", round(value/sum(value)*100, 1), "%)"))

inset_wet <- inset_wet %>%
  mutate(label = paste0(Breeding_Site_Recode2, "\n", value, " (", round(value/sum(value)*100, 1), "%)"))

inset_dry <- inset_dry %>%
  mutate(label = paste0(Breeding_Site_Recode2, "\n", value, " (", round(value/sum(value)*100, 1), "%)"))

# ---------------------------
# ----Make plots--------------

# Main pie
main_pie <- ggplot(main_data, aes(x = "", y = value, fill = Season)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5) +
  theme_void() +
  scale_fill_brewer(palette = "Set2") +
  ggtitle("Habitats with Anopheles Larvae by Season and type of habitat")

# Insets
inset_wet_pie <- ggplot(inset_wet, aes(x = "", y = value, fill = Breeding_Site_Recode2)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  theme_void() +
  scale_fill_brewer(palette = "Pastel1") +
  ggtitle("Wet Season Breakdown")

inset_dry_pie <- ggplot(inset_dry, aes(x = "", y = value, fill = Breeding_Site_Recode2)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  theme_void() +
  scale_fill_brewer(palette = "Pastel2") +
  ggtitle("Dry Season Breakdown")

# ----Combine-------#
Fig4a <- ggdraw() +
  draw_plot(main_pie, 0, 0, 1, 1) +            # Main pie full canvas
  draw_plot(inset_dry_pie, 0.05, 0.55, 0.3, 0.3) +  # Wet inset
  draw_plot(inset_wet_pie, 0.6, 0.05, 0.3, 0.3)     # Dry inset

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), "/", ' Seasonal distribution of larval habitats positive for Anopheles larvae in Ibadan .pdf'), Fig4a, width = 11, height = 8)


#Figure 4B: Distribution of breeding sites with anopheles larva by settlement type and season
#Summarize data
anoph_data <- lav_overall %>%
  mutate(
    Anopheles_Caught = unlist(Anopheles_Caught),
    Season = unlist(season),
    Settlement = unlist(`Settlement Type`)
  ) %>%
  filter(Anopheles_Caught == "Yes") %>%
  group_by(`Settlement Type`, season) %>%
  summarize(value = n(), .groups = "drop") %>%
  dplyr::select(`Settlement Type`, season, value)

# Make plot
Fig4b <- ggplot(anoph_data, aes(x = `Settlement Type`, y = value, fill = season)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = value), position = position_stack(vjust = 0.5), color = "white", size = 4) +
  labs(title = "Distribution of larval positiv breeding Sites by Settlement and season",
       x = "Settlement",
       y = "Number of breeding sites") +
  scale_fill_manual(values = c("Wet" = "#1f78b4", "Dry" = "sienna")) +
  theme_manuscript()

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), "/", 'Distribution of larval positiv breeding Sites by Settlement and season.pdf'), Fig4b, width = 9, height = 8)


#Figure 4C
##Logistic Regression to determine the odds of findinga a breeding site with anopheles larva

# Convert outcome to binary 
lav_overall <- lav_overall %>%
  mutate(PermanentSite = ifelse(Breeding_Site_Recode2 == "Permanent", 1, 0))
##Create new variables 
lav_overall_1 <- lav_overall %>%
  mutate(TypeSite = ifelse(Breeding_Site_Recode2 == "Artificial", 1, 0),
         Habitat_Positivity = ifelse(Anopheles_Caught == "Yes", 1, 0))

##Remove Informal settlement before running regression
lav_reg <- lav_overall_1 %>% 
  dplyr::filter(!settlement == "Informal")

##Reorder level and factor of outcome
lav_reg$habitat_type <- factor(
  lav_reg$habitat_type,
  levels = c(0, 1),
  labels = c("Permanent", "Temporary")
)

# Column name mapping 
col_settlement   <- "settlement"      # slum / formal
col_season       <- "season"         # dry / wet
col_habitat_type <- "habitat_type"   # permanent / temporary
col_outcome      <- "positive_habitat"  # 1 = positive, 0 = negative

# Ensure outcome is numeric 0/1
lav_reg[[col_outcome]] <- as.integer(as.logical(lav_reg[[col_outcome]]))


# Logistic regression model(Season)
modelseas <- glm(
  as.formula(paste(col_outcome, "~", col_season)),
  data = lav_reg,
  family = binomial
)

# Extract odds ratios with CI
odds_by_season <- broom::tidy(modelseas, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    factor = "Settlement",
    level = gsub(paste0(col_season), "", term)
  ) %>%
  dplyr::select(factor, level, odds_ratio = estimate, lower = conf.low, upper = conf.high)

odds_by_season

# Logistic regression model(Settlement)
modelsett <- glm(
  as.formula(paste(col_outcome, "~", col_settlement)),
  data = lav_reg,
  family = binomial
)

# Extract odds ratios with CI
odds_by_settlement <- broom::tidy(modelsett, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    factor = "Season",
    level = gsub(paste0(col_settlement), "", term)
  ) %>%
  dplyr::select(factor, level, odds_ratio = estimate, lower = conf.low, upper = conf.high)

odds_by_settlement


# Logistic regression model(Habitat)
modelhab <- glm(
  as.formula(paste(col_outcome, "~", col_habitat_type)),
  data = lav_reg,
  family = binomial
)

# Extract odds ratios with CI
odds_by_habitat <- broom::tidy(modelhab, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    factor = "Habitat Type",
    level = gsub(paste0(col_habitat_type), "", term)
  ) %>%
  dplyr::select(factor, level, odds_ratio = estimate, lower = conf.low, upper = conf.high)

odds_by_habitat

unadj <- bind_rows(odds_by_settlement, odds_by_season, odds_by_habitat) %>%
  dplyr::select(factor, level, odds_ratio, lower, upper)

unadj$Type <- "Unadjusted"

# # 2. Adjusted ORs from logistic regression
# adj <- data.frame(
#   factor = c("Settlement", "Season", "Habitat Type"),
#   level  = c("Slum", "Wet", "Temporary"),
#   odds_ratio = exp(coef(model)[-1]),          # remove intercept
#   lower = exp(confint(model)[-1, 1]),
#   upper = exp(confint(model)[-1, 2]),
#   Type = "Adjusted"
# )

# # 3. Combine
# plot_data <- bind_rows(unadj, adj)
# plot_data

##Make plot of only unadjusted odd ratios
Fig4c <- ggplot(unadj, aes(x = odds_ratio, y = level, color = Type)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  #facet_wrap(~factor, scales = "free_y") +
  scale_x_log10() +   # log scale is typical for ORs
  labs(
    x = "Odds Ratio (log scale)",
    y = "",
    color = ""
  ) +
  theme_manuscript()

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), 'Log Regression of habitat positivity.pdf'), Fig4c, width = 10, height = 6) 


##Figure 4D Mean Larval densities by breeding site type, season and settlement

##Read in larval density dataset
lav_den_sum_all <- read.csv(file.path(Entodir, "lav_density_dataset.csv"))

Fig4d <- ggplot(lav_den_sum_all, aes(x = Breeding_Site_Recode, y = AvgLD)) +
  geom_point(aes(color = `Settlement Type`, size = 4.5), , alpha = 0.7) +  
  facet_wrap(~ `season`)+
  scale_color_manual(values = c(Formal = "#f57362", Slum = "#f9caa7"))+
  geom_text(aes(label = round(AvgLD, 2)), vjust = -1.2, hjust = 0.5, size = 3) + 
  geom_text(aes(label = Breeding_Site_Recode), vjust = -1.2, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(2, 10)) + 
  labs(title = "Average Larval Density per Breeding Sites by settlement type ",
       y = "Average Larval Density",
       size = "Average Larval Density") +
  guides(size = FALSE)+
  theme_manuscript()

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), 'Larval Density of Breeding sites by settlement.pdf'), Fig4d, width = 8, height = 6) 


##Figures 4E and 4F(Pareto analysis of Anopheles-positive breeding sites)

#Figure4E
# Comppute Pareto data(Slum)
pareto_data_slum <- lav_den_sum_all %>% 
  dplyr::filter(`Settlement Type` == "Slum") %>%
  arrange(desc(AvgLD), .by_group = TRUE) %>%    # sort within group
  mutate(
    CumSum = cumsum(AvgLD),
    Total = sum(AvgLD),
    CumPerc = CumSum / Total * 100
  ) %>%
  ungroup()

##Summarize Breeding sites in Pareto data
pareto_data_slum <- pareto_data_slum %>% 
  #dplyr::filter(`Settlement Type` == "Slum") %>%
  group_by(Breeding_Site_Recode) %>% 
  summarise(
    AvgLD = mean(AvgLD),
    Total = first(Total)      # keep the original Total
  ) %>% 
  arrange(desc(AvgLD), .by_group = TRUE) %>%    # sort within group
  mutate(
    CumSum = cumsum(AvgLD),
    Total = sum(AvgLD),
    CumPerc = CumSum / Total * 100
  ) %>%
  ungroup()


# Reorder globally by AvgLD
pareto_data_slum <- pareto_data_slum %>%
  #group_by(`Settlement Type`) %>%
  arrange(desc(AvgLD)) %>%
  mutate(Breeding_Site_Recode = factor(Breeding_Site_Recode, levels = unique(Breeding_Site_Recode))) %>%
  ungroup()

# Make Plot

Fig4e <- ggplot(pareto_data_slum, aes(x = reorder(Breeding_Site_Recode, -AvgLD), y = AvgLD)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(y = CumPerc * max(AvgLD)/100, group = 1), color = "red", size = 1) +
  geom_point(aes(y = CumPerc * max(AvgLD)/100), color = "red", size = 2) +
  scale_y_continuous(
    name = "Average Larval Density",
    limits = c(0, max(pareto_data_slum$AvgLD)),  # ensures main axis starts at 0
    sec.axis = sec_axis(~ . * 100 / max(pareto_data_slum$AvgLD),
                        name = "Cumulative %", 
                        breaks = seq(0, 100, 20))  # secondary axis from 0 to 100
  ) +
  geom_hline(yintercept = 0.8 * max(pareto_data_slum$AvgLD), 
             linetype = "dashed", color = "darkgreen", size = 1) +
  labs(x = "Breeding Site Type", 
       title = "Pareto Plot of Breeding Sites by Average Larval Density (Slum") +
  theme_manuscript() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), "/", ' Pareto Plot of Breeding Sites by Average Larval Density (Slum) .pdf'), Fig4e, width = 9, height = 8)



# Compute Pareto data(Formal)
pareto_data_formal <- lav_den_sum_all %>% 
  dplyr::filter(`Settlement Type` == "Formal") %>%
  arrange(desc(AvgLD), .by_group = TRUE) %>%    # sort within group
  mutate(
    CumSum = cumsum(AvgLD),
    Total = sum(AvgLD),
    CumPerc = CumSum / Total * 100
  ) %>%
  ungroup()

##Summarize Breeding sites in Pareto data
pareto_data_formal <- pareto_data_formal %>% 
  dplyr::filter(`Settlement Type` == "Formal") %>%
  group_by(Breeding_Site_Recode) %>% 
  summarise(
    AvgLD = mean(AvgLD),
    Total = first(Total)      
  ) %>% 
  arrange(desc(AvgLD), .by_group = TRUE) %>%    # sort within group
  mutate(
    CumSum = cumsum(AvgLD),
    Total = sum(AvgLD),
    CumPerc = CumSum / Total * 100
  ) %>%
  ungroup()


# Reorder globally by AvgLD
pareto_data_formal <- pareto_data_formal %>%
  #group_by(`Settlement Type`) %>%
  arrange(desc(AvgLD)) %>%
  mutate(Breeding_Site_Recode = factor(Breeding_Site_Recode, levels = unique(Breeding_Site_Recode))) %>%
  ungroup()

# Make Plot

Fig4f <- ggplot(pareto_data_formal, aes(x = reorder(Breeding_Site_Recode, -AvgLD), y = AvgLD)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(y = CumPerc * max(AvgLD)/100, group = 1), color = "red", size = 1) +
  geom_point(aes(y = CumPerc * max(AvgLD)/100), color = "red", size = 2) +
  scale_y_continuous(
    name = "Average Larval Density",
    limits = c(0, max(pareto_data_formal$AvgLD)),  # ensures main axis starts at 0
    sec.axis = sec_axis(~ . * 100 / max(pareto_data_formal$AvgLD),
                        name = "Cumulative %", 
                        breaks = seq(0, 100, 20))  # secondary axis from 0 to 100
  ) +
  geom_hline(yintercept = 0.8 * max(pareto_data_formal$AvgLD), 
             linetype = "dashed", color = "darkgreen", size = 1) +
  labs(x = "Breeding Site Type", 
       title = "Pareto Plot of Breeding Sites by Average Larval Density (Formal)") +
  theme_manuscript() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), "/", ' Pareto Plot of Breeding Sites by Average Larval Density (Formal) .pdf'), Fig4f, width = 9, height = 8)





#Figure 5A
##Read in breeding site summaries generated from scripts 
#"02_breeding site analysis_Aguguslumwet.R", "02_breeding site analysis_Aguguslumwet.R"
#"02_breeding site analysis_Challengeformalwet.R", "01_breeding site analysis_Olopomewa.R"

Agudry_summary <- read.csv(file.path(Entodir, "Agudry_summary.csv"))
Aguwet_summary <- read.csv(file.path(Entodir, "Aguwet_summary.csv"))
Chal_summary <- read.csv(file.path(Entodir, "Chal_summary.csv"))
Olop_summary <- read.csv(file.path(Entodir, "Olop_summary.csv"))

##Merge all files
den_summary_all <- rbind(Agudry_summary, Aguwet_summary, Chal_summary, Olop_summary)

##Boxplot of breeding site densities
# Convert to long format
den_summary_long <- den_summary_all %>%
  pivot_longer(
    cols = c(mean_density, sd_density, min_density, max_density),
    names_to = "Density_Metric",
    values_to = "Value"
  )

# # View result
# head(den_summary_long)

##Make boxplot
Fig5a <- ggplot(den_summary_long, aes(x = settlment, y = Value, fill = settlment)) +
  geom_boxplot() +
  facet_wrap(~ season)+
  scale_fill_manual(values = c(Formal = "#f57362", Slum = "#f9caa7"))+
  labs(
    title = "Distribution of Breeding Site Densities by settlement and season",
    x = "Ward",
    y = "Breeding Site Density (sites/km²)"
  ) +
  theme_manuscript() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), "/", ' Pareto Plot of Breeding Sites by Average Larval Density (Slum) .pdf'), Fig5a, width = 9, height = 8)




##Figure 5B: Kernel weighted analysis of assumed dispersal scales
## This script has the data wrangling and analysis
##"Breeding site and Household Malaria Status analysis002.R"

##Read in final kernel weighted dataframe
kernel_filtered <- read.csv(file.path(Entodir, "kernel_filtered.csv"))


##Making plots for manuscript
# Plot
Fig5b <- ggplot(kernel_filtered, aes(x = lambda, y = OR, color = settlement, linetype = settlement)) +
  
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

ggsave(paste0(Lavplotsdir,"/", Sys.Date(), "/", 'kernel decay by buffer distance.pdf'), Fig5b, width = 8, height = 6)


