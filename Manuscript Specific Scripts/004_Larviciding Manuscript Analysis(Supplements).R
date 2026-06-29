#loadpath
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[//]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
shapefileDir <- "C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan"
Entodir <- "C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento"
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


##Supplementary Figure 1A and 1B
##Types of breeding sites prospected(Dry and Wet Season)

##Supplementary Figure 1A(Dry Season)
#lav_df_dry
##Recode breeding sites to match
lav_df_dry <- lav_df_dry %>% 
  mutate(Breeding_Site_Recode = recode(`Breeding site`,
                                       "Abandoned well" = "Dug Well",
                                       "OpenDrain/Puddle" = "Open Drain/Puddles",
                                       "Tank" = "Open Tank",
                                       "Stream" = "Canal"))

# Summarize data by breeding site type
breeding_site_sum_dry <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo") %>% 
  group_by(Breeding_Site_Recode) %>%  
  summarize(
    SitesVisited = n(),  
  ) 

# Calculate percentages
breeding_site_sum_dry <- breeding_site_sum_dry %>%
  mutate(Percentage = SitesVisited / sum(SitesVisited) * 100,
         Label = paste0(SitesVisited, " (", round(Percentage, 1), "%)"))  

#Number and type of breeding sites
S1A <- ggplot(data = breeding_site_sum_dry, aes(x = "", y = SitesVisited, fill = Breeding_Site_Recode)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +  
  scale_fill_brewer(palette = "Pastel1")+
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "black", size = 2.5) +  # Center text with both count and percentage
  theme_void() +  
  theme(legend.position = "right") +  
  ggtitle("Number and type of breeding sites visited in Ibadan, Jan-March, 2023")

print(S1A)

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), 'dry season breeding sites visited in Ibadan2.pdf'), S1A, width = 8, height = 6)



#Supplementary Figure 1B (Wet Season(S1B))
#Breeding site analysis
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

##Recode breeding sites to macth
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

# Summarize data by breeding site type
breeding_site_sum_wet <- lav_df_wet %>% 
  filter(!is.na(`Breeding_Site_Recode`)) %>%
  group_by(`Breeding_Site_Recode`) %>%  
  summarize(
    SitesVisited = n(),  
  ) 

# Calculate percentages
breeding_site_sum_wet <- breeding_site_sum_wet %>%
  mutate(Percentage = SitesVisited / sum(SitesVisited) * 100,
         Label = paste0(SitesVisited, " (", round(Percentage, 1), "%)"))  

#Number and type of breeding sites
##Make plots
S1B <- ggplot(data = breeding_site_sum_wet, aes(x = "", y = SitesVisited, fill = `Breeding_Site_Recode`)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +  
  scale_fill_brewer(palette = "Pastel1")+
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "black", size = 2.5) +  # Center text with both count and percentage
  theme_void() +  
  theme(legend.position = "right") +  
  ggtitle("Number and type of breeding sites visited in Ibadan, July-August, 2024")

print(S1B)

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), 'wet season breeding sites visited in Ibadan2.pdf'), S1B, width = 8, height = 6)


##Supplementary Figure 1C
##Generate dataframe from figures in report
data <- tibble::tibble(
  Season = c("Dry", "Wet"),
  Larvae = c(75, 989),
  Adults  = c(15, 25)
) %>%
  mutate(
    Survival = Adults / Larvae
  )

attrition_data <- data %>%
  pivot_longer(cols = c(Larvae, Adults),
               names_to = "Stage",
               values_to = "Count") %>%
  mutate(
    Stage = factor(Stage, levels = c("Larvae", "Adults"))
  )

##Make plot of seasonal attrition
S1C <- ggplot(attrition_data,
       aes(x = Stage, y = Count, group = Season)) +
  geom_line(aes(linetype = Season),
            linewidth = 1, color = "grey30") +
  geom_point(aes(shape = Season),
             size = 3, color = "black") +
  geom_text(aes(label = Count, hjust = -1))+
  facet_wrap(~ Season, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  labs(
    x = NULL,
    y = "Number of individuals",
    title = "Seasonal attrition of Anopheles during laboratory rearing"
  ) +
  theme_manuscript()

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), 'Seasonal attrition of Anopheles during laboratory rearing.pdf'), S1C, width = 8, height = 6) 


##Morphological classification Analysis(Supplementary Figure 1D)
##Wet Season
lav_mol_df <- read_excel(file.path(Entodir, "Wet Season Data_Ibadan", "Molecular ID for larval mosquitoes.xlsx"))

lav_mol_df_rec <- lav_mol_df %>%
  group_by(`Location`, `Breeding site`) %>%
  summarise(count = n())

lav_mol_df_rec$Species <- "An_coluzii"

lav_mol_df_rec$season <- "Wet"

lav_mol_df_wet <- lav_mol_df_rec %>%
  mutate(settlement = case_when(
    Location == "Agugu" ~ "Slum",
    Location == "Challenge" ~ "Formal"
  ))

lav_mol_df_wet <- lav_mol_df_wet %>%
  mutate(`Breeding site` = case_when(
    `Breeding site` == "Puddle" ~ "Open Drain/Puddles",
    `Breeding site` == "Tyre" ~ "Tyre",
    `Breeding site` == "Plastic" ~ "Artificial containers",
    `Breeding site` == "Gutter" ~ "Drainage/Gutter/Dithces",
  ))

colnames(lav_mol_df_wet)[1] <- "Ward"


# Create dataframe for Dry Season
dt <- data.frame(
  Ward = c("Agugu", "Olopomewa", "Challenge", "Agugu", "Olopomewa", "Challenge"),
  Location = c("Drainage/Gutter/Dithces", "Drainage/Gutter/Dithces", "Drainage/Gutter/Dithces", "Tyre tracks", "Tyre tracks", "Tyre tracks"),
  An_gambiae_ss = c(8, 3, 0, 2, 0, 0),
  An_coluzzii = c(2, 0, 0, 0, 0, 0)
)

# Convert data to long format
data_long <- pivot_longer(dt, cols = c(An_gambiae_ss, An_coluzzii), names_to = "Species", values_to = "Value")

lav_mol_df_dry <- data_long[data_long$Value > 0, ]

lav_mol_df_dry$season <- "Dry"


lav_mol_df_dry <- lav_mol_df_dry %>%
  mutate(settlement = case_when(
    Ward == "Agugu" ~ "Slum",
    Ward == "Olopomewa" ~ "Formal"
  ))

colnames(lav_mol_df_dry)[2] <- "Breeding site"

colnames(lav_mol_df_dry)[4] <- "count"

#Combine Dry and Wet season 
lav_mol_df_all <- rbind(lav_mol_df_dry, lav_mol_df_wet)

#Correct Specie name
lav_mol_df_all <- lav_mol_df_all %>%
  mutate(Species = recode(Species,
                          "An_coluzii" = "An_coluzzii"))

# Summarize data by season and species, and add proportions
season_summary <- lav_mol_df_all %>%
  group_by(season, Species) %>%
  summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  group_by(season) %>%
  mutate(
    season_total = sum(total_count),
    proportion = total_count / season_total * 100
  ) %>%
  ungroup()

season_summary



# Visualize with stacked bar plot
S1D <- ggplot(season_summary, aes(x = season, y = total_count , fill = Species)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c(An_gambiae_ss = "#ffdbac", An_coluzzii = "#fcbf49"))+
  #facet_wrap(~season)+
  labs(title = "Distribution of adult larvae(mosquito) by Species Type", x = "Season", y = "Number of adult mosquito reared by specie") +
  theme_manuscript() +
  theme(legend.position = "right")

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), 'Number of adult mosquito reared by specie.pdf'), S1D, width = 8, height = 6) 



##Supplementary Figures 1E and F
##Physicochemical dataframe "lav_physicodf_dry_wet" created in 001 script 

##Supplementary Figure 1F
#pH
range(lav_physicodf_dry_wet$pH, na.rm = TRUE)
mean(lav_physicodf_dry_wet$pH, na.rm = TRUE)

# Compute mean and SD by season(S1E)
summary_pH_overall <- lav_physicodf_dry_wet %>%
  group_by(Ward.Name, season, Anopheles_Caught) %>%
  summarise(
    mean_pH = mean(pH, na.rm = TRUE),
    sd_pH = sd(pH, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(round(mean_pH, 1), " ± ", round(sd_pH, 1))
  )

##Make seasonal plot
S1E <- ggplot(summary_pH_overall,
                 aes(x = Ward.Name,
                     y = mean_pH,
                     color = Anopheles_Caught,
                     group = Anopheles_Caught)) +
  geom_point(
    position = position_dodge(width = 0.4),
    size = 3
  ) +
  geom_errorbar(
    aes(ymin = mean_pH - sd_pH,
        ymax = mean_pH + sd_pH),
    width = 0.15,
    position = position_dodge(width = 0.4)
  ) +
  geom_text(
    aes(label = label),
    position = position_dodge(width = 0.4),
    vjust = -1,
    size = 3
  ) +
  facet_wrap(~ season) +
  scale_color_manual(
    name   = "Anopheles larvae",
    values = c(
      "Yes" = "#1b9e77",  # green
      "No"  = "#7570b3"   # muted purple
    ),
    labels = c(
      "Yes" = "Present",
      "No"  = "Absent"
    )
  )+
  labs(
    title = "Mean pH of Breeding Sites by Season",
    x = "Settlement Type",
    y = "Mean pH (± SD)",
    color = "Anopheles Larvae"
  ) +
  theme_manuscript()

print(S1E)

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), "/", 'pH Distribution of breeding sites(overall).pdf'), S1E, width = 8, height = 6)



##(S1F)
# Compute mean and SD per Settlement Type and season
summary_pH <- lav_physicodf_dry_wet %>%
  group_by(season, `Settlement Type`, Anopheles_Caught) %>%
  summarise(
    mean_pH = mean(pH, na.rm = TRUE),
    sd_pH = sd(pH, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(round(mean_pH, 1), " ± ", round(sd_pH, 1))
  )



##Make season and settlement plot
S1F <- ggplot(summary_pH,
                 aes(x = `Settlement Type`,
                     y = mean_pH,
                     color = Anopheles_Caught,
                     group = Anopheles_Caught)) +
  geom_point(
    position = position_dodge(width = 0.4),
    size = 3
  ) +
  geom_errorbar(
    aes(ymin = mean_pH - sd_pH,
        ymax = mean_pH + sd_pH),
    width = 0.15,
    position = position_dodge(width = 0.4)
  ) +
  geom_text(
    aes(label = label),
    position = position_dodge(width = 0.4),
    vjust = -1,
    size = 3
  ) +
  facet_wrap(~ season) +
  scale_color_manual(
    name   = "Anopheles larvae",
    values = c(
      "Yes" = "#1b9e77",  # green
      "No"  = "#7570b3"   # muted purple
    ),
    labels = c(
      "Yes" = "Present",
      "No"  = "Absent"
    )
  )+
  labs(
    title = "Mean pH of Breeding Sites by Settlement Type and Season",
    x = "Settlement Type",
    y = "Mean pH (± SD)",
    color = "Anopheles Larvae"
  ) +
  theme_manuscript()

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), "/", 'pH Distribution of breeding sites.pdf'), S1F, width = 8, height = 6)



###New physicochemical plots for positive breeding sites (22nd June, 2026)
##Supplement 3A: Percentage of breeding site by water nature
##Extract for only positive sites
lav_physicodf_dry_wet <- read.csv(file.path(Entodir, "lav_physico_dataset.csv"))

poslav_physicodf_dry_wet <- lav_physicodf_dry_wet %>% 
  dplyr::filter(Anopheles_Caught == "Yes")
##Recode and clean variable names
#Water Origin
poslav_physicodf_dry_wet <- poslav_physicodf_dry_wet %>%
  mutate(
    origin_clean = case_when(
      # Rain categories
      str_detect(`Origin.of.water`, regex("^rain", ignore_case = TRUE)) ~ "Rain",
      
      # River
      str_detect(`Origin.of.water`, regex("river", ignore_case = TRUE)) ~ "River/Stream",
      
      # Drainage / gutter / ditch
      str_detect(`Origin.of.water`, regex("drain|ditch|gutter", ignore_case = TRUE)) ~ "Drainage/Gutter",
      
      # Waste water / sewage
      str_detect(`Origin.of.water`, regex("waste|sewage", ignore_case = TRUE)) ~ "Waste water/Sewage",
      
      # Domestic / Household
      str_detect(`Origin.of.water`, regex("domestic|household", ignore_case = TRUE)) ~ "Household/Domestic",
      
      # Man-made containers
      str_detect(`Origin.of.water`, regex("man made|manmade", ignore_case = TRUE)) ~ "Man-made",
      
      # Borehole
      str_detect(`Origin.of.water`, regex("borehole", ignore_case = TRUE)) ~ "Borehole",
      
      # Well
      str_detect(`Origin.of.water`, regex("well", ignore_case = TRUE)) ~ "Well",
      
      # Default
      TRUE ~ "Other/Unknown"
    )
  )


poslav_physicodf_dry_wet <- poslav_physicodf_dry_wet %>%
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
poslav_physicodf_plot <- poslav_physicodf_dry_wet %>%
  group_by(`Ward.Name`, season, waterorigin4) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(`Ward.Name`, season) %>%
  mutate(
    percent = count / sum(count) * 100,
    waterorigin4_ordered = fct_reorder(waterorigin4, desc(count))
  ) %>%
  ungroup()


##Ensure order is preserved
poslav_physicodf_plot$waterorigin4_ordered <- factor(
  poslav_physicodf_plot$waterorigin4_ordered,
  levels = c(
    "Rainwater",
    "Groundwater",
    "Anthropogenic/Domestic",
    "Surface water"
  )
)

# Make Plot
Supl3a <- ggplot(poslav_physicodf_plot, aes(x = `Ward.Name`, y = percent, fill = waterorigin4_ordered)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = paste0(count, " (", round(percent, 1), "%)")),
            position = position_stack(vjust = 0.5), 
            size = 3)  +
  facet_grid(~season) +
  scale_y_continuous(labels = percent_format(scale = 1)) +  
  scale_fill_brewer(palette = "Set3") +
  labs(
    x = "Ward",
    y = "Percentage of Water Sources",
    fill = "Water Origin",
    title = "Distribution of Water Origins by Ward"
  ) +
  theme_manuscript()

print(Supl3a)

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), "/", 'Water Origin of breeding sites.pdf'), Fig3d, width = 8, height = 6)


#Suppl 3b: Percentage of breeding sites by water nature
##Water Nature
#table(lav_physicodf_dry_wet$waternature)

poslav_physicodf_dry_wet <- poslav_physicodf_dry_wet %>%
  mutate(waternature = case_when(
    `Water.nature` == "Clear" ~ "Clean",
    
    `Water.nature` %in% c("Clean", "clean") ~ "Clear",
    
    `Water.nature` %in% c("polluted",
                          "Polluted") ~ "Polluted"
  ))

# Calculate counts and percentages within each Settlement Type
poslav_physicodf_plot2 <- poslav_physicodf_dry_wet %>%
  dplyr::group_by(`Ward.Name`, season, waternature) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(`Ward.Name`, season) %>%
  mutate(
    percent = count / sum(count) * 100,
    waternature_ordered = fct_reorder(waternature, desc(count))
  ) %>%
  ungroup()

#Make Plot by Season and Settlement Type
Supl3b <- ggplot(
  poslav_physicodf_plot2,
  aes(x = `Ward.Name`, y = percent, fill = waternature_ordered)
) +
  geom_col(width = 0.7, color = "black") +
  geom_text(aes(label = paste0(count, " (", round(percent, 1), "%)")),
            position = position_stack(vjust = 0.5), 
            size = 3) +
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

print(Supl3b)

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), "/", 'Nature of Water by Ward and Season.pdf'), Fig3e, width = 8, height = 6)




##Sunlight
poslav_physicodf_dry_wet <- poslav_physicodf_dry_wet %>%
  mutate(sunlight = case_when(
    `Sunlight.exposure` %in% c("Yes", 
                               "sunlit",
                               "Sunlit",
                               "sunnlit") ~ "Yes",
    
    `Sunlight.exposure` %in% c("No",
                               "Shaded") ~ "No"
  ))

table(poslav_physicodf_dry_wet$sunlight)

#Suppl 3C: Percentage of breeding site type by vegetation
##Vegetation
table(poslav_physicodf_dry_wet$Vegetation)

poslav_physicodf_dry_wet <- poslav_physicodf_dry_wet %>%
  mutate(Vegetation = case_when(
    `Vegetation` %in% c("Yes", 
                        "yes",
                        "submerge",
                        "Submerge") ~ "Yes",
    
    `Vegetation` %in% c("No",
                        "no") ~ "No"
  ))


# Calculate counts and percentages within each Settlement Type
poslav_physicodf_plot3 <- poslav_physicodf_dry_wet %>%
  group_by(season, `Ward.Name`, Vegetation) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(season, `Ward.Name`) %>%
  mutate(
    percent = count / sum(count) * 100,
    Vegetation_ordered = fct_reorder(Vegetation, desc(count))
  ) %>%
  ungroup()

poslav_physicodf_plot3 <- poslav_physicodf_plot3 %>%
  mutate(`n (%)` = paste0(count, " (", round(percent, 1), "%)"))


##Ensure order is preserved
poslav_physicodf_plot3$Vegetation_ordered <- factor(
  poslav_physicodf_plot3$Vegetation_ordered,
  levels = c("Yes", "No")
)

Supl3c <- ggplot(poslav_physicodf_plot3, 
                aes(x = as.factor(`Ward.Name`), 
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
    x = "Ward",
    y = "Proportion (%)",
    fill = "Vegetation Presence"
  ) +
  facet_wrap(~ season) +
  theme_manuscript()

print(Supl3c)

ggsave(paste0(Lavplotsdir, '/plots/', Sys.Date(), "/", 'Vegetation Cover by Settlement Type.pdf'), Fig3f, width = 8, height = 9)


##Overall pH summaries
# Compute mean and SD by season(S1E)
sum_pH_overall_seas <- lav_physicodf_dry_wet %>%
  group_by(season) %>%
  summarise(
    mean_pH = mean(pH, na.rm = TRUE),
    sd_pH = sd(pH, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(round(mean_pH, 1), " ± ", round(sd_pH, 1))
  )

sum_pH_overall_ward <- lav_physicodf_dry_wet %>%
  group_by(Ward.Name) %>%
  summarise(
    mean_pH = mean(pH, na.rm = TRUE),
    sd_pH = sd(pH, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(round(mean_pH, 1), " ± ", round(sd_pH, 1))
  )


kruskal.test(pH ~ season, data = lav_physicodf_dry_wet)
kruskal.test(pH ~ Ward.Name, data = lav_physicodf_dry_wet)


#Checking for longitudinal nature of larval prospection

lav_df_wet_check <- lav_df_wet %>% 
  dplyr::select(X, Date, WardName, X_Breeding.site.coordinates_latitude, X_Breeding.site.coordinates_longitude)


library(tidyverse)
library(sf)
library(leaflet)
library(lubridate)

# ── 1. Prepare data ──────────────────────────────────────────────────────────
df <- lav_df_wet_check %>%
  mutate(
    DateTime = mdy_hm(Date),
    Day      = as.Date(DateTime),
    Zone     = ifelse(X_Breeding.site.coordinates_latitude > 7.36, "North (~7.38°N)", "South (~7.34°N)")
  )

# Convert to sf object
df_sf <- st_as_sf(df,
                  coords = c("X_Breeding.site.coordinates_longitude",
                             "X_Breeding.site.coordinates_latitude"),
                  crs = 4326
)

# ── 2. Load your shapefile (swap path when ready) ────────────────────────────
# shp <- st_read("path/to/your_shapefile.shp") %>% st_transform(crs = 4326)

# ── 3. Static ggplot2 map ────────────────────────────────────────────────────
ggplot() +
  # Uncomment when shapefile is ready:
  # geom_sf(data = shp, fill = "grey92", color = "grey60", linewidth = 0.4) +
  geom_sf(data = df_sf,
          aes(color = Zone, shape = Zone),
          size = 2, alpha = 0.75) +
  facet_wrap(~Day, ncol = 4) +
  scale_color_manual(values = c("North (~7.38°N)" = "#1D9E75",
                                "South (~7.34°N)" = "#378ADD")) +
  scale_shape_manual(values = c("North (~7.38°N)" = 16,
                                "South (~7.34°N)" = 17)) +
  labs(
    title    = "Larval sampling locations by day",
    subtitle = "Two zones sampled simultaneously on most days",
    color    = "Zone", shape = "Zone",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 9)
  )

ggsave("sampling_points_by_day.png", width = 14, height = 10, dpi = 300)

# ── 4. Interactive leaflet map (all days together) ───────────────────────────
pal <- colorFactor(
  palette = c("#1D9E75", "#378ADD"),
  domain  = df$Zone
)

m <- leaflet(df) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  # Uncomment when shapefile is ready:
  # addPolygons(data = shp, color = "#555", weight = 1.5,
  #             fillColor = "transparent", group = "Study area") %>%
  addCircleMarkers(
    lng    = ~X_Breeding.site.coordinates_longitude,
    lat    = ~X_Breeding.site.coordinates_latitude,
    radius = 5,
    color  = ~pal(Zone),
    fillOpacity = 0.8,
    stroke = FALSE,
    popup  = ~paste0(
      "<b>", Date, "</b><br>",
      "Zone: ", Zone, "<br>",
      "Lat: ", round(X_Breeding.site.coordinates_latitude, 5), "<br>",
      "Lon: ", round(X_Breeding.site.coordinates_longitude, 5)
    )
  ) %>%
  addLegend("bottomright",
            pal    = pal,
            values = ~Zone,
            title  = "Sampling zone"
  ) %>%
  addLayersControl(
    overlayGroups = c("Study area"),
    options = layersControlOptions(collapsed = FALSE)
  )


library(leaflet)
library(htmlwidgets)
library(lubridate)
library(tidyverse)

ag_pts <- st_read("C:/Users/ebamgboye/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/Larvalsites_Agugu_Wet.shp")
ch_pts <- st_read("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/Challenge wetseason larval data.shp")

# ── Prep data ────────────────────────────────────────────────────────────────
library(leaflet)
library(htmlwidgets)
library(lubridate)
library(tidyverse)
library(sf)

# ── Prep data ────────────────────────────────────────────────────────────────
df <- lav_df_wet_check %>%
  mutate(
    DateTime = mdy_hm(Date),
    Day      = as.character(as.Date(DateTime)),
    Zone     = ifelse(X_Breeding.site.coordinates_latitude > 7.36,
                      "North", "South")
  )

# ── Ensure shapefiles are in WGS84 (EPSG:4326) to match leaflet ──────────────
df_ib_c <- st_transform(df_ib_c, crs = 4326)
df_ib_a <- st_transform(df_ib_a, crs = 4326)

# ── Color palette by date ────────────────────────────────────────────────────
dates     <- sort(unique(df$Day))
pal_color <- colorFactor(
  palette = colorRampPalette(c("#1D9E75", "#378ADD", "#D85A30",
                               "#D4537E", "#BA7517", "#534AB7",
                               "#639922", "#E24B4A", "#888780",
                               "#5DCAA5", "#F09595", "#85B7EB"))(length(dates)),
  domain = dates
)

# ── Build map ────────────────────────────────────────────────────────────────
m <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  fitBounds(
    lng1 = min(df$X_Breeding.site.coordinates_longitude) - 0.005,
    lat1 = min(df$X_Breeding.site.coordinates_latitude)  - 0.005,
    lng2 = max(df$X_Breeding.site.coordinates_longitude) + 0.005,
    lat2 = max(df$X_Breeding.site.coordinates_latitude)  + 0.005
  ) %>%
  
  # ── Shapefiles as toggleable layers ────────────────────────────────────────
  addPolygons(
    data        = df_ib_c,
    color       = "#534AB7",       # purple border
    weight      = 2,
    fillColor   = "#534AB7",
    fillOpacity = 0.1,
    group       = "df_ib_c",
    popup       = ~paste0("<b>Site:</b> ", if("name" %in% names(df_ib_c)) name else "df_ib_c")
  ) %>%
  addPolygons(
    data        = df_ib_a,
    color       = "#D85A30",       # coral border
    weight      = 2,
    fillColor   = "#D85A30",
    fillOpacity = 0.1,
    group       = "df_ib_a",
    popup       = ~paste0("<b>Site:</b> ", if("name" %in% names(df_ib_a)) name else "df_ib_a")
  )

# ── Add sampling points per date ─────────────────────────────────────────────
for (d in dates) {
  df_day <- df %>% filter(Day == d)
  m <- m %>%
    addCircleMarkers(
      data        = df_day,
      lng         = ~X_Breeding.site.coordinates_longitude,
      lat         = ~X_Breeding.site.coordinates_latitude,
      radius      = 6,
      color       = pal_color(d),
      fillColor   = pal_color(d),
      fillOpacity = 0.85,
      stroke      = TRUE,
      weight      = 1.5,
      opacity     = 1,
      group       = d,
      popup       = ~paste0(
        "<b>Date:</b> ", Date,  "<br>",
        "<b>Zone:</b> ", Zone,  "<br>",
        "<b>Lat:</b> ",  round(X_Breeding.site.coordinates_latitude,  5), "<br>",
        "<b>Lon:</b> ",  round(X_Breeding.site.coordinates_longitude, 5)
      )
    )
}

# ── Legend + layer toggle ────────────────────────────────────────────────────
m <- m %>%
  addLegend(
    position = "bottomright",
    pal      = pal_color,
    values   = df$Day,
    title    = "Sampling date",
    opacity  = 0.9
  ) %>%
  addLayersControl(
    overlayGroups = c("df_ib_c", "df_ib_a", dates),  # shapefiles + dates all toggleable
    options       = layersControlOptions(collapsed = FALSE)
  )

# ── Save and open ─────────────────────────────────────────────────────────────
saveWidget(m, "sampling_map_with_shapefiles.html", selfcontained = TRUE)
browseURL("sampling_map_with_shapefiles.html")