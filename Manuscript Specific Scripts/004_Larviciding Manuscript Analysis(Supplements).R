#loadpath
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
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

#Summarize data by season
season_summary <- lav_mol_df_all %>%
  group_by(season, Species) %>%
  summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop")

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
  group_by(season, Anopheles_Caught) %>%
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
                 aes(x = season,
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
  #facet_wrap(~ season) +
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



