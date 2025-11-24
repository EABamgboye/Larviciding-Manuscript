#loadpath
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
shapefileDir <- "C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan"
Entodir <- "C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento"

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
## read ibadan ward shape files
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

##---End Data Preparation------------------------------------------------------##

##--- DESCRIPTIVE ANALYSIS----------------------------------------------------##
#------------------------------------------------------------------------------#

##Breeding site analysis
##Dry Season
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
bs_dry1 <- ggplot(data = breeding_site_sum_dry, aes(x = "", y = SitesVisited, fill = Breeding_Site_Recode)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +  
  scale_fill_brewer(palette = "Pastel1")+
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "black", size = 2.5) +  # Center text with both count and percentage
  theme_void() +  
  theme(legend.position = "right") +  
  ggtitle("Number and type of breeding sites visited in Ibadan, Jan-March, 2023")

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'dry season breeding sites visited in Ibadan2.pdf'), bs_dry1, width = 8, height = 6)

###New breeding site analysis(Two groups)
##Dry Season
##Recode breeding sites to match
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



# Summarize data by breeding site type
breeding_site_sum_dry2 <- lav_df_dry %>% 
  dplyr::filter(State=="Oyo") %>% 
  group_by(Breeding_Site_Recode2) %>%  
  summarize(
    SitesVisited = n(),  
  ) 

# Calculate percentages
breeding_site_sum_dry2 <- breeding_site_sum_dry2 %>%
  mutate(Percentage = SitesVisited / sum(SitesVisited) * 100,
         Label = paste0(SitesVisited, " (", round(Percentage, 1), "%)"))  

#Number and type of breeding sites
bs_dry2 <- ggplot(data = breeding_site_sum_dry2, aes(x = "", y = SitesVisited, fill = Breeding_Site_Recode2)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +  
  scale_fill_brewer(palette = "Pastel1")+
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "black", size = 2.5) +  # Center text with both count and percentage
  theme_void() +  
  theme(legend.position = "right") +  
  ggtitle("Number and type of breeding sites visited in Ibadan, Jan-March, 2023")

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'dry season breeding sites visited in Ibadan2grps.pdf'), bs_dry2, width = 8, height = 6)


#Breeding site analysis
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
bs_wet1 <- ggplot(data = breeding_site_sum_wet, aes(x = "", y = SitesVisited, fill = `Breeding_Site_Recode`)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +  
  scale_fill_brewer(palette = "Pastel1")+
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "black", size = 2.5) +  # Center text with both count and percentage
  theme_void() +  
  theme(legend.position = "right") +  
  ggtitle("Number and type of breeding sites visited in Ibadan, July-August, 2024")

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'wet season breeding sites visited in Ibadan2.pdf'), bs_wet1, width = 8, height = 6)

##New Breeding site analysis(2 groups)
##Recode breeding sites to macth
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

# Summarize data by breeding site type
breeding_site_sum_wet2 <- lav_df_wet %>% 
  filter(!is.na(`Breeding_Site_Recode`)) %>%
  group_by(`Breeding_Site_Recode2`) %>%  
  summarize(
    SitesVisited = n(),  
  ) 

# Calculate percentages
breeding_site_sum_wet2 <- breeding_site_sum_wet2 %>%
  mutate(Percentage = SitesVisited / sum(SitesVisited) * 100,
         Label = paste0(SitesVisited, " (", round(Percentage, 1), "%)"))  

#Number and type of breeding sites
##Make plots
bs_wet2 <- ggplot(data = breeding_site_sum_wet2, aes(x = "", y = SitesVisited, fill = `Breeding_Site_Recode2`)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +  
  scale_fill_brewer(palette = "Pastel1")+
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "black", size = 2.5) +  # Center text with both count and percentage
  theme_void() +  
  theme(legend.position = "right") +  
  ggtitle("Number and type of breeding sites visited in Ibadan, July-August, 2024")

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'wet season breeding sites visited in Ibadan2grps.pdf'), bs_wet2, width = 8, height = 6)


##Larval Abundance
###Number and type of breeding sites anopheles was caught
##Dry Season
b_site_ano_sum_dry_sett <- lav_df_dry %>% 
  filter(!is.na(`Breeding_Site_Recode`)) %>% 
  group_by(`Anopheles_Caught`, `Settlement Type`, `Breeding_Site_Recode`, `Breeding_Site_Recode2`) %>%  # Group by breeding site type
  summarize(
    SitesVisited = n(),  # Number of sites visited per type
  )

b_site_ano_sum_dry_sett1 <- lav_df_dry %>%
  filter(!is.na(`Breeding_Site_Recode`)) %>%
  group_by(Anopheles_Caught, `Breeding_Site_Recode`, `Breeding_Site_Recode2`) %>%
  summarise(
    SitesVisited = n(),
    .groups = 'drop'
  )

##Convert data to wide to compute proportion of breeding site with larva
b_site_ano_sum_dry_wide_sett <- b_site_ano_sum_dry_sett %>% 
  pivot_wider(names_from = Anopheles_Caught, values_from = SitesVisited, values_fill = 0) %>%
  mutate(
    Total_Sites = No + Yes, 
    Proportion_Larvae_Caught = Yes / Total_Sites * 100   # Proportion of sites with larvae caught
  )

b_site_ano_sum_dry_wide_sett$Season <- "Dry"

##Wet Season
b_site_ano_sum_wet_sett <- lav_df_wet %>% 
  filter(!is.na(`Breeding_Site_Recode`)) %>% 
  group_by(`Settlement Type`, `Breeding_Site_Recode`, `Anopheles_Caught`, `Breeding_Site_Recode2`) %>%  # Group by breeding site type
  summarize(
    SitesVisited = n(),  # Number of sites visited per type
  )

##Convert data to wide to compute proportion of breeding site with larva
b_site_ano_sum_wet_wide_sett <- b_site_ano_sum_wet_sett %>% 
  pivot_wider(names_from = Anopheles_Caught, values_from = SitesVisited, values_fill = 0) %>%
  mutate(
    Total_Sites = No + Yes, 
    Proportion_Larvae_Caught = Yes / Total_Sites * 100   # Proportion of sites with larvae caught
  )

b_site_ano_sum_wet_wide_sett$Season <- "Wet"


##Combine Dry and Wet Season
b_site_ano_sum_all_wide_sett <- rbind(b_site_ano_sum_dry_wide_sett, b_site_ano_sum_wet_wide_sett)

b_site_ano_sum_all_wide_sett_y <- b_site_ano_sum_all_wide_sett %>% 
  dplyr::filter(Yes > 0, )


Fig7 <- ggplot(b_site_ano_sum_all_wide_sett_y, aes(x = Breeding_Site_Recode, y = Yes)) +
  geom_point(aes(color = `Settlement Type`, size = 4.5), , alpha = 0.7) +  
  facet_wrap(~ `Season`)+
  scale_color_manual(values = c(Formal = "#f57362", Slum = "#f9caa7"))+
  geom_text(aes(label = Yes), vjust = -1.2, hjust = 0.5, size = 3) + 
  geom_text(aes(label = Breeding_Site_Recode), vjust = -1.2, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(2, 10)) + 
  labs(title = "Number and type of Breeding Sites by settlement type ",
       y = "Number of Breeding sites",
       size = "Number of Breeding Sites Visited") +
  guides(size = FALSE)+
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Breeding sites with anopheles by settlement.pdf'), Fig7, width = 8, height = 6)


Fig8 <- ggplot(b_site_ano_sum_all_wide_sett_y, aes(x = Breeding_Site_Recode, y = Proportion_Larvae_Caught, size = Total_Sites)) +
  geom_point(aes(color = `Settlement Type`, size = Total_Sites), , alpha = 0.7) +  
  facet_wrap(~ `Season`)+
  scale_color_manual(values = c(Formal = "#f57362", Slum = "#f9caa7"))+
  geom_text(aes(label = Breeding_Site_Recode), vjust = -1.2, hjust = 0.5, size = 3) + 
  scale_size_continuous(range = c(2, 10),
                        breaks = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 75),     # Custom breaks for size legend
                        #labels = c("5 Sites", "10 Sites", "15 Sites", "20 Sites")  # Custom labels for the breaks
  ) + 
  labs(title = "Proportion of Anopheles Caught Across Breeding Sites",
       y = "Proportion of breeding site yielding Anopheles(%)",
       size = "Number of Breeding Sites Visited") +
  guides(
    size = guide_legend(
      title = "No of breeding sites visited",  # Customize the title of the size legend
      override.aes = list(shape = 21, fill = "#07c7f7"),  # Customize legend appearance
      keywidth = 1, keyheight = 1,   # Adjust size of the legend keys
      label.position = "bottom",     # Customize label positioning
      nrow = 3                       # Arrange legend in a single row
    )
  ) +
  theme_manuscript() 


ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Proportion of Breeding sites with anopheles by settlement1.pdf'), Fig8, width = 8, height = 6) 


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
  group_by(`Settlement Type`, `Breeding_Site_Recode`) %>% 
  summarize(
    AvgLD = mean(`Larva_Density`, na.rm = TRUE)  # Average number of Anopheles caught per site
  )
lav_den_sum_dry$season <- "Dry"

#Wet
lav_den_sum_wett <- lav_den_sum_wet %>% 
  group_by(`Settlement Type`, `Breeding_Site_Recode`) %>%  
  summarize(
    AvgLD = mean(`Larva_Density`, na.rm = TRUE)  # Average number of Anopheles caught per site
  )

lav_den_sum_wett$season <- "Wet"

##Combine Larval Density Data
lav_den_sum_all <- rbind(lav_den_sum_dry, lav_den_sum_wett)


Fig9 <- ggplot(lav_den_sum_all, aes(x = Breeding_Site_Recode, y = AvgLD)) +
  geom_point(aes(color = `Settlement Type`, size = 4.5), , alpha = 0.7) +  
  facet_wrap(~ `season`)+
  scale_color_manual(values = c(Formal = "#f57362", Slum = "#f9caa7"))+
  geom_text(aes(label = AvgLD), vjust = -1.2, hjust = 0.5, size = 3) + 
  geom_text(aes(label = Breeding_Site_Recode), vjust = -1.2, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(2, 10)) + 
  labs(title = "Average Larval Density per Breeding Sites by settlement type ",
       y = "Average Larval Density",
       size = "Average Larval Density") +
  guides(size = FALSE)+
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Larval Density of Breeding sites by settlement.pdf'), Fig9, width = 8, height = 6) 



#Converting Table 2 to figure
# ---------------------------
# 1️⃣ Prepare data
# ---------------------------

# Main pie chart: Wet vs Dry
main_data <- data.frame(
  Season = c("Wet", "Dry"),
  value = c(26, 5)  # number of habitats with larvae
)

# Inset pie charts: Temporary vs Permanent per season
inset_wet <- data.frame(
  Habitat = c("Temporary", "Permanent"),
  value = c(15, 11)
)

inset_dry <- data.frame(
  Habitat = c("Temporary", "Permanent"),
  value = c(1, 4)
)

# Add percentages for labeling
main_data <- main_data %>%
  mutate(label = paste0(Season, "\n", value, " (", round(value/sum(value)*100, 1), "%)"))

inset_wet <- inset_wet %>%
  mutate(label = paste0(Habitat, "\n", value, " (", round(value/sum(value)*100, 1), "%)"))

inset_dry <- inset_dry %>%
  mutate(label = paste0(Habitat, "\n", value, " (", round(value/sum(value)*100, 1), "%)"))

# ---------------------------
# 2️⃣ Create ggplot pies
# ---------------------------

# Main pie
main_pie <- ggplot(main_data, aes(x = "", y = value, fill = Season)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5) +
  theme_void() +
  scale_fill_brewer(palette = "Set2") +
  ggtitle("Habitats with Anopheles Larvae by Season and type of habitat")

# Insets
inset_wet_pie <- ggplot(inset_wet, aes(x = "", y = value, fill = Habitat)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  theme_void() +
  scale_fill_brewer(palette = "Pastel1") +
  ggtitle("Wet Season Breakdown")

inset_dry_pie <- ggplot(inset_dry, aes(x = "", y = value, fill = Habitat)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  theme_void() +
  scale_fill_brewer(palette = "Pastel2") +
  ggtitle("Dry Season Breakdown")

# ---------------------------
# 3️⃣ Combine with cowplot
# ---------------------------

table2_plot <- ggdraw() +
  draw_plot(main_pie, 0, 0, 1, 1) +            # Main pie full canvas
  draw_plot(inset_dry_pie, 0.05, 0.55, 0.3, 0.3) +  # Wet inset
  draw_plot(inset_wet_pie, 0.6, 0.05, 0.3, 0.3)     # Dry inset

ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/", ' Seasonal distribution of larval habitats positive for Anopheles larvae in Ibadan .pdf'), table2_plot, width = 11, height = 8)


#-------------------------------------------------------------------------------#
##------------------------------------------------------------------------------##
###-------------------Extra manuscript analysis---------------------------------###
####----------------------------------------------------------------------------####
lav_df_dry$season <- "Dry"

lav_df_wet$season <- "Wet"

lav_dfd <- lav_df_dry %>% 
  dplyr::select(`Settlement Type`, Anopheles_Caught,
                season, Breeding_Site_Recode, Breeding_Site_Recode2)

lav_dfw <- lav_df_wet %>% 
  dplyr::select(`Settlement Type`, Anopheles_Caught,
                season, Breeding_Site_Recode, Breeding_Site_Recode2)

lav_overall <- rbind(lav_dfd, lav_dfw)

write.csv(lav_overall, file.path(LuDir, "lav_dataset.csv"))

# Create contingency table(Breeding site vs Season)
table_chi <- table(lav_overall$Breeding_Site_Recode2, lav_overall$season)

# Run Chi-square test
chisq.test(table_chi)

# Create contingency table(Breeding site vs `Settlement Type`)
table_chi2 <- table(lav_overall$Breeding_Site_Recode2, lav_overall$`Settlement Type`)

# Run Chi-square test
chisq.test(table_chi2)

lav_overall %>%
  group_by(season) %>%
  group_split() %>%
  lapply(function(df){
    tbl <- table(df$Breeding_Site_Recode2, df$`Settlement Type`)
    chisq_result <- chisq.test(tbl)
    return(list(season = unique(df$season), result = chisq_result))
  })

###Anova Analysis
# Run one-way ANOVA for each season separately
lav_den_sum_all %>%
  group_by(season) %>%
  group_map(~ {
    cat("ANOVA results for season:", unique(.x$season), "\n")
    print(summary(aov(AvgLD ~ `Settlement Type`, data = .x)))
    cat("\n")
  })


#Boxplot
ld_bxp <- ggplot(lav_den_sum_all, aes(x = `Settlement Type`, y = AvgLD, fill = `Settlement Type`)) +
  geom_boxplot() +
  facet_wrap(~season) +
  scale_fill_manual(values = c(Formal = "#f57362", Slum = "#f9caa7"))+
  theme_manuscript() +
  labs(title = "Average Larval Density by Settlement Type and Season",
       y = "Average Larval Density",
       x = "Settlement Type")

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Box plot of larval densitities by settlement n season.pdf'), ld_bxp, width = 8, height = 6)

##Exploring breeding site type and anopheles presence
lav_overall %>%
  filter(Anopheles_Caught == "Yes") %>%
  group_by(season, Breeding_Site_Recode2) %>%
  summarise(Sites = n(), .groups = 'drop') %>%
  group_by(season) %>%
  mutate(Proportion = Sites / sum(Sites))# %>%
arrange(season, Breeding_Site_Recode2)


##Exploring total anopheles per season and settlement
##Some data wrangling before computation
lav_anop_sum_dry <-lav_den_sum %>%
  dplyr::select(`Settlement Type`, Anopheles, Breeding_Site_Recode, Breeding_Site_Recode2, Larva_Density,season)

lav_anop_sum_wet <-lav_den_sum_wet %>%
  dplyr::select(`Settlement Type`, `Number of Anopheles`, Breeding_Site_Recode, Breeding_Site_Recode2, Larva_Density,season)

colnames(lav_anop_sum_wet)[2] <- "Anopheles"


lav_anop_sum_all <- rbind(lav_anop_sum_dry, lav_anop_sum_wet)

##Estimating total anopheles
lav_anop_sum_all %>%
  summarise(Total_Anopheles = sum(Anopheles, na.rm = TRUE))

anoph_sum <- lav_anop_sum_all %>%
  group_by(season, `Settlement Type`, Breeding_Site_Recode) %>%
  summarise(Total_Anopheles = sum(Anopheles, na.rm = TRUE)) %>%
  arrange(season, `Settlement Type`, Breeding_Site_Recode)

#Redo summaries
anoph_sum <- lav_anop_sum_all %>%
  group_by(season, Breeding_Site_Recode) %>%
  summarise(Total_Anopheles = sum(Anopheles, na.rm = TRUE)) %>%
  arrange(season, Breeding_Site_Recode)


##Kruskal Wallis (due to small sample sizes)
#Function to run Kruskal-Wallis test by Season
run_kruskal_test <- function(season_name, data){
  season_data <- data %>% filter(season == season_name)
  
  if(length(unique(season_data$`Settlement Type`)) > 1){
    result <- kruskal.test(AvgLD ~ `Settlement Type`, data = season_data)
    return(result)
  } else {
    return("Not enough groups for Kruskal-Wallis Test.")
  }
}

# Run Kruskal-Wallis tests for both Dry and Wet seasons
kruskal_dry <- run_kruskal_test("Dry", lav_den_sum_all)
kruskal_wet <- run_kruskal_test("Wet", lav_den_sum_all)

# Print test results
cat("\nKruskal-Wallis Test for Dry Season:\n")
print(kruskal_dry)

cat("\nKruskal-Wallis Test for Wet Season:\n")
print(kruskal_wet)



# Summarize median larval density and sample size by Settlement Type and Season
summary_stats <- lav_den_sum_all %>%
  group_by(season, `Settlement Type`) %>%
  summarise(
    N = n(),
    Median_Larval_Density = median(AvgLD, na.rm = TRUE),
    Min = min(AvgLD, na.rm = TRUE),
    Max = max(AvgLD, na.rm = TRUE)
  ) %>%
  arrange(season, `Settlement Type`)

# Print summary table
cat("\nSummary of Median Larval Densities by Settlement Type and Season:\n")
print(summary_stats)

ggplot(lav_den_sum_all, aes(x = `Settlement Type`, y = AvgLD)) +
  geom_boxplot() +
  facet_wrap(~ season) +
  theme_minimal()

lav_den_sum_all %>% 
  group_by(season, `Settlement Type`) %>% 
  summarise(median_AvgLD = median(AvgLD, na.rm = TRUE),
            IQR = IQR(AvgLD, na.rm = TRUE),
            n = n())


##Prepare data for breeding site analysis
lav_bs_dry <- lav_df_dry %>% 
  dplyr::select(SN, `Settlement Type`, Latitude, Longitude, Anopheles_Caught, Breeding_Site_Recode) %>%
  rename(site_label = SN,
         latitude = Latitude,
         longitude = Longitude,
         anophw = Anopheles_Caught)

lav_bs_slum_dry <- lav_bs_dry %>% 
  dplyr::filter(`Settlement Type` == "Slum") %>% 
  filter(!`site_label` %in% c(17, 27, 40))

lav_bs_formal_dry <- lav_bs_dry %>% 
  dplyr::filter(`Settlement Type` == "Formal")%>% 
  filter(!`site_label` %in% c(22, 6, 9))


lav_bs_wet <- lav_df_wet %>% 
  dplyr::select(`Household Code/Number`, `Settlement Type`, `_Breeding site coordinates_latitude`,`_Breeding site coordinates_longitude`, Anopheles_Caught, Breeding_Site_Recode) %>% 
  rename(site_label = `Household Code/Number`,
         latitude =  `_Breeding site coordinates_latitude`,
         longitude = `_Breeding site coordinates_longitude`,
         anophw = Anopheles_Caught)

lav_bs_slum_wet <- lav_bs_wet %>% 
  dplyr::filter(`Settlement Type` == "Slum")

lav_bs_formal_wet <- lav_bs_wet %>% 
  dplyr::filter(`Settlement Type` == "Formal")%>% 
  filter(!`site_label` %in% c(272, 147, 98, 71))


##Boxplot of breeding site densities
den_summary_all <- rbind(Agudry_summary, Aguwet_summary, Chal_summary, Olop_summary)

# Convert to long format
den_summary_long <- den_summary_all %>%
  pivot_longer(
    cols = c(mean_density, sd_density, min_density, max_density),
    names_to = "Density_Metric",
    values_to = "Value"
  )

# View result
head(den_summary_long)


ggplot(den_summary_long, aes(x = settlment, y = Value, fill = settlment)) +
  geom_boxplot() +
  facet_wrap(~ season)+
  scale_fill_manual(values = c(Formal = "#f57362", Slum = "#f9caa7"))+
  labs(
    title = "Distribution of Breeding Site Densities by Ward",
    x = "Ward",
    y = "Breeding Site Density (sites/km²)"
  ) +
  theme_manuscript() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

##-----------------------------------------------------------------------------#
  ##More statistical tests for manuscript write up
##-----------------------------------------------------------------------------#  
  # Subset to only sites with Anopheles larvae present
  data_anopheles <- subset(lav_overall, Anopheles_Caught == "Yes")


# Check result
table(data_anopheles$Breeding_Site_Recode2, data_anopheles$`Settlement Type`)

--------------------------------------------------------------------------------
  ##Logistic Regression
# Set Outcome Variable: Breeding site type (Artificial = 1, Permanent = 0)
data_anopheles$Breeding_Site_Recode2 <- factor(data_anopheles$Breeding_Site_Recode2, levels = c("Permanent", "Artificial"))
data_anopheles$`Settlement Type` <- factor(data_anopheles$`Settlement Type`, levels = c("Formal", "Slum"))

# Run logistic regression
model <- glm(Breeding_Site_Recode2 ~ `Settlement Type`, family = binomial, data = data_anopheles)

# View results
summary(model)

# Odds ratios
exp(cbind(OR = coef(model), confint(model)))

#Include Season
# Fit the model including season
data_anopheles$season <- factor(data_anopheles$season, levels = c("Dry", "Wet"))
model <- glm(
  Breeding_Site_Recode2 ~ `Settlement Type` + season,
  family = binomial,
  data = data_anopheles
)

# Extract ORs and confidence intervals
forest_df <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  rename(
    Variable = term,
    OR = estimate,
    Lower = conf.low,
    Upper = conf.high
  )

# Clean up labels
forest_df <- forest_df %>%
  mutate(
    Variable = recode(Variable,
                      `(Intercept)` = "Intercept (Reference: Formal, Dry)",
                      `Settlement TypeSlum` = "Settlement: Slum vs Formal",
                      `seasonWet` = "Season: Wet vs Dry")
  )

# Remove intercept 
forest_plot_df <- forest_df %>%
  filter(Variable != "Intercept (Reference: Formal, Dry)")

# Make Plot
ggplot(forest_plot_df, aes(x = OR, y = reorder(Variable, OR))) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, color = "steelblue") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +  # reference line at OR = 1
  geom_text(aes(label = round(OR, 2)), 
            hjust = -0.3, vjust = 1.2, size = 3.5, color = "black") +  # add OR values
  scale_x_log10() +  # log scale for OR
  labs(
    title = "Forest Plot of Odds Ratios",
    x = "Odds Ratio (log scale)",
    y = ""
  ) +
  theme_manuscript() +
  theme(axis.text.y = element_text(size = 12))


--------------------------------------------------------------------------------
  # Create a 2x2 table to explore more relationships
table_overall <- table(data_anopheles$Breeding_Site_Recode2, data_anopheles$season)

# Run Chi-square test
chisq.test(table_overall)

table(data_anopheles$Breeding_Site_Recode2, data_anopheles$season)
fisher.test(table(data_anopheles$Breeding_Site_Recode2, data_anopheles$season))

table_overall <- matrix(c(4,11,1,15), nrow=2, byrow=TRUE)
rownames(table_overall) <- c("Permanent", "Artificial")
colnames(table_overall) <- c("Dry", "Wet")

fisher.test(table_overall)



-------------------------------------------------------------------------------
  ###New Analysis based on literature to strengthen manuscript
  -------------------------------------------------------------------------------
  ##Make Pareto Plot
  # Calculate Pareto data(Overall)
  pareto_data_all <- lav_den_sum_all %>% 
  #dplyr::filter(`Settlement Type` == "Formal") %>%
  arrange(desc(AvgLD), .by_group = TRUE) %>%    # sort within group
  mutate(
    CumSum = cumsum(AvgLD),
    Total = sum(AvgLD),
    CumPerc = CumSum / Total * 100
  ) %>%
  ungroup()

##Summarize Breeding sites in Pareto data
pareto_data_all <- pareto_data_all %>% 
  #dplyr::filter(`Settlement Type` == "Formal") %>%
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
pareto_data_all <- pareto_data_all %>%
  #group_by(`Settlement Type`) %>%
  arrange(desc(AvgLD)) %>%
  mutate(Breeding_Site_Recode = factor(Breeding_Site_Recode, levels = unique(Breeding_Site_Recode))) %>%
  ungroup()

# Plot

ggplot(pareto_data_all, aes(x = reorder(Breeding_Site_Recode, -AvgLD), y = AvgLD)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(y = CumPerc * max(AvgLD)/100, group = 1), color = "red", size = 1) +
  geom_point(aes(y = CumPerc * max(AvgLD)/100), color = "red", size = 2) +
  scale_y_continuous(
    name = "Average Larval Density",
    limits = c(0, max(pareto_data_all$AvgLD)),  # ensures main axis starts at 0
    sec.axis = sec_axis(~ . * 100 / max(pareto_data_all$AvgLD),
                        name = "Cumulative %", 
                        breaks = seq(0, 100, 20))  # secondary axis from 0 to 100
  ) +
  geom_hline(yintercept = 0.8 * max(pareto_data_all$AvgLD), 
             linetype = "dashed", color = "darkgreen", size = 1) +
  labs(x = "Breeding Site Type", 
       title = "Pareto Plot of Breeding Sites by Average Larval Density (Formal)") +
  theme_manuscript() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate Pareto data(Formal)
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
    Total = first(Total)      # keep the original Total
  ) %>% 
  arrange(desc(AvgLD), .by_group = TRUE) %>%    # sort within group
  mutate(
    CumSum = cumsum(AvgLD),
    Total = sum(AvgLD),
    CumPerc = CumSum / Total * 100
  ) %>%
  ungroup()

# ##Compute Av. Larval density
# #Dry
# lav_den_sum_dry <- lav_den_sum %>% 
#   group_by(`Settlement Type`, `Breeding_Site_Recode`) %>%  # Group by breeding site type
#   summarize(
#     AvgLD = mean(`Larva_Density`, na.rm = TRUE)  # Average number of Anopheles caught per site
#   )
# lav_den_sum_dry$season <- "Dry"
# 
# #Wet
# lav_den_sum_wett <- lav_den_sum_wet %>% 
#   group_by(`Settlement Type`, `Breeding_Site_Recode`) %>%  # Group by breeding site type
#   summarize(
#     AvgLD = mean(`Larva_Density`, na.rm = TRUE)  # Average number of Anopheles caught per site
#   )
# 
# lav_den_sum_wett$season <- "Wet"
# 
# ##Combine Larval Density Data
# lav_den_sum_all <- rbind(lav_den_sum_dry, lav_den_sum_wett)
# 
# pareto_data1 <- lav_den_sum_all %>%
#   arrange(desc(AvgLD), .by_group = TRUE) %>%    # sort within group
#   mutate(
#     CumSum = cumsum(AvgLD),
#     Total = sum(AvgLD),
#     CumPerc = CumSum / Total * 100
#   ) %>%
#   ungroup()

# Reorder globally by AvgLD
pareto_data_formal <- pareto_data_formal %>%
  group_by(`Settlement Type`) %>%
  arrange(desc(AvgLD)) %>%
  mutate(Breeding_Site_Recode = factor(Breeding_Site_Recode, levels = unique(Breeding_Site_Recode))) %>%
  ungroup()

# Plot

ggplot(pareto_data_formal, aes(x = reorder(Breeding_Site_Recode, -AvgLD), y = AvgLD)) +
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



# Calculate Pareto data(Slum)
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

# Plot

ggplot(pareto_data_slum, aes(x = reorder(Breeding_Site_Recode, -AvgLD), y = AvgLD)) +
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




  

##Read in wet season larval data to compute time difference
larva_wet <- read_xlsx("C:/Users/ebamgboye/Downloads/WET_SEASON_ENTO_COLLECTION_LARVAL_PROSPECTION_-_all_versions_-_labels_-_2025-06-17-20-42-08.xlsx")

# Example dataframe
df <- data.frame(
  start = c("2024-07-19T07:54:10.516+01:00", "2024-07-19T10:00:00.000+01:00"),
  end   = c("2024-07-19T13:20:09.785+01:00", "2024-07-19T14:30:00.000+01:00")
)

# Parse to datetime
larva_wet$start_parsed <- ymd_hms(larva_wet$start)
larva_wet$end_parsed   <- ymd_hms(larva_wet$end)

# Compute time difference (in minutes)
larva_wet$time_diff_mins <- as.numeric(difftime(larva_wet$end_parsed, larva_wet$start_parsed, units = "mins"))

# View result
print(larva_wet)

pos_lav_wet <- larva_wet %>% 
  dplyr::filter(`Number of Anopheles` > 0)


##Extra analysis
t.test(pH ~ Anopheles_Caught, data = lav_c_dffw)

View(lav_den_sum_wet)
cor.test(lav_den_sum_wet$`Temperature(Celcius)`, lav_den_sum_wet$Larva_Density, method = "spearman")

ggplot(lav_den_sum_wet, aes(x = `pH`, y = Larva_Density)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

cor.test(lav_den_sum_wet$`pH`, lav_den_sum_wet$Larva_Density, method = "spearman")

library(dplyr)
library(purrr)
library(broom)

results <- lav_den_sum_dry %>%
  group_by(`Settlement Type`) %>%
  group_split() %>%
  map_df(~ {
    test <- cor.test(.x$pH, .x$Larva_Density, method = "pearson")
    tibble(
      Settlement_Type = unique(.x$`Settlement Type`),
      cor_coef = test$estimate,
      p_value = test$p.value,
      n = nrow(.x)
    )
  })

results

library(ggplot2)

ggplot(lav_den_sum_wet, aes(x = pH, y = Larva_Density, color = `Settlement Type`)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between pH and Larva Density by Settlement Type",
    x = "pH of Breeding Site",
    y = "Larva Density (per site)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11),
    legend.position = "top"
  )


#Quick Analysis of pH, settlement type and season(Redo plot)

lav_a_ph <- lav_a_dffw %>% 
  dplyr::select(Settlement.Type, pH, Anopheles_Caught)%>% 
  mutate(season = "Wet")

lav_c_ph <- lav_c_dffw %>% 
  dplyr::select(Settlement.Type, pH, Anopheles_Caught) %>% 
  mutate(season = "Wet")


lav_ph_wetdf <- lav_df_wet %>% 
  dplyr::select(`Settlement Type`, pH, Anopheles_Caught) %>% 
  mutate(season = "Wet",
         pH = as.numeric(pH))

lav_ph_drydf <- lav_df_dry %>% 
  dplyr::select(`Settlement Type`, pH, Anopheles_Caught) %>% 
  mutate(season = "Dry")

lav_ph_all <- rbind(lav_ph_wetdf, lav_ph_drydf)

##Explore distribution with boxplot
ggplot(lav_ph_all, aes(x = season, y = pH)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.5, color = "darkblue") +
  labs(
    title = "Distribution of pH by Season",
    x = "Season",
    y = "pH"
  ) +
  theme_minimal()

##Compute statistics
stats_lavdf <- lav_ph_all %>% 
  group_by(season) %>% 
  summarise(
    mean_pH = mean(pH, na.rm = TRUE),
    sd_pH   = sd(pH, na.rm = TRUE),
    n       = sum(!is.na(pH))
  )


ggplot(lav_ph_all, aes(season, pH)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.5, color = "darkblue") +
  annotate("text",
           x = stats_df$season,
           y = max(lav_ph_all$pH, na.rm = TRUE) + 0.5,
           label = paste0(
             "Mean = ", round(stats_df$mean_pH, 2),
             "\nSD = ", round(stats_df$sd_pH, 2),
             "\nn = ", stats_df$n
           ),
           size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  theme_manuscript()


#Run T-test
t_results <- lav_ph_all %>%
  filter(`Settlement Type` != "Informal") %>%  
  filter(season == "Wet") %>% 
  group_by(season, `Settlement Type`) %>%
  do(tidy(t.test(pH ~ Anopheles_Caught, data = .)))

t_results


##Make plots with T-test values
library(dplyr)
library(broom)

# Filter out Informal if needed
subset_df <- lav_ph_all %>%
  filter(`Settlement Type` %in% c("Formal", "Slum")) %>% 
  filter(season == "Wet")

# Compute t-test for each Settlement Type × Season
t_test_annotations <- subset_df %>%
  group_by(season, `Settlement Type`) %>%
  
  group_map(~ {
    # Check if both Anopheles_Caught groups exist
    if(length(unique(.x$Anopheles_Caught)) == 2) {
      tidy(t.test(pH ~ Anopheles_Caught, data = .x))
    } else {
      tibble(estimate=NA, estimate1=NA, estimate2=NA,
             statistic=NA, p.value=NA, parameter=NA,
             conf.low=NA, conf.high=NA, method=NA, alternative=NA)
    }
  }) %>%
  bind_rows(.id = "group_id") %>%
  bind_cols(subset_df %>% group_by(season, `Settlement Type`) %>% summarise(), .)

# Prepare label text for plotting
t_test_annotations <- t_test_annotations %>%
  mutate(label = ifelse(!is.na(p.value), paste0("p = ", signif(p.value, 3)), ""))


library(ggplot2)

ggplot(subset_df, aes(x = Anopheles_Caught, y = pH, fill = Anopheles_Caught)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +   # hide default outliers to avoid overlap
  geom_jitter(width = 0.2, height = 0, alpha = 0.6, size = 2) +
  facet_grid(season ~ `Settlement Type`) +
  geom_text(data = t_test_annotations,
            aes(x = 1.5, y = max(subset_df$pH, na.rm = TRUE) + 0.3, label = label),
            inherit.aes = FALSE, size = 3) +
  labs(
    title = "pH by Anopheles Presence, Settlement Type, and Season",
    x = "Anopheles Caught",
    y = "pH"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("No" = "#F8766D", "Yes" = "#00BFC4"))

library(dplyr)
library(ggplot2)

# Summarize stats per group
summary_stats <- subset_df %>%
  group_by(Anopheles_Caught, `Settlement Type`, season) %>%
  summarise(
    mean_pH = mean(pH, na.rm = TRUE),
    min_pH = min(pH, na.rm = TRUE),
    max_pH = max(pH, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Create a single label for each box
  mutate(label = paste0("Mean: ", round(mean_pH, 2), 
                        "\nMin: ", round(min_pH, 2),
                        "\nMax: ", round(max_pH, 2)))

# Plot
ggplot(subset_df, aes(x = Anopheles_Caught, y = pH, fill = Anopheles_Caught)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, width = 0.6) +
  geom_jitter(width = 0.3, height = 0, alpha = 0.7, size = 3) +
  facet_grid(season ~ `Settlement Type`) +
  
  # Add the combined label above each box
  geom_text(data = summary_stats,
            aes(x = Anopheles_Caught, y = max_pH + 0.2, label = label),
            inherit.aes = FALSE, size = 3, lineheight = 0.9) +
  # Add t-test annotation if you have it
  geom_text(data = t_test_annotations,
            aes(x = 1.5, y = max(subset_df$pH, na.rm = TRUE) + 0.6, label = label),
            inherit.aes = FALSE, size = 3) +
  
  labs(
    title = "pH by Anopheles Presence, Settlement Type, and Season",
    x = "Anopheles Caught",
    y = "pH"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("No" = "#F8766D", "Yes" = "#00BFC4"))








# ##Forest plot of Breeding Site Type and Settlement
# library(dplyr)
# 
# # Count breeding sites by Settlement Type and Breeding_Site_Recode2
# site_counts <- lav_overall %>%
#   filter(`Settlement Type` %in% c("Formal", "Slum")) %>% 
#   group_by(`Settlement Type`, Breeding_Site_Recode2) %>%
#   summarise(n = n(), .groups = "drop")
# 
# site_counts
# 
# library(epitools)
# ct <- tidyr::pivot_wider(
#   site_counts,
#   names_from = Breeding_Site_Recode2,
#   values_from = n,
#   values_fill = 0
# )
# 
# ct_matrix <- as.matrix(ct[, -1])
# rownames(ct_matrix) <- ct$`Settlement Type`
# ct_matrix
# 
# 
# # Make a contingency table: rows = Settlement, cols = Breeding_Site type
# #ct <- table(lav_overall$`Settlement Type`, lav_overall$Breeding_Site_Recode2)
# 
# # Compute odds ratios using epitools
# or_result <- oddsratio(ct_matrix, rev = "columns")
# or_result$measure
# 
# 
# library(ggplot2)
# 
# # Convert OR results to a tibble
# or_df <- as.data.frame(or_result$measure)
# 
# or_df <- or_df %>%
#   mutate(
#     Settlement = rownames(or_df),
#     OR = estimate,
#     lower = lower,
#     upper = upper
#   )
# 
# 
# ggplot(or_df, aes(x = Settlement, y = OR, ymin = lower, ymax = upper)) +
#   geom_pointrange(color = "steelblue", size = 1) +
#   geom_hline(yintercept = 1, linetype = 2) +
#   coord_flip() +
#   ylab("Odds Ratio (Permanent vs Artificial)") +
#   xlab("Settlement Type") +
#   theme_minimal(base_size = 14)
# 
# 
# ##Adjusting for season
# # =====================================================
# # Forest Plot of Odds Ratios by Settlement Type, Stratified by Season
# # =====================================================
# 
# # Load required libraries
# library(dplyr)
# library(tidyr)
# library(epitools)
# library(ggplot2)
# 
# # -------------------------
# # 1️⃣ Prepare binary outcome
# # -------------------------
# # Permanent = 1, Artificial = 0
# lav_overall <- lav_overall %>%
#   mutate(PermanentSite = ifelse(Breeding_Site_Recode2 == "Permanent", 1, 0))
# 
# # -------------------------
# # 2️⃣ Compute ORs stratified by season
# # -------------------------
# # For each season, compute a 2x2 table and odds ratio
# library(dplyr)
# library(tidyr)
# library(epitools)
# 
# library(dplyr)
# library(epitools)
# 
# # Filter data first
# df_filtered <- lav_overall %>%
#   filter(`Settlement Type` %in% c("Formal", "Slum")) %>%
#   mutate(
#     `Settlement Type` = factor(`Settlement Type`, levels = c("Formal","Slum")),
#     Breeding_Site_Recode2 = factor(Breeding_Site_Recode2, levels = c("Artificial","Permanent"))
#   )
# 
# # Compute 2x2 tables per season
# season_tables <- df_filtered %>%
#   group_by(season) %>%
#   summarise(ct_matrix = list(table(`Settlement Type`, Breeding_Site_Recode2)), .groups = "drop")
# 
# # Compute odds ratios in a separate step
# OR_list <- lapply(season_tables$ct_matrix, function(tbl) {
#   oddsratio(tbl, rev = "columns")$measure
# })
# 
# # Combine results into a tibble
# season_ORs <- season_tables %>%
#   mutate(
#     OR_data = OR_list
#   ) %>%
#   tidyr::unnest_wider(OR_data)
# 
# season_ORs
# 
# #3Tidy up dataframe
# library(dplyr)
# library(tidyr)
# library(purrr)
# 
# season_ORs_tidy <- season_ORs %>%
#   mutate(
#     Formal_OR    = Formal[, 1],
#     Formal_lower = Formal[, 2],
#     Formal_upper = Formal[, 3],
#     
#     Slum_OR      = Slum[, 1],
#     Slum_lower   = Slum[, 2],
#     Slum_upper   = Slum[, 3]
#   ) %>%
#   select(
#     season,
#     Formal_OR, Formal_lower, Formal_upper,
#     Slum_OR, Slum_lower, Slum_upper
#   )
# 
# formal_ORs <- season_ORs %>%
#   select(season) %>%
#   mutate(
#     Settlement = "Formal",
#     OR = 1,
#     lower = NA,
#     upper = NA
#   )
# 
# season_ORs_tidy <- bind_rows(formal_ORs, season_ORs_tidy) %>%
#   arrange(season, Settlement)
# 
# ##Long format
# season_ORs_long <- season_ORs_tidy %>%
#   select(season, Formal_OR, Formal_lower, Formal_upper, Slum_OR, Slum_lower, Slum_upper) %>%
#   pivot_longer(
#     cols = -season,
#     names_to = c("Settlement", ".value"),
#     names_sep = "_"
#   )
# 
# 
# # -------------------------
# # 3️⃣ Create forest plot
# # -------------------------
# forest_plot <- library(ggplot2)
# 
# ggplot(season_ORs_long, 
#        aes(x = Settlement, y = OR, ymin = lower, ymax = upper, color = season)) +
#   geom_pointrange(size = 1.2, position = position_dodge(width = 0.5)) +
#   geom_hline(yintercept = 1, linetype = 2, color = "red") +
#   coord_flip() +
#   ylab("Odds Ratio (Permanent vs Artificial)") +
#   xlab("Settlement Type") +
#   theme_minimal(base_size = 14) +
#   geom_text(aes(label = sprintf("%.2f", OR)), 
#             hjust = -0.2, size = 4, position = position_dodge(width = 0.5)) +
#   scale_color_brewer(palette = "Set1") +
#   ggtitle("Odds of Permanent vs Artificial Breeding Sites by Settlement Type, Stratified by Season")
# 
# # Print plot
# print(forest_plot)
# 
# 
# ##Another trial
# library(dplyr)
# library(epitools)
# library(tidyr)
# library(ggplot2)
# 
# # 1️⃣ Prepare binary outcome
# lav_overall <- lav_overall %>%
#   mutate(PermanentSite = ifelse(Breeding_Site_Recode2 == "Permanent", 1, 0))
# 
# # 2️⃣ Compute ORs stratified by season
# seasons <- unique(lav_overall$season)
# 
# results_list <- lapply(seasons, function(s) {
#   
#   df_season <- lav_overall %>% filter(season == s)
#   
#   # Contingency table: Settlement x Breeding Site type
#   ct <- table(df_season$`Settlement Type`, df_season$Breeding_Site_Recode2)
#   
#   # Compute odds ratio
#   or_res <- oddsratio(ct, rev = "columns")
#   
#   # Convert to tidy data frame
#   or_df <- as.data.frame(or_res$measure)
#   or_df <- or_df %>%
#     mutate(
#       Settlement = rownames(or_df),
#       OR = `odds ratio with 95% C.I. estimate`,
#       lower = lower,
#       upper = upper,
#       season = s
#     )
#   return(or_df)
# })
# 
# # Combine all seasons
# season_ORs <- bind_rows(results_list)
# 
# # 3️⃣ Forest plot
# ggplot(season_ORs, aes(x = Settlement, y = OR, ymin = lower, ymax = upper, color = season)) +
#   geom_pointrange(size = 1.2, position = position_dodge(width = 0.5)) +
#   geom_hline(yintercept = 1, linetype = 2, color = "red") +
#   coord_flip() +
#   ylab("Odds Ratio (Permanent vs Artificial)") +
#   xlab("Settlement Type") +
#   theme_minimal(base_size = 14) +
#   geom_text(aes(label = sprintf("%.2f", OR)), 
#             hjust = -0.2, size = 4, position = position_dodge(width = 0.5)) +
#   scale_color_brewer(palette = "Set1") +
#   ggtitle("Odds of Permanent vs Artificial Breeding Sites by Settlement Type, Stratified by Season")
# 














#Retry Pareto plot
ggplot(pareto_data1, aes(x = Breeding_Site_Recode, y = AvgLD)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(y = CumPerc * max(AvgLD)/100, group = 1), color = "red", size = 1) +
  geom_point(aes(y = CumPerc * max(AvgLD)/100), color = "red", size = 2) +
  scale_y_continuous(
    name = "Average Larval Density",
    sec.axis = sec_axis(~ . * 100 / max(pareto_data1$AvgLD), name = "Cumulative %")
  ) +
  facet_wrap(~`Settlement Type`) +
  labs(x = "Breeding Site Type", title = "Pareto Plot of Breeding Sites by Average Larval Density") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##Rerun Larval Density summary
##Compute Av. Larval density
#Dry
lav_bden_sum_dry <- lav_den_sum %>% 
  group_by(`Breeding_Site_Recode`) %>%  # Group by breeding site type
  summarize(
    AvgLD = mean(`Larva_Density`, na.rm = TRUE)  # Average number of Anopheles caught per site
  )
lav_bden_sum_dry$season <- "Dry"

#Wet
lav_bden_sum_wett <- lav_den_sum_wet %>% 
  group_by(`Breeding_Site_Recode`) %>%  # Group by breeding site type
  summarize(
    AvgLD = mean(`Larva_Density`, na.rm = TRUE)  # Average number of Anopheles caught per site
  )

lav_bden_sum_wett$season <- "Wet"

##Combine Larval Density Data
lav_bden_sum_all <- rbind(lav_bden_sum_dry, lav_bden_sum_wett)

pareto_data2 <- lav_bden_sum_all %>%
  #group_by(Breeding_Site_Recode) %>% 
  arrange(desc(AvgLD), .by_group = TRUE) %>%    # sort within group
  mutate(
    CumSum = cumsum(AvgLD),
    Total = sum(AvgLD),
    CumPerc = CumSum / Total * 100
  ) %>%
  ungroup()

##Ensure only one breeding site category
pareto_data1 <- pareto_data1 %>%
  group_by(Breeding_Site_Recode) %>%
  summarise(
    AvgLD = mean(AvgLD),
    Total = first(Total)      # keep the original Total
  ) %>%
  arrange(desc(AvgLD)) %>%    # for Pareto order
  mutate(
    CumSum = cumsum(AvgLD),                    # cumulative sum of AvgLD
    CumPerc = CumSum / sum(AvgLD) * 100       # cumulative percentage
  ) %>%
  ungroup()

# Plot
ggplot(pareto_data2 %>% dplyr::filter(season == "Wet"), aes(x = reorder(Breeding_Site_Recode, -AvgLD), y = AvgLD)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(y = CumPerc * max(AvgLD)/100, group = 1), color = "red", size = 1) +
  geom_point(aes(y = CumPerc * max(AvgLD)/100), color = "red", size = 2) +
  scale_y_continuous(
    name = "Average Larval Density",
    sec.axis = sec_axis(~ . * 100 / max(pareto_data2$AvgLD), name = "Cumulative %")
  ) +
  facet_wrap(~Settlment Type)+
  labs(x = "Breeding Site Type", title = "Pareto Plot of Breeding Sites by Average Larval Density") +
  theme_manuscript() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






library(dplyr)
library(ggplot2)

# Collapse duplicates by averaging AvgLD and recalculating cumulative percentage per Settlement Type
pareto_data1_summarized <- pareto_data1 %>%
  group_by(`Settlement Type`, Breeding_Site_Recode) %>%
  summarise(
    AvgLD = mean(AvgLD),
    .groups = "drop"
  ) %>%
  group_by(`Settlement Type`) %>%
  arrange(desc(AvgLD)) %>%
  mutate(
    Breeding_Site_Recode = factor(Breeding_Site_Recode, levels = Breeding_Site_Recode),
    CumPerc = cumsum(AvgLD)/sum(AvgLD)*100
  ) %>%
  ungroup()

# Check
pareto_data1_summarized

ggplot(pareto_data1_summarized %>% dplyr::filter(`Settlement Type` == "Slum"), aes(x = Breeding_Site_Recode, y = AvgLD)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(y = CumPerc * max(AvgLD)/100, group = 1), color = "red", size = 1) +
  geom_point(aes(y = CumPerc * max(AvgLD)/100), color = "red", size = 2) +
  scale_y_continuous(
    name = "Average Larval Density",
    sec.axis = sec_axis(~ . * 100 / max(pareto_data1_summarized$AvgLD), name = "Cumulative %")
  ) +
  facet_wrap(~`Settlement Type`, scales = "free_x") +
  labs(x = "Breeding Site Type", title = "Pareto Plot of Breeding Sites by Average Larval Density") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(dplyr)
library(ggplot2)

# Aggregate to ensure one value per Breeding Site per Settlement Type
pareto_data2 <- pareto_data1 %>%
  group_by(`Settlement Type`, Breeding_Site_Recode) %>%
  summarise(
    AvgLD = mean(AvgLD),
    CumPerc = mean(CumPerc)  # or recalc after if needed
  ) %>%
  arrange(`Settlement Type`, desc(AvgLD)) %>%
  ungroup()

# Set factor levels per Settlement Type
pareto_data2 <- pareto_data2 %>%
  group_by(`Settlement Type`) %>%
  mutate(Breeding_Site_Recode = factor(Breeding_Site_Recode, levels = unique(Breeding_Site_Recode))) %>%
  ungroup()

# Plot
ggplot(pareto_data2, aes(x = reorder(Breeding_Site_Recode, -AvgLD), y = AvgLD)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(y = CumPerc * max(AvgLD)/100, group = 1), color = "red", size = 1) +
  geom_point(aes(y = CumPerc * max(AvgLD)/100), color = "red", size = 2) +
  facet_grid(~`Settlement Type`, scales = "free_x") +
  scale_y_continuous(
    name = "Average Larval Density",
    sec.axis = sec_axis(~ . * 100 / max(pareto_data2$AvgLD), name = "Cumulative %")
  ) +
  labs(x = "Breeding Site Type", title = "Pareto Plot of Breeding Sites by Average Larval Density") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




##Alluvial Plot of Breeding site dynamics
# Load libraries
library(ggplot2)
library(ggalluvial)
library(dplyr)

# Create the data
alluvial_data <- tribble(
  ~Settlement, ~Breeding_Site, ~Type, ~Count,
  # Formal
  "Formal", "Drainage/Gutter/Ditches", "Permanent", 9,
  "Formal", "Artificial Containers", "Artificial", 1,
  "Formal", "Tyre", "Artificial", 1,
  "Formal", "Open Drain/Puddles", "Artificial", 1,
  # Slum
  "Slum", "Open Drain/Puddles", "Artificial", 6,
  "Slum", "Drainage", "Permanent", 5,
  "Slum", "Artificial Container", "Artificial", 2,
  "Slum", "Dug well", "Artificial", 2,
  "Slum", "Canal", "Permanent", 1,
  "Slum", "Tyre", "Artificial", 1,
  "Slum", "Open Tank", "Artificial", 1,
  "Slum", "Tyre Track", "Artificial", 1
)

# Make sure data is alluvia-friendly
alluvial_data <- alluvial_data %>%
  mutate(across(c(Settlement, Breeding_Site, Type), as.factor))

# Plot
ggplot(alluvial_data,
       aes(axis1 = Settlement, axis2 = Breeding_Site, axis3 = Type, y = Count)) +
  geom_alluvium(aes(fill = Settlement), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey80", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Settlement", "Breeding Site", "Type"), expand = c(.05, .05)) +
  labs(title = "Alluvial Plot of Breeding Sites by Settlement Type",
       y = "Number of Sites") +
  theme_minimal()


# 1️⃣ Prepare label positions
stratum_labels <- alluvial_data %>%
  group_by(Settlement, Breeding_Site, Type) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  mutate(
    x1 = 1,  # Settlement axis
    x2 = 2,  # Breeding_Site axis
    x3 = 3   # Type axis
  )

ggplot(alluvial_data,
       aes(axis1 = Settlement, axis2 = Breeding_Site, axis3 = Type, y = Count)) +
  geom_alluvium(aes(fill = Settlement), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey80", color = "black") +
  geom_text(data = stratum_labels,
            aes(x = x1, y = Count/2, label = Count),
            size = 3)+ 
  scale_x_discrete(limits = c("Settlement", "Breeding_Site", "Type"), expand = c(.05, .05)) +
  labs(title = "Alluvial Plot of Breeding Sites by Settlement Type",
       y = "Number of Sites") +
  theme_minimal()



library(dplyr)
library(ggalluvial)
library(ggplot2)

# 1️⃣ Prepare label positions
stratum_labels <- alluvial_data %>%
  group_by(Settlement, Breeding_Site, Type) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  mutate(
    x1 = 1,  # Settlement axis
    x2 = 2,  # Breeding_Site axis
    x3 = 3   # Type axis
  )

# 2️⃣ Plot
ggplot(alluvial_data,
       aes(axis1 = Settlement, axis2 = Breeding_Site, axis3 = Type, y = Count)) +
  geom_alluvium(aes(fill = Settlement), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey80", color = "black") +
  geom_text(data = stratum_labels,
            aes(x = x1, y = Count/2, label = Count),
            size = 3) +
  geom_text(data = stratum_labels,
            aes(x = x2, y = Count/2, label = Count),
            size = 3) +
  geom_text(data = stratum_labels,
            aes(x = x3, y = Count/2, label = Count),
            size = 3) +
  scale_x_discrete(limits = c("Settlement", "Breeding_Site", "Type"), expand = c(.05, .05)) +
  labs(title = "Alluvial Plot of Breeding Sites by Settlement Type",
       y = "Number of Sites") +
  theme_minimal()




library(ggalluvial)
library(ggplot2)
library(dplyr)

# Example: custom colors
settlement_colors <- c("Formal" = "#1f78b4", "Slum" = "#33a02c")
stratum_colors <- c(
  "Formal" = "#a6cee3", "Slum" = "#b2df8a",
  "Drainage/Gutter/Ditches" = "#fb9a99",
  "Artificial Containers" = "#fdbf6f",
  "Tyre" = "#ff7f00",
  "Open Drain/Puddles" = "#cab2d6",
  "Open Drain/Puddles" = "#cab2d6",
  "Drainage" = "#ffff99",
  "Dug well" = "#6a3d9a",
  "Canal" = "#b15928",
  "Open Tank" = "#e31a1c",
  "Tyre Track" = "#1f78b4"
)

# Optional: prepare labels positions
stratum_labels <- alluvial_data %>%
  mutate(
    x1 = 1, x2 = 2, x3 = 3,
    y1 = Count / 2, y2 = Count / 2, y3 = Count / 2
  )

ggplot(alluvial_data,
       aes(axis1 = Settlement, axis2 = Breeding_Site, axis3 = Type, y = Count)) +
  # Flows
  geom_alluvium(aes(fill = Settlement), width = 1/12, alpha = 0.8) +
  # Strata (axes)
  geom_stratum(aes(fill = Settlement), width = 1/12, color = "black") +
  # # Labels on strata
  geom_text(data = stratum_labels, aes(x = x1, y = y1, label = Count), size = 3) +
  geom_text(data = stratum_labels, aes(x = x2, y = y2, label = Count), size = 3) +
  geom_text(data = stratum_labels, aes(x = x3, y = y3, label = Count), size = 3) +
  # Customize x-axis
  scale_x_discrete(limits = c("Settlement", "Breeding_Site", "Type"), expand = c(.05, .05)) +
  # Custom colors for flows
  scale_fill_manual(values = stratum_colors) +
  labs(title = "Alluvial Plot of Breeding Sites by Settlement Type",
       y = "Number of Sites") +
  theme_minimal()


library(tidyverse)

# Create the data frame
df <- tibble(
  Settlement = c("Formal", "Formal", "Slum", "Slum"),
  Season  = c("Wet", "Dry", "Wet", "Dry"),
  Count      = c(9, 3, 17, 2)
)

# Stacked bar plot
ggplot(df, aes(x = Settlement, y = Count, fill = Season)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5), color = "white", size = 4) +
  labs(title = "Distribution of Breeding Sites by Settlement and Condition",
       x = "Settlement",
       y = "Number of Sites") +
  scale_fill_manual(values = c("Wet" = "#1f78b4", "Dry" = "sienna")) +
  theme_manuscript()
