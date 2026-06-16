# Operational Insights for Larval Source Management Programs: A case study of Anopheles Breeding Habitat Dynamics across Urban Wards in Ibadan, Nigeria


# Overview
This repository contains the code and documentation associated with the paper titled above. The study was conducted as part of a larger parent project that sought to understand the burden of malaria at the smallest administrative unit in Nigeria—the ward level.

The parent study comprised both epidemiological and entomological components. The epidemiological component involved household surveys among residents of the study wards, surveys of pregnant women recruited from health facilities, and longitudinal follow-up of children aged 0–10 years. Participants were interviewed and tested for malaria using rapid diagnostic tests (RDTs).

The entomological component included adult mosquito collection using indoor and outdoor CDC light traps, pyrethrum spray catches, and larval habitat prospection. Where data permitted, key entomological indices—including the Human Biting Rate (HBR), Entomological Inoculation Rate (EIR), and sporozoite rates—were calculated.

The overarching goal of the parent study was to generate evidence for the Nigerian National Malaria Elimination Programme (NMEP) to support the design and tailoring of interventions for the 2026 Urban Malaria Control Strategy.

# Background
The present study analyzed the entomological data from selected wards in Ibadan metropolis to characterize Anopheles larval habitats, assess seasonal and ward-level variation, evaluate household spatial malaria risk, and identify environmental drivers of Anophles larva habitat suitability

# Methodology
Potential breeding habitats were surveyed across three wards representing formal, informal, and slum settlement archetypes. Habitats were assessed for Anopheles larval presence, and larvae were collected and identified using standard methods. Breeding site density per km² was estimated using a simulated pathway technique. Spatial associations with household malaria risk were evaluated using kernel - based distance -decay weighting, while environmental drivers were modeled using MaxEnt.

# Repository Structure
## /scripts: contains code in two major folders 
### 1) General Analysis scripts 
  This folder contains data wrangling scripts
### 2) Manuscript Specific Scripts 
This folder contains data analysis to produce all the figures and results presented in the manuscript in four different domains
* Exploratory Data Analysis/Descriptive : 
* Household Spatial Scale Analysis
* Habitat Suitability 
* Sensitivity Analysis 

## /figures: 
This folder contains all main and supplementary figures generated from this analysis, provided in PDF and Adobe Illustrator (AI) file formats.

# Key Findings
* Only 7.4%(31/420) of the prospected larval habitats contained Anopheles larvae. 
* Larvae were detected mainly during the wet season (83.9%).
* The slum settlement predominated ward had the highest proportion of positive anopheles larval habitats(61.3%), larval density, breeding site density, and habitat diversity
* Puddles, dug wells, and drainages/gutters accounted for about 80% of larval abundance.
* Households nearest larval habitats had higher malaria odds in both seasons. Odds declined with distance in the informal -dominated ward but remained elevated in the slum -dominated ward, especially during the dry season (OR: 1.3; 95% CI: 1.1- 1.6).
* Habitat suitability increased with distance from large water bodies, areas with higher population density and positive NDWI values in the slum-settlement predominated ward, while areas with lower night -time lights and negatove NDMI values were most suitable for Anophelse larval breeding in the informal-settlement predominated ward.
  
# Impact
Targeted larval source management can strengthen urban malaria control in urban slums by combining environmental management and larviciding interventions while larviciding-focused interventions might be best in other settlement types (formal or informal). Prioritizing larval source management interventions during the dry season, when breeding habitats are fewer and easier to identify, could enhance cost-effectiveness and optimize resource allocation.

# Contact
For any inquiries or contributions, please contact: Ifeoma Ozodiegwu, Assistant Professor @ Loyola University and Principal Investigator at the Urban Malaria Project via iozodiegwu@luc.edu

# Acknowledgments
Special thanks to the NMEP, Osun State University, and all field researchers
