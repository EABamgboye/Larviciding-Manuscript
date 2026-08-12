# Operational Insights for Larval Source Management Programs: A case study of Anopheles Breeding Habitat Dynamics across Urban Wards in Ibadan, Nigeria


# Overview
This repository contains the code and documentation associated with the paper titled "Operational Insights for Larval Source Management Programs: A case study of Anopheles Breeding Habitat Dynamics across Urban Wards in Ibadan, Nigeria". The study was conducted as part of a larger parent project that sought to understand the burden of malaria at the smallest administrative unit in Nigeria—the ward level.

The parent study comprised both epidemiological and entomological components. The epidemiological component involved household surveys among residents of the study wards, survey of pregnant women attending antenatal care at selected health facilities, and longitudinal follow-up of children aged 0–10 years. Participants were interviewed and tested for malaria using rapid diagnostic tests (RDTs).

The entomological component included adult mosquito collection using indoor and outdoor CDC light traps, pyrethrum spray catches, and larval habitat prospection. Where data permitted, key entomological indices—including the Human Biting Rate (HBR), Entomological Inoculation Rate (EIR), and sporozoite rates—were calculated, however not used explicitly in this paper.

The overarching goal of the parent study was to generate evidence for the Nigerian National Malaria Elimination Programme (NMEP) to support the design and tailoring of interventions for the 2026 Urban Malaria Control Strategy.

# Background
The present study analyzed the entomological data from selected wards in Ibadan metropolis to characterize Anopheles larval habitats, assess seasonal and ward-level variation, evaluate household spatial malaria risk, and identify environmental drivers of Anopheles larva habitat suitability

# Methodology
Potential breeding habitats were surveyed across three wards representing formal, informal, and slum settlement archetypes. Habitats were assessed for Anopheles larval presence, and larvae were collected and identified using standard methods. Breeding site density per km² was estimated using a simulated pathway technique. Spatial associations with household malaria risk were evaluated using kernel - based distance -decay weighting, while environmental drivers were modeled using MaxEnt.

# Repository Structure
## /scripts: contains code in four major folders 
### 1) General Analysis Scripts
  This folder contains data wrangling scripts
### 2) Manuscript Specific Scripts 
This folder contains data analysis to produce data used for the final analysis including
* Exploratory Data Analysis/Descriptive 
* Habitat Suitability 
* Sensitivity Analysis 

### 3) Breeding site density analysis scripts 
This folder contains data analysis to estimate the breeding site density using the simulated pathway approach where the total distance covered during larval prospection was estimated using the Vincenty's formula and the density was computed based on the number of Anopheles mosquito while accounting for Effective Strip Width. The folder contains scripts relating to the various locations(Agugu, Olopomewa and Challenge) in the dry and wet season.

### 4) Distance decay analysis scripts 
This folder contains the scripts relating to the estimation of the spatial relationship between malaria household positivity and the proximity to a larval habitat (assuming all identified larval habitats has the potential of habouring Anopheles). 

## /figures: 
This folder contains all main and supplementary figures generated from this analysis, provided in PDF and Adobe Illustrator (AI) file formats.

# Key Findings
* Only 7.4%(31/420) of the prospected larval habitats contained Anopheles larvae. 
* Larvae were detected mainly during the wet season (83.9%).
* The slum settlement predominated ward had the highest proportion of positive Anopheles larval habitats(61.3%), larval density, breeding site density, and habitat diversity
* Puddles, dug wells, and drainages/gutters accounted for about 80% of larval abundance.
* Modeled mosquito dispersal scale showed best fit at 30–32m in Challenge (OR 1.41, 95% CI: 1.05–1.89) during the wet season and 16–18m in Agugu (OR 1.29, 95% CI: 1.04–1.60) during the dry season.
* Habitat suitability in Agugu was higher farther from large water bodies and in areas with higher population density and positive Normalized Difference Water Index values. In Challenge, suitability was higher in areas with lower nighttime light levels, positive Normalized Difference Water Index values, and negative Normalized Difference Moisture Index values. 

# Recommendation
Further studies incorporating multiple wards across diverse urban settings are needed to determine whether differences in larval ecology between settlement archetypes provide a reliable basis for planning larval source management 


# Contact
For any inquiries or contributions, please contact: Ifeoma Ozodiegwu, Assistant Professor @ Loyola University and Principal Investigator at the Urban Malaria Project via iozodiegwu@luc.edu and Eniola Bamgboye, Post dotoral Research Fellow @ Urban Malaria Laboratory, Loyola University Chicago

# Acknowledgments
Special thanks to the Nigerian National Elimination Progeamme(NMEP), Osun State University, and all field researchers and collaborators
