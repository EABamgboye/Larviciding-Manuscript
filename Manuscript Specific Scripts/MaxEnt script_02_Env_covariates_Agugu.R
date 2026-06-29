library(raster)
library(terra)
library(dismo)
library(terra)
library(sp)
library(caret)
library(stringr)
library(sf)
        
source("functions.R")


# --- Load rasters by type ---

# EVI files
evi_dir <- "C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/HLS30m/EVI/Ibadan"

evi_dir <- "C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/HLS30m/EVI/Ibadan"

# List all EVI files
evi_files <- list.files(evi_dir, pattern = "\\.tif$", full.names = TRUE)

# Extract month and year from filename
evi_info <- data.frame(
  file = evi_files,
  year = as.numeric(str_extract(evi_files, "(?<=month\\d{2}_)\\d{4}")),
  month = as.numeric(str_extract(evi_files, "(?<=month)\\d{2}(?=_)"))
)

# Filter to Months of interest -(Nov-Dec; 2022 & Jan-Feb 2023(Dry) and May–Aug 2024(Wet)
evi_study <- evi_info %>% 
  dplyr::filter(
        (year == 2024 & month %in% 5:7)| ##Wet
        (year == 2022 & month %in% 11:12) | ##Dry 
        (year == 2023 & month %in% 1:2) ## Dry
    )

# Stack the filtered rasters
evi_stack <- stack(evi_study$file)

# build names from year and month
evi_names <- sprintf("EVI_%d_%02d", evi_study$year, evi_study$month)

names(evi_stack) <- evi_names

# Check
print(evi_stack)
plot(evi_stack)


# # NDVI files
# ndvi_files <- list.files(pattern = "^NDVI_.*\\.tif$", full.names = TRUE)
# ndvi_stack <- stack(ndvi_files)

# NDWI files
ndWi_dir <- ("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/field_study_ndwi_30m")

ndWi_dir<- ("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/field_study_ndwi_30m")

# List all NDWI files in that directory
ndWi_files <- list.files(ndWi_dir, pattern = "\\.tif$", full.names = TRUE)

# Extract month and year from filename (adjust regex based on your naming convention)
ndWi_info <- data.frame(
  file = ndWi_files,
  year  = as.numeric(sapply(strsplit(basename(ndWi_files), "_"), `[`, 5)),  # 5th element is year
  month = as.numeric(sub("\\.tif$", "", sapply(strsplit(basename(ndWi_files), "_"), `[`, 6)))  # 6th element is month
)

# Filter to the months of interest (Dry and Wet season months)
ndWi_study <- ndWi_info %>% 
dplyr::filter(
  (year == 2024 & month %in% 5:7)| ##Wet
    (year == 2022 & month %in% 11:12) | ##Dry 
    (year == 2023 & month %in% 1:2) ## Dry
)

# Stack only the filtered rasters
ndWi_stack <- stack(ndWi_study$file)

# build names from year and month
ndWi_names <- sprintf("NDWI_%d_%02d", ndWi_study$year, ndWi_study$month)

names(ndWi_stack) <- ndWi_names

# Check
print(ndWi_stack)
plot(ndWi_stack)


# Directory where NDMI rasters are stored
ndmi_dir <- ("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/field_study_ndmi_30m")

ndmi_dir <- ("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/field_study_ndmi_30m")


# List all NDMI files in that directory
ndmi_files <- list.files(ndmi_dir, pattern = "\\.tif$", full.names = TRUE)

# Extract month and year from filename (adjust regex based on your naming convention)
ndmi_info <- data.frame(
  file = ndmi_files,
  year  = as.numeric(sapply(strsplit(basename(ndmi_files), "_"), `[`, 5)),  # 5th element is year
  month = as.numeric(sub("\\.tif$", "", sapply(strsplit(basename(ndmi_files), "_"), `[`, 6)))  # 6th element is month
)



# Filter to the months of interest (Dr and Wet Season)
ndmi_study <- ndmi_info %>% 
  dplyr::filter(
    (year == 2024 & month %in% 5:7)| ##Wet
    (year == 2022 & month %in% 11:12) | ##Dry 
    (year == 2023 & month %in% 1:2) ## Dry
)

# Stack only the filtered rasters
ndmi_stack <- stack(ndmi_study$file)

# build names from year and month
ndmi_names <- sprintf("NDMI_%d_%02d", ndmi_study$year, ndmi_study$month)

names(ndmi_stack) <- ndmi_names


# Check
print(ndmi_stack)
plot(ndmi_stack)


#------------------------------------------------------------------------------
#Night time Lights
#------------------------------------------------------------------------------

ntl_rast <- rast("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/night_timel_lights/VIIRS_NTL_2024_Nigeria.tif")

#ntl_rast21 <- rast("C:/Users/DELL/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/night_timel_lights/VIIRS_NTL_Nigeria_2021.tif")

plot(ntl_rast)

##convert raster
# ntl_stack_terra <- rast(evi_stack)
# crs(evi_stack_terra) <- "EPSG:32631"

# Crop raster to polygon extent
ntl_crop <- crop(ntl_rast, df_ib_a)

plot(ntl_crop)
ag_vect <- vect(df_ib_a)  # convert sf → SpatVector
plot(ag_vect, add = TRUE, border = "red", lwd = 2)


#--------------------------------------------------------------------------
##Soil Wetness
#--------------------------------------------------------------------------
# soil_moist <- rast("C:/Users/ebamgboye/Downloads/GIOVANNI-g4.timeAvgOverlayMap.GLDAS_NOAH10_M_2_1_SoilMoi100_200cm_inst.20240501-20250930.2E_3N_15E_15N.tif")
# soil_moist_crop <- crop(soil_moist, df_ib_a)
# plot(soil_moist_crop)
# plot(ag_vect, add = TRUE, border = "red", lwd = 2)

##Population density
#popn_den <- rast("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/Population/NGA_pop_density/gpw_v4_population_density_rev11_2020_1_deg.tif") 

popn_den <- rast("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/Population/NGA_pop_density/gpw_v4_population_density_rev11_2020_1_deg.tif") 

# Crop raster to polygon extent
popn_den_crop <- crop(popn_den, df_ib_a)
plot(popn_den_crop)

ag_vect <- vect(df_ib_a)  # convert sf → SpatVector
popn_den_crop <- crop(popn_den, ag_vect)
popn_den_mask <- mask(popn_den_crop, ag_vect)

ag_vect_buf <- buffer(ag_vect, width = 0.05)  # ~5 km

ag_ext <- ext(ag_vect)
ag_ext <- extend(ag_ext, 2 * res(popn_den)[1])  # add 2 raster cells

popn_den_crop <- crop(popn_den, ag_ext)
popn_den_mask <- mask(popn_den_crop, ag_vect)


plot(popn_den_mask)
plot(ag_vect, add = TRUE, border = "red", lwd = 2)
plot(ag_vect, add = TRUE, border = "red", lwd = 2)

#------------------------------------------------------------------------------
##Distance to Water Bodies
#------------------------------------------------------------------------------
#dwb <- rast("C:/Users/ebamgboye/Downloads/distance2water_30arcsec.tif")

dwb <- rast("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/distance_to_water_bodies/distance2water_30arcsec.tif") 

# Crop raster to polygon extent
dwb_crop <- crop(dwb, df_ib_a)
plot(dwb_crop)
ag_vect <- vect(df_ib_a)  # convert sf → SpatVector
plot(ag_vect, add = TRUE, border = "red", lwd = 2)


#LST
#agugu_lst <- rast("C:/Users/ebamgboye/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/agugu_lstraster.tif")

agugu_lst <- rast("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/agugu_lstraster.tif")

plot(agugu_lst)
plot(ag_vect, add = TRUE, border = "red", lwd = 2)


##------------------------------------------------------------------------------
#Land Use
##------------------------------------------------------------------------------
# hq_rast <- rast("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/housing/2019_Nature_Africa_Housing_2015_NGA.tiff")
# 
# # Crop raster to polygon extent
# hq_crop <- crop(hq_rast, df_ib_a)
# plot(hq_crop)
# ag_vect <- vect(df_ib_a)  # convert sf → SpatVector
# plot(ag_vect, add = TRUE, border = "red", lwd = 2)

#bfp <- st_read("C:/Users/ebamgboye/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/building_footprints/nigeria_footprints/nigeria blocks 2/Nigeria_Blocks_V1.shp")

bfp <- st_read("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/building_footprints/nigeria_footprints/nigeria blocks 2/Nigeria_Blocks_V1.shp")

bfp_oyo <- bfp %>% 
  dplyr::filter(state == "Oyo")

bfp_oyo <- sf::st_zm(bfp_oyo, drop = TRUE, what = "ZM")

# Check validity
sum(!st_is_valid(bfp_oyo))

# Fix invalid geometries
bfp_oyo <- st_make_valid(bfp_oyo)

ggplot(bfp_oyo) +
  geom_sf(aes(fill = landuse), color = NA) +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal() +
  labs(title = "Landuse in Oyo State", fill = "Land Use")

##Filter to Agugu
st_crs(bfp_oyo) <- st_crs(df_ib_a)

agu_bfp1 <- st_intersection(bfp_oyo, st_union(df_ib_a))

ggplot(agu_bfp1) +
  geom_sf(aes(fill = landuse), color = NA) +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal() +
  labs(title = "Landuse in Agugu", fill = "Land Use")



##Building Morphology
agu_bfp <- read.csv("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/building_morphology/challenge_agugu/cbound_vectors/Agugu/Agugu_cbound_with_coords.csv")

##Create raster stack
library(sp)
library(raster)

# 1. Fix EVI CRS once (metre extent → UTM 31N)
crs(evi_stack) <- CRS("+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs")
tmpl <- evi_stack[[1]]

# 2. Points in lon/lat
agu_pts_ll <- SpatialPointsDataFrame(
  coords = agu_bfp[, c("lon", "lat")],
  data   = agu_bfp,
  proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs")
)

# 3. Reproject to UTM (same CRS as tmpl)
agu_pts_utm <- spTransform(agu_pts_ll, crs(tmpl))

# 4. Rasterise on tmpl
pred_cols <- c("nndist_mean","area_mean","perimeter_mean",
               "compact_mean","angle_mean","shape_mean")

r_list <- lapply(pred_cols, function(v) {
  r <- rasterize(agu_pts_utm, tmpl, field = v, fun = mean)
  names(r) <- v
  r
})

agu_bf_stack <- stack(r_list)

# 5. Now extents do overlap; compute ward extent in UTM
ext_ward <- extent(agu_pts_utm)

plot(agu_bf_stack[[2]], xlim = c(ext_ward@xmin, ext_ward@xmax),
     ylim = c(ext_ward@ymin, ext_ward@ymax))

points(agu_pts_utm, add = TRUE, pch = 20, col = "red")

plot(agu_bf_stack, xlim = c(ext_ward@xmin, ext_ward@xmax),
     ylim = c(ext_ward@ymin, ext_ward@ymax))

# Convert to terra
agubmph_terra <- rast(agu_bf_stack)

# # Ensure evi_stack_crop is also terra
# evi_terra <- rast(evi_stack_crop)

#-------------------------------------------------------------------------------
##Make sure all rasters are aligned and stack
##------------------------------------------------------------------------------
#EVI
# Convert RasterStack to SpatRaster
evi_stack_terra <- rast(evi_stack)

crs(evi_stack_terra) <- "EPSG:32631"

#Make sure polygon is in the same CRS
df_ib_a_proj <- st_transform(df_ib_a, crs(evi_stack_terra))
df_ib_a_geom <- df_ib_a_proj["geometry"]
ward_vect <- vect(df_ib_a_geom)

ward_vect_proj <- terra::project(ward_vect, evi_stack_terra)
evi_stack_crop <- terra::crop(evi_stack_terra, ward_vect_proj)
evi_stack_mask <- terra::mask(evi_stack_crop, ward_vect_proj)

# Now crop & mask
evi_stack_crop <- crop(evi_stack_terra, ward_vect_proj)
evi_stack_mask <- mask(evi_stack_crop, ward_vect_proj)

# Plot
plot(evi_stack_mask[[1]])
plot(ward_vect_proj, add=TRUE)

#--------------------------------------------------------------------------------
#NDWI
# Convert RasterStack to SpatRaster
ndWi_stack_terra <- rast(ndWi_stack)

# Make sure polygon is in the same CRS
df_ib_a_projWi <- st_transform(df_ib_a, crs(ndWi_stack_terra))
df_ib_a_geomWi <- df_ib_a_projWi["geometry"]
ward_vect_Wi <- vect(df_ib_a_geomWi)

# Now crop & mask
ndWi_stack_crop <- crop(ndWi_stack_terra, ward_vect_Wi)
ndWi_stack_mask <- mask(ndWi_stack_terra, ward_vect_Wi)

# Plot
plot(ndWi_stack_crop[[1]])
plot(ward_vect_Wi, add=TRUE)

#-----------------------------------------------------------
#NDMI
# Convert RasterStack to SpatRaster
ndmi_stack_terra <- rast(ndmi_stack)

# Make sure polygon is in the same CRS
df_ib_a_projmi <- st_transform(df_ib_a, crs(ndmi_stack_terra))
df_ib_a_geommi <- df_ib_a_projmi["geometry"]
ward_vect_mi <- vect(df_ib_a_geommi)

# Now crop & mask
ndmi_stack_crop <- crop(ndmi_stack_terra, ward_vect_mi)
ndmi_stack_mask <- mask(ndmi_stack_terra, ward_vect_mi)

# Plot
plot(ndmi_stack_crop[[1]])
plot(ward_vect_mi, add=TRUE)

##Landuse continued
###Convert to land use to raster to ensure ease of stacking
# Convert sf → SpatVector
agu_vect <- vect(agu_bfp1)

# Encode landuse categories as numeric codes
agu_vect$landuse_code <- as.numeric(as.factor(agu_vect$landuse))

# Use one of your predictor rasters as template (ensures same extent/res)
templatea <- evi_stack_crop[[1]]  # first layer of your EVI stack

# Make sure agu_vect is in the same CRS as your template
agu_vect_utm <- project(agu_vect, crs(templatea))

# Crop template to chal_vect extent
template_cropa <- crop(templatea, ext(agu_vect_utm))

# Rasterize landuse using numeric codes
landusea_raster <- rasterize(agu_vect_utm, templatea, field = "landuse_code")

plot(landusea_raster)



##Stack Environmental Covariates

# 1. Make sure all rasters have the same CRS
ndWi_stack_crop <- project(ndWi_stack_crop, crs(evi_stack_crop))
ndmi_stack_crop <- project(ndmi_stack_crop, crs(evi_stack_crop))
lst_stack_crop  <- project(agugu_lst,  crs(evi_stack_crop))
ntl_stack_crop  <- project(ntl_crop,  crs(evi_stack_crop))
popn_den_crop  <-  project(popn_den_crop,  crs(evi_stack_crop))
dwb_crop       <-  project(dwb_crop,  crs(evi_stack_crop))
landusea_stack_crop  <- project(landusea_raster,  crs(evi_stack_crop))
bmph_stack_crop  <- project(agubmph_terra,  crs(evi_stack_crop))


#maxtemp_stack_crop  <- project(maxtemp_stack_crop,  crs(evi_stack_crop))

# 2. Align extents and resolution
ndWi_stack_res <- resample(ndWi_stack_crop, evi_stack_crop, method="bilinear")
ndmi_stack_res <- resample(ndmi_stack_crop, evi_stack_crop, method="bilinear")
lst_stack_res  <- resample(lst_stack_crop,  evi_stack_crop, method="bilinear")
ntl_stack_res  <- resample(ntl_stack_crop,  evi_stack_crop, method="bilinear")
popn_den_res  <- resample(popn_den_crop,  evi_stack_crop, method="bilinear")
dwb_res  <- resample(dwb_crop,  evi_stack_crop, method="bilinear")
landusea_stack_res  <- resample(landusea_stack_crop,  evi_stack_crop, method="bilinear")
bmph_stack_res  <- resample(bmph_stack_crop,  evi_stack_crop, method="bilinear")


# 3. Combine into one multilayer stack for analysis
all_env_vars <- c(evi_stack_crop, ndWi_stack_res, ndmi_stack_res,
                  ntl_stack_res, popn_den_res, dwb_res, lst_stack_res,
                  landusea_stack_res, bmph_stack_res)

# Convert to stack
env_stack <- stack(all_env_vars)

plot(all_env_vars)


##Explore Correlation
# Sample values across your study area
vals <- getValues(env_stack)

# Remove rows with NA
vals <- na.omit(vals)

# Compute correlation matrix
cor_mat <- cor(vals)

# Find highly correlated pairs (r > 0.7 or r < -0.7)
high_cor <- which(abs(cor_mat) > 0.7 & abs(cor_mat) < 1, arr.ind = TRUE)

high_cor

library(ggcorrplot)

aggcorr <- ggcorrplot(cor_mat, 
           hc.order = TRUE,       # hierarchical clustering
           type = "upper",
           lab = TRUE,            # show correlation values
           lab_size = 1.0,
           colors = c("blue", "white", "red"), 
           title = "Correlation Matrix of Environmental Variables")

print(aggcorr)

ggsave(paste0(LuDir, '/plots/', Sys.Date(), "/", 'Correlation Matrix for Agugu.pdf'), aggcorr, width = 11, height = 10)

library(usdm)

vif_result <- vifstep(all_env_vars, th = 5)
vif_result

# List selected layers (after dropping collinear ones)
#vif_result@selected


# Extract variable names of selected variables
vars_keep <- data.frame(variable = vif_result@results$Variables)

vars_keep

##
# subset the SpatRaster
# If vars_keep is a dataframe
vars_keep_vec <- vars_keep$variable  # Extract character vector

##Write variable names to file
write.csv(vars_keep_vec, file.path(LuOneDir, "variable_names.csv"))

predictors_a_subset <- env_stack[[vars_keep_vec]]

# check result
predictors_a_subset

# Save as multi-layer GeoTIFF
writeRaster(predictors_a_subset, 
            filename = "predictors_a_subset.tif", 
            overwrite = TRUE)

predictors_a_subset <- rast("C:/Users/ebamg/OneDrive - Loyola University Chicago/Documents/IB_KA_field_study-main/IB_KA_field_study-main/predictors_a_subset.tif")



























### Compute mean of EVI, NDMI and NDWI across its layers
library(terra)

# Compute mean of EVI across layers
evi_mean <- app(evi_stack_crop, fun = base::mean, na.rm = TRUE)
names(evi_mean) <- "EVI_mean"

# Compute mean of NDWI across layers
ndwi_mean <- app(ndWi_stack_res, fun = base::mean, na.rm = TRUE)
names(ndwi_mean) <- "NDWI_mean"

# Compute mean of NDMI across layers
ndmi_mean <- app(ndmi_stack_res, fun = base::mean, na.rm = TRUE)
names(ndmi_mean) <- "NDMI_mean"

# Inspect
evi_mean
ndwi_mean
ndmi_mean

# 3. (Optional) Combine into one multilayer stack for analysis
all_env_vars1 <- c(evi_mean, ndwi_mean, ndmi_mean, 
                  lst_stack_res, ntl_stack_res)

# # 3. Now combine
# predictors <- c(evi_stack_crop, ndWi_stack_res, ndmi_stack_res)
# 
# predictorsE <- c(evi_stack_crop, ndmi_stack_res)




##New environmental variables
library(terra)

library(terra)

# Step 0: Convert template to lon/lat if it was in UTM
# Convert RasterStack (raster package) to SpatRaster (terra)
evi_terra <- rast(evi_stack)   # now evi_terra is a SpatRaster
crs(evi_terra) <- "EPSG:32632"

template_ll <- project(evi_terra, "EPSG:4326", method="bilinear")
# Reproject to lon/lat (EPSG:4326)
template_ll <- project(evi_terra, "EPSG:4326", method="bilinear")


# Crop each raster to template extent first
ntl_crop  <- crop(ntl_rast, template_ll)
lst_crop  <- crop(lst_rast, template_ll)
popn_crop <- crop(popn_den_crop, template_ll)
ndwi_crop <- crop(ndWi_stack_crop, template_ll)
ndmi_crop <- crop(ndmi_stack_crop, template_ll)

# Then project
ntl_proj  <- project(ntl_crop, template_ll, method="bilinear")
lst_proj  <- project(lst_crop, template_ll, method="bilinear")
popn_proj <- project(popn_crop, template_ll, method="bilinear")
ndwi_proj <- project(ndwi_crop, template_ll, method="bilinear")
ndmi_proj <- project(ndmi_crop, template_ll, method="bilinear")

# Resample (to exactly match template_ll)
ntl_res  <- resample(ntl_proj, template_ll, method="bilinear")
lst_res  <- resample(lst_proj, template_ll, method="bilinear")
popn_res <- resample(popn_proj, template_ll, method="bilinear")
ndwi_res <- resample(ndwi_proj, template_ll, method="bilinear")
ndmi_res <- resample(ndmi_proj, template_ll, method="bilinear")

# Stack everything
env_stack <- c(template_ll, ndwi_res, ndmi_res, ntl_res, lst_res, popn_res)


# Step 3: Stack everything
env_stack <- c(template_ll, ndwi_res, ndmi_res, ntl_res, lst_res, popn_res)

# Step 4: Inspect
env_stack
plot(env_stack[[1]])


