# setwd("~/Documents/Projects/vox_military_jop")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("spdep", "rgdal", "rgeos", "geosphere",
  "spatialreg", "dplyr", "stringr", "sp")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
lapply(pkg, library, character.only = TRUE)

# ------------------------

# Load GIS
shp = readOGR("download_shp/shp_secciones_2019/SECC_CE_20190101.shp",
  layer = "SECC_CE_20190101")

# Load data
data = read.csv("create_dataset/output/dataset.csv") %>%
  # Limit to sections within 20km
  filter(min_dist_army < 20000) %>%
  # Fix CUSEC to merge with shapefile
  mutate(CUSEC = as.character(CUSEC)) %>%
  mutate(CUSEC = ifelse(str_length(CUSEC)==9, paste0("0", CUSEC), CUSEC))

# Get rid of missing data rows now
missing = is.na(data[,
  c("VOX_a19", "part_a19", "pop_2017_l", "renta_h_2017_l", "muni_pop_2017_l")])
data = data[-which(rowSums(missing) > 0),]

# Limit shapefile to sections in data (excl Ceuta & Melilla)
shp = shp[shp$CUSEC %in% data$CUSEC,]

# Merge both datasets
shp@data = left_join(shp@data, data)

# Save
saveRDS(shp, "slm_datajoin/output/shp.rds")
