# setwd("~/Documents/Projects/vox_military_jop")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("tidyr", "dplyr", "rgdal", "rgeos", "stringr")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# muniSpain (Github)
if(!"muniSpain" %in% rownames(installed.packages())){
  if(!"devtools" %in% rownames(installed.packages())){install.packages("devtools")}
  library(devtools)
  install_github("franvillamil/muniSpain")
}
# Load
lapply(c(pkg, "muniSpain"), library, character.only = TRUE)
# -------------------------

# Load
cuarteles = adapt(read.csv("input_data/cuarteles.csv"))
shp = readOGR("download_shp/shp_secciones_2019/SECC_CE_20190101.shp", layer = "SECC_CE_20190101")

# Transform to longlat
shp = spTransform(shp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Get coordinates
coords = str_split(cuarteles$latlon, ",")
cuarteles$lat = as.numeric(unlist(lapply(coords, function(x) x[1])))
cuarteles$long = as.numeric(unlist(lapply(coords, function(x) x[2])))

# Set up spatial data
coordinates(cuarteles) = ~long+lat
proj4string(cuarteles) = proj4string(shp)

# Spatial overlay
cuarteles$CUSEC = over(cuarteles, shp)$CUSEC

# Clean
cuarteles_df = cuarteles@data %>%
  select(provincia, nombre, tipo, CUSEC) %>%
  mutate(long = coordinates(cuarteles)[,1],
    lat = coordinates(cuarteles)[,2])

# SAVE
write.csv(cuarteles_df, "spatial_overlay/output/cuarteles_CUSEC.csv", row.names = FALSE)
