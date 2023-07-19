# setwd("~/Documents/Projects/vox_military_jop")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("tidyr", "dplyr", "rgdal", "rgeos")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
lapply(pkg, library, character.only = TRUE)
# -------------------------

# Load
c = read.csv("spatial_overlay/output/cuarteles_CUSEC.csv") %>%
  filter(tipo %in% c("base/cuartel", "formacion", "HQ"))
shp = readOGR("download_shp/shp_secciones_2019/SECC_CE_20190101.shp", layer = "SECC_CE_20190101")

# Turn into spatial object
coordinates(c) = ~long+lat
proj4string(c) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Transform to UTM
c_utm = spTransform(c, proj4string(shp))

# Create centroids
shp_centroids = gCentroid(shp, byid = TRUE)
shp_centroids = spTransform(shp_centroids, proj4string(c_utm))

# Calculate distances from each centroid
dist_mat = gDistance(c_utm, shp_centroids, byid = TRUE)

# Get closer distance for each municipality
min_dist = apply(dist_mat, 1, function(x) min(x))

# Create df
data = data.frame(CUSEC = shp@data$CUSEC, min_dist_army = min_dist)

# SAVE
write.csv(data, "distance_matrix/output/min_dist.csv", row.names = FALSE)
