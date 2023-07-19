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

# Load data
shp = readRDS("slm_datajoin/output/shp.rds")

# Inverse distance matrix
centroids = gCentroid(shp, byid = TRUE)
dist_mat = gDistance(centroids, byid = TRUE)
inv_mat = 1/dist_mat
diag(inv_mat) = 0

# Transform to listw
inv_listw = mat2listw(inv_mat, style = "W")

# Clean up memory space
rm(inv_mat, dist_mat, centroids)

# ------------------------

# SEM
sem_ild = errorsarlm(VOX_a19 ~ army +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc,
  data = shp@data, listw = inv_listw, zero.policy = TRUE)
# S-DEM
sdem_ild = errorsarlm(VOX_a19 ~ army +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc,
  data = shp@data, list = inv_listw, etype = "mixed", zero.policy = TRUE)

# Save models
saveRDS(sem_ild, "slm/output/m_sem_ild.rds")
saveRDS(sdem_ild, "slm/output/m_sdem_ild.rds")
