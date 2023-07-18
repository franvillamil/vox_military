# setwd("~/Documents/Projects/cuarteles_militares")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("rgdal", "rgeos", "spdep", "tidyr", "dplyr", "stringr")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
lapply(pkg, library, character.only = TRUE)
# -------------------------

# Load GIS
shp = readOGR("download_shp/shp_secciones_2019/SECC_CE_20190101.shp", layer = "SECC_CE_20190101")

# Add data on cuarteles
c = read.csv("spatial_overlay/output/cuarteles_CUSEC.csv") %>%
  filter(tipo %in% c("base/cuartel", "formacion", "HQ")) %>%
  group_by(CUSEC) %>%
  summarize(
    army_noHQ = ifelse(any(tipo %in% c("base/cuartel", "formacion")), 1, 0)) %>%
  mutate(army = 1, CUSEC = as.character(CUSEC)) %>%
  mutate(CUSEC = ifelse(str_length(CUSEC)==9, paste0("0", CUSEC), CUSEC)) %>%
as.data.frame

# Add variables to sf
shp@data = left_join(shp@data, c)
shp@data$army[is.na(shp@data$army)] = 0
shp@data$army_noHQ[is.na(shp@data$army_noHQ)] = 0

# Contiguity Neighborhood
shp_nb = poly2nb(shp, queen=TRUE)

# Get neighbors within 2km, 5km, and 10km
nb10 = dnearneigh(coordinates(shp), d1 = 0, d2 = 10000)
nb5 = dnearneigh(coordinates(shp), d1 = 0, d2 = 5000)
nb2 = dnearneigh(coordinates(shp), d1 = 0, d2 = 2000)

# Create lags (binary)
get_lags = function(nb_vector, var){
  if(all(nb_vector==0)){
    return(0)
  } else {
    return(max(shp@data[nb_vector, var]))
  }
}

# Get dataframe
data = shp@data[, c("CUSEC", "army", "army_noHQ")]
# Fill with spatial lags
data$army_nb = sapply(shp_nb, get_lags, "army")
data$army_nb10 = sapply(nb10, get_lags, "army")
data$army_nb5 = sapply(nb5, get_lags, "army")
data$army_nb2 = sapply(nb2, get_lags, "army")
data$army_noHQ_nb = sapply(shp_nb, get_lags, "army_noHQ")
data$army_noHQ_nb10 = sapply(nb10, get_lags, "army_noHQ")
data$army_noHQ_nb5 = sapply(nb5, get_lags, "army_noHQ")
data$army_noHQ_nb2 = sapply(nb2, get_lags, "army_noHQ")

## SAVE
write.csv(data, "spatial_lags/output/cuarteles_lags.csv", row.names = FALSE)
