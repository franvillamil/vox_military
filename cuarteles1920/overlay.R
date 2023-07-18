# setwd("~/Documents/Projects/cuarteles_militares")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("tidyr", "dplyr", "rgdal", "rgeos", "stringr")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
lapply(pkg, library, character.only = TRUE)
# -------------------------

# Load
c20 = read.csv("input_data/cuarteles1920.csv", sep = ";") %>%
  filter(!is.na(coords)) %>%
  filter(capacidad >= 100 | is.na(capacidad))
shp = readOGR("download_shp/shp_secciones_2019/SECC_CE_20190101.shp", layer = "SECC_CE_20190101")

## NOTE remote later
c20 = subset(c20, reg_militar %in% c(1,3,4,5))
if(any(c20$coords=="")){stop("forgot to code something!")}

# Transform to longlat
shp = spTransform(shp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Get coordinates
coords = str_split(c20$coords, ",")
c20$lat = as.numeric(unlist(lapply(coords, function(x) x[1])))
c20$long = as.numeric(unlist(lapply(coords, function(x) x[2])))

# Set up spatial data
coordinates(c20) = ~long+lat
proj4string(c20) = proj4string(shp)

# Spatial overlay
c20$CUSEC = over(c20, shp)$CUSEC

# Clean
c20_df = c20@data %>%
  select(nombre, capacidad, CUSEC) %>%
  mutate(
    long = coordinates(c20)[,1],
    lat = coordinates(c20)[,2]) %>%
  mutate(muni_code = str_sub(CUSEC, -10L, -6L))

# SAVE
write.csv(c20_df, "cuarteles1920/output/c1920_CUSEC.csv", row.names = FALSE)
