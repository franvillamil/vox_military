# setwd("~/Documents/Projects/cuarteles_militares")
options(stringsAsFactors = FALSE)
options("modelsummary_format_numeric_latex" = "plain")
# List of packages
pkg = c("plyr", "dplyr", "stringr", "ggplot2",
  "rgdal", "rgeos", "maptools", "RColorBrewer", "cowplot",
  "readr", "modelsummary") # "lavaan", "smooth", "Hmisc"
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
# Functions
source("func/raincloud_func.R")
# -------------------------

# Function to fix coordinates (Canary Islands)
# Borrowed from https://stackoverflow.com/questions/13757771
fix1 = function(object, params){
  r = params[1]
  scale = params[2]
  shift = params[3:4]
  object = elide(object, rotate = r)
  size = max(apply(bbox(object), 1, diff))/scale
  object = elide(object, scale = size)
  object = elide(object, shift = shift)
  object
}


# Load data
data = read.csv("create_dataset/output/dataset.csv") %>%
  mutate(CUSEC = as.character(CUSEC)) %>%
  mutate(CUSEC = ifelse(str_length(CUSEC)==9, paste0("0", CUSEC), CUSEC))

# Table - summary statistics
data_desc = data %>%
  filter(muni_pop_2017_l != -Inf) %>%
  dplyr::select(
    `Military facility` = army,
    `VOX support, April 2019` = VOX_a19,
    `Turnout, April 2019` = part_a19,
    `Log. Population, 2017` = pop_2017_l,
    `Log. Household income, 2019` = renta_h_2017_l,
    `Log. Municipal Pop, 2017` = muni_pop_2017_l,
    `Military region HQ` = region_militar_franc)

datasummary_skim(data_desc,
  title = "Local-level data descriptive statistics\\label{tab:local_desc}",
  histogram = FALSE,
  output = "descriptives/output/local_descriptives.tex")
system("sed -i '' 's/egin{table}/egin{table}[!h]/g' descriptives/output/local_descriptives.tex")

# Load cuarteles (current and old)
cuarteles = read.csv("input_data/cuarteles.csv") %>%
  filter(tipo %in% c("base/cuartel", "formacion", "HQ"))
c1920 = read.csv("input_data/cuarteles1920.csv", sep = ";") %>%
  filter(!is.na(coords)) %>%
  filter(capacidad >= 100 | is.na(capacidad))

# Load shapefiles
shp = readOGR("download_shp/shp_secciones_2019/SECC_CE_20190101.shp",
  layer = "SECC_CE_20190101")
shp = shp[shp$NPRO == "Madrid",]
shp = spTransform(shp, CRS("+init=epsg:4326"))
prov = readOGR("download_shp/shp_provincias/gadm36_ESP_2.shp", layer = "gadm36_ESP_2")
prov@data$prov = adapt(prov@data$NAME_2, tolower = TRUE)

### 1. SUMMARY TABLE


### 2. MAP SPAIN

# Changing Canary Islands
provEA = spTransform(prov,CRS("+init=epsg:2163"))
ci = provEA[provEA$prov %in% c("santa cruz de tenerife", "las palmas"),]
ci = fix1(ci, c(-20.5,1,-2.5e05,2e06))
proj4string(ci) = proj4string(provEA)
provEA = provEA[!provEA$prov %in% c("santa cruz de tenerife", "las palmas"),]
provEA = rbind(provEA, ci)
prov2 = spTransform(provEA, CRS("+init=epsg:4326"))

# Get whole country
c = gUnaryUnion(prov2)

# Change coordinates of cuarteles
cuarteles = cuarteles %>%
  mutate(
    lat = sapply(str_split(latlon, ","), function(x) as.numeric(x[1])),
    long = sapply(str_split(latlon, ","), function(x) as.numeric(x[2]))) %>%
  mutate(lat2 = ifelse(lat < 30, lat + 7.08, lat),
    long2 = ifelse(long < -10, long + 16.8, long))

c1920 = c1920 %>%
  mutate(
    lat = sapply(str_split(coords, ","), function(x) as.numeric(x[1])),
    long = sapply(str_split(coords, ","), function(x) as.numeric(x[2]))) %>%
  mutate(lat2 = ifelse(lat < 30, lat + 7.08, lat),
    long2 = ifelse(long < -10, long + 16.7, long))


pdf("descriptives/output/cuarteles_map.pdf", width = 10, height = 8)
plot(prov2, col = "white", border = "grey", lwd = 0.25)
plot(c, border = grey(0.5), lwd = 1, add = T)
points(x = cuarteles$long2, y = cuarteles$lat2, col = "red", pch = 20)
segments(x0 = -1.85, y0 = 34.1, y1 = 36, col = grey(0.5), lwd = 2, lty = "dashed")
segments(x0 = -0.5, x1 = 4, y0 = 36.75, col = grey(0.5), lwd = 2, lty = "dashed")
segments(x0 = -1.85, y0 = 36, x1 = -0.5, y1 = 36.75, col = grey(0.5), lwd = 2, lty = "dashed")
dev.off()

pdf("descriptives/output/cuarteles1920_map.pdf", width = 10, height = 8)
plot(prov2, col = "white", border = "grey", lwd = 0.25)
plot(c, border = grey(0.5), lwd = 1, add = T)
points(x = c1920$long2, y = c1920$lat2, col = "red", pch = 20)
segments(x0 = -1.85, y0 = 34.1, y1 = 36, col = grey(0.5), lwd = 2, lty = "dashed")
segments(x0 = -0.5, x1 = 4, y0 = 36.75, col = grey(0.5), lwd = 2, lty = "dashed")
segments(x0 = -1.85, y0 = 36, x1 = -0.5, y1 = 36.75, col = grey(0.5), lwd = 2, lty = "dashed")
dev.off()

pdf("descriptives/output/cuarteles_comparison_map.pdf", width = 10, height = 8)
plot(prov2, col = "white", border = "grey", lwd = 0.25)
plot(c, border = grey(0.5), lwd = 1, add = T)
points(x = c1920$long2, y = c1920$lat2, col = alpha("blue", 0.3), pch = 20)
points(x = cuarteles$long2, y = cuarteles$lat2, col = alpha("red", 0.3), pch = 20)
segments(x0 = -1.85, y0 = 34.1, y1 = 36, col = grey(0.5), lwd = 2, lty = "dashed")
segments(x0 = -0.5, x1 = 4, y0 = 36.75, col = grey(0.5), lwd = 2, lty = "dashed")
segments(x0 = -1.85, y0 = 36, x1 = -0.5, y1 = 36.75, col = grey(0.5), lwd = 2, lty = "dashed")
legend(0.5, 37.75, cex = 0.7,
  legend = c("Military facilities in 1920", "Contemporary facilities (2021)"),
  pch = c(20, 20), col = c("blue", "red"))
dev.off()

# NOTE When transitioning to sf:
# - https://ropenspain.github.io/mapSpain/reference/esp_get_can_box.html
# library(sf)
# library(mapSpain)
# map_sp_prov = esp_get_prov(moveCAN = c(16.7, 7.08), epsg = 4326)
# plot(st_geometry(map_sp_prov))
# pdf("try.pdf")
# ggplot(map_sp_prov) +
#   geom_sf() +
#   geom_point(data = c1920, aes(x = long2, y = lat2), size = 4,
#     shape = 20, fill = "darkred") +
#   theme_bw()
# dev.off()

### 2. ZOOM MAP MADRID

# Load VOX data
shp$VOX_a19 = data$VOX_a19[match(shp$CUSEC, data$CUSEC)]
shp$VOX_n19 = data$VOX_n19[match(shp$CUSEC, data$CUSEC)]

# Setup color palette
greens = brewer.pal(5, "Greens")
shp$VOX_a19_col = ifelse(shp$VOX_a19 > 0.05, greens[1], "white")
shp$VOX_a19_col[shp$VOX_a19 >= 0.1] = greens[2]
shp$VOX_a19_col[shp$VOX_a19 >= 0.15] = greens[3]
shp$VOX_a19_col[shp$VOX_a19 >= 0.2] = greens[4]
shp$VOX_a19_col[shp$VOX_a19 >= 0.25] = greens[5]

# Get limits for Madrid
minlat = min(coordinates(shp[shp$NMUN == "Madrid",])[,2])
maxlat = max(coordinates(shp[shp$NMUN == "Madrid",])[,2])
minlon = min(coordinates(shp[shp$NMUN == "Madrid",])[,1])
maxlon = max(coordinates(shp[shp$NMUN == "Madrid",])[,1])

# Plot
pdf("descriptives/output/madrid_map.pdf")
par(mar=c(0,0,0,0))
plot(shp, col = shp$VOX_a19_col, border = alpha("black", 0.3), lwd = 0.1,
  xlim = c(minlon, maxlon), ylim = c(minlat, maxlat))
points(x = cuarteles$long, y = cuarteles$lat, col = "red", pch = 20, cex = 2)
legend("bottomright", bg = "white", inset = 0.05,
  legend = c("0-5%", "5-10%", "10-15%", "15-20%", "20-25%", "+25%"),
  fill = c("white", greens))
dev.off()

### 4. RAINPLOTS (VOX & MILITARY)

pdf("descriptives/output/rainplot_army_a19.pdf", width = 3.5, height = 4)
ggplot(subset(data, !is.na(VOX_a19) & !is.na(army)),
    aes(x = factor(army), y = VOX_a19)) +#, fill = factor(army))) +
  geom_flat_violin(position = position_nudge(x = .25, y = 0),
    adjust = 2, trim = FALSE, color = "#00000000", fill = gray(0.75)) +
  geom_point(position = position_jitter(width = .15), size = .25, alpha = 0.5) +
  geom_boxplot(aes(x = factor(army), y = VOX_a19),
    position = position_nudge(x = .25, y = 0),
    outlier.shape = NA, alpha = 0.3, width = .1, colour = "black") +
  theme_classic() +
  # theme(legend.position = "none") +
  # scale_fill_manual(values = c("#2c7fb8", "#2ca25f")) +
  scale_x_discrete(labels = c("No presence", "Military facility")) +
  labs(x = "", y = "VOX electoral share")
dev.off()

pdf("descriptives/output/rainplot_army_nb_a19.pdf", width = 3.5, height = 4)
ggplot(subset(data, !is.na(VOX_a19) & !is.na(army_nb) & army == 0),
    aes(x = factor(army_nb), y = VOX_a19)) +#, fill = factor(army_nb))) +
  geom_flat_violin(position = position_nudge(x = .25, y = 0),
    adjust = 2, trim = FALSE, color = "#00000000", fill = gray(0.75)) +
  geom_point(position = position_jitter(width = .15), size = .25, alpha = 0.5) +
  geom_boxplot(aes(x = factor(army_nb), y = VOX_a19),
    position = position_nudge(x = .25, y = 0),
    outlier.shape = NA, alpha = 0.3, width = .1, colour = "black") +
  theme_classic() +
  # theme(legend.position = "none") +
  # scale_fill_manual(values = c("#2c7fb8", "#2ca25f")) +
  scale_x_discrete(labels = c("No presence", "Neighboring\nmilitary facility")) +
  labs(x = "", y = "VOX electoral share")
dev.off()

pdf("descriptives/output/rainplot_army_n19.pdf", width = 3.5, height = 4)
ggplot(subset(data, !is.na(VOX_n19) & !is.na(army)),
    aes(x = factor(army), y = VOX_a19)) +#, fill = factor(army))) +
  geom_flat_violin(position = position_nudge(x = .25, y = 0),
    adjust = 2, trim = FALSE, color = "#00000000", fill = gray(0.75)) +
  geom_point(position = position_jitter(width = .15), size = .25, alpha = 0.5) +
  geom_boxplot(aes(x = factor(army), y = VOX_n19),
    position = position_nudge(x = .25, y = 0),
    outlier.shape = NA, alpha = 0.3, width = .1, colour = "black") +
  theme_classic() +
  # theme(legend.position = "none") +
  # scale_fill_manual(values = c("#2c7fb8", "#2ca25f")) +
  scale_x_discrete(labels = c("No presence", "Military facility")) +
  labs(x = "", y = "VOX electoral share")
dev.off()

pdf("descriptives/output/rainplot_army_nb_n19.pdf", width = 3.5, height = 4)
ggplot(subset(data, !is.na(VOX_n19) & !is.na(army_nb) & army == 0),
    aes(x = factor(army_nb), y = VOX_a19)) +#, fill = factor(army_nb))) +
  geom_flat_violin(position = position_nudge(x = .25, y = 0),
    adjust = 2, trim = FALSE, color = "#00000000", fill = gray(0.75)) +
  geom_point(position = position_jitter(width = .15), size = .25, alpha = 0.5) +
  geom_boxplot(aes(x = factor(army_nb), y = VOX_n19),
    position = position_nudge(x = .25, y = 0),
    outlier.shape = NA, alpha = 0.3, width = .1, colour = "black") +
  theme_classic() +
  # theme(legend.position = "none") +
  # scale_fill_manual(values = c("#2c7fb8", "#2ca25f")) +
  scale_x_discrete(labels = c("No presence", "Neighboring\nmilitary facility")) +
  labs(x = "", y = "VOX electoral share")
dev.off()

### 5. RAINPLOTS (INCOME & MILITARY)

pdf("descriptives/output/rainplot_army_income.pdf", width = 3.5, height = 4)
ggplot(subset(data, !is.na(renta_h_2017_l) & !is.na(army)),
    aes(x = factor(army), y = renta_h_2017_l)) +#, fill = factor(army))) +
  geom_flat_violin(position = position_nudge(x = .25, y = 0),
    adjust = 2, trim = FALSE, color = "#00000000", fill = gray(0.75)) +
  geom_point(position = position_jitter(width = .15), size = .25, alpha = 0.5) +
  geom_boxplot(aes(x = factor(army), y = renta_h_2017_l),
    position = position_nudge(x = .25, y = 0),
    outlier.shape = NA, alpha = 0.3, width = .1, colour = "black") +
  theme_classic() +
  # theme(legend.position = "none") +
  # scale_fill_manual(values = c("#2c7fb8", "#2ca25f")) +
  scale_x_discrete(labels = c("No presence", "Military facility")) +
  labs(x = "", y = "(Log) Mean household income")
dev.off()

pdf("descriptives/output/rainplot_army_income_cities50k.pdf", width = 3.5, height = 4)
ggplot(subset(data, !is.na(renta_h_2017_l) & !is.na(army) & cities50k == 1),
    aes(x = factor(army), y = renta_h_2017_l)) +#, fill = factor(army))) +
  geom_flat_violin(position = position_nudge(x = .25, y = 0),
    adjust = 2, trim = FALSE, color = "#00000000", fill = gray(0.75)) +
  geom_point(position = position_jitter(width = .15), size = .25, alpha = 0.5) +
  geom_boxplot(aes(x = factor(army), y = renta_h_2017_l),
    position = position_nudge(x = .25, y = 0),
    outlier.shape = NA, alpha = 0.3, width = .1, colour = "black") +
  theme_classic() +
  # theme(legend.position = "none") +
  # scale_fill_manual(values = c("#2c7fb8", "#2ca25f")) +
  scale_x_discrete(labels = c("No presence", "Military facility")) +
  labs(x = "", y = "(Log) Mean household income")
dev.off()

pdf("descriptives/output/rainplot_army_noHQ_income.pdf", width = 3.5, height = 4)
ggplot(subset(data, !is.na(renta_h_2017_l) & !is.na(army_noHQ)),
    aes(x = factor(army_noHQ), y = renta_h_2017_l)) +#, fill = factor(army_noHQ))) +
  geom_flat_violin(position = position_nudge(x = .25, y = 0),
    adjust = 2, trim = FALSE, color = "#00000000", fill = gray(0.75)) +
  geom_point(position = position_jitter(width = .15), size = .25, alpha = 0.5) +
  geom_boxplot(aes(x = factor(army_noHQ), y = renta_h_2017_l),
    position = position_nudge(x = .25, y = 0),
    outlier.shape = NA, alpha = 0.3, width = .1, colour = "black") +
  theme_classic() +
  # theme(legend.position = "none") +
  # scale_fill_manual(values = c("#2c7fb8", "#2ca25f")) +
  scale_x_discrete(labels = c("No presence", "Military facility\n(excluding HQ)")) +
  labs(x = "", y = "(Log) Mean household income")
dev.off()
