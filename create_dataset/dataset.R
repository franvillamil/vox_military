# setwd("~/Documents/Projects/vox_military_jop")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("tidyr", "dplyr", "stringr")
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

## Electoral data
elec_data = read.csv("download_elections/output/elec_data.csv") %>%
  mutate(CUSEC = sprintf("%010.f", CUSEC))

## Income data (latest: 2017)
renta = read.csv("input_data/secc_censal_renta.csv", skip = 5)[-1, 1:7]
renta = renta[1:(nrow(renta) - 4), ]
names(renta) = c("loc", "renta_p_2017", "renta_p_2016", "renta_p_2015",
  "renta_h_2017", "renta_h_2016", "renta_h_2015")
for(i in names(renta)[grepl("renta", names(renta))]){
  renta[,i] = as.numeric(gsub("\\.", "", renta[,i]))
}
renta = renta %>%
  mutate(CUSEC = gsub("(\\d+)\\s.*", "\\1", loc)) %>%
  filter(CUSEC %in% elec_data$CUSEC) %>%
  select(CUSEC, renta_h_2017)

## Demographic
demo = read.csv("input_data/secc_censal_indic_demograficos.csv",
  sep = ";", skip = 5)
demo = demo[-1, c(1, which(demo[1,]==2017))]
names(demo) = c("loc", paste0(c("edad_media", "pop_under18", "pop_over65",
  "mean_hhold_size", "single_hhold", "pop"), "_2017"))
for(i in names(demo)[grepl("_2017", names(demo))]){
  demo[,i] = gsub("\\.", "", demo[,i])
  demo[,i] = as.numeric(gsub(",", ".", demo[,i]))
}
demo = demo %>%
  mutate(CUSEC = gsub("(\\d+)\\s.*", "\\1", loc)) %>%
  filter(CUSEC %in% elec_data$CUSEC) %>%
  select(CUSEC, pop_2017, mean_hhold_size_2017, pop_over65_2017)

## Merge basic dataframe
data = merge(elec_data, renta, all.x = TRUE)
data = merge(data, demo, all.x = TRUE)

## Cuarteles
c = read.csv("spatial_lags/output/cuarteles_lags.csv") %>%
  mutate(CUSEC = as.character(CUSEC)) %>%
  mutate(CUSEC = ifelse(str_length(CUSEC)==9, paste0("0", CUSEC), CUSEC)) %>%
as.data.frame

# Merge (diff because of new sections throughout 2019)
data = merge(data, c, all.x = TRUE)

## Get mininum distance to cuarteles
mindist = read.csv("distance_matrix/output/min_dist.csv") %>%
  mutate(CUSEC = as.character(CUSEC)) %>%
  mutate(CUSEC = ifelse(str_length(CUSEC)==9, paste0("0", CUSEC), CUSEC)) %>%
as.data.frame
# Merge
data = merge(data, mindist, all.x = TRUE)

# Create inverse distance
data$inv_log_dist = 1 / log(data$min_dist_army)

## Muni and prov IDs
data = data %>%
  mutate(
    muni_code = str_sub(CUSEC, 1, 5),
    prov = code_to_prov(str_sub(CUSEC, 1, 2))
  )

## Flag capitales regiones militares
cg = c("Madrid", "Sevilla", "Valencia", "Barcelona", "Burgos", "Coruna, A",
  "Zaragoza", "Valladolid", "Granada")
HQ_francoism = name_to_code(cg, prov = gsub("Coruna, A", "a coruna", cg))
HQ_democ = name_to_code(cg[1:5], prov = gsub("Coruna, A", "a coruna", cg[1:5]))
# Ciudades
data$region_militar_franc = ifelse(data$muni_code %in% HQ_francoism, 1, 0)
data$region_militar_democ = ifelse(data$muni_code %in% HQ_democ, 1, 0)

## Get total municipality population
m_pop = data %>%
  group_by(muni_code) %>%
  summarize(
    muni_pop_2017 = sum(pop_2017, na.rm = TRUE),
    muni_num_secs = length(unique(CUSEC))) %>%
  mutate(muni_pop_per_sec = muni_pop_2017 / muni_num_secs) %>%
as.data.frame
# And merge
data = merge(data, m_pop, all.x = TRUE)

## Mark municipalities over 50k population & army location
cities50k = data %>%
  filter(muni_pop_2017 > 50000) %>%
  group_by(muni_code) %>%
  summarize(army = as.integer(any(army == 1))) %>%
  filter(army == 1)
data$cities50k = ifelse(data$muni_code %in% cities50k$muni_code, 1, 0)

## Cuarteles 1920
c20 = read.csv("cuarteles1920/output/c1920_CUSEC.csv") %>%
  group_by(CUSEC) %>%
  summarize(
    cuarteles1920 = length(CUSEC),
    cuarteles1920_totalcap = sum(capacidad)) %>%
  ungroup() %>%
  mutate(CUSEC = as.character(CUSEC)) %>%
  mutate(CUSEC = ifelse(str_length(CUSEC)==9, paste0("0", CUSEC), CUSEC)) %>%
as.data.frame

# Merge and turn NA to 0
data = merge(data, c20, all.x = TRUE)
data$cuarteles1920 = coalesce(data$cuarteles1920, 0)

## Population lost between 1930 and 2011
census = read.csv("input_data/INE_census.csv") %>%
  mutate(muni_code = changes_newcode(muni_code, "1930", "2011")) %>%
  group_by(muni_code) %>%
  summarize(
    pop1930 = sum(c1930, na.rm = T),
    pop2011 = sum(c2011, na.rm = T)) %>%
  ungroup() %>%
  mutate(pop_chg_3011 = (pop2011 - pop1930)/pop1930) %>%
  select(muni_code, pop_chg_3011) %>%
  rename(muni_code3011 = muni_code)

## Election results in 1930s
elec36 = read.csv("input_data/results1936.csv") %>%
  mutate(muni_code3011 = sprintf("%0.5d", muni_code)) %>%
  select(-muni_code)

# Merge old data (adapting codes)
data$muni_code3011 = changes_newcode(data$muni_code, "1930", "2011")
data = merge(data, census, all.x = TRUE)
data = merge(data, elec36, all.x = TRUE)
data = data %>% select(-muni_code3011)

## Log
data = data %>%
  mutate(
  # Logs
    pop_2017_l = log(pop_2017),
    muni_pop_2017_l = log(muni_pop_2017),
    renta_h_2017_l = log(renta_h_2017))

# Remove Ceuta y Melilla
data = data %>% filter(!prov %in% c("ceuta", "melilla"))

## SAVE
write.csv(data, "create_dataset/output/dataset.csv", row.names = FALSE)
