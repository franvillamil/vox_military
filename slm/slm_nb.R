# setwd("~/Documents/Projects/vox_military_jop")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("spdep", "rgdal", "rgeos", "geosphere", "ggplot2",
  "spatialreg", "dplyr", "forcats", "stringr", "sp", "RColorBrewer", "classInt")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
lapply(pkg, library, character.only = TRUE)

# ------------------------

# Function to make First Letter capital
capitalize = function(str){
  c = strsplit(str, " ")[[1]]
  out = paste(toupper(substring(c, 1,1)), substring(c, 2), sep="", collapse=" ")
  return(out)
}

# ------------------------

# Load data
shp = readRDS("slm_datajoin/output/shp.rds")

# Get neighbor lists (contiguous and within 2km)
nb = poly2nb(shp, queen = TRUE)
nb2 = dnearneigh(coordinates(shp), d1 = 0, d2 = 2000)

# Transform to listw
nb_listw = nb2listw(nb, style = "W", zero.policy = TRUE)
nb2_listw = nb2listw(nb2, style = "W", zero.policy = TRUE)

# ------------------------

# SEM
sem_nb = errorsarlm(VOX_a19 ~ army +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc,
  data = shp@data, listw = nb_listw, zero.policy = TRUE)
sem_nb2 = errorsarlm(VOX_a19 ~ army +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc,
  data = shp@data, listw = nb2_listw, zero.policy = TRUE)
# S-DEM
sdem_nb = errorsarlm(VOX_a19 ~ army +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc,
  data = shp@data, list = nb_listw, etype = "mixed", zero.policy = TRUE)
sdem_nb2 = errorsarlm(VOX_a19 ~ army +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc,
  data = shp@data, list = nb2_listw, etype = "mixed", zero.policy = TRUE)

# Save models
saveRDS(sem_nb, "slm/output/m_sem_nb.rds")
saveRDS(sem_nb2, "slm/output/m_sem_nb2.rds")
saveRDS(sdem_nb, "slm/output/m_sdem_nb.rds")
saveRDS(sdem_nb2, "slm/output/m_sdem_nb2.rds")

# ------------------------

# Effect calculation army in SDEM (nb)
# get d = direct + indirect effects
W = listw2mat(nb_listw)
I = diag(nrow(W))
beta_army = coef(sdem_nb)[3]
theta_army = coef(sdem_nb)[9]
d = (I * beta_army + W * theta_army)
# get dx_k^i for each unit, taking an increase in army in
# a random section of Gijon
x = rep(0, nrow(shp@data))
x = matrix(x, ncol = 1)
eff_muni = 33024 # Gijón
set.seed(1991)
i = sample(which(shp@data$muni_code == eff_muni), 1)
x[i] = 1
# Calculate effect
dy = d %*% x
# Show larger effects
effects = data.frame(CUSEC = shp@data$CUSEC, dy)
effects = effects[order(-effects$dy), ]
# Transform to longlat and merge
shp_ll = spTransform(shp, CRS("+init=epsg:4326"))
shp_ll@data = left_join(shp_ll@data, effects)
# Assign colors
shp_ll@data$dy_col = "white"
shp_ll@data$dy_col[shp_ll@data$dy > min(effects$dy)] = gray(0.75)
shp_ll@data$dy_col[shp_ll@data$dy == median(effects$dy[effects$dy > min(effects$dy)])] = gray(0.5)
shp_ll@data$dy_col[shp_ll@data$dy > median(effects$dy[effects$dy > min(effects$dy)])] = gray(0.25)
shp_ll@data$dy_col[shp_ll@data$dy == max(shp_ll@data$dy)] = "red"

cd = 0.04
longlimit = c(coordinates(shp_ll[i,])[1] - cd, coordinates(shp_ll[i,])[1] + cd)
latlimit = c(coordinates(shp_ll[i,])[2] - cd, coordinates(shp_ll[i,])[2] + cd)
med = round(median(effects$dy[effects$dy > min(effects$dy)]),3)
max = round(max(shp_ll@data$dy),3)

pdf("slm/output/pred_dy_nb.pdf")
plot(shp_ll, xlim = longlimit, ylim = latlimit,
  col = shp_ll$dy_col)
legend("bottomright", bg = "white", inset = 0.05,
    legend = c("0", paste0("(", "0-", med, ")"), med, paste0("(", med, "-", max, ")"), max),
    fill = c("white", gray(c(0.75, 0.5, 0.25)), "red"))
dev.off()

# Getting all predicted effects, actual data
x_real = rep(0, nrow(shp@data))
x_real = matrix(x_real, ncol = 1)
i_bases = which(shp@data$army == 1)
x_real[i_bases] = 1
# Calculate effect
dy_real = d %*% x_real
# Get DF with effects
eff_real = data.frame(dy_real,
  CUSEC = shp@data$CUSEC,
  army = shp@data$army,
  prov = shp@data$prov,
  VOX_a19 = shp@data$VOX_a19,
  censo_a19 = shp@data$censo_a19,
  part_a19 = shp@data$part_a19)
# Calculate number of votes linked to military facilities
eff_real = eff_real %>%
  mutate(validos = censo_a19 * part_a19) %>%
  mutate(v_vox = validos * VOX_a19,
    v_vox_mt = validos * dy_real)
# And by province
eff_real_prov = eff_real %>%
  dplyr::group_by(prov) %>%
  summarize(
    v_vox = sum(v_vox),
    v_vox_mt_all = sum(v_vox_mt),
    v_vox_mt_diff = sum(v_vox_mt[army == 0])) %>%
  mutate(v_vox_mt_all_sh = v_vox_mt_all / v_vox,
    v_vox_mt_diff_sh = v_vox_mt_diff / v_vox) %>%
  mutate(prov = sapply(prov, function(x) capitalize(x))) %>%
as.data.frame

# Capitalize first letter of province
eff_real_prov$prov = factor(eff_real_prov$prov)
eff_real_prov$prov = fct_relevel(eff_real_prov$prov,
  levels(eff_real_prov$prov)[order(eff_real_prov$v_vox_mt_diff)])

pdf("slm/output/real_pred_dy_nb_diff_prov.pdf", height = 7.5, width = 6)
  ggplot(eff_real_prov, aes(x = reorder(prov, v_vox_mt_diff), y = v_vox_mt_diff)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      strip.text = element_text(colour = "grey30", size = 10),
      strip.background = element_blank()) +
    labs(y = "\nVotes attributed to nearby military bases", x = "")
dev.off()

pdf("slm/output/real_pred_dy_nb_diff_prov_sh.pdf", height = 7.5, width = 6)
  ggplot(eff_real_prov, aes(x = reorder(prov, v_vox_mt_diff_sh), y = v_vox_mt_diff_sh)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      strip.text = element_text(colour = "grey30", size = 10),
      strip.background = element_blank()) +
    labs(y = "\nVotes attributed to nearby military bases, as share of Vox votes", x = "")
dev.off()

pdf("slm/output/real_pred_dy_nb_all_prov.pdf", height = 7.5, width = 6)
  ggplot(eff_real_prov, aes(x = reorder(prov, v_vox_mt_all), y = v_vox_mt_all)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      strip.text = element_text(colour = "grey30", size = 10),
      strip.background = element_blank()) +
    labs(y = "\nVotes attributed to military bases", x = "")
dev.off()

pdf("slm/output/real_pred_dy_nb_all_prov_sh.pdf", height = 7.5, width = 6)
  ggplot(eff_real_prov, aes(x = reorder(prov, v_vox_mt_all_sh), y = v_vox_mt_all_sh)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      strip.text = element_text(colour = "grey30", size = 10),
      strip.background = element_blank()) +
    labs(y = "\nVotes attributed to military bases, as share of Vox votes", x = "")
dev.off()
