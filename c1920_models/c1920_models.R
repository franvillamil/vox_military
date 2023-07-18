# setwd("~/Documents/Projects/cuarteles_militares")
options(stringsAsFactors = FALSE)
options("modelsummary_format_numeric_latex" = "plain")
# List of packages
pkg = c("modelsummary", "kableExtra", "dplyr", "ggplot2")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
lapply(pkg, library, character.only = TRUE)
# --------------------------------------------------

## Modelsummary stuff

# Label models (for modelsummary)
modnames = function(l){
  names(l) = paste0("(", 1:length(l), ")")
  return(l)
}

coef_recode = c(
  'c1920_bin' = 'Military facility in 1920',
  'army' = 'Military facility in 2021',
  'cuartel_lost' = 'Facility in 1920 but abandoned',
  'cuartel_lost_na' = 'Facility in 1920 but abandoned',
  'VOX_a19' = 'VOX share (April 2019)',
  'pop_chg_3011' = 'Pop. change, 1930-2011 (municipality)',
  'pop_2017_l' = '(Log) Population, 2017',
  'region_militar_franc' = 'Military region HQ',
  'muni_pop_2017_l' = '(Log) Municipality pop, 2017',
  'renta_h_2017_l' = '(Log) Household income, 2017',
  'dcha1936' = 'Rightist support, 1936 (muni-level)',
  'PP_1982_10' = 'AP support, 1982',
  'ERight_1982_10' = 'Extreme Right support, 1982',
  'PP_1980s' = 'AP/PP support, mean 1980s',
  'ERight_1980s' = 'Extreme Right support, mean 1980s'
)

gs = list(
  list("raw" = "nobs", "clean" = "$n$", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 2),
  list("raw" = "adj.r.squared", "clean" = "Adj. $R^2$", "fmt" = 2),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1))

n = "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001."

# --------------------------------------------------

# Add REGION (1920) for fixed effects
r1 = c("madrid", "toledo", "ciudad real", "cuenca", "badajoz", "jaen")
r2 = c("huelva", "sevilla", "cadiz", "malaga", "granada", "cordoba")
r3 = c("valencia", "alicante", "albacete", "murcia", "almeria")
r4 = c("barcelona", "girona", "huesca", "lleida")
r5 = c("huesca", "zaragoza", "teruel", "castellon", "guadalajara", "soria")
r6 = c("cantabria", "bizkaia", "gipuzkoa", "alava", "palencia", "burgos", "la rioja", "navarra")
r7 = c("caceres", "salamanca", "zamora", "avila", "valladolid", "segovia")
r8 = c("ourense", "pontevedra", "lugo", "a coruna", "asturias", "leon")
rbal = "baleares"
rcan = c("las palmas", "santa cruz de tenerife")

# Load data, remove non-existing secciones
data = read.csv("create_dataset/output/dataset.csv") %>%
  filter(!is.na(pop_2017)) %>%
  ## Variable creation: cuarteles in 1920, lost, etc
  mutate(
    new_cuartel = ifelse(cuarteles1920 == 0 & army == 1, 1, 0),
    cuartel_lost = ifelse(cuarteles1920 > 0 & army == 0, 1, 0),
    c1920_bin = ifelse(cuarteles1920 > 0, 1, 0),
    cuartel_1920_or_now = ifelse(cuarteles1920 > 0 | army == 1, 1, 0)) %>%
  mutate(cuartel_lost_na = ifelse(cuarteles1920 == 0, NA, cuartel_lost)) %>%
  # # Regiones militares in 1920
  # mutate(reg1920 = ifelse(prov %in% r1, "r1", NA)) %>%
  # mutate(reg1920 = ifelse(prov %in% r2, "r2", reg1920)) %>%
  # mutate(reg1920 = ifelse(prov %in% r3, "r3", reg1920)) %>%
  # mutate(reg1920 = ifelse(prov %in% r4, "r4", reg1920)) %>%
  # mutate(reg1920 = ifelse(prov %in% r5, "r5", reg1920)) %>%
  # mutate(reg1920 = ifelse(prov %in% r6, "r6", reg1920)) %>%
  # mutate(reg1920 = ifelse(prov %in% r7, "r7", reg1920)) %>%
  # mutate(reg1920 = ifelse(prov %in% r8, "r8", reg1920)) %>%
  # mutate(reg1920 = ifelse(prov %in% rbal, "rbal", reg1920)) %>%
  # mutate(reg1920 = ifelse(prov %in% rcan, "rcan", reg1920)) %>%
  # Create mean ERight & PP between 1982-1989
  rowwise() %>%
  mutate(
    PP_1980s = mean(c(PP_1982_10, PP_1986_06, PP_1989_10), na.rm = TRUE),
    ERight_1980s = mean(c(ERight_1982_10, ERight_1986_06, ERight_1989_10), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pop_chg_3011 = ifelse(pop_chg_3011 == "Inf", NA, pop_chg_3011))


### ------------------------------------------
### MODEL 1 - what correlates with losing cuarteles?
# NOTE: be careful when including pop_chg_3011, missing data involved!

m_cuartel_lost = modnames(list(
  glm(cuartel_lost_na ~ pop_2017_l + muni_pop_2017_l + pop_chg_3011 +
    renta_h_2017_l + region_militar_franc,
    data = data, family = "binomial"),
  glm(cuartel_lost_na ~ pop_2017_l + muni_pop_2017_l + pop_chg_3011 +
    renta_h_2017_l + region_militar_franc + dcha1936,
    data = data, family = "binomial"),
  glm(cuartel_lost_na ~ pop_2017_l + muni_pop_2017_l + pop_chg_3011 +
    renta_h_2017_l + region_militar_franc + PP_1980s,
    data = data, family = "binomial")
  ))


modelsummary(
  models = m_cuartel_lost,
  output = "latex",
  estimate = "{estimate}{stars}",
  coef_map = coef_recode,
  # coef_omit = "reg1920",
  gof_map = gs,
  title = "Abandoned military facilities between 1920 and 2021\\label{tab:glm_lost_c1920}",
  # add_rows = as.data.frame(rbind(
  #   c("Military region (1920) FE", rep(c("No", "Yes"), length(m_cuartel_lost)/2) ))),
  threeparttable = TRUE) %>%
# add_header_above(c(" " = 1, "h1" = 2, "h2" = 2)) %>%
footnote(general = n, threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
kable_styling(latex_options = "hold_position") %>%
save_kable(file = "c1920_models/output/tab_lost_c1920.tex")

### ------------------------------------------
### MODEL 2 - Getting new cuarteles

m_newcuartel = modnames(list(
  glm(new_cuartel ~ pop_2017_l + muni_pop_2017_l + pop_chg_3011 +
    renta_h_2017_l + region_militar_franc + factor(prov),
    data = data, family = "binomial"),
  glm(new_cuartel ~ pop_2017_l + muni_pop_2017_l + pop_chg_3011 +
    renta_h_2017_l + region_militar_franc + dcha1936 + factor(prov),
    data = data, family = "binomial"),
  glm(new_cuartel ~ pop_2017_l + muni_pop_2017_l + pop_chg_3011 +
    renta_h_2017_l + region_militar_franc + PP_1980s + factor(prov),
    data = data, family = "binomial")
  ))

modelsummary(
  models = m_newcuartel,
  output = "latex",
  estimate = "{estimate}{stars}",
  coef_map = coef_recode,
  coef_omit = "prov",
  gof_map = gs,
  title = "New military facilities between 1920 and 2021\\label{tab:glm_new_cuarteles}",
  add_rows = as.data.frame(rbind(
    c("Province FE", rep("Yes", length(m_newcuartel)) ))),
  threeparttable = TRUE) %>%
# add_header_above(c(" " = 1, "h1" = 2, "h2" = 2)) %>%
footnote(general = paste(n, "Province FE not shown."),
  threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
kable_styling(latex_options = "hold_position") %>%
save_kable(file = "c1920_models/output/tab_new_cuarteles.tex")

### ------------------------------------------
### MODEL 3 - Placebo analyses

# Formulae
controls = paste(c("part_a19", "pop_2017_l", "renta_h_2017_l", "muni_pop_2017_l",
  "region_militar_franc", "factor(prov)"), collapse = " + ")
c1920_formulae = list(
  as.formula(paste0("VOX_a19 ~ c1920_bin + ", controls)),
  as.formula(paste0("VOX_a19 ~ c1920_bin + army + ", controls)),
  as.formula(paste0("VOX_a19 ~ cuartel_lost + ", controls)),
  as.formula(paste0("VOX_a19 ~ cuartel_lost_na + ", controls))
  )

# Main models
m_vox_c1920 = lapply(c1920_formulae, function(x) lm(x, data = data))
# 3 subsamples: a) cities above 50k, b) region capitals, c) high income
m_vox_c1920_50k = lapply(c1920_formulae, function(x)
  lm(x, data = subset(data, cities50k == 1)))
m_vox_c1920_HQ = lapply(c1920_formulae, function(x)
  lm(x, data = subset(data, region_militar_franc == 1)))
m_vox_c1920_hinc = lapply(c1920_formulae, function(x)
  lm(x, data = subset(data, renta_h_2017 > quantile(data$renta_h_2017, 0.75, na.rm=T))))


## TABLES

modelsummary(
  models = m_vox_c1920,
  output = "latex",
  # vcov = ~prov,
  estimate = "{estimate}{stars}",
  coef_map = coef_recode,
  coef_omit = "prov",
  gof_map = gs,
  title = "VOX support in 2019 and military presence at different periods\\label{tab:lm_vox_c1920}",
  add_rows = as.data.frame(rbind(
    c("Province FE", rep("Yes", length(m_vox_c1920)) ))),
  threeparttable = TRUE) %>%
# add_header_above(c(" " = 1, "h1" = 2, "h2" = 2)) %>%
footnote(general = paste(n, "Province FE not shown."),
  threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
kable_styling(latex_options = "hold_position") %>%
save_kable(file = "c1920_models/output/tab_vox_c1920.tex")

modelsummary(
  models = m_vox_c1920_50k,
  output = "latex",
  # vcov = ~prov,
  estimate = "{estimate}{stars}",
  coef_map = coef_recode,
  coef_omit = "prov",
  gof_map = gs,
  title = "VOX support in 2019 and military presence at different periods, in census sections located in cities above 50,000 inhabitants in 2017\\label{tab:lm_vox_c1920_50k}",
  add_rows = as.data.frame(rbind(
    c("Province FE", rep("Yes", length(m_vox_c1920_50k)) ))),
  threeparttable = TRUE) %>%
# add_header_above(c(" " = 1, "h1" = 2, "h2" = 2)) %>%
footnote(general = paste(n, "Province FE not shown."),
  threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
kable_styling(latex_options = "hold_position") %>%
save_kable(file = "c1920_models/output/tab_vox_c1920_50k.tex")

modelsummary(
  models = m_vox_c1920_HQ,
  output = "latex",
  # vcov = ~prov,
  estimate = "{estimate}{stars}",
  coef_map = coef_recode,
  coef_omit = "prov",
  gof_map = gs,
  title = "VOX support in 2019 and military presence at different periods, in census sections located in former military region capitals\\label{tab:lm_vox_c1920_HQ}",
  add_rows = as.data.frame(rbind(
    c("Province FE", rep("Yes", length(m_vox_c1920_HQ)) ))),
  threeparttable = TRUE) %>%
# add_header_above(c(" " = 1, "h1" = 2, "h2" = 2)) %>%
footnote(general = paste(n, "Province FE not shown."),
  threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
kable_styling(latex_options = "hold_position") %>%
save_kable(file = "c1920_models/output/tab_vox_c1920_HQ.tex")

modelsummary(
  models = m_vox_c1920_hinc,
  output = "latex",
  # vcov = ~prov,
  estimate = "{estimate}{stars}",
  coef_map = coef_recode,
  coef_omit = "prov",
  gof_map = gs,
  title = "VOX support in 2019 and military presence at different periods, in the wealthiest census sections ($>q3$)\\label{tab:lm_vox_c1920_hinc}",
  add_rows = as.data.frame(rbind(
    c("Province FE", rep("Yes", length(m_vox_c1920_hinc)) ))),
  threeparttable = TRUE) %>%
# add_header_above(c(" " = 1, "h1" = 2, "h2" = 2)) %>%
footnote(general = paste(n, "Province FE not shown."),
  threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
kable_styling(latex_options = "hold_position") %>%
save_kable(file = "c1920_models/output/tab_vox_c1920_hinc.tex")
