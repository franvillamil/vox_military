# setwd("~/Documents/Projects/vox_military_jop")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("tidyr", "dplyr", "haven", "ggplot2", "modelsummary", "kableExtra")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
lapply(pkg, library, character.only = TRUE)

# ------------------------------------------

# Label models (for modelsummary)
modnames = function(l){
  names(l) = paste0("(", 1:length(l), ")")
  return(l)
}

# ------------------------------------------

# Unzip, read, disconnect
tmp = tempdir()
unzip("input_data/ZA2391_v13-0-0.dta.zip", exdir = tmp)
data = read_dta(file.path(tmp, "ZA2391_v13-0-0.dta"))
unlink(tmp)

# Mutate and select
df = data %>%
  filter(v4 >= 2013) %>%
  rename(
    month = v3,
    year = v4,
    voteint = v6,
    partyattach = v72,
    age_cat = v56,
    religion = v52,
    ideology = v22,
    gender = v54) %>%
  mutate(ideology = ifelse(ideology == 99, NA, ideology-1)) %>%
  mutate(christian = ifelse(religion %in% 1:2, 1, 0)) %>%
  mutate(age_cat = ifelse(age_cat == 99, NA, age_cat)) %>%
  mutate(age_cat2 = ifelse(age_cat %in% 1:5, 1, 2)) %>%
  mutate(age_cat2 = ifelse(age_cat %in% 9:11, 3, age_cat2)) %>%
  mutate(female = gender - 1) %>%
  mutate(survey = paste0(month, "-", year)) %>%
  mutate(soldat = ifelse(v65 == 14, 1, 0)) %>%
  mutate(AfD_int = ifelse(voteint == 49, 1, 0)) %>%
  mutate(AfD_att = ifelse(partyattach == 11, 1, 0))

## PLOTS

# Aggregate by year and group
year_agg = df %>%
  group_by(soldat, year) %>%
  summarize(
    n = n(),
    AfD = sum(AfD_int)/length(AfD_int),
    AfD_att = sum(AfD_att)/length(AfD_att)) %>%
  mutate(year = as.integer(year)) %>%
  mutate(mil = ifelse(soldat == 1, "Military", "Non-military"))

# Plot
pdf("politbarometer/output/vote_int_AfD.pdf", width = 5, height = 3)
ggplot(year_agg,
    aes(x = year, y = AfD, color = factor(mil))) +
  geom_point() +
  geom_line() +
  labs(y = "% Vote intention to AfD", x = "", color = "") +
  scale_color_manual(values = c("#31A354", "#0e80d8")) +
  theme_classic() +
  theme(legend.position = "bottom")
dev.off()

# Plot
pdf("politbarometer/output/attach_AfD.pdf", width = 5, height = 3)
ggplot(year_agg %>% filter(year >= 2016),
    aes(x = year, y = AfD_att, color = factor(mil))) +
  geom_point() +
  geom_line() +
  labs(y = "% Feeling attached to AfD", x = "", color = "") +
  scale_color_manual(values = c("#31A354", "#0e80d8")) +
  theme_classic() +
  theme(legend.position = "bottom")
dev.off()

## MODELS

# Formulae
controls = paste(c("factor(age_cat2)", "female"), collapse = " + ")
f_int = paste0("AfD_int ~ soldat + factor(survey)", " + ", controls)
f_att = paste0("AfD_att ~ soldat + factor(survey)", " + ", controls)

# Years
yr = c(list(as.integer(unique(df$year))), as.list(as.integer(unique(df$year))))
names(yr) = c("Pooled", as.character(unique(df$year)))
yr_att = yr[c("Pooled", as.character(2016:2020))]

# Model list
m_int = modnames(lapply(yr, function(x)
  glm(as.formula(f_int), data = subset(df, year %in% x), family = "binomial")))
m_att = modnames(lapply(yr_att, function(x)
  glm(as.formula(f_att), data = subset(df, year %in% x), family = "binomial")))

## TABLES

# Modelsummary stuff

coef_recode = c(
  'soldat' = 'Military',
  "female" = "Female",
  "factor(age_cat2)2" = "Age 40-59",
  "factor(age_cat2)3" = "Age 60+")

gs = list(
  list("raw" = "nobs", "clean" = "$n$", "fmt" = 0),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1))

n = "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001. Military variable refers to ISCO occupation groups 0. Survey FE not shown. Source data: Politbarometer, 2013--2020."

header_int = c("", rep(1, length(yr)))
names(header_int) = c("", names(yr))
header_att = c("", rep(1, length(yr_att)))
names(header_att) = c("", names(yr_att))

modelsummary(
  models = m_int,
  output = "latex",
  estimate = "{estimate}{stars}",
  coef_map = coef_recode,
  coef_omit = "survey",
  gof_map = gs,
  title = "Military occupation and vote intention for AfD in Germany\\label{tab:glm_politbaro_int}",
  add_rows = as.data.frame(rbind(
    c("Survey FE", rep("Yes", length(m_int)) ))),
  threeparttable = TRUE) %>%
add_header_above(header_int) %>%
kable_styling(latex_options = c("scale_down", "hold_position")) %>%
footnote(general = n, threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
save_kable(file = "politbarometer/output/tab_politbaro_int.tex")

modelsummary(
  models = m_att,
  output = "latex",
  estimate = "{estimate}{stars}",
  coef_map = coef_recode,
  coef_omit = "survey",
  gof_map = gs,
  title = "Military occupation and party attachment for AfD in Germany\\label{tab:glm_politbaro_att}",
  add_rows = as.data.frame(rbind(
    c("Survey FE", rep("Yes", length(m_att)) ))),
  threeparttable = TRUE) %>%
add_header_above(header_att) %>%
kable_styling(latex_options = "hold_position") %>%
footnote(general = paste(n, "Models start in 2016 because that is the year when AfD starts to appear in party attachment questions."), threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
save_kable(file = "politbarometer/output/tab_politbaro_att.tex")

# library(labelled)
# varnames = data.frame(n = names(data), label = NA)
#
# for(n in seq_along(names(data))){
# varnames[n, 2] = as.character(var_label(data[,n]))
# }
