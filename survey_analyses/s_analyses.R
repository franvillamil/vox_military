# setwd("~/Documents/Projects/vox_military")
options(stringsAsFactors = FALSE)
options("modelsummary_format_numeric_latex" = "plain")
# List of packages
pkg = c("haven", "dplyr", "tidyr", "miceadds", "interflex", "modelsummary", "zoo",
  "marginaleffects", "RColorBrewer", "ggplot2", "purrr", "stringr", "kableExtra", "MASS")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
lapply(pkg, library, character.only = TRUE)
# Functions
source("func/se.R")
source("func/raincloud_func.R")
# -------------------------

# "MASS"

## Modelsummary stuff

# Label models (for modelsummary)
modnames = function(l){
  names(l) = paste0("(", 1:length(l), ")")
  return(l)
}

coef_recode = c(
  'mili' = "Military",
  'leftright' = "Ideology (left-right)",
  'mili:leftright' = "Military $\\times$ Ideology",
  'female' = "Gender (female)",
  'age' = "Age",
  'edu_level' = "Education level",
  'working' = "Employed",
  'unemployed' = "Unemployed",
  'religious' = "Religious")

gs = list(
  list("raw" = "nobs", "clean" = "$n$", "fmt" = 0),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1))

n = "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001. All models include survey-clustered SE."
n2 = "Controls: gender, age, education level, religion, employment status. Full table in Appendix."

# -------------------------


## LOADING AND PREPARING
data = read.csv("input_data/barometers_full.csv") %>%
  # From Sept 2018
  mutate(date = as.Date(date)) %>%
  filter(date >= "2018-09-01") %>%
  # Recode some variables
  mutate(
    survey = as.factor(survey),
    municipio = paste0(CCAA, MUN),
    edu_level = recode(estudios,
      "sin_estudios" = 0,
      "primaria" = 1,
      "secundaria1" = 2,
      "secundaria2" = 3,
      "FP" = 4,
      "otros" = 4,
      "superiores" = 5))

## Select surveys with enough variation and subset
excl_dates = c()
for(d in unique(data$date)){
  t = with(data %>% filter(date == d), table(votevox, mili))
  if(any(t == 0)){excl_dates = c(excl_dates, d)}}
data = subset(data, !date %in% excl_dates)


## Save key numbers
# Number of military
fcon = file("survey_analyses/output/mili_number.tex")
writeLines(as.character(sum(data$mili==1, na.rm = TRUE)), fcon)
close(fcon)
# Ideology of military
fcon = file("survey_analyses/output/ideo_military.tex")
writeLines(as.character(round(mean(data$leftright[data$mili == 1], na.rm = TRUE), 2)), fcon)
close(fcon)
# Ideology of civilians
fcon = file("survey_analyses/output/ideo_civilians.tex")
writeLines(as.character(round(mean(data$leftright[data$mili == 0], na.rm = TRUE), 2)), fcon)
close(fcon)

## DESCRIPTIVES

# Table - summary statistics
data_desc = data %>%
  dplyr::select(
    `Vote VOX` = votevox,
    `Military` = mili,
    `Ideology (left-right)` = leftright,
    `Gender (female)` = female,
    `Age` = age,
    `Education level` = edu_level,
    `Employed` = working,
    `Unemployed` = unemployed,
    `Religious` = religious)

datasummary_skim(data_desc,
  title = "Survey data descriptive statistics\\label{tab:survey_desc}",
  histogram = FALSE,
  output = "survey_analyses/output/descriptives.tex")
# kable_styling(latex_options = "hold_position") %>%
# save_kable(file = "survey_analyses/output/descriptives.tex")
system("sed -i '' 's/egin{table}/egin{table}[!h]/g' survey_analyses/output/descriptives.tex")

# Plot, over time
group_mean = data %>%
  group_by(mili, date) %>%
  summarize(vox = mean(votevox)) %>%
  mutate(military = recode(mili, "0" = "Non-military", "1" = "Military"))

pdf("survey_analyses/output/group_mean_overtime.pdf", width = 6, height = 3.5)
ggplot(group_mean, aes(x = date, y = vox, group = military, color = military)) +
  geom_vline(xintercept = as.Date("2019-04-29"), linetype = "dashed", color = "gray") +
  geom_smooth(size = 0, method = "loess", alpha = 0.2, show.legend = FALSE) +
  geom_line(stat = "smooth", method = "loess", formula = y ~ x, alpha = 0.5) +
  geom_point() +
  labs(x = "", y = "Average VOX support") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_classic() +
  theme(panel.background = element_blank(),
        legend.position = c(0.03, 0.8), legend.justification = c(0,0),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  scale_color_manual(values = c("#0e80d8", "#31A354")) +
  annotate(geom = "text", x = as.Date("2019-06-20"), y = .5, size = 2.5,
    color = "gray57", fontface = 2, label = "April 2019\nelection")
dev.off()

# Tables
data_cat_all = data %>%
  filter(!is.na(mili)) %>%
  group_by(mili) %>%
  summarize(
    total = n(),
    vox = mean(votevox)) %>%
  mutate(leftright_cat = "0all")

data_cat = data %>%
  mutate(leftright_cat = case_when(
    is.na(leftright) ~ "4DK",
    leftright %in% 1:3 ~ "1left",
    leftright %in% 4:6 ~ "2center",
    leftright %in% 7:10 ~ "3right")) %>%
  filter(!is.na(leftright_cat) & !is.na(mili)) %>%
  group_by(mili, leftright_cat) %>%
  summarize(
    total = n(),
    vox = mean(votevox)) %>%
  bind_rows(data_cat_all) %>%
  arrange(mili, leftright_cat)

row_m0 = round(data_cat$vox[data_cat$mili == 0]*100, 1)
row_m1 = round(data_cat$vox[data_cat$mili == 1]*100, 1)
row_totalm0 = data_cat$total[data_cat$mili == 0]
row_totalm1 = data_cat$total[data_cat$mili == 1]
row_total = prettyNum(paste(row_totalm0 + row_totalm1, collapse = " & "), big.mark = ",")
row_m0 = paste(paste0(row_m0, "\\%"), collapse = " & ")
row_m1 = paste(paste0(row_m1, "\\%"), collapse = " & ")
row_totalm0 = paste(paste0("(", prettyNum(row_totalm0, big.mark = ","), ")"), collapse = " & ")
row_totalm1 = paste(paste0("(", prettyNum(row_totalm1, big.mark = ","), ")"), collapse = " & ")

data_cat$total = prettyNum(data_cat$total, big.mark=",")

# Military gap in support for VOX (table)
freq = c(
  "\\begin{table}[!htbp] \\centering",
  paste0("\\caption{Support for VOX depending on being on the military and ideology}"),
  paste0("\\label{tab:surveyfreq}"),
  "\\small",
  "\\begin{tabular}{lccccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  "& All & Leftist & Center & Rightist & DK/NA \\\\",
  "\\cline{2-6} \\\\[-1.8ex]",
  paste0("Military & ", row_m1, " \\\\"),
  paste0(" & ", row_totalm1, " \\\\"),
  paste0("Non-military & ", row_m0, " \\\\"),
  paste0(" & ", row_totalm0, " \\\\"),
  "\\cline{2-6} \\\\[-1.8ex]",
  paste0("$n$ & ", row_total, " \\\\"),
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  "\\multicolumn{6}{c}{\\parbox[t]{0.75\\textwidth}{\\textit{Note:} Percentages refer to the share of individuals who report voting for VOX, respective to the whole sample. Self-reported ideology (1-10 scale): 1-3 = left, 4-6 = center, 7-10 = right. DK/NA = Don't know / Non response.}}\\\\",
  "\\end{tabular}",
  "\\end{table}")

fcon = file("survey_analyses/output/tab_survey_freq.tex")
writeLines(paste0(freq), fcon)
close(fcon)

# Rainplot, left-right ideology
pdf("survey_analyses/output/rainplot_mili_leftright.pdf", width = 4, height = 4.5)
ggplot(data, aes(x = factor(mili), y = leftright)) +#, fill = factor(mili))) +
  geom_flat_violin(position = position_nudge(x = .25, y = 0),
    adjust = 2, trim = FALSE, color = "#00000000", fill = gray(0.75)) +
  geom_point(shape = ".", position = position_jitter(width = .15), size = .25, alpha = 0.25) +
  geom_boxplot(aes(x = factor(mili), y = leftright),
    position = position_nudge(x = .25, y = 0),
    outlier.shape = NA, alpha = 0.3, width = .1, colour = "black") +
  theme_classic() +
  # theme(legend.position = "none") +
  # scale_fill_manual(values = c("#2c7fb8", "#2ca25f")) +
  scale_x_discrete(labels = c("Not military", "Military personnel")) +
  labs(x = "", y = "Self-reported ideology (left-right)")
dev.off()

## MODELS

# Models
m1 = glm(votevox ~ mili +
  factor(survey) + factor(municipio),
  data = data, family = "binomial")

m2 = glm(votevox ~ mili +
  female + age + edu_level + working + unemployed + religious +
  factor(survey) + factor(municipio),
  data = data, family = "binomial")

m3 = glm(votevox ~ mili + leftright +
  female + age + edu_level + working + unemployed + religious +
  factor(survey) + factor(municipio),
  data = data, family = "binomial")

m4 = glm(votevox ~ mili * leftright +
  female + age + edu_level + working + unemployed + religious +
  factor(survey) + factor(municipio),
  data = data, family = "binomial")

mlist = modnames(list(m1, m2, m3, m4))

modelsummary(
  models = mlist,
  output = "latex",
  vcov = ~survey,
  estimate = "{estimate}{stars}",
  coef_rename = coef_recode,
  coef_omit = "survey|municipio|female|age|edu_level|working|unemployed|religious",
  gof_map = gs,
  title = "Individual-level analyses on VOX support\\label{tab:survey_glm}",
  add_rows = as.data.frame(rbind(
    c("Controls", "No", rep("Yes", length(mlist)-1)),
    c("Survey FE", rep("Yes", length(mlist))),
    c("Municipality FE", rep("Yes", length(mlist)))
    )),
  threeparttable = TRUE, escape = FALSE) %>%
# add_header_above(c(" " = 1, "h1" = 2, "h2" = 2)) %>%
footnote(general = paste(n, n2),
  threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
kable_styling(latex_options = "hold_position") %>%
save_kable(file = "survey_analyses/output/tab_survey_models.tex")

modelsummary(
  models = mlist,
  output = "latex",
  vcov = ~survey,
  estimate = "{estimate}{stars}",
  coef_rename = coef_recode,
  coef_omit = "survey|municipio",
  gof_map = gs,
  title = "Individual-level analyses on VOX support\\label{tab:survey_glm}",
  add_rows = as.data.frame(rbind(
    c("Survey FE", rep("Yes", length(mlist))),
    c("Municipality FE", rep("Yes", length(mlist)))
    )),
  threeparttable = TRUE, escape = FALSE) %>%
# add_header_above(c(" " = 1, "h1" = 2, "h2" = 2)) %>%
footnote(general = n,
  threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
kable_styling(latex_options = "hold_position") %>%
save_kable(file = "survey_analyses/output/tab_survey_models_full.tex")

## MARGINAL EFFECTS

# AMEs of different models
ame2 = marginaleffects(m2, variables = "mili")
ame3 = marginaleffects(m3, variables = "mili")
ame2_t = as.character(round(tidy(ame2)$estimate, 3))
ame3_t = as.character(round(tidy(ame3)$estimate, 3))


f = file("survey_analyses/output/AME_m2_without_ideo.tex")
writeLines(ame2_t, f)
close(f)
f = file("survey_analyses/output/AME_m3_with_ideo.tex")
writeLines(ame3_t, f)
close(f)

## SIMULATIONS

# Prepare simulation
# Coefficients and matrix with main variables
coefs = m4$coefficients
mat = expand.grid(mili = 0:1, leftright = 1:10)
mat$mili_x_leftright = mat$mili * mat$leftright
mat = cbind(mat, data.frame(
  female = 0,
  age = mean(data$age, na.rm = TRUE),
  unemployed = 0,
  working = 1,
  religious = 1,
  edu_level = 5))
# Add intercept to X matrix
mat$Intercept = 1
# Add fixed effects (keep it at ref category)
fe = names(coefs)[grepl("factor\\(", names(coefs))]
mat = cbind(mat, matrix(0, ncol = length(fe), dimnames = list(NULL, fe)))
# Change names
names(mat)[names(mat) == "Intercept"] = "(Intercept)"
names(mat) = gsub("_x_", ":", names(mat))
# Check that all variables are there and order them
if(!all(names(coefs) %in% names(mat))){stop("Missing variables in X matrix")}
mat = mat[,match(names(coefs), names(mat))]

# Simulate
# VCOV matrix
covmat = vcov(m4)
# Random draws of coefficients
betadraw = mvrnorm(n = 1000, mu = coefs, Sigma = covmat)
# Create list of point estimates and add them from sims
est = vector("list", nrow(mat))
# Inverse logit function
for(i in 1:nrow(mat)){est[[i]] = 1 / (1 + exp(-(betadraw %*% as.numeric(mat[i,]))))}
# Labels for each scenario
names(est) = paste0("mili", mat[, "mili"], "_ideo", mat[, "leftright"])

# Summaries
mili_gap_est = vector("list", 10)
names(mili_gap_est) = paste0("ideo", 1:10)
for(i in 1:10){
  mili_gap_est[[i]] = est[[paste0("mili1_ideo", i)]] - est[[paste0("mili0_ideo", i)]]
  }
mili_gap_ratio = vector("list", 10)
names(mili_gap_ratio) = paste0("ideo", 1:10)
for(i in 1:10){
  mili_gap_ratio[[i]] = est[[paste0("mili1_ideo", i)]] / est[[paste0("mili0_ideo", i)]]
  }
# Mean and 95 CI
mili_gap = data.frame(
  ideo = as.numeric(gsub("ideo", "", names(mili_gap_est))),
  mean = map_dbl(mili_gap_est, mean),
  lwr = map_dbl(mili_gap_est, quantile, 0.025),
  upr = map_dbl(mili_gap_est, quantile, 0.975))
mili_gap_ratio = data.frame(
  ideo = as.numeric(gsub("ideo", "", names(mili_gap_ratio))),
  mean = map_dbl(mili_gap_ratio, mean),
  lwr = map_dbl(mili_gap_ratio, quantile, 0.025),
  upr = map_dbl(mili_gap_ratio, quantile, 0.975))

dsum = data %>%
  filter(!is.na(leftright)) %>%
  group_by(leftright) %>%
  summarize(n = n()) %>%
  mutate(s = n/sum(n))

dsum2 = data %>%
  filter(!is.na(leftright)) %>%
  group_by(leftright, mili) %>%
  summarize(n = n()) %>%
  group_by(mili) %>%
  mutate(s = n/sum(n))

pdf("survey_analyses/output/sim_mil_gap_ideo.pdf", width = 5.5, height = 3)
ggplot(mili_gap, aes(x = factor(ideo), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1) +
  labs(x = "Self-reported ideology (left-right)",
    y = "Difference in VOX support between\nmilitary and non-military individuals") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_classic() +
  theme(panel.background = element_blank(),
        legend.position = c(0.05, 0.8), legend.justification = c(0,0),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  geom_segment(data = dsum, size = 15, show.legend = FALSE,
    aes(x = leftright, xend = leftright, y = -0.07, yend = (s/5-0.07))) +
  # geom_segment(data = subset(dsum2, mili==0), color = "#0e80d8", size = 5, show.legend = FALSE,
  #   aes(x = leftright-0.16, xend = leftright-0.16, y = -0.07, yend = (s/5-0.07))) +
  # geom_segment(data = subset(dsum2, mili==1), color = "#31A354", size = 5, show.legend = FALSE,
  #   aes(x = leftright+0.16, xend = leftright+0.16, y = -0.07, yend = (s/5-0.07))) +
  scale_y_continuous(limits = c(-0.07, max(mili_gap$upr) + 1e-05))
dev.off()


pdf("survey_analyses/output/sim_mil_gap_ideo_ratio.pdf", width = 5.5, height = 3.25)
ggplot(mili_gap_ratio, aes(x = factor(ideo), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1) +
  labs(x = "Self-reported ideology (left-right)",
    y = "Military/non-military ratio in VOX support") +
  theme_classic() +
  theme(panel.background = element_blank(),
        legend.position = c(0.05, 0.8), legend.justification = c(0,0),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank())
dev.off()

### EXTRAS

m4_w = glm(votevox ~ mili * leftright +
  age + edu_level + working + unemployed + religious +
  factor(survey) + factor(municipio),
  data = subset(data, female == 1), family = "binomial")


## GENDER SUBSETS

# Models
m1_w = glm(votevox ~ mili +
  factor(survey) + factor(municipio),
  data = subset(data, female == 1), family = "binomial")

m2_w = glm(votevox ~ mili +
  age + edu_level + working + unemployed + religious +
  factor(survey) + factor(municipio),
  data = subset(data, female == 1), family = "binomial")

m3_w = glm(votevox ~ mili + leftright +
  age + edu_level + working + unemployed + religious +
  factor(survey) + factor(municipio),
  data = subset(data, female == 1), family = "binomial")

m4_w = glm(votevox ~ mili * leftright +
  age + edu_level + working + unemployed + religious +
  factor(survey) + factor(municipio),
  data = subset(data, female == 1), family = "binomial")

# Table
mlist_w = modnames(list(m1_w, m2_w, m3_w, m4_w))

modelsummary(
  models = mlist_w,
  output = "latex",
  vcov = ~survey,
  estimate = "{estimate}{stars}",
  coef_rename = coef_recode,
  coef_omit = "survey|municipio",
  gof_map = gs,
  title = "Individual-level analyses on VOX support, women subsample\\label{tab:survey_glm_full_women}",
  add_rows = as.data.frame(rbind(
    c("Survey FE", rep("Yes", length(mlist_w))),
    c("Municipality FE", rep("Yes", length(mlist_w)))
    )),
  threeparttable = TRUE, escape = FALSE) %>%
# add_header_above(c(" " = 1, "h1" = 2, "h2" = 2)) %>%
footnote(general = n,
  threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
kable_styling(latex_options = "hold_position") %>%
save_kable(file = "survey_analyses/output/tab_survey_models_full_women.tex")

# Models
m1_m = glm(votevox ~ mili +
  factor(survey) + factor(municipio),
  data = subset(data, female == 0), family = "binomial")

m2_m = glm(votevox ~ mili +
  age + edu_level + working + unemployed + religious +
  factor(survey) + factor(municipio),
  data = subset(data, female == 0), family = "binomial")

m3_m = glm(votevox ~ mili + leftright +
  age + edu_level + working + unemployed + religious +
  factor(survey) + factor(municipio),
  data = subset(data, female == 0), family = "binomial")

m4_m = glm(votevox ~ mili * leftright +
  age + edu_level + working + unemployed + religious +
  factor(survey) + factor(municipio),
  data = subset(data, female == 0), family = "binomial")

# Table
mlist_m = modnames(list(m1_m, m2_m, m3_m, m4_m))

modelsummary(
  models = mlist_m,
  output = "latex",
  vcov = ~survey,
  estimate = "{estimate}{stars}",
  coef_rename = coef_recode,
  coef_omit = "survey|municipio",
  gof_map = gs,
  title = "Individual-level analyses on VOX support, men subsample\\label{tab:survey_glm_full_men}",
  add_rows = as.data.frame(rbind(
    c("Survey FE", rep("Yes", length(mlist_m))),
    c("Municipality FE", rep("Yes", length(mlist_m)))
    )),
  threeparttable = TRUE, escape = FALSE) %>%
# add_header_above(c(" " = 1, "h1" = 2, "h2" = 2)) %>%
footnote(general = n,
  threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
kable_styling(latex_options = "hold_position") %>%
save_kable(file = "survey_analyses/output/tab_survey_models_full_men.tex")

## STABILITY OVER TIME AND TIME TRENDS

# (data from 2016 to 2019 A elections)
data_16_19 = read.csv("input_data/barometers_full.csv") %>%
  # From Sept 2018
  mutate(date = as.Date(date)) %>%
  # filter(date <= "2019-12-01") %>%
  # Recode some variables
  mutate(
    votecs = ifelse(partyvote == "Cs", 1, 0),
    votepsoe = ifelse(partyvote == "PSOE", 1, 0),
    survey = as.factor(survey),
    municipio = paste0(CCAA, MUN),
    edu_level = recode(estudios,
      "sin_estudios" = 0,
      "primaria" = 1,
      "secundaria1" = 2,
      "secundaria2" = 3,
      "FP" = 4,
      "otros" = 4,
      "superiores" = 5))

m_date = bind_rows(
  crossing(date = unique(data_16_19$date[data_16_19$date >= "2018-09-01"])) %>%
    mutate(model = map(.x = date,
      .f = ~ broom::tidy(glm(votevox ~ mili,
        data = filter(data_16_19, date == .x))))) %>%
    unnest(model) %>%
    filter(term == "mili") %>%
    mutate(controls = "No controls"),
  crossing(date = unique(data_16_19$date[data_16_19$date >= "2018-09-01"])) %>%
    mutate(model = map(.x = date,
      .f = ~ broom::tidy(glm(votevox ~ mili + leftright +
        female + age + edu_level + working + unemployed + religious,
        data = filter(data_16_19, date == .x))))) %>%
    unnest(model) %>%
    filter(term == "mili") %>%
    mutate(controls = "With controls")) %>%
  rename(se = std.error, est = estimate) %>%
  as.data.frame() %>%
  mutate(
    upr = est + se * qnorm(0.975),
    lwr = est + se * qnorm(0.025))

pdf("survey_analyses/output/models_VOX_by_date.pdf", width = 7.5, height = 3)
ggplot(m_date, aes(x = date, y = est, ymin = lwr, ymax = upr)) +
  geom_errorbar(width = 0) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = "Effect of military on vox vote + 95% CI", x = "") +
  # scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("grey", "black")) +
  geom_vline(xintercept = 4.5, linetype = "dotted") +
  geom_vline(xintercept = 9.5, linetype = "dotted") +
  facet_wrap(~controls)
dev.off()

## Linearity assumptions (Hainmueller et al. 2019)

interflex = interflex(
  estimator = "binning",
  method = "logit",
  data = data,
  na.rm = TRUE,
  Y = "votevox",
  D = "mili",
  ylab = "ME of being in the military on VOX support",
  X = "leftright",
  Xlabel = "Ideological self-placement",
  xlab = "Ideological self-placement",
  Z = c("female", "age", "edu_level", "working", "unemployed", "religious"),
  theme.bw = TRUE,
  file = "survey_analyses/output/interaction_lin.pdf",
  height = 4, width = 7)

## Looking at longitudinal trends and PP vote


# ## Just in case: Select surveys with enough variation and subset
# excl_dates_16_19 = c()
# for(d in unique(data_16_19$date)){
#   t = with(data_16_19 %>% filter(date == d), table(votepp, mili))
#   if(any(t == 0)){excl_dates_16_19 = c(excl_dates_16_19, d)}}
# data_16_19 = subset(data_16_19, !date %in% excl_dates_16_19)

# Models
m1_PP = glm(votepp ~ mili + leftright +
  female + age + edu_level + working + unemployed + religious +
  factor(survey) + factor(municipio),
  data = subset(data_16_19, date < "2017-10-01"),
  family = "binomial")
m2_PP = glm(votepp ~ mili + leftright +
  female + age + edu_level + working + unemployed + religious +
  factor(survey) + factor(municipio),
  data = subset(data_16_19, date >= "2017-10-01" & date < "2018-12-01"),
  family = "binomial")
m3_PP = glm(votepp ~ mili + leftright +
  female + age + edu_level + working + unemployed + religious +
  factor(survey) + factor(municipio),
  data = subset(data_16_19, date >= "2018-12-01" & date <= "2019-12-01"),
  family = "binomial")


mlist_PP = modnames(list(m1_PP, m2_PP, m3_PP))

extran = "The three models are run on subsamples including all surveys run before the Catalan referendum and unilateral declaration of independence in October 2017 (1), between this moment and the unexpected result of VOX in the Andalusia regional elections in December 2018 (2), and after it (3)."

modelsummary(
  models = mlist_PP,
  output = "latex",
  vcov = ~survey,
  estimate = "{estimate}{stars}",
  coef_rename = coef_recode,
  coef_omit = "survey|municipio",
  gof_map = gs,
  title = "Individual-level analyses on PP support, 2016--2019\\label{tab:survey_glm_PP}",
  add_rows = as.data.frame(rbind(
    c("Survey FE", rep("Yes", length(mlist_PP))),
    c("Municipality FE", rep("Yes", length(mlist_PP)))
    )),
  threeparttable = TRUE, escape = FALSE) %>%
add_header_above(c(" " = 1, "Pre Catalonia" = 1, "Oct 17 - Nov 18" = 1, "Post Andalucia" = 1)) %>%
footnote(general = paste(n, extran),
  threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
kable_styling(latex_options = "hold_position") %>%
save_kable(file = "survey_analyses/output/tab_survey_models_PP.tex")



# Modelos por trimestre
data_16_19$Q = as.yearqtr(data_16_19$date)
qlist = crossing(quarter = unique(data_16_19$Q[data_16_19$date <= "2019-12-01"]))

# PP
m_quarters = bind_rows(
  qlist %>%
    mutate(model = map(.x = quarter,
      .f = ~ broom::tidy(glm(votepp ~ mili + leftright +
        female + age + edu_level + working + unemployed + religious +
        factor(survey) + factor(municipio),
        data = filter(data_16_19, Q == .x))))) %>%
    unnest(model) %>%
    filter(term == "mili") %>%
    mutate(q = format(quarter, "%Y-0%q")) %>%
    rename(se = std.error, est = estimate) %>%
    as.data.frame() %>%
    mutate(
      upr = est + se * qnorm(0.975),
      lwr = est + se * qnorm(0.025),
      party = "PP",
      q = gsub(" ", "", as.character(quarter))),
  qlist %>%
    mutate(model = map(.x = quarter,
      .f = ~ broom::tidy(glm(votevox ~ mili + leftright +
        female + age + edu_level + working + unemployed + religious +
        factor(survey) + factor(municipio),
        data = filter(data_16_19, Q == .x))))) %>%
    unnest(model) %>%
    filter(term == "mili") %>%
    mutate(q = format(quarter, "%Y-0%q")) %>%
    rename(se = std.error, est = estimate) %>%
    as.data.frame() %>%
    mutate(
      upr = est + se * qnorm(0.975),
      lwr = est + se * qnorm(0.025),
      party = "VOX",
      q = gsub(" ", "", as.character(quarter))) %>%
    mutate(
      est = ifelse(quarter < "2018 Q4", NA, est),
      upr = ifelse(quarter < "2018 Q4", NA, upr),
      lwr = ifelse(quarter < "2018 Q4", NA, lwr))
)

# Plot
pdf("survey_analyses/output/models_quarter.pdf", width = 8, height = 3)
ggplot(m_quarters, aes(x = q, y = est, ymin = lwr, ymax = upr, color = party)) +
  geom_errorbar(position = position_dodge(1/4), width = 0) +
  geom_point(position = position_dodge(1/4)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = "Effect of being in the military\non support (incl 95% CI)", x = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 4.5, linetype = "dotted") +
  geom_vline(xintercept = 9.5, linetype = "dotted") +
  scale_color_manual(name = "", values = c("#0e80d8", "#31A354")) # +
  # annotate("text", x = 9.75, y = 0.15, label = "", size = 3)
dev.off()
