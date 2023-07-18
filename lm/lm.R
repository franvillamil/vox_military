# setwd("~/Documents/Projects/cuarteles_militares")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("stargazer", "dplyr", "ggplot2", "broom")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
lapply(pkg, library, character.only = TRUE)
# Function
source("func/my_stargazer.R")
# -------------------------

# Load data, remove non-existing secciones
data = read.csv("create_dataset/output/dataset.csv") %>%
  filter(!is.na(pop_2017))

## Models using army location

m_base1 = lm(VOX_a19 ~ army +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = data)
m_base2 = lm(VOX_a19 ~ army +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, min_dist_army < 20000))
m_base3 = lm(VOX_a19 ~ army +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, cities50k == 1))
m_base4 = lm(VOX_a19 ~ army +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + factor(prov),
  data = subset(data, region_militar_franc == 1))
m_base5 = lm(VOX_a19 ~ army +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, renta_h_2017 > quantile(data$renta_h_2017, 0.75, na.rm=T)))

my_stargazer(dest_file = "lm/output/tab_base.tex",
  model_list = list(m_base1, m_base2, m_base3, m_base4, m_base5),
  omit = c("prov", "part_a19", "pop_2017_l", "renta_h_2017_l",
    "muni_pop_2017_l", "region_militar_franc"),
  label = "tab:lm_base",
  title = "Support for VOX and military presence",
  order = c("Constant", "army", "part_a19", "pop_2017_l",
    "renta_h_2017_l", "muni_pop_2017_l", "region_militar_franc"),
  covariate.labels = c("(Intercept)", "Military facility",
  "Turnout (Apr 2019)", "(Log) Population", "(Log) Household income",
  "(Log) Municipality pop.", "Military region HQ"),
  notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Models 1 includes full sample. Model 2 includes only sections within 20km of military facilities. Model 3 includes only municipalities with more than 50,000 inhabitants in 2017. Model 4 only includes municipalities that were HQ of main military regions. Model 5 restricts the sample to the wealthiest sections ($>q3$). Controls: turnout, logged section population, logged mean household income, logged municipality population, dummy for military region capital. Full table in Appendix.}")

my_stargazer(dest_file = "lm/output/tab_base_full.tex",
  model_list = list(m_base1, m_base2, m_base3, m_base4, m_base5),
  omit = c("prov"),
  label = "tab:lm_base_full",
  title = "Support for VOX and military presence",
  order = c("Constant", "army", "part_a19", "pop_2017_l",
    "renta_h_2017_l", "muni_pop_2017_l", "region_militar_franc"),
  covariate.labels = c("(Intercept)", "Military facility",
  "Turnout (Apr 2019)", "(Log) Population", "(Log) Household income",
  "(Log) Municipality pop.", "Military region HQ"),
  add_lines = list(c("Province FE", rep("\\multicolumn{1}{c}{Yes}", 5))),
  notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Models 1 includes full sample. Model 2 includes only sections within 20km of military facilities. Model 3 includes only municipalities with more than 50,000 inhabitants in 2017. Model 4 only includes municipalities that were HQ of main military regions. Model 5 restricts the sample to the wealthiest sections ($>q3$).}")

## November elections

m_base_nov1 = lm(VOX_n19 ~ army +
  part_n19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = data)
m_base_nov2 = lm(VOX_n19 ~ army +
  part_n19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, min_dist_army < 20000))
m_base_nov3 = lm(VOX_n19 ~ army +
  part_n19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, cities50k == 1))
m_base_nov4 = lm(VOX_n19 ~ army +
  part_n19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + factor(prov),
  data = subset(data, region_militar_franc == 1))
m_base_nov5 = lm(VOX_n19 ~ army +
  part_n19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, renta_h_2017 > quantile(data$renta_h_2017, 0.75, na.rm=T)))

my_stargazer(dest_file = "lm/output/tab_base_full_nov.tex",
  model_list = list(m_base_nov1, m_base_nov2, m_base_nov3, m_base_nov4, m_base_nov5),
  omit = c("prov"),
  label = "tab:lm_base_nov_full",
  title = "Support for VOX (November elections) and military presence",
  order = c("Constant", "army", "part_n19", "pop_2017_l",
    "renta_h_2017_l", "muni_pop_2017_l", "region_militar_franc"),
  covariate.labels = c("(Intercept)", "Military facility",
  "Turnout (Nov 2019)", "(Log) Population", "(Log) Household income",
  "(Log) Municipality pop.", "Military region HQ"),
  add_lines = list(c("Province FE", rep("\\multicolumn{1}{c}{Yes}", 5))),
  notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Models 1 includes full sample. Model 2 includes only sections within 20km of military facilities. Model 3 includes only municipalities with more than 50,000 inhabitants in 2017. Model 4 only includes municipalities that were HQ of main military regions. Model 5 restricts the sample to the wealthiest sections ($>q3$).}")

## Models using army location & interaction con renta

m_int1 = lm(VOX_a19 ~ army * renta_h_2017_l +
  part_a19 + pop_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = data)
m_int2 = lm(VOX_a19 ~ army * renta_h_2017_l +
  part_a19 + pop_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, min_dist_army < 20000))
m_int3 = lm(VOX_a19 ~ army * renta_h_2017_l +
  part_a19 + pop_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, cities50k == 1))
m_int4 = lm(VOX_a19 ~ army * renta_h_2017_l +
  part_a19 + pop_2017_l + muni_pop_2017_l + factor(prov),
  data = subset(data, region_militar_franc == 1))
m_int4 = lm(VOX_a19 ~ army * renta_h_2017_l +
  part_a19 + pop_2017_l + muni_pop_2017_l + factor(prov),
  data = subset(data, region_militar_franc == 1))

my_stargazer(dest_file = "lm/output/tab_base_int.tex",
  list(m_int1, m_int2, m_int3, m_int4),
  omit = c("prov", "part_a19", "pop_2017_l",
    "muni_pop_2017_l", "region_militar_franc"),
  label = "tab:lm_base_int",
  title = "Support for VOX and military presence",
  order = c("Constant", "army$", "renta_h_2017_l", "^army:",
    "part_a19", "pop_2017_l", "muni_pop_2017_l", "region_militar_franc"),
  covariate.labels = c("(Intercept)", "Military facility",
    "(Log) Household income", "Military $\\times$ Income",
    "Turnout (Apr 2019)", "(Log) Population",
    "(Log) Municipality pop.", "Military region HQ"),
  notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Models 1 includes full sample. Model 2 includes only sections within 20km of military facilities. Model 3 includes only municipalities with more than 50,000 inhabitants in 2017. Model 4 only includes municipalities that were HQ of main military regions. Controls: turnout, logged section population, logged municipality population, dummy for military region capital. Full table in Appendix.}")

my_stargazer(dest_file = "lm/output/tab_base_int_full.tex",
  list(m_int1, m_int2, m_int3, m_int4),
  omit = c("prov"),
  label = "tab:lm_base_int_full",
  title = "Support for VOX and military presence",
  order = c("Constant", "army$", "renta_h_2017_l", "^army:",
    "part_a19", "pop_2017_l", "muni_pop_2017_l", "region_militar_franc"),
  covariate.labels = c("(Intercept)", "Military facility",
    "(Log) Household income", "Military $\\times$ Income",
    "Turnout (Apr 2019)", "(Log) Population",
    "(Log) Municipality pop.", "Military region HQ"),
  add_lines = list(c("Province FE", rep("\\multicolumn{1}{c}{Yes}", 4))),
  notes_table = "\\parbox[t]{0.75\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Models 1 includes full sample. Model 2 includes only sections within 20km of military facilities. Model 3 includes only municipalities with more than 50,000 inhabitants in 2017. Model 4 only includes municipalities that were HQ of main military regions.}")

# Predicted probabilities plot

nd = expand.grid(army = 0:1, renta_h_2017_l = seq(8.5, 11, 0.25))
nd$part_a19 = mean(data$part_a19, na.rm=T)
nd$pop_2017_l = mean(data$pop_2017_l, na.rm=T)
nd$muni_pop_2017_l = mean(data$muni_pop_2017_l, na.rm=T)
nd$region_militar_franc = 1
nd$prov = "madrid"
nd$y = predict(m_int2, newdata = nd)
nd$se = predict(m_int2, newdata = nd, se.fit=T)$se.fit
nd$upr = nd$y + 1.96 * nd$se
nd$lwr = nd$y - 1.96 * nd$se
nd$army = factor(nd$army)
levels(nd$army) = c("No military facility", "Military facility")

pdf("lm/output/base_int_pp.pdf", height = 3.5, width = 3.5)
ggplot(nd, aes(x = renta_h_2017_l, y = y, group = army)) +
  geom_line(aes(color = army)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = army), alpha = 0.2) +
  labs(x = "(Log) Mean household income, 2017", y = "Pr(VOX electoral share)") +
  theme_classic() +
  theme(panel.background = element_blank(),
        legend.position = c(0.05, 0.75), legend.justification = c(0,0),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  scale_color_manual(values = c("#0e80d8", "#31A354")) +
  scale_fill_manual(values = c("#0e80d8", "#31A354"))
dev.off()

## ---------------------------
## Checking effect of bases using on PP

dvPP =  c("PP_1982_10 ~ part_1982_10", "PP_1986_06 ~ part_1986_06",
  "PP_1989_10 ~ part_1989_10", "PP_1993_06 ~ part_1993_06",
  "PP_1996_03 ~ part_1996_03", "PP_2000_03 ~ part_2000_03",
  "PP_2004_03 ~ part_2004_03", "PP_2008_03 ~ part_2008_03",
  "PP_2011_11 ~ part_2011_11", "PP_2015_12 ~ part_2015_12",
  "PP_2016_06 ~ part_2016_06", "PP_2019_04 ~ part_2019_04",
  "PP_2019_11 ~ part_2019_11")

label_elec = gsub("PP_(.*)_(.*) ~.*", "\\1/\\2", dvPP)

f = "+ army + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov)"

my_tidy = function(x, label){
  df = broom::tidy(x, conf.int = TRUE) %>%
    filter(term == "army") %>%
    rename(est = estimate, sd = std.error,
      lwr = conf.low, upr = conf.high) %>%
    select(est, sd, lwr, upr) %>%
    mutate(sample = label)
}

m_PP_all = lapply(dvPP, function(x){lm(as.formula(paste0(x, f)),
  data = data)})
m_PP_20km = lapply(dvPP, function(x){lm(as.formula(paste0(x, f)),
  data = subset(data, min_dist_army < 20000))})
m_PP_50kpop = lapply(dvPP, function(x){lm(as.formula(paste0(x, f)),
  subset(data, cities50k == 1))})
m_PP_HQ = lapply(dvPP, function(x){lm(as.formula(paste0(x, f)),
  data = subset(data, region_militar_franc == 1))})
m_PP_hinc = lapply(dvPP, function(x){lm(as.formula(paste0(x, f)),
  data = subset(data, renta_h_2017 > quantile(data$renta_h_2017, 0.75, na.rm=T)))})

# Coefficient list
coef_list = c(
  lapply(m_PP_all, function(x) my_tidy(x, label = "all")),
  lapply(m_PP_20km, function(x) my_tidy(x, label = "20km")),
  lapply(m_PP_50kpop, function(x) my_tidy(x, label = "50kpop")),
  lapply(m_PP_HQ, function(x) my_tidy(x, label = "HQ")),
  lapply(m_PP_hinc, function(x) my_tidy(x, label = "hinc"))) %>%
  bind_rows %>%
  mutate(elec = rep(label_elec, 5)) %>%
  mutate(sample_l = recode(sample,
    "all" = "All sections",
    "20km" = "Within 20km",
    "50kpop" = "Cities over 50k pop.",
    "HQ" = "Former military HQ",
    "hinc" = "High-income sections"))

pdf("lm/output/models_PP.pdf", height = 4, width = 9)
ggplot(coef_list, aes(x = elec, y = est, color = sample_l)) +
  geom_point(position = position_dodge(0.6)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0,
    position = position_dodge(0.6)) +
  labs(x = "", y = "Coefficient of military facility") +
  # facet_wrap(~sample_l, ncol = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  guides(color = guide_legend(ncol = 2)) +
  scale_color_manual(values = c("#363636", "#0e80d8", "#31A354", "#e3a617", "#cc79a7"))
dev.off()
