# setwd("~/Desktop/vox_military")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("stargazer", "dplyr", "ggplot2")
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
  filter(!is.na(pop_2017) & army == 0)


## Models using neighboring army location

m_diff_nb = lm(VOX_a19 ~ army_nb +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = data)
m_diff_nb2 = lm(VOX_a19 ~ army_nb2 +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = data)
m_diff_ild = lm(VOX_a19 ~ inv_log_dist +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = data)

my_stargazer(dest_file = "lm_diff/output/tab_lm_diff.tex",
  list(m_diff_nb, m_diff_nb2, m_diff_ild),
  omit = c("prov", "part_a19", "pop_2017_l",
    "renta_h_2017_l", "muni_pop_2017_l", "region_militar_franc"),
  title = "Support for VOX and nearby military presence",
  label = "tab:lm_diff",
  order = c("Constant", "army_nb", "army_nb2", "inv_log_dist"),
  covariate.labels = c("(Intercept)",
    "Military in contiguous section",
    "Military within 2km",
    "Inverse logged distance (m)"),
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. All models exclude census sections with army facilities from the sample. Controls: turnout, logged section population, logged municipality population, dummy for military region capital. Full table in Appendix.}")

my_stargazer(dest_file = "lm_diff/output/tab_lm_diff_full.tex",
  list(m_diff_nb, m_diff_nb2, m_diff_ild),
  omit = c("prov"),
  title = "Support for VOX and nearby military presence",
  label = "tab:lm_diff_full",
  order = c("Constant", "army_nb", "army_nb2", "inv_log_dist", "part_a19", "pop_2017_l",
    "renta_h_2017_l", "muni_pop_2017_l", "region_militar_franc"),
  covariate.labels = c("(Intercept)",
    "Military in contiguous section", "Military within 2km",
    "Inverse logged distance (m)",
    "Turnout", "(Log) Population", "(Log) HH Income",
    "(Log) Municipality population", "Military region HQ"),
  add_lines = list(c("Province FE", rep("\\multicolumn{1}{c}{Yes}", 3))),
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. All models exclude census sections with army facilities from the sample.}")



## Models using neighboring army location & interaction

m_diff_nb_int = lm(VOX_a19 ~ army_nb * renta_h_2017_l +
  part_a19 + pop_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = data)
m_diff_nb2_int = lm(VOX_a19 ~ army_nb2 * renta_h_2017_l +
  part_a19 + pop_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = data)
m_diff_ild_int = lm(VOX_a19 ~ inv_log_dist * renta_h_2017_l +
  part_a19 + pop_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = data)

my_stargazer(dest_file = "lm_diff/output/tab_lm_diff_int.tex",
  list(m_diff_nb_int, m_diff_nb2_int, m_diff_ild_int),
  omit = c("prov", "part_a19", "pop_2017_l",
    "muni_pop_2017_l", "region_militar_franc"),
  title = "Support for VOX and nearby military presence",
  label = "tab:lm_diff_int",
  order = c("Constant", "army_nb$", "army_nb2$", "inv_log_dist$", "^renta"),
  covariate.labels = c("(Intercept)",
    "Military in contiguous section",
    "Military within 2km",
    "Inverse logged distance (m)",
    "(Log) Household income",
    "Contiguous $\\times$ Income",
    "Within 2km $\\times$ Income",
    "Inv. dist. $\\times$ Income"),
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. All models exclude census sections with army facilities from the sample. Controls: turnout, logged section population, logged municipality population, dummy for military region capital. Full table in Appendix.}")

my_stargazer(dest_file = "lm_diff/output/tab_lm_diff_int_full.tex",
  list(m_diff_nb_int, m_diff_nb2_int, m_diff_ild_int),
  omit = c("prov"),
  title = "Support for VOX and nearby military presence",
  label = "tab:lm_diff_int_full",
  order = c("Constant", "army_nb$", "army_nb2$", "inv_log_dist$", "renta_h_2017_l$",
    "part_a19", "pop_2017_l", "muni_pop_2017_l", "region_militar_franc"),
  covariate.labels = c("(Intercept)", "Military in contiguous section",
    "Military within 2km", "Inverse logged distance (m)",
    "(Log) Household income", "Contiguous $\\times$ Income",
    "Within 2km $\\times$ Income", "Inv. dist. $\\times$ Income",
    "Turnout", "(Log) Population", "(Log) Municipality population", "Military region HQ"),
  add_lines = list(c("Province FE", rep("\\multicolumn{1}{c}{Yes}", 3))),
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. All models exclude census sections with army facilities from the sample.}")

## Predicted probabilities

nd = expand.grid(army_nb = 0:1,
  renta_h_2017_l = seq(min(data$renta_h_2017_l, na.rm=T), max(data$renta_h_2017_l, na.rm=T), 0.25))
nd$part_a19 = mean(data$part_a19, na.rm=T)
nd$pop_2017_l = mean(data$pop_2017_l, na.rm=T)
nd$muni_pop_2017_l = mean(data$muni_pop_2017_l, na.rm=T)
nd$region_militar_franc = 1
nd$prov = "madrid"
nd$y = predict(m_diff_nb_int, newdata = nd)
nd$se = predict(m_diff_nb_int, newdata = nd, se.fit=T)$se.fit
nd$upr = nd$y + 1.96 * nd$se
nd$lwr = nd$y - 1.96 * nd$se
nd$army_nb = factor(nd$army_nb)
levels(nd$army_nb) = c("No military facility", "Contiguous military facility")

plot_diff_int_pp = ggplot(nd, aes(x = renta_h_2017_l, y = y, group = army_nb)) +
  geom_line(aes(color = army_nb)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = army_nb), alpha = 0.2) +
  labs(x = "(Log) Mean household income, 2017", y = "Pr(VOX electoral share)") +
  theme_classic() +
  theme(panel.background = element_blank(),
        legend.position = c(0.03, 0.73), legend.justification = c(0,0),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  scale_color_manual(values = c("#0e80d8", "#31A354")) +
  scale_fill_manual(values = c("#0e80d8", "#31A354")) +
  geom_rug(data = subset(m_diff_nb_int$model, army_nb==1),
    aes(x = renta_h_2017_l), length = unit(0.05, "npc"), color = "#31A354",
    alpha = 0.05, sides = "b", inherit.aes = F) +
  geom_rug(data = subset(m_diff_nb_int$model, army_nb==0),
    aes(x = renta_h_2017_l), length = unit(0.05, "npc"), color = "#0e80d8",
    alpha = 0.025, sides = "t", inherit.aes = F) +
  scale_y_continuous(expand = c(0, 0.02))

pdf("lm_diff/output/diff_int_pp.pdf", height = 3.5, width = 3.5)
plot_diff_int_pp
dev.off()

ggsave("lm_diff/output/diff_int_pp.eps",
  height = 3.5, width = 3.5, units = "in", device = cairo_ps)

nd_ild = expand.grid(inv_log_dist = seq(min(data$inv_log_dist), max(data$inv_log_dist), 0.01),
  renta_h_2017_l = log(c(1,2,4) * 10000))
nd_ild$part_a19 = mean(data$part_a19, na.rm=T)
nd_ild$pop_2017_l = mean(data$pop_2017_l, na.rm=T)
nd_ild$muni_pop_2017_l = mean(data$muni_pop_2017_l, na.rm=T)
nd_ild$region_militar_franc = 1
nd_ild$prov = "madrid"
nd_ild$y = predict(m_diff_ild_int, newdata = nd_ild)
nd_ild$se = predict(m_diff_ild_int, newdata = nd_ild, se.fit=T)$se.fit
nd_ild$upr = nd_ild$y + 1.96 * nd_ild$se
nd_ild$lwr = nd_ild$y - 1.96 * nd_ild$se
nd_ild$distance = exp(1 / nd_ild$inv_log_dist)
nd_ild$renta = factor(nd_ild$renta_h_2017_l)
levels(nd_ild$renta) = c("Low income (10k)", "Medium (20k)", "High income (40k)")

rug_data_ild = subset(m_diff_ild_int$model,
  abs(exp(renta_h_2017_l) - 10000) < 1000 |
  abs(exp(renta_h_2017_l) - 40000) < 1000)
rug_data_ild$r = ifelse(rug_data_ild$renta_h_2017_l > 10, "h", "l")

diff_int_ild_pp = ggplot(subset(nd_ild, !grepl("Medium", renta)),
    aes(x = inv_log_dist, y = y, group = renta)) +
  geom_line(aes(color = renta)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = renta), alpha = 0.2) +
  labs(x = "Inverse logged distance (m)", y = "Pr(VOX electoral share)") +
  theme_classic() +
  theme(panel.background = element_blank(),
        legend.position = c(0.03, 0.73), legend.justification = c(0,0),
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
        strip.background = element_blank()) +
  scale_color_manual(values = c("#0e80d8", "#31A354")) +
  scale_fill_manual(values = c("#0e80d8", "#31A354")) +
  geom_rug(data = subset(rug_data_ild, r=="h"),
    aes(x = inv_log_dist), length = unit(0.05, "npc"), color = "#31A354",
    alpha = 0.15, sides = "t", inherit.aes = F) +
  geom_rug(data = subset(rug_data_ild, r=="l"),
    aes(x = inv_log_dist), length = unit(0.05, "npc"), color = "#0e80d8",
    alpha = 0.15, sides = "b", inherit.aes = F) +
  scale_y_continuous(expand = c(0, 0.02))

pdf("lm_diff/output/diff_int_ild_pp.pdf", height = 3.5, width = 3.5)
diff_int_ild_pp
dev.off()

ggsave("lm_diff/output/diff_int_ild_pp.eps",
  height = 3.5, width = 3.5, units = "in", device = cairo_ps)

## Robustness tests, base models

# Only sections within 20km

m_diff_nb_20km = lm(VOX_a19 ~ army_nb +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, min_dist_army <= 20000))
m_diff_nb2_20km = lm(VOX_a19 ~ army_nb2 +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, min_dist_army <= 20000))
m_diff_ild_20km = lm(VOX_a19 ~ inv_log_dist +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, min_dist_army <= 20000))

my_stargazer(dest_file = "lm_diff/output/tab_lm_diff_20km.tex",
  list(m_diff_nb_20km, m_diff_nb2_20km, m_diff_ild_20km),
  omit = c("prov"),
  title = "Support for VOX and nearby military presence, only sections within 20km of military facilities",
  label = "tab:lm_diff_20km",
  order = c("Constant", "army_nb", "army_nb2", "inv_log_dist",
    "part_a19", "pop_2017_l", "renta_h_2017_l", "muni_pop_2017_l", "region_militar_franc"),
  covariate.labels = c("(Intercept)",
    "Military in contiguous section",
    "Military within 2km",
    "Inverse logged distance (m)",
    "Turnout", "(Log) Population", "(Log) HH Income",
    "(Log) Municipality population", "Military region HQ"),
  add_lines = list(c("Province FE", rep("\\multicolumn{1}{c}{Yes}", 3))),
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. All models exclude census sections with army facilities from the sample. Only including sections within 20km of military facilities.}")

# Only cities >50k pop

m_diff_nb_50kpop = lm(VOX_a19 ~ army_nb +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, cities50k == 1))
m_diff_nb2_50kpop = lm(VOX_a19 ~ army_nb2 +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, cities50k == 1))
m_diff_ild_50kpop = lm(VOX_a19 ~ inv_log_dist +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, cities50k == 1))

my_stargazer(dest_file = "lm_diff/output/tab_lm_diff_50kpop.tex",
  list(m_diff_nb_50kpop, m_diff_nb2_50kpop, m_diff_ild_50kpop),
  omit = c("prov"),
  title = "Support for VOX and nearby military presence, only sections in municipalities of more than 50000 inhabitants",
  label = "tab:lm_diff_50kpop",
  order = c("Constant", "army_nb", "army_nb2", "inv_log_dist",
    "part_a19", "pop_2017_l", "renta_h_2017_l", "muni_pop_2017_l", "region_militar_franc"),
  covariate.labels = c("(Intercept)",
    "Military in contiguous section",
    "Military within 2km",
    "Inverse logged distance (m)",
    "Turnout", "(Log) Population", "(Log) HH Income",
    "(Log) Municipality population", "Military region HQ"),
  add_lines = list(c("Province FE", rep("\\multicolumn{1}{c}{Yes}", 3))),
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. All models exclude census sections with army facilities from the sample. Only including sections in municipalities with more than 50000 inhabitants.}")

# Only capitals of military regions

m_diff_nb_milreg = lm(VOX_a19 ~ army_nb +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, region_militar_franc == 1))
m_diff_nb2_milreg = lm(VOX_a19 ~ army_nb2 +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, region_militar_franc == 1))
m_diff_ild_milreg = lm(VOX_a19 ~ inv_log_dist +
  part_a19 + pop_2017_l + renta_h_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, region_militar_franc == 1))

my_stargazer(dest_file = "lm_diff/output/tab_lm_diff_milreg.tex",
  list(m_diff_nb_milreg, m_diff_nb2_milreg, m_diff_ild_milreg),
  omit = c("prov"),
  title = "Support for VOX and nearby military presence, only sections in military region capitals",
  label = "tab:lm_diff_milreg",
  order = c("Constant", "army_nb", "army_nb2", "inv_log_dist",
    "part_a19", "pop_2017_l", "renta_h_2017_l", "muni_pop_2017_l", "region_militar_franc"),
  covariate.labels = c("(Intercept)",
    "Military in contiguous section",
    "Military within 2km",
    "Inverse logged distance (m)",
    "Turnout", "(Log) Population", "(Log) HH Income",
    "(Log) Municipality population", "Military region HQ"),
  add_lines = list(c("Province FE", rep("\\multicolumn{1}{c}{Yes}", 3))),
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. All models exclude census sections with army facilities from the sample. Only including sections in municipalities that were HQ of main military regions (Barcelona, Burgos, A Coruña, Granada, Madrid, Sevilla, Valencia, Valladolid, and Zaragoza).}")

## Robustness tests, interaction models

# Only sections within 20km

m_diff_nb_20km_int = lm(VOX_a19 ~ army_nb * renta_h_2017_l +
  part_a19 + pop_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, min_dist_army <= 20000))
m_diff_nb2_20km_int = lm(VOX_a19 ~ army_nb2 * renta_h_2017_l +
  part_a19 + pop_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, min_dist_army <= 20000))
m_diff_ild_20km_int = lm(VOX_a19 ~ inv_log_dist * renta_h_2017_l +
  part_a19 + pop_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, min_dist_army <= 20000))

my_stargazer(dest_file = "lm_diff/output/tab_lm_diff_20km_int.tex",
  list(m_diff_nb_20km_int, m_diff_nb2_20km_int, m_diff_ild_20km_int),
  omit = c("prov"),
  title = "Support for VOX and nearby military presence, only sections within 20km of military facilities",
  label = "tab:lm_diff_20km_int",
  order = c("Constant", "army_nb$", "army_nb2$", "inv_log_dist$", "renta_h_2017_l$",
    "part_a19", "pop_2017_l", "muni_pop_2017_l", "region_militar_franc"),
  covariate.labels = c("(Intercept)", "Military in contiguous section",
    "Military within 2km", "Inverse logged distance (m)",
    "(Log) Household income", "Contiguous $\\times$ Income",
    "Within 2km $\\times$ Income", "Inv. dist. $\\times$ Income",
    "Turnout", "(Log) Population", "(Log) Municipality population", "Military region HQ"),
  add_lines = list(c("Province FE", rep("\\multicolumn{1}{c}{Yes}", 3))),
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. All models exclude census sections with army facilities from the sample. Only including sections within 20km of military facilities.}")

# Only cities >50k pop

m_diff_nb_50kpop_int = lm(VOX_a19 ~ army_nb * renta_h_2017_l +
  part_a19 + pop_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, cities50k == 1))
m_diff_nb2_50kpop_int = lm(VOX_a19 ~ army_nb2 * renta_h_2017_l +
  part_a19 + pop_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, cities50k == 1))
m_diff_ild_50kpop_int = lm(VOX_a19 ~ inv_log_dist * renta_h_2017_l +
  part_a19 + pop_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, cities50k == 1))

my_stargazer(dest_file = "lm_diff/output/tab_lm_diff_50kpop_int.tex",
  list(m_diff_nb_50kpop_int, m_diff_nb2_50kpop_int, m_diff_ild_50kpop_int),
  omit = c("prov"),
  title = "Support for VOX and nearby military presence, only sections in municipalities of more than 50000 inhabitants",
  label = "tab:lm_diff_50kpop_int",
  order = c("Constant", "army_nb$", "army_nb2$", "inv_log_dist$", "renta_h_2017_l$",
    "part_a19", "pop_2017_l", "muni_pop_2017_l", "region_militar_franc"),
  covariate.labels = c("(Intercept)", "Military in contiguous section",
    "Military within 2km", "Inverse logged distance (m)",
    "(Log) Household income", "Contiguous $\\times$ Income",
    "Within 2km $\\times$ Income", "Inv. dist. $\\times$ Income",
    "Turnout", "(Log) Population", "(Log) Municipality population", "Military region HQ"),
  add_lines = list(c("Province FE", rep("\\multicolumn{1}{c}{Yes}", 3))),
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. All models exclude census sections with army facilities from the sample. Only including sections in municipalities with more than 50000 inhabitants.}")

# Only capitals of military regions

m_diff_nb_milreg_int = lm(VOX_a19 ~ army_nb * renta_h_2017_l +
  part_a19 + pop_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, region_militar_franc == 1))
m_diff_nb2_milreg_int = lm(VOX_a19 ~ army_nb2 * renta_h_2017_l +
  part_a19 + pop_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, region_militar_franc == 1))
m_diff_ild_milreg_int = lm(VOX_a19 ~ inv_log_dist * renta_h_2017_l +
  part_a19 + pop_2017_l + muni_pop_2017_l + region_militar_franc + factor(prov),
  data = subset(data, region_militar_franc == 1))

my_stargazer(dest_file = "lm_diff/output/tab_lm_diff_milreg_int.tex",
  list(m_diff_nb_milreg_int, m_diff_nb2_milreg_int, m_diff_ild_milreg_int),
  omit = c("prov"),
  title = "Support for VOX and nearby military presence, only sections in military region capitals",
  label = "tab:lm_diff_milreg_int",
  order = c("Constant", "army_nb$", "army_nb2$", "inv_log_dist$", "renta_h_2017_l$",
    "part_a19", "pop_2017_l", "muni_pop_2017_l", "region_militar_franc"),
  covariate.labels = c("(Intercept)", "Military in contiguous section",
    "Military within 2km", "Inverse logged distance (m)",
    "(Log) Household income", "Contiguous $\\times$ Income",
    "Within 2km $\\times$ Income", "Inv. dist. $\\times$ Income",
    "Turnout", "(Log) Population", "(Log) Municipality population", "Military region HQ"),
  add_lines = list(c("Province FE", rep("\\multicolumn{1}{c}{Yes}", 3))),
  notes_table = "\\parbox[t]{0.65\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. All models exclude census sections with army facilities from the sample. Only including sections in municipalities that were HQ of main military regions (Barcelona, Burgos, A Coruña, Granada, Madrid, Sevilla, Valencia, Valladolid, and Zaragoza).}")
