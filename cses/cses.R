# setwd("~/Documents/Projects/cuarteles_militares")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("tidyr", "dplyr", "purrr", "broom", "kableExtra", "ggplot2")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
lapply(pkg, library, character.only = TRUE)

# ------------------------------------------

## Function for t-test
tt = function(contvar, binvar, out = "pval"){
  t = t.test(contvar[binvar==1], contvar[binvar==0])
  if(out == "pval"){return(round(t$p.value, 3))}
  if(out == "mean1"){return(round(t$estimate[1], 2))}
  if(out == "mean0"){return(round(t$estimate[2], 2))}
  if(out == "diff"){return(round(t$estimate[1]-t$estimate[2], 2))}
  if(out == "diff_noround"){return(t$estimate[1]-t$estimate[2])}
  if(out == "lwr"){return(t$conf.int[1])}
  if(out == "upr"){return(t$conf.int[2])}
}

# ------------------------------------------


## Download CSES csv file, unzip, assign, remove
url = "https://cses.org/wp-content/uploads/2020/12/cses_imd_csv.zip"
tmp = tempfile()
download.file(url, tmp)
data = read.csv(unzip(tmp, "cses_imd.csv"))
unlink(tmp)
file.remove("cses_imd.csv")
# data = read.csv("~/Downloads/cses_imd.csv")

# Rename, select, etc
data = data %>%
  rename(
    countryname = IMD1006_NAM,
    year = IMD1008_YEAR,
    ses = IMD2016,
    gender = IMD2002,
    educ = IMD2003,
    age = IMD2001_1) %>%
  mutate(
    military = ifelse(IMD2015_ISCO_08 %in% 0:3 | IMD2015_ISCO_88 == 1, 1, 0),
    vote_right = ifelse(IMD3002_LR_CSES == 3, 1, 0),
    ideo = ifelse(IMD3006 > 90, NA, IMD3006)) %>%
  mutate(military = ifelse(IMD2015_ISCO_08 %in% 997:998, NA, military)) %>%
  mutate(military = ifelse(IMD2015_ISCO_88 %in% 997:998, NA, military)) %>%
  mutate(military = ifelse(IMD2015_ISCO_08 == 999 & IMD2015_ISCO_88 == 999, NA, military)) %>%
  select(military, vote_right, ideo,
    countryname, year,
    ses, gender, educ, age) %>%
  mutate(gender = ifelse(gender %in% 1:2, gender, NA) - 1) %>%
  mutate(age = ifelse(age %in% 9997:9999, NA, age)) %>%
  mutate(countryname = recode(countryname,
    "United States of America" = "USA",
    "Russian Federation" = "Russia"))

# Reduce to countries where min number of military
min_mil = 10
exclude = table(data$countryname, data$military)
exclude = rownames(exclude)[exclude[,2] < min_mil]
data = data %>%
  filter(!countryname %in% exclude)

## --------------------------------------
## MODELS

m = crossing(countryname = unique(data$countryname)) %>%
  mutate(model = map(.x = countryname,
    .f = ~ broom::tidy(lm(ideo ~ military + gender + age + educ,
      data = filter(data, countryname == .x))))) %>%
  unnest(model) %>%
  filter(term != "(Intercept)") %>%
  # filter(term == "military") %>%
  mutate(var = recode(term,
    "military" = "Military",
    "educ" = "Education level",
    "gender" = "Gender (female)",
    "age" = "Age")) %>%
  rename(se = std.error, est = estimate) %>%
  as.data.frame() %>%
  mutate(
    upr = est + se * qnorm(0.975),
    lwr = est + se * qnorm(0.025),
    sig95 = ifelse(p.value < 0.05, 1, 0))

## --------------------------------------
## TABLE

tab = data %>%
  group_by(countryname) %>%
  summarize(
    years = paste(min(year), max(year), sep = "-"),
    n_civil = sum(military==0, na.rm = T),
    n_mil = sum(military==1, na.rm = T),
    civ_ideo = tt(ideo, military, "mean0"),
    mil_ideo = tt(ideo, military, "mean1"),
    diff = tt(ideo, military, "diff"),
    pval = tt(ideo, military)) %>%
  left_join(m[m$term=="military", c("countryname", "est", "p.value")]) %>%
  rename(beta = est, beta_pv = p.value) %>%
  mutate(
    civ_ideo = paste0(sprintf("%.01f", civ_ideo),
      " ($n$ = ", prettyNum(n_civil, big.mark = ","), ")"),
    mil_ideo = paste0(sprintf("%.01f", mil_ideo),
      " ($n$ = ", prettyNum(n_mil, big.mark = ","), ")")) %>%
  arrange(desc(diff)) %>%
  mutate(sig = ifelse(pval < 0.05, "*", "")) %>%
  mutate(sig = ifelse(pval < 0.01, "**", sig)) %>%
  mutate(sig = ifelse(pval < 0.001, "***", sig)) %>%
  mutate(diff = paste0(sprintf("%.02f", diff), sig)) %>%
  mutate(beta_sig = ifelse(beta_pv < 0.05, "*", "")) %>%
  mutate(beta_sig = ifelse(beta_pv < 0.01, "**", beta_sig)) %>%
  mutate(beta_sig = ifelse(beta_pv < 0.001, "***", beta_sig)) %>%
  mutate(beta = paste0(sprintf("%.02f", beta), beta_sig)) %>%
  select(-sig, -n_civil, -n_mil) %>%
  select(
    "Country" = countryname,
    "Years" = years,
    # "Civilians" = n_civil,
    # "Military" = n_mil,
    "Ideology (civilians)" = civ_ideo,
    "Ideology (military)" = mil_ideo,
    "Diff" = diff,
    "$\\beta$" = beta
  )

n = "\\\\textit{Note:} `Diff' shows the difference in means and the significance level in a two-sided t-test. `$\\\\beta$' shows the coefficient for being in the military in a linear model controlling for gender, education level, and age. * p$<$.05, ** p$<$.01, *** p$<$.001. Data from CSES (\\\\href{www.cses.org}{www.cses.org})."

tabtex = kable(tab,
    format = "latex",
    escape = FALSE,
    booktabs = TRUE,
    linesep = "",
    caption = "Differences in ideology between military and civilians across several countries\\label{tab:cses}") %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  footnote(general = n,
    general_title = "", threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
  save_kable(file = "cses/output/tab_cses.tex")



## --------------------------------------
## PLOT (t-test )

plotd = data %>%
  group_by(countryname) %>%
  summarize(
    diff = tt(ideo, military, "diff"),
    upr = tt(ideo, military, "upr"),
    lwr = tt(ideo, military, "lwr"),
    pval = tt(ideo, military, "pval")) %>%
  mutate(sig95 = ifelse(pval < 0.05, 1, 0))

# plotd$ccode = countrycode(plotd$countryname, "country.name", "iso3c")

pdf("cses/output/CSES_ttest.pdf", width = 4, height = 5)
ggplot(plotd, aes(x = reorder(countryname, diff),
    y = diff, ymin = lwr, ymax = upr, color = factor(sig95))) +
  geom_errorbar(width = 0) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = "Difference in ideology + 95% CI", x = "") +
  # scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("grey", "black")) +
  coord_flip()
dev.off()

## PLOTS FROM MODELS

# Only estimate for military
pdf("cses/output/CSES_lm.pdf", width = 4.5, height = 5)
ggplot(m %>% filter(term=="military"),
    aes(x = reorder(countryname, est),
    y = est, ymin = lwr, ymax = upr, color = factor(sig95))) +
  geom_errorbar(width = 0) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = "Coefficient estimate (military) + 95% CI", x = "") +
  # scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("grey", "black")) +
  coord_flip()
dev.off()

# Full coefficient plot
pdf("cses/output/CSES_lm_full.pdf", width = 8, height = 9)
ggplot(m, aes(x = var, y = est, ymin = lwr, ymax = upr, color = factor(sig95))) +
  geom_errorbar(width = 0) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = "Coefficient estimate + 95% CI", x = "") +
  # scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~countryname, ncol = 5, scales="free_x") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("grey", "black")) +
  coord_flip()
dev.off()
