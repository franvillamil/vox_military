# setwd("~/Documents/Projects/cuarteles_militares")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("stargazer", "ggplot2", "dplyr", "stringr", "spdep", "spatialreg")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
lapply(pkg, library, character.only = TRUE)
# Functions
source("func/my_stargazer.R")
source("func/lambda_row.R")
# -------------------------

# For some fucking reason LR.sarlm doesnt work (conflict with spdep?)
# Just changing logLik(x) to logLik.sarlm(x)
myLR.sarlm = function (x, y)
{
    if (!inherits(x, "logLik"))
        LLx <- logLik.sarlm(x)
    else LLx <- x
    if (!inherits(y, "logLik"))
        LLy <- logLik.sarlm(y)
    else LLy <- y
    statistic <- 2 * (LLx - LLy)
    attr(statistic, "names") <- "Likelihood ratio"
    parameter <- abs(attr(LLx, "df") - attr(LLy, "df"))
    if (parameter < 1)
        stop("non-positive degrees of freedom: no test possible")
    attr(parameter, "names") <- "df"
    p.value <- 1 - pchisq(abs(statistic), parameter)
    estimate <- c(LLx, LLy)
    attr(estimate, "names") <- c(paste("Log likelihood of", deparse(substitute(x))),
        paste("Log likelihood of", deparse(substitute(y))))
    method <- "Likelihood ratio for spatial linear models"
    res <- list(statistic = statistic, parameter = parameter,
        p.value = p.value, estimate = estimate, method = method)
    class(res) <- "htest"
    res
}

#

# Load models

sem_nb = readRDS("slm/output/m_sem_nb.rds")
sem_nb2 = readRDS("slm/output/m_sem_nb2.rds")
sem_ild = readRDS("slm/output/m_sem_ild.rds")
sdem_nb = readRDS("slm/output/m_sdem_nb.rds")
sdem_nb2 = readRDS("slm/output/m_sdem_nb2.rds")
sdem_ild = readRDS("slm/output/m_sdem_ild.rds")

# ------------------------
# Print full tables

# LR tests
LR_nb = myLR.sarlm(sdem_nb, sem_nb)
LR_nb2 = myLR.sarlm(sdem_nb2, sem_nb2)
LR_ild = myLR.sarlm(sdem_ild, sem_ild)
writeLines(text = paste0("LR test = ", sprintf("%.02f", LR_nb$statistic),
    ", p-value = ", sprintf("%.03f", LR_nb$p.value)),
  con = file("slm_tables/output/lr_test_nb.tex"))
writeLines(text = paste0("LR test = ", sprintf("%.02f", LR_nb2$statistic),
    ", p-value = ", sprintf("%.03f", LR_nb2$p.value)),
  con = file("slm_tables/output/lr_test_nb2.tex"))
writeLines(text = paste0("LR test = ", sprintf("%.02f", LR_ild$statistic),
    ", p-value = ", sprintf("%.03f", LR_ild$p.value)),
  con = file("slm_tables/output/lr_test_ild.tex"))
closeAllConnections()

my_stargazer_slm(dest_file = "slm_tables/output/tab_sem.tex",
  model_list = list(sem_nb, sem_nb2, sem_ild),
  omit = c("part_a19", "pop_2017_l", "renta_h_2017_l",
    "muni_pop_2017_l", "region_militar_franc"),
  label = "tab:sem",
  title = "Spatial Error Models on support for VOX",
  order = c("Constant", "army"),
  covariate.labels = c("(Intercept)", "Military facility", "Military (spatial lag)"),
  notes_table = "\\parbox[t]{0.6\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Models 1 includes spatial weights based on queen-type contiguity. Model 2 does so identifying neighbors as sections within 2km. Model 3 uses spatial weights based on the inverse logged distance (m). Controls: turnout, logged section population, logged household income, logged municipality population, dummy for military region capital. Full table in Appendix.}")

my_stargazer_slm(dest_file = "slm_tables/output/tab_sdem.tex",
  model_list = list(sdem_nb, sdem_nb2, sdem_ild),
  omit = c("part_a19", "pop_2017_l", "renta_h_2017_l",
    "muni_pop_2017_l", "region_militar_franc"),
  label = "tab:sdem",
  title = "Spatial Durbin Error Models on support for VOX",
  order = c("Constant", "army", "lag.army"),
  covariate.labels = c("(Intercept)", "Military facility", "Military (spatial lag)"),
  notes_table = "\\parbox[t]{0.6\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Models 1 includes spatial weights based on queen-type contiguity. Model 2 does so identifying neighbors as sections within 2km. Model 3 uses spatial weights based on the inverse logged distance (m). Controls: turnout, logged section population, logged household income, logged municipality population, dummy for military region capital. Full table in Appendix.}")

my_stargazer_slm(dest_file = "slm_tables/output/tab_sem_full.tex",
  model_list = list(sem_nb, sem_nb2, sem_ild),
  omit = NULL,
  add_lines = list(lambda_row(list(sem_nb, sem_nb2, sem_ild))),
  label = "tab:sem_full",
  title = "Spatial Error Models on support for VOX",
  order = c("Constant", "army", "part_a19", "pop_2017_l", "renta_h_2017_l",
    "muni_pop_2017_l", "region_militar_franc"),
  covariate.labels = c("(Intercept)", "Military facility", "Turnout",
    "(Log) Population", "(Log) HH Income", "(Log) Municipality population", "Military capital"),
  notes_table = "\\parbox[t]{0.6\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Models 1 includes spatial weights based on queen-type contiguity. Model 2 does so identifying neighbors as sections within 2km. Model 3 uses spatial weights based on the inverse logged distance (m).}")

my_stargazer_slm(dest_file = "slm_tables/output/tab_sdem_full.tex",
  model_list = list(sdem_nb, sdem_nb2, sdem_ild),
  omit = NULL,
  add_lines = list(lambda_row(list(sdem_nb, sdem_nb2, sdem_ild))),
  label = "tab:sdem_full",
  title = "Spatial Durbin Error Models on support for VOX",
  order = c("Constant", "army", "lag.army", "part_a19", "lag.part_a19", "pop_2017_l",
    "lag.pop_2017_l", "renta_h_2017_l", "lag.renta_h_2017_l", "muni_pop_2017_l",
    "lag.muni_pop_2017_l", "region_militar_franc", "lag.region_militar_franc"),
  covariate.labels = c("(Intercept)", "Military facility", "Military (sp lag)", "Turnout",
    "Turnout (sp lag)", "(Log) Population", "Population (sp lag)", "(Log) HH Income",
    "Income (sp lag)", "(Log) Municipality population", "Muni population (sp lag)",
    "Military capital", "Milit capital (sp lag)"),
  notes_table = "\\parbox[t]{0.6\\textwidth}{\\textit{Note:} $+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001$. Models 1 includes spatial weights based on queen-type contiguity. Model 2 does so identifying neighbors as sections within 2km. Model 3 uses spatial weights based on the inverse logged distance (m).}")


mlsem = list(sem_nb, sem_nb2, sem_ild)
mlsdem = list(sdem_nb, sdem_nb2, sdem_ild)

coef = lapply(mlsdem, function(x){
  coef = sprintf("%.03f", x$coefficients)
  t = abs(x$coefficients / x$rest.se)
  s = rep("", length(t))
  s[t > qnorm(0.975)] = paste0(s[t > qnorm(0.975)], "*")
  s[t > qnorm(0.995)] = paste0(s[t > qnorm(0.995)], "*")
  s[t > qnorm(0.9995)] = paste0(s[t > qnorm(0.9995)], "*")
  s[t <= qnorm(0.975) & t > qnorm(0.95)] = paste0(s[t <= qnorm(0.975) & t > qnorm(0.95)], "+")
  coef = cbind(paste0(coef, s))
  })
# coef = c(cbind(names(mlsdem[[1]]$coefficients)), coef)

se = lapply(mlsdem, function(x){
  se = paste0("(", sprintf("%.03f", x$rest.se), ")")
  return(cbind(se))
  })



xtable(cbind(names(mlsdem[[1]]$coefficients), do.call(cbind, coef)))
xtable(do.call(cbind, se))
