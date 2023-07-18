modelsummary(
  models = XXXX,
  output = "latex",
  vcov = ~survey,
  estimate = "{estimate}{stars}",
  coef_rename = coef_recode,
  coef_omit = "survey|municipio",
  gof_map = gs,
  title = "XXXX\\label{tab:XXXX}",
  add_rows = as.data.frame(rbind(
    c("Controls", "No", rep("Yes", length(XXXX)-1)),
    c("Survey FE", rep("Yes", length(XXXX))),
    c("Municipality FE", rep("Yes", length(XXXX)))
    )),
  threeparttable = TRUE, escape = FALSE) %>%
# add_header_above(c(" " = 1, "h1" = 2, "h2" = 2)) %>%
footnote(general = n,
  threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
kable_styling(latex_options = "hold_position") %>%
save_kable(file = "survey_analyses/output/XXXX.tex")
