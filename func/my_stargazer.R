# LMs
# My Stargazer
my_stargazer = function(dest_file, model_list,
  omit, title, label, order, covariate.labels, notes_table,
  add_lines = list(
    c("Controls", rep("\\multicolumn{1}{c}{Yes}", length(model_list))),
    c("Province FE", rep("\\multicolumn{1}{c}{Yes}", length(model_list))))){

  filecon = file(dest_file)
  writeLines(
    stargazer(model_list, omit = omit, title = title, label = label,
      order = order, covariate.labels = covariate.labels, notes = notes_table,
      omit.stat = c("f", "ser"),
      intercept.bottom = FALSE,
      column.sep.width = "-20pt",
      multicolumn = FALSE,
      dep.var.labels.include = FALSE,
      dep.var.caption = "",
      font.size = "small",
      digits = 3,
      digits.extra = 0,
      star.char = c("+", "*", "**", "***"),
      star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
      notes.align = "c",
      align = TRUE,
      no.space = TRUE,
      add.lines = add_lines,
      notes.label = "",
      notes.append = FALSE),
  filecon)
  close(filecon)

}

# Spatial Models
my_stargazer_slm = function(dest_file, model_list,
  omit, title, label, order, covariate.labels, notes_table,
  add_lines = list(
    c("Controls", rep("\\multicolumn{1}{c}{Yes}", length(model_list))),
    lambda_row(model_list))
  ){

  st = stargazer(model_list, omit = omit, title = title, label = label,
    order = order, covariate.labels = covariate.labels, notes = notes_table,
    omit.stat = c("wald", "ll", "sigma2", "lr"),
    intercept.bottom = FALSE,
    column.sep.width = "-20pt",
    multicolumn = FALSE,
    dep.var.labels.include = FALSE,
    dep.var.caption = "",
    font.size = "small",
    digits = 3,
    digits.extra = 0,
    star.char = c("+", "*", "**", "***"),
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    notes.align = "c",
    align = TRUE,
    no.space = TRUE,
    add.lines = add_lines,
    notes.label = "",
    notes.append = FALSE)

    filecon = file(dest_file)
    writeLines(st, filecon)
    close(filecon)

}
