# Function to get lambda coef
lambda_row = function(model_list){

  lambda = unlist(lapply(model_list, function(x){
    if(class(x) %in% c("sarlm", "Sarlm")){x$lambda
    } else {x = NA}
    }))
  lambda_se = unlist(lapply(model_list, function(x){
    if(class(x) %in% c("sarlm", "Sarlm")){x$lambda.se
    } else {x = NA}
    }))
  z = lambda / lambda_se
  p = 2 * pnorm(-abs(z))
  p[is.na(p)] = 1

  lambda = sprintf("%.02f", lambda)
  lambda[lambda == "NA"] = ""
  lambda[p < 0.05] = paste0(lambda[p < 0.05], "*")
  lambda[p < 0.01] = paste0(lambda[p < 0.001], "*")
  lambda[p < 0.001] = paste0(lambda[p < 0.0001], "*")
  lambda = gsub("(\\*+)", "\\$\\^\\{\\1\\}\\$", lambda)
  lambda = paste0("\\multicolumn{1}{c}{", lambda, "}")

  return(c("Lambda", lambda))

}
