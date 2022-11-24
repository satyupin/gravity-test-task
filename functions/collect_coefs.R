# The function collects the coefficient from the results of the fixols 
# estimation for each subsample for the selected variable variable.
# It takes as in input a fixols/fixest model object model (result of the 
# estimation) and variable as a variable of interest for which the coefficients
# will be collected, confIntr is numeric, the % for confidence interval that 
# will be computed, the default in NULL (no interval)
collect_coefs <- function(model, variable, confIntr = NULL){
  
  # initialize objects
  smpl <- numeric(length(model))
  coef <- numeric(length(model))
  left <- numeric(length(model))
  right <- numeric(length(model))
  
  # calculate conf intervals
  if(!is.null(confIntr)) {
    
  for (i in 1:length(model)) {
    # populate objects for each subsample
    smpl[i] <- model[[i]]$model_info$sample$value
    coef[i] <- model[[i]]$coefficients[variable][[1]]
    left[i] <- model[[i]]$coefficients[variable][[1]] - 
      model[[i]]$se[variable][[1]]*qt(confIntr, model[[i]]$nobs)
    
    right[i] <- model[[i]]$coefficients[variable][[1]] + 
      model[[i]]$se[variable][[1]]*qt(confIntr, model[[i]]$nobs)
  }
  # merge together
  coefs <- cbind(smpl, coef, left, right)
  } else {
    
    for (i in 1:length(model)) {
      smpl[i] <- model[[i]]$model_info$sample$value
      coef[i] <- model[[i]]$coefficients[variable][[1]]
    }
    coefs <- cbind(smpl, coef)
  }
return(coefs)
  }