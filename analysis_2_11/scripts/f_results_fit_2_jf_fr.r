f_results_fit_2_cumcat2short_fr <- function(fit, casescat)
{
  n_outputs <- nrow(attr(fit,"wald_ci"))
  
  names <-c("data_set","outcome","subjects","scans","cases","lag","stratified","exp(coef)","se(coef)","wald_ci_low","wald_ci_upp","deviance","lrt_stat","lrt_pval","linear_trend_pval")
  results <- as.data.frame(matrix(nrow=n_outputs+1,ncol=length(names)))
  results[1,1]  <- data_set_format
  results[1,2]  <- outcome_name
  results[1,3]  <- attr(fit,"desc")$desc$n_subjects
  results[1,4]  <- attr(fit,"desc")$desc$n_observations
  results[1,5]  <- attr(fit,"desc")$desc$n_events
  results[1,6]  <- lag
  results[1,7]  <- paste(f_parse_formula(formula)$strata_vars,collapse=" ")
  results[2:(n_outputs+2),5] <- c(casescat$n)
  results[2:(n_outputs+2),7] <- c("Ref", rownames(attr(fit,"wald_ci")))
  results[2:(n_outputs+2),8] <- c(1, attr(fit,"wald_ci")[,1])
  results[2:(n_outputs+2),9] <- c("", attr(fit,"loglin_coef")[,3])
  results[2:(n_outputs+2),10] <- c("", attr(fit,"wald_ci")[,3])
  results[2:(n_outputs+2),11] <- c("", attr(fit,"wald_ci")[,4])
  results[1,12] <- 2*attr(fit,"details")$value
  results[1,13] <- attr(fit,"lrt")$lrt_stat
  results[1,14] <- 1 - pchisq(results[1,13], n_outputs, lower.tail = T)
  
  # max dose involved in the model: need to recompute the risksets
  dt2           <- f_to_model_data(formula, dt1_fr, id_name="patientids", time_name="age")
  n_lin_vars    <- attr(dt2, "n_lin_vars")
  n_loglin_vars <- attr(dt2, "n_loglin_vars")
  rsets         <- f_risksets(formula, data = dt2, lag, id_name="patientids", time_name="age")
  
  # extract the maximum dose from the risksets
  max_dose <- max(unlist(lapply(rsets,function(x) max(dt1_fr$dose_cum[x]))))
  
  # mid points of intervals of categories of exposure
  #x <- c(1,7.5,12.5,20,37.5,(max_dose-100)/2)
  x <- c(1,7.5,14,(max_dose-50)/2)
  # estimates for each interval
  y <- c(results[2:(n_outputs+2),8])
  
  linear_trend_pval <- lmp(lm(y~x))
  
  results[1,15] <- linear_trend_pval
  
  names(results) <- names
  results
}
