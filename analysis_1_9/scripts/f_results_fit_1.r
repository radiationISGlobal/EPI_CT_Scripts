
f_results_fit_1 <- function(fit)
{
  names <-c("data_set","outcome","subjects","scans","cases","lag","stratified","beta_est","se_beta","lrt_ci_low","lrt_ci_upp","deviance","lrt_stat","lrt_pval")
  res <- vector()
  
  
  sum<-summary(fit)
  res[1]  <- data_set_format
  res[2]  <- outcome_name
  res[3]  <- attr(fit,"desc")$desc$n_subjects
  res[4]  <- attr(fit,"desc")$desc$n_observations
  res[5]  <- attr(fit,"desc")$desc$n_events
  res[6]  <- lag
  res[7]  <- paste(f_parse_formula(formula)$strata_vars,collapse=" ")
  res[8]  <- sum$linear_coefficients[1,1]*100
  res[9]  <- sum$linear_coefficients[1,2]
  res[10] <- attr(fit,"lrt_ci")[1]*100
  res[11] <- attr(fit,"lrt_ci")[2]*100
  res[12] <- 2*attr(fit,"details")$value
  res[13] <- attr(fit,"lrt")$lrt_stat
  res[14] <- attr(fit,"lrt")$lrt_pval
  
  names(res) <- names
  res
}
