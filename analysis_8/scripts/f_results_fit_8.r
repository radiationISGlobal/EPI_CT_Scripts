f_results_fit_8 <- function(fit, N)
{
  # save results
  names <-c("data_set","outcome","subset","subjects","scans","cases","lag","stratified","beta_est","se_beta","lrt_ci_low","lrt_ci_upp","deviance","lrt_stat","lrt_pval")
  n_outputs <- nrow(attr(fit,"lrt_ci"))
  
  results <- as.data.frame(matrix(nrow=n_outputs+1,ncol=length(names)))
  
  sum<-summary(fit)
  
  results[1,1]  <- data_set_format
  results[1,2]  <- outcome_name
  results[1,3]  <- "overall"
  results[1,4]  <- attr(fit,"desc")$desc$n_subjects
  results[1,5]  <- attr(fit,"desc")$desc$n_observations
  results[1,6]  <- attr(fit,"desc")$desc$n_events
  results[1,7]  <- lag
  results[1,8]  <- paste(f_parse_formula(formula2)$strata_vars,collapse=" ")
  results[2:(n_outputs+1),6]  <- N
  results[2:(n_outputs+1),8]  <- rownames(attr(fit,"lrt_ci"))
  results[2:(n_outputs+1),9]  <- sum$linear_coefficients[,1]
  results[2:(n_outputs+1),10] <- sum$linear_coefficients[,2]
  results[2:(n_outputs+1),11] <- attr(fit,"lrt_ci")[,1]
  results[2:(n_outputs+1),12] <- attr(fit,"lrt_ci")[,2]
  results[1,13] <- 2*attr(fit,"details")$value
  results[1,14] <- attr(fit,"lrt")$lrt_stat
  results[1,15] <- 1 - pchisq(results[1,14], n_outputs, lower.tail = T)
  
  names(results) <- names
  results
}
