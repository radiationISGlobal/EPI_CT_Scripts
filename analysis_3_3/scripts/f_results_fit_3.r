
f_results_fit_3 <- function(fit)
{
  # save results
  names <-c("data_set","outcome","subset","subjects","scans","cases","lag","stratified","beta_est","se_beta","lrt_ci_low","lrt_ci_upp","deviance","lrt_stat","lrt_pval")
  res <- vector()
  
  if(class(fit)=="try-error")
  {
    res[1]  <- data_set_format
    res[2]  <- outcome_name
    res[3]  <- subsets[i]
    res[4]  <- length(dt1$country[which(dt1$n_pe==0)]) 
    res[5]  <- dim(dt1)[1]-as.numeric(res[4])
    res[6]  <- sum(dt1[,which(names(dt1)==outcome_name)])
    res[7]  <- lag
    res[15] <- NA
  }else {
    sum<-summary(fit)
    res[1]  <- data_set_format
    res[2]  <- outcome_name
    res[3]  <- subsets[i]
    res[4]  <- attr(fit,"desc")$desc$n_subjects
    res[5]  <- attr(fit,"desc")$desc$n_observations
    res[6]  <- attr(fit,"desc")$desc$n_events
    res[7]  <- lag
    res[8]  <- paste(f_parse_formula(formula)$strata_vars,collapse=" ")
    res[9]  <- sum$linear_coefficients[1,1]
    res[10] <- sum$linear_coefficients[1,2]
    res[11] <- attr(fit,"lrt_ci")[1]
    res[12] <- attr(fit,"lrt_ci")[2]
    res[13] <- 2*attr(fit,"details")$value
    res[14] <- attr(fit,"lrt")$lrt_stat
    res[15] <- attr(fit,"lrt")$lrt_pval
  }
  names(res) <- names
  
  results[] <- lapply(results,as.character)
  results <- rbind(results,res)
  names(results) <- names
  results
}
