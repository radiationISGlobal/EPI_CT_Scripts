f_results_fit_5 <- function(fit1,fit2)
{
  # save results
  names <-c("data_set","outcome","subjects","scans","cases","lag","stratified","beta_est","beta_est_ses_adj","se_beta","se_beta_ses_adj",
            "lrt_ci_low","lrt_ci_upp","lrt_ci_low_ses_adj","lrt_ci_upp_ses_adj","lrt_pval","deviance","deviance_adj","subset")
  res <- vector()
  
  if(class(fit1)=="try-error")
  {
    #sum1 <- summary(fit1)
    sum2 <- summary(fit2)
    res[1]  <- data_set_format
    res[2]  <- outcome_name
    res[3]  <- attr(fit2,"desc")$desc$n_subjects
    res[4]  <- attr(fit2,"desc")$desc$n_observations
    res[5]  <- attr(fit2,"desc")$desc$n_events
    res[6]  <- lag
    res[7]  <- paste(f_parse_formula(formula1)$strata_vars,collapse=" ")
    res[8]  <- NA
    res[9]  <- sum2$linear_coefficients[1,1]
    res[10] <- NA
    res[11] <- sum2$linear_coefficients[1,2]
    res[12] <- NA
    res[13] <- NA
    res[14] <- attr(fit2,"lrt_ci")[1]
    res[15] <- attr(fit2,"lrt_ci")[2]
    res[16] <- NA
    res[17] <- NA
    res[18] <- 2*attr(fit2,"details")$value
    res[19] <- "ses_countries"
  } else
    if(class(fit2)=="try-error")
    {
      sum1 <- summary(fit1)
      #sum2 <- summary(fit2)
      res[1]  <- data_set_format
      res[2]  <- outcome_name
      res[3]  <- attr(fit1,"desc")$desc$n_subjects
      res[4]  <- attr(fit1,"desc")$desc$n_observations
      res[5]  <- attr(fit1,"desc")$desc$n_events
      res[6]  <- lag
      res[7]  <- paste(f_parse_formula(formula1)$strata_vars,collapse=" ")
      res[8]  <- sum1$linear_coefficients[1,1]
      res[9]  <- NA
      res[10] <- sum1$linear_coefficients[1,2]
      res[11] <- NA
      res[12] <- attr(fit1,"lrt_ci")[1]
      res[13] <- attr(fit1,"lrt_ci")[2]
      res[14] <- NA
      res[15] <- NA
      res[16] <- NA
      res[17] <- 2*attr(fit1,"details")$value
      res[18] <- NA
      res[19] <- "ses_countries"
    }else
    {
      lrt_test <- f_lrt(fit1,fit2)
      
      sum1 <- summary(fit1)
      sum2 <- summary(fit2)
      res[1]  <- data_set_format
      res[2]  <- outcome_name
      res[3]  <- attr(fit1,"desc")$desc$n_subjects
      res[4]  <- attr(fit1,"desc")$desc$n_observations
      res[5]  <- attr(fit1,"desc")$desc$n_events
      res[6]  <- lag
      res[7]  <- paste(f_parse_formula(formula1)$strata_vars,collapse=" ")
      res[8]  <- sum1$linear_coefficients[1,1]
      res[9]  <- sum2$linear_coefficients[1,1]
      res[10] <- sum1$linear_coefficients[1,2]
      res[11] <- sum2$linear_coefficients[1,2]
      res[12] <- attr(fit1,"lrt_ci")[1]
      res[13] <- attr(fit1,"lrt_ci")[2]
      res[14] <- attr(fit2,"lrt_ci")[1]
      res[15] <- attr(fit2,"lrt_ci")[2]
      res[16] <- lrt_test$lrt_pval
      res[17] <- 2*attr(fit1,"details")$value
      res[18] <- 2*attr(fit2,"details")$value
      res[19] <- "ses_countries"
    }
  names(res) <- names
  res
}
