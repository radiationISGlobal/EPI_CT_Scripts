# canvis per Jordi F:
#	- comentada la linia 80 que elimina tacs
#	- comentada línia 82 i substituida per línies 83-88. depenent si failtime - lag és <0 o >=0
#			sel.leccionarem tacs reals o la línia fictícia n_pe = -1
f_risksets_jf <- function (formula, data, lag, id_name, time_name) {
  id <- NULL
  n_row <- NULL
  formula_sv <- formula[[2]]
  formula_terms <- sum(gregexpr("+", paste0(as.character(formula[[3]]), 
  collapse = ""), fixed = TRUE)[[1]] > 0) + 1
  if (formula_terms == 1) {
    if (any(grepl("lin", formula[[3]]))) 
      formula_lin <- formula[[3]]
    if (any(grepl("logl", formula[[3]]))) 
      formula_loglin <- formula[[3]]
    if (any(grepl("strata", formula[[3]]))) 
      formula_strat <- formula[[3]]
    if (any(grepl("logl", formula_lin))) 
      rm(formula_lin)
  }
  if (formula_terms == 2) {
    for (i in 2:3) {
      if (any(grepl("lin", formula[[3]][[i]]))) 
        formula_lin <- formula[[3]][[i]]
      if (any(grepl("logl", formula[[3]][[i]]))) 
        formula_loglin <- formula[[3]][[i]]
      if (any(grepl("strata", formula[[3]][[i]]))) 
        formula_strat <- formula[[3]][[i]]
    }
	
    if (any(grepl("logl", formula_lin))) 
      rm(formula_lin)
  }
  if (formula_terms == 3) {
    if (any(grepl("lin", formula[[3]][[3]]))) 
      formula_lin <- formula[[3]][[3]]
    if (any(grepl("logl", formula[[3]][[3]]))) 
      formula_loglin <- formula[[3]][[3]]
    if (any(grepl("strata", formula[[3]][[3]]))) 
      formula_strat <- formula[[3]][[3]]
    for (i in 2:3) {
      if (any(grepl("lin", formula[[3]][[2]][[i]]))) 
        formula_lin <- formula[[3]][[2]][[i]]
      if (any(grepl("logl", formula[[3]][[2]][[i]]))) 
        formula_loglin <- formula[[3]][[2]][[i]]
      if (any(grepl("strata", formula[[3]][[2]][[i]]))) 
        formula_strat <- formula[[3]][[2]][[i]]
    }
    if (any(grepl("logl", formula_lin))) 
      rm(formula_lin)
  }
  if (exists("formula_lin")) {
    lin_vars <- unlist(strsplit(as.character(formula_lin)[2:length(formula_lin)], 
                                split = "+", fixed = T))
    lin_vars <- gsub(" ", "", lin_vars)
  }
  if (exists("formula_loglin")) {
    loglin_vars <- unlist(strsplit(as.character(formula_loglin)[2:length(formula_loglin)], 
                                   split = "+", fixed = T))
    loglin_vars <- gsub(" ", "", loglin_vars)
  }
  if (exists("formula_strat")) {
    strata_vars <- unlist(strsplit(as.character(formula_strat)[2:length(formula_strat)], 
                                   split = "+", fixed = T))
    strata_vars <- gsub(" ", "", strata_vars)
  }
  v_id <- eval(parse(text = paste0("data$", id_name)))
  v_n_pe <- eval(parse(text = paste0("data$", "n_pe")))
  v_entry <- eval(parse(text = paste0("data$", formula_sv[[2]])))
  v_exit <- eval(parse(text = paste0("data$", formula_sv[[3]])))
  v_outcome <- eval(parse(text = paste0("data$", formula_sv[[4]])))
  v_time <- eval(parse(text = paste0("data$", time_name)))
  nrow_cases <- which(v_outcome == 1)
  failtimes <- v_exit[nrow_cases]
  id_cases <- v_id[nrow_cases]
  rsets <- list()
  nrows_cases_ <- vector()
  for (i in 1:length(failtimes)) {
    dt <- data[which(v_entry < failtimes[i] & v_exit >= failtimes[i]), 1:7]
    #dt <- dt[which(dt[, 7] <= failtimes[i] - lag), ]
    names(dt)[c(2, 4:6, 7)] <- c("id", "entry", "exit", "outcome", "time")
    #dt <- dt %>% group_by(id) %>% dplyr::summarize(row = max(n_row))
    if ((failtimes[i] - lag) < 0){
      dt <- dt[which(dt[, 7] <=0 & dt[, 3] == -1), ] %>% group_by(id) %>% dplyr::summarize(row = max(n_row))
    } else {
      dt <- dt[which(dt[, 7] <= failtimes[i] - lag), ] %>% group_by(id) %>% 
        dplyr::summarize(row = max(n_row))
    }
    nrows_cases_[i] <- dt$row[which(dt$id == id_cases[i])]
    dt$IN <- T
    if (exists("formula_strat")) 
      for (j in 1:length(strata_vars)) {
        x <- eval(parse(text = paste0("data$", strata_vars[j])))
        x_case <- x[nrow_cases[i]]
        dt$x <- x[dt$row]
        dt$IN <- dt$x == x_case
        dt <- dt[dt$IN, ]
      }
    rsets[[i]] <- dt$row
  }
  names(rsets) <- nrows_cases_
  rsets_2 <- list()
  for (i in 1:length(rsets)) {
    rsets_2[[i]] <- rsets[[order(failtimes)[i]]]
    names(rsets_2)[i] <- names(rsets)[order(failtimes)[i]]
  }
  return(rsets_2)
}
