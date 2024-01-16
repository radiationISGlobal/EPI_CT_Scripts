	# canvis per Jordi F:
  # - noves línies: de 25 a 29 - afegim una 1a linia per tots els subjectes and age i dosi a 0
  # - modificació linia 33: incoporem n_pe per ordenar data, deixant nova linia n_pe = -1 la primera
  # - nova linia 38: posem les dosis dels tacs que cauen en lagged time a 0
f_to_event_table_ef_v2_jf <- function (id, start, stop, outcome, data, times, doses, covars) 
{
  id_ <- NULL
  time_aux <- NULL
  call <- match.call()
  id_name <- eval(id)
  dose_name <- eval(doses)
  stop_name <- eval(stop)
  time_name <- eval(times)
  #data$id_ <- eval(parse(text = paste0(call$data, "$", id_name)))
  #data$id_ <- eval(parse(text = paste0("data$", id_name)))
  data$id_ <- data$patientids
  data <- data %>% group_by(id_) %>% dplyr::mutate(n_pe = 1:length(id_))
  dt <- data[which(data$n_pe == 1), ]
  dt$n_pe <- 0
  dt[, which(names(dt) == dose_name)] <- 0
  dt[, which(names(dt) == time_name)] <- eval(parse(text = paste0("dt$",stop_name)))
  data[, which(names(data) == outcome)] <- 0
  data <- rbind(data, dt)
  ## jordi: add new row, unexposed
  dt <- data[which(data$n_pe == 1), ]
  dt$n_pe <- -1
  dt[, which(names(dt) == dose_name)] <- 0
  dt[, which(names(dt) == time_name)] <- 0
  data <- rbind(data, dt)
  ##
  data$time_aux <- eval(parse(text = paste0("data$", time_name)))
  #data <- plyr::arrange(data, id_, time_aux)
  data <- plyr::arrange(data, id_, time_aux, n_pe)
  data <- data[, -dim(data)[2]]
  dose_num <- eval(parse(text = paste0("data$", dose_name)))
  data$dose_num <- dose_num
  # jordi: doses to 0 in lagged CTs
  data[eval(parse(text = paste0("data$", exit_name))) - eval(parse(text = paste0("data$", time_name))) < lag, c("dose_num")] <- 0
  data <- data %>% group_by(id_) %>% dplyr::mutate(dose_cum = cumsum(dose_num))
  a <- plyr::count(data$id_)
  data$id1 <- unlist(lapply(seq_along(1:dim(dt)[1]), function(x) rep(x, 
                                                                     each = a$freq[x])))
  return(data)
}
