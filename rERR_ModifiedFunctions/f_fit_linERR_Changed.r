# canvis per Jordi F:
#	- a línia 76 he afegit la condició "& data$n_pe != -1" per no contar la primera línia fictícia de cada subjecte com a tac
f_fit_linERR_jf <- function (formula, data, rsets, n_lin_vars, n_loglin_vars, id_name, 
	time_name) 
{
    formula_sv <- formula[[2]]
    v_id <- eval(parse(text = paste0("data$", id_name)))
    v_n_pe <- eval(parse(text = paste0("data$", "n_pe")))
    v_entry <- eval(parse(text = paste0("data$", formula_sv[[2]])))
    v_exit <- eval(parse(text = paste0("data$", formula_sv[[3]])))
    v_outcome <- eval(parse(text = paste0("data$", formula_sv[[4]])))
    v_time <- eval(parse(text = paste0("data$", time_name)))
    nrow_cases <- which(v_outcome == 1)
    failtimes <- v_exit[nrow_cases]
    id_cases <- v_id[nrow_cases]
    nrow_cases <- names(rsets)
    beta <- rep(0.1, n_lin_vars + n_loglin_vars)
    beta <- as.list(beta)
    names(beta) <- paste0("x", 1:length(beta))
    add.arguments <- function(f, n) {
        t = paste("arg <- alist(", paste(sapply(1:n, function(i) paste("x", 
            i, "=", sep = "")), collapse = ","), ")", sep = "")
        formals(f) <- eval(parse(text = t))
        f
    }
    p.est <- function() {
        beta3 <- vector()
        for (i in 1:length(beta)) {
            beta3[i] <- eval(parse(text = paste0("x", i)))
        }
        beta_2 <- as.list(beta3)
        beta_2 <- unlist(lapply(beta_2, as.numeric))
        constr_ind <- as.integer(0)
        res <- .Call("llhood_linear_v3", beta_2, length(beta_2), 
            length(rsets), rsets, nrseti, data, nrow_cases, n_lin_vars, 
            n_loglin_vars, constr_ind)
        res <- res[-length(res)]
        res1 <- log(res)
        return(-sum(log(res)))
    }
    p.est <- add.arguments(p.est, length(beta))
    data <- lapply(data, as.numeric)
    rsets <- lapply(rsets, as.numeric)
    nrseti <- lapply(rsets, length)
    nrseti <- lapply(nrseti, as.numeric)
    n_lin_vars <- as.integer(n_lin_vars)
    n_loglin_vars <- as.integer(n_loglin_vars)
    nrow_cases <- lapply(nrow_cases, as.numeric)
    constr <- vector()
    if (n_lin_vars > 0) 
        for (i in 0:(n_lin_vars - 1)) {
            beta_2 <- unlist(lapply(beta, as.numeric))
            res <- .Call("llhood_linear_v3", beta_2, length(beta_2), 
                length(rsets), rsets, nrseti, data, nrow_cases, 
                n_lin_vars, n_loglin_vars, as.integer(i))
            constr[i + 1] <- res[length(res)]
        }
    llim <- c(constr, rep(-Inf, length(beta) - n_lin_vars))
    llim2 <- c(constr, rep(0.1, length(beta) - n_lin_vars))
    reduction <- 0.999
    p.est_num <- function(x) {
        x <- as.list(x)
        names(x) <- paste0("x", 1:length(x))
        return(do.call(p.est, x))
    }
    suppressWarnings(while (any(is.na(diag(numDeriv::hessian(p.est_num, 
        llim2 * reduction))))) reduction <- reduction^2)
    res <- mle(minuslogl = p.est, start = beta, method = "L-BFGS-B", 
        lower = llim * reduction)
    names <- names(data)[8:(8 + n_lin_vars + n_loglin_vars - 
        1)]
    names(attr(res, "coef")) <- names
    names(attr(res, "fullcoef")) <- names
    attr(res, "desc") <- list(desc = list(n_events = length(rsets), 
        n_observations = length(data$n_pe[which(data$n_pe != 
            0 & data$n_pe != -1)]), n_subjects = length(data$n_pe[which(data$n_pe == 
            1)])))
    if (n_lin_vars > 0) {
        lrt_ci_mat <- matrix(nrow = n_lin_vars, ncol = 2)
        rownames(lrt_ci_mat) <- names[1:n_lin_vars]
        colnames(lrt_ci_mat) <- c("lower .95", "upper .95")
        for (i in 1:n_lin_vars) {
            attr(res, "details")
            beta_good <- attr(res, "coef")
            llik <- attr(res, "details")$value
            prob <- 0.95
            llim <- constr * 0.999
            g <- function(x) {
                aux <- beta_good
                aux <- as.list(aux)
                aux[[i]] <- x
                names(aux) <- paste0("x", 1:length(aux))
                y <- do.call(p.est, aux)
                return(-y + llik + qchisq(prob, 1)/2)
            }
            l1_t <- try(uniroot(g, lower = llim[i], upper = beta_good[i], 
                extendInt = "no")$root)
            l2_t <- try(uniroot(g, lower = beta_good[i], upper = 1e+05, 
                extendInt = "yes")$root)
            if (is.numeric(l1_t)) 
                l1 <- l1_t
            else l1 <- -Inf
            if (is.numeric(l2_t)) 
                l2 <- l2_t
            else l2 <- Inf
            if (beta_good[i] > l2) 
                l2 <- Inf
            lrt_ci_mat[i, 1] <- l1
            lrt_ci_mat[i, 2] <- l2
        }
        attr(res, "lrt_ci") <- lrt_ci_mat
        sum <- summary(res)
        coef <- attr(sum, "coef")[1:n_lin_vars, 1]
        se <- attr(sum, "coef")[1:n_lin_vars, 2]
        coef_mat <- matrix(nrow = n_lin_vars, ncol = 4)
        rownames(coef_mat) <- names[1:n_lin_vars]
        colnames(coef_mat) <- c("coef", "se(coef)", "z", "Pr(>|z|)")
        coef_mat[, 1] <- coef
        coef_mat[, 2] <- se
        coef_mat[, 3] <- coef_mat[, 1]/coef_mat[, 2]
        coef_mat[, 4] <- 2 * (1 - pnorm(abs(coef_mat[, 3])))
        attr(res, "lin_coef") <- coef_mat
    }
    if (n_loglin_vars > 0) {
        sum <- summary(res)
        coef <- attr(sum, "coef")[(n_lin_vars + 1):(n_lin_vars + 
            n_loglin_vars), 1]
        se <- attr(sum, "coef")[(n_lin_vars + 1):(n_lin_vars + 
            n_loglin_vars), 2]
        coef_mat <- matrix(nrow = n_loglin_vars, ncol = 5)
        rownames(coef_mat) <- names[(n_lin_vars + 1):(n_lin_vars + 
            n_loglin_vars)]
        colnames(coef_mat) <- c("coef", "exp(coef)", "se(coef)", 
            "z", "Pr(>|z|)")
        coef_mat[, 1] <- coef
        coef_mat[, 2] <- exp(coef)
        coef_mat[, 3] <- se
        coef_mat[, 4] <- coef_mat[, 1]/coef_mat[, 3]
        coef_mat[, 5] <- 2 * (1 - pnorm(abs(coef_mat[, 4])))
        conf_mat <- matrix(nrow = n_loglin_vars, ncol = 4)
        colnames(conf_mat) <- c("exp(coef)", "exp(-coef)", "lower .95", 
            "upper .95")
        rownames(conf_mat) <- names[(n_lin_vars + 1):(n_lin_vars + 
            n_loglin_vars)]
        conf_mat[, 1] <- exp(coef)
        conf_mat[, 2] <- exp(-coef)
        conf_mat[, 3] <- exp(coef_mat[, 1] - qnorm(0.975, mean = 0, 
            sd = 1) * se)
        conf_mat[, 4] <- exp(coef_mat[, 1] + qnorm(0.975, mean = 0, 
            sd = 1) * se)
        attr(res, "wald_ci") <- conf_mat
        attr(res, "loglin_coef") <- coef_mat
    }
    lrt_stat <- 2 * (-attr(res, "details")$value - sum(log(1/unlist(lapply(rsets, 
        length)))))
    lrt_pval <- 1 - pchisq(lrt_stat, 1, lower.tail = T)
    attr(res, "formula") <- formula
    attr(res, "lrt") <- list(lrt_stat = lrt_stat, lrt_pval = lrt_pval)
    attr(res, "se") <- attr(summary(res), "coef")[, 2]
    attr(res, "AIC") <- AIC(res)
    class(res) <- "rERR"
    return(res)
}
