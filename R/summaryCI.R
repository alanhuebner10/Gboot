#' @export
summaryCI <- function(.result, ConfLevel, rounding) {
  # GstudyEstimates (means and standard error)
  means <- colMeans(.result)
  sds <- apply(.result, 2, sd)
  gstudy_est <- round(rbind(c(means[1:7], sds[1:7])), rounding)
  colnames(gstudy_est) <- c("p Var", "i Var", "o Var", "pi Var", "po Var",
                            "io Var", "ResidVar", "p Var_SE", "i Var_SE", "o Var_SE", "pi Var_SE",
                            "po Var_SE", "io Var_SE", "ResidVar_SE")
  rownames(gstudy_est) <- c("value")
  # CIs
  lb_g <- round(apply(.result, 2, quantile, probs = (1 - ConfLevel)/2, names = F), rounding)
  ub_g <- round(apply(.result, 2, quantile, probs = (1 - (1 - ConfLevel)/2), names = F), rounding)
  gstudy_cis_vec <- noquote(paste0("(", lb_g, ", ", ub_g, ")"))
  gstudy_cis <- rbind(gstudy_cis_vec[1:7])
  colnames(gstudy_cis) <- c("p Var", "i Var", "o Var", "pi Var", "po Var",
                            "io Var", "ResidVar")
  rownames(gstudy_cis) <- c("CI")
  # DstudyEstimates (mean and standard error)
  dstudy_est <- round(rbind(c(means[8:10], sds[8:10])), 4)
  colnames(dstudy_est) <- c("AbsErrVar", "GenCoef", "DepCoef", "AbsErrVar_SE", "GenCoef_SE",
                            "DepCoef_SE")
  rownames(dstudy_est) <- c("value")
  # DstudyCIs
  dstudy_cis <- rbind(gstudy_cis_vec[8:10])
  colnames(dstudy_cis) <- c("AbsErrVar_SE", "GenCoef_SE", "DepCoef_SE")
  rownames(dstudy_cis) <- c("CI")
  # Return four matrices in a named list
  return(list(Gstudy_Estimates = gstudy_est, Gstudy_Intervals = gstudy_cis, Dstudy_Estimates = dstudy_est,
              Dstudy_Intervals = dstudy_cis))
}
