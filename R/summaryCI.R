#' Produces summary confidence intervals from CalcGTheoryCI function.
#' @param .result A matrix returned from CalcGTheoryCI.
#' @param ConfLevel The confidence level. Should be between 0 and 1, Defaults to .8.
#' @param rounding The number of rounding digits. Defaults to 4
#' @return A list of matrices.
#' @examples
#' \dontrun{
#' data("Brennan.3.1")
#' set.seed(123)
#' test_boot <- CalcGTheoryCI(Data = Brennan.3.1, B = 1000, type = "pi")
#' sumTest <- summaryCI(test_boot, 0.80, 4)
#' }
#' @importFrom stats quantile sd
#' @export
summaryCI <- function(.result = NULL, ConfLevel = .8, rounding = 4) {

  if(!"G_out" %in% class(.result)) stop("Please use the matrix returned from CalcGTheoryCI")

  if(ConfLevel > 1 | ConfLevel < 0) stop("ConfLevel must be between 0 and 1")

  # GstudyEstimates (means and standard error)
  means <- colMeans(.result)
  sds <- apply(.result, 2, sd)
  gstudy_est <- round(rbind(c(means[1:7], sds[1:7])), rounding)
  colnames(gstudy_est) <- c("p_Var", "i_Var", "o_Var", "pi_Var", "po_Var",
                            "io_Var", "ResidVar", "p_Var_SE", "i_Var_SE", "o_Var_SE", "pi_Var_SE",
                            "po_Var_SE", "io_Var_SE", "ResidVar_SE")
  rownames(gstudy_est) <- c("value")
  # CIs
  lb_g <- round(apply(.result, 2, quantile, probs = (1 - ConfLevel)/2, names = F), rounding)
  ub_g <- round(apply(.result, 2, quantile, probs = (1 - (1 - ConfLevel)/2), names = F), rounding)
  gstudy_cis_vec <- noquote(paste0("(", lb_g, ", ", ub_g, ")"))
  gstudy_cis <- rbind(gstudy_cis_vec[1:7])
  colnames(gstudy_cis) <- c("p_Var", "i_Var", "o_Var", "pi_Var", "po_Var",
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
