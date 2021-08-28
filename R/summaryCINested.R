#' Produces summary confidence intervals from CalcGTheoryCINested function.
#' @param .result A matrix returned from CalcGTheoryCINested.
#' @param ConfLevel The confidence level. Should be between 0 and 1, Defaults to .8.
#' @param rounding The number of rounding digits. Defaults to 4
#' @return A list of matrices.
#' @examples
#' \dontrun{
#' data("Brennan.3.1")
#' set.seed(123)
#' test_boot <- CalcGTheoryCINested(Data = Brennan.3.1, B = 1000, type = "po")
#' sumTest <- summaryCINested(test_boot, 0.80, 4)
#' }
#' @importFrom stats quantile sd
#' @export
summaryCINested <- function(.result = NULL, ConfLevel = .8, rounding = 4) {

  if(!"G_out" %in% class(.result)) stop("Please use the matrix returned from CalcGTheoryCI")

  if(ConfLevel > 1 | ConfLevel < 0) stop("ConfLevel must be between 0 and 1")

  # GstudyEstimates (means and standard error)
  means <- colMeans(.result)
  sds <- apply(.result, 2, sd)
  gstudy_est <- round(as.data.frame(cbind(means[1:5], sds[1:5])), rounding)
  rownames(gstudy_est) <- c("p_Var", "o_Var", "po_Var",
                            "i:o_Var", "ResidVar")
  colnames(gstudy_est) <- c("Mean", "S.E.")
  # CIs
  lb_g <- round(apply(.result, 2, quantile, probs = (1 - ConfLevel)/2, names = F), rounding)
  ub_g <- round(apply(.result, 2, quantile, probs = (1 - (1 - ConfLevel)/2), names = F), rounding)
  gstudy_cis <- as.data.frame(cbind(lb_g[1:5], ub_g[1:5]))
  colnames(gstudy_cis) <- c("LowerBound", "UpperBound")
  rownames(gstudy_cis) <- c("p_Var", "o_Var", "po_Var", "i:o_Var", "Resid_Var")
  # DstudyEstimates (mean and standard error)
  dstudy_est <- round(as.data.frame(cbind(means[6:8], sds[6:8])), rounding)
  colnames(dstudy_est) <- c("Mean", "S.E.")
  rownames(dstudy_est) <- c("AbsErrVar", "GenCoef", "DepCoef")
  # DstudyCIs
  dstudy_cis <- as.data.frame(cbind(lb_g[6:8], ub_g[6:8]))
  colnames(dstudy_cis) <- c("LowerBound", "UpperBound")
  rownames(dstudy_cis) <- c("AbsErrVar", "GenCoef", "DepCoef")
  # Return four matrices in a named list
  return(list(Gstudy_Estimates = gstudy_est, Gstudy_Intervals = gstudy_cis, Dstudy_Estimates = dstudy_est,
              Dstudy_Intervals = dstudy_cis))
}
