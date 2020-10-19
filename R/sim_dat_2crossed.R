#' Generates simulated data set for two-facet crossed design.
#'
#' This function generates simulated data for the two-facet crossed design.
#' @param np Sample size for persons for simulated data.
#' @param ni Sample size for Facet 1 for simulated data.
#' @param nr Sample size for Facet 2 for simulated data.
#' @param sig_p Generating (i.e. population) value for person standard deviation.
#' @param sig_i Generating (i.e. population) value for Facet 1 deviation.
#' @param sig_r Generating (i.e. population) value for Facet 2 deviation.
#' @param sig_pi Generating (i.e. population) value for person X Facet 1 interaction standard deviation.
#' @param sig_pr Generating (i.e. population) value for person X Facet 2 interaction standard deviation.
#' @param sig_ir Generating (i.e. population) value for Facet 1 X Facet 2 standard deviation.
#' @param sig_pir Generating (i.e. population) value for residual standard deviation.
#'
#' @return A data frame in long format with columns Person, Facet 1, Facet 2, and simulated Scores
#' @examples
#' \dontrun{
#' #Generate a single data set with sample sizes and parameters as in Table 3 of Tong and Brennan (2006)
#' sim_dat_2crossed(np = 100, ni = 20, nr = 2,
#'                  sig_p = 4, sig_i =  2, sig_r = 1,
#'                  sig_pi = 8, sig_pr = sqrt(2), sig_ir = sqrt(3), sig_pir = 12)
#'
#' }
#' @references Tong, Y., & Brennan, R. L. (2006). Bootstrap techniques for estimating variability in generalizability theory. (Report No. 15).  Center for Advanced Studies in Measurement and Assessment, University of Iowa.
#' @export
sim_dat_2crossed <- function(np, ni, nr, sig_p, sig_i, sig_r,
                                      sig_pi, sig_pr, sig_ir, sig_pir) {
  #Create design matrix
  sim_data <- construct_DesMat_2crossed(np, ni, nr)
  #Generate random effects
  Zp <- rnorm(np)
  Zi <- rnorm(ni)
  Zr <- rnorm(nr)
  Zpi <- matrix(rnorm(np*ni), nrow = np, ncol = ni)
  Zpr <- matrix(rnorm(np*nr), nrow = np, ncol = nr)
  Zir <- matrix(rnorm(np*ni), nrow = ni, ncol = nr)
  score_index <- 1
  for (p in 1:np) {
    for (i in 1:ni) {
      for (r in 1:nr) {
        Score_pir <- sig_p*Zp[p] + sig_i*Zi[i] + sig_r*Zr[r] +
          sig_pi*Zpi[p,i] + sig_pr*Zpr[p,r] + sig_ir*Zir[i,r] + sig_pir*rnorm(1)
        sim_data$Score[score_index] <- Score_pir
        score_index <- score_index + 1
      }
    }
  }
  return(sim_data)
}
