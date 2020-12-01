#' Generates simulated data set for two-facet nested design.
#'
#' This function generates simulated data for the two-facet nested design.
#' @param np Sample size for persons for simulated data.
#' @param ni Sample size for Facet 1 (nested within Facet 2) for simulated data.
#' @param nr Sample size for Facet 2 for simulated data.
#' @param sig_p Generating (i.e. population) value for person standard deviation.
#' @param sig_r Generating (i.e. population) value for Facet 2 standard deviation.
#' @param sig_pr Generating (i.e. population) value for person X Facet 2 interaction standard deviation.
#' @param sig_i.r Generating (i.e. population) value for Facet 1 : Facet 2 standard deviation.
#' @param sig_pi.r Generating (i.e. population) value for residual standard deviation.
#'
#' @return A data frame in long format with columns Person, Facet 1, Facet 2, and simulated Scores
#' @examples
#' \dontrun{
#' #Generate a single data set with sample sizes and parameters as in Table 1 of Tong and Brennan (2004),
#' # i.e. 100 subjects taking 40 items total, with 20 items nested within each rater.
#' sim_dat_2nested(np = 100, ni = 20, nr = 2,
#'                  sig_p = 4, sig_r = 1,
#'                  sig_pr = sqrt(2), sig_i.r = sqrt(7), sig_pi.r = sqrt(208))
#'
#' }
#' @references Tong, Y., & Brennan, R. L. (2004). Bootstrap techniques for estimating variability in generalizability theory. (Report No. 15).  Center for Advanced Studies in Measurement and Assessment, University of Iowa.
#' @export
sim_dat_2nested <- function(np, ni, nr, sig_p, sig_r, sig_pr, sig_i.r, sig_pi.r){
  #Create design matrix
  sim_data <- construct_DesMat_2nested(np, ni, nr)
  #Generate random effects
  Zp <- rnorm(np, 0, 1)
  Zr <- rnorm(nr, 0, 1)
  Zpr <- matrix(rnorm(np*nr, 0, 1), nrow = np, ncol = nr)
  Zi.r <- matrix(rnorm(ni*nr, 0, 1), nrow = ni, ncol = nr)
  #Simulate scores
  score_index <- 1
  for (p in 1:np){
    for (r in 1:nr){
      for (i in 1:ni){
        sim_data$Score[score_index] <- sig_p*Zp[p] + sig_r*Zr[r] + sig_pr*Zpr[p, r] +
                                       sig_i.r*Zi.r[i, r] + sig_pi.r*rnorm(1)
        score_index <- score_index + 1
      }
    }
  }
  return(sim_data)
}
