#' Create linear random effects model design matrix for nested simulated data
#'
#' This function produces a linear random effects model design matrix for simulating data from a two-facet crossed design.
#' @param np Sample size for persons
#' @param ni Sampe size for facet 1
#' @param nr Sample size for facet 2
#' @return A design matrix which is then given to the sim_study_2nested() function
#' @export
construct_DesMat_2nested <- function(np, ni, nr){
  sim_data <- data.frame(matrix(0, np*ni*nr), ncol = 4)
  colnames(sim_data) <- c("Person", "Fac1", "Fac2", "Score")
  sim_data$Person <- rep(1:np, each = ni*nr)
  sim_data$Fac1 <- rep(1:ni, times = np*nr)
  sim_data$Fac2 <- rep(1:nr, each = ni, times = np)
  return(sim_data)
}
