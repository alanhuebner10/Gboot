#' Compute confidence intervals for G-theory quantities for the crossed model
#'
#' This function computes and stores the raw bootstrap samples for G-theory variance components and coefficients.
#' @param Data Data in long format in the following order: person, facet1, facet2, score
#' @param B Number of bootstrap iterations
#' @param type Type of bootstrap sampling scheme as described in Tong & Brennan (2004)
#' @return A matrix of bootstrap values which is then given to the summaryCI() function
#' @examples
#' \dontrun{
#' data("Brennan.3.1")
#' CalcGTheoryCI(Data = Brennan.3.1, B = 1000, type = "pi")
#' }
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom iterators icount
#' @export
CalcGTheoryCI <- function(Data = NULL, B = 1000,
                          type = c("p", "i", "o", "pi", "po", "io", "pio")) {

  if(is.null(Data)) stop("The Data argument has no default.\nPlease specify an object in the specified format")

  if(ncol(Data) != 4) stop("Please ensure that your data has only 4 columns, with the columns in the specified order.")

  # source("R/calcVarComp.R")
  # source("R/calcAdjustedVar.R")
  colnames(Data) <- c("p", "i", "o", "Score")
  Data$p <- factor(Data$p)
  Data$i <- factor(Data$i)
  Data$o <- factor(Data$o)
  np <- nlevels(Data$p)
  ni <- nlevels(Data$i)
  no <- nlevels(Data$o)

  results <- NULL
  registerDoParallel(cores = (parallel::detectCores() - 1))
  r <- foreach(icount(B), .combine = rbind) %dopar% {
    switch(type,
           "p" = {
             boot_indices <- sample(1:np, np, replace = TRUE)
             boot_sample <- unlist(sapply(boot_indices, function(x) which(Data$p == x)))
             boot_data <- Data[boot_sample, ]
             # re-index person
             boot_data$p <- Data[order(Data$p), ]$p
             # Compute estimated variance components
             boot_var <- calcVarComp(boot_data)
             # Compute adjusted variance component estimates
             boot_AdjVar <- calcAdjustedVar(boot_data, boot_var[1], boot_var[2], boot_var[3], boot_var[4],
                                            boot_var[5], boot_var[6], boot_var[7], type = "p")
           },
           "i" = {
             boot_indices <- sample(1:ni, ni, replace = TRUE)
             boot_sample <- unlist(sapply(boot_indices, function(x) which(Data$i == x)))
             boot_data <- Data[boot_sample, ]
             # re-index item
             boot_data$i <- Data[order(Data$i), ]$i
             # Compute estimated variance components
             boot_var <- calcVarComp(boot_data)
             # Compute adjusted variance component estimates
             boot_AdjVar <- calcAdjustedVar(boot_data, boot_var[1], boot_var[2], boot_var[3], boot_var[4],
                                            boot_var[5], boot_var[6], boot_var[7], type = "i")
           },
           "o" = {
             boot_indices <- sample(1:no, no, replace = TRUE)
             boot_sample <- unlist(sapply(boot_indices, function(x) which(Data$o == x)))
             boot_data <- Data[boot_sample, ]
             # re-index occasion
             boot_data$o <- Data[order(Data$o), ]$o
             # Compute estimated variance components
             boot_var <- calcVarComp(boot_data)
             # Compute adjusted variance component estimates
             boot_AdjVar <- calcAdjustedVar(boot_data, boot_var[1], boot_var[2], boot_var[3], boot_var[4],
                                            boot_var[5], boot_var[6], boot_var[7], type = "o")
           },
           "pi" = {
             boot_indices_p <- sample(1:np, np, replace = TRUE)
             boot_indices_i <- sample(1:ni, ni, replace = TRUE)
             # draw person
             boot_sample_p <- unlist(sapply(boot_indices_p, function(x) which(Data$p == x)))
             boot_data_p <- Data[boot_sample_p, ]
             # re-index person
             boot_data_p$p <- Data[order(Data$p), ]$p
             # draw item
             boot_sample <- unlist(sapply(boot_indices_i, function(x) which(boot_data_p$i == x)))
             boot_data <- boot_data_p[boot_sample, ]
             # re-index item
             boot_data$i <- Data[order(Data$i), ]$i
             # Compute estimated variance components
             boot_var <- calcVarComp(boot_data)
             # Compute adjusted variance component estimates
             boot_AdjVar <- calcAdjustedVar(boot_data, boot_var[1], boot_var[2], boot_var[3], boot_var[4],
                                            boot_var[5], boot_var[6], boot_var[7], type = "pi")
           },
           "po" = {
             boot_indices_p <- sample(1:np, np, replace = TRUE)
             boot_indices_o <- sample(1:no, no, replace = TRUE)
             # draw person
             boot_sample_p <- unlist(sapply(boot_indices_p, function(x) which(Data$p == x)))
             boot_data_p <- Data[boot_sample_p, ]
             # re-index person
             boot_data_p$p <- Data[order(Data$p), ]$p
             # draw occasion
             boot_sample <- unlist(sapply(boot_indices_o, function(x) which(boot_data_p$o == x)))
             boot_data <- boot_data_p[boot_sample, ]
             # re-index item
             boot_data$o <- Data[order(Data$o), ]$o
             # Compute estimated variance components
             boot_var <- calcVarComp(boot_data)
             # Compute adjusted variance component estimates
             boot_AdjVar <- calcAdjustedVar(boot_data, boot_var[1], boot_var[2], boot_var[3], boot_var[4],
                                            boot_var[5], boot_var[6], boot_var[7], type = "po")
           },
           "io" = {
             boot_indices_i <- sample(1:ni, ni, replace = TRUE)
             boot_indices_o <- sample(1:no, no, replace = TRUE)
             # draw item
             boot_sample_i <- unlist(sapply(boot_indices_i, function(x) which(Data$i == x)))
             boot_data_i <- Data[boot_sample_i, ]
             # re-index item
             boot_data_i$i <- Data[order(Data$i), ]$i
             # draw occasion
             boot_sample <- unlist(sapply(boot_indices_o, function(x) which(boot_data_i$o == x)))
             boot_data <- boot_data_i[boot_sample, ]
             # re-index occasion
             boot_data$o <- Data[order(Data$o), ]$o
             # Compute estimated variance components
             boot_var <- calcVarComp(boot_data)
             # Compute adjusted variance component estimates
             boot_AdjVar <- calcAdjustedVar(boot_data, boot_var[1], boot_var[2], boot_var[3], boot_var[4],
                                            boot_var[5], boot_var[6], boot_var[7], type = "io")
           },
           "pio" = {
             boot_indices_p <- sample(1:np, np, replace = TRUE)
             boot_indices_i <- sample(1:ni, ni, replace = TRUE)
             boot_indices_o <- sample(1:no, no, replace = TRUE)
             # draw person
             boot_sample_p <- unlist(sapply(boot_indices_p, function(x) which(Data$p == x)))
             boot_data_p <- Data[boot_sample_p, ]
             # re-index person
             boot_data_p$p <- Data[order(Data$p), ]$p
             # draw item
             boot_sample_pi <- unlist(sapply(boot_indices_i, function(x) which(boot_data_p$i ==
                                                                                 x)))
             boot_data_pi <- boot_data_p[boot_sample_pi, ]
             # re-index item
             boot_data_pi$i <- Data[order(Data$i), ]$i
             # draw occasion
             boot_sample <- unlist(sapply(boot_indices_o, function(x) which(boot_data_pi$o == x)))
             boot_data <- boot_data_pi[boot_sample, ]
             # re-index occasion
             boot_data$o <- Data[order(Data$o), ]$o
             # Compute estimated variance components
             boot_var <- calcVarComp(boot_data)
             # Compute adjusted variance component estimates
             boot_AdjVar <- calcAdjustedVar(boot_data, boot_var[1], boot_var[2], boot_var[3], boot_var[4],
                                            boot_var[5], boot_var[6], boot_var[7], type = "pio")
           })
    ## Compute D-study coefficients
    boot_AbsErrVar <- boot_AdjVar[2]/ni + boot_AdjVar[3]/no + boot_AdjVar[4]/ni + boot_AdjVar[5]/no +
      boot_AdjVar[6]/(ni * no) + boot_AdjVar[7]/(ni * no)
    boot_GenCoef <- boot_AdjVar[1]/(boot_AdjVar[1] + boot_AdjVar[4]/ni + boot_AdjVar[5]/no +
                                      boot_AdjVar[7]/(ni * no))
    boot_DepCoef <- boot_AdjVar[1]/(boot_AdjVar[1] + boot_AbsErrVar)

    # Accumulate results such that cols 1-10 are varp,vari,varo,varpi,varpo,vario,varpio,
    # AbsErrVar,GenCoef,DepCoef
    result <- c(boot_AdjVar, boot_AbsErrVar, boot_GenCoef, boot_DepCoef)
    result
  }
  results <- rbind(results, r)
  class(results) <- append(class(results), "G_out")
  return(results)
}
