#' Compute confidence intervals for G-theory quantities for the nested model
#'
#' This function computes and stores the raw bootstrap samples for G-theory variance components and coefficients.
#' @param Data Data in long format in the following order: person, facet1, facet2 (facet1 is nested within facet2), score
#' @param B Number of bootstrap iterations
#' @param type Type of bootstrap sampling scheme as described in Tong & Brennan (2004)
#' @return A matrix of bootstrap values which is then given to the summaryCI() function
#' @examples
#' \dontrun{
#' data("Brennan.3.1")
#' CalcGTheoryCINested(Data = Brennan.3.1, B = 1000, type = "po")
#' }
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom iterators icount
#' @export
CalcGTheoryCINested <- function(Data = NULL, B = 1000,
                          type = c("p", "o", "po", "io", "pio")) {

  if(is.null(Data)) stop("The Data argument has no default.\nPlease specify an object in the specified format")

  if(ncol(Data) != 4) stop("Please ensure that your data has only 4 columns, with the columns in the specified order.")

  # source("R/calcVarCompNested.R")
  # source("R/calcAdjustedVarNested.R")
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
             ## Get indices for p boot-p
             boot_indices <- sample(1:np, np, replace = TRUE)
             boot_sample <- unlist(sapply(boot_indices, function(x) which(Data$p == x)))
             boot_data <- Data[boot_sample, ]
             # re-index person
             boot_data$p <- Data[order(Data$p), ]$p
             # Compute estimated variance components
             boot_var <- calcVarCompNested(boot_data)
             # Compute adjusted variance component estimates
             boot_AdjVar <- calcAdjustedVarNested(boot_data, boot_var[1], boot_var[2], boot_var[3], boot_var[4],
                                                  boot_var[5], type = "p")
           },
           "o" = {
             ## Get indices for o boot-o
             boot_indices <- sample(1:no, no, replace = TRUE)
             boot_sample <- unlist(sapply(boot_indices, function(x) which(Data$o == x)))
             boot_data <- Data[boot_sample, ]
             # re-index occasion
             boot_data$o <- Data[order(Data$o), ]$o
             # Compute estimated variance components
             boot_var <- calcVarCompNested(boot_data)
             # Compute adjusted variance component estimates
             boot_AdjVar <- calcAdjustedVarNested(boot_data, boot_var[1], boot_var[2], boot_var[3], boot_var[4],
                                                  boot_var[5], type = "o")
           },
           "po" = {
             ## Get indices for p and o
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
             boot_var <- calcVarCompNested(boot_data)
             # Compute adjusted variance component estimates
             boot_AdjVar <- calcAdjustedVarNested(boot_data, boot_var[1], boot_var[2], boot_var[3], boot_var[4],
                                                  boot_var[5], type = "po")
           },
           "io" = {
             ## Get indices for o
             boot_indices_o <- sample(1:no, no, replace = TRUE)
             # draw occasion
             boot_sample_o <- unlist(sapply(boot_indices_o, function(x) which(Data$o == x)))
             boot_data_o <- Data[boot_sample_o, ]
             # re-index occasion
             boot_data_o$o <- Data[order(Data$o), ]$o
             boot_data <- NULL
             for (t in 1:no){
               boot_indices_i <- sample(1:ni, ni, replace = TRUE)
               # draw item within each occasion
               boot_sample <- unlist(sapply(boot_indices_i, function(x) which(boot_data_o[which(boot_data_o[,'o'] == t), ]$i == x)))
               boot_data <- rbind(boot_data, boot_data_o[which(boot_data_o[,'o'] == t), ][boot_sample, ])
             }
             # re-index item
             boot_data$i <- Data[order(Data$i), ][order(Data$o), ]$i
             # Compute estimated variance components
             boot_var <- calcVarCompNested(boot_data)
             # Compute adjusted variance component estimates
             boot_AdjVar <- calcAdjustedVarNested(boot_data, boot_var[1], boot_var[2], boot_var[3], boot_var[4],
                                                  boot_var[5], type = "io")
           },
           "pio" = {
             ## Get indices for p
             boot_indices_p <- sample(1:np, np, replace = TRUE)
             # draw person
             boot_sample_p <- unlist(sapply(boot_indices_p, function(x) which(Data$p == x)))
             boot_data_p <- Data[boot_sample_p, ]
             # re-index person
             boot_data_p$p <- Data[order(Data$p), ]$p
             ## Get indices for o
             boot_indices_o <- sample(1:no, no, replace = TRUE)
             # draw occasion
             boot_sample_o <- unlist(sapply(boot_indices_o, function(x) which(boot_data_p$o == x)))
             boot_data_o <- boot_data_p[boot_sample_o, ]
             # re-index occasion
             boot_data_o$o <- Data[order(Data$o), ]$o
             boot_data <- NULL
             for (t in 1:no){
               boot_indices_i <- sample(1:ni, ni, replace = TRUE)
               # draw item within each occasion
               boot_sample <- unlist(sapply(boot_indices_i, function(x) which(boot_data_o[which(boot_data_o[,'o'] == t), ]$i == x)))
               boot_data <- rbind(boot_data, boot_data_o[which(boot_data_o[,'o'] == t), ][boot_sample, ])
             }
             # re-index item
             boot_data$i <- Data[order(Data$i), ][order(Data$o), ]$i
             # Compute estimated variance components
             boot_var <- calcVarCompNested(boot_data)
             # Compute adjusted variance component estimates
             boot_AdjVar <- calcAdjustedVarNested(boot_data, boot_var[1], boot_var[2], boot_var[3], boot_var[4],
                                                  boot_var[5], type = "pio")
           })
    ## Compute D-study coefficients, Brennan p. 16
    boot_AbsErrVar <- boot_AdjVar[4]/(ni * no) + boot_AdjVar[5]/(ni * no)
    boot_GenCoef <- (boot_AdjVar[1] + boot_AdjVar[3]/no)/(boot_AdjVar[1] + boot_AdjVar[3]/no + boot_AdjVar[5]/(ni * no))
    boot_DepCoef <- (boot_AdjVar[1] + boot_AdjVar[3]/no)/(boot_AdjVar[1] + boot_AdjVar[3]/no + boot_AbsErrVar)

    # Accumulate results such that cols 1-8 are varp,varo,varpo,vario,varpio,
    # AbsErrVar,GenCoef,DepCoef
    result <- c(boot_AdjVar, boot_AbsErrVar, boot_GenCoef, boot_DepCoef)
    result
  }
  results <- rbind(results, r)
  class(results) <- append(class(results), "G_out")
  return(results)
}
