#' Calculate variance components.
#'
#' This function computes 2-facet crossed variance components using ANOVA.
#' @param Data Data in long format in the following order: person, facet1, facet2, score
#' @return A list of variance components
#' @examples
#' \dontrun{
#' data("Brennan.3.1")
#' calcVarComp(Data = Brennan.3.1)
#' }
#' @importFrom stats aggregate
#' @export

calcVarComp <- function(Data = NULL) {

  if(is.null(Data)) stop("The Data argument has no default.\nPlease specify an object in the specified format")

  if(ncol(Data) != 4) stop("Please ensure that your data has only 4 columns, with the columns in the specified order.")

  ## Calculate 2-facet variance components via ANOVA from 'long' data Get sample sizes
  colnames(Data) <- c("p", "i", "o", "Score")
  Data$p <- factor(Data$p)
  Data$i <- factor(Data$i)
  Data$o <- factor(Data$o)
  np <- nlevels(Data$p)
  ni <- nlevels(Data$i)
  no <- nlevels(Data$o)
  # Compute Sums of Squared Mean Scores (T), Brennan p.69
  xbar <- mean(Data$Score)
  Tp <- no * ni * sum(aggregate(Data[, 4], list(Data$p), mean)[, 2]^2)
  Ti <- np * no * sum(aggregate(Data[, 4], list(Data$i), mean)[, 2]^2)
  To <- np * ni * sum(aggregate(Data[, 4], list(Data$o), mean)[, 2]^2)
  Tpi <- no * sum(aggregate(Data[, 4], list(Data$p, Data$i), mean)[, 3]^2)
  Tpo <- ni * sum(aggregate(Data[, 4], list(Data$p, Data$o), mean)[, 3]^2)
  Tio <- np * sum(aggregate(Data[, 4], list(Data$o, Data$i), mean)[, 3]^2)
  Tpio <- sum(Data$Score^2)
  Tmu <- np * ni * no * xbar^2
  # Compute Sum of Squares (SS)
  SSp <- Tp - Tmu
  SSi <- Ti - Tmu
  SSo <- To - Tmu
  SSpi <- Tpi - Tp - Ti + Tmu
  SSpo <- Tpo - Tp - To + Tmu
  SSio <- Tio - Ti - To + Tmu
  SSpio <- Tpio - Tpi - Tpo - Tio + Tp + Ti + To - Tmu
  # Comupute Mean squares (MS)
  MSp <- SSp/(np - 1)
  MSi <- SSi/(ni - 1)
  MSo <- SSo/(no - 1)
  MSpi <- SSpi/((np - 1) * (ni - 1))
  MSpo <- SSpo/((np - 1) * (no - 1))
  MSio <- SSio/((ni - 1) * (no - 1))
  MSpio <- SSpio/((np - 1) * (ni - 1) * (no - 1))
  # Compute variance components
  var_pio <- MSpio
  var_p <- (MSp - MSpi - MSpo + MSpio)/(ni * no)
  var_i <- (MSi - MSpi - MSio + MSpio)/(np * no)
  var_o <- (MSo - MSpo - MSio + MSpio)/(np * ni)
  var_pi <- (MSpi - MSpio)/no
  var_po <- (MSpo - MSpio)/ni
  var_io <- (MSio - MSpio)/np
  # Return a list of seven variance components
  var_comp <- c(var_p, var_i, var_o, var_pi, var_po, var_io, var_pio)
  names(var_comp) <- c("var_p", "var_i", "var_o", "var_pi", "var_po", "var_io", "var_pio")
  return(var_comp)
}
