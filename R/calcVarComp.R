#' Calculate variance components.
#'
#' This is an internal function for CalcGTheoryCI
#' @param x Internal input
#' @importFrom stats aggregate
#' @export

calcVarComp <- function(x) {
  ## Calculate 2-facet variance components via ANOVA from 'long' data Get sample sizes
  np <- nlevels(x$p)
  ni <- nlevels(x$i)
  no <- nlevels(x$o)
  # Compute Sums of Squared Mean Scores (T), Brennan p.69
  xbar <- mean(x$Score)
  Tp <- no * ni * sum(aggregate(x[, 4], list(x$p), mean)[, 2]^2)
  Ti <- np * no * sum(aggregate(x[, 4], list(x$i), mean)[, 2]^2)
  To <- np * ni * sum(aggregate(x[, 4], list(x$o), mean)[, 2]^2)
  Tpi <- no * sum(aggregate(x[, 4], list(x$p, x$i), mean)[, 3]^2)
  Tpo <- ni * sum(aggregate(x[, 4], list(x$p, x$o), mean)[, 3]^2)
  Tio <- np * sum(aggregate(x[, 4], list(x$o, x$i), mean)[, 3]^2)
  Tpio <- sum(x$Score^2)
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
  return(c(var_p, var_i, var_o, var_pi, var_po, var_io, var_pio))
}
