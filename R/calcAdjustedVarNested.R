#' Calculate adjusted variance components for the nested model.
#'
#' This is an internal function for CalcGTheoryCINested
#' @param .data Internal input
#' @param .var_p Internal input
#' @param .var_o Internal input
#' @param .var_po Internal input
#' @param .var_io Internal input
#' @param .var_pio Internal input
#' @param type Internal input
#' @export
#'
calcAdjustedVarNested <- function(.data, .var_p, .var_o, .var_po,
                            .var_io, .var_pio, type) {
  # Calculate adjusted variance components, given data and original components
  np <- nlevels(.data$p)
  ni <- nlevels(.data$i)
  no <- nlevels(.data$o)
  switch(type,
         "p" = {
           adj_var_p <- (np/(np - 1)) * .var_p
           adj_var_o <- .var_o - (1/(np - 1)) * .var_po
           adj_var_po <- (np/(np - 1)) * .var_po
           adj_var_io <- .var_io - (1/(np - 1)) * .var_pio
           adj_var_pio <- (np/(np - 1)) * .var_pio
         },
         "o" = {
           adj_var_p <- .var_p - (1/(no - 1)) * .var_po
           adj_var_o <- (no/(no - 1)) * .var_o
           adj_var_po <- (no/(no - 1)) * .var_po
           adj_var_io <- .var_io
           adj_var_pio <- .var_pio
         },
         "po" = {
           adj_var_p <- (np/(np - 1)) * .var_p - (np/((np - 1) * (no - 1))) * .var_po
           adj_var_o <- (no/(no - 1)) * .var_o - (no/((np - 1) * (no - 1))) * .var_po
           adj_var_po <- (np * no/((np - 1) * (no - 1))) * .var_po
           adj_var_io <- .var_io - (1/(np - 1)) * .var_pio
           adj_var_pio <- np/(np - 1) * .var_pio
         },
         "io" = {
           adj_var_p <- .var_p - (1/(no * (ni - 1))) * .var_pio
           adj_var_o <- (no/(no - 1)) * .var_o - 1/(ni - 1) * .var_io
           adj_var_po <- (no/(no - 1)) * .var_po - 1/(ni - 1) * .var_pio
           adj_var_io <- (ni/(ni - 1)) * .var_io
           adj_var_pio <- (ni/(ni - 1)) * .var_pio
         },
         "pio" = {
           adj_var_p <- (np/(np - 1)) * .var_p - (np/((np - 1) * (no - 1))) * .var_po
           adj_var_o <- (no/(no - 1)) * .var_o - (no/((np - 1) * (no - 1))) * .var_po - 1/(ni - 1) * .var_io + 1/((np - 1) * (ni - 1)) * .var_pio
           adj_var_po <- (np * no/((np - 1) * (no - 1))) * .var_po - np/((np - 1) * (ni - 1)) * .var_pio
           adj_var_io <- (ni/(ni - 1)) * .var_io - (ni/((np - 1) * (ni - 1))) * .var_po
           adj_var_pio <- np * ni/((np - 1) * (ni - 1)) * .var_pio
         })
  return(c(adj_var_p, adj_var_o, adj_var_po, adj_var_io, adj_var_pio))
}
