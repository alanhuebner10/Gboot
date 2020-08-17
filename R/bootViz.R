#' Produce visualizations for boostrap results
#'
#' This function creates histograms or density plots for results from CalcGTheoryCI
#' @param .result A matrix returned from CalcGTheoryCI.
#' @param type Visualization produced: histogram or density plot. Defaults to histogram.
#' @param bins The number of bins for histograms
#' @return A ggplot object containing small multiples.
#' @examples
#' \dontrun{
#' data("Brennan.3.1")
#' set.seed(123)
#' test_boot <- CalcGTheoryCI(Data = Brennan.3.1, B = 1000, type = "pi")
#' bootViz(test_boot, type = "density")
#' }
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density facet_wrap vars theme_minimal %+%
#' @importFrom stats reshape
#' @export
bootViz <- function(.result = NULL, type = c("histogram", "density"), bins = 30) {
  if(is.null(.result)) stop("The .result argument has no default.\nPlease specify an object in the specified format")

  value <- variable <- run <- NULL

  df <- as.data.frame(.result)

  dfMelt <- reshape(df, varying = list(names(df)),
                    v.names = "value",
                    idvar = "run",
                    timevar = "variable",
                    times = names(df),
                    ids = rownames(df),
                    direction = "long", sep = "")

  if(length(type) > 1) {
    type <- "histogram"
  }

  switch(type,
         "histogram" = {
           ggplot(dfMelt, aes(value)) +
             geom_histogram(bins = bins) +
             facet_wrap(vars(variable), scales = "free") +
             theme_minimal()
         },
         "density" = {
           ggplot(dfMelt, aes(value)) +
             geom_density() +
             facet_wrap(vars(variable), scales = "free") +
             theme_minimal()
         })
}
