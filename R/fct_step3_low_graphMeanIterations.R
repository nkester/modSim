#' Graph the mean difference between LOS and ACQ
#'
#' @param meanIterationData Means found over iterations
#'
#' @return
#' @export Step3_graphMeanIterations
#'
#' @importFrom ggplot2 ggplot geom_point aes theme_minimal labs facet_grid
#'
#' @note Location: ./R/fct_step3_low_graphMeanIterations.R
#' @note RMarkdown location: ./inst/step3_plotting/Step3_plotting.Rmd
Step3_graphMeanIterations <- function(meanIterationData){

  p <- ggplot2::ggplot(meanDiffByDesignPointAndTime) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = time_s,
                                               y = diff_mean)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "time (s)",
                  y = "Mean Difference Between LOS and ACQ",
                  title = "Difference between a sensor's LOS and ACQ records by time step") +
    ggplot2::facet_grid(sensorShortName ~ designPoint)

  return(p)

} # close Step3_graphMeanIterations function
