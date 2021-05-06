#' Calculate and Produce Boxplot of data across iterations.
#'
#' @param iterationData raw data without summarization
#'
#' @return plot and data
#' @export Step3_boxPlotOfDifferences
#'
#' @importFrom dplyr select mutate
#' @importFrom tidyr pivot_wider
#' @importFrom ggplot2 ggplot geom_boxplot aes theme_minimal theme element_text
#'
#' @note Location: ./R/fct_step3_low_boxPlotOfDifferences.R
#' @note RMarkdown location: ./inst/step3_plotting/Step3_plotting.Rmd
Step3_boxPlotOfDifferences <- function(iterationData){

  DiffByDesignPoint <- iterationData %>%
    dplyr::select(time_s,designPoint,iteration,sensorShortName,count,type) %>%
    tidyr::pivot_wider(names_from = type,values_from = count) %>%
    dplyr::mutate(diff = LOS - ACQ)

  p <- ggplot2::ggplot(DiffByDesignPoint) +
    ggplot2::geom_boxplot(mapping = ggplot2::aes(x = designPoint,y = diff)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0))

  return(list(DiffByDesignPoint,
              p))

}
