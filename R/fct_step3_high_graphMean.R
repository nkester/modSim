#' Graph the Mean Data
#'
#' Return a consistent formatted graph of the data.
#'
#' @author Neil Kester, \email{nkester1@@jhu.edu}
#'
#' @param graphData This is the mean data resulting from `multiDesignPointAndSensorDataPrep`
#'  function
#' @param losColor A color string
#' @param acqColor A color string
#' @param errorbars Boolean
#'
#' @return A ggplot2 plot
#'
#' @export Step3_graphMean
#'
#' @importFrom ggplot2 ggplot geom_point aes scale_color_manual theme_minimal theme
#'  element_text labs facet_grid geom_errorbar
#'
#' @note Location: ./R/fct_step3_high_graphMean.R
#' @note RMarkdown location: ./inst/step3_plotting/Step3_plotting.Rmd
Step3_graphMean <- function(graphData,
                            losColor = "blue",
                            acqColor = "black",
                            errorbars=TRUE){

  p <- ggplot2::ggplot(data = graphData) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = time_s,
                                               y = count_mean,
                                               color = type)) +
    ggplot2::scale_color_manual(values = c(losColor,
                                           acqColor),
                                name = "Type") +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0)) +
    ggplot2::labs(x = "time (s)",
                  y = "Mean Target Count",
                  title = "Line of Sight and Positive Sensor Acquisitions Over Time",
                  subtitle = "Error bars show mean +- standard error") +
    ggplot2::facet_grid(sensorShortName ~ designPoint)

  if(errorbars){

    p <- p +
      ggplot2::geom_errorbar(mapping = ggplot2::aes(x = time_s,
                                                    ymax = count_mean + count_se,
                                                    ymin = count_mean - count_se,
                                                    color = type),
                             alpha = 0.25)

  } # close if errorbars

  return(p)

} # Step3_graphMean function
