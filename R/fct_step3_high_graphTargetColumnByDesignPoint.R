#' Graph the Sensor - Target Column Chart, Faceted by Design Point
#'
#' This function accepts parameters, fetches data, transforms it, and produces a
#'  graph that shows the number of target entities a sensor entity has line of
#'  sight with and has acquired. This is further broken down by color that
#'  corresponds to the target's name.
#'
#' @param sensorForce This is either "BLUEFORCE" or "REDFORCE".
#' @param targetForce This is either "BLUEFORCE" or "REDFORCE".
#' @param designPoints This is a character vector of names describing designPoints
#'  as stored in the PostgreSQL datanase.
#' @param connParamList A five element named list containing the following elements:
#'  "pgHost", "pgPort", "pgUser", "pgPass", and "pgDb".
#' @param sensors This is a character vector of sensor entity names you want to
#'  structure data for.
#'
#' @return This returns a list of two elements, the data and the plot it made.
#' @export graphTargetColumnByDesignPoint
#'
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom ggplot2 ggplot geom_col aes facet_grid theme_minimal theme element_text
#'
#' @note Location: ./R/fct_step3_high_graphTargetColumnByDesignPoint.R
#' @note RMarkdown location: ./inst/step3_plotting/Step3_plotting.Rmd
graphTargetColumnByDesignPoint <- function(consolidateGraphDataTargetColumn){


  { # Produce the Plot

    p <-ggplot2::ggplot(consolidateGraphDataTargetColumn) +
      ggplot2::geom_col(mapping = ggplot2::aes(x = time_s,
                                               y = count_mean,
                                               fill = targetShortName)) +
      #ggplot2::geom_errorbar(mapping = ggplot2::aes(x = time_s,ymax = count_mean + count_se,ymin = count_mean - count_se,color = targetShortName)) +
      ggplot2::facet_grid(type ~ designPoint) +
      ggplot2::theme_minimal() +
      ggplot2::theme(title = ggplot2::element_text("LOS and ACQ broken out by DesignPoint and Target"))

  } # close Produce the Plot

  return(p)

} # close graphTargetColumnByDesign function
