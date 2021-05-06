#' Find the mean difference between the # of LOS and ACQ per timestep
#'
#' @param iterationData from modSim::Step3_multiDesingPointAndSensorDataPrep
#'
#' @export Step3_meanDiffByDesignPointAndTime
#'
#' @importFrom dplyr select mutate group_by summaris n ungroup
#' @importFrom tidyr pivot_wider
#'
#' @note Location: ./R/fct_step3_low_targetColumnGraphData.R
#' @note RMarkdown location: ./inst/step3_plotting/Step3_plotting.Rmd
Step3_meanDiffByDesignPointAndTime <- function(iterationData){

  meanDiffByDesignPointAndTime <- graphData$byIteration %>%
    dplyr::select(time_s,designPoint,iteration,sensorShortName,count,type) %>%
    tidyr::pivot_wider(names_from = type,values_from = count) %>%
    dplyr::mutate(diff = LOS - ACQ) %>%
    dplyr::group_by(time_s,designPoint,sensorShortName) %>%
    dplyr::summarise(diff_mean = mean(diff),
                     diff_sd = sd(diff),
                     diff_var = var(diff),
                     diff_se = diff_sd/sqrt(dplyr::n()),
                     .groups = "keep") %>%
    dplyr::ungroup()

  return(meanDiffByDesignPointAndTime)

}
