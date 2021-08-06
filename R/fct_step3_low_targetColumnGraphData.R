#' Target-Sensor Entity Data
#'
#' This function prepares the LOS and ACQ data at the target level by time step
#'  rather than the design point level. This allows us to understand which targets
#'  the chosen sensor has line of sight with and which it is able to acquire.
#'
#' @author Neil Kester, \email{nkester1@@jhu.edu}
#'
#' @param losMatViewData A tibble resulting from the `low_queryData` function.
#' @param acqMatViewData A tibble resulting from the `low_queryData` function.
#' @param sensor A single character string for the sensor you want to structure
#'  data for. For multiple sensors, execute this function multiple times.
#'
#' @return This function returns a single tibble of time, sensor name, iteration,
#'  design point, target name, and type (LOS or ACQ).
#'
#' @importFrom dplyr mutate distinct_all filter group_by summarise n ungroup
#'  bind_rows
#' @importFrom tidyr replace_na
#'
#' @note Location: ./R/fct_step3_low_targetColumnGraphData.R
#' @note RMarkdown location: ./inst/step3_plotting/Step3_plotting.Rmd
targetColumnGraphData <- function(losMatViewData,
                                  acqMatViewData,
                                  sensor){

  { # LOS Data Prep

    los <- losMatViewData %>%
      dplyr::mutate(time_s = as.numeric(time_s)) %>%
      dplyr::mutate(time_s = round(x = time_s,
                                   digits = 0)) %>%
      dplyr::distinct_all() %>%
      dplyr::filter(sensorShortName %in% sensor) %>%
      dplyr::group_by(time_s,
                      sensorShortName,
                      iteration,
                      designPoint,
                      targetShortName) %>%
      dplyr::summarise(count = sum(hasLOS),
                       .groups = "keep") %>%
      dplyr::ungroup() %>%
      dplyr::group_by(time_s,
                      sensorShortName,
                      designPoint,
                      targetShortName) %>%
      tidyr::replace_na(list(count = 0)) %>%
      dplyr::summarise(count_mean = mean(count),
                       count_var = var(count),
                       count_sd = sd(count),
                       count_se = count_sd/sqrt(dplyr::n()),
                       type = "LOS",
                       .groups = "keep") %>%
      dplyr::ungroup()

  } # close LOS Data Prep

  { # ACQ Data Prep

    acq <- acqMatViewData %>%
      dplyr::mutate(time_s = as.numeric(time_s)) %>%
      dplyr::mutate(time_s = round(x = time_s,
                                   digits = 0)) %>%
      dplyr::distinct_all() %>%
      dplyr::filter(sensorShortName %in% sensor) %>%
      dplyr::group_by(time_s,
                      sensorShortName,
                      iteration,
                      designPoint,
                      targetShortName) %>%
      dplyr::summarise(count = sum(hasAcq),
                       .groups = "keep") %>%
      dplyr::ungroup() %>%
      dplyr::group_by(time_s,
                      sensorShortName,
                      designPoint,
                      targetShortName) %>%
      tidyr::replace_na(list(count = 0)) %>%
      dplyr::summarise(count_mean = mean(count),
                       count_var = var(count),
                       count_sd = sd(count),
                       count_se = count_sd/sqrt(dplyr::n()),
                       type = "ACQ",
                       .groups = "keep") %>%
      dplyr::ungroup()

  } # close ACQ Data Prep

  histData <- dplyr::bind_rows(los,acq)

  return(histData)

} # close targetColumnGraphData function
