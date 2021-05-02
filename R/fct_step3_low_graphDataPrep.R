#' Prepare the Queried Data for Analysis
#'
#' This takes the results from `low_queryData` and returns expanded data for each
#'  time step both at the iteration level and also aggregated to the designPoint
#'  level.
#'
#' @author Neil Kester, \email{nkester1@@jhu.edu}
#'
#' @param losMatViewData A tibble resulting from the `low_queryData` function.
#' @param acqMatViewData A tibble resulting from the `low_queryData` function.
#' @param sensor A single character string for the sensor you want to structure
#'  data for. For multiple sensors, execute this function multiple times.
#'
#' @return A four element named list of raw (by iteration) and aggregated (mean)
#'  data for line of sight and acquisition data.
#'
#' @importFrom dplyr mutate distinct_all filter group_by summarise ungroup
#'  select n left_join
#' @importFrom tidyr unnest fill replace_na
#'
#' @note Location: ./R/fct_step3_low_graphDataPrep.R
graphDataPrep <- function(losMatViewData,
                          acqMatViewData,
                          sensor){

  { # LOS Data Prep

    los <- losMatViewData %>%
      dplyr::mutate(time_s = as.numeric(time_s)) %>%
      dplyr::mutate(time_s = round(x = time_s,digits = 0)) %>%
      dplyr::distinct_all() %>%
      dplyr::filter(sensorShortName %in% sensor) %>%
      dplyr::group_by(time_s,
                      sensorShortName,
                      iteration,
                      designPoint) %>%
      dplyr::summarise(count = sum(hasLOS),
                       .groups = "keep") %>%
      dplyr::ungroup() %>%
      dplyr::select(-sensorShortName)

    los2 <- losMatViewData %>%
      dplyr::group_by(designPoint,
                      iteration) %>%
      dplyr::mutate(time_s = as.numeric(x = time_s)) %>%
      dplyr::mutate(time_s = round(x = time_s,
                                   digits = 0)) %>%
      dplyr::summarise(maxTime = max(time_s),
                       .groups = "keep") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(time_s = list(0:maxTime)) %>%
      dplyr::select(-maxTime) %>%
      tidyr::unnest(time_s) %>%
      dplyr::select(time_s,
                    designPoint,
                    iteration) %>%
      dplyr::left_join(x = .,
                       y = los,
                       by = c("time_s",
                              "designPoint",
                              "iteration")) %>%
      tidyr::fill(data = .,
                  count,
                  .direction = "down") %>%
      tidyr::replace_na(list(count = 0)) %>%
      dplyr::mutate(sensorShortName = sensor,
                    type = "LOS")

    los3 <- los2 %>%
      dplyr::group_by(time_s,designPoint,sensorShortName) %>%
      dplyr::summarise(count_mean = mean(count),
                       count_var = var(count),
                       count_sd = sd(count),
                       count_se = count_sd/sqrt(dplyr::n()),
                       type = "LOS",
                       .groups = "keep")

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
                      designPoint) %>%
      dplyr::summarise(count = sum(hasAcq),
                       .groups = "keep") %>%
      dplyr::ungroup() %>%
      dplyr::select(-sensorShortName)

    acq2 <- acqMatViewData %>%
      dplyr::group_by(designPoint,iteration) %>%
      dplyr::mutate(time_s = as.numeric(x = time_s)) %>%
      dplyr::mutate(time_s = round(x = time_s,
                                   digits = 0)) %>%
      dplyr::summarise(maxTime = max(time_s),
                       .groups = "keep") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(time_s = list(0:maxTime)) %>%
      dplyr::select(-maxTime) %>%
      tidyr::unnest(time_s) %>%
      dplyr::select(time_s,
                    designPoint,
                    iteration) %>%
      dplyr::left_join(x = .,
                       y = acq,
                       by = c("time_s",
                              "designPoint",
                              "iteration")) %>%
      tidyr::fill(data = .,
                  count,
                  .direction = "down") %>%
      tidyr::replace_na(list(count = 0)) %>%
      dplyr::mutate(sensorShortName = sensor,
                    type = "ACQ")

    acq3 <- acq2 %>%
      dplyr::group_by(time_s,designPoint,sensorShortName) %>%
      dplyr::summarise(count_mean = mean(count),
                       count_var = var(count),
                       count_sd = sd(count),
                       count_se = count_sd/sqrt(dplyr::n()),
                       type = "ACQ",
                       .groups = "keep")

  } # close ACQ Data Prep

  return(list("LOSbyIteration" = los2,
              "LOSmeanByDesignPoint" = los3,
              "ACQbyIteration" = acq2,
              "ACQmeanByDesignPoint" = acq3))

} # close graphData Prep function
