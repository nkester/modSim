#' Query and Prepare Data for Multiple Design Points and Sensors
#'
#' This function executes and integrates the two Step 3 low functions across a
#'  number of designPoints and sensor entities.
#'
#' @author Neil Kester, \email{nkester1@@jhu.edu}
#'
#' @param connParamList A five element named list containing the following elements:
#'  "pgHost", "pgPort", "pgUser", "pgPass", and "pgDb".
#' @param sensorForce This is either "BLUEFORCE" or "REDFORCE".
#' @param targetForce This is either "BLUEFORCE" or "REDFORCE".
#' @param designPoints This is a character vector of names describing designPoints
#'  as stored in the PostgreSQL datanase.
#' @param sensors This is a character vector of sensor entity names you want to
#'  structure data for.
#'
#' @return This returns a two element named list of consolidated data at the iteration
#'  and designPoint level. DesignPoint level data is aggregated across the iterations
#'  executed for each designPoint. Metrics calculated for that aggregation include
#'  the mean, variance, standard deviation, and standard error.
#'
#' @export Step3_multiDesingPointAndSensorDataPrep
#'
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom tidyr drop_na
#'
#' @note Location: ./R/fct_step3_high_multiDesignPointAndSensorDataPrep.R
#' @note RMarkdown location: ./inst/step3_plotting/Step3_plotting.Rmd
Step3_multiDesingPointAndSensorDataPrep <- function(pgConnParam,
                                                    sensorForce,
                                                    targetForce,
                                                    designPoint,
                                                    sensors){

  { # set up

    consolidateGraphDataIteration <- tibble::tibble(time_s = NA,
                                                    designPoint = NA,
                                                    iteration = NA,
                                                    count = NA,
                                                    sensorShortName = NA)

    consolidateGraphDataMean <- tibble::tibble(time_s = NA,
                                               designPoint = NA,
                                               sensorShortName = NA,
                                               count_mean = NA,
                                               count_var = NA,
                                               count_sd = NA,
                                               count_se = NA)

    consolidateGraphDataTargetColumn <- tibble::tibble(time_s = NA,
                                                       sensorShortName = NA,
                                                       designPoint = NA,
                                                       targetShortName = NA,
                                                       count_mean = NA,
                                                       count_var = NA,
                                                       count_sd = NA,
                                                       count_se = NA,
                                                       type = NA)

  } # close set up

  for(designPoint in designPoints){

    message(paste0("\nWorking on ",
                   designPoint,
                   " designPoint.\n"))

    { # Query

      designPointData <- queryData(pgConnParam = pgConnParam,
                                   sensorForce = sensorForce,
                                   targetForce = targetForce,
                                   designPoint = designPoint)

    } # close Query

    for(sensor in sensors){

      message(paste0("\nWorking on ",
                     sensor,
                     " sensor in ",
                     designPoint,
                     " designPoint.\n"))

      temp <- graphDataPrep(losMatViewData = designPointData$losMatViewData,
                            acqMatViewData = designPointData$acqMatViewData,
                            sensor = sensor)

      hist <- targetColumnGraphData(losMatViewData = designPointData$losMatViewData,
                                    acqMatViewData = designPointData$acqMatViewData,
                                    sensor = sensor)

      consolidateGraphDataIteration <- dplyr::bind_rows(temp$LOSbyIteration,
                                                        temp$ACQbyIteration,
                                                        consolidateGraphDataIteration)

      consolidateGraphDataMean <- dplyr::bind_rows(temp$LOSmeanByDesignPoint,
                                                   temp$ACQmeanByDesignPoint,
                                                   consolidateGraphDataMean)

      consolidateGraphDataTargetColumn <- dplyr::bind_rows(consolidateGraphDataTargetColumn,
                                                           hist)

      rm(temp,hist)

    } # close sensors loop

    rm(designPointData)

  } # close designPoints loop

  consolidateGraphDataIteration <- tidyr::drop_na(consolidateGraphDataIteration)

  consolidateGraphDataMean <- tidyr::drop_na(consolidateGraphDataMean)

  consolidateGraphDataTargetColumn <- tidyr::drop_na(consolidateGraphDataTargetColumn)

  return(list("byIteration" = consolidateGraphDataIteration,
              "byDesignPoint" = consolidateGraphDataMean,
              "byTarget" = consolidateGraphDataTargetColumn))

} # close Step3_multiDesingPointAndSensorDataPrep function
