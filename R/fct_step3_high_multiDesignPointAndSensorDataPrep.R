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
#'  and designPoint level.
#'
#' @export Step3_multiDesingPointAndSensorDataPrep
#'
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom tidyr drop_na
Step3_multiDesingPointAndSensorDataPrep <- function(pgConnParam,
                                                    sensorForce,
                                                    targetForce,
                                                    designPoints,
                                                    sensors){

  { # set up

    consolidateGraphDataIteration <- tibble::tibble(time_s=NA,
                                                    designPoint=NA,
                                                    iteration=NA,
                                                    count=NA,
                                                    sensorShortName=NA)

    consolidateGraphDataMean <- tibble::tibble(time_s=NA,
                                               designPoint=NA,
                                               sensorShortName=NA,
                                               count_mean=NA,
                                               count_var=NA,
                                               count_sd=NA,
                                               count_se=NA)

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

      consolidateGraphDataIteration <- dplyr::bind_rows(temp$LOSbyIteration,
                                                        temp$ACQbyIteration,
                                                        consolidateGraphDataIteration)

      consolidateGraphDataMean <- dplyr::bind_rows(temp$LOSmeanByDesignPoint,
                                                   temp$ACQmeanByDesignPoint,
                                                   consolidateGraphDataMean)

      rm(temp)

    } # close sensors loop

    rm(designPointData)

  } # close designPoints loop

  consolidateGraphDataIteration <- tidyr::drop_na(consolidateGraphDataIteration)

  consolidateGraphDataMean <- tidyr::drop_na(consolidateGraphDataMean)

  return(list("byIteration" = consolidateGraphDataIteration,
              "byDesignPoint" = consolidateGraphDataMean))

} # close Step3_multiDesingPointAndSensorDataPrep function
