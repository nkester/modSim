#' Query ModSim Materialized Views
#'
#' This function queries the materialized views created in Step 1 and filled in
#'  Step 2.
#'
#' @author Neil Kester, \email{nkester1@@jhu.edu}
#'
#' @param connParamList A five element named list containing the following elements:
#'  "pgHost", "pgPort", "pgUser", "pgPass", and "pgDb".
#' @param sensorForce This is either "BLUEFORCE" or "REDFORCE".
#' @param targetForce This is either "BLUEFORCE" or "REDFORCE".
#' @param designPoint This is a string name describing the designPoint as stored
#'  in the PostgreSQL datanase.
#'
#' @return This returns a named list of two elements, the results from the LOS
#'  and the ACQ Materialized View queries.
#'
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#'
#' @export queryData
#'
#' @note Location: ./R/fct_step3_low_queryData.R
#' @note RMarkdown location: ./inst/step3_plotting/Step3_plotting.Rmd
queryData <- function(pgConnParam,
                      sensorForce,
                      targetForce,
                      designPoint){

  pgConn <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                           host = pgConnParam[["pgHost"]],
                           port = pgConnParam[["pgPort"]],
                           user = pgConnParam[["pgUser"]],
                           password = pgConnParam[["pgPass"]],
                           dbname = pgConnParam[["pgDb"]])

  query_losMatViewData <- sprintf("
      SELECT \"time_s\", \"sensorShortName\", \"iteration\", \"designPoint\", \"hasLOS\",\"targetShortName\"
      FROM los_sensor_target_pairs_materialized
      WHERE \"sensorForce\" = '%s' AND \"targetForce\" = '%s' AND \"designPoint\" = '%s'
      ",
                                  sensorForce,
                                  targetForce,
                                  designPoint)

  query_acqMatViewData <- sprintf("
      SELECT \"time_s\", \"sensorShortName\", \"iteration\", \"designPoint\", \"hasAcq\",\"targetShortName\"
      FROM acq_sensor_target_pairs_materialized
      WHERE \"sensorForce\" = '%s' AND \"targetForce\" = '%s' AND \"designPoint\" = '%s'
      ",
                                  sensorForce,
                                  targetForce,
                                  designPoint)

  losMatViewData <- DBI::dbGetQuery(conn = pgConn,
                                    statement = query_losMatViewData)

  acqMatViewData <- DBI::dbGetQuery(conn = pgConn,
                                    statement = query_acqMatViewData)

  DBI::dbDisconnect(conn = pgConn)

  rm(pgConn)

  return(list("losMatViewData" = losMatViewData,
              "acqMatViewData" = acqMatViewData))

} # close low_queryData
