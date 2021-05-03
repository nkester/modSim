#' Query MongoDB And Fill PostgreSQL
#'
#' This is the high level function executed in step 2 to extract data from the
#'   simulation's No-SQL MongoDB, transform it, and load it into the analytic
#'   PostgreSQL relational database created with `createModSimDb`. This is
#'   executed for one design point at a time.
#'
#' @author Neil Kester, \email{nkester1@@jhu.edu}
#'
#' @param mongoConnParam This is a two element named list including the "mongoUri"
#'   which includes the user name and password and a single character string and
#'   the "mongoDb" name as a character string.
#' @param pgConnParam A five element named list containing the following elements:
#'  "pgHost", "pgPort", "pgUser", "pgPass", and "pgDb".
#' @param designPoint This is a single character string with the designPoint you
#'   would like to extract from the MongoDB and place into the PostgreSQL database.
#'   If multiple designPoints are required then execute this function multiple
#'   times. Note that this pulls ALL iterations executed for that designPoint.
#' @param batchSize A numeric integer representing how many records you want to
#'  write to the PostgreSQL database at a time.
#'
#' @return This returns messages to the console updating the user on the function's
#'   status but returns no information.
#'
#' @export Step2_queryMongoAndFillPg
#'
#' @note Location: ./R/fct_step2_high_queryMongoAndFillPg.R
#' @note RMarkdown location: ./inst/step2_queryMongoAndFillPg/Step2_queryMongoAndFillPg.Rmd
Step2_queryMongoAndFillPg <- function(mongoConnParam,
                                      pgConnParam,
                                      designPoint,
                                      batchSize=100){

  message("Reading from MongDB Acquire.State.Sensor collection and writing to PostgreSQL sensorDescription, entityIdToName, sensorToEntityId, and unnestedSensorState tables.")

  etlSensorToEntityMappingTables(mongoConnParam = mongoConnParam,
                                 pgConnParam = pgConnParam,
                                 designPoint = designPoint,
                                 batchSize = batchSize)

  message("Reading from MongoDB AcquireModel.event.LOSTargetStatus collection and writing to PostgreSQL losState table.")

  etlLosData(mongoConnParam = mongoConnParam,
             pgConnParam = pgConnParam,
             designPoint = designPoint)

  message("Reading from MongoDb AcquireModel.event.C2SimulationMessage collection and writing to PostgreSQL sensorAcqState table.")

  etlSensorAcq(mongoConnParam = mongoConnParam,
               pgConnParam = pgConnParam,
               designPoint = designPoint)

  { # Refresh the materialized Views ----

    #> After writing all of the data for this designPoint to the PostgreSQL
    #>  database, update the materialized views so that the data is ready to be
    #>  queried. Failing to do this negates the utility of having a pre-executed
    #>  view ready to be queried.

    pgConn <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                             host = pgConnParam[["pgHost"]],
                             port = pgConnParam[["pgPort"]],
                             user = pgConnParam[["pgUser"]],
                             password = pgConnParam[["pgPass"]],
                             dbname = pgConnParam[["pgDb"]])

    DBI::dbSendQuery(conn = pgConn,
                     statement = "REFRESH MATERIALIZED VIEW los_sensor_target_pairs_materialized")

    DBI::dbSendQuery(conn = pgConn,
                     statement = "REFRESH MATERIALIZED VIEW acq_sensor_target_pairs_materialized")

    DBI::dbDisconnect(conn = pgConn)

    rm(pgConn)

  } # close Refresh the materialized Views section

  return("Complete!")

} # close Step2_queryMongoAndFillPg
