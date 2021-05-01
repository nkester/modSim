#' ETL Line of Sight Data
#'
#' This function queries the LOSTargetStatus collection in the simulation
#'  MongoDB, extracts the required fields, and then writes it to the PostgreSQL
#'  tables created by the `creatModSimDb` function. This operates as an iterator
#'  with MongoDB so it executes one record at a time.
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
#'
#' @return This returns messages to the console updating the user on the function's
#'   status but returns no information.
#'
#' @importFrom mongolite mongo
#' @importFrom RPostgreSQL PostgreSQL
#' @importFrom DBI dbConnect dbSendQuery dbDisconnect
#' @importFrom tibble tibble
#'
#' @note Location: ./R/fct_step2_low_etlLosData.R
etlLosData <- function(mongoConnParam,
                       pgConnParam,
                       designPoint){

  requireNamespace(package = "magrittr")

  { # Complete the MongoDB Connection Parameters ----

    mongoConnParam[["collection"]] <- "AcquireModel.event.LOSTargetStatus"

    mongoConnParam[["query"]] <- sprintf("{\"designPoint\": \"%s\"}",
                                         designPoint)

    mongoConnParam[["fields"]] <- "{\"_id\": true, \"runId\": true, \"runTime\": true,
  \"designPoint\": true, \"iteration\": true, \"time\": true,\"event\": true}"

  } # close Complete the MongoDB Connection Parameters

  { # Iterate

    message("Beginning Iteration")

    #> Connect to the MongoDB
    mongoConn <- mongolite::mongo(url = mongoConnParam$mongoUri,
                                  db = mongoConnParam$mongoDb,
                                  collection = mongoConnParam$collection)

    #> Return the number of records present in the query (for status)
    numRecs <- mongoConn$count(query = mongoConnParam$query)

    #> Create an iterator (cursor) in the MongoDB
    it <- mongoConn$iterate(query = mongoConnParam$query,
                            fields = mongoConnParam$fields)

    #> Connect to the PostgreSQL Database
    pgConn <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                             host = pgConnParam$pgHost,
                             port = pgConnParam$pgPort,
                             user = pgConnParam$pgUser,
                             password = pgConnParam$pgPass,
                             dbname = pgConnParam$pgDb)

    rdx <- 1

    #> The iterator returns `null` when it reaches the last record.
    while(!is.null(x <- it$one())){

      message(paste0("LOS Row: ",
                     as.character(rdx),
                     "  is ",
                     round(x = rdx/numRecs,
                           digits = 3)*100,
                     "% complete!")
      )

      x <- it$one()

      temp <- tibble::tibble("losState_pkid" = NA,
                             "id" = x$`_id`,
                             "runId" = x$runId,
                             "runTime" = x$runTime,
                             "designPoint" = x$designPoint,
                             "iteration" = x$iteration,
                             "time_ms" = x$time,
                             "time_s" = x$time/1000,
                             "sensorId" = x$event$sensorId,
                             "targetId" = x$event$targetId,
                             "hasLOS" = x$event$hasLOS)

      temp_query <- fillTableQuery(data = temp,
                                   tableName = "\"losState\"",
                                   serial = "DEFAULT")

      DBI::dbSendQuery(conn = pgConn,
                       statement = temp_query)

      rdx <- rdx + 1

    }

    #> Disconnect from the databases when the job is complete.
    DBI::dbDisconnect(conn = pgConn)
    mongoConn$disconnect()

  } # close Iterate

  message("LOS Complete")

} # close fct_low_etlLosData
