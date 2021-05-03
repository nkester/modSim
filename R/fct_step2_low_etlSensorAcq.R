#' ETL Sensor Acquisition Data
#'
#' This function queries the C2SimulationMessage collection in the simulation
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
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @note Location: ./R/fct_step2_low_etlSensorAcq.R
#' @note RMarkdown location: ./inst/step2_queryMongoAndFillPg/Step2_queryMongoAndFillPg.Rmd
etlSensorAcq <- function(mongoConnParam,
                         pgConnParam,
                         designPoint){

  { # Complete the MongoDB Connection Parameters ----

    mongoConnParam[["collection"]] <- "AcquireModel.event.C2SimulationMessage"

    mongoConnParam[["query"]] <- sprintf("{\"designPoint\": \"%s\", \"event.messageData.javaClass\": \"sensorproto.SensorModel$DetectedTarget\"}",
                                         designPoint)

    mongoConnParam[["fields"]] <- "{\"runId\": 1,\"runTime\": 1, \"designPoint\": 1,\"iteration\": 1, \"time\": 1, \"event.receiverId\": 1, \"event.senderId\": 1,\"event.messageData.any.sensorDetection\": 1}"

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

    rdx <- list(1)

    #> Establish a progress bar
    pb <- utils::txtProgressBar(min = 0,
                                max = numRecs,
                                style = 3)

    #> The iterator returns `null` when it reaches the last record.
    while(!is.null(x <- it$one())){

      utils::setTxtProgressBar(pb = pb,
                               value = rdx[[1]])

      # message(paste0("Acq Row: ",
      #                as.character(rdx),
      #                "  is ",
      #                round(x = rdx/numRecs,
      #                      digits = 3)*100,
      #                "% complete!")
      # )

      x <- it$one()

      temp <- tibble::tibble("sensorAcqState_pkid" = NA,
                             "id" = x$`_id`,
                             "runId" = x$runId,
                             "runTime" = x$runTime,
                             "designPoint" = x$designPoint,
                             "iteration" = x$iteration,
                             "time_ms" = x$time,
                             "time_s" = x$time/1000,
                             "receiverId" = x$event$receiverId,
                             "senderId" = x$event$senderId,
                             "sensorId" = x$event$messageData$any$sensorDetection$sensorId,
                             "entityId" = x$event$messageData$any$sensorDetection$entityId,
                             "targetId" = x$event$messageData$any$sensorDetection$targetId,
                             "detectionLevel" = x$event$messageData$any$sensorDetection$detectionLevel,
                             "previousDetectionLevel" = x$event$messageData$any$sensorDetection$previousDetectionLevel,
                             "timeToDetection" = x$event$messageData$any$sensorDetection$timeToDetection)

      temp_query <- fillTableQuery(data = temp,
                                   tableName = "\"sensorAcqState\"",
                                   serial = "DEFAULT")

      DBI::dbSendQuery(conn = pgConn,
                       statement = temp_query)

      rdx[[1]] <- rdx[[1]] + 1

    }

    #> Clean up the progress bar object
    close(pb)
    rm(pb)

    #> Disconnect from the database when the job is complete.
    DBI::dbDisconnect(conn = pgConn)
    mongoConn$disconnect()

  } # close Iterate

  message("Sensor Acq ETL Complete")

} # close fct_low_etlSensorAcq
