#' ETL Sensor Acquisition Data
#'
#' This function queries the simulation's MongoDB C2SimulationMessage collection,
#'   transforms and unnestes it, and finally writes it to the PostgreSQL tables
#'   created by the `createModSimDb` function.
#'
#' @author Neil Kester, \email{nkester1@@jhu.edu}
#'
#' @param mongoConnParamThis is a two element named list including the "mongoUri"
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
#' @importFrom dplyr rename mutate
etlSensorAcq <- function(mongoConnParam,pgConnParam,designPoint,batchSize){

  requireNamespace(package = "magrittr")

  { # Complete the MongoDB Connection Parameters ----

    mongoConnParam[["collection"]] <- "AcquireModel.event.C2SimulationMessage"

    mongoConnParam[["query"]] <- sprintf("{\"designPoint\": \"%s\", \"event.messageData.javaClass\": \"sensorproto.SensorModel$DetectedTarget\"}",
                                         designPoint)

    mongoConnParam[["fields"]] <- "{\"runId\": 1,\"runTime\": 1, \"designPoint\": 1,\"iteration\": 1, \"time\": 1, \"event.receiverId\": 1, \"event.senderId\": 1,\"event.messageData.any.sensorDetection\": 1}"

  } # close Complete the MongoDB Connection Parameters

  { # Iterate

    message("Beginning Iteration")

    mongoConn <- mongolite::mongo(url = mongoConnParam$mongoUri,
                                  db = mongoConnParam$mongoDb,
                                  collection = mongoConnParam$collection)

    numRecs <- mongoConn$count(query = mongoConnParam$query)

    it <- mongoConn$iterate(query = mongoConnParam$query,
                            fields = mongoConnParam$fields)

    pgConn <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                             host = pgConnParam$pgHost,
                             port = pgConnParam$pgPort,
                             user = pgConnParam$pgUser,
                             password = pgConnParam$pgPass,
                             dbname = pgConnParam$pgDb)

    rdx <- 1

    while(!is.null(x <- it$one())){
      #Testing
      #for(idx in 1:1000){

      message(paste0("Acq Row: ",as.character(rdx),
                     "  % comp: ",round(x = rdx/numRecs,
                                        digits = 3)*100))

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

      temp_query <- fillTableQuery(data = temp,tableName = "\"sensorAcqState\"",serial = "DEFAULT")

      DBI::dbSendQuery(conn = pgConn,
                       statement = temp_query)

      rdx <- rdx + 1

    }

    DBI::dbDisconnect(conn = pgConn)
    mongoConn$disconnect()

  } # close Iterate

  # { # Extract ----
  #
  #   message("Extracting data from the MongoDB")
  #
  #   sensorAcqData <- modSim::sensorAcquisition(mongoUri = mongoConnParam[["mongoUri"]],
  #                                              mongoDb = mongoConnParam[["mongoDb"]],
  #                                              mongoCollection = mongoConnParam[["collection"]],
  #                                              mongoQuery = mongoConnParam[["query"]],
  #                                              mongoFields = mongoConnParam[["fields"]],
  #                                              recursiveUnnests = c("event",
  #                                                                   "messageData",
  #                                                                   "any",
  #                                                                   "sensorDetection"))
  #
  # } # close Extract
  #
  # { # Transform and Load ----
  #
  #   message("Transforming and loading sensorAcqState Data!")
  #
  #   sensorAcqData <- sensorAcqData %>%
  #     dplyr::rename("id" = "_id",
  #                   "time_ms" = "time") %>%
  #     dplyr::mutate(time_s = time_ms/1000,
  #                   sensorAcqState_pkId = NA)
  #
  #   batch_fillAndWrite(data = sensorAcqData,
  #                      pgConnParam = pgConnParam,
  #                      tableName = "sensorAcqState",
  #                      batchSize = batchSize,
  #                      database = "PostgreSQL")
  #
  # } # close Transform and Load

  message("Sensor Acq ETL Complete")

} # close fct_low_etlSensorAcq
