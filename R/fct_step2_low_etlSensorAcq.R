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
#'
#' @return This returns messages to the console updating the user on the function's
#'   status but returns no information.
#'
#' @importFrom dplyr rename mutate
etlSensorAcq <- function(mongoConnParam,pgConnParam,designPoint){

  requireNamespace(package = "magrittr")

  { # Complete the MongoDB Connection Parameters ----

    mongoConnParam[["collection"]] <- "AcquireModel.event.C2SimulationMessage"

    mongoConnParam[["query"]] <- sprintf("{\"designPoint\": \"%s\", \"event.messageData.javaClass\": \"sensorproto.SensorModel$DetectedTarget\"}",
                                         designPoint)

    mongoConnParam[["fields"]] <- "{\"runId\": 1,\"runTime\": 1, \"designPoint\": 1,\"iteration\": 1, \"time\": 1, \"event.receiverId\": 1, \"event.senderId\": 1,\"event.messageData.any.sensorDetection\": 1}"

  } # close Complete the MongoDB Connection Parameters

  { # Extract ----

    message("Extracting data from the MongoDB")

    sensorAcqData <- modSim::sensorAcquisition(mongoUri = mongoConnParam[["mongoUri"]],
                                               mongoDb = mongoConnParam[["mongoDb"]],
                                               mongoCollection = mongoConnParam[["collection"]],
                                               mongoQuery = mongoConnParam[["query"]],
                                               mongoFields = mongoConnParam[["fields"]],
                                               recursiveUnnests = c("event",
                                                                    "messageData",
                                                                    "any",
                                                                    "sensorDetection"))

  } # close Extract

  { # Transform and Load ----

    message("Transforming and loading sensorAcqState Data!")

    sensorAcqData <- sensorAcqData %>%
      dplyr::rename("id" = "_id",
                    "time_ms" = "time") %>%
      dplyr::mutate(time_s = time_ms/1000,
                    sensorAcqState_pkId = NA)

    query_sensorAcqData <- fillTableQuery(data = sensorAcqData,
                                          tableName = paste0("\"sensorAcqState\" (",
                                                             paste0("\"",
                                                                    names(sensorAcqData),
                                                                    "\"",
                                                                    collapse = ","),
                                                             ")"),
                                          serial = "DEFAULT")

    #> This is required because pg uses the unquoted `DEFAULT` for its auto-incrementing columns.
    # query_sensorAcqData <- stringr::str_replace_all(string = query_sensorAcqData,
    #                                                 pattern = "NULL",
    #                                                 replacement = "DEFAULT")

    sendPgFillTableQuery(query = query_sensorAcqData,
                         host = pgConnParam[["pgHost"]],
                         port = pgConnParam[["pgPort"]],
                         user = pgConnParam[["pgUser"]],
                         password = pgConnParam[["pgPass"]],
                         dbname = pgConnParam[["pgDb"]])

  } # close Transform and Load

} # close fct_low_etlSensorAcq
