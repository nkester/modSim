#' ETL Sensor To Entity Mapping Tables
#'
#' This function queries the simulation's MongoDB, extracting specific information
#'   from the state.sensors collection, transforms it by unnesting and relating
#'   the fields. Finally, it writes the resulting data to tables in PostgreSQL
#'   created by the `createModSimDb` function.
#'
#' @author Neil Kester, \email{nkester1@@jhu.edu}
#'
#' @param mongoConnParam This is a two element named list including the "mongoUri"
#'   which includes the username and password and a single character string and
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
#' @export etlSensorToEntityMappingTables
#'
#' @importFrom dplyr distinct mutate case_when rename
#' @importFrom stringr str_replace_all str_detect str_extract
etlSensorToEntityMappingTables <- function(mongoConnParam,pgConnParam,designPoint){

  requireNamespace(package = "magrittr")

  { # Complete the MongoDB Connection Parameters ----

    mongoConnParam[["collection"]] <- "AcquireModel.state.sensors"

    mongoConnParam[["query"]] <- sprintf("{\"designPoint\": \"%s\"}",
                                         designPoint)

    mongoConnParam[["fields"]] <- "{\"_id\": 1, \"runId\": 1, \"runTime\": 1,
  \"designPoint\": 1, \"iteration\": 1, \"time\": 1, \"state.sensorId\": 1,
  \"state.entityId\": 1, \"state.acquireSensorType\": 1,
  \"state.magnification\": 1, \"state.status.source\": 1}"

  } # close Complete the MongoDB Connection Parameters

  { # Extract ----

    { # Query MongoDb and unnest information about sensors and entities ----

      message("Extracting data from MongoDB")

      entitySensorMapping <- modSim::mapSensorsAndEntities(mongoUri = mongoConnParam[["mongoUri"]],
                                                           mongoDb = mongoConnParam[["mongoDb"]],
                                                           mongoCollection = mongoConnParam[["collection"]],
                                                           mongoFields = mongoConnParam[["fields"]],
                                                           mongoQuery = mongoConnParam[["query"]])

      metaData <- entitySensorMapping$UnnestedSensorState %>%
        dplyr::distinct(.data = .,
                        runId,
                        designPoint,
                        iteration)

    } # close Query MongoDb and unnest information about sensors and entities section

  } # close Extract section

  { # Transform and Load ----

    { # sensorDescription ----

      message("Transforming and loading sensorDescription data.")

      entitySensorMapping$SensorDescription <- entitySensorMapping$SensorDescription %>%
        dplyr::mutate(.data = .,
                      designPoint = dplyr::distinct(metaData,
                                                    designPoint)[[1]],
                      sensorId_pkId = NA)

      query_sensorDescription <- fillTableQuery(data = entitySensorMapping$SensorDescription,
                                                tableName = paste0("\"sensorDescription\" (",
                                                                   paste0("\"",
                                                                          names(entitySensorMapping$SensorDescription),
                                                                          "\"",
                                                                          collapse = ","),
                                                                   ")"),
                                                serial = "DEFAULT")

      #> This is required because pg uses the unquoted `DEFAULT` for its auto-incrementing columns.
      # query_sensorDescription <- stringr::str_replace_all(string = query_sensorDescription,
      #                                                     pattern = "NULL",
      #                                                     replacement = "DEFAULT")

      sendPgFillTableQuery(query = query_sensorDescription,
                           host = pgConnParam[["pgHost"]],
                           port = pgConnParam[["pgPort"]],
                           user = pgConnParam[["pgUser"]],
                           password = pgConnParam[["pgPass"]],
                           dbname = pgConnParam[["pgDb"]])

      rm(query_sensorDescription)

    } # close sensorDescription section

    { # entityIdToName ----

      message("Transforming and loading entityIdToName data.")

      entitySensorMapping$EntityIdToName <- entitySensorMapping$EntityIdToName %>%
        dplyr::mutate(.data = .,
                      designPoint = dplyr::distinct(metaData,
                                                    designPoint)[[1]],
                      force = dplyr::case_when(
                        stringr::str_detect(string = source,
                                            pattern = "^(?i)(blueforce)") ~ "BLUEFORCE",
                        stringr::str_detect(string = source,
                                            pattern = "^(?i)(redforce)") ~ "REDFORCE",
                        TRUE ~ "OTHER"
                      ),
                      shortName = stringr::str_extract(string = source,
                                                       pattern = "[^/]*$"),
                      entityId_pkId = NA)

      query_entityIdToName <- fillTableQuery(data = entitySensorMapping$EntityIdToName,
                                             tableName = paste0("\"entityIdToName\" (",
                                                                paste0("\"",
                                                                       names(entitySensorMapping$EntityIdToName),
                                                                       "\"",
                                                                       collapse = ","),
                                                                ")"))

      #> This is required because pg uses the unquoted `DEFAULT` for its auto-incrementing columns.
      query_entityIdToName <- stringr::str_replace_all(string = query_entityIdToName,
                                                       pattern = "NULL",
                                                       replacement = "DEFAULT")

      sendPgFillTableQuery(query = query_entityIdToName,
                           host = pgConnParam[["pgHost"]],
                           port = pgConnParam[["pgPort"]],
                           user = pgConnParam[["pgUser"]],
                           password = pgConnParam[["pgPass"]],
                           dbname = pgConnParam[["pgDb"]])

      rm(query_entityIdToName)

    } # close entityIdToName section

    { # sensorToEntityId ----

      message("Transforming and loading sensorToEntityId data.")

      entitySensorMapping$SensorToEntityId <- entitySensorMapping$SensorToEntityId %>%
        dplyr::mutate(.data = .,
                      designPoint = dplyr::distinct(metaData,
                                                    designPoint)[[1]],
                      sensorToEntityId_pkId = NA)

      query_sensorToEntityId <- fillTableQuery(data = entitySensorMapping$SensorToEntityId,
                                               tableName = paste0("\"sensorToEntityId\" (",
                                                                  paste0("\"",
                                                                         names(entitySensorMapping$SensorToEntityId),
                                                                         "\"",
                                                                         collapse = ","),
                                                                  ")"),
                                               serial = "DEFAULT")

      #> This is required because pg uses the unquoted `DEFAULT` for its auto-incrementing columns.
      # query_sensorToEntityId <- stringr::str_replace_all(string = query_sensorToEntityId,
      #                                                    pattern = "NULL",
      #                                                    replacement = "DEFAULT")

      sendPgFillTableQuery(query = query_sensorToEntityId,
                           host = pgConnParam[["pgHost"]],
                           port = pgConnParam[["pgPort"]],
                           user = pgConnParam[["pgUser"]],
                           password = pgConnParam[["pgPass"]],
                           dbname = pgConnParam[["pgDb"]])

      rm(query_sensorToEntityId)

    } # close sensorToEntityId section

    { # unnestedSensorState ----

      message("Transforming and loading unnestedSensorState data.")

      #> This is the full set of un-nested data from the original query. While not truly "raw", this could be considered the original data set.

      entitySensorMapping$UnnestedSensorState <- entitySensorMapping$UnnestedSensorState %>%
        dplyr::rename(.data = .,
                      "id" = "_id",
                      "time_ms" = "time")

      query_unnestedSensorState <- fillTableQuery(data = entitySensorMapping$UnnestedSensorState,
                                                  tableName = paste0("\"unnestedSensorState\" (",
                                                                     paste0("\"",
                                                                            names(entitySensorMapping$UnnestedSensorState),
                                                                            "\"",
                                                                            collapse = ","),
                                                                     ")"),
                                                  serial = "DEFAULT")

      sendPgFillTableQuery(query = query_unnestedSensorState,
                           host = pgConnParam[["pgHost"]],
                           port = pgConnParam[["pgPort"]],
                           user = pgConnParam[["pgUser"]],
                           password = pgConnParam[["pgPass"]],
                           dbname = pgConnParam[["pgDb"]])

      rm(query_unnestedSensorState)

    } # close unnestedSensorState section

  } # close Transform and Load section

} # close fct_low_etlSensorToEntityMappingTables
