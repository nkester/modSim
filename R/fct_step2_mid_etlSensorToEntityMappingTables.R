#' ETL Sensor To Entity Mapping Tables
#'
#' This function queries the simulation's MongoDB, extracting specific information
#'   from the state.sensors collection, and it writes the resulting data to tables
#'   in PostgreSQL created by the `createModSimDb` function.
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
#' @importFrom dplyr distinct mutate case_when rename
#' @importFrom stringr str_replace_all str_detect str_extract
#'
#' @note Location: ./R/fct_step2_mid_etlSensorToEntityMappingTables.R
etlSensorToEntityMappingTables <- function(mongoConnParam,
                                           pgConnParam,
                                           designPoint,
                                           batchSize){

  requireNamespace(package = "magrittr")

  { # Complete the MongoDB Connection Parameters ----

    mongoConnParam[["collection"]] <- "AcquireModel.state.sensors"

  } # close Complete the MongoDB Connection Parameters

  { # Extract ----

    { # Query MongoDb and unnest information about sensors and entities ----

      message("Extracting data from MongoDB")

      entitySensorMapping <- mapSensorsAndEntities(mongoUri = mongoConnParam[["mongoUri"]],
                                                   mongoDb = mongoConnParam[["mongoDb"]],
                                                   mongoCollection = mongoConnParam[["collection"]],
                                                   designPoint)

      metaData <- entitySensorMapping$metaData

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

      batch_fillAndWrite(data = entitySensorMapping$SensorDescription,
                         pgConnParam = pgConnParam,
                         tableName = "sensorDescription",
                         batchSize = batchSize,
                         database = "PostgreSQL")

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

      batch_fillAndWrite(data = entitySensorMapping$EntityIdToName,
                         pgConnParam = pgConnParam,
                         tableName = "entityIdToName",
                         batchSize = batchSize,
                         database = "PostgreSQL")


    } # close entityIdToName section

    { # sensorToEntityId ----

      message("Transforming and loading sensorToEntityId data.")

      entitySensorMapping$SensorToEntityId <- entitySensorMapping$SensorToEntityId %>%
        dplyr::mutate(.data = .,
                      designPoint = dplyr::distinct(metaData,
                                                    designPoint)[[1]],
                      sensorToEntityId_pkId = NA)

      batch_fillAndWrite(data = entitySensorMapping$SensorToEntityId,
                         pgConnParam = pgConnParam,
                         tableName = "sensorToEntityId",
                         batchSize = batchSize,
                         database = "PostgreSQL")

    } # close sensorToEntityId section

  } # close Transform and Load section

} # close fct_step2_mid_etlSensorToEntityMappingTables
