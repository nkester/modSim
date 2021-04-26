#' Map Sensors and Entities
#'
#' This function accepts the same inputs as the `modSim::mongoUnnest` function
#'  minus the `unnestCols` parameter. It executes the requested query which is
#'  expected to be from the `AcquireModel.state.sensors` collection.
#'
#' @author Neil Kester, \email{nkester1@@jhu.edu}
#'
#' @param mongoUri This is a double quoted character string of the connection
#'  object required for the simulation's MongoDB.
#' @param mongoDb This is a double quoted character string of the database name
#'  containing the required files.
#' @param mongoCollection This is a double quoted character string of the mongo
#'  collection within the named database containing the required information.
#' @param mongoQuery This is a double quoted character string containing the json
#'  query to pass to the MongoDB. Note that users should escape nested double
#'  quotes with the \ escape character.
#' @param mongoFields This is a double quoted json object of collection fields
#'  that should be returned (annoted with `true`) and those that should not be
#'  returned (annoted with `false`).
#'
#' @return This function returns a four element named list of tibbles that
#'  describe all sensors and entities, their IDs and description.
#'
#' @importFrom dplyr distinct
#'
#' @export mapSensorsAndEntities
mapSensorsAndEntities <- function(mongoUri,
                                  mongoDb,
                                  mongoCollection,
                                  mongoFields,
                                  mongoQuery){

  sensorState <- mongoUnnest(mongoUri = mongoUri,
                                     mongoDb = mongoDb,
                                     mongoCollection = mongoCollection,
                                     mongoFields = mongoFields,
                                     mongoQuery = mongoQuery,
                                     unnestCols = "state")

  sensorState <- mongoUnnest(mongoData = sensorState,
                                     unnestCols = "status")

  sensorDescription <- dplyr::distinct(.data = sensorState,
                                       sensorId,
                                       acquireSensorType,
                                       magnification)

  entityIdToName <- dplyr::distinct(.data = sensorState,
                                    entityId,
                                    source)

  sensorToEntity <- dplyr::distinct(.data = sensorState,
                                    sensorId,
                                    entityId)

  return(list("SensorDescription" = sensorDescription,
              "EntityIdToName" = entityIdToName,
              "SensorToEntityId" = sensorToEntity,
              "UnnestedSensorState" = sensorState))

} # close mapSensorsAndEntities function
