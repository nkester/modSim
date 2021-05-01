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
                                  mongoCollection){#,
                                  #mongoFields,
                                  #mongoQuery){

  # sensorState1 <- mongoUnnest(mongoUri = mongoUri,
  #                                    mongoDb = mongoDb,
  #                                    mongoCollection = mongoCollection,
  #                                    mongoFields = mongoFields,
  #                                    mongoQuery = mongoQuery,
  #                                    unnestCols = "state")
  #
  # mongoConnection <- mongolite::mongo(url = mongoUri,
  #                                     db = mongoDb,
  #                                     collection = mongoCollection)
  #
  # aggState <- mongoConnection$aggregate(pipeline = "[{\"$match\":{\"designPoint\": \"WASP1_high_high_20210426\"}},{\"$group\":{\"_id\": {\"designPoint\": \"$designPoint\",\"runId\": \"$runId\",\"iteration\": \"$iteration\"}}}]")
  # tibble::tibble(aggState)
  # mongoData <- mongoConnection$iterate(query = mongoQuery,
  #                                   fields = mongoFields)
  #
  # mongoConnection$disconnect()
  #
  # sensorState <- mongoUnnest(mongoData = sensorState,
  #                                    unnestCols = "status")

  mongoConnection <- mongolite::mongo(url = mongoUri,
                                      db = mongoDb,
                                      collection = mongoCollection)

  { # NEW SENSOR DESCRIPTION ----

    sensorDescription <- mongoConnection$aggregate(pipeline = "[{\"$match\":{\"designPoint\": \"WASP1_high_high_20210426\"}},{\"$group\":{\"_id\": {\"sensorId\": \"$state.sensorId\",\"acquireSensorType\": \"$state.acquireSensorType\",\"magnification\": \"$state.magnification\"}}}]")
    names(sensorDescription) <- "id"
    sensorDescription <- tibble::tibble(sensorDescription$id)

  } # close NEW SENSOR DESCRIPTION section

  { # OLD SENSOR DESCRIPTION ----

    # sensorDescription <- dplyr::distinct(.data = sensorState,
    #                                      sensorId,
    #                                      acquireSensorType,
    #                                      magnification)

  } # close OLD SENSOR DESCRIPTION section

  { # NEW ENTITY ID TO NAME ----

    entityIdToName <- mongoConnection$aggregate(pipeline = "[{\"$match\":{\"designPoint\": \"WASP1_high_high_20210426\"}},{\"$group\":{\"_id\": {\"entityId\": \"$state.entityId\",\"source\": \"$state.status.source\"}}}]")
    names(entityIdToName) <- "id"
    entityIdToName <- tibble::tibble(entityIdToName$id)

  } # close NEW ENTITY ID TO NAME section


  { # OLD ENTITY ID TO NAME ----

    # entityIdToName <- dplyr::distinct(.data = x,#sensorState,
    #                                   entityId,
    #                                   source)

  } # close OLD ENTITY ID TO NAME section

  { # NEW SENSOR TO ENTITY ----

    sensorToEntity <- mongoConnection$aggregate(pipeline = "[{\"$match\":{\"designPoint\": \"WASP1_high_high_20210426\"}},{\"$group\":{\"_id\": {\"entityId\": \"$state.entityId\",\"sensorId\": \"$state.sensorId\"}}}]")
    names(sensorToEntity) <- "id"
    sensorToEntity <- tibble::tibble(sensorToEntity$id)

  } # close NEW SENSOR TO ENTITY section

  { # OLD SENSOR TO ENTITY -----

  # sensorToEntity <- dplyr::distinct(.data = sensorState,
  #                                   sensorId,
  #                                   entityId)

  } # close OLD SENSOR TO ENTITY section

  { # NEW META DATA ----

    metaData <- mongoConnection$aggregate(pipeline = "[{\"$match\":{\"designPoint\": \"WASP1_high_high_20210426\"}},{\"$group\":{\"_id\": {\"runId\": \"$runId\",\"designPoint\": \"$designPoint\",\"iteration\": \"$iteration\"}}}]")
    names(metaData) <- "id"
    metaData <- tibble::tibble(metaData$id)

  } # close NEW META DATA section

  return(list("SensorDescription" = sensorDescription,
              "EntityIdToName" = entityIdToName,
              "SensorToEntityId" = sensorToEntity,
              "metaData" = metaData))

} # close mapSensorsAndEntities function
