#' Map Sensors and Entities
#'
#' This function accepts the same inputs as the basic MongoDB connection information
#'  and submits aggregation pipelines to MongoDB to return information about the
#'  sensors and entities present in the specified designPoint.
#'
#' @author Neil Kester, \email{nkester1@@jhu.edu}
#'
#' @param mongoUri This is a double quoted character string of the connection
#'  object required for the simulation's MongoDB.
#' @param mongoDb This is a double quoted character string of the database name
#'  containing the required files.
#' @param mongoCollection This is a double quoted character string of the mongo
#'  collection within the named database containing the required information.
#' @param designPoint This is a single character string with the designPoint you
#'   would like to extract from the MongoDB and place into the PostgreSQL database.
#'   If multiple designPoints are required then execute this function multiple
#'   times. Note that this pulls ALL iterations executed for that designPoint.
#'
#' @return This function returns a four element named list of tibbles that
#'  describe all sensors and entities, their IDs and description.
#'
#' @importFrom tibble tibble
#' @importFrom mongolite mongo
#'
#' @note Location: ./R/fct_step2_low_mapSensorsAndEntities.R
#' @note RMarkdown location: ./inst/step2_queryMongoAndFillPg/Step2_queryMongoAndFillPg.Rmd
mapSensorsAndEntities <- function(mongoUri,
                                  mongoDb,
                                  mongoCollection,
                                  designPoint){

  mongoConnection <- mongolite::mongo(url = mongoUri,
                                      db = mongoDb,
                                      collection = mongoCollection)

  { # SENSOR DESCRIPTION ----

    pipeline_sensorDescription <- sprintf("[{\"$match\":{\"designPoint\": \"%s\"}},{\"$group\":{\"_id\": {\"sensorId\": \"$state.sensorId\",\"acquireSensorType\": \"$state.acquireSensorType\",\"magnification\": \"$state.magnification\"}}}]",
                                          designPoint)

    sensorDescription <- mongoConnection$aggregate(pipeline = pipeline_sensorDescription)
    names(sensorDescription) <- "id"
    sensorDescription <- tibble::tibble(sensorDescription$id)

  } # close SENSOR DESCRIPTION section

  { # ENTITY ID TO NAME ----

    pipeline_entityIdToName <- sprintf("[{\"$match\":{\"designPoint\": \"%s\"}},{\"$group\":{\"_id\": {\"entityId\": \"$state.entityId\",\"source\": \"$state.status.source\"}}}]",
                                       designPoint)

    entityIdToName <- mongoConnection$aggregate(pipeline = pipeline_entityIdToName)
    names(entityIdToName) <- "id"
    entityIdToName <- tibble::tibble(entityIdToName$id)

  } # close ENTITY ID TO NAME section

  { # SENSOR TO ENTITY ----

    pipeline_sensorToEntity <- sprintf("[{\"$match\":{\"designPoint\": \"%s\"}},{\"$group\":{\"_id\": {\"entityId\": \"$state.entityId\",\"sensorId\": \"$state.sensorId\"}}}]",
                                       designPoint)

    sensorToEntity <- mongoConnection$aggregate(pipeline = pipeline_sensorToEntity)
    names(sensorToEntity) <- "id"
    sensorToEntity <- tibble::tibble(sensorToEntity$id)

  } # close SENSOR TO ENTITY section

  { # META DATA ----

    pipeline_metaData <- sprintf("[{\"$match\":{\"designPoint\": \"%s\"}},{\"$group\":{\"_id\": {\"runId\": \"$runId\",\"designPoint\": \"$designPoint\",\"iteration\": \"$iteration\"}}}]",
                                 designPoint)

    metaData <- mongoConnection$aggregate(pipeline = pipeline_metaData)
    names(metaData) <- "id"
    metaData <- tibble::tibble(metaData$id)

  } # close META DATA section

  return(list("SensorDescription" = sensorDescription,
              "EntityIdToName" = entityIdToName,
              "SensorToEntityId" = sensorToEntity,
              "metaData" = metaData))

} # close mapSensorsAndEntities function
