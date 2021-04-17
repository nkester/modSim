#' Structure Sensor-Target Acquisitions
#'
#' This function queries the MongoDB and un-nests the C2 Simulation Message
#'  collection. This is unique because all of the nests are recursive meaning
#'  they must be performed as a loop.
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
#' @param recursiveUnnests This is a character vector of sequential unnests to
#'  perform. These are executed in the order they are listed.
#'
#' @return This returns a tibble where every row is a sensorID to targetID pair
#'  and contains the detectionLevel as well as the previousDetectionLevel.
#' @export sensorAcquisition
sensorAcquisition<-function(mongoUri,
                            mongoDb,
                            mongoCollection,
                            mongoQuery,
                            mongoFields,
                            recursiveUnnests){

  sensorAcqData <- mongoUnnest(mongoUri = mongoUri,
                               mongoDb = mongoDb,
                               mongoCollection = mongoCollection,
                               mongoQuery = mongoQuery,
                               mongoFields = mongoFields)

  for(recursiveUnnest in recursiveUnnests){

    sensorAcqData <- mongoUnnest(mongoData = sensorAcqData,
                                 unnestCols = recursiveUnnest)

  }

  return(sensorAcqData)

} # close sensorAcquisition
