#' ETL Line of Sight Data
#'
#' This function queries the LOSTargetStatus collection in the simulation
#'  MongoDB, transforms and un-nests it and then writes it to the PostgreSQL
#'  tables  created by the `creatModSimDb` function.
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
#' @importFrom dplyr rename mutate
#' @importFrom stringr str_replace_all
etlLosData <- function(mongoConnParam,pgConnParam,designPoint,batchSize){

  requireNamespace(package = "magrittr")

  { # Complete the MongoDB Connection Parameters ----

    mongoConnParam[["collection"]] <- "AcquireModel.event.LOSTargetStatus"

    mongoConnParam[["query"]] <- sprintf("{\"designPoint\": \"%s\"}",
                                         designPoint)

    mongoConnParam[["fields"]] <- "{\"_id\": true, \"runId\": true, \"runTime\": true,
  \"designPoint\": true, \"iteration\": true, \"time\": true,\"event\": true}"

  } # close Complete the MongoDB Connection Parameters

  { # Extract ----

    message("Extracting data from the MongoDB")

    losData <- mongoUnnest(mongoUri = mongoConnParam[["mongoUri"]],
                           mongoDb = mongoConnParam[["mongoDb"]],
                           mongoCollection = mongoConnParam[["collection"]],
                           mongoFields = mongoConnParam[["fields"]],
                           mongoQuery = mongoConnParam[["query"]],
                           unnestCols = "event")

  } # close Extract

  { # Transform and Load ----

    message("Transforming and loading losData")

    losData <- losData %>%
      dplyr::rename("id" = "_id",
                    "time_ms" = "time") %>%
      dplyr::mutate(time_s = time_ms/1000,
                    losState_pkId = NA)

    batch_fillAndWrite(data = losData,
                       pgConnParam = pgConnParam,
                       tableName = "losState",
                       batchSize = batchSize,
                       database = "PostgreSQL")

  } # close Transform and Load

} # close fct_low_etlLosData
