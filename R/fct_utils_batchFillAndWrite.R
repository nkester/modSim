#' Utility Batch Fill and Write
#'
#' This is a utility function used in modSim to take a set of data, break it up
#'  into batches and write it to the required PostgreSQL database.
#'
#' @author Neil Kester, \email{nkester1@@jhu.edu}
#'
#' @param data A tibble of data
#' @param pgConnParam The connection strings required to connect to the PostgreSQL
#'  database of choice.
#' @param tableName The table of the PostgreSQL table this data is to be inserted
#'  into. It should include `\\"` around names requiring preservation of case.
#'  Optionally: It can include the SQL Field name specification to deal with a
#'  tibble that may not be ordered the same as the SQL Table. This should look
#'  like: `"\"tableName\" (\"fieldOneName\",\"fieldTwoName\")"`
#' @param batchSize How many records to execute at a time.
#' @param database What type of database is it going into? Options are: PostgreSQL
#'  or SQLite.
#'
#' @export batch_fillAndWrite
#'
#' @return Returns nothing
#'
#' @note Location: ./R/fct_utils_batchFillAndWrite.R
#' @note RMarkdown location: ./inst/step2_queryMongoAndFillPg/Step2_queryMongoAndFillPg.Rmd
batch_fillAndWrite <- function(data,
                               pgConnParam,
                               tableName,
                               batchSize=100,
                               database = "PostgreSQL"){

  if(database == "PostgreSQL"){

    serial <- "DEFAULT"

  }else if(database == "SQLite"){

    serial <- "NULL"

  }else{

    stop("The database parameter must be either \"SQLite\" or \"PostgreSQL\"")

  }


  startSize <- nrow(data)

  while(nrow(data) != 0){

    if(nrow(data)<batchSize){

      query_data <- fillTableQuery(data = data[1:nrow(data),],
                                   tableName = paste0("\"",
                                                      tableName,
                                                      "\" (",
                                                      paste0("\"",
                                                             names(data),
                                                             "\"",
                                                             collapse = ","),
                                                      ")"),
                                   serial = serial)

      sendPgFillTableQuery(query = query_data,
                           host = pgConnParam[["pgHost"]],
                           port = pgConnParam[["pgPort"]],
                           user = pgConnParam[["pgUser"]],
                           password = pgConnParam[["pgPass"]],
                           dbname = pgConnParam[["pgDb"]])

      data <- data[-(1:nrow(data)),]

      rm(query_data)

    }else{

      query_data <- fillTableQuery(data = data[1:batchSize,],
                                   tableName = paste0("\"",
                                                      tableName,
                                                      "\" (",
                                                      paste0("\"",
                                                             names(data),
                                                             "\"",
                                                             collapse = ","),
                                                      ")"),
                                   serial = serial)

      sendPgFillTableQuery(query = query_data,
                           host = pgConnParam[["pgHost"]],
                           port = pgConnParam[["pgPort"]],
                           user = pgConnParam[["pgUser"]],
                           password = pgConnParam[["pgPass"]],
                           dbname = pgConnParam[["pgDb"]])

      data <- data[-(1:batchSize),]

      rm(query_data)

    } # close else

    message(paste0((nrow(data)/startSize)*100,"% complete with ",tableName," table!"))

  }


} # batch_fillAndWrite
