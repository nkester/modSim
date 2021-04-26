#' Connect and Send Query to PostgreSQL
#'
#' This is a utility function to help connect to, send, and disconnect from
#'   a PostgreSQL database.
#'
#' @author Neil Kester, \email{nkester1@@jhu.edu}
#'
#' @param query This is a character string describing a query to send to PostgreSQL.
#'   Note that this conducts a `dbSendQuery` so this is not suitable for returning
#'   data but rather to write data or send CREATE instructions.
#' @param host A character vector to the desired PostgreSQL instance.
#' @param port An integer port number the PostreSQL instance listen on. Standard
#'   is 5432.
#' @param user A character vector of the user name you will connect as.
#' @param password A character vector of the user name's password in plain text.
#' @param dbname A character vector of the database name to connect to. If an
#'   empty string is provided it should connect to the admin db.
#'
#' @return Nothing
#' @export sendPgFillTableQuery
#'
#' @importFrom DBI dbConnect dbSendQuery dbDisconnect
#' @importFrom RPostgreSQL PostgreSQL
sendPgFillTableQuery <- function(query,
                                 host,
                                 port,
                                 user,
                                 password,
                                 dbname){


  pgConn <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                           host = host,
                           port = port,
                           user = user,
                           password = password,
                           dbname = dbname)

  DBI::dbSendQuery(conn = pgConn,
                           statement = query)

  DBI::dbDisconnect(conn = pgConn)

}
