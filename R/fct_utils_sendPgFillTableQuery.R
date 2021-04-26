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
