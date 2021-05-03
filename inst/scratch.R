# Create an iterator

source("./inst/connectionObjects/mongoConnectionObj.R")
designPoint <- "WASP1_high_high_20210426"
mongoUri = mongoConnParam[["mongoUri"]]
mongoDb = mongoConnParam[["mongoDb"]]

{ # Create Pg

  source("./inst/connectionObjects/pgConnectionObj.R")
  pgConnParam[["pgDb"]] <- "modSimIt6"

  createModSimDb(connParamList = pgConnParam)

} # close Create Pg

{ # Step 2
  modSim::queryMongoAndFillPg(mongoConnParam = mongoConnParam,
                              pgConnParam = pgConnParam,
                              designPoint = designPoint)
} # close Step 2

{ # LOS Data ----

  mongoCollection <- "AcquireModel.event.LOSTargetStatus"
  mongoFields <- "{\"_id\": true, \"runId\": true, \"runTime\": true,
  \"designPoint\": true, \"iteration\": true, \"time\": true,\"event\": true}"
  mongoQuery <- sprintf("{\"designPoint\": \"%s\"}",
                        designPoint)

  mongoConn <- mongolite::mongo(url = mongoUri,
                                db = mongoDb,
                                collection = mongoCollection)

  numRecs <- mongoConn$count(query = mongoQuery)

  it <- mongoConn$iterate(query = mongoQuery,fields = mongoFields)

  pgConn <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                           host = pgConnParam$pgHost,
                           port = pgConnParam$pgPort,
                           user = pgConnParam$pgUser,
                           password = pgConnParam$pgPass,
                           dbname = pgConnParam$pgDb)

  rdx <- 1

  #while(!is.null(x <- it$one())){
  for(idx in 1:1000){

    message(paste0("Row: ",as.character(rdx),
                 "  % comp: ",rdx/numRecs))

    x <- it$one()

    temp <- tibble::tibble("losState_pkid" = NA,
                           "id" = x$`_id`,
                           "runId" = x$runId,
                           "runTime" = x$runTime,
                           "designPoint" = x$designPoint,
                           "iteration" = x$iteration,
                           "time_ms" = x$time,
                           "time_s" = x$time/1000,
                           "sensorId" = x$event$sensorId,
                           "targetId" = x$event$targetId,
                           "hasLOS" = x$event$hasLOS)

    temp_query <- fillTableQuery(data = temp,tableName = "\"losState\"",serial = "DEFAULT")

    DBI::dbSendQuery(conn = pgConn,
                     statement = temp_query)

    rdx <- rdx + 1

  }

  DBI::dbDisconnect(conn = pgConn)
  mongoConn$disconnect()

} # close LOS Data section

{ # Acq Data

  mongoCollection <- "AcquireModel.event.C2SimulationMessage"
  mongoFields <- "{\"runId\": 1,\"runTime\": 1, \"designPoint\": 1,\"iteration\": 1, \"time\": 1, \"event.receiverId\": 1, \"event.senderId\": 1,\"event.messageData.any.sensorDetection\": 1}"
  mongoQuery <- sprintf("{\"designPoint\": \"%s\", \"event.messageData.javaClass\": \"sensorproto.SensorModel$DetectedTarget\"}",
                        designPoint)

  mongoConn <- mongolite::mongo(url = mongoUri,
                                db = mongoDb,
                                collection = mongoCollection)

  numRecs <- mongoConn$count(query = mongoQuery)

  it <- mongoConn$iterate(query = mongoQuery,fields = mongoFields)

  pgConn <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                           host = pgConnParam$pgHost,
                           port = pgConnParam$pgPort,
                           user = pgConnParam$pgUser,
                           password = pgConnParam$pgPass,
                           dbname = pgConnParam$pgDb)

  rdx <- 1

  #while(!is.null(x <- it$one())){
  for(idx in 1:1000){

    message(paste0("Row: ",as.character(rdx),
                   "  % comp: ",rdx/numRecs))

    x <- it$one()

    temp <- tibble::tibble("sensorAcqState_pkid" = NA,
                           "id" = x$`_id`,
                           "runId" = x$runId,
                           "runTime" = x$runTime,
                           "designPoint" = x$designPoint,
                           "iteration" = x$iteration,
                           "time_ms" = x$time,
                           "time_s" = x$time/1000,
                           "receiverId" = x$event$receiverId,
                           "senderId" = x$event$senderId,
                           "entityId" = x$event$messageData$any$sensorDetection$entityId,
                           "targetId" = x$event$messageData$any$sensorDetection$targetId,
                           "detectionLevel" = x$event$messageData$any$sensorDetection$detectionLevel,
                           "previousDetectionLevel" = x$event$messageData$any$sensorDetection$previousDetectionLevel,
                           "timeToDetection" = x$event$messageData$any$sensorDetection$timeToDetection)

    temp_query <- fillTableQuery(data = temp,tableName = "\"sensorAcqState\"",serial = "DEFAULT")

    DBI::dbSendQuery(conn = pgConn,
                     statement = temp_query)

    rdx <- rdx + 1

  }

  DBI::dbDisconnect(conn = pgConn)
  mongoConn$disconnect()

} # close Acq Data section



