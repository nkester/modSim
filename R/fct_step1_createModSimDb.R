#' Create ModSim PostgreSQL Database
#'
#' This function automates the creation of the specific PostgreSQL database used
#'  to support this thesis work. It takes only a list of connection parameters
#'  required to connect to the PostgreSQL instance. From that, it creates all
#'  tables and materialized views used elsewhere in the analytic work flow. This
#'  is purpose built and not meant to be a general use function.
#'
#' @author Neil Kester, \email{nkester1@@jhu.edu}
#'
#' @param connParamList A five element named list containing the following elements:
#'  "pgHost", "pgPort", "pgUser", "pgPass", and "pgDb".
#'
#' @return This returns messages as the build progresses and then a `NULL` response.
#'
#' @export Step1_createModSimDb
#'
#' @examples createModSimDb(connParamList = pgConnParam)
#'
#' @importFrom RPostgreSQL PostgreSQL
#' @importFrom DBI dbConnect dbGetQuery dbSendQuery dbDisconnect
#' @importFrom magrittr %>%
#'
#' @note R script location: ./R/fct_step1_createModSimDb.R
#' @note RMarkdown location: ./inst/step1_CreatePgDataBase/Step1_configurePostgresDb.Rmd
Step1_createModSimDb <- function(connParamList){

  { # Write Query Statements

    createDatabase <- sprintf("CREATE DATABASE \"%s\"",
                              connParamList[["pgDb"]])

    createSensorDescriptionTable <- "CREATE TABLE IF NOT EXISTS \"sensorDescription\" (
                   \"sensorId_pkId\" SERIAL PRIMARY KEY,
                   \"sensorId\" TEXT,
                   \"acquireSensorType\" TEXT,
                   \"magnification\" TEXT,
                   \"designPoint\" TEXT)"

    createEntityIdToNameTable <- "CREATE TABLE IF NOT EXISTS \"entityIdToName\" (
                   \"entityId_pkId\" SERIAL PRIMARY KEY,
                   \"entityId\" TEXT,
                   \"source\" TEXT,
                   \"designPoint\" TEXT,
                   \"force\" TEXT,
                   \"shortName\" TEXT)"

    createSensorToEntityIdTable <- "CREATE TABLE IF NOT EXISTS \"sensorToEntityId\" (
                   \"sensorToEntityId_pkId\" SERIAL PRIMARY KEY,
                   \"sensorId\" TEXT,
                   \"entityId\" TEXT,
                   \"designPoint\" TEXT)"

    createUnnestedSensorStateTable <- "CREATE TABLE IF NOT EXISTS \"unnestedSensorState\" (
                   \"id\" TEXT,
                   \"runId\" TEXT,
                   \"runTime\" TEXT,
                   \"designPoint\" TEXT,
                   \"iteration\" TEXT,
                   \"time_ms\" TEXT,
                   \"sensorId\" TEXT,
                   \"entityId\" TEXT,
                   \"acquireSensorType\" TEXT,
                   \"magnification\" TEXT,
                   \"source\" TEXT)"

    createLosTable <- "CREATE TABLE IF NOT EXISTS \"losState\" (
                   \"losState_pkId\" SERIAL PRIMARY KEY,
                   \"id\" TEXT,
                   \"runId\" TEXT,
                   \"runTime\" TEXT,
                   \"designPoint\" TEXT,
                   \"iteration\" TEXT,
                   \"time_ms\" TEXT,
                   \"time_s\" TEXT,
                   \"sensorId\" TEXT,
                   \"targetId\" TEXT,
                   \"hasLOS\" BOOLEAN)"

    createSensorAcq <- "CREATE TABLE IF NOT EXISTS \"sensorAcqState\" (
                   \"sensorAcqState_pkId\" SERIAL PRIMARY KEY,
                   \"id\" TEXT,
                   \"runId\" TEXT,
                   \"runTime\" TEXT,
                   \"designPoint\" TEXT,
                   \"iteration\" TEXT,
                   \"time_ms\" TEXT,
                   \"time_s\" TEXT,
                   \"receiverId\" TEXT,
                   \"senderId\" TEXT,
                   \"sensorId\" TEXT,
                   \"entityId\" TEXT,
                   \"targetId\" TEXT,
                   \"detectionLevel\" TEXT,
                   \"previousDetectionLevel\" TEXT,
                   \"timeToDetection\" TEXT)"

  } # close Write Query Statements

  { # Create Materialized Views

    { # LOS Sensor Target Pairs

      #> Drop with: "DROP MATERIALIZED VIEW los_sensor_target_pairs_materialized"
      #> Refresh with: "REFRESH MATERIALIZED VIEW CONCURRENTLY los_sensor_target_pairs_materialized"

      query_createLosMatView <- "
                CREATE MATERIALIZED VIEW IF NOT EXISTS los_sensor_target_pairs_materialized AS
                SELECT
                  los.\"time_s\",
                  ent2.\"shortName\" AS \"sensorShortName\",
                  ent2.\"force\" AS \"sensorForce\",
                  ent1.\"shortName\" AS \"targetShortName\",
                  ent1.\"force\" AS \"targetForce\",
                  los.\"hasLOS\",
                  los.\"designPoint\",
                  los.\"iteration\",
                  los.\"targetId\",
                  los.\"sensorId\",
                  sens.\"entityId\" AS \"sensorEntityId\"
                FROM \"losState\" AS los
                  LEFT JOIN \"entityIdToName\" AS ent1
                    ON los.\"targetId\" = ent1.\"entityId\"
                    LEFT JOIN \"sensorToEntityId\" AS sens
                      ON los.\"sensorId\" = sens.\"sensorId\"
                      LEFT JOIN \"entityIdToName\" AS ent2
                        ON sens.\"entityId\" = ent2.\"entityId\"
                "

    } # close LOS Sensor Target Pairs

    { # Sensor Acquisition Sensor Target Pairs

      #> Drop with: "DROP MATERIALIZED VIEW acq_sensor_target_pairs_materialized"
      #> Refresh with: "REFRESH MATERIALIZED VIEW CONCURRENTLY acq_sensor_target_pairs_materialized"

      query_createAcqMatView <- "
                      CREATE MATERIALIZED VIEW IF NOT EXISTS acq_sensor_target_pairs_materialized AS
                      SELECT
                        acq.\"time_s\",
                        ent2.\"shortName\" AS \"sensorShortName\",
                        ent2.\"force\" AS \"sensorForce\",
                        ent1.\"shortName\" AS \"targetShortName\",
                        ent1.\"force\" AS \"targetForce\",
                        CASE WHEN acq.\"detectionLevel\" = 'SENSOR_NODETECTION' THEN FALSE
                               ELSE TRUE
                             END AS \"hasAcq\",
                        acq.\"detectionLevel\",
                        acq.\"designPoint\",
                        acq.\"iteration\",
                        acq.\"targetId\",
                        acq.\"sensorId\",
                        sens.\"entityId\" AS \"sensorEntityId\"
                      FROM \"sensorAcqState\" AS acq
                        LEFT JOIN \"entityIdToName\" AS ent1
                          ON acq.\"targetId\" = ent1.\"entityId\"
                            LEFT JOIN \"sensorToEntityId\" AS sens
                              ON acq.\"sensorId\" = sens.\"sensorId\" AND acq.\"designPoint\" = sens.\"designPoint\"
                                LEFT JOIN \"entityIdToName\" AS ent2
                                  ON sens.\"entityId\" = ent2.\"entityId\"
                      "

    } # close Sensor Acquisition Sensor Target Pairs

  } # close Create Materialized Views

  { # Connect and send queries

    { # Check if the database already exists

      pgConn <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               host = connParamList[["pgHost"]],
                               port = connParamList[["pgPort"]],
                               user = connParamList[["pgUser"]],
                               password = connParamList[["pgPass"]])

      existingDatabases <- DBI::dbGetQuery(conn = pgConn,
                                           statement = "SELECT * FROM pg_database")$datname

      if(connParamList[["pgDb"]] %in% existingDatabases){

        message(sprintf("%s Database already exists, moving on to creating tables",
                        connParamList[["pgDb"]]))

      }else{

        DBI::dbSendQuery(conn = pgConn,
                         statement = createDatabase)

        message(sprintf("%s Database created! Moving on to creating tables",
                        connParamList[["pgDb"]]))

      } # close else

      DBI::dbDisconnect(conn = pgConn)

      rm(pgConn)

    } # close Check if the database already exists

    { # Connect to the supplied database and create tables if they do not exist

      pgConn <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               host = connParamList[["pgHost"]],
                               port = connParamList[["pgPort"]],
                               user = connParamList[["pgUser"]],
                               password = connParamList[["pgPass"]],
                               dbname = connParamList[["pgDb"]])
      { # Create tables

        message("Working: Creating tables.")

        DBI::dbSendQuery(conn = pgConn,
                         statement = createSensorDescriptionTable)

        DBI::dbSendQuery(conn = pgConn,
                         statement = createEntityIdToNameTable)

        DBI::dbSendQuery(conn = pgConn,
                         statement = createSensorToEntityIdTable)

        DBI::dbSendQuery(conn = pgConn,
                         statement = createUnnestedSensorStateTable)

        DBI::dbSendQuery(conn = pgConn,
                         statement = createLosTable)

        DBI::dbSendQuery(conn = pgConn,
                         statement = createSensorAcq)

      } # close Create tables

      { # Create Materialized Views

        message("Working: Creating Materialized Views.")

        DBI::dbSendQuery(conn = pgConn,
                         statement = query_createLosMatView)

        DBI::dbSendQuery(conn = pgConn,
                         statement = query_createAcqMatView)

      } # close Create Materialized Views

    } # close Connect to the supplied database and create tables if they do not exist

  } # close Connect and send queries

  DBI::dbDisconnect(conn = pgConn)

  message(sprintf("Complete: The %s database exists and all tables have been created!",
                  connParamList[["pgDb"]]))

} # close Step1_createModSimDb
