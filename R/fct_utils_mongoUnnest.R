
# low_mapAndUnnest --------------------------------------------------------


#' Row-wise Map and Unnest JSON by column
#'
#' This function is intended to be used in a row-wise application to map and
#'  unnest multilevel columns resulting from nested JSON objects.
#'
#' @importFrom magrittr %>%
#' @importFrom purrr map_if
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
low_mapAndUnnest <- function(data,
                             unnestCols){

  if(any(!(unnestCols %in% names(data)))){

    stop("The cols argument must exist as a column name in the data argument.")

  }

  stepData <- data %>%
    purrr::map_if(.x = .,
                  .p = is.data.frame,
                  .f = list) %>%
    tibble::as_tibble(.) %>%
    tidyr::unnest(cols = dplyr::all_of(unnestCols))

  return(stepData)

} # close los_low_mapAndUnnest


# mid_multiColRowwiseUnnest -----------------------------------------------


#' Multi Column Row-wise Unnest
#'
#' Execute the low function `los_low_mapAndUnnest` over many rows and applied
#'  to multiple columns iteratively. Columns are unnested in the order entered
#'  in the `cols` character vector.
#'
#' @param queryResults The results from a mongolite query. It should be a multi-level
#'  data frame.
#' @param unnestCols A character vector of column names to apply the `los_low_mapAndUnnest`
#'  function to.
#'
#' @return This returns a tibble.
#'
#' @importFrom dplyr bind_rows
mid_multiColRowwiseUnnest <- function(data,
                                      unnestCols){

  unnestedResults <-  NULL

  for(oneCol in unnestCols){

    message(paste0("Working on column: ",
                   oneCol))

    for(idx in 1:nrow(data)){

      unnestedResults <- dplyr::bind_rows(unnestedResults,
                                          low_mapAndUnnest(data = data[idx,],
                                                           unnestCols = oneCol))

    } # close idx loop

  } # close cols loop

  return(unnestedResults)

} # close mid_multiColRowwiseUnnest


# mongoUnnest -------------------------------------------------------------


#' Query and/or Transform MongoDB
#'
#' This function accepts up to five parameters to describe the connection, query,
#'  and fields to return. In the process, it conducts specific unnesting functions
#'  required for the collection in question. As such, it is not appropriate for
#'  general use.
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
#' @param mongoData This is a data frame passed to this function to unnest
#'  specific columns. If this is used, do not provide the `mongoUri`, `mongoDb`,
#'  `mongoCollection`, `mongoQuery`, or `mongoFields` pramaters.
#' @param unnestCols This is a character vector of the columns you need to unnest.
#'
#' @return This function returns an unnested tibble in which each row is an
#'  observation of a line of sight event.
#'
#' @importFrom magrittr %>%
#' @importFrom mongolite mongo
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest
#' @importFrom dplyr select
#' @export mongoUnnest
mongoUnnest <- function(mongoUri = "",
                        mongoDb = "",
                        mongoCollection = "",
                        mongoQuery = "{}",
                        mongoFields = "{}",
                        mongoData = "",
                        unnestCols = ""){

  require(package = "magrittr",
          quietly = TRUE)

  { # Input checks ----

    { # Check if Query information is provided ----

      if(all(nchar(mongoUri)!=0,
             nchar(mongoDb)!=0,
             nchar(mongoCollection)!=0)){

        uriInput <- TRUE

      }else{

        uriInput <- FALSE

      }

    } # close Check if Query information is provided section

    { # Check if nested data is provided ----

      if(any(nchar(mongoData)!=0)){

        nestedData <- TRUE

      }else{

        nestedData <- FALSE

      }

    } # close Check if nested data is provided section

    { # Check if any unnesting needs to happen ----

      if(any(nchar(unnestCols)!=0)){

        unnest <- TRUE

      }else{

        unnest <- FALSE

      }

    } # close Check if any unnesting needs to happen section

    { # One valid input object provided ----

      if(all(!uriInput,
             !nestedData)){

        stop("Either a connection object or an existing tibble must be provided!")

      }

    } # close One valid input object provided section

    { # Only one valid input object provided ----

      #> This function can't deal with both a mongo data set and a mongo query at
      #>  the same time.

      if(all(uriInput,
             nestedData)){

        stop("Either provide the elements of a mongoDB connection object OR an
             existing nested dataframe to unnest. Not both")

      }


    } # close Only one valid input object provided section

  } # close Input checks section

  #> Only Queyr the MongoDB if the connection objects have been provided

  if(uriInput){

    mongoConnection <- mongolite::mongo(url = mongoUri,
                                        db = mongoDb,
                                        collection = mongoCollection)

    mongoData <- mongoConnection$find(query = mongoQuery,
                                      fields = mongoFields)

    mongoConnection$disconnect()

  }

  if(unnest){

    mongoData <- mid_multiColRowwiseUnnest(data = mongoData,
                                           unnestCols = unnestCols)

  }

  if(!exists("mongoData")){

    mongoData <- NULL

    warning("Either the query didn't produce a response or no input data was provided!")

  }

  return(mongoData)

}
