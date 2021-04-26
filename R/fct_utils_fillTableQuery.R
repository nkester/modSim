#' SQL Insert Query Builder
#'
#' This function takes a data frame and a table name (optionally including the
#'   field names in parentheses) and returns a string of a properly built SQL
#'   INSERT Query. This is most used with SQLite and PostgreSQL. Of note, to
#'   add the proper entry for auto-incrementing columns, pass an `NA` value for
#'   that field in the `data` parameter and provide the keyword in the `serial`
#'   parameter.
#'
#' @author Neil Kester, \email{nkester1@@jhu.edu}
#'
#' @param data This is a tibble of any dimension although thought should be given
#'  to very large datasets. It may be better to break the INSERT query into multiple
#'  smaller pushes.
#'
#' @param tableName This is the name of the table. Note that some DBMS (namely)
#'   PostgreSQL does not automatically honor case. It is best to be explicit by
#'   wrapping all names within \"<name>\". Additionally, you may be explicity by
#'   including in this parameter the order of the table's fields you are providing.
#'   This ensures the columns provided in the `data` parameter go to the proper
#'   location. Do this like this: tableName = "\"<tableName\" (\"fieldOne\",
#'   \"fieldTwo\")".
#'
#' @param serial Any field in the `data` parameter with `NA` will be treated as
#'   if it is the auto-incrementing primary key for the table. If using
#'   PostgreSQL this should be "DEFAULT". If using SQLite it should be "NULL".
#'
#' @return This returns a character string that can be passed to a DBI::dbSendQuery
#'   function for execution on the DBMS.
#'
#' @export fillTableQuery
#'
fillTableQuery <- function(data,tableName,serial = "DEFAULT"){

  query<-sprintf("INSERT INTO %s VALUES",
                 tableName)

  for(rdx in 1:nrow(data)){

    query <- paste(query,"(",sep = '')

    row <- NULL

    for(cdx in 1:ncol(data)){

      if(cdx == ncol(data)){

        if(is.na(data[rdx,cdx][[1]])){

          row <- paste(row, serial, sep = '')

        }else{

          row <- paste(row,"'",data[rdx,cdx][[1]],"'",sep = '')

        }

      }else{

        if(is.na(data[rdx,cdx][[1]])){

          row <-paste(row,serial,",",sep = '')

        }else{

          row <- paste(row,"'",data[rdx,cdx][[1]],"',",sep = '')

        }
      }

    }#close cdx loop

    query <- paste(query,row,sep = '')

    if(rdx == nrow(data)){

      query <- paste(query,")",sep = '')

    }else{#if complete

      query <- paste(query,"),",sep = '')

    }#close else

  }#close rdx loop

  return(query)

}#close fillTableQuery function
