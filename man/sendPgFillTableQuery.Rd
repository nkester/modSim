% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_utils_sendPgFillTableQuery.R
\name{sendPgFillTableQuery}
\alias{sendPgFillTableQuery}
\title{Connect and Send Query to PostgreSQL}
\usage{
sendPgFillTableQuery(query, host, port, user, password, dbname)
}
\arguments{
\item{query}{This is a character string describing a query to send to PostgreSQL.
Note that this conducts a `dbSendQuery` so this is not suitable for returning
data but rather to write data or send CREATE instructions.}

\item{host}{A character vector to the desired PostgreSQL instance.}

\item{port}{An integer port number the PostreSQL instance listen on. Standard
is 5432.}

\item{user}{A character vector of the user name you will connect as.}

\item{password}{A character vector of the user name's password in plain text.}

\item{dbname}{A character vector of the database name to connect to. If an
empty string is provided it should connect to the admin db.}
}
\value{
Nothing
}
\description{
This is a utility function to help connect to, send, and disconnect from
  a PostgreSQL database.
}
\note{
Location: ./R/fct_utils_sendPgFillTableQuery.R

RMarkdown location: ./inst/step2_queryMongoAndFillPg/Step2_queryMongoAndFillPg.Rmd
}
\author{
Neil Kester, \email{nkester1@jhu.edu}
}
