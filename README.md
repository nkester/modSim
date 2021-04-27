# `{modSim}`

This package contains the functions required for my thesis work "Applying trade-space analysis to modeling and simulation as a service (MSaaS): A Study in applying established systems engineering methodologies in a novel setting."

This function is not meant to be general and re-useable as many of the functions are purpose built for this work. That said, there are some utility functions that have re-use potential. In fact, some have been re-used here from work I have done in the past.

# Usage  

This is how you would use the functions in this package. A pre-requisite is that a MonogDB instance must already exist and be filled with at least one design point's results from the `mdo-sim` simulation composition and the user must have a valid connection string to it. The second pre-requisite is that a PostgreSQL instance must already exist and the user must have valid connection parameters to it. The pg user must have the rights to create tables and views at the least. If the database does not already exist, the user must also have the rights to create new databases.  

## Define the connection information as a list for both MongoDB and PostgreSQL
```
mongoConnParam <- list(
  "mongoUri" = "mongodb://<username>:<password>@<URI>:<port>",
  "mongoDb" = "<database name>"
)

pgConnParam <- list(
  "pgHost" = "<your host>",
  "pgPort" = <your port>,
  "pgUser" = "<your username>",
  "pgPass" = "<your password>",
  "pgDb" = "<your database name>"
)
```  

## Establishing The PostgreSQL Database  

This only needs to be done if a new database is required or the database has not yet been created.  

```

modSim::createModSimDb(connParamList = pgConnParam)

```  

## Extract Tranform and Load a Design Point  

This should be executed for each designPoint that needs to be added to the PostgreSQL database.  

```

designPoint <- "<designPoint>"

modSim::queryMongoAndFillPg(mongoConnParam = mongoConnParam,
                            pgConnParam = pgConnParam,
                            designPoint = designPoint)

```  

## FUTURE: Apply Analytic and Produce Visuals  

To be added.
