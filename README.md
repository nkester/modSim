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

## Step 1: Establishing The PostgreSQL Database  

This only needs to be done if a new database is required or the database has not yet been created.  

```

modSim::Step1_createModSimDb(connParamList = pgConnParam)

```  

## Step 2: Extract Transfrom and Load (ETL) data from MongoDB to PostgreSQL by design point  

This should be executed for each designPoint that needs to be added to the PostgreSQL database.  

```

designPoints <- "<designPoint>"

# OR

designPoints <- c("<designPoint1>","<designPoint2>","<designPoint3>","<designPoint4>")

for(designPoint in designPoints){
  
  modSim::Step2_queryMongoAndFillPg(mongoConnParam = mongoConnParam,
                                    pgConnParam = pgConnParam,
                                    designPoint = designPoint)
  
}

```  

## Step 3: Visualize  

In addition to the parameters required above, these must be specified:  

```
sensorForce <- "BLUEFORCE"
targetForce <- "REDFORCE"

sensors <- "WASP 1" #c("EPBV 90 1","EPBV 90 2","WASP 1")

losColor <- "blue"

acqColor <- "black"

```

Extract data from the two PostgreSQL Materialized Views created by the function in step 1.  

```

graphData <- modSim::Step3_multiDesingPointAndSensorDataPrep(pgConnParam = pgConnParam,
                                                             sensorForce = sensorForce,
                                                             targetForce = targetForce,
                                                             designPoints = designPoints,
                                                             sensors = sensors)

```  

With the data on your local environment, the next step is to graph the mean across the iterations.  

```

p_mean <- modSim::Step3_graphMean(graphData = graphData$byDesignPoint,
                                  losColor = losColor,
                                  acqColor = acqColor,
                                  errorbars = TRUE)

```  
### EXPERIMENTAL: 

Another interesting thing to do is to graph the data after aggregating it to expose which target entities the sensor entities has line of sight with and has acquired over time.

```

graphTargetColumnByDesignPoint(designPoints,
                               pgConnParam,
                               sensorForce,
                               targetForce,
                               sensor)
                      
```
