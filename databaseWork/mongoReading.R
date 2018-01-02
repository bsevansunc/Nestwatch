library(RMySQL)

options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 3306,
  "user" = "myuser",
  "password" = "mypassword"
))
databaseName <- "myshinydatabase"
table <- "responses"

dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
          port = options()$mysql$port, user = options()$mysql$user, 
          password = options()$mysql$password)


library(mongolite)

options(mongodb = list(
  "host" = "ds025232.mlab.com:25232",
  "username" = "bsevans",
  "password" = "33shazam"
))

mongo.create(username = 'bsevans', password = '33shazam', db = 'nndataentry')



mongo.create("ds025232.mlab.com:25232")

mongo(collection = collectionName,
      url = sprintf(
        "mongodb://%s:%s@%s/%s",
        options()$mongodb$username,
        options()$mongodb$password,
        options()$mongodb$host,
        databaseName))

sprintf(
  "mongodb://%s:%s@%s/%s",
  options()$mongodb$username,
  options()$mongodb$password,
  options()$mongodb$host,
  databaseName)



# mongodb://<dbuser>:<dbpassword>@ds025232.mlab.com:25232/nndataentry

mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry


databaseName <- "nndataentry"
collectionName <- "nndata"

uriString <- "mongodb://bsevans:33shazam@ds025232"

uriString <- "mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry"

uriString <- "mongodb://bsevans:33shazam@ds025232.mlab.com:25232"

uriString <- "mongodb://bsevans:33shazam@ds025232.mlab.com"

sprintf(
  "mongodb://%s:%s@%s/%s",
  options()$mongodb$username,
  options()$mongodb$password,
  options()$mongodb$host,
  databaseName)


mongo(collectionName, databaseName, uriString)

mongo(colletionName, databaseName,
      url = sprintf(
        "mongodb://bsevans:33shazam@ds025232",
        options()$mongodb$username,
        options()$mongodb$password,
        options()$mongodb$host,
        databaseName))


dbUrl <- ' mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry'

mongo(collection = 'nndata','nndataentry', 'mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry')

mongo(collection = 'nndata',
      db = 'nndataentry',
      url = 'mongodb://bsevans:33shazam@ds025232.mlab.com')



testFrame <- data.frame(helloWorld = c('foo', 'bar'))


saveData <- function(data) {
  # Connect to the database
  db <- mongo(collection = collectionName,
              db = 'nndataentry',
              url = dbUrl)
#               url = sprintf(
#                 "mongodb://%s:%s@%s/%s",
#                 options()$mongodb$username,
#                 options()$mongodb$password,
#                 options()$mongodb$host,
#                 databaseName))
  # Insert the data into the mongo collection as a data.frame
  data <- as.data.frame(t(data))
  db$insert(data)
}

saveData(testFrame)

loadData <- function() {
  # Connect to the database
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # Read all the entries
  data <- db$find()
  data
}