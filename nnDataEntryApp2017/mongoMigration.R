# Function to read then tibble

readTbl <- function(x) read.csv(x) %>% tbl_df

# Function to put data in mongo

mongoInsert <- function(inTable, mongoTable){
  connectionMongo <- mongo(mongoTable, url = mongoURL)
  connectionMongo$insert(inTable)
}

mongoRemove <- function(mongoTable){
  connectionMongo <- mongo(mongoTable, url = mongoURL)
  connectionMongo$remove(query = '{}', multiple = TRUE)
}

# Function to turn NA to 'noData'

naToNoData <- function(x){
  x <- ifelse(is.na(x) & !is.numeric(x), 'noData', x)
  x <- ifelse(is.na(x) & is.numeric(x), -99999, x)
  return(x)
}

# Function to fix time (adds a zero if it's the morning):

fixTime <- function(x){
  ifelse(str_detect(x, ':') & str_count(x) == 4,
         paste0(0, x), x)
}

# Function to change names to those of the field codes:

namesToFieldCodes <- function(inData, fieldCodes){
  data <- readTbl(inData)
  names(data) <- fieldCodes
  return(data)
}

mongoURL <- "mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry"

# Vector of mongo tables:

mongoNames <- c('contactTable', 'addressTable', 'locationTable',
                'visitTable','captureTable','forayEffortTable',
                'forayCountUnbandedTable','techRsTable', 'pcTable')

for(i in 1:length(mongoNames)){
  inData <- paste0('startData/', mongoNames[i], '.csv')
  outName <- mongoNames[i] %>% str_replace('Table', '')
  namesVector <- paste0(
    'startData/', str_replace(mongoNames[i],'Table', ''),'Names.csv') %>%
    readTbl %>% .$names
  # Read, change NA to noData, and set field names:
  outData <- readTbl(inData) %>%
    mutate_all(.funs = naToNoData) %>%
    mutate_all(.funs = fixTime) %>%
    distinct
  names(outData) <- namesVector
  # Add to mongo:
  mongoRemove(mongoNames[i])
  mongoInsert(outData, mongoNames[i])
}

