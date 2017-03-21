# Function to read then tibble

readTbl <- function(x) read.csv(x) %>% tbl_df

# Function to put data in mongo

mongoInsert <- function(inTable, mongoTable){
  connectionMongo <- mongo(mongoTable, url = mongoURL)
  connectionMongo$insert(inTable)
}

# Function to turn NA to 'noData'

naToNoData <- function(x) ifelse(is.na(x), 'noData', x)

# Function to change names to those of the field codes:

namesToFieldCodes <- function(inData, fieldCodes){
  data <- readTbl(inData)
  names(data) <- fieldCodes
  return(data)
}

mongoURL <- "mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry"

# Vector of mongo tables:

mongoNames <- c('contactTable', 'addressTable', 'siteLocationTable',
                'visitTable','captureTable','forayEffortTable',
                'forayCountUnbandedTable','techRsTable', 'pcTable')

for(i in 2:length(mongoNames)){
  inData <- paste0('startData/', mongoNames[i], '.csv')
  outName <- mongoNames[i] %>% str_replace('Table', '')
  namesVector <- paste0(
    'startData/', str_replace(mongoNames[i],'Table', ''),'Names.csv') %>%
    readTbl %>% .$names
  # Read, change NA to noData, and set field names:
  outData <- readTbl(inData) %>%
    mutate_all(.funs = naToNoData)
  names(outData) <- namesVector
  # Add to mongo:
  mongoInsert(outData, mongoNames[i])
}
