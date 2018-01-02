# Function to read then tibble

readTbl <- function(x) read.csv(x) %>% tbl_df

# Migrate data to mongo:

readTbl('startData/addressTable.csv')

readTbl('startData/birdTable.csv')

readTbl('startData/contactTable.csv')

readTbl('startData/forayCountUnbandedTable.csv')

readTbl('startData/forayEffortTable.csv')

readTbl('startData/pcTable.csv')

readTbl('startData/queryTable.csv')

readTbl('startData/siteIdTable.csv')

readTbl('startData/siteLocationTable.csv')

readTbl('startData/techRsTable.csv')

readTbl('startData/visitTable.csv')













