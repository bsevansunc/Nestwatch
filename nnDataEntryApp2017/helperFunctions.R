# FUNCTIONS
#=================================================================================*
# ---- GENERAL FUNCTIONS ----
#=================================================================================*

# readTbl ---- 
# Function to read then tibble:

readTbl <- function(x) read.csv(x) %>% tbl_df

# naToNoData ----
# Function to turn NA to 'noData':

naToNoData <- function(x) ifelse(is.na(x), 'noData', x)

# namesToFieldCodes ----
# Function to change names to those of the field codes:

namesToFieldCodes <- function(inData, fieldCodes){
  data <- readTbl(inData)
  names(data) <- fieldCodes
  return(data)
}

# getColorCombos ----
# Function to generate color combinations:

getColorCombos <- function(colorVector, nBands, 
                           xPositions = 1:4, aluminum = TRUE){
  if(aluminum == TRUE) colorVector <- c(colorVector, 'X')
  if(nBands!= 4) colorVector <- c(colorVector, '-')
  colorList <- rep(list(colorVector), 4)
  colorGrid <- expand.grid(colorList, stringsAsFactors = FALSE) %>%
    unite(col = combinedCol, 1:4, sep = '') %>%
    filter(str_count(combinedCol, '-') == 4 - nBands)
  if(aluminum == TRUE){
    colorGrid <- colorGrid %>%
      filter(str_count(combinedCol, 'X') == 1,
             str_locate(combinedCol, 'X')[,1] %in% xPositions)
  }
  colorCombos <- colorGrid %>%
    separate(col = combinedCol, into = c('colorL', 'colorR'), sep = 2) %>%
    mutate_all(str_replace_all, pattern = '-', replacement = '')
  return(
    sample_n(colorCombos, nrow(colorCombos)) %>%
      distinct
  )
}

# getAllPossibleCombos ----
# Function to generate all possible color combinations:

getAllPossibleCombos <- function(colorValues){
  colorComboList <- vector('list', length = 3)
  for(i in 1:length(colorComboList)){
    colorComboList[[i]] <- getColorCombos(colorValues, i+1)
  }
  colorCombos <- bind_rows(colorComboList) %>%
    mutate(colorL = ifelse(colorL == '', '-', colorL),
           colorR = ifelse(colorR == '', '-', colorR)) %>%
    bind_rows(
      data.frame(colorL = 'X', colorR = '-'),
      data.frame(colorL = '-', colorR = 'X')
    ) %>%
    distinct %>%
    arrange(colorL, colorR) %>%
    transmute(colorCombo = paste(colorL, colorR, sep = ',')) %>%
    .$colorCombo
  return(colorCombos)
}

#=================================================================================*
# ---- MONGO FUNCTIONS ----
#=================================================================================*

# mongoToTblDf ----
# Convert mongo data frame to a dplyr table dataframe

mongoToTblDf <- function(data){
  if(nrow(data) > 0 & ncol(data) > 0){
    for(j in 1:ncol(data)){
      data[,j] <- as.character(data[,j])
    }
    data <- data %>% tbl_df
  }
  data
}

# mongoInsert ----
# Function to put data in mongo:

mongoInsert <- function(inTable, mongoTable){
  connectionMongo <- mongo(mongoTable, url = mongoURL)
  connectionMongo$insert(inTable)
}

# mongoDeleteThenInsert ----
# Function to delete then insert a table:

mongoDeleteThenInsert <- function(inTable, mongoTable){
  connectionMongo <- mongo(mongoTable, url = mongoURL)
  connectionMongo$remove()
  connectionMongo$insert(inTable)
}


#=================================================================================*
# ---- SERVER FUNCTIONS ----
#=================================================================================*

# siteNameSubsetter ----
# Function to return a vector of sites given a hub selection:

siteNameSubsetter <- function(inHub){
  encounters %>%
    select(hub, site) %>%
    distinct %>% 
    bind_rows(
      data.frame(hub = c('Atlanta', 'Colorado', 'DC', 'Gainesville',
                         'Pittsburgh','Raleigh', 'Springfield'),
                 site = rep('', 6))) %>%
    filter(hub == inHub) %>%
    arrange(site) %>%
    .$site
}

# getTableMetadata ----
# Function assigns fancy column headers to the field codes:

getTableMetadata <- function(fieldCodes, fieldNames) {
  fields <- fieldNames
  names(fields) <- fieldCodes
  result <- list(fields = fields)
  return (result)
}

# createBlankInputs ----
# Make some submissions blank:

createBlankInputs <- function(fieldCodes, session){
  for(i in 1:length(fieldCodes)){
    updateTextInput(session, fieldCodes[i], value = '')
  }
}

createZeroInputs <- function(fieldCodes, session){
  for(i in 1:length(fieldCodes)){
    updateTextInput(session, fieldCodes[i], value = 0)
  }
}

# castData ----
# Function takes values of inputs, if there are any, and puts them in a 1-row data frame:

castData <- function(fieldValues){
  for(i in 1:length(fieldValues)){
    if(is.na(fieldValues[i])) fieldValues[i] <- 'noData'
    if(fieldValues[i] == '') fieldValues[i] <- 'noData'
  }
  return(data.frame(t(as.list(fieldValues)),
                    stringsAsFactors = FALSE))
}

# dataAddOrModify ----
# Adding or modifying records:

dataAddOrModify <- function(df, rowSelected, formOutput){
  # If the data frame is empty for  a given site:
  if(length(df) == 0){
    df <- castData(formOutput)
  } else {
    # If a row has not been selected, add row:
    if(length(rowSelected) < 1){
      df[nrow(df) + 1,] <- castData(formOutput)
      # If a row has been selected replace row:
    } else {
      df[rowSelected,] <- castData(formOutput)
    }
  }
  return(df)
}

# siteNameSubsetter ----
# Update the field inputs on the ui:

updateInputs <- function(data, fieldCodes, session) {
  for(i in 1:length(fieldCodes)){
    updateTextInput(
      session, fieldCodes[i],
      value = unname(data[fieldCodes[i]])
    )
  }
}

# showRecordInputs ----
# Show selected record in inputs:

showRecordInputs <- function(data, rowSelected, fieldCodes, session){
  if (length(rowSelected) == 1){
    fieldValues <- data[rowSelected,]
    for(i in 1:length(fieldCodes)){
      updateTextInput(
        session, fieldCodes[i],
        value = unname(fieldValues[i])
      )
    }
  }
}

# deleteRecord ----
# Delete a selected row:

deleteRecord <- function(df, rowSelected){
  if(length(rowSelected == 1)){
    return(df[-rowSelected,])
    createBlankInputs(blankFieldsEnc, session)
  }
}

# siteQuery ----
# Query mongo by site:

siteQuery <- function(siteField, inSite){
  str_c('{',
        shQuote(siteField, type = 'cmd'),
        ':',
        shQuote(inSite, type = 'cmd'),
        '}')
}

# hubQuery ----
# Query mongo by hub:

hubQuery <- function(hubField, inHub){
  str_c('{',
        shQuote(hubField, type = 'cmd'),
        ':',
        shQuote(inHub, type = 'cmd'),
        '}')
}

# hubSiteQuery ----
hubSiteQuery <- function(inHub, inSite){
  str_c('{"hub":',
        shQuote(inHub, type = 'cmd'),
        ', "site":',
        shQuote(inSite, type = 'cmd'),
        '}'
  )
}

# siteDateQuery ----
siteDateQuery <- function(siteField, inSite, dateField, inDate){
  str_c('{',
        shQuote(siteField, type = 'cmd'),
        ':',
        shQuote(inSite, type = 'cmd'),
        ', ',
        shQuote(dateField, type = 'cmd'),
        ':',
        shQuote(inDate, type = 'cmd'),
        '}')
}

# emptyDataFrame ----
# Make an empty data frame with the number of columns = the number of field codes:

emptyDataFrame <- function(fieldCodes){
  matrix(nrow = 0,
         ncol = length(fieldCodes)) %>%
    data.frame %>%
    setNames(fieldCodes)
}

# getSiteDistanceFrame ----
# This function returns a data frame of sites and their distances from a target site:

getSiteDistanceFrame <- function(targetSite){
  # Subset site location table to target site:
  targetFrame <- siteLocationTable %>%
    filter(siteID == targetSite) %>%
    select(siteID, long, lat) %>%
    left_join(siteIdTable, by = 'siteID')
  # Get all points with latitude and longitude, subset to region:
  allSitesSubset <- siteLocationTable %>%
    left_join(siteIdTable, by = 'siteID') %>%
    filter(!is.na(long),
           !is.na(lat),
           region == targetFrame$region) %>%
    select(siteID, long, lat)
  # Make spatial:
  targetSiteSp <- SpatialPoints(
    targetFrame[,c('long','lat')], 
    proj4string = CRS('+proj=longlat +datum=WGS84'))
  # Make remaining points a spatial points data frame:
  allSitesSubsetSp <- SpatialPoints(
    allSitesSubset[,c('long','lat')], 
    proj4string = CRS('+proj=longlat +datum=WGS84'))
  # Calculate the distance between all sites and target site:
  siteDistanceFrame <- allSitesSubset %>%
    select(siteID) %>%
    mutate(siteDist = distm(targetSiteSp, allSitesSubsetSp) %>%
             as.vector)
  return(siteDistanceFrame)
}

# getSitesByDistance ----
# This function returns a vector of sites within a given distance from the
# target site:

sitesByDistance <- function(targetSite, maxDistance){
  getSiteDistanceFrame(targetSite) %>%
    filter(siteDist < maxDistance) %>%
    .$siteID
}
