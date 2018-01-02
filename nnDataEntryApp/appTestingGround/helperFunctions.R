# FUNCTIONS

#---------------------------------------------------------------------------------*
# ---- Site list update ----
#=================================================================================*

siteNameSubsetter <- function(inHub){
  encounters %>%
    select(hub, site) %>%
    distinct %>% 
    bind_rows(
      data.frame(hub = c('DC', 'Atlanta', 'Gainesville',
                         'Pittsburgh', 'Raleigh', 'Springfield'),
                 site = rep('', 6))) %>%
    filter(hub == inHub) %>%
    arrange(site) %>%
    .$site
}

#---------------------------------------------------------------------------------*
# ---- Create, Read, Update, Delete ----
#=================================================================================*

# Function assigns fancy column headers to the field codes:

getTableMetadata <- function(fieldCodes, fieldNames) {
  fields <- fieldNames
  names(fields) <- fieldCodes
  result <- list(fields = fields)
  return (result)
}

# Function takes values of inputs, if there are any, and puts them in a 1-row data frame:

castData <- function(fieldValues){
  return(data.frame(t(as.list(fieldValues)),
                    stringsAsFactors = FALSE))
}

# Make some submissions blank:

createBlankInputs <- function(fieldCodes, session){
  for(i in 1:length(fieldCodes)){
    updateTextInput(session, fieldCodes[i], value = '')
  }
}

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

# Update the field inputs on the ui:

updateInputs <- function(data, fieldCodes, session) {
  for(i in 1:length(fieldCodes)){
    updateTextInput(
      session, fieldCodes[i],
      value = unname(data[fieldCodes[i]])
    )
  }
}

# Show selected record in inputs:

# showRecordInputs <- function(data, rowSelected, fieldCodes, session){
#   if (length(rowSelected) == 1){
#     updateInputs(data[rowSelected,], fieldCodes, session)
#   }
# }

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

# Delete a selected row:

deleteRecord <- function(df, rowSelected){
  if(length(rowSelected == 1)){
    return(df[-rowSelected,])
    createBlankInputs(blankFieldsEnc, session)
  }
}

# Query mongo by site:

siteQuery <- function(siteField, inSite){
  str_c('{',
        shQuote(siteField, type = 'cmd'),
        ':',
        shQuote(inSite, type = 'cmd'),
        '}')
  # ', "_id" : 0}')
}

# Query mongo by hub:

hubQuery <- function(hubField, inHub){
  str_c('{',
        shQuote(hubField, type = 'cmd'),
        ':',
        shQuote(inHub, type = 'cmd'),
        '}')
}

hubSiteQuery <- function(inHub, inSite){
  str_c('{"hub":',
        shQuote(inHub, type = 'cmd'),
        ', "site":',
        shQuote(inSite, type = 'cmd'),
        '}'
  )
}

# Convert mongo data frame to a dplyr table dataframe

mongoToTblDf <- function(data){
  #   hubs <- unlist(unique(data$hub))
  #   hubList <- vector('list', length = length(hubs))
  #   for(i in 1:length(hubs)){
  #     dataHubSubset <- data[data$hub == hubs[i],]
  # for(i in 1:nrow(dataHubSubset)){
  if(nrow(data) > 0 & ncol(data) > 0){
    for(j in 1:ncol(data)){
      data[,j] <- as.character(data[,j])
      # }
    }
    data <- data %>% tbl_df
  }
  data
  # hubList[[i]] <- dataHubSubset
  # }
  # bind_rows(hubList) %>% tbl_df
}

emptyDataFrame <- function(fieldCodes){
  matrix(nrow = 0,
         ncol = length(fieldCodes)) %>%
    data.frame %>%
    setNames(fieldCodes)
}

# exampleFrame <- data.frame(hub = c('a', 'b'), obs1 = c(1,2), obs2 = c(2,1))

# here's what i'm thinking ... data where there is a null value for a given field eliminates the field ... perhaps this should be come a table data frame after bind it with a data frame that's blank but defined by the proper fields.



