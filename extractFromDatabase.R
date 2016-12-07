#=================================================================================*
# ---- SET-UP ----
#=================================================================================*
# setwd('C:/Users/Brian/Dropbox/nnDataEntryApp')
# setwd('/Users/bsevans/Dropbox/nnDataEntryApp')

library(readr)
library(mongolite)
library(tidyr)
library(dplyr)

# Convert mongo data frame to a dplyr table dataframe:

mongoToTblDf <- function(data){
  if(nrow(data) > 0 & ncol(data) > 0){
    for(j in 1:ncol(data)){
      data[,j] <- as.character(data[,j])
    }
    data <- data %>% tbl_df
  }
  data
}

# Set url for mongo:

mongoURL <- 'mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry'

siteMongo <- mongo('site_data', url = mongoURL)

#=================================================================================*
# ---- GET DATA ----
#=================================================================================*

siteTable <- siteMongo$find(
  fields = '{"_row" : 0, "_id" : 0}') %>%
  mongoToTblDf


visitMongo <- mongo('visit_data', url = mongoURL)

visitTable <- visitMongo$find(
  fields = '{"_row" : 0, "_id" : 0}'
  ) %>%
  mongoToTblDf

encMongo <- mongo('encounter_data', url = mongoURL)

encTable <- encMongo$find(
  fields = '{"_row" : 0, "_id" : 0}') %>%
  mongoToTblDf

encTable %>%
  group_by(bandNumber) %>%
  summarize(nSite = length(unique(site))) %>%
  filter(nSite > 1)

pcMongo <- mongo('pointCount_data', url = mongoURL)

pcTable <- pcMongo$find(
  fields = '{"_row" : 0, "_id" : 0}') %>%
  mongoToTblDf
  
nestSummaryMongo <- mongo('nest_summary_data', url = mongoURL)

nestSummaryTable <- nestSummaryMongo$find(
  fields = '{"_row" : 0, "_id" : 0}') %>%
  mongoToTblDf

nestMongo <- mongo('nest_observation_data', url = mongoURL)

nestTable <- nestMongo$find(
  fields = '{"_row" : 0, "_id" : 0}') %>%
  mongoToTblDf
  
 