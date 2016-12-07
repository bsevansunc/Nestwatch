#=================================================================================*
# ---- SERVER ----
#=================================================================================*
# setwd('C:/Users/Brian/Dropbox/nnDataEntryApp')
# setwd('/Users/bsevans/Dropbox/nnDataEntryApp')

source('setUp.R', local=TRUE)
library(readr)
library(mongolite)
library(tidyr)
library(dplyr)

library(ghit)
devtools::install_github("ropensci/EML", dependencies=c("Depends", "Imports"))
library(EML)


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

mongoURL <- 'mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry'

siteMongo <- mongo('site_data', url = mongoURL)

siteTable <- siteMongo$find(
  fields = '{"_row" : 0, "_id" : 0}') %>%
  mongoToTblDf

###
attributes <-
  data.frame(
    attributeName = names(siteTable), 
    attributeDefinition = c(
      "Metropolitan Region",
      "Site code",
      "Last name of participant of name of school",
      "First name of participant",
      "Longitude of site",
      "Latitude of site",
      "Accuracy of site coordinate",
      "Address of site",
      "First year as participant",
      "Notes on the location of the site",
      "Primary phone number",
      "Secondary phone number",
      "Primary email",
      "Secondary email",
      "Additional notes"),
    formatString = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      "House number, Street, City, FIPS state, Zip code",
      "YYYY",
      NA,
      "XXX-XXX-XXXX",
      "XXX-XXX-XXXX",
      NA,
      NA,
      NA),
    definition = c(
      'City with no state. District of Columbia is provided as DC',
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA),
    unit = c(
      NA,
      NA,
      NA,
      NA,
      'degree',
      'degree',
      'meter',
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA),
    numberType = c(
      NA,
      NA,
      NA,
      NA,
      'real',
      'real',
      'integer',
      NA,
      NA,
      'integer',
      NA,
      NA,
      NA,
      NA,
      NA),
    stringsAsFactors = FALSE
  )

attributeList <- set_attributes(
  attributes,
  col_classes = c(
    "character",
    "character",
    "character",
    "character",
    "numeric",
    "numeric",
    "numeric",
    "character",
    "Date",
    "character",
    "character",
    "character",
    "character",
    "character",
    "character"))

physical <- set_physical("siteData.csv")

dataTable <- new("dataTable",
                 entityName = "siteData.csv",
                 entityDescription = "neighborhood nestwatch site data",
                 physical = physical,
                 attributeList = attributeList)

geographicDescription <- 'Residintial habitats in the metropolitan regions of Atlanta, GA, Gainesville, FL, Pittsburgh, PA, Raleigh, NC, Springfield, PA, and Washington DC.'

coverage <- 
  set_coverage(begin = '2000-01-01', end = '2016-12-31',
               sci_names = data.frame(
                 kingdom = 'Animalia',
                 phylum = 'Chordata',
                 class = 'Aves'),
               geographicDescription = geographicDescription,
               west = -80.4, east = -72.2, 
               north = 42.7, south = 28.8)

methods_file <- system.file("examples/hf205-methods.docx", package = "EML")
methods <- set_methods(methods_file)

R_person <- as.person("Brian Evans <EvansBr@si.edu>")
brian <-as(R_person, "creator")

bob <- as.person("Robert Reitsma [ctb] <ReitsmaR@si.edu>")
associatedParty <- as(bob, "associatedParty")

smbcAddress <- new(
  "address",
  deliveryPoint = "Smithsonian Migratory Bird Center\nNational Zoological Park\nMRC5003",
  city = "Washington",
  administrativeArea = "DC",
  postalCode = "20013-7012",
  country = "USA"
  )

publisher <- new(
  "publisher",
  organizationName = "Smithsonian Migratory Bird Center",
  address = smbcAddress
)


###

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
  
 