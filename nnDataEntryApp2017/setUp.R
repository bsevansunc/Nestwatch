#=================================================================================*
# ---- SET-UP ----
#=================================================================================*

# Sources used:

# Form input: http://deanattali.com/2015/06/14/mimicking-google-form-shiny/
# CRUD application: http://ipub.com/shiny-crud-app/
# DT instructions: http://rstudio.github.io/DT/shiny.html
# Data storage: http://deanattali.com/blog/shiny-persistent-data-storage/
# R-Dropbox Package: https://cran.r-project.org/web/packages/rdrop2/rdrop2.pdf

# Libraries:

# Survival simulation 
#===============================================================================*
# ---- SET-UP ----
#===============================================================================*

# Smart installer will check list of packages that are installed, install any
# necessary package that is missing, and load the library:

smartInstallAndLoad <- function(packageVector){
  for(i in 1:length(packageVector)){
    package <- packageVector[i]
    if(!package %in% rownames(installed.packages())){
      install.packages(packageVector[i],repos="http://cran.rstudio.com/",
                       dependencies=TRUE)
    }
  }
  lapply(packageVector, library, character.only = TRUE)
}

# No strings as factors!

options(stringsAsFactors = FALSE)

# Load and potentially install libraries:

smartInstallAndLoad(c('markdown', 'tidyverse', 'stringr',
                      'lubridate', 'shiny', 'shinyBS',
                      'R.utils', 'DT', 'shinyjs', 'mongolite'))

# Function to read then tibble:

readTbl <- function(x) read.csv(x) %>% tbl_df

# Function to put data in mongo:

mongoInsert <- function(inTable, mongoTable){
  connectionMongo <- mongo(mongoTable, url = mongoURL)
  connectionMongo$insert(inTable)
}

# Function to delete then insert a table:

mongoDeleteThenInsert <- function(inTable, mongoTable){
  connectionMongo <- mongo(mongoTable, url = mongoURL)
  connectionMongo$remove()
  connectionMongo$insert(inTable)
}

mongoURL <- "mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry"

# Function to turn NA to 'noData':

naToNoData <- function(x) ifelse(is.na(x), 'noData', x)

# Function to change names to those of the field codes:

namesToFieldCodes <- function(inData, fieldCodes){
  data <- readTbl(inData)
  names(data) <- fieldCodes
  return(data)
}

#---------------------------------------------------------------------------------*
# ---- Source functions ----
#---------------------------------------------------------------------------------*

# The functions that make things go:

source('helperFunctions.R', local = TRUE)

source('generateColorCombos.R', local = TRUE)

#---------------------------------------------------------------------------------*
# ---- Some data loading ----
#---------------------------------------------------------------------------------*

# All possible AOU codes (for point counts):

mongoUrl <- 'mongodb://bsevans:33shazam@ds025232.mlab.com:25232/nndataentry'

aouCodes <- mongoToTblDf(
  mongo('aou_codes', url = mongoUrl)$find()
)

justAlphaCode <- aouCodes %>% .$Alpha

# Generate color combinations:

# Add choice color combos as data frame:

colorValues <- c('B', 'N', 'K', 'G', 'E', 'O', 'P', 'M', 'R', 'Y', 'W')

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

# Entries for drop-down menu items:

source('fieldOptions.R', local = TRUE)

# Paragraphs, field descriptions, and such:

source('textComponents.R', local = TRUE)
