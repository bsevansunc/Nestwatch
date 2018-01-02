# Script to generate color combinations.

#===============================================================================*
# ---- SET UP ----
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

# Load and potentially install libraries:

smartInstallAndLoad(c('tidyverse', 'stringr', 'lubridate'))

#===============================================================================*
# ---- FUNCTIONS ----
#===============================================================================*

# Function to generate color combinations:
# Inputs are:
# - colorVector: A vector of colors (does not include aluminum)
# - nBands: The number of total bands (including aluminum)
# - xPositions: The location of the aluminum band (defaults to any):
#     * 1 = Left leg, top
#     * 2 = Left leg, bottom
#     * 3 = Right leg, top
#     * 4 = Right leg, bottom
# - aluminum: TRUE/FALSE (defaults to TRUE), whether you want combinations
#      that include an aluminum band

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

# Function to return a data frame of color combos, given a data frame of used
# combos. Inputs include:
# - colorComboFrame: Generated color combination data frame from output of
#   getColorCombos, with the columns colorL and colorR
# - previousComboFrame: Data frame of used color combinations with the
#   columns colorL and colorR

getUnusedCombos <- function(colorComboFrame, previousComboFrame){
  newCombos <- colorComboFrame %>%
    anti_join(
      previousComboFrame,
      by = c('colorL', 'colorR')
    )
  return(
    sample_n(newCombos, nrow(newCombos))
  )
}

# Function to return a data frame of color combos, given a data frame of combos
# used and the last date in which a bird was observed. Inputs include:
# - colorComboFrame: Generated color combination data frame from output of
#   getColorCombos, with the columns colorL and colorR
# - comboDateFrame: Data frame of used color combinations with the
#   columns colorL and colorR and a date column (ISO 8601) with the last
#   date a bird was observed
# - yearBuffer: The number of years in which a color combination should be
#   exclude. This buffer is designed as "less than or equal to" so if, for
#   example, the year buffer is 3, the output would exlude any combination
#   used in the last 3 years.

getCombosByYearBuffer <- function(colorComboFrame, comboDateFrame, yearBuffer){
  currentYear <- year(Sys.Date())
  combosToExclude <- comboDateFrame %>%
    filter(currentYear - year(date) <= yearBuffer)
  return(
    getUnusedCombos(colorComboFrame, combosToExclude)
  )
}