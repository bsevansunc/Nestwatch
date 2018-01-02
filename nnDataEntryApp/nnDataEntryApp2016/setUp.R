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

library(markdown)
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(shinyBS)
library(R.utils)
# library(rdrop2)
# library(googlesheets)
library(DT)
library(shinyjs)
library(mongolite)

#---------------------------------------------------------------------------------*
# ---- Source functions ----
#---------------------------------------------------------------------------------*

# The functions that make things go:

source('helperFunctions.R', local=TRUE)

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

colorValues <- c('', 'A', 'BU', 'BR', 'BK', 'G','GY', 'O','PK', 'P','R', 'Y', 'W')

choiceColorCombos <- expand.grid(rep(list(colorValues), 4)) %>%
  tbl_df %>%
  transmute(L = paste(Var1, Var2, sep = '/'),
            R = paste(Var3, Var4, sep = '/'), 
            combo = paste(L, R, sep = ',')) %>%
  select(-c(L, R)) %>%
  filter(str_count(combo, 'A') == 1) %>%
  mutate(combo = combo %>%
           str_replace_all('/,', ',') %>%
           str_replace_all(',/', ',') %>%
           str_replace_all(',$',',-') %>%
           str_replace_all('^,', '-,') %>%
           str_replace_all('^/', '') %>%
           str_replace_all('/$', '')) %>%
  distinct %>% 
  arrange(combo) %>%
  .$combo

choiceColorCombos <- c('', choiceColorCombos)

# Entries for drop-down menu items:

source('fieldOptions.R', local = TRUE)

# Paragraphs, field descriptions, and such:

source('textComponents.R', local = TRUE)

