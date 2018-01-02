# Just site names

# site, oldSite, lastName, firstName, longitude, latitude, streetAddress, city, state, zip

# Pittsburgh:

#=================================================================================*
# ---- PITTSBURGH ----
#=================================================================================*
# setwd('/Users/bsevans/Dropbox/NeighborhoodNestwatch_bandingData')

pittSites <- read.csv('Pittsburgh_site_names.csv') %>%
  tbl_df %>%
  mutate(l1 = toupper(Last.Name),
         f1 = toupper(First.Names),
         newSite = str_c(toupper(last) %>%
                           str_sub(end = 4),
                         toupper(first) %>%
                           str_sub(end = 3),
                         'PA1')) %>%
  select(l1, f1, last, first, newSite) %>%
  distinct %>%
  select(newSite, l1, last, first) %>%
  mutate(newSite = ifelse(l1 == 'DEMARCO_M', 'DEMAMARPA1', newSite),
         newSite = ifelse(l1 == 'MCCLELLAN ELEMENTARY SCHOOL',
                          'MCCLELEPA1', newSite)) %>%
  rename(site = newSite, oldSite = l1, lastName = last, firstName = first)

# Some spatial data:

read.csv('paBanding0.csv', stringsAsFactors = FALSE) %>% tbl_df %>%
  select(Last.Name, Address, City, State, Zip, Latitude, Longitude) %>%
  distinct %>%
  rename(lastName = Last.Name, streetAddress = Address,
         city = City, state = State, zip = Zip, 
         latitude = Latitude, longitude = Longitude) %>%
  select(lastName, longitude, latitude, streetAddress:zip)


# Note: Add Contact information (at some point, not now):
# site, lastName, firstName, email1, email2, phone1, phone2