# CODE: DISPERSAL IN THE URBAN MATRIX: ASSESSING THE INFLUENCE OF LANDSCAPE
# PERMIABILITY ON THE SETTLEMENT PATTERNS OF BREEDING SONGBIRDS

#============================================================================*
# ---- Set up ---- 
#============================================================================*

options(stringsAsFactors = FALSE)

# Smart require checks to see if the libraries are present, installs them if
# necessary, and loads them:

smartRequire <- function(packages){
  # Get vector of packages listed that aren't on the current computer
  packagesToInstall <- packages[!packages %in% rownames(installed.packages())]
  if(length(packagesToInstall > 0)){
    install.packages(packagesToInstall, dependencies = TRUE)
  }
  lapply(packages, require, character.only = TRUE)
}

smartRequire(c('raster','gdistance','lubridate', 'rgeos','tidyverse',
               'rgdal', 'plotrix', 'stringr'))

# plyr, don't load the package, but install if necessary:

# install.packages('plyr')

# Fixed functions:

select <- dplyr::select

extract <- raster::extract

rename <- dplyr::rename

rescale <- plotrix::rescale

round_any <- plyr::round_any


# Projection (Same as Google Maps):

gProj <- '+init=epsg:4326'

# Set paths to raw raster files:

pathToImp <- 'nlcd_2011_impervious_2011_edition_2014_10_10/'
  
pathToCan <- 'nlcd_2011_USFS_tree_canopy_2011_edition_2016_02_08_cartographic/CONUSCartographic_2_8_16/Cartographic/'

pathToLc <- 'nlcd_2011_landcover_2011_edition_2014_10_10/'

# Load raster data:

imp <- paste0(
  pathToImp,
  'nlcd_2011_impervious_2011_edition_2014_10_10.img'
  ) %>%
  raster

can <- paste0(
  pathToCan,
  'nlcd2011_usfs_conus_canopy_cartographic.img'
) %>%
  raster

lc <- paste0(
  pathToLc,
  'nlcd_2011_landcover_2011_edition_2014_10_10.img'
) %>%
  raster

# Resolution of the 30 m pixel rasters in epsg 4326:

rasterResLonLat <- c(0.000342, 0.000262)

# Point data:

pts <- data.frame(
  region = c('atlanta', 'raleigh', 'pittsburgh', 'dc',
             'springfield', 'gainesville', 'denver'),
  long = c(-84.4, -79.5, -80.0,-77.0,-72.6,-82.3, -105),
  lat = c(33.7, 35.6,40.4,38.9,42.1,29.7, 39.7)
)

#----------------------------------------------------------------------------*
# ---- Functions ----
#----------------------------------------------------------------------------*

# Read csv, then tbl:

readTbl <- function(x) read.csv(x) %>% tbl_df

# Remove water

removeWater <- function(targetRaster, waterRaster){
  targetNoWater <- overlay(
    targetRaster, waterRaster, fun = function(x, y){
      x[y == 1] <- NA
      return(x)
    }
  )
  return(targetNoWater)
}

# Function to get the spatial centroid for a region:

projectRegionCenter <- function(hub){
  ptRegion <- pts %>%
    filter(region == hub) %>%
    select(long, lat)
  SpatialPoints(ptRegion, proj4string = CRS(gProj)) %>%
    spTransform(crs(imp))
}

# Function to get circular raster clipped to 100 km radius:

makeBufferedRaster <- function(r, hub, radLength = 140E3){
  # Points:
  centroid <- projectRegionCenter(hub)
  # Get regional buffer:
  regionExtent <- gBuffer(centroid, width = radLength)
  # Crop raster (speeds up processing time to do this first)
  rCrop <- crop(r, regionExtent) %>% trim
  # Get raster values:
  rValues <- values(rCrop)
  # Calculate distance between center and all cells, cells over 100 km are NA:
  outRaster <- distanceFromPoints(rCrop, centroid) %>%
    as.data.frame(xy = TRUE) %>%
    rename(dFromCenter = layer) %>%
    mutate(layerValue = ifelse(dFromCenter > radLength, NA, rValues),
           layerValue = ifelse(layerValue < 0, NA, layerValue),
           layerValue = ifelse(layerValue > 100, NA, layerValue)) %>%
    select(x, y, layerValue) %>%
    rasterFromXYZ(res = res(r), crs = crs(r)) %>%
    projectRaster(crs = gProj)
  return(outRaster)
}

#============================================================================*
# ---- Running: Write land cover rasters for each hub  ---- 
#============================================================================*

# Run for each hub:

regions <- pts$region

rList <- vector('list', length = length(regions))
names(rList) <- regions

for(i in 1:length(regions)){
  hub <- regions[i]
  # Get land cover raster and make water:
  lcRaw <- makeBufferedRaster(lc, hub)
  waterR <- lcRaw == 11
  # Get imp and can rasters, remove water from all:
  impR <- makeBufferedRaster(imp, hub) %>%
    removeWater(waterR)
  canR <- makeBufferedRaster(can, hub) %>%
    removeWater(waterR)
  lcR <- lcRaw %>%
    removeWater(waterR)
  # # Stack rasters and write as a csv file:
  # stack(impR, canR, lcR) %>%
  #   as.data.frame(xy = TRUE) %>%
  #   write_csv(paste('rStack', hub, sep = '_'))
  # Write raster files:
  writeRaster(impR,
              paste('imp', hub, sep = '_'), overwrite = TRUE)
  writeRaster(canR,
              paste('can', hub, sep = '_'), overwrite = TRUE)
  writeRaster(lcR,
              paste('lc', hub, sep = '_'), overwrite = TRUE)
}

#============================================================================*
# ---- Extract land cover to sites  ---- 
#============================================================================*

extractLandCoverToSites <- function(siteFrame, lcStack){
  lcVariables <- c('imp', 'can', 'lc')
  names(lcStack) <- lcVariables
  
  ptsSp <- SpatialPointsDataFrame(siteFrame %>% select(long, lat), 
                                  data = siteFrame,
                                  proj4string = CRS(gProj))
  
  
  bufferDists <- c(1, 100, 500, 1000)
  
  outerList <- vector('list', length = length(lcVariables))
  
  for(i in 1:length(lcVariables)){
    lcVar <- lcVariables[i]
    if(lcVar == 'lc'){
      outerList[[i]] <- bind_cols(
        siteFrame,
        data.frame(
          variable = 'lc',
          bufferDist = 1, 
          value = extract(lcStack$lc, ptsSp)
        )
      )
    } else {
      innerList <- vector('list', length = length(bufferDists))
      for(j in 1:length(bufferDists)){
        lcVar <- lcVariables[i]
        lcDist <- bufferDists[j]
        innerList[[j]] <- bind_cols(
          siteFrame,
          data.frame(
            variable = lcVar,
            bufferDist = bufferDists[j], 
            value = extract(lcStack[[lcVar]], ptsSp,
                            buffer = lcDist, fun = mean, na.rm = TRUE)
          )
        )
      }
      outerList[[i]] <- bind_rows(innerList)
    }
  }
  
  outFrame <- bind_rows(outerList) %>% tbl_df
  
  return(outFrame)
}

#----------------------------------------------------------------------------*
# ---- Sites  ---- 
#----------------------------------------------------------------------------*

participantActivity <- readTbl('participantActivity.csv') %>%
  rename(site = siteID)

sites <- readTbl ('siteLocationTable.csv') %>% 
  select(sitePlotID, long, lat) %>%
  rename(site = sitePlotID) %>%
  filter(site %in% participantActivity$site) %>%
  na.omit %>%
  mutate(long = ifelse(site == 'TANKLAUDC1', -76.99236, long),
         lat = ifelse(site == 'TANKLAUDC1', 38.94117, lat),
         long = ifelse(long > 0, long*-1, long))


sitesDc <- sites %>%
  filter(str_detect(str_sub(site, start = -3), 'DC')|
           str_detect(str_sub(site, start = -3), 'VA')|
           str_detect(str_sub(site, start = -3), 'MD')
  ) %>%
  distinct

sitesFl <- sites %>%
  filter(str_detect(str_sub(site, start = -3), 'FL')) %>%
  distinct

sitesGa <- sites %>%
  filter(str_detect(str_sub(site, start = -3), 'GA')) %>%
  distinct

sitesNc <- sites %>%
  filter(str_detect(str_sub(site, start = -3), 'NC')) %>%
  distinct

sitesMa <- sites %>%
  filter(str_detect(str_sub(site, start = -3), 'MA')) %>%
  distinct

sitesPa <- sites %>%
  filter(str_detect(str_sub(site, start = -3), 'PA')) %>%
  distinct

#----------------------------------------------------------------------------*
# ---- Land cover  ---- 
#----------------------------------------------------------------------------*

lcDc <- stack(
  raster('imp_dc'),
  raster('can_dc'),
  raster('lc_dc')
)

lcFl <- stack(
  raster('imp_gainesville'),
  raster('can_gainesville'),
  raster('lc_gainesville')
)

lcGa <- stack(
  raster('imp_atlanta'),
  raster('can_atlanta'),
  raster('lc_atlanta')
)


lcMa <- stack(
  raster('imp_springfield'),
  raster('can_springfield'),
  raster('lc_springfield')
)

lcNc <- stack(
  raster('imp_raleigh'),
  raster('can_raleigh'),
  raster('lc_raleigh')
)

lcPa <- stack(
  raster('imp_pittsburgh'),
  raster('can_pittsburgh'),
  raster('lc_pittsburgh')
)

#----------------------------------------------------------------------------*
# ---- Extraction  ---- 
#----------------------------------------------------------------------------*
# 
# 
# dc <- extractLandCoverToSites(sitesDc, lcDc)
# 
# fl <- extractLandCoverToSites(sitesFl, lcFl)
# 
# ga <- extractLandCoverToSites(sitesGa, lcGa)
# 
# ma <- extractLandCoverToSites(sitesMa, lcMa)
# 
# nc <- extractLandCoverToSites(sitesNc, lcNc)
# 
# pa <- extractLandCoverToSites(sitesPa, lcPa)

# Function to get separate imp, can columns for a given data frame, add hub:

makeLcHubFrame <- function(hub, hubName){
  hubDf <- hub
  # hubDf <- get(hub)
  hubImpCan <- bind_cols(
    hubDf %>%
      filter(variable == 'imp' & bufferDist == 500) %>%
      select(site, value) %>%
      rename(imp = value),
    hubDf %>%
      filter(variable == 'can' & bufferDist == 500)  %>%
      select(value) %>%
      rename(can = value)
  ) %>%
    mutate(region = hubName) %>%
    select(region, site:can) %>%
    distinct
  return(hubImpCan)
}


rasterFiles <- c('lcDc','lcFl','lcGa','lcMa','lcNc','lcPa')

siteFiles <- c('sitesDc', 'sitesFl','sitesGa',
               'sitesMa', 'sitesNc', 'sitesPa')

hubs <- c('dc', 'fl', 'ga', 'ma', 'nc', 'pa')

lcHubList <- vector('list', length = length(hubs))

for(i in 1:length(hubs)){
  hubData <- extractLandCoverToSites(
    get(siteFiles[i]), get(rasterFiles[i]))
  lcHubList[[i]] <- makeLcHubFrame(hubData, hubs[i])
}

dataAcrossHubs <- bind_rows(lcHubList) %>%
  filter(!is.na(imp)) %>%
  mutate(hub = toupper(region)) %>%
  select(hub, site, imp, can)
  
dataAcrossHubs %>%
  ggplot(aes(imp,can)) + 
  stat_bin2d(aes(fill = ..density..), binwidth = c(10, 10)) +
  scale_fill_gradientn(colors=rev(rainbow(3))) +
  facet_wrap(~region) +
  theme_bw() +
  xlab('Impervious surface cover within 500 m (%)') +
  ylab('Canopy cover within 500 m (%)')


#----------------------------------------------------------------------------*
# ---- Get visit table for current sites  ---- 
#----------------------------------------------------------------------------*

activeSites <- participantActivity %>%
  filter(site != 'UNK') %>%
  .$site 

#----------------------------------------------------------------------------*
# ---- Binding data  ---- 
#----------------------------------------------------------------------------*
# 
# dataAcrossHubs <- dataAcrossHubs %>% 
#   mutate(
#     hub = toupper(hub)
#     # factor(
#     #   region, 
#     #   levels = c('dc', 'fl', 'ga', 'ma', 'nc', 'pa'),
#     #   labels = c('DC', 'Florida', 'Georgia', 
#     #              'Massachusetts', 'N. Carolina', 'Pennsylvania'))
#   ) %>%
#   select(hub, site:can)

# Potential frame:
# 
# potential5 <- expand.grid(
#   imp = seq(5, 100, by = 5),
#   can = seq(5, 100, by = 5)
# ) %>% tbl_df %>%
#   mutate(total = imp + can) %>%
#   filter(total <= 100)
# 
# potential5Hub <- bind_rows(
#   potential5 %>% mutate(hub = 'DC'),
#   potential5 %>% mutate(hub = 'Florida'),
#   potential5 %>% mutate(hub = 'Georgia'),
#   potential5 %>% mutate(hub = 'Massachussets'),
#   potential5 %>% mutate(hub = 'N. Carolina'),
#   potential5 %>% mutate(hub = 'Pennsylvania')
# ) %>%
#   select(hub, imp, can)

# Potential 10

potential10 <- expand.grid(
  imp = seq(10, 100, by = 10),
  can = seq(10, 100, by = 10)
) %>% tbl_df %>%
  mutate(total = imp + can) %>%
  filter(total <= 100)

potential10Hub <- bind_rows(
  potential10 %>% mutate(hub = 'DC'),
  potential10 %>% mutate(hub = 'FL'),
  potential10 %>% mutate(hub = 'GA'),
  potential10 %>% mutate(hub = 'MA'),
  potential10 %>% mutate(hub = 'NC'),
  potential10 %>% mutate(hub = 'PA')
) %>%
  select(hub, imp, can)

# Proportional frame, 5% increments:
# 
# round5 <- dataAcrossHubs %>%
#   mutate(imp = plyr::round_any(imp, 5, ceiling),
#          can = plyr::round_any(can, 5, ceiling)) %>%
#   group_by(hub) %>%
#   mutate(tSites = length(unique(site))) %>%
#   group_by(hub, imp, can, tSites) %>%
#   summarize(nSitesClass = length(unique(site))) %>%
#   ungroup %>%
#   mutate(percentSites = nSitesClass/tSites*100) %>%
#   full_join(potential5Hub, by = c('hub', 'imp', 'can')) %>%
#   arrange(imp, can) %>%
#   mutate(
#     percentSites = ifelse(is.na(percentSites), 0, percentSites),
#     targetPercent = 1/nrow(potential5)*100,
#     diffPercent = percentSites-targetPercent)

# Make a data frame of the total number of sites per hub:

sitesPerHub <- dataAcrossHubs %>%
  group_by(hub) %>%
  summarize(tSites = length(unique(site))) %>%
  ungroup

sitesPerLcClass <- dataAcrossHubs %>%
  mutate(
    imp = plyr::round_any(imp, 10, ceiling),
    can = plyr::round_any(can, 10, ceiling),
    imp = ifelse(imp == 0, 10, imp),
    can = ifelse(can == 0, 10, can),
    imp = ifelse(imp > 90, 90, imp),
    can = ifelse(can > 90, 90, can)
    ) %>%
  group_by(hub, imp, can) %>%
  summarize(nSitesClass = length(unique(site))) %>%
  ungroup


round10 <- sitesPerLcClass %>%
  full_join(potential10Hub, by = c('hub', 'imp', 'can')) %>%
  left_join(sitesPerHub, by = 'hub') %>%
  group_by(hub) %>%
  mutate(potentialSites = 1/nrow(potential10)*tSites) %>%
  arrange(imp, can) %>%
  mutate(
    nSitesClass = ifelse(is.na(nSitesClass), 0, nSitesClass),
    diffSites = nSitesClass - potentialSites,
    targets = ifelse(nSitesClass == 0, 'noRep', NA),
    targets = ifelse(diffSites > 1.5, 'overRep', targets),
    targets = ifelse(diffSites > 3, 'vOverRep', targets),
    targets = ifelse(nSitesClass != 0 & diffSites < -1.5, 'underRep', targets),
    targets = ifelse(nSitesClass != 0 & diffSites <= 1.5 & diffSites >= -1.5, 'wellRep', targets),
    Representation = factor(
      targets, 
      levels = c('noRep', 'underRep', 'wellRep', 'overRep', 'vOverRep'),
      labels = c('No representation', 'Under represented', 'Well represented',
                 'Somewhat over represented', 'Very over represented'))
         )
  # mutate(
  #   # percentSites = ifelse(is.na(percentSites), 0, percentSites),
  #   targetPercent = 1/45*100,
  #   diffPercent = scale(percentSites-targetPercent)[,1]) %>%
  # mutate(diffPercent = ifelse(diffPercent > .5, 1,
  #                             ifelse(diffPercent <= -.25, -1, 0)),
  #        diffPercent = ifelse(is.na(diffPercent), -2, diffPercent),
  #        siteSelection = factor(diffPercent, 
  #                             labels = c('No sites',
  #                                        'Too few',
  #                                        'Just right',
  #                                        'Too many')))
  
round10 %>%
  ggplot(aes(x = imp, y = can)) + 
  facet_wrap(~hub) +
  geom_tile(aes(fill = Representation), color  = 'gray60') +
  # scale_fill_gradientn(colors=rev(rainbow(12))) +
  scale_fill_manual(values = c('gray50','blue', 'yellow', 'orange', 'red')) +
  theme_bw() +
  xlab('Impervious surface cover within 500 m (%)') +
  ylab('Canopy cover within 500 m (%)') +
  xlim(0, 100) +
  ylim(0, 100)



round10Imp <- dataAcrossHubs %>%
  mutate(imp = plyr::round_any(imp, 10, ceiling),
         imp = ifelse(imp == 0, 10, imp),
         imp = ifelse(imp > 90, 90, imp)) %>%
  group_by(hub) %>%
  mutate(tSites = length(unique(site))) %>%
  ungroup %>%
  group_by(hub, imp, tSites) %>%
  summarize(nSitesClass = length(unique(site))) %>%
  ungroup %>%
  full_join(potential10Hub %>%
              select(hub, imp) %>%
              distinct, by = c('hub', 'imp')) %>%
  group_by(hub) %>%
  mutate(tSites = max(tSites, na.rm = TRUE),
         potentialSites = 1/9*tSites) %>%
  ungroup %>%
  mutate(nSitesClass = ifelse(is.na(nSitesClass), 0, nSitesClass),
         diffSites = nSitesClass - potentialSites) %>%
  group_by(hub) %>%
  mutate(diffSiteScaled = scale(diffSites)[,1]) %>%
  ungroup %>%
  mutate(
    targets = ifelse(nSitesClass == 0, 'noRep', NA),
    targets = ifelse(diffSites > 3, 'overRep', targets),
    targets = ifelse(diffSites > 6, 'vOverRep', targets),
    targets = ifelse(nSitesClass != 0 & diffSites < -3, 'underRep', targets),
    targets = ifelse(nSitesClass != 0 & diffSites <= 3 & diffSites >= -3, 'wellRep', targets),
    Representation = factor(
      targets, 
      levels = c('noRep', 'underRep', 'wellRep', 'overRep', 'vOverRep'),
      labels = c('No representation', 'Under represented', 'Well represented',
                 'Somewhat over represented', 'Very over represented'))
  )

# round5Imp <- dataAcrossHubs %>%
#   mutate(imp = plyr::round_any(imp, 5, ceiling),
#          imp = ifelse(imp == 0, 5, imp),
#          imp = ifelse(imp > 95, 95, imp)) %>%
#   group_by(hub) %>%
#   mutate(tSites = length(unique(site))) %>%
#   ungroup %>%
#   group_by(hub, imp, tSites) %>%
#   summarize(nSitesClass = length(unique(site))) %>%
#   ungroup %>%
#   full_join(potential5Hub %>%
#               select(hub, imp) %>%
#               distinct, by = c('hub', 'imp')) %>%
#   group_by(hub) %>%
#   mutate(tSites = max(tSites, na.rm = TRUE),
#          potentialSites = 1/19*tSites) %>%
#   ungroup %>%
#   mutate(nSitesClass = ifelse(is.na(nSitesClass), 0, nSitesClass),
#          diffSites = nSitesClass - potentialSites) %>%
#   group_by(hub) %>%
#   mutate(diffSitesScaled = scale(diffSites)[,1]) %>%
#   ungroup %>%
#   mutate(
#     targets = ifelse(nSitesClass == 0, 'noRep', NA),
#     targets = ifelse(diffSites > 3, 'overRep', targets),
#     targets = ifelse(diffSites > 6, 'vOverRep', targets),
#     targets = ifelse(nSitesClass != 0 & diffSites < -3, 'underRep', targets),
#     targets = ifelse(nSitesClass != 0 & diffSites <= 3 & diffSites >= -3, 'wellRep', targets),
#     Representation = factor(
#       targets, 
#       levels = c('noRep', 'underRep', 'wellRep', 'overRep', 'vOverRep'),
#       labels = c('No representation', 'Under represented', 'Well represented',
#                  'Somewhat over represented', 'Very over represented'))
#   )

round10Imp %>%
  ggplot(aes(x = imp, y = diffSites)) +
  geom_bar(stat = 'identity', aes(fill = Representation), color = 'black') + 
  scale_fill_manual(values = c('gray40','blue', 'yellow', 'orange', 'red')) +
  facet_wrap(~hub) +
  geom_hline(yintercept = 0, size = 1) +
  theme_bw() +
  ylab('Number of sites') + 
  xlab('Proportion impervious surface (500 m)') +
  xlim(0, 95)


dataAcrossHubs %>%
  ggplot(aes(imp,can)) + 
  facet_wrap(~hub) +
  stat_bin2d(aes(fill = ..density..), binwidth = c(10, 10)) +
  scale_fill_gradientn(colors=rev(rainbow(3))) +
  geom_rug(aes(imp), sides = 'b') +
  geom_rug(aes(can), sides = 'l') +
  theme_bw()
# 
# rugIn %>%
#   ggplot(aes(variable, value, fill = variable)) + 
#   geom_rug(data = rugIn %>%
#              filter(variable == 'imp'), 
#            aes(factor(variable), value),
#            sides = 't',
#            color = 'orange2') +
#   geom_rug(data = rugIn %>%
#              filter(variable == 'can'), 
#            aes(factor(variable), value),
#            sides = 'b',
#            color = 'olivedrab') +
#   geom_violin(size = 1) +
#   facet_wrap(~hub) +
#   scale_fill_manual(values = c('olivedrab', 'orange2')) +
#   coord_flip() + 
#   theme_bw() +
#   xlab('Land cover metric') +
#   ylab('Proportional cover (500 m)')


#----------------------------------------------------------------------------*
# ---- Site selection, land cover  ---- 
#----------------------------------------------------------------------------*

getLandCoverRanks <- function(nnRegion){
  sitesLc <- dataAcrossHubs %>%
    filter(hub == nnRegion) %>%
    mutate(impClass = plyr::round_any(imp, 10, ceiling),
           canClass = plyr::round_any(can, 10, ceiling),
           impClass = ifelse(imp == 0, 10, impClass),
           canClass = ifelse(can == 0, 10, canClass),
           impClass = ifelse(imp > 90, 90, impClass),
           canClass = ifelse(can > 90, 90, canClass),
           imp = imp,
           can = can) %>%
    select(hub, site, imp, can, impClass, canClass) %>%
    distinct
  
  impRank <- round10Imp %>%
    filter(hub == nnRegion) %>%
    select(hub, imp, diffSites) %>%
    rename(impClass  = imp) %>%
    inner_join(sitesLc %>%
                 select(impClass) %>%
                 distinct,
               by = 'impClass') %>%
    mutate(impRank = diffSites) %>% #-rescale(diffSites, range(-1, 1))) %>%
    left_join(sitesLc %>%
                select(site, impClass), by = 'impClass') %>%
    select(site, impRank) %>% distinct
  
  impCanRank <- round10 %>%
    ungroup %>%
    filter(hub == nnRegion) %>%
    select(hub, imp, can, diffSites) %>%
    rename(impClass  = imp, canClass = can) %>%
    inner_join(sitesLc %>%
                 select(impClass, canClass) %>%
                 distinct,
               by = c('impClass', 'canClass')) %>%
    mutate(impCanRank = diffSites) %>% #-rescale(diffSites, range(-1, 1))) %>%
    left_join(sitesLc %>%
                select(site, impClass, canClass),
              by = c('impClass', 'canClass')) %>%
    select(site, impCanRank) %>%
    distinct
  
  sitesLc %>%
    left_join(impRank, by = 'site') %>% distinct %>%
    left_join(impCanRank, by = 'site') %>% distinct
  
}

nnRegions <- dataAcrossHubs$hub %>% as.character %>% unique

lcRankList <- vector('list', length = length(nnRegions))

names(lcRankList) <- nnRegions

for(i in 1:length(nnRegions)){
  lcRankList[[i]] <- getLandCoverRanks(nnRegions[i])
}

lcRankFrame <- bind_rows(lcRankList)


#----------------------------------------------------------------------------*
# ---- Add participant activity level ---- 
#----------------------------------------------------------------------------*

participantAct <- readTbl('participantActivity.csv') %>%
  rename(site = siteID) %>%
  mutate(rsEver = ifelse(nResights > 0, 1, 0),
         rsPerYr = nResights/nYears,
         rsDatesPerYr = nResightDates/nYears,
         nestsPerYr = nNests/nYears,
         engagement = ifelse(pE == 0, NA, pE)) %>%
  select(site, rsEver, nResights, rsPerYr, rsDatesPerYr,
         pResight, nestsPerYr, engagement)

siteSummaryList <- vector('list', length = length(nnRegions))

names(siteSummaryList) <- nnRegions

for(i in 1:length(nnRegions)){
  siteSummaryList[[i]] <- lcRankList[[i]] %>%
    # mutate(site = str_sub(site, end = -4)) %>%
    left_join(participantAct, by = 'site')
    # mutate(rsPerYr = rescale(rsPerYr, range(-1, 1)),
    #        rsDatesPerYr = rescale(rsDatesPerYr, range(-1, 1)),
    #        pResight = rescale(pResight, range(-1, 1)),
    #        nestsPerYr = rescale(nestsPerYr, range(-1, 1)),
    #        engagement = rescale(pE, range(-1, 1))
    # ) %>%
    # select(-pE)
}

siteSummaryFrame <- bind_rows(siteSummaryList)

# re-encounters (demographic goals):

siteSummaryWithReE <- readTbl('reEncounterSummary.csv') %>%
  rename(site = siteID) %>%
  inner_join(siteSummaryFrame,
             by = 'site') %>%
  select(hub, site:engagement)

# Value for evaluating demography:

demographicValue <- siteSummaryWithReE %>%
  select(hub, site, nCaptures, nReEncountered,
         pReEncountered, reEncounterPyear, nestsPerYr) %>%
  group_by(hub) %>%
  mutate(
    nCaptures = rescale(rank(nCaptures),  range(1,4)),
    nReEncountered = rescale(rank(nReEncountered),  range(1,4)),
    pReEncountered = rescale(rank(pReEncountered),  range(1,4)),
    reEncounterPyear = rescale(rank(reEncounterPyear),  range(1,4)),
    nestsPerYr = rescale(rank(nestsPerYr),  range(1,4))
  ) %>%
  group_by(site) %>%
  mutate(demographicValue = sum(nCaptures, nReEncountered,
                                pReEncountered,reEncounterPyear,
                                nestsPerYr, na.rm = TRUE)) %>%
  group_by(hub) %>%
  mutate(demographicRank = rescale(demographicValue, range(1, 4))) %>%
  ungroup %>%
  distinct

# Value for assessing the rural-to-urban gradient:

lcValue <- lcRankFrame %>%
  select(hub, site, impRank,impCanRank) %>%
  group_by(hub) %>%
  mutate(
    impRank = rescale(-impRank, range(1, 8)),
    impCanRank = rescale(-impCanRank, range(1, 4))
  ) %>%
  ungroup %>%
  mutate(lcValue = rowSums(.[,3:4], na.rm = TRUE)) %>%
  group_by(hub) %>%
  mutate(lcRank = rescale(rank(lcValue), range(1,4))) %>%
  ungroup %>%
  distinct 

# Citizen science value:

citSciValue <- siteSummaryWithReE %>%
  select(hub, site) %>%
  left_join(participantAct, by = 'site') %>%
  group_by(hub) %>%
  mutate(
    nResights = rescale(nResights, range(1, 4)),
    rsPerYr = rescale(rsPerYr, range(1, 4)),
    rsDatesPerYr = rescale(rsDatesPerYr, range(1, 4)),
    pResight = rescale(pResight, range(1, 4))#,
    # nestsPerYr = rescale(nestsPerYr, range(1, 4))
  ) %>%
  ungroup %>%
  mutate(citSciValue = rowSums(.[,3:7], na.rm = TRUE)) %>%
  group_by(hub) %>%
  mutate(citSciRank = rescale(rank(citSciValue), range(1,4))) %>%
  ungroup %>%
  distinct

cullFrame <- siteSummaryFrame %>%
  filter(!site %in% c('WOODLGAGA1')) %>%
  filter(!str_detect(site, 'NZP')) %>%
  filter(!site %in% c('WARDDOUVA1','MARRPETMD2','BUCKLMSMD1', 'LAKEAESVA1',
                      'SLIGOPKMD1', 'OPALDPAMD1','BAILEESVA1', 'SPRINPKMD1',
                      'FORESESMD1', 'ROSAPESMD1','BROOKSIMD1',
                      'WHEATPKMD1', 'CAPITCSDC1','HERNELEVA1','JAMESESVA1',
                      'MAHOJIMMD1','RCPSMBCDC1','WAPLEESVA1', 'BLADENSDC1')) %>%
  select(hub, site) %>%
  inner_join(demographicValue %>%
               select(site, demographicRank),
             by = 'site') %>%
  inner_join(lcValue %>%
               select(site, lcRank),
             by = 'site') %>%
  inner_join(citSciValue %>%
               select(site, citSciRank),
             by = 'site') %>%
  inner_join(siteSummaryFrame %>%
               select(site, engagement) %>%
               distinct,
             by = 'site') %>%
  inner_join(siteSummaryWithReE %>%
               select(site, nYears) %>%
               distinct,
             by = 'site') %>%
  mutate(engagement = ifelse(site == 'JACOSYDMD1', 4, engagement)) %>%
  mutate(engagement = ifelse(site == 'DONOLORDC2', 4, engagement)) %>%
  mutate(engagement = ifelse(site == 'TATEJUAMD1', 4, engagement)) %>%
  mutate(score = rowMeans(.[,3:6], na.rm = TRUE)) %>%
  group_by(hub) %>%
  mutate(siteRank = rank(-score)) %>%
  arrange(hub, siteRank) %>%
  mutate(dontCull = ifelse(nYears == 1|demographicRank >= 3.5|lcRank >= 3.5|citSciRank >=3, 1, 0))  %>%
  rename(demographicScore = demographicRank,
         urbanGradientScore = lcRank,
         citSciScore = citSciRank) %>%
  ungroup %>%
  arrange(hub, site)

write_csv(cullFrame, 'cullFrame.csv')

cullFrame %>%
  filter(hub == 'DC') %>% arrange(siteRank) %>% View

cullFrame %>%
  filter(hub == 'DC') %>%
  .$dontCull %>% sum
# 
# 
# cullFrame <- siteSummaryFrame %>%
#   select()
#   select(-c(imp:canClass)) %>%
#   filter(!str_detect(site, 'NZP')) %>%
#   filter(!site %in% c('WARDDOUVA1','MARRPETMD2','BUCKLMSMD1', 'LAKEAESVA1',
#                       'SLIGOPKMD1', 'OPALDPAMD1','BAILEESVA1', 'SPRINPKMD1',
#                       'FORESESMD1', 'ROSAPESMD1','BROOKSIMD1',
#                       'WHEATPKMD1', 'CAPITCSDC1','HERNELEVA1','JAMESESVA1',
#                       'MAHOJIMMD1','RCPSMBCDC1','WAPLEESVA1', 'BLADENSDC1')) %>%
  # mutate(engagement = ifelse(site == 'JACOSYDMD1', 4, engagement)) %>%
  # mutate(engagement = ifelse(site == 'DONOLORDC2', 4, engagement)) %>%
  # mutate(engagement = ifelse(site == 'TATEJUAMD1', 4, engagement)) %>%
#   group_by(hub) %>%
#   select(-nResights) %>%
#   # Scale variables by hub:
#   mutate(
#     impRank = rescale(impRank, range(1, 4)),
#     impCanRank = rescale(impCanRank, range(1, 4)),
#     rsEver = rsEver,
#     rsPerYr = rescale(rank(rsPerYr), range(1,4)),
#     rsDatesPerYr = rescale(rank(rsDatesPerYr),  range(1,4)),
#     pResight = rescale(rank(pResight), range(1,4)),
#     nestsPerYr = rescale(rank(nestsPerYr), range(1,4)),
#     # Experience with participant ranking:
#     engagement = rescale(engagement,  range(1,4))
#   ) %>%
#   group_by(hub, site) %>%
#   # Sum class variables:
#   mutate(
#     lcScore = sum(impRank, impCanRank, na.rm = TRUE),
#     citSciScore = sum(rsEver,rsPerYr,rsDatesPerYr,pResight, nestsPerYr, na.rm = TRUE)
#   ) %>%
#   ungroup %>%
#   group_by(hub) %>%
#   mutate(
#     lcScore = rescale(lcScore, range(1,5)),
#     citSciScore = rescale(citSciScore, range(1,4))
#   ) %>%
#   ungroup %>%
#   mutate(score = rowMeans(.[10:12], na.rm  = TRUE)) %>%
#   group_by(hub) %>%
#   mutate(participantRank = rank(-score)) %>%
#   ungroup %>%
#   select(hub, site, engagement:participantRank)
#   
# cullFrame %>% filter(hub == 'DC') %>%
#   arrange(desc(participantRank)) %>%
#   View
# 
# 
# cullFrame %>% filter(hub == 'DC') %>%
#   arrange(participantRank)
#     
# 
#   
#   write_csv('cullingFrame.csv')
#     



