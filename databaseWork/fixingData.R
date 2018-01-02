
read_csv('nnData/measurementTable.csv') %>%
  mutate(visitID = str_replace(visitID, '-', '_'),
         breedingCondition = ifelse(str_detect(breedingCondition, 'BP'), 'BP', breedingCondition),
         breedingCondition = ifelse(str_detect(breedingCondition, 'CP'), 'CP', breedingCondition),
         breedingCondition = ifelse(!(breedingCondition %in% c('CP', 'BP')), NA, breedingCondition)) %>%
  mutate(fat = ifelse(fat == 'T', .5, fat),
         fat = ifelse(fat == '.5.5', .5, fat),
         fat = ifelse(fat %in% c('HT', '-', 'CP', 'BP', 'CP-', 'NO DATA', 'LT','U','9'), NA, fat),
         fat = as.numeric(fat)) %>%
  select(birdID:rectrices) %>%
  write_csv('nnData/measurementTable.csv')

birdTable <- read_csv('nnData/birdTable.csv')

birdTableLLegFix <- birdTable %>%
  mutate(colorComboL = ifelse(is.na(colorComboL), '-', colorComboL),
         colorComboR = ifelse(colorComboL == 'A(R.LEG)', 'X', colorComboR),
         colorComboL = ifelse(colorComboL == 'A(R.LEG)', '-', colorComboL),
         colorComboL = ifelse(colorComboL == 'NONE', '-', colorComboL),
         colorComboL = str_replace_all(colorComboL, 'DB', 'B'),
         colorComboL = str_replace_all(colorComboL, 'BU', 'B'),
         colorComboL = str_replace_all(colorComboL, 'LB', 'B'),
         colorComboL = str_replace_all(colorComboL, 'BL', 'B'),
         colorComboL = str_replace_all(colorComboL, 'LG', 'G'),
         colorComboL = str_replace_all(colorComboL, 'GR', 'G'),
         colorComboL = str_replace_all(colorComboL, 'DG', 'G'),
         colorComboL = str_replace_all(colorComboL, 'PI', 'Z'),
         colorComboL = str_replace_all(colorComboL, 'Y/MG', 'YG'),
         colorComboL = str_replace_all(colorComboL, 'PNK', 'Z'),
         colorComboL = str_replace_all(colorComboL, 'PK', 'Z'),
         colorComboL = str_replace_all(colorComboL, 'PU', 'M'),
         colorComboL = str_replace_all(colorComboL, '0', 'O'),
         colorComboL = str_replace_all(colorComboL, 'GY', 'E'),
         colorComboL = str_replace_all(colorComboL, '\n', ''),
         colorComboL = str_replace_all(colorComboL, 'WH', 'W'),
         colorComboL = str_replace_all(colorComboL, 'MG/A', 'GX'),
         colorComboL = str_replace_all(colorComboL, 'BK', 'K'),
         species = ifelse(colorComboL == 'CARW', 'CARW', species),
         species = ifelse(colorComboL == 'SOSP', 'SOSP', species),
         colorComboL = ifelse(colorComboL == 'CARW', 'UNK', colorComboL),
         colorComboL = ifelse(colorComboL == 'SOSP', 'UNK', colorComboL),
         colorComboR = ifelse(colorComboL == 'R.A/R', 'XR', colorComboR),
         colorComboL = ifelse(colorComboL == 'R.A/R', 'R', colorComboL),
         colorComboR = ifelse(colorComboL == 'BK.A/P', 'XM', colorComboR),
         colorComboL = ifelse(colorComboL == 'BK.A/P', 'K', colorComboL),
         colorComboL = ifelse(colorComboL %in% c('ND','7/3/2-14'), 'UNK', colorComboL),
         colorComboR = ifelse(colorComboL == 'BK.A/G', 'XG', colorComboR),
         colorComboL = ifelse(colorComboL == 'BK.A/G', 'KX', colorComboL),
         colorComboR = ifelse(colorComboL == 'B.A/B', 'XB', colorComboR),
         colorComboL = ifelse(colorComboL == 'B.A/B', 'B', colorComboL),
         colorComboR = ifelse(colorComboL == 'A.Y/Y', 'YY', colorComboR),
         colorComboL = ifelse(colorComboL == 'A.Y/Y', 'X', colorComboL),
         colorComboR = ifelse(colorComboL == 'R.W/A', 'WX', colorComboR),
         colorComboL = ifelse(colorComboL == 'R.W/A', 'R', colorComboL),
         colorComboR = ifelse(colorComboL == 'B.A/R', 'XR', colorComboR),
         colorComboL = ifelse(colorComboL == 'B.A/R', 'B', colorComboL),
         colorComboR = ifelse(colorComboL == 'A.O/R', 'OR', colorComboR),
         colorComboL = ifelse(colorComboL == 'A.O/R', 'X', colorComboL),
         colorComboR = ifelse(colorComboL == 'A.Y/O', 'YO', colorComboR),
         colorComboL = ifelse(colorComboL == 'A.Y/O', 'X', colorComboL),
         colorComboR = ifelse(colorComboL == 'G/A.BK', 'XK', colorComboR),
         colorComboL = ifelse(colorComboL == 'G/A.BK', 'G', colorComboL),
         colorComboL = ifelse(colorComboL == 'Y.R', 'UNK', colorComboL),
         colorComboR = ifelse(colorComboL == 'A/G.W', 'W', colorComboR),
         colorComboL = ifelse(colorComboL == 'A/G.W', 'XG', colorComboL),
         colorComboR = ifelse(colorComboL == 'K.A/P', 'XM', colorComboR),
         colorComboL = ifelse(colorComboL == 'K.A/P', 'K', colorComboL),
         colorComboR = ifelse(colorComboL == 'G/A.K', 'XK', colorComboR),
         colorComboL = ifelse(colorComboL == 'G/A.K', 'G', colorComboL),
         colorComboR = ifelse(colorComboL == 'K.A/G', 'G', colorComboR),
         colorComboL = ifelse(colorComboL == 'K.A/G', 'KX', colorComboL),
         colorComboL = ifelse(colorComboL == 'P' & colorComboR != 'XK', 'M', colorComboL),
         colorComboL = ifelse(colorComboL == 'PP', 'ZZ', colorComboL),
         colorComboL = ifelse(colorComboL == 'PX', 'ZX', colorComboL),
         colorComboL = ifelse(colorComboL == 'BP', 'BZ', colorComboL),
         colorComboL = ifelse(colorComboL == 'YP', 'YZ', colorComboL),
         colorComboL = ifelse(colorComboL == 'XP', 'XZ', colorComboL),
         colorComboL = ifelse(colorComboL == 'OP', 'OZ', colorComboL),
         colorComboL = str_replace_all(colorComboL, 'P', 'M'),
         colorComboL = ifelse(str_count(colorComboL, "\\/") > 1, 'UNK', colorComboL),
         colorComboL = str_replace_all(colorComboL, '\\-/', ''),
         colorComboL = str_replace_all(colorComboL, 'A', 'X'),
         colorComboL = str_replace_all(colorComboL, 'Z', 'P'),
         colorComboL = str_replace_all(colorComboL, 'BR', 'N'),
         colorComboL = str_replace_all(colorComboL, 'UB', '-'),
         colorComboL = str_replace_all(colorComboL, 'RX/W', 'X/W'),
         colorComboL = str_replace_all(colorComboL, 'X/X', 'X'),
         colorComboL = str_replace_all(colorComboL, '\\/', ''))

birdTableLRfix <- birdTableLLegFix %>%
  mutate(colorComboR = str_replace_all(colorComboR, '\\--', '-'),
         colorComboR = str_replace_all(colorComboR, '\\-/', ''),
         colorComboR = str_replace_all(colorComboR, '\\.', '/'),
         colorComboR = str_replace_all(colorComboR, '\\,', '/'),
         colorComboR = str_replace_all(colorComboR, 'PK', 'Z'),
         colorComboR = str_replace_all(colorComboR, 'PI', 'Z'),
         colorComboR = str_replace_all(colorComboR, 'BK', 'K'),
         colorComboR = str_replace_all(colorComboR, 'BR', 'N'),
         colorComboR = str_replace_all(colorComboR, 'CA', 'A'),
         colorComboR = str_replace_all(colorComboR, 'WHX', 'WX'),
         colorComboR = str_replace_all(colorComboR, 'DB', 'B'),
         colorComboR = str_replace_all(colorComboR, 'BU', 'B'),
         colorComboR = str_replace_all(colorComboR, 'Bu', 'B'),
         colorComboR = str_replace_all(colorComboR, 'BL', 'B'),
         colorComboR = str_replace_all(colorComboR, 'DG', 'G'),
         colorComboR = str_replace_all(colorComboR, 'GY', 'E'),
         colorComboR = str_replace_all(colorComboR, 'GR', 'G'),
         colorComboR = str_replace_all(colorComboR, 'PU', 'M'),
         colorComboR = str_replace_all(colorComboR, '\\\\O', 'O'),
         colorComboR = str_replace_all(colorComboR, '\\?', ''),
         colorComboR = str_replace_all(colorComboR, 'P', 'M'),
         colorComboR = str_replace_all(colorComboR, '\\\n', ''),
         colorComboR = str_replace_all(colorComboR, 'Z', 'P'),
         colorComboR = str_replace_all(colorComboR, 'A', 'X'),
         colorComboR = ifelse(is.na(colorComboR), 'UNK', colorComboR),
         colorComboR = str_replace_all(colorComboR, '/','')
  )

birdTableSpFix <- birdTableLRfix %>% filter(!is.na(birdID)) %>% filter(!is.na(species)) %>% 
  filter(species != 'BAND LOST') %>% 
  filter(species != 'ABDU') %>%
  distinct

birdTableSpFix %>%
  group_by(birdID) %>%
  summarize(count = n()) %>%
  ungroup %>%
  filter(count > 1) %>%
  left_join(captureTable, by = 'birdID') %>%
  select(visitID) %>%
  distinct %>%
  separate(visitID, into = c('site', 'date'), sep = '\\_') %>%
  arrange(site) %>%
  write_csv('getTheseVisitSheets.csv')

measurementTable %>% filter(birdID == '-')

write_csv(birdTableSpFix, 'nnData/birdTable.csv')


captureTable <- read_csv('nnData/captureTable.csv')

birdTableSpFix %>%
  select(birdID, colorComboL, colorComboR) %>%
  inner_join(captureTable %>%
               select(-c(bandNumber:colorComboR)),
             by = 'birdID')

problemBirds <- birdTableSpFix %>%
  distinct %>%
  mutate_all(str_trim) %>%
  arrange(birdID) %>%
  group_by(birdID, species) %>%
  summarize(n = n()) %>%
  filter(n > 1) %>%
  .$birdID

birdTableSpFix %>%
  mutate_all(str_trim) %>%
  filter(birdID %in% problemBirds) %>%
  arrange(birdID)

captureTableBandFix <- captureTable %>%
  mutate_all(str_trim) %>%
  filter(birdID %in% problemBirds) %>%
  arrange(birdID) %>%
  filter(visitID != 'SIMPEDWNC1_2016-05-22') %>%
  filter(!(visitID == 'MCCOJOHMD1_2004-05-11' & bandNumber == '1841-58960' & is.na(colorComboL))) %>%
  filter(visitID != 'WISSSTEGA1_2016-05-03') %>%
  mutate(
         bandNumber = ifelse(bandNumber == '1352-27683' & colorComboL == 'O/R', '1352-27684', bandNumber),
         bandNumber = ifelse(bandNumber == '1991-18052' & visitID == 'ENNEBECFL1_2013-05-07', '1991-18052B', bandNumber),
         bandNumber = ifelse(bandNumber == '2180-92454' & visitID == 'BAILPATNC1_2015-05-19', '2180-92452', bandNumber),
         bandNumber = ifelse(bandNumber == '2331-09133' & visitID == 'ADAMVICFL1_2013-05-18', '2331-09135', bandNumber),
         bandNumber = ifelse(bandNumber == '2430-14749' & visitID == 'BRUNKIMPA1_2015-06-16', '2430-14749B', bandNumber),
         bandNumber = ifelse(bandNumber == '2541-40415' & colorComboL == 'A', '2541-40415B', bandNumber),
         bandNumber = ifelse(bandNumber == '2571-70429' & colorComboR == 'G/BU', '2571-70430', bandNumber),
         bandNumber = ifelse(bandNumber == '2571-70452' & colorComboL == 'Y/P', '2571-70460', bandNumber),
         bandNumber = ifelse(bandNumber == '2571-70474' & colorComboL == 'R/Y', '2571-70476', bandNumber),
         bandNumber = ifelse(bandNumber == '2640-61857' & colorComboL == 'W/A', '2640-61862', bandNumber),
         bandNumber = ifelse(bandNumber == '2651-43372' & colorComboR == 'A', '2651-43371', bandNumber),
         bandNumber = ifelse(bandNumber == '2651-43447' & colorComboL == 'R', '2651-43497', bandNumber),
         bandNumber = ifelse(bandNumber == '2651-56680' & colorComboL == 'B', '2651-56681', bandNumber),
         bandNumber = ifelse(bandNumber == '2690-87711' & colorComboR == 'W/O', '2690-87711B', bandNumber),
         bandNumber = ifelse(bandNumber == '2711-77124' & colorComboR == 'G/Y', '2711-77134', bandNumber),
         bandNumber = ifelse(bandNumber == '2711-77295' & colorComboR == 'O', '2711-77295B', bandNumber),
         bandNumber = ifelse(bandNumber == '2711-77325' & colorComboR == 'PK', '2711-77325B', bandNumber),
         bandNumber = ifelse(bandNumber == '2711-77328' & colorComboR == 'BK/A', '2711-77328B', bandNumber),
         bandNumber = ifelse(bandNumber == '2711-77530' & colorComboL == 'A', '2711-77530B', bandNumber),
         bandNumber = ifelse(bandNumber == '2750-34973' & colorComboL == 'G', '2750-34974', bandNumber),
         bandNumber = ifelse(bandNumber == '2750-51201' & colorComboL == 'A/Y', '2750-51201B', bandNumber),
         bandNumber = ifelse(bandNumber == '2750-51210' & colorComboL == 'O/A', '2750-51210B', bandNumber),
         bandNumber = ifelse(bandNumber == '2780-14692' & colorComboL == 'GY', '2780-14693', bandNumber),
         bandNumber = ifelse(bandNumber == '962-47702' & colorComboL == 'A', '962-47702B', bandNumber),
         colorComboL = ifelse(bandNumber == '1212-36664', 'OM', colorComboL),
         colorComboR = ifelse(bandNumber == '1212-36664', 'X', colorComboR),
         colorComboL = ifelse(bandNumber == '2371-04365', 'OX', colorComboL),
         colorComboR = ifelse(bandNumber == '2371-04365', 'W', colorComboR),
         colorComboR = ifelse(bandNumber == '2531-91102', 'XW', colorComboR),
         colorComboL = ifelse(bandNumber == '2531-91104', 'XM', colorComboL),
         colorComboR = ifelse(bandNumber == '2531-91104', 'R', colorComboR),
         colorComboL = ifelse(bandNumber == '2531-91183', 'XY', colorComboL),
         colorComboR = ifelse(bandNumber == '2531-91183', 'B', colorComboR),
         colorComboL = ifelse(bandNumber == '2561-86637', 'RG', colorComboL),
         colorComboR = ifelse(bandNumber == '2561-86637', 'X', colorComboR),
         colorComboL = ifelse(bandNumber == '2561-86644', 'YX', colorComboL),
         colorComboR = ifelse(bandNumber == '2561-86644', 'K', colorComboR),
         colorComboR = ifelse(bandNumber == '2571-70452' & colorComboR == 'A/P', 'PX', colorComboR),
         colorComboL = ifelse(bandNumber == '2590-82090', 'X', colorComboL),
         colorComboR = ifelse(bandNumber == '2590-82090', '-', colorComboR),
         typeCapture = ifelse(bandNumber == '2590-82090' & visitID == 'MILLKARFL1_2015-06-05', 'R', typeCapture),
         colorComboL = ifelse(bandNumber == '2640-61826', 'XM', colorComboL),
         colorComboR = ifelse(bandNumber == '2640-61826', '-', colorComboR),
         colorComboL = ifelse(bandNumber == '2641-63316', 'M', colorComboL),
         colorComboR = ifelse(bandNumber == '2641-63316', 'BX', colorComboR),
         colorComboL = ifelse(bandNumber == '2651-51801', 'X', colorComboL),
         colorComboR = ifelse(bandNumber == '2651-51801', 'WW', colorComboR),
         typeCapture = ifelse(bandNumber == '2651-51801' & visitID == 'HUNTDAVMD2_2016-06-13', 'R', typeCapture),
         colorComboL = ifelse(bandNumber == '2661-33039', 'X', colorComboL),
         colorComboR = ifelse(bandNumber == '2661-33039', 'RY', colorComboR),
         typeCapture = ifelse(bandNumber == '2661-33039' & visitID == 'MALLTANPA1_2014-07-16', 'R', typeCapture),
         colorComboL = ifelse(bandNumber == '2661-33060', 'XG', colorComboL),
         colorComboR = ifelse(bandNumber == '2661-33060', 'R', colorComboR),
         typeCapture = ifelse(bandNumber == '2661-33060' & visitID == 'CUNKMARPA1_2014-07-19', 'R', typeCapture),
         colorComboL = ifelse(bandNumber == '2661-33100', 'Y', colorComboL),
         colorComboR = ifelse(bandNumber == '2661-33100', 'WX', colorComboR),
         typeCapture = ifelse(bandNumber == '2661-33100' & visitID == 'SAHNRAMPA1_2014-07-17', 'R', typeCapture),
         colorComboL = ifelse(bandNumber == '2690-87836', 'XM', colorComboL),
         colorComboR = ifelse(bandNumber == '2690-87836', 'O', colorComboR),
         colorComboL = ifelse(bandNumber == '2691-06139', 'YW', colorComboL),
         colorComboR = ifelse(bandNumber == '2691-06139', 'X', colorComboR),
         colorComboL = ifelse(bandNumber == '2701-39019', 'X', colorComboL),
         colorComboR = ifelse(bandNumber == '2701-39019', 'RW', colorComboR),
         typeCapture = ifelse(bandNumber == '2701-39019' & visitID == 'SAHNRAMPA1_2014-07-17', 'R', typeCapture),
         colorComboL = ifelse(bandNumber == '2720-99524', 'O', colorComboL),
         colorComboR = ifelse(bandNumber == '2720-99524', 'WX', colorComboR),
         colorComboL = ifelse(bandNumber == '2720-99652', 'B', colorComboL),
         colorComboR = ifelse(bandNumber == '2720-99652', 'XK', colorComboR)
  ) %>%
  distinct
  
  
birdTableBandFix <- birdTableSpFix %>%
  mutate_all(str_trim) %>%
  filter(birdID %in% problemBirds) %>%
  arrange(birdID) %>%
  distinct %>%
  filter(!(bandNumber == '2701-39019' & colorComboR == 'RW')) %>%
  mutate(bandNumber = ifelse(bandNumber == '1352-27683' & colorComboL == 'OR', '1352-27684', bandNumber),
         bandNumber = ifelse(bandNumber == '1991-18052' & colorComboL == 'XR', '1991-18052B', bandNumber),
         bandNumber = ifelse(bandNumber == '2331-09133' & colorComboL == 'X', '2331-09135', bandNumber),
         bandNumber = ifelse(bandNumber == '2180-92454' & colorComboL == 'W', '2180-92452', bandNumber),
         bandNumber = ifelse(bandNumber == '2430-14749' & colorComboL == 'BP', '2430-14749B', bandNumber),
         bandNumber = ifelse(bandNumber == '2541-40415' & colorComboL == 'X', '2541-40415B', bandNumber),
         bandNumber = ifelse(bandNumber == '2571-70429' & colorComboR == 'GB', '2571-70430', bandNumber),
         bandNumber = ifelse(bandNumber == '2571-70452' & colorComboL == 'YM', '2571-70460', bandNumber),
         bandNumber = ifelse(bandNumber == '2571-70474' & colorComboL == 'RY', '2571-70476', bandNumber),
         bandNumber = ifelse(bandNumber == '2640-61857' & colorComboL == 'WX', '2640-61862', bandNumber),
         bandNumber = ifelse(bandNumber == '2651-43372' & colorComboR == 'X', '2651-43371', bandNumber),
         bandNumber = ifelse(bandNumber == '2651-43447' & colorComboL == 'R', '2651-43497', bandNumber),
         bandNumber = ifelse(bandNumber == '2651-56680' & colorComboL == 'B', '2651-56681', bandNumber),
         bandNumber = ifelse(bandNumber == '2690-87711' & colorComboR == 'WO', '2690-87711B', bandNumber),
         bandNumber = ifelse(bandNumber == '2711-77124' & colorComboR == 'GY', '2711-77134', bandNumber),
         bandNumber = ifelse(bandNumber == '2711-77295' & colorComboR == 'O', '2711-77295B', bandNumber),
         bandNumber = ifelse(bandNumber == '2711-77325' & colorComboR == 'P', '2711-77325B', bandNumber),
         bandNumber = ifelse(bandNumber == '2711-77328' & colorComboR == 'KX', '2711-77328B', bandNumber),
         bandNumber = ifelse(bandNumber == '2711-77530' & colorComboL == 'X', '2711-77530B', bandNumber),
         bandNumber = ifelse(bandNumber == '2750-34973' & colorComboL == 'G', '2750-34974', bandNumber),
         bandNumber = ifelse(bandNumber == '2750-51201' & colorComboL == 'XY', '2750-51201B', bandNumber),
         bandNumber = ifelse(bandNumber == '2750-51210' & colorComboL == 'OX', '2750-51210B', bandNumber),
         bandNumber = ifelse(bandNumber == '2780-14692' & colorComboL == 'E', '2780-14693', bandNumber),
         bandNumber = ifelse(bandNumber == '962-47702' & colorComboL == 'X', '962-47702B', bandNumber),
         colorComboL = ifelse(bandNumber == '1212-36664', 'OM', colorComboL),
         colorComboR = ifelse(bandNumber == '1212-36664', 'X', colorComboR),
         colorComboL = ifelse(bandNumber == '2371-04365', 'OX', colorComboL),
         colorComboR = ifelse(bandNumber == '2371-04365', 'W', colorComboR),
         colorComboR = ifelse(bandNumber == '2531-91102', 'XW', colorComboR),
         colorComboL = ifelse(bandNumber == '2531-91104', 'XM', colorComboL),
         colorComboR = ifelse(bandNumber == '2531-91104', 'R', colorComboR),
         colorComboL = ifelse(bandNumber == '2531-91183', 'XY', colorComboL),
         colorComboR = ifelse(bandNumber == '2531-91183', 'B', colorComboR),
         colorComboL = ifelse(bandNumber == '2561-86637', 'RG', colorComboL),
         colorComboR = ifelse(bandNumber == '2561-86637', 'X', colorComboR), 
         colorComboL = ifelse(bandNumber == '2561-86644', 'YX', colorComboL),
         colorComboR = ifelse(bandNumber == '2561-86644', 'K', colorComboR),
         colorComboR = ifelse(bandNumber == '2571-70452' & colorComboR == 'XP', 'PX', colorComboR),
         colorComboL = ifelse(bandNumber == '2590-82090', 'X', colorComboL),
         colorComboR = ifelse(bandNumber == '2590-82090', '-', colorComboR),
         colorComboL = ifelse(bandNumber == '2640-61826', 'XM', colorComboL),
         colorComboR = ifelse(bandNumber == '2640-61826', '-', colorComboR),
         colorComboL = ifelse(bandNumber == '2641-63316', 'M', colorComboL),
         colorComboR = ifelse(bandNumber == '2641-63316', 'BX', colorComboR),
         colorComboL = ifelse(bandNumber == '2651-51801', 'X', colorComboL),
         colorComboR = ifelse(bandNumber == '2651-51801', 'WW', colorComboR),
         colorComboL = ifelse(bandNumber == '2661-33039', 'X', colorComboL),
         colorComboR = ifelse(bandNumber == '2661-33039', 'RY', colorComboR),
         colorComboL = ifelse(bandNumber == '2661-33060', 'XG', colorComboL),
         colorComboR = ifelse(bandNumber == '2661-33060', 'R', colorComboR),
         colorComboL = ifelse(bandNumber == '2661-33100', 'Y', colorComboL),
         colorComboR = ifelse(bandNumber == '2661-33100', 'WX', colorComboR),
         colorComboL = ifelse(bandNumber == '2690-87836', 'XM', colorComboL),
         colorComboR = ifelse(bandNumber == '2690-87836', 'O', colorComboR),
         colorComboL = ifelse(bandNumber == '2691-06139', 'YW', colorComboL),
         colorComboR = ifelse(bandNumber == '2691-06139', 'X', colorComboR),
         colorComboL = ifelse(bandNumber == '2720-99524', 'O', colorComboL),
         colorComboR = ifelse(bandNumber == '2720-99524', 'WX', colorComboR),
         colorComboL = ifelse(bandNumber == '2720-99652', 'B', colorComboL),
         colorComboR = ifelse(bandNumber == '2720-99652', 'XK', colorComboR)
         ) %>%
  filter(!(bandNumber == '1841-58960' & colorComboR == 'UNK')) %>%
  distinct %>%
  mutate(birdID = bandNumber)



birdTableBandCombine <- bind_rows(
  birdTableSpFix %>%
    mutate_all(str_trim) %>%
    filter(!birdID %in% problemBirds),
  birdTableBandFix
) %>%
  arrange(species, bandNumber) %>%
  distinct %>%
  filter(birdID != '-')


captureTableBandCombine <- captureTable %>%
  mutate_all(str_trim) %>%
  filter(!birdID %in% problemBirds) %>%
  bind_rows(captureTableBandFix) %>%
  mutate(visitID2 = visitID) %>%
  separate(visitID2, into = c('site', 'date'), sep = '\\_') %>%
  mutate(date = as.Date(date)) %>%
  arrange(date, site) %>%
  filter(!is.na(birdID)) %>%
  filter(visitID != 'VERSBETMD2_2016-06-11') %>%
  filter(birdID %in% birdTableBandCombine$birdID) %>%
  select(-c(site, date)) %>%
  filter(birdID != '-') %>%
  mutate(birdID = bandNumber)

captureTableForBeth <- captureTableBandCombine %>%
  mutate(typeCapture = typeCapture %>%
           str_replace_all('Band', 'B') %>%
           str_replace_all('Recap', 'R')) %>%
  mutate(age = ifelse(sex == 'AHY', 'AHY', age),
         age = ifelse(sex == 'SY', 'SY', age),
         age = ifelse(sex == 'ASY', 'ASY', age),
         sex = ifelse(sex %in% c('AHY', 'SY', 'ASY'), NA, sex),
         sex = ifelse(sex == 'UNK', 'U', sex)) %>%
  select(-c(colorComboL,colorComboR)) %>%
  left_join(birdTableBandCombine %>%
              select(birdID, colorComboL, colorComboR),
            by = 'birdID') %>%
  select(birdID:bandNumber, colorComboL, colorComboR, age:dispositionCode) %>%
  mutate(colorComboL = ifelse(colorComboL == 'BOB', 'BO', colorComboL),
         colorComboR = ifelse(colorComboL == 'XYG', 'G', colorComboR),
         colorComboL = ifelse(colorComboL == 'XYG', 'XY', colorComboL),
         colorComboL = ifelse(colorComboL == 'Q', 'W', colorComboL),
         colorComboL = ifelse(colorComboL == 'TB', 'B', colorComboL),
         colorComboR = ifelse(colorComboR == 'XMG', 'XG', colorComboR))
  
birdTableForBeth <- birdTableBandCombine %>%
  mutate(colorComboL = ifelse(colorComboL == 'BOB', 'BO', colorComboL),
         colorComboR = ifelse(colorComboL == 'XYG', 'G', colorComboR),
         colorComboL = ifelse(colorComboL == 'XYG', 'XY', colorComboL),
         colorComboL = ifelse(colorComboL == 'Q', 'W', colorComboL),
         colorComboL = ifelse(colorComboL == 'TB', 'B', colorComboL),
         colorComboR = ifelse(colorComboR == 'XMG', 'XG', colorComboR))


write_csv(captureTableForBeth, 'nnData/captureTable.csv')

write_csv(birdTableForBeth, 'nnData/birdTablecsv')

# ============================

read_csv('nnData/attachmentTable.csv')
  
read_csv('nnData/biosampleTable.csv')

read_csv('nnData/contactInfoTable.csv')

measures <- read_csv('nnData/measurementTable.csv') %>%
  mutate_all(str_trim) %>%
  filter(birdID != '-') %>%
  mutate(fat = as.numeric(fat),
         mass = as.numeric(mass),
         wing = as.numeric(tl),
         tarsus = as.numeric(tarsus),
         nares = as.numeric(nares),
         depth = as.numeric(depth),
         width = as.numeric(width), 
         culmen= as.numeric(culmen),
         moltLimit = ifelse(moltLimit == 'N?', 'N', moltLimit),
         moltLimit = ifelse(moltLimit == 'P1,P2', 'Y', moltLimit),
         rectrices = ifelse(rectrices == 'TAP?', 'TAP', rectrices))

write_csv(measures, 'nnData/measurementTable.csv')

unique(pcTab$distanceClass)

pcTab <- read_csv('nnData/pointCountTable.csv') %>%
  mutate(distanceClass = ifelse(distanceClass %in% c('10-40','20-40','0-20', '10-30',
                                                     '20-20', '10-50', '10/20/2016',
                                                     '> 10', '10/30/2016', '20-50'), NA, distanceClass))

write_csv(pcTab, 'nnData/pointCountTable.csv')

rsEffortTab <- read_csv('nnData/resightEffortTable.csv') %>%
  mutate(pathDistance = as.numeric(pathDistance),
         rsStart = ifelse(rsStart == 'noData', NA, rsStart),
         rsStart = ifelse(rsStart == '12.5', NA, rsStart))

write_csv(rsEffortTab, 'nnData/resightEffortTable.csv')

rsPartFix <- read_csv('nnData/resightPartTable.csv') %>%
  mutate(birdID = str_trim(birdID),
         resightTypePart = ifelse(resightTypePart == 'p', 'P', resightTypePart)) %>%
  filter(birdID %in% birdTableForBeth$birdID) %>%
  select(-yearResight)
  
write_csv(rsPartFix, 'nnData/resightPartTable.csv')

rsTechFix <- read.csv('nnData/resightTechTable.csv') %>%
  tbl_df() %>%
  mutate(birdID = str_trim(birdID)) %>%
  filter(birdID %in% birdTableForBeth$birdID)

rsTechFix %>% write_csv('nnData/resightTechTable.csv')

siteID <- read_csv('nnData/siteIdentifierTable.csv') %>%
  mutate(region = ifelse(projectID == 'grcaMigration' & str_detect(siteID, 'DC'), 'DC', region),
         region = ifelse(projectID == 'grcaMigration' & str_detect(siteID, 'MD'), 'DC', region),
         region = ifelse(projectID == 'grcaMigration' & str_detect(siteID, 'CO'), 'CO', region),
         region = ifelse(projectID == 'grcaMigration' & str_detect(siteID, 'GA'), 'GA', region))

siteID %>% write_csv('nnData/siteIdentifierTable.csv')

siteLoc <- read_csv('nnData/siteLocationTable.csv')

siteLoc %>%
  select(sitePlotID:locationMethod) %>%
  write_csv('nnData/siteLocationTable.csv')

vTable <- read_csv('nnData/visitTable.csv') %>%
  mutate(encounteredBirds = ifelse(encounteredBirds %in% c('Yes', 'yes', '1'),'Y', encounteredBirds),
         encounteredBirds = ifelse(encounteredBirds %in% c('No', 'no'),'N', encounteredBirds)) %>%
  mutate(netHours = as.numeric(netHours) %>%
           round(2))

write_csv(vTable, 'nnData/visitTable.csv')


n1 <- read_csv('nnData/nestObservTable.csv')

n1 %>%
  mutate(observerNest = ifelse(observerNest == 'p', 'P', observerNest)) %>%
  write_csv('nnData/nestObservTable.csv')

n2 <- read_csv('nnData/nestSummaryTable.csv')

n2 %>%
  mutate(fateNest = ifelse(fateNest == 'Successful', 'S', fateNest),
         fateNest = ifelse(fateNest == 'Failed', 'F-Other', fateNest),
         fateNest = ifelse(fateNest == 'Active', NA, fateNest),
         dateHatch = as.Date(dateHatch),
         dateFledgeFail = as.Date(dateFledgeFail)
         ) %>%
  write_csv('nnData/nestSummaryTable.csv')





