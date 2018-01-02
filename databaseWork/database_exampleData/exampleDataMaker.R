df <- data.frame(site = c(rep('foo',9),rep('helloWorld', 11)),
                 species = c(rep('noca', 5), 'grca', rep('noca',3),
                             rep('amro',2), rep('grca',5),
                             rep('amro',4)),
                 bandNumber = c(rep(124236413,5), 124236414, rep(124236415,3),
                                rep(235651218,2), 124236416, rep(124236417,4),
                                rep(23561219, 4)),
                 observationDate = c('5/29/2009', '6/12/2009','10/14/2009', '5/12/2010',
                                             '7/13/2010', '5/29/2009','5/29/2009', '11/12/2009',
                                             '3/15/2010', '5/30/2009','6/18/2009', '5/30/2009',
                                             '5/30/2009', '6/18/2009','8/7/2009', '7/17/2010',
                                             '7/17/2010', '7/28/2010','8/23/2010', '9/12/2010'),
                 observationType = c('c','rp', 'rp','rc','d',
                                     'c', 
                                     'c','rp','rp',
                                     'c','rp',
                                     'c',
                                     'c','rp','rp','rt',
                                     'c','rp','rp','rp')
                 )

df

df$observationDate <- as.Date(df$observationDate, format = '%m/%d/%Y')

df$site <- tolower(df$site)

write.csv(df, 'C:/Users/Brian/Desktop/gits/Nestwatch/exampleEncounterData.csv', row.names = FALSE)

read.csv('C:/Users/Brian/Desktop/gits/Nestwatch/exampleEncounterData.csv')

exampleMeasurementData <- data.frame(site = c(rep('foo', 4), rep('helloworld', 4)),
                                     species = c('noca','noca','grca','noca','amro','grca','grca','amro'),
                                     bandNumber = c(124236413, 124236413, 124236414,124236415,
                                                    235651218, 124236416,124236417, 23561219),
                                     observationDate = c('2009/05/29','2010/05/12',
                                                         '2009/05/29','2009/05/29',
                                                         '2009/05/30','2009/05/30', '2009/05/30','2010/07/17'),
                                     age = c('ahy','ahy','hy', 'ahy','ahy','hy','ahy','ahy'),
                                     sex = c('f','f','u', 'm','f', 'u','m','f'),
                                     breedingCondition = c('bp','bp+', 'j', 'cp','bp','j','cp','bp'),
                                     mass = c(43.4,45.9, 36.3, 42.6, 77.3, 37.8, 35.7, 77.7),
                                     wing = c(90.6, 91.7, 87.9, 92.8, 124.7, 88.7, 89.9, 135.7),
                                     tail = c(94.7, 96.2, 90.7, 99.7, 33.0, 91.2, 93.5, 33.5),
                                     tarsus = c(27.9, 27.8, 27.4, 28.3, 31.0, 26.3, 28.1, 29.6))


write.csv(exampleMeasurementData, 'C:/Users/Brian/Desktop/gits/Nestwatch/exampleMeasurementData.csv', row.names = FALSE)

read.csv('C:/Users/Brian/Desktop/gits/Nestwatch/exampleMeasurementData.csv')


exampleSiteData <- data.frame(region = c('springfield', 'springfield'),
                              site = c('foo', 'helloworld'),
                              long = c(-72.605, -72.6325),
                              lat = c(42.1247, 42.2499))

write.csv(exampleSiteData, 'C:/Users/Brian/Desktop/gits/Nestwatch/exampleSiteData.csv', row.names = FALSE)

read.csv('C:/Users/Brian/Desktop/gits/Nestwatch/exampleSiteData.csv')
                              long = )

                 
                 