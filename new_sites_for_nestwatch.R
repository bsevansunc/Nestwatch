require('plyr')
source('source_functions.R')

filePath = 'site_selection_nn/nn_dc_potential_sites/'

fileNames = list.files(filePath)
filePaths = paste(filePath, fileNames, sep ='')

fileList = list(length = length(fileNameList))
  for(i in 1:length(fileNames)){
    fileRaw = read.delim(filePaths[i], header = F, sep =':')
    fileTransposed = t(fileRaw[,2:ncol(fileRaw)])
    colnames(fileTransposed) <- fileRaw[,1]
    file.mat = matrix(fileTransposed, ncol = length(fileTransposed))
    file.df = data.frame(file.mat)
      names(file.df) = fileRaw[,1]
    file.df$address = do.call('paste', data.frame(file.df[,4:6]))
    file.df = file.df[,-c(5:6)]
    fileList[[i]] = file.df
  }

# Write the file:

participantFrame = rbind.fill(fileList)
outPath = paste('site_selection_nn/nn_dc_potential_sites/potentialParticipants.csv')
write.csv(rbind.fill(fileList),outPath, row.names = F)

# Get lat longs and update the file:

writeAddressToLatLong(outPath, outPath)

# Read it back again:

participantFrame = read.csv(outPath)

# Get 500 m moving window raster:

r = raster('LandCover/r500mw')

list.files()

NNCurrentLL = read.csv('exampleLL.csv')[,c('Long','Lat')]

NNPotential = getNNValuesDF(outPath, r)

NNPotentialLL = NNPotential[,c('Long','Lat')]

require(geosphere)

NNPotential$minDistNew = 9999
NNPotential$minDistExisting = 9999

for(i in 1:nrow(NNPotential)){
  targetPoint = NNPotentialLL[i,]
  otherPoints = NNPotentialLL[-i, ]
  NNPotential$minDistNew[i] = min(distm(targetPoint, otherPoints))
  NNPotential$minDistExisting[i] = min(distm(targetPoint, NNCurrentLL))
}


minDist = min(distm(NN1[,c(8,7)]))

require('dplyr')
filter(NNPotential,propImpervious > 30 & minDistExisting >300)
