noSites = c('NE1','NE2','NE3','NE4','NE5','NE6','NE7','NW1','NW2','NW3','NW4')
impNN = impFiltered[!impFiltered$site %in% noSites,]

ggplot(impNN,aes(x = imp)) + geom_histogram(binwidth = 5, fill = 'gray50') +
  ggtitle('Site distribution in relation to \n the impervious surface gradient in D.C.') +
  ylab("Number of sites") + 
  xlab('Impervious surface (%) within 100 m of site center') +
  theme_manuscript() +
  theme(plot.title=element_text(family="Times", face="bold", size=24)) +
  theme(axis.title=element_text(family="Times", face="bold", size=18)) +
  scale_x_continuous(limits = c(0,100))
