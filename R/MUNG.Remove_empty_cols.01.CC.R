## Remove empty columns.  Useful for when you've subsetted for example a species abundance matrix and you don't want columns full of 0s to be analysed
## This code will ignore/skip any columns that are non-numeric

##Code provided by Chris Cesar 19/05/2016
##=========================================

require(plyr)
mydata <- mydata[, -which(numcolwise(sum)(mydata) == 0)]
detach("package:plyr", unload=TRUE)
