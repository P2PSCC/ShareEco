data = as.data.frame(cbind(tableData$Resource_Type, tableData$Year_Launch))
colnames(data) = c("Resource_Type", "Year_Launch")

###############################################
# platforms per year and resource type
data = data[!data$Year_Launch == "?",]
data = data[!is.na(data$Year_Launch),]
cars = c("cars / ride", "cars / rent", "cars / taxi")
accommodations = c("accommodations", "accommodations / flatmate")
other = data$Resource_Type[!data$Resource_Type %in% c(cars, accommodations)]
other = unique(other)
yearSplit = data.frame(cbind(year = (1999:2015), acco = numeric(length((1999:2015))), 
                             car = numeric(length((1999:2015))), other = numeric(length(1999:2015))))
for (i in 1:nrow(yearSplit)) {
  yearSplit[i,2] = nrow(data[data$Year_Launch == yearSplit[i,1] & 
                               data$Resource_Type %in% accommodations,])
  yearSplit[i,3] = nrow(data[data$Year_Launch == yearSplit[i,1] & 
                               data$Resource_Type %in% cars,])
  yearSplit[i,4] = nrow(data[data$Year_Launch == yearSplit[i,1] & 
                               data$Resource_Type %in% other,])
}
yearSplit = cbind(yearSplit, sum = apply(yearSplit[,2:4], 1, sum))
yearSplit = cbind(yearSplit, cumsum = cumsum(yearSplit$sum))

# generate tex
sink("outputTex/LaunchYearResourceSplit.tex", append = TRUE)
for (i in 2:ncol(yearSplit)) {
  cat(paste("\\newcommand\\nineNine", colnames(yearSplit)[i], "{",yearSplit[yearSplit$year == "1999",i], "}", sep = ""))
  cat(paste("\\newcommand\\zero", colnames(yearSplit)[i], "{",yearSplit[yearSplit$year == "2000",i], "}", sep = ""))
  cat(paste("\\newcommand\\one", colnames(yearSplit)[i], "{",yearSplit[yearSplit$year == "2001",i], "}", sep = ""))
  cat(paste("\\newcommand\\two", colnames(yearSplit)[i], "{",yearSplit[yearSplit$year == "2002",i], "}", sep = ""))
  cat(paste("\\newcommand\\three", colnames(yearSplit)[i], "{",yearSplit[yearSplit$year == "2003",i], "}", sep = ""))
  cat(paste("\\newcommand\\four", colnames(yearSplit)[i], "{",yearSplit[yearSplit$year == "2004",i], "}", sep = ""))
  cat(paste("\\newcommand\\five", colnames(yearSplit)[i], "{",yearSplit[yearSplit$year == "2005",i], "}", sep = ""))
  cat(paste("\\newcommand\\six", colnames(yearSplit)[i], "{",yearSplit[yearSplit$year == "2006",i], "}", sep = ""))
  cat(paste("\\newcommand\\seven", colnames(yearSplit)[i], "{",yearSplit[yearSplit$year == "2007",i], "}", sep = ""))
  cat(paste("\\newcommand\\eight", colnames(yearSplit)[i], "{",yearSplit[yearSplit$year == "2008",i], "}", sep = ""))
  cat(paste("\\newcommand\\nine", colnames(yearSplit)[i], "{",yearSplit[yearSplit$year == "2009",i], "}", sep = ""))
  cat(paste("\\newcommand\\ten", colnames(yearSplit)[i], "{",yearSplit[yearSplit$year == "2010",i], "}", sep = ""))
  cat(paste("\\newcommand\\eleven", colnames(yearSplit)[i], "{",yearSplit[yearSplit$year == "2011",i], "}", sep = ""))
  cat(paste("\\newcommand\\twelf", colnames(yearSplit)[i], "{",yearSplit[yearSplit$year == "2012",i], "}", sep = ""))
  cat(paste("\\newcommand\\thirteen", colnames(yearSplit)[i], "{",yearSplit[yearSplit$year == "2013",i], "}", sep = ""))
  cat(paste("\\newcommand\\fourteen", colnames(yearSplit)[i], "{",yearSplit[yearSplit$year == "2014",i], "}", sep = ""))
  cat(paste("\\newcommand\\fifteen", colnames(yearSplit)[i], "{",yearSplit[yearSplit$year == "2015",i], "}", sep = ""))
}
closeAllConnections()