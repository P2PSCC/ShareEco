deadData = tableData[
  ifelse(is.na(tableData$removed), FALSE, 
         tableData$removed == "x"),]
aliveData = tableData[
  ifelse(is.na(tableData$removed), TRUE, 
         !tableData$removed == "x"),]
outputDead = generateOutput(deadData, "dead")
outputDead[,1] = round(outputDead[,1]*100,2)
outputAlive = generateOutput(aliveData, "alive")
outputAlive[,1] = round(outputAlive[,1]*100,2)
output = as.data.frame(cbind(outputDead[,1], outputAlive[,1]))
colnames(output) = c("dead", "alive")
write.table(output, "outputCsv/outputDeadVsAlive.csv", sep = excelSepOutput)

# generate tex
sink("outputTex/DeadPlatforms.tex", append = TRUE)
for (i in 1:nrow(outputDead)) {
  cat(paste("\\newcommand\\", gsub("2", "", gsub("_", "", rownames(outputDead)[i])), "{", 
            round(outputDead[i,1], 0), "}", sep = ""))
}
closeAllConnections()
sink("outputTex/AlivePlatforms.tex", append = TRUE)
for (i in 1:nrow(outputAlive)) {
  cat(paste("\\newcommand\\", gsub("2", "", gsub("_", "", rownames(outputAlive)[i])), "{", 
            round(outputAlive[i,1], 0), "}", sep = ""))
}
closeAllConnections()

# analyze reasons for dead websites
acquisition = sum(grepl("acquired", deadData$removal.reason))
acquisition = acquisition + sum(grepl("same", deadData$removal.reason))
changedConcept = sum(grepl("changed", deadData$removal.reason))
platformDown = sum(grepl("ceased", deadData$removal.reason))
platformDown = platformDown + sum(grepl("website", deadData$removal.reason))
other = nrow(deadData) - (acquisition + changedConcept + platformDown)

sink("outputTex/RemovalReasons.tex", append = TRUE)
cat(paste("\\newcommand\\deadAcquisition{",acquisition, "}", sep = ""))
cat(paste("\\newcommand\\deadChangedConcept{",changedConcept, "}", sep = ""))
cat(paste("\\newcommand\\deadPlatformDown{",platformDown, "}", sep = ""))
cat(paste("\\newcommand\\deadOther{",other, "}", sep = ""))
cat(paste("\\newcommand\\deadAcquisitionPercentage{",round((acquisition/nrow(deadData)*100),0), "}", sep = ""))
cat(paste("\\newcommand\\deadChangedConceptPercentage{",round(changedConcept/nrow(deadData)*100,0), "}", sep = ""))
cat(paste("\\newcommand\\deadPlatformDownPercentage{",round(platformDown/nrow(deadData)*100,0), "}", sep = ""))
cat(paste("\\newcommand\\deadOtherPercentage{",round(other/nrow(deadData)*100,0), "}", sep = ""))
cat(paste("\\newcommand\\deadTotal{",nrow(deadData), "}", sep = ""))
closeAllConnections()

# analyze launch years of dead websites
sink("outputTex/TerminationLaunchYear.tex", append = TRUE)
# from 1999 to 2015
yearData = table(deadData$Year_Launch)
cat(paste("\\newcommand\\deadNineNine{",yearData[names(yearData) == "1999"], "}", sep = ""))
cat(paste("\\newcommand\\deadZero{",yearData[names(yearData) == "2000"], "}", sep = ""))
cat(paste("\\newcommand\\deadOne{",yearData[names(yearData) == "2001"], "}", sep = ""))
cat(paste("\\newcommand\\deadTwo{",yearData[names(yearData) == "2002"], "}", sep = ""))
cat(paste("\\newcommand\\deadThree{",yearData[names(yearData) == "2003"], "}", sep = ""))
cat(paste("\\newcommand\\deadFour{",yearData[names(yearData) == "2004"], "}", sep = ""))
cat(paste("\\newcommand\\deadFive{",yearData[names(yearData) == "2005"], "}", sep = ""))
cat(paste("\\newcommand\\deadSix{",yearData[names(yearData) == "2006"], "}", sep = ""))
cat(paste("\\newcommand\\deadSeven{",yearData[names(yearData) == "2007"], "}", sep = ""))
cat(paste("\\newcommand\\deadEight{",yearData[names(yearData) == "2008"], "}", sep = ""))
cat(paste("\\newcommand\\deadNine{",yearData[names(yearData) == "2009"], "}", sep = ""))
cat(paste("\\newcommand\\deadTen{",yearData[names(yearData) == "2010"], "}", sep = ""))
cat(paste("\\newcommand\\deadEleven{",yearData[names(yearData) == "2011"], "}", sep = ""))
cat(paste("\\newcommand\\deadTwelf{",yearData[names(yearData) == "2012"], "}", sep = ""))
cat(paste("\\newcommand\\deadThirteen{",yearData[names(yearData) == "2013"], "}", sep = ""))
cat(paste("\\newcommand\\deadFourteen{",yearData[names(yearData) == "2014"], "}", sep = ""))
cat(paste("\\newcommand\\deadFifteen{",yearData[names(yearData) == "2015"], "}", sep = ""))
closeAllConnections()

# analyze dead resource types
types = table(deadData$Resource_Type)/table(tableData$Resource_Type[tableData$Resource_Type %in% deadData$Resource_Type])
# built compounds for most popular research types
mostPopular = numeric(14)
names(mostPopular) = c("Accommodation", "Cars", "Parking spaces", "Boats", "Food", "Work spaces", 
                       "Clothing", "Land", "Storage spaces", "Camping vehicles", "Miscellaneous",
                       "CarsRide", "CarsRent", "CarsTaxi")
mostPopular[1] = (types[names(types) == "accommodations"] * sum(tableData$Resource_Type == "accommodations")
                  + types[names(types) == "accommodations / flatmate"] * sum(tableData$Resource_Type == "accommodations / flatmate")) / 
  (sum(tableData$Resource_Type == "accommodations")+sum(tableData$Resource_Type == "accommodations / flatmate"))
mostPopular[2] = (types[names(types) == "cars / rent"] * sum(tableData$Resource_Type == "cars / rent")
                  + types[names(types) == "cars / ride"] * sum(tableData$Resource_Type == "cars / ride") +
                    + types[names(types) == "cars / taxi"] * sum(tableData$Resource_Type == "cars / taxi")) / 
  (sum(tableData$Resource_Type == "cars / rent")+sum(tableData$Resource_Type == "cars / ride")+sum(tableData$Resource_Type == "cars / taxi"))
mostPopular[3] = types[names(types) == "parking spaces"]
mostPopular[4] = types[names(types) == "boats"]
mostPopular[5] = (types[names(types) == "food / dining"] * sum(tableData$Resource_Type == "food / dining")
                  + types[names(types) == "food / self-grown"] * sum(tableData$Resource_Type == "food / self-grown")) / 
  (sum(tableData$Resource_Type == "food / dining")+sum(tableData$Resource_Type == "food / self-grown"))
mostPopular[6] = types[names(types) == "work spaces"]
mostPopular[7] = types[names(types) == "clothing"]
mostPopular[8] = types[names(types) == "land"]
mostPopular[9] = types[names(types) == "storage spaces"]
mostPopular[10] = types[names(types) == "camping vehicles"]
mostPopular[11] = types[names(types) == "miscellaneous"]
mostPopular[12] = types[names(types) == "cars / ride"]
mostPopular[13] = types[names(types) == "cars / rent"]
mostPopular[14] = types[names(types) == "cars / taxi"]
mostPopular = round(mostPopular * 100, 0)

sink("outputTex/TerminationResourceTypes.tex", append = TRUE)
for (i in 1:length(mostPopular)) {
  cat(paste("\\newcommand\\dead", sub(" ", "", names(mostPopular)[i]),"{",mostPopular[i], "}", sep = ""))
}
closeAllConnections()
