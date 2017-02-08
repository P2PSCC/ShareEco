data = tableData
oldData = data[
  ifelse(is.na(data$Year_Launch), FALSE, 
         data$Year_Launch < 2014),]
newData = data[
  ifelse(is.na(data$Year_Launch), FALSE, 
         data$Year_Launch >= 2014),]
outputOld = generateOutput(oldData, "old")
outputNew = generateOutput(newData, "new")
outputOld[,1] = round(outputOld[,1] * 100,2)
outputNew[,1] = round(outputNew[,1] * 100,2)
output = as.data.frame(cbind(outputNew[,1], outputOld[,1]))
colnames(output) = c("new", "old")
write.table(output, "outputCsv/outputOldVsNew.csv", sep = excelSepOutput)

# generate tex
sink("outputTex/OldPlatforms.tex", append = TRUE)
for (i in 1:nrow(outputOld)) {
  cat(paste("\\newcommand\\", gsub("2", "", gsub("_", "", rownames(outputOld)[i])), "{", 
            round(outputOld[i,1], 0), "}", sep = ""))
}
closeAllConnections()
sink("outputTex/NewPlatforms.tex", append = TRUE)
for (i in 1:nrow(outputNew)) {
  cat(paste("\\newcommand\\", gsub("2", "", gsub("_", "", rownames(outputNew)[i])), "{", 
            round(outputNew[i,1], 0), "}", sep = ""))
}
closeAllConnections()