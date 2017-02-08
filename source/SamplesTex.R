data = read.csv("inputdata/Iterations Sharing Monitor.csv", sep = excelSep)

colnames(data) = gsub("\\.", "", colnames(data))
sink("outputTex/Samples.tex", append = TRUE)
cat(paste("\\newcommand\\analyzedPlatforms{",sum(data$Numberofnewplatforms), "}", sep = ""))
cat(paste("\\newcommand\\deadPlatforms{",sum(data$Numberofdeadplatforms), "}", sep = ""))

for (i in 1:nrow(data)) {
  for (j in 2:ncol(data)) {
    cat(paste("\\newcommand\\", data$Iteration[i], colnames(data)[j],
              "{",data[i,j], "}", sep = ""))
  }
}
closeAllConnections()