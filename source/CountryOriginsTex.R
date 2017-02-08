origins = read.csv("inputdata/countryOrigins.csv", sep = excelSep, header = TRUE, stringsAsFactors =FALSE)
countryData = as.data.frame(table(origins))
countryData = countryData[!countryData$origins == "?",]
nrow(countryData)


sink("outputTex/CountryOrigins.tex", append = TRUE)
cat(paste("\\newcommand\\countCountries{",nrow(countryData), "}", sep = ""))
for (i in 1:nrow(countryData)) {
  cat(paste("\\newcommand\\origins", countryData$origins[i], "{", countryData$Freq[i], "}", sep = ""))
}
for (i in 1:nrow(countryData)) {
  cat(paste("\\newcommand\\origins", countryData$origins[i], "percent{", 
            round(countryData$Freq[i]/sum(countryData$Fre)*100,0), "}", sep = ""))
}
closeAllConnections()