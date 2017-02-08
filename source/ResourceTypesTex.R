tableDataCopy = tableData

# popular resources
resources = tableDataCopy$Resource_Type
accommodations = c("accommodations", "accommodations / flatmate")
cars = c("cars / ride", "cars / rent", "cars / taxi")
food = c("food / dining", "food / self-grown", "food")
misc = c("miscellaneous", "misc")
resources[resources %in% cars] = "car"
resources[resources %in% accommodations] = "accommodation"
resources[resources %in% food] = "food"
resources[resources %in% misc] = "misc"
freqs = as.data.frame(table(resources))
freqs$resources = gsub(" ", "", freqs$resources)
freqs$resources = gsub("-", "", freqs$resources)
freqs$resources = gsub("3", "three", freqs$resources)

# generate tex
sink("outputTex/Resources.tex", append = TRUE)
for (i in 1:nrow(freqs)) {
  cat(paste("\\newcommand\\res", freqs$resources[i], "{", 
            freqs$Freq[i], "}", sep = ""))
}
lessPopular = c("aircrafts", "bicycles", "books", "threedprinters", "cameras", "dogs", "retailspaces",
                "wifirouters", "sportfacilities", "pets", "media", "laundrymachines", 
                "agriculturalmachinery")
mixed = c("misc", "transporters", "tools", "spaces", "venues", "adventureandoutdoorrelatedresources",
          "sportsgear", "motorizedvehicles", "babyrelated")
cat(paste("\\newcommand\\reslesspopular{", 
          sum(freqs$Freq[freqs$resources %in% lessPopular]), "}", sep = ""))
cat(paste("\\newcommand\\resmixed{", 
          sum(freqs$Freq[freqs$resources %in% mixed]), "}", sep = ""))
for (i in 1:nrow(freqs)) {
  cat(paste("\\newcommand\\res", freqs$resources[i], "percent{", 
            round(freqs$Freq[i]/nrow(tableDataCopy)*100), "}", sep = ""))
}
cat(paste("\\newcommand\\reslesspopularpercent{", 
          round(sum(freqs$Freq[freqs$resources %in% lessPopular])/nrow(tableDataCopy)*100,0), "}", sep = ""))
cat(paste("\\newcommand\\resmixedpercent{", 
          round(sum(freqs$Freq[freqs$resources %in% mixed])/nrow(tableDataCopy)*100,0), "}", sep = ""))
cat(paste("\\newcommand\\numberofresources{", 
          nrow(freqs), "}", sep = ""))
closeAllConnections()
