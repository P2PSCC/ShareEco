outputAll = generateOutput(tableData, "all")
outputAll[,1] = round(outputAll[,1] * 100, 2)
write.table(outputAll, "outputCsv/allSummary.csv", sep = excelSepOutput)

# generate tex
sink("outputTex/AllPlatforms.tex", append = TRUE)
for (i in 1:nrow(outputDead)) {
  cat(paste("\\newcommand\\", gsub("2", "", gsub("_", "", rownames(outputDead)[i])), "{", 
            round(outputDead[i,1], 0), "}", sep = ""))
}