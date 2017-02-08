excelSep = ";"
excelSepOutput = ","
unlink("outputTex", force = TRUE, recursive = TRUE)
dir.create("outputTex", showWarnings = FALSE)
unlink("outputCsv", force = TRUE, recursive = TRUE)
dir.create("outputCsv", showWarnings = FALSE)

source("source/HelperFunctions.R")
tableData = readDirectoriesDataXLSX("inputdata/Relevant_Websites.xlsx", 1)
#remove NA rows
tableData = tableData[!is.na(tableData$Resource_Type),]

# websites that actually are SCC
tableData = tableData[
  ifelse(is.na(!tableData$removal.reason == "no scc website"), TRUE, 
         !tableData$removal.reason == "no scc website"),]
source("source/DeadVsAliveTex.R")
source("source/OldVsNewTex.R")
source("source/ViewsTex.R")
source("source/LaunchYearTex.R")
source("source/CountryOriginsTex.R")
source("source/ResourceTypesTex.R")
source("source/SamplesTex.R")
source("source/OverallTex.R")

