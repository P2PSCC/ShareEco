
###############################################
# Analyze page views
pageViews = read.csv("inputData/pageViewsPerPlatform.csv", sep = excelSep, stringsAsFactors = FALSE)
if (excelSep == ";") pageViews$TotalViews = gsub(",", "\\.", pageViews$TotalViews)

tabledataCopy = tableData

# normalize URL data
tabledataCopy$stripped_url = sub("https://", "", tabledataCopy$stripped_url)
tabledataCopy$stripped_url = sub("http://", "", tabledataCopy$stripped_url)
tabledataCopy$stripped_url = sub("www.", "", tabledataCopy$stripped_url)
for (i in 1:nrow(tabledataCopy)) {
  tabledataCopy$stripped_url[i] = strsplit(tabledataCopy$stripped_url[i], "[.]")[[1]][1]
}
pageViews$URL = sub("https://", "", pageViews$URL)
pageViews$URL = sub("http://", "", pageViews$URL)
pageViews$URL = sub("www.", "", pageViews$URL)
for (i in 1:nrow(pageViews)) {
  pageViews$URL[i] = strsplit(pageViews$URL[i], "[.]")[[1]][1]
}
# now a match in 96% of the cases, this shall suffice
data = as.data.frame(cbind(URL = pageViews$URL[pageViews$URL %in% tabledataCopy$stripped_url], 
                           Views = pageViews$TotalViews[pageViews$URL %in% tabledataCopy$stripped_url],
                           Resource = character(sum(pageViews$URL %in% tabledataCopy$stripped_url))), 
                     stringsAsFactors = FALSE)
data$Resource = as.character(data$Resource)
data$Views = as.numeric(data$Views)
for (i in 1:nrow(data)) {
  if (pageViews$URL[i] %in% tabledataCopy$stripped_url) {
    data$Resource[i] = tabledataCopy$Resource_Type[which(tabledataCopy$stripped_url == data$URL[i])]
  }
  else data$Resource[i] = 0
}

accommodations = c("accommodations", "accommodations / flatmate")
cars = c("cars / ride", "cars / rent", "cars / taxi")
food = c("food / dining", "food / self-grown", "food")
# group
data$Resource[data$Resource %in% (accommodations)] = "accommodation"
data$Resource[data$Resource %in% (cars)] = "cars"
data$Resource[data$Resource %in% (food)] = "food"
viewsPerResource = as.data.frame(cbind(Resource = unique(data$Resource), 
                                       Views = numeric(length(unique(data$Resource)))))
viewsPerResource$Views = as.numeric(viewsPerResource$Views)
for (i in 1:length(unique(data$Resource))) {
  viewsPerResource[i,2] = sum(na.omit(data$Views[data$Resource == unique(data$Resource)[i]]))
}

##########
# read all previous csv results
allFiles = list.files("inputdata/awisresultsold")
aggregated = as.data.frame(cbind(URL = character(length(allFiles)), 
                                 Views = numeric(length(allFiles))), stringsAsFactors = FALSE)
aggregated$Views = as.numeric(aggregated$Views)
for (i in 1:length(allFiles)) {
  platform = read.csv(paste("inputdata/awisresultsold", allFiles[i], sep = "/"), sep = excelSep, 
                      stringsAsFactors = FALSE)
  if (excelSep == ";") platform$PAGEVIEWS = gsub(",", "\\.", platform$PAGEVIEWS)
  aggregated$URL[i] = platform[1,1]
  aggregated$Views[i] = platform[1,2]
}
# normalize URL data
aggregated$URL = sub("https://", "", aggregated$URL)
aggregated$URL = sub("http://", "", aggregated$URL)
aggregated$URL = sub("www.", "", aggregated$URL)
for (i in 1:nrow(aggregated)) {
  aggregated$URL[i] = strsplit(aggregated$URL[i], "[.]")[[1]][1]
}
data2 = as.data.frame(cbind(URL = aggregated$URL[aggregated$URL %in% tabledataCopy$stripped_url], 
                            Views = aggregated$Views[aggregated$URL %in% tabledataCopy$stripped_url],
                            Resource = character(sum(aggregated$URL %in% tabledataCopy$stripped_url))),
                      stringsAsFactors = FALSE)
data2$Views = as.numeric(data2$Views)
for (i in 1:nrow(data2)) {
  if (data2$URL[i] %in% tabledataCopy$stripped_url) {
    data2$Resource[i] = tabledataCopy$Resource_Type[which(tabledataCopy$stripped_url == data2$URL[i])]
  }
  else data2$Resource[i] = 0
}

accommodations = c("accommodations", "accommodations / flatmate")
cars = c("cars / ride", "cars / rent", "cars / taxi")
food = c("food / dining", "food / self-grown", "food")
# group
data2$Resource[data2$Resource %in% (accommodations)] = "accommodation"
data2$Resource[data2$Resource %in% (cars)] = "cars"
data2$Resource[data2$Resource %in% (food)] = "food"
viewsPerResource2 = as.data.frame(cbind(Resource = unique(data2$Resource), 
                                        Views = numeric(length(unique(data2$Resource)))))
viewsPerResource2$Views = as.numeric(viewsPerResource2$Views)
for (i in 1:length(unique(data2$Resource))) {
  viewsPerResource2[i,2] = sum(na.omit(data2$Views[data2$Resource == unique(data2$Resource)[i]]))
}
viewsPerResource$Views = round(viewsPerResource$Views,0)
viewsPerResource2$Views = round(viewsPerResource2$Views,0)

sink("outputTex/PageViews.tex", append = TRUE)
cat(paste("\\newcommand\\viewsTotalOld{", 
          sum(na.omit(viewsPerResource2$Views)), "}", sep = ""))
cat(paste("\\newcommand\\viewsAccommodationOld{", 
          viewsPerResource2$Views[viewsPerResource2$Resource == "accommodation"], "}", sep = ""))
cat(paste("\\newcommand\\viewsCarsOld{", 
          viewsPerResource2$Views[viewsPerResource2$Resource == "cars"], "}", sep = ""))
cat(paste("\\newcommand\\viewsMiscOld{", 
          viewsPerResource2$Views[viewsPerResource2$Resource == "miscellaneous"], "}", sep = ""))
cat(paste("\\newcommand\\viewsOtherOld{", 
          sum(na.omit(viewsPerResource2$Views[!viewsPerResource2$Resource %in% c("accommodation", 
                                                                                 "cars", "miscellaneous")])), 
          "}", sep = ""))
cat(paste("\\newcommand\\viewsTotalNew{", 
          sum(na.omit(viewsPerResource$Views)), "}", sep = ""))
cat(paste("\\newcommand\\viewsAccommodationNew{", 
          viewsPerResource$Views[viewsPerResource$Resource == "accommodation"], "}", sep = ""))
cat(paste("\\newcommand\\viewsCarsNew{", 
          viewsPerResource$Views[viewsPerResource$Resource == "cars"], "}", sep = ""))
cat(paste("\\newcommand\\viewsMiscNew{", 
          viewsPerResource$Views[viewsPerResource$Resource == "miscellaneous"], "}", sep = ""))
cat(paste("\\newcommand\\viewsOtherNew{", 
          sum(na.omit(viewsPerResource$Views[!viewsPerResource$Resource %in% c("accommodation", 
                                                                               "cars", "miscellaneous")])), 
          "}", sep = ""))

cat(paste("\\newcommand\\viewsAccommodationOldPercent{", 
          round(viewsPerResource2$Views[viewsPerResource2$Resource == "accommodation"] / 
                  sum(na.omit(viewsPerResource2$Views)) * 100,0), "}", sep = ""))
cat(paste("\\newcommand\\viewsCarsOldPercent{", 
          round(viewsPerResource2$Views[viewsPerResource2$Resource == "cars"] / 
                  sum(na.omit(viewsPerResource2$Views)) * 100,0), "}", sep = ""))
cat(paste("\\newcommand\\viewsMiscOldPercent{", 
          round(viewsPerResource2$Views[viewsPerResource2$Resource == "miscellaneous"] / 
                  sum(na.omit(viewsPerResource2$Views)) * 100,0), "}", sep = ""))
cat(paste("\\newcommand\\viewsOtherOldPercent{", 
          round(sum(na.omit(viewsPerResource2$Views[!viewsPerResource2$Resource %in% c("accommodation", 
                                                                                       "cars", "miscellaneous")])) / 
                  sum(na.omit(viewsPerResource2$Views)) * 100,0), 
          "}", sep = ""))
cat(paste("\\newcommand\\viewsAccommodationNewPercent{", 
          round(viewsPerResource$Views[viewsPerResource$Resource == "accommodation"] / 
                  sum(na.omit(viewsPerResource$Views)) * 100,0), "}", sep = ""))
cat(paste("\\newcommand\\viewsCarsNewPercent{", 
          round(viewsPerResource$Views[viewsPerResource$Resource == "cars"] / 
                  sum(na.omit(viewsPerResource$Views)) * 100,0), "}", sep = ""))
cat(paste("\\newcommand\\viewsMiscNewPercent{", 
          round(viewsPerResource$Views[viewsPerResource$Resource == "miscellaneous"] / 
                  sum(na.omit(viewsPerResource$Views)) * 100,0), "}", sep = ""))
cat(paste("\\newcommand\\viewsOtherNewPercent{", 
          round(sum(na.omit(viewsPerResource$Views[!viewsPerResource$Resource %in% c("accommodation", 
                                                                                     "cars", "miscellaneous")])) / 
                  sum(na.omit(viewsPerResource$Views)) * 100,0), 
          "}", sep = ""))

cat(paste("\\newcommand\\viewsAirbnbOld{", 
          round(data2$Views[data2$URL == "airbnb"],0), "}", sep = ""))

cat(paste("\\newcommand\\viewsVrboOld{", 
          round(data2$Views[data2$URL == "vrbo"],0), "}", sep = ""))

cat(paste("\\newcommand\\viewsUberOld{", 
          round(data2$Views[data2$URL == "uber"],0), "}", sep = ""))

# duplicate entry for homeaway (UK and rest)
cat(paste("\\newcommand\\viewsHomeawayOld{", 
          max(round(data2$Views[data2$URL == "homeaway"],0)), "}", sep = ""))

cat(paste("\\newcommand\\viewsSpareroomOld{", 
          round(data2$Views[data2$URL == "spareroom"],0), "}", sep = ""))

cat(paste("\\newcommand\\viewsAirbnbNew{", 
          round(data$Views[data$URL == "airbnb"],0), "}", sep = ""))

cat(paste("\\newcommand\\viewsVrboNew{", 
          round(data$Views[data$URL == "vrbo"],0), "}", sep = ""))

cat(paste("\\newcommand\\viewsUberNew{", 
          round(data$Views[data$URL == "uber"],0), "}", sep = ""))

cat(paste("\\newcommand\\viewsHomeawayNew{", 
          max(round(data$Views[data$URL == "homeaway"],0)), "}", sep = ""))

cat(paste("\\newcommand\\viewsSpareroomNew{", 
          round(data$Views[data$URL == "spareroom"],0), "}", sep = ""))
closeAllConnections()
