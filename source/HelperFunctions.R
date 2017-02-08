if (!"stringi" %in% installed.packages()[,"Package"]) install.packages("stringi")
if (!"XLConnect" %in% installed.packages()[,"Package"]) install.packages("XLConnect")
require(stringi)
require(XLConnect)

#normalizes URLs and names of a dataset
#adds an ID column to a dataset
#returns the normalized dataset
normalize_name = function(data) {
  #remove entries that have NA values for URLs
  data$url[data$url == "N/A"] = NA
  data = data[complete.cases(data$url),]
  
  #format urls as characters
  data$url = vapply(data$url, as.character, as.character(1))
  
  #format names as characters
  data$name = vapply(data$name, as.character, as.character(1))
  
  #create a column for normalized names
  #set names to lowercase
  for (i in 1:nrow(data)) {
    data$name_normalized[i] = tolower(data$name[i])
  }
  
  #remove all blanks from names
  for (i in 1:nrow(data)) {
    data$name_normalized[i] = gsub(" ", "", data$name_normalized[i], fixed = TRUE)
  }
  return(data)
}

trim_url = function(data) {
  require(stringi) || install.packages("stringi")
  
  #copy the url
  data$url_trimmed = data$url
  
  #trim URL strings
  #first dataset
  for (i in 1:length(data$url)) {
    #transform to lowercase
    data$url_trimmed[i] = tolower(data$url_trimmed[i])
    
    #remove www. and keep the string after that
    data$url_trimmed[i] = stri_split(data$url_trimmed[i], regex = "www.")[[1]][length(stri_split(data$url_trimmed[i], regex = "www.")[[1]])]

    #remove http:// and keep the string after that
    data$url_trimmed[i] = stri_split(data$url_trimmed[i], regex = "http://")[[1]][length(stri_split(data$url_trimmed[i], regex = "http://")[[1]])]

    #remove https:// and keep the string after that
    data$url_trimmed[i] = stri_split(data$url_trimmed[i], regex = "https://")[[1]][length(stri_split(data$url_trimmed[i], regex = "https://")[[1]])]

    #take only the last part of the URL (aka the part behind the last /) if it contains collaborativeconsumption.com/directoryitem/
    #turned off right now. The reason is that some URLs are just parts of facebook, this would trim the essential part
    #if (stri_detect(str = data$url[i], regex = "collaborativeconsumption.com/directoryitem/") == TRUE) {
    #  data$url[i] = stri_split(data$url[i], regex = "/")[[1]][length(stri_split(data$url[i], regex = "/")[[1]])]
    #}
    #trim the string removing everything after the first "/"
    data$url_trimmed[i] = stri_split(data$url_trimmed[i], regex = "/")[[1]][1]
  }
  return(data)
}

#performs matching of two datasets based on naive name matching
#returns a list of the matching indexes of the first set and the second set
perform_name_matching = function(data1, data2) {
  matches_1_2 = as.data.frame(cbind(data1 = numeric(0), data2 = numeric(0)))
  for (i in 1:nrow(data1)) {
    for (j in 1:nrow(data2)) {
      if (grepl(pattern = data1$name_normalized[i], x = data2$name_normalized[j], fixed = TRUE) == TRUE) {
        matches_1_2 = rbind(matches_1_2, c(i, j))
        next
      }
    }  
  }
  colnames(matches_1_2) = c("data1", "data2")
  return(matches_1_2)
}

#returns string matches up to the maximum distance specified and their corresponding distance
perform_name_matching_distance = function(data1, data2, maxdistance) {
  require(stringdist) || install.packages("stringdist")
  results = as.data.frame(cbind(index = numeric(0), distance = numeric(0)))
  for (i in 1:maxdistance) {
    matches = amatch(data1$name, data2$name, maxdistance)
    #only add matches that were not previously matched with a lower distance
    newmatches = matches[-matches %in% results$index]
    distances = rep(maxdistance, times = length(newmatches))
    new = cbind(index = newmatches, distance = distances)
    results = rbind(results, new)
  }
  return(results)
}

removeTrailingWhitespace = function(data) {
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      #remove trailing blanks
      data[i,j] = gsub("\\s+$", "",as.character(data[i,j]),perl = TRUE)
    }
  }
  return(data)
}

#reads XLSX files that have the format of auswertung_JBE_final and formats NAs and factor variables
#returns the formatted file as a dataframe
readDirectoriesDataXLSX = function(filePath, sheetNo) {
  library(XLConnect)
  #read xlsx file
  firstSet <- readWorksheet(loadWorkbook(filePath),sheet=sheetNo)
  
  #format missing values as NAs
  for (i in 1:ncol(firstSet)) {
    for (j in 1:nrow(firstSet)) {
      firstSet[j,i] = ifelse(firstSet[j,i] == "", NA, firstSet[j,i])
      firstSet[j,i] = ifelse(firstSet[j,i] == " ", NA, firstSet[j,i])
      firstSet[j,i] = tolower(firstSet[j,i])
      
      #remove trailing blanks
      firstSet[j,i] = gsub("\\s+$", "",firstSet[j,i],perl = TRUE)
    }
  }
  
  #format certain columns as factors
  firstSet$economical = ifelse(firstSet$economical == "x", TRUE, FALSE)[1:nrow(firstSet)]
  firstSet$environmental = ifelse(firstSet$environmental == "x", TRUE, FALSE)[1:nrow(firstSet)]
  firstSet$social = ifelse(firstSet$social == "x", TRUE, FALSE)[1:nrow(firstSet)]
  firstSet$Per_transaction = ifelse(firstSet$Per_transaction == "x", TRUE, FALSE)[1:nrow(firstSet)]
  firstSet$Per_listing = ifelse(firstSet$Per_listing == "x", TRUE, FALSE)[1:nrow(firstSet)]
  firstSet$Membership_fee = ifelse(firstSet$Membership_fee == "x", TRUE, FALSE)[1:nrow(firstSet)]
  firstSet$Combination = ifelse(firstSet$Combination == "x", TRUE, FALSE)[1:nrow(firstSet)]
  firstSet$iOS = ifelse(firstSet$iOS == "x", TRUE, FALSE)[1:nrow(firstSet)]
  firstSet$Android = ifelse(firstSet$Android == "x", TRUE, FALSE)[1:nrow(firstSet)]
  firstSet$WindowsPhone = ifelse(firstSet$WindowsPhone == "x", TRUE, FALSE)[1:nrow(firstSet)]
  
  #format Year_Launch as numeric
  firstSet$Year_Launch = as.numeric(firstSet$Year_Launch)
  return(firstSet)
}

save.xlsx = function (file, objects) {
  require(XLConnect)
  for (i in 1:length(objects)) {
    writeWorksheetToFile(file = file, data = objects[[i]], sheet = names(output)[i])
  }
}


#DEPRECATED
#Throws an OutOfMemory error when reading larger fiels
#reads general XLSX files
#formats NAs and sets everything to lowercase
readXLSX = function(filePath) {
  library(XLConnect)
  
  #give Java virtual machine more memory for larger excel files
  #apparently does not work
  options(java.parameters = "- Xmx2048m")
  
  #read xlsx file
  data = readWorksheet(loadWorkbook(filePath),sheet=1)
  
  #format missing values as NAs
  for (i in 1:ncol(data)) {
    for (j in 1:nrow(data)) {
      data[j,i] = ifelse(data[j,i] == "", NA, data[j,i])
      data[j,i] = ifelse(data[j,i] == " ", NA, data[j,i])
      data[j,i] = tolower(data[j,i])
    }
  }
  return(data)
}

generateOutput = function(data, suffix) {
  
  requiredNames = c("economical",
                    "environmental",
                    "social",
                    "P2P_Pattern",
                    "Market_Mediation",
                    "Type_of_Accessed_Object",
                    "Resource_Owner",
                    "Consumer_Involvement",
                    "Money_Flow",
                    "Global_Integration",
                    "Global_Integration_finest_level",
                    "Per_transaction",
                    "Per_listing",
                    "Membership_fee",
                    "iOS",
                    "Android",
                    "WindowsPhone")
  if(sum(!requiredNames %in% colnames(data)) > 0) {
    for (i in 1:length(requiredNames)) {
      if (requiredNames[i] %in% colnames(data)) warning(paste("column", requiredNames[i], "missing", sep = " "))
    }
    stop("Colnames missing!")
  }
  
  n = nrow(data)
  promo_eco = sum(data$economical)/n
  promo_env = sum(data$environmental)/n
  promo_soc = sum(data$social)/n
  
  #################
  # Patterns
  options(warning.length=8000)
  if(sum(as.character(data$P2P_Pattern) %in% c("deferred", "recurrent", "immediate") == FALSE) > 0) {
    warning(paste("Column 'P2P_Pattern' contains value that differ from 'deferred', 'recurrent' or 'immediate'. Check the spelling! 
                  Check entry", which(as.character(data$P2P_Pattern) %in% c("deferred", "recurrent", "immediate") == FALSE), "\n"))
  }
  pattern_def1 = sum(as.character(data$P2P_Pattern) == "deferred")
  pattern_rec1 = sum(as.character(data$P2P_Pattern) == "recurrent")
  pattern_imm1 = sum(as.character(data$P2P_Pattern) == "immediate")
  pattern_def = pattern_def1 / (pattern_def1 + pattern_rec1 + pattern_imm1)
  pattern_rec = pattern_rec1 / (pattern_def1 + pattern_rec1 + pattern_imm1)
  pattern_imm = pattern_imm1 / (pattern_def1 + pattern_rec1 + pattern_imm1)

  
  #################
  # Type
  if(sum(as.character(data$Type_of_Accessed_Object) %in% c("functional", "mixed") == FALSE) > 0) {
    warning(paste("Column 'Type_of_Accessed_Object' contains value that differ from 'functional' or 'mixed'. Check the spelling! 
                  Check entry", which(as.character(data$Type_of_Accessed_Object) %in% c("functional", "mixed") == FALSE), "\n"))
  }
  type_funct1 = sum(data$Type_of_Accessed_Object == "functional")
  type_mixed1 = sum(data$Type_of_Accessed_Object == "mixed")
  type_funct = type_funct1 / (type_funct1 + type_mixed1)
  type_mixed = type_mixed1 / (type_funct1 + type_mixed1)
  
  
  #################
  # Owner
  if(sum(as.character(data$Resource_Owner) %in% c("private", "private and business") == FALSE) > 0) {
    warning(paste("Column 'Resource_Owner' contains value that differ from 'private' or 'private and business'. Check the spelling! 
                  Check entry", which(as.character(data$Resource_Owner) %in% c("private", "private and business") == FALSE), "\n"))
  }
  owner_private1 = sum(data$Resource_Owner == "private")
  owner_both1 = sum(data$Resource_Owner == "private and business")
  owner_private = owner_private1 / (owner_private1 + owner_both1)
  owner_both = owner_both1 / (owner_private1 + owner_both1)

  #################
  # Smartphone support
  android = sum(data$Android)/n
  ios = sum(data$iOS == "TRUE")/n
  windows = sum(data$WindowsPhone == "TRUE")/n
  
  if(is.na(android)) {
    warning(paste("Android app column contain NAs.
                  Check entry"), which(is.na(data$Android)))
  }
  if(is.na(ios)) {
    warning(paste("IOS app column contain NAs.
                  Check entry"), which(is.na(data$iOS)))
  }
  if(is.na(windows)) {
    warning(paste("WindowsPhone app column contain NAs.
                  Check entry"), which(is.na(data$WindowsPhone)))
  }
  #################
  # Consumer involvment
  if(sum(as.character(data$Consumer_Involvement) %in% c("full-service", 
                                                        "self-service",
                                                        "in-between") == FALSE) > 0) {
    warning(paste("Column 'Consumer_Involvement' contains value that differ from 'full-service', 'self-service' or 'in-between'. Check the spelling! 
                  Check entry", which(as.character(data$Consumer_Involvement) %in% c("full-service", 
                                                                                     "self-service",
                                                                                     "in-between") == FALSE), "\n"))
  }
  involvement_self1 = sum(data$Consumer_Involvement == "self-service")
  involvement_between1 = sum(data$Consumer_Involvement == "in-between")
  involvement_full1 = sum(data$Consumer_Involvement == "full-service")
  involvement_self = involvement_self1 / (involvement_self1 + involvement_between1 + involvement_full1)
  involvement_between = involvement_between1 / (involvement_self1 + involvement_between1 + involvement_full1)
  involvement_full = involvement_full1 / (involvement_self1 + involvement_between1 + involvement_full1)
  
  #################
  # Market mediation
  if(sum(as.character(data$Market_Mediation) %in% c("profit from peer providers",
                                                    "profit from both peer consumers and peer providers",
                                                    "profit from peer consumers",
                                                    "indirect profit",
                                                    "not-for-profit") == FALSE) > 0) {
    warning(paste("Column 'Market_Mediation' contains value that differ from 'profit from peer providers', 'profit from both peer consumers and peer providers', 'profit from peer consumers', 'not-for-profit' or 'indirect profit'. Check the spelling! 
                  Check entry", which(as.character(data$Market_Mediation) %in% c("profit from peer providers",
                                                                                 "profit from both peer consumers and peer providers",
                                                                                 "profit from peer consumers",
                                                                                 "indirect profit",
                                                                                 "not-for-profit") == FALSE), "\n"))
  }
  mediation_profit1 = sum(data$Market_Mediation %in% c("profit from peer providers",
                                                      "profit from both peer consumers and peer providers",
                                                      "profit from peer consumers",
                                                      "indirect profit"))
  mediation_noProfit1 = sum(data$Market_Mediation == "not-for-profit")
  mediation_profit = mediation_profit1 / (mediation_profit1 + mediation_noProfit1)
  mediation_noProfit = mediation_noProfit1 / (mediation_profit1 + mediation_noProfit1)
  
  mediation_consumer1 = sum(data$Market_Mediation == "profit from peer consumers")
  mediation_provider1 = sum(data$Market_Mediation == "profit from peer providers")
  mediation_both1 = sum(data$Market_Mediation == "profit from both peer consumers and peer providers")
  mediation_indirect1 = sum(data$Market_Mediation == "indirect profit")
  mediation_consumer = mediation_consumer1 / (mediation_consumer1 + mediation_provider1 + mediation_both1 + mediation_indirect1 + mediation_noProfit1)
  mediation_provider = mediation_provider1 / (mediation_consumer1 + mediation_provider1 + mediation_both1 + mediation_indirect1 + mediation_noProfit1)
  mediation_both = mediation_both1 / (mediation_consumer1 + mediation_provider1 + mediation_both1 + mediation_indirect1 + mediation_noProfit1)
  mediation_indirect = mediation_indirect1 / (mediation_consumer1 + mediation_provider1 + mediation_both1 + mediation_indirect1 + mediation_noProfit1)
  
  #################
  # Money flow
  if(sum(as.character(data$Money_Flow) %in% c("c2c", 
                                              "c2b2c",
                                              "c2b",
                                              "free") == FALSE) > 0) {
    warning(paste("Column 'Money_Flow' contains value that differ from 'c2c', 'c2b2c', 'c2b' or 'free'. Check the spelling! 
                  Check entry", which(as.character(data$Money_Flow) %in% c("c2c", 
                                                                           "c2b2c",
                                                                           "c2b",
                                                                           "free") == FALSE), "\n"))
  }
  money_C2C1 = sum(data$Money_Flow == "c2c")
  money_C2B2C1 = sum(data$Money_Flow == "c2b2c")
  money_C2B1 = sum(data$Money_Flow == "c2b")
  money_free1 = sum(data$Money_Flow == "free")
  money_C2C = money_C2C1 / (money_C2C1 + money_C2B2C1 + money_C2B1 + money_free1)
  money_C2B2C = money_C2B2C1 / (money_C2C1 + money_C2B2C1 + money_C2B1 + money_free1)
  money_C2B = money_C2B1 / (money_C2C1 + money_C2B2C1 + money_C2B1 + money_free1)
  money_free = money_free1 / (money_C2C1 + money_C2B2C1 + money_C2B1 + money_free1)
  
  #################
  # Global integration
  if(sum(as.character(data$Global_Integration_finest_level) %in% c("global", 
                                                                   "city-wide",
                                                                   "country-wide", 
                                                                   "region-wide") == FALSE) > 0) {
    warning(paste("Column 'Global_Integration_finest_level' contains value that differ from 'global', 'city-wide', 'country-wide', 'region-wide', 'continent-wide' or 'global'. Check the spelling! 
                  Check entry", which(as.character(data$Global_Integration_finest_level) %in% c("global", 
                                                                                                "city-wide",
                                                                                                "country-wide", 
                                                                                                "region-wide") == FALSE), "\n"))
  }
  if(sum(as.character(data$Global_Integration) %in% c("separated communities",
                                                      "integrated") == FALSE) > 0) {
    warning(paste("Column 'Global_Integration' contains value that differ from 'separated communities' or 'integrated'. Check the spelling! 
                  Check entry", which(as.character(data$Global_Integration) %in% c("separated communities",
                                                                                   "integrated")  == FALSE), "\n"))
  }
  
  integration_separated = data$Global_Integration_finest_level[data$Global_Integration == "separated communities"]
  integration_integrated = data$Global_Integration_finest_level[data$Global_Integration == "integrated"]
  integrated_total = length(integration_integrated) / (length(integration_separated) + length(integration_integrated))
  separated_total = length(integration_separated) / (length(integration_separated) + length(integration_integrated))
  
  integrated_city1 = sum(integration_integrated == "city-wide")
  integrated_country1 = sum(integration_integrated == "country-wide")
  integrated_region1 = sum(integration_integrated == "region-wide")
  integrated_global1 = sum(integration_integrated == "global")
  separated_city1 = sum(integration_separated == "city-wide")
  separated_country1 = sum(integration_separated == "country-wide")
  separated_region1 = sum(integration_separated == "region-wide")
  integrated_city = integrated_city1 / (integrated_city1 + integrated_country1 + integrated_region1 + integrated_global1 + 
                                          separated_city1 + separated_country1 + separated_region1)
  integrated_country = integrated_country1 / (integrated_city1 + integrated_country1 + integrated_region1 + integrated_global1 + 
                                                separated_city1 + separated_country1 + separated_region1)
  integrated_region = integrated_region1 / (integrated_city1 + integrated_country1 + integrated_region1 + integrated_global1 + 
                                              separated_city1 + separated_country1 + separated_region1)
  integrated_global = integrated_global1 / (integrated_city1 + integrated_country1 + integrated_region1 + integrated_global1 + 
                                              separated_city1 + separated_country1 + separated_region1)
  separated_city = separated_city1 / (integrated_city1 + integrated_country1 + integrated_region1 + integrated_global1 + 
                                        separated_city1 + separated_country1 + separated_region1)
  separated_country = separated_country1 / (integrated_city1 + integrated_country1 + integrated_region1 + integrated_global1 + 
                                              separated_city1 + separated_country1 + separated_region1)
  separated_region = separated_region1 / (integrated_city1 + integrated_country1 + integrated_region1 + integrated_global1 + 
                                            separated_city1 + separated_country1 + separated_region1)
  
  #################
  # Transaction type
  transaction1 = sum(data$Per_transaction)
  listing1 = sum(data$Per_listing)
  membership1 = sum(data$Membership_fee)
  combinedFees1 = sum(data$Combination)
  transaction = transaction1 / (transaction1 + listing1 + membership1 + combinedFees1)
  listing = listing1 / (transaction1 + listing1 + membership1 + combinedFees1)
  membership = membership1 / (transaction1 + listing1 + membership1 + combinedFees1)
  combinedFees = combinedFees1 / (transaction1 + listing1 + membership1 + combinedFees1)
  
  result = cbind(promo_eco, promo_env, promo_soc, 
                 pattern_def, pattern_imm, pattern_rec,
                 type_funct, type_mixed, 
                 owner_private, owner_both,
                 involvement_self, involvement_between, involvement_full, integrated_total, separated_total,
                 integrated_city, integrated_country, integrated_region, integrated_global, 
                 separated_city, separated_country, separated_region, 
                 mediation_profit, mediation_noProfit, mediation_consumer, mediation_provider, mediation_both, mediation_indirect,
                 money_C2C, money_C2B2C, money_C2B, money_free,
                 transaction, listing, membership, combinedFees,
                 android, ios, windows)
  colnames(result) = paste(colnames(result), suffix, sep = "_")
  return(t(result))
}



filter = function(data, columnName, columnLevel, negation = FALSE) {
  if (negation == FALSE) filteredData = data[as.character(data[[columnName]]) %in% columnLevel,]
  else filteredData = data[!as.character(data[[columnName]]) %in% columnLevel,]
  print(paste("Number of records filtered: ", nrow(filteredData), sep = ""))
  return(filteredData)
}


parseResourceTypeFrequencies = function(data, groupings) {
  # find resource types not included so far
  notInc = unique(data$Resource_Type) # should be an empty list in the end
  for (i in 1:length(groupings)) {
    current = groupings[[i]]
    notInc = notInc[!notInc %in% current]
  }
  if (length(notInc) > 0) {
    for (i in 1:length(notInc)) {
     warning(paste("resource type", notInc[i], "not included", sep = " "))
    }
  }
  
  # get frequencies
  freqs = numeric(0)
  abs = numeric(0)
  for (i in 1:length(groupings)) {
    current = groupings[[i]]
    freqs = c(freqs,length(data$Resource_Type[data$Resource_Type %in% current])/nrow(data))
    abs = c(abs,length(data$Resource_Type[data$Resource_Type %in% current]))
  }
  return(cbind(names(groupings), abs, freqs))
}

parseYearLaunchData = function(data, thresholdYear, currentYear) {
  for (i in 1:length(data)) {
    current = data[[i]]
    current = current[names(current) >=thresholdYear]
    data[[i]] = current
  }
  years = thresholdYear:currentYear
  yearsData = as.data.frame(years)
  for (i in 1:length(data)) {
    yearsData = as.data.frame(cbind(yearsData, numeric(length(yearsData))))
  }
  colnames(yearsData) = c("Years", names(data))
  for (i in 1:length(years)) {
    for (j in 1:length(data)) {
      current = data[[j]]
      column = which(names(current) == years[i])
      if (length(column) == 1) yearsData[i, j+1] = current[column]
    }
  }
  return(yearsData)
}


createTexTable = function(data, rowNames, file, caption, label) {
  cat("\\begin{table}[h] \n" , file = file)
  cat("\\resizebox{\\textwidth}{!}{% \n" , file = file, append = TRUE)
  cat("\\begin{tabular}[h] {", file = file, append = TRUE)
  cat("l ", file = file, append = TRUE)
  for (i in 1:(nrow(data))) {
    cat("l ", file = file, append = TRUE)
  }
  cat("} \n", file = file, append = TRUE)
  cat("\\toprule \n" , file = file, append = TRUE)
  cat(paste("\\textbf{", rowNames[1], "}&", sep = ""), file = file, append = TRUE)
  for (i in 1:(nrow(data)-1)) {
    cat(paste(data[i,1], "&"), file = file, append = TRUE)
  }
  cat(paste(data[nrow(data),1], "\\\\ \n"), file = file, append = TRUE)
  cat("\\midrule \n" , file = file, append = TRUE)
  for (i in 2:ncol(data)) {
    cat(paste("\\textbf{", rowNames[i], "}&", sep = ""), file = file, append = TRUE)
    for (j in 1:(nrow(data)-1)) {
      cat(paste(data[j,i], "&"), file = file, append = TRUE)
    }
    cat(paste(data[nrow(data),i], "\\\\ \n"), file = file, append = TRUE)
  }
  cat("\\bottomrule \n" , file = file, append = TRUE)
  cat("\\end{tabular}} \n" , file = file, append = TRUE)
  cat(paste("\\caption{", caption, "} \n", sep = ""), file = file, append = TRUE)
  cat(paste("\\label{", label, "} \n", sep = ""), file = file, append = TRUE)
  cat("\\end{table} \n" , file = file, append = TRUE)
}

createDistanceMatrix = function(x) {
  distmat = matrix(nrow = nrow(x), ncol = nrow(x))
  for(i in 1:nrow(x)) {
    print(paste("Attribute", i))
    for(j in 1:nrow(x)) {
      dist = 0
      for(k in 1:ncol(x)) {
        if (!x[i,k] == x[j,k]) dist = dist + 1
      }
      distmat[i,j] = dist
    }
  }
  return(distmat)
}
