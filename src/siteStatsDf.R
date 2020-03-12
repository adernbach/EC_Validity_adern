
siteStatsDf <- function(Dir, site, dataproduct, level, exchange) {
print(paste0("calculating percentages for ", site))

setwd(paste0(Dir, "/", site))

if (!file.exists(paste0(site, "_data_", level, ".rds"))) {
  
  fluxData <-
    stackEddy(filepath = paste0(Dir, "/", site, "/filesToStack00200"),
              level = level, avg=30)
  saveRDS(fluxData, (paste0(site, "_data_", level, ".rds")))
} else {
  fluxData <- readRDS((paste0(site, "_data_", level, ".rds")))
}

siteData <- fluxData[[site]]

timeB <-
  substring(siteData$timeBgn, 1, nchar(siteData$timeBgn) - 4)
timeB <-
  strptime(timeB, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")
timeB <- as.POSIXct(timeB)
siteData <- cbind(timeB, fluxData[[site]])


missingQF <-
  siteData[is.na(siteData[[paste0("qfqm.", dataproduct, ".", exchange, ".qfFinl")]]),]

hasQF <-
  siteData[!is.na(siteData[[paste0("qfqm.", dataproduct, ".", exchange, ".qfFinl")]]),]

flaggedData <-
  hasQF[which(hasQF[[paste0("qfqm.", dataproduct, ".", exchange, ".qfFinl")]] == 1),]

goodData <-
  hasQF[which(hasQF[[paste0("qfqm.", dataproduct, ".", exchange, ".qfFinl")]] == 0),]


percentNaN <-
  round(nrow(siteData[is.na(siteData[[paste0("data.", dataproduct, ".", exchange, ".flux")]]),]) /
          nrow(siteData), digits = 2) * 100

percentMissingQF <-
  round(nrow(siteData[is.na(siteData[[paste0("qfqm.", dataproduct, ".", exchange, ".qfFinl")]]),]) /
          nrow(siteData), digits = 2) * 100

percentFlagged <-
  round(nrow(hasQF[which(hasQF[[paste0("qfqm.", dataproduct, ".", exchange, ".qfFinl")]] == 1),]) /
          nrow(siteData), digits = 2) * 100

percentGood <-
  round(nrow(hasQF[which(hasQF[[paste0("qfqm.", dataproduct, ".", exchange, ".qfFinl")]] == 0),]) /
          nrow(siteData), digits = 2) * 100

cols <- c("site", "percentNaN", "percentNoQF", "raisedQF", "goodData")

siteStats <- as.list(c(site, percentNaN, percentMissingQF, percentFlagged, percentGood))

names(siteStats) <- cols

siteStats <- data.frame(siteStats)

return(siteStats)
}

statsDf = NULL

names(statsDf) <- c("site", "percentNaN", "percentNoQF", "raisedQF", "goodData")

 for (site in allsites) {
  tmp <- siteStatsDf(Dir = Dir, site = site, dataproduct = "fluxTemp", level = "dp04", exchange = "stor")
  statsDf <- rbind(statsDf, tmp)
}

setwd("~/eddy/data")

write.csv(statsDf, "fluxTempStor_validity_stats.csv")
