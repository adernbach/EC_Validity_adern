


#need to add function argument for horver to separate out ECSE MLs and validations
plotBySite <-
  function(download = TRUE,
           DirIn,
           DirOut,
           site,
           dataproduct,
           exchange = "nsae",
           level = "dp04", 
           ymin = -50,
           ymax = 50
  ) {
    
    library(neonUtilities)
    library(rhdf5)
    library(ggplot2)
    
    if (download == TRUE) {
      if (!dir.exists(paste0(Dir, "/", site))) {
        dir.create(paste0(Dir, "/", site))
      }
      
      zipsByProduct(
        dpID = "DP4.00200.001",
        package = "basic",
        site = site,
        startdate = "2010-01",
        enddate = "2020-01",
        savepath = paste0(Dir, "/", site),
        check.size = F
      )
    }
    
    if (level == "dp04") {
    print(paste0("plotting data for ", site))
    
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
    
    #validity <- need to think about how to calculate this due to the missing quality flags for turb and nsae. Cove currently just does availability for these products. If I calculate true validity here, it can only be for the period with non-missing quality flags. 
    
    setwd(DirOut)
    
    cols <-
      c("noQf" = "gray",
        "qfFnl_1" = "#f04546",
        "qfFnl_0" = "black")
    
    p <-
      ggplot(missingQF, aes(x = timeB, y = missingQF[[paste0("data.", dataproduct, ".", exchange, ".flux")]]), colors = cols) +
      geom_point(color = "gray") +
      theme_bw() +
      xlab("Date") +
      ylab(ifelse(dataproduct == "fluxCo2", 
                  paste0(dataproduct, "_", "_umolCo2 m-2 s-1_", exchange), 
                  paste0(dataproduct, "_",  "_W m-2_", exchange))) +
      ylim(ymin, ymax) +
      xlim(min(siteData$timeB), max(siteData$timeB)) +
      ggtitle(
        paste0(
          site,
          "_",
          exchange
        ),
        subtitle = paste0(percentNaN,
                          "%NaN,  ",
                          percentMissingQF,
                          "%noQF,  ",
                          percentFlagged,
                          "%raisedQF,  ",
                          percentGood,
                          "%goodData")
      ) 
    
    if (nrow(flaggedData) > 0) {
      
      p <- p + geom_point(data = flaggedData,
                          mapping = aes(x = timeB, y = flaggedData[[paste0("data.", dataproduct, ".", exchange, ".flux")]], color = "qfFnl_1")) 
    }  
    
    if (nrow(goodData) > 0) {
      p <-  p + geom_point(data = goodData,
                           mapping = aes(x = timeB, y = goodData[[paste0("data.", dataproduct, ".", exchange, ".flux")]], color = "qfFnl_0")) 
    } 
    
    p + scale_colour_manual(
      name = "",
      values = cols,
      guide = guide_legend(override.aes = aes(fill =
                                                NA))
    ) +
      
      ggsave(
        paste0(site, "_", exchange, "_", dataproduct, ".png"),
        width = 10,
        height = 6
      )
    
  setwd(DirIn)  
    
    }
    
    if (level == "dp01") {
      
      print(paste0("plotting data for ", site))
      
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
      
      ##in the future change dataType to dataproduct and dataproduct to datastream
      
      if (dataproduct == "rtioMoleDryCo2" & exchange == "turb") {
        dataType <- "co2Turb"
      }
      
      if (dataproduct == "rtioMoleDryH2o" & exchange == "turb") {
        dataType <- "h2oTurb"
      }
      
      if (dataproduct == "rtioMoleDryCo2" & exchange == "stor") {
        dataType <- "co2Stor"
      }
      
      if (dataproduct == "rtioMoleDryH2o" & exchange == "stor") {
        dataType <- "h2oStor"
      }
      
      
      if (dataproduct == "dlta13CCo2") {
        dataType <- "isoCo2"
      } 
      
      if (dataproduct == "dlta18OH2o") {
        dataType <- "isoH2o"
      }
      
      #should add subsetting by measurement level 
      
      missingQF <-
        siteData[is.na(siteData[[paste0("qfqm.", dataType, ".", dataproduct, ".qfFinl")]]),]
      
      hasQF <-
        siteData[!is.na(siteData[[paste0("qfqm.", dataType, ".", dataproduct, ".qfFinl")]]),]
      
      flaggedData <-
        hasQF[which(hasQF[[paste0("qfqm.", dataType, ".", dataproduct, ".qfFinl")]] == 1),]
      
      goodData <-
        hasQF[which(hasQF[[paste0("qfqm.", dataType, ".", dataproduct, ".qfFinl")]] == 0),]
      
      
      percentNaN <-
        round(nrow(siteData[is.na(siteData[[paste0("data.", dataType, ".", dataproduct, ".mean")]]),]) /
                nrow(siteData), digits = 2) * 100
      
      percentMissingQF <-
        round(nrow(siteData[is.na(siteData[[paste0("qfqm.", dataType, ".", dataproduct, ".qfFinl")]]),]) /
                nrow(siteData), digits = 2) * 100
      
      percentFlagged <-
        round(nrow(hasQF[which(hasQF[[paste0("qfqm.", dataType, ".", dataproduct, ".qfFinl")]] == 1),]) /
                nrow(siteData), digits = 2) * 100
      
      percentGood <-
        round(nrow(hasQF[which(hasQF[[paste0("qfqm.", dataType, ".", dataproduct, ".qfFinl")]] == 0),]) /
                nrow(siteData), digits = 2) * 100
      
      
      if (!is.null(DirOut)) {
        setwd(DirOut)
      }
      
      
      cols <-
        c("noQf" = "gray",
          "qfFnl_1" = "#f04546",
          "qfFnl_0" = "black")
      
      p <-
        ggplot(missingQF, aes(x = timeB, y = missingQF[[paste0("data.", dataType, ".", dataproduct, ".mean")]]), colors = cols) +
        geom_point(color = "gray") +
        theme_bw() +
        xlab("Date") +
        ylab(dataproduct) +
        ylim(ymin, ymax) +
        xlim(min(siteData$timeB), max(siteData$timeB)) +
        ggtitle(
          paste0(
            site,
            "_",
            dataproduct,
            "_",
            exchange
          ),
          subtitle = paste0(percentNaN,
                            "%NaN,  ",
                            percentMissingQF,
                            "%noQF,  ",
                            percentFlagged,
                            "%raisedQF,  ",
                            percentGood,
                            "%goodData")
        ) 
      
      if (nrow(flaggedData) > 0) {
        
        p <- p + geom_point(data = flaggedData,
                            mapping = aes(x = timeB, y = flaggedData[[paste0("data.", dataType, ".", dataproduct, ".mean")]], color = "qfFnl_1")) 
      }  
      
      if (nrow(goodData) > 0) {
        p <-  p + geom_point(data = goodData,
                             mapping = aes(x = timeB, y = goodData[[paste0("data.", dataType, ".", dataproduct, ".mean")]], color = "qfFnl_0")) 
      } 
      
      p + scale_colour_manual(
        name = "",
        values = cols,
        guide = guide_legend(override.aes = aes(fill =
                                                  NA))
      ) +
        
        ggsave(
          paste0(site, "_", exchange, "_", dataproduct, ".png"),
          width = 10,
          height = 6
        )
      
      setwd(DirIn)   
      
    }
    
  }
    
    
