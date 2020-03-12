###
#run plotBySite function over all sites with download=F for each flux data product 

allsites = c("BART","HARV","BLAN","SCBI","SERC","OSBS","DSNY","JERC","GUAN","LAJA",
"UNDE","STEI","TREE","KONZ","UKFS","KONA","ORNL","MLBS","GRSM",
"TALL","LENO","DELA","WOOD","DCFS","NOGP","STER","CPER","RMNP","NIWO",
"CLBJ","OAES","YELL","MOAB","ONAQ","SRER","JORN","WREF","ABBY",
"SJER","SOAP","TEAK","TOOL","BARR","BONA","DEJU","HEAL")

waterIsoSites <- c("HARV","SCBI","OSBS",
"GUAN","UNDE","KONZ","ORNL","TALL",
"WOOD","CPER","NIWO","CLBJ","YELL",
"ONAQ","SRER","WREF","SJER","TOOL",
"BARR","BONA", "BARR")

lapply(allsites,
       plotBySite,
       DirIn = "/home/cflorian/eddy/data",
       DirOut = NULL,
       download = F, 
       level = "dp01",
       exchange = "stor",
       dataproduct = "rtioMoleDryCo2",
       ymin = 300,
       ymax = 500
)

lapply(allsites,
       plotIsotopeData,
       DirIn = "/home/cflorian/eddy/data",
       DirOut = NULL,
       download = F, 
       dataproduct = "dlta13CCo2",
       ymin = -30
)

lapply(waterIsoSites,
       plotIsotopeData,
       DirIn = "/home/cflorian/eddy/data",
       DirOut = NULL,
       download = F, 
       dataproduct = "dlta18OH2o",
       ymin = -30
)


D19sites <- c("BONA","DEJU","HEAL")

lapply(D19sites, 
       plotBySite, 
       download = FALSE, 
       DirIn = "~/eddy/data", 
       DirOut = "~/eddy/data/D19", 
       dataproduct = "fluxCo2", 
       exchange = "turb", 
       level = "dp04", 
       ylmt = 50)

lapply(D19sites, 
       plotBySite, 
       download = FALSE, 
       DirIn = "~/eddy/data", 
       DirOut = "~/eddy/data/D19", 
       dataproduct = "fluxH2o", 
       exchange = "turb", 
       level = "dp04", 
       ylmt = 500)

lapply(D19sites, 
       plotBySite, 
       download = FALSE, 
       DirIn = "~/eddy/data", 
       DirOut = "~/eddy/data/D19", 
       dataproduct = "fluxCo2", 
       exchange = "stor", 
       level = "dp04", 
       ylmt = 50)

lapply(D19sites, 
       plotBySite, 
       download = FALSE, 
       DirIn = "~/eddy/data", 
       DirOut = "~/eddy/data/D19", 
       dataproduct = "fluxH2o", 
       exchange = "stor", 
       level = "dp04", 
       ylmt = 500)


plotByDomian function(domain) {

if (domain == "D01") {
  sitesByDomain = c("BART", "HARV")
}

if (domain == "D02") {
  sitesByDomain = c("BLAN","SCBI","SERC")
}

if (domain == "D03") {
  sitesByDomain = c("OSBS","DSNY","JERC")
}

if (domain == "D04") {
  sitesByDomain = c("GUAN","LAJA")
}

if (domain == "D05") {
  sitesByDomain = c("UNDE","STEI","TREE")
}

if (domain == "D06") {
  sitesByDomain = c("KONZ","UKFS","KONA")
}

if (domain == "D07") {
  sitesByDomain = c("ORNL","MLBS","GRSM")
}

if (domain == "D08") {
  sitesByDomain = c("TALL","LENO","DELA")
}

if (domain == "D09") {
  sitesByDomain = c("WOOD","DCFS","NOGP")
}

if (domain == "D10") {
  sitesByDomain = c("STER","CPER","RMNP")
}

