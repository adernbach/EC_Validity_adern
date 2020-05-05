### EddyCo LI7200 validation analysis ###

rm(list=ls()) #clear all variables
graphics.off() #closes plots


options(stringsAsFactors = F)
library(neonUtilities)
library(plyr)

#load eddyco data, first looking at D16 plots

zipsByProduct(dpID = "DP4.00200.001", package = "expanded",
              site=c("ABBY","WREF"),
              startdate = "2019-06", enddate = "2019-12",
              savepath = getwd(),
              check.size = F)

#configure the workind directory
wd <- paste0(getwd(),"/filesToStack00200")
flux <- stackEddy(filepath = wd, level = "dp04")
names(flux)

#break up headers into terms of interest and their descriptions
term <- unlist(strsplit(names(flux$WREF),split = ".", fixed = T))
Descriptions <- flux$objDesc[which(flux$objDesc$Object %in% term),] #ummm yeah...tutorial line of code, not super sure what this is
View(Descriptions) #view these descriptions in a nice table

#converting Time stamps from strings to R date-time
#best for consistancy to use start times for averaging period
timeB <- as.POSIXct(flux$WREF$timeBgn, 
                    format = "%Y-%m-%dT%H:%M:%S",
                    tz = "GMT")
flux$WREF <- cbind(timeB, flux$WREF)  #bind time columns and the flux column from WREF

turb <- flux$WREF$data.fluxCo2.turb.flux
#raw_flux <- flux$WREF$
View(head(turb))

# get percentage of NaN streams for the whole year
f_nan <- sum(turb == "NaN")
downtime <- (f_nan/length(turb))*100 #percent of downtime for the year

jan <- as.POSIXct()


#break it down into months
plot(flux$WREF$data.fluxCo2.nsae.flux~timeB,
     pch = 20, xlab = "Date", ylab = "CO2 Flux", main = "Jan Flux",
     xlim = c(as.POSIXct("2019-01-01", tx="GMT"),
              as.POSIXct("2019-01-31",tx="GMT")),
     ylim=c(-20,20), xaxt="n")
axis.POSIXct(1, x=timeB, format = "%Y-%m-%d %H:%M:%S")
