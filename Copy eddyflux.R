devtools::install_github('NEONScience/NEON-utilities/neonUtilities', ref='master')


flux <- stackEddy(filepath="C:/Users/aliso/Documents/Git and R Projects/Other projects/filesToStack00200", level="dp04")
#no longer get the error I wrote about in my daily notes but nonw stackeddy could not be found

install.packages("neonUtilities")
##don't need to install package, just make sure neonUtil is checked in packages on the right
flux <- stackEddy(filepath="C:/Users/aliso/Documents/Git and R Projects/Other projects/filesToStack00200", level="dp04")
## getting error about number of columns of arguments do not match
## retried 20200415 and it worked! no errors, make sure neonUtil is loaded

library(neonUtilities)
names(flux)
timeB <- as.POSIXct(flux$ONAQ$timeBgn, format="%Y-%m-%dT%H:%M:%S", tz="GMT")
flux$ONAQ <- cbind(timeB, flux$ONAQ)


#plot nsae flux for fun


plot(flux$ONAQ$data.fluxCo2.nsae.flux~timeB,pch=".", xlab = "Date", ylab = "CO2 flux", xaxt="n")
axis.POSIXct(1, x=timeB, format="%Y-%m-%d")

#plot turb flux
plot(flux$ONAQ$data.fluxCo2.turb.flux~timeB,pch=".", xlab = "Date", ylab = "CO2 flux", xaxt="n")
axis.POSIXct(1, x=timeB, format="%Y-%m-%d")


#different syntax to plot turb flux

plot(timeB, flux$ONAQ$data.fluxCo2.turb.flux, pch=".", xlab = "Date", ylab = "CO2 flux", xaxt="n")
axis.POSIXct(1,x=timeB, format = "%Y-%m-%d")

#plot turb flux, fluxRaw, fluxCor on same graph. run all lines as same time
###RUN THIS WHOLE SECTION TO GET GRAPH###
library(neonUtilities)
flux <- stackEddy(filepath="C:/Users/aliso/Documents/Git and R Projects/Other projects/filesToStack00200", level="dp04")
timeB <- as.POSIXct(flux$ONAQ$timeBgn, format="%Y-%m-%dT%H:%M:%S", tz="GMT")
flux$ONAQ <- cbind(timeB, flux$ONAQ)
turb <- flux$ONAQ$data.fluxCo2.turb.flux
fRaw <- flux$ONAQ$data.fluxCo2.turb.fluxRaw
fCor <- flux$ONAQ$data.fluxCo2.turb.fluxCor
plot(timeB, turb, pch=".", xlab = "Date", ylab = "CO2 flux", col="red", main = "2019 ONAQ CO2 Flux")
points(timeB, fRaw, pch=".", col="green")
points(timeB, fCor, pch=".", col="blue")
legend("bottomright", c("fRaw","fCor"), fill = c("green", "blue"), bty = "n", y.intersp = 0.7)


#green showing on graph means the raw data was not corrected?
#green areas correspond to NaN in HDF viewer


###^^^ RUN THIS WHOLE SECTION TO GET GRAPH ^^^####
#yellow showing on graph means the raw data was not corrected. checked, and corresponds to NaN in HDF viewer


obsCor <- flux[["ONAQ"]][["data.fluxCo2.turb.fluxCor"]]
obsCor  #all of the observed Cor values
obsRaw <- flux[["ONAQ"]][["data.fluxCo2.turb.fluxRaw"]]
obsRaw  #all of the observed Raw values

Dataframe <- data.frame(timeB, obsCor, obsRaw)
library(plyr)

Jan <- grep("2019-01", timeB, value = T) 
Jan #All of January 1392 rows

JanDF <- Dataframe[grep("2019-01", timeB),]#got it!
JanRows <- nrow(JanDF)


NaNcount <- grep("NaN", obsCor)
NaNcount  #displays row # that has Nan in Cor column, 5747 total


NaNs <- sum(Dataframe$obsCor == "NaN") #5747 count of NaNs
NaNs
TotalRow <- nrow(Dataframe) #14640 total rows = total data points
TotalRow
#figure out how to do an easy percent of NaNs per month
(NaNs/TotalRow)*100   #39.2% downtime total for year


#there has to be a better way to get the percentage instead of month by month
#use loops?
JanDF <- Dataframe[grep("2019-01", timeB),]
JanRows <- nrow(JanDF)

NaNs <- sum(Dataframe$obsCor == "NaN") #Got it! 5747
TotalRow <- nrow(Dataframe) #14640
#figure out how to do an easy percent of NaNs per month
(NaNs/TotalRow)*100   #39.2% downtime total for year

JanNaNs <- sum(JanDF$obsCor == "NaN")
JanNaNs
P01 <- ((JanNaNs/JanRows)*100)#18.46% downtime in Jan

FebDF <- Dataframe[grep("2019-02", timeB),]
FebRows <- nrow(FebDF)
FebNaNs <- sum(FebDF$obsCor == "NaN")
P02 <- (FebNaNs/FebRows)*100   #7.716% downtime in Feb

{MarDF <- Dataframe[grep("2019-03", timeB),]
MarRows <- nrow(MarDF)
MarNaNs <- sum(MarDF$obsCor == "NaN")
P03 <- (MarNaNs/MarRows)*100}  #2.546% downtime in Mar

{AprDF <- Dataframe[grep("2019-04", timeB),]
  AprRows <- nrow(AprDF)
  AprNaNs <- sum(AprDF$obsCor == "NaN")
  P04 <- (AprNaNs/AprRows)*100}

{MayDF <- Dataframe[grep("2019-05", timeB),]
  MayRows <- nrow(MayDF)
  MayNaNs <- sum(MayDF$obsCor == "NaN")
  P05 <- (MayNaNs/MayRows)*100}

{JunDF <- Dataframe[grep("2019-06", timeB),]
  JunRows <- nrow(JunDF)
  JunNaNs <- sum(JunDF$obsCor == "NaN")
  P06 <- (JunNaNs/JunRows)*100}

{JulDF <- Dataframe[grep("2019-07", timeB),]
  JulRows <- nrow(JulDF)
  JulNaNs <- sum(JulDF$obsCor == "NaN")
  P07 <- (JulNaNs/JulRows)*100}

{AugDF <- Dataframe[grep("2019-08", timeB),]
  AugRows <- nrow(AugDF)
  AugNaNs <- sum(AugDF$obsCor == "NaN")
  P08 <- (AugNaNs/AugRows)*100}

{SepDF <- Dataframe[grep("2019-09", timeB),]
  SepRows <- nrow(SepDF)
  SepNaNs <- sum(SepDF$obsCor == "NaN")
  P09 <- (SepNaNs/SepRows)*100}

{OctDF <- Dataframe[grep("2019-10", timeB),]
  OctRows <- nrow(OctDF)
  OctNaNs <- sum(OctDF$obsCor == "NaN")
  P10 <- (OctNaNs/OctRows)*100}

{NovDF <- Dataframe[grep("2019-11", timeB),]
  NovRows <- nrow(NovDF)
  NovNaNs <- sum(NovDF$obsCor == "NaN")
  P11 <- (NovNaNs/NovRows)*100}

{DecDF <- Dataframe[grep("2019-12", timeB),]
  DecRows <- nrow(DecDF)
  DecNaNs <- sum(DecDF$obsCor == "NaN")
  P12 <- (DecNaNs/DecRows)*100}


PercentTable <- data.frame("Month" = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                           "Percent Down Time" = c(P01, P02, P03, P04, P05, P06, 
                                                   P07, P08, P09, P10, P11, P12))
PercentTable <- data.frame("Month" = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                           "Percent Down Time" = c(P01, P02, P03, P04, P05, P06, P07, P08, P09, P10, P11, P12))


barplot(PercentTable$Percent.Down.Time, xlab="Month", ylab="Percentage", 
        names.arg = PercentTable$Month, col = "blue", cex.names = .4, 
        main = "ONAQ_Validation_System_Percent_Down_Time", cex.main = .6,
        cex.lab = .8)

