##install required packages
install.packages("neonUtilities")
install.packages("tidyverse")
install.packages("BiocManager")
BiocManager::install('rhdf5')

##strings not automatically assigned to factor and load packages
options(stringsAsFactors=F)
library(neonUtilities)
library(ggplot2)
library(tidyverse)
library(lubridate)

##Download eddy co bundled data package
zipsByProduct(dpID="DP4.00200.001", package="expanded",
                site=c("TREE"),
                startdate="2019-01", enddate="2019-12",
                savepath="C:/Users/cslemmons/Documents/ExploreNEON",
                check.size=F)

##Stack and save to list of data frames
flux <- stackEddy("C:/Users/cslemmons/Documents/ExploreNEON/filesToStack00200",
                  level = "dp04")

##save to data frame
TREEflux <- flux$TREE

##Save flux data frame locally
saveRDS(TREEflux, "C:/Users/cslemmons/Documents/ExploreNEON/filesToStack00200/TREEflux.rds")

##Load flux data
TREEflux <- readRDS(file="C:/Users/cslemmons/Documents/ExploreNEON/filesToStack00200/TREEflux.rds")

##Convert time stamp to R date - time format, add to data frame
TREEflux$TimeB <- as.POSIXct(TREEflux$timeBgn,
                      format="%Y-%m-%dT%H:%M:%S",
                      tz="GMT")

##Add year and month column for summary
TREEflux$YearMonth <- substr(TREEflux$TimeB,0,7)

##Add year, month, day column for summary
TREEflux$YearMonthDay = substr(TREEflux$TimeB,0,10)

##Evaluate fluxCor and fluxRaw column for NAs, calculate % NAs by month
TREEflux %>%
  mutate(FluxCorNA=is.na(data.fluxCo2.turb.fluxCor)) %>%
  mutate(FluxRawNA=is.na(data.fluxCo2.turb.fluxRaw)) %>%
  group_by(YearMonth) %>%
  summarise(FluxCorNAPercent = mean(FluxCorNA), FluxRawNAPercent = mean(FluxRawNA))

##calculate difference between raw and corrected flux and calculate mean difference grouped by month
TREEflux %>%
  mutate(DiffCor=data.fluxCo2.turb.fluxRaw-data.fluxCo2.turb.fluxCor) %>%
  group_by(YearMonth) %>%
  summarise(DiffCor = mean(DiffCor, na.rm=TRUE))

##calculate difference between raw and corrected flux and calculate mean difference grouped by day
TREEflux %>%
  mutate(DiffCor=data.fluxCo2.turb.fluxRaw-data.fluxCo2.turb.fluxCor) %>%
  group_by(YearMonthDay) %>%
  summarise(DiffCor = mean(DiffCor, na.rm=FALSE))

TREEflux$DiffCor <- TREEflux$data.fluxCo2.turb.fluxRaw - TREEflux$data.fluxCo2.turb.fluxCor

##plot raw and corrected turbulent flux data for whole dataset
plot(TREEflux$data.fluxCo2.turb.fluxRaw,
     col="red", pch=".", xlab="Date", ylab="CO2 flux",
     xaxt="n", main="2019 TREE Turbulent Flux")
axis.POSIXct(1, x=Treeflux$TimeB, format="%Y-%m-%d")

points(TREEflux$data.fluxCo2.turb.fluxCor,
       col="green", pch="."
        )

legend("bottomright", c("rawFlux","corrFlux"), fill = c("red", "green"), bty = "n", y.intersp = 0.7)

##plot difference between raw and corrected flux
plot(TREEflux$DiffCor,
     col="red", pch=".", xlab="Date", ylab="CO2 flux",
     xlim=c(as.POSIXct("2019-01-01", tz="GMT"),
            as.POSIXct("2019-06-30", tz="GMT"),
     xaxt="n", main="2019 TREE Turbulent Flux"),
axis.POSIXct(1, x=TREEflux$TimeB, format="%Y-%m-%d"))

##try ggplot
ggplot(TREEflux, aes(x=TimeB)) +geom_point(aes(y=data.fluxCo2.turb.fluxRaw, color="red")) +geom_point(aes(y=data.fluxCo2.turb.fluxCor, color="green"))+ labs(title="2019 TREE Turbulent Flux", x="Date", y="umol CO2 m2/s-1")

qplot(x=TREEflux$TimeB, y=TREEflux$data.fluxCo2.turb.fluxRaw,
      xlab="Date", ylab="CO2 flux", color=TREEflux$data.fluxCo2.turb.fluxRaw,
      main="2019 TREE Turbulent Flux")

##plot corrected turbulent flux for Jan-June 2019 and + or -20
plot(flux$TREE$data.fluxCo2.turb.fluxRaw~timeB,
     col="red", pch=".", xlab="Date", ylab="CO2 flux",
     xlim=c(as.POSIXct("2019-01-01", tz="GMT"),
            as.POSIXct("2019-06-30", tz="GMT")),
     ylim=c(-20,20), xaxt="n")
axis.POSIXct(1, x=timeB, format="%Y-%m-%d %H:%M:%S")

points(flux$TREE$data.fluxCo2.turb.fluxCor~timeB,
       col="green", pch="."
)

legend("bottomright", c("rawFlux","corrFlux"), fill = c("red", "green"), bty = "n", y.intersp = 0.7)

##save to CSV
savefilename <- "C:/Users/cslemmons/Documents/ExploreNEON/filesToStack00200/TREEFlux.csv"
write.table(TREEflux, "C:/Users/cslemmons/Documents/ExploreNEON/filesToStack00200/TREEFlux.csv", sep=",")

