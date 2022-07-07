## This exploratory analysis is to answer the question, "Which city
## has seen greater changes to motor vehicle emissions over 
## time (from 1999–2008) - Baltimore City (fips=="24510") or
## LA County, CA (fips=="06037")?" 
## The plot created by this script shows the total PM2.5 
## emissions in Baltimore City & LA County from motor vehicle sources
## for years 1998,2002,2005,&2008.
## *Note: LA County totals have been scaled to 1=10 for plot
## readability.*
## The results show emissions remaining steady or decreasing for
## Baltimore City, while LA County totals increased until the
## period between 2005 & 2008. Baltimore City has seen the 
## greatest overall change from 1998 to 2008.


## load libraries

library(tidyverse)
library(data.table)

## Read data

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Write data to txt files. This allows us to read subsets of the
## dataframe into R - saving processing time and space.

write.table(SCC,"SCC.txt",row.names=FALSE)
write.table(NEI, "NEI.txt",row.names=FALSE)

## Clean environment. Keep SCC data. It is needed to determine the SCC 
## number for coal combustion related sources
rm(NEI)

## Review SCC data to see what variable(s) indicate motor vehicle
## sources of PM2.5 
view(SCC)

## Data.Category==onroad is the only one that has data for motor vehicles.
## Motor vehicle sources are also the only data for onroad.
## Use that to get an index of SCC numbers 

sccindex <- grep("[O,o]nroad", SCC$Data.Category)

## Use sccindex to create character vector of SCC numbers

sccnums <- as.character(SCC[sccindex,1])

## Read SCC, Emissions, and year columns from NEI.txt.

subdb <- fread("NEI.txt",header=TRUE,select = c(1,2,4,6))


## Filter SCC numbers that match those in sccnums vector

vehicles <- filter(subdb, SCC %in% sccnums)

## Filter for Baltimore City (fips=="24510")

BC_LAdata <- filter(vehicles, fips=="24510" | fips=="06037")


## Total emissions per year for all motor vehicle sources for Bcdata 

BC_LAbyyr <- BC_LAdata %>%
        group_by(year,fips) %>%
        summarise(emission_total = sum(Emissions))

## Create subsets for LA County & BC emission totals 

LAdata <- BC_LAbyyr %>%
        filter(fips=="06037") %>%
        select ("emission_total") %>%
        mutate(emission_total=emission_total/10) ## scaled for plot
                                                 ## readability

BCdata <- BC_LAbyyr %>%
        filter(fips=="24510") %>%
        select("emission_total")


yr <- c(1999,2002,2005,2008)


## Create plot & send it to a PNG file

png("plot6.png")

plot(yr,BCdata$emission_total,type="l",main="Motor Vehicle PM2.5 Emissions",
       sub="Note:LA County totals are scaled at 1=10 for plot readability",
       cex.sub=.75,col="red", xlab="Year", ylab="Total_Emissions",
       xaxt="n", ylim=c(0,900))
lines(yr,LAdata$emission_total,type="l",col="blue")
axis(1, at=yr)
mtext("For LA County & Baltimore City",side=3)
legend("bottomleft",legend=c("LA County","Baltimore City"),lwd=2,
       col=c("blue","red"),bty="n")


graphics.off()
