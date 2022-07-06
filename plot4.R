## This exploratory analysis is to answer the question, "Across the
## how have emissions from coal combustion-related sources changed 
## from 1999–2008?" 
## The plot created by this script shows the total PM2.5 
## emissions in the US by coal combustion related sources for the
## years 1998,2002,2005,&2008.
## The results indicate there was very little change from
## 1998-2005 and a drastic decrease between 2005 & 2008. Since
## there is no data for '06-'07, it's impossible to tell if that
## was a sudden change or a gradual change.


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

## Get row index values for "Fuel Comb...-Coal" strings in the
## EI.Sector column of SCC

sccindex <- grep("^Fuel.*Coal$", SCC$EI.Sector)

## Use sccindex to create character vector of SCC numbers

sccnums <- as.character(SCC[sccindex,1])

## Read SCC, Emissions, and year columns from NEI.txt.

subdb <- fread("NEI.txt",header=TRUE,select = c(2,4,6))


## Filter SCC numbers that match those in sccnums vector

coal <- filter(subdb, SCC %in% sccnums)


## Total emissions per year for all coal combustion related sources 

byyr <- coal %>%
        group_by(year) %>%
        summarise(emission_total = sum(Emissions)/1000)

## Create plot & send it to a PNG file

png("plot4.png")

barplot(byyr$emission_total,main="PM2.5 Emission Totals in the US",
        xlab="Year",ylab="Total Emissions (in thousands)",axes=TRUE,
        names.arg=c("1999","2002","2005","2008"), axis.lty=1)

mtext("From Coal Combustion Related Sources",side=3,outer=FALSE)

graphics.off()
