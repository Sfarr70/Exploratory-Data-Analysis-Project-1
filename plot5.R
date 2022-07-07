## This exploratory analysis is to answer the question, "How have
## emissions from motor vehicle sources changed 
## from 1999–2008 in Baltimore City, MD (fips=="24510")?" 
## The plot created by this script shows the total PM2.5 
## emissions in Baltimore City from motor vehicle sources for the
## years 1998,2002,2005,&2008.
## The results indicate there was a drastic decrease between 1999
## & 2002; very little change between 2002 & 2005; and another drastic
## decrease between 2005 & 2008. Since there is no data for years
## other than those plotted, it's impossible to tell if those changes
## were sudden or if they were more gradual.


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

vehicles <- filter(vehicles, fips=="24510")


## Total emissions per year for all motor vehicle sources 

byyr <- vehicles %>%
        group_by(year) %>%
        summarise(emission_total = sum(Emissions))

## Create plot & send it to a PNG file

png("plot5.png")

barplot(byyr$emission_total,main="PM2.5 Emission Totals:Baltimore City, MD",
        xlab="Year",ylab="Total Emissions",axes=TRUE,
        names.arg=c("1999","2002","2005","2008"), axis.lty=1)

mtext("From Motor Vehicle Sources",side=3,outer=FALSE)

graphics.off()
