## This exploratory analysis is to answer the question, "Have total emissions 
## from PM2.5 decreased in Baltimore City,MD (fips =="24510") from 1999
## to 2008? The plot created by this script shows the total PM2.5 
## emissions from all sources for the years 1999,2002,2005, & 2008 in
## Baltimore City. 


## load libraries

library(tidyverse)
library(data.table)

## Read data

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Write to txt files for speed and space

write.table(SCC,"SCC.txt",row.names=FALSE)
write.table(NEI, "NEI.txt",row.names=FALSE)

## Read Emissions and year columns from NEI.txt. Baltimore 
## City fips is"24510", so the code skips rows until it
## reads the first instance of "24510". The entire file is read after
## that point. It will be necessary to filter the rows for "24510".

subdb <- fread("NEI.txt",skip="24510",select = c(1,4,6))

## Add column names
var <- c("fips","Emissions","year")
names(subdb) <- var

## Filter Baltimore City rows ("24510")

BCdata <- subdb %>% filter(fips=="24510")


## Total emissions per year 

byyr <- BCdata %>%
        group_by(year) %>%
        summarise(emission_total = sum(Emissions))

## Create plot & send it to a PNG file

png("plot2.png")

barplot(byyr$emission_total,main="PM2.5 Emission Totals",
        xlab="Year",ylab="Total Emissions",axes=TRUE,
        names.arg=c("1999","2002","2005","2008"), axis.lty=1)
mtext("Baltimore City, MD", side=3,outer=FALSE)

graphics.off()
