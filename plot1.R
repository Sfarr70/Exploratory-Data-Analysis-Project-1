## This exploratory analysis is to answer the question, "Have total emissions 
## from PM2.5 decreased in the United States from 1999 to 2008? The plot
## created by this script shows the total PM2.5 emissions from all
## sources for the years 1999,2002,2005, & 2008. There is a noticeable
## decrease in PM2.5 emissions from 7.4 million in 1999 to 
## 3.4 million in 2008.

## load libraries

library(tidyverse)
library(data.table)

## Read data

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Write to txt files for speed and space

write.table(SCC,"SCC.txt",row.names=FALSE)
write.table(NEI, "NEI.txt",row.names=FALSE)

## Read in Emissions and year columns of NEI file

yrtotals <- fread("NEI.txt", select = c("Emissions", "year"))

## Total emissions per year and divide by 1,000,000 for clear plot labels

byyr <- yrtotals %>%
        group_by(year) %>%
        summarise(emission_total = round(sum(Emissions)/1000000,3))

## Create plot & send it to a PNG file

png("plot1.png")

barplot(byyr$emission_total,main="PM2.5 Emission Totals in the US",
        xlab="Year",ylab="Total Emissions (in millions)",axes=FALSE,
        names.arg=c("1999","2002","2005","2008"), axis.lty=1, yaxs="i")
axis(2,at=c(0,2,4,6,8),tick=TRUE,lty=1)
mtext("From all sources", side=3,outer=FALSE)

graphics.off()
