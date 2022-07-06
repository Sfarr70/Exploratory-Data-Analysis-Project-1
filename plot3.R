## This exploratory analysis is to answer the question, "Of the four 
## types of sources indicated by the type (point, nonpoint, onroad, 
## nonroad) variable, which of these four sources have seen decreases
## in emissions from 1999–2008 for Baltimore City? Which have seen 
## increases in emissions from 1999–2008?" 
## Baltimore City,MD (fips =="24510")
## The plot created by this script shows the total PM2.5 
## emissions by type for the years 1999 & 2008 in Baltimore City.
## Non-road, Nonpoint, & On-road emissions all show a decrease. but
## point shows a slight increase.


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

## Clean environment
rm(NEI, SCC)

## Read fips, Emissions, and year columns from NEI.txt. Baltimore 
## City fips is"24510". This code skips rows until it
## reads the first instance of "24510". The entire file is read after
## that point, so it will be necessary to filter the rows for "24510".

subdb <- fread("NEI.txt",skip="24510",select = c(1,4,5,6))

## Add column names
var <- c("fips","Emissions","type","year")
names(subdb) <- var

## Filter Baltimore City rows ("24510")

BCdata <- subdb %>% filter(fips=="24510")


## Total emissions by type per year 

byyr <- BCdata %>%
        group_by(year,type) %>%
        summarise(emission_total = sum(Emissions))

## Filter years 1999 & 2008 for plotting

byyr <- byyr %>% filter(year == "1999" | year == "2008" )

## Create plot & send it to a PNG file

png("plot3.png")

qplot(year, emission_total, data=byyr, color=type, geom=c("point","line"))


graphics.off()
