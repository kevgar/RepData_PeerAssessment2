rm(list=ls())
library("data.table"); library("dplyr"); library("ggplot2")

f <- file.path(getwd(),'2FStormData.csv.bz2')
url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
download.file(url, f, mode="wb") 
DT_storm <- data.table(read.csv(f))

## Select relevant subset of the data
dt <- DT_storm %>% select(EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)

## Data Cleansing
# Replace empty values in dt with 0's
dt[FATALITIES == "", FATALITIES := "0"]
dt[INJURIES == "", INJURIES := "0"]
dt[PROPDMG == "", PROPDMG := "0"]
dt[CROPDMG == "", CROPDMG := "0"]
# Normalize damage values
dt[PROPDMGEXP == "", PROPDMGEXP := "0"]
dt[PROPDMGEXP == "+" | PROPDMGEXP == "-" | PROPDMGEXP == "?", PROPDMGEXP := "1"]
dt[PROPDMGEXP == "h" | PROPDMGEXP == "H", PROPDMGEXP := "2"]
dt[PROPDMGEXP == "k" | PROPDMGEXP == "K", PROPDMGEXP := "3"]
dt[PROPDMGEXP == "m" | PROPDMGEXP == "M", PROPDMGEXP := "6"]
dt[PROPDMGEXP == "B", PROPDMGEXP := "9"]
dt[CROPDMGEXP == "", CROPDMGEXP := "0"]
dt[CROPDMGEXP == "+" | CROPDMGEXP == "-" | CROPDMGEXP == "?", CROPDMGEXP := "1"]
dt[CROPDMGEXP == "h" | CROPDMGEXP == "H", CROPDMGEXP := "2"]
dt[CROPDMGEXP == "k" | CROPDMGEXP == "K", CROPDMGEXP := "3"]
dt[CROPDMGEXP == "m" | CROPDMGEXP == "M", CROPDMGEXP := "6"]
dt[CROPDMGEXP == "B", CROPDMGEXP := "9"]
# Verify normalized values look ok
dt %>% select(PROPDMGEXP) %>% distinct() %>% setorder(PROPDMGEXP) %>% data.table()
dt %>% select(CROPDMGEXP) %>% distinct() %>% setorder(CROPDMGEXP) %>% data.table()
# Recode values as numeric for computation of next step
dt[,PROPDMGEXP := as.integer(PROPDMGEXP)]
dt[,CROPDMGEXP := as.integer(CROPDMGEXP)]

## Calculate the Total Damage for each weather event
dt[, damageUSD := PROPDMG * 10^(PROPDMGEXP) + CROPDMG * 10^(CROPDMGEXP)] 

## Aggregating the data
# Calculating total `FATALITIES` by `EVTYPE`
dt <- group_by(dt, EVTYPE) #group data
fatalities <- dt %>% summarize(FATALITIES=sum(FATALITIES))
summarise(ungroup(dt), sum(FATALITIES)) #ungroup data
# Calculating total `INJURIES` by `EVTYPE`
dt <- group_by(dt, EVTYPE) #group data
injuries <- dt %>% summarize(INJURIES=sum(INJURIES))
summarise(ungroup(dt), sum(INJURIES)) #ungroup data
# Calculating total `damageUSD` by `EVTYPE`
dt <- group_by(dt, EVTYPE) #group data
damages <- dt %>% summarize(DAMAGES.USD=sum(damageUSD))
summarise(ungroup(dt), sum(damageUSD)) #ungroup data

## Preparing data for results
# Identifying top event types by fatalities
topFatalities <- fatalities %>% setorder(-FATALITIES) %>% head(10)
# Identifying top event types by injuries
topInjuries <- injuries %>% setorder(-INJURIES) %>% head(10)
# Identifying top event types by damages
topDamages <- damages %>% setorder(-DAMAGES.USD) %>% head(10)

## Results 
# Plotting total `FATALITIES` by `EVTYPE` for top 10 event types
par(las=2) # make label text perpendicular to axis
par(mar=c(5,13,4,2)) # increase y-axis margin.
with(topFatalities,
     barplot(FATALITIES, 
             main="Total Fatalities for Top 10 Event Types", 
             horiz=TRUE,names.arg = EVTYPE))
# Using ggplot
ggplot(data = topFatalities, 
       aes(x = topFatalities$EVTYPE, y = topFatalities$FATALITIES)) + 
        geom_bar(stat = "identity") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        xlab("Event type") + 
        ylab("Number of fatalities") + 
        ggtitle("Total Fatalities by Weather Event Type, \n Top 10 Event Types, 1950-2011")
# Plotting total `INJURIES` by `EVTYPE` for top 10 event types
par(las=2) # make label text perpendicular to axis
par(mar=c(5,13,4,2)) # increase y-axis margin.
with(topInjuries,
     barplot(INJURIES, 
             main="Total Injuries for Top 10 Event Types", 
             horiz=TRUE,names.arg = EVTYPE))
# Using ggplot
ggplot(data = topInjuries, 
       aes(x = topInjuries$EVTYPE, y = topInjuries$INJURIES)) + 
        geom_bar(stat = "identity") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        xlab("Event type") + 
        ylab("Number of injuries") + 
        ggtitle("Total Injuries by Weather Event Type, \n Top 10 Event Types, 1950-2011")
# Plotting total `DAMAGES.USD` by `EVTYPE` for top 10 event types
par(las=2) # make label text perpendicular to axis
par(mar=c(5,13,4,2)) # increase y-axis margin.
with(topDamages,
     barplot(log(DAMAGES.USD), 
             main="Total Damages (USD) for Top 10 Event Types", 
             horiz=TRUE,names.arg = EVTYPE))
#Using ggplot
ggplot(data = topDamages,
       aes(x = topDamages$EVTYPE, y = topDamages$DAMAGES.USD)) + 
        geom_bar(stat = "identity") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        xlab("Event type") + 
        ylab("Damages (USD)") + 
        ggtitle("Total Damages by Weather Event Type, \n Top 10 Event Types, 1950-2011")

       