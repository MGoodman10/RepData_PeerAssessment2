rawfile <- paste(getwd(),"/repro_p_2", "/", "StormData.csv.bz2",
                  sep="")
csvfile <- paste(getwd(),"/repro_p_2", "/", "StormData.csv",
                 sep="")

# download file from URL
if (!file.exists(rawfile)) {
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                      rawfile)
}
# unzip file
if (!file.exists(csvfile)) {
        library(R.utils)
        bunzip2(rawfile, csvfile, remove = FALSE)
}
# load data into R
weather.df <- read.csv(csvfile)

# subset the data to include only the colums being analyzed
weather.df <- weather.df[c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", 
                      "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

# Replace property exponent data
weather.df$PROPDMGEXP <- as.character(weather.df$PROPDMGEXP)
weather.df$PROPDMGEXP <- toupper(weather.df$PROPDMGEXP)
weather.df$PROPDMGEXP[weather.df$PROPDMGEXP == "H"] <- 100
weather.df$PROPDMGEXP[weather.df$PROPDMGEXP == "K"] <- 1000
weather.df$PROPDMGEXP[weather.df$PROPDMGEXP == "M"] <- 1e+06
weather.df$PROPDMGEXP[weather.df$PROPDMGEXP == "B"] <- 1e+09
weather.df$PROPDMGEXP[weather.df$PROPDMGEXP == ""] <- 1
weather.df$PROPDMGEXP[weather.df$PROPDMGEXP == "0"] <- 1
weather.df$PROPDMGEXP[weather.df$PROPDMGEXP == "1"] <- 10
weather.df$PROPDMGEXP[weather.df$PROPDMGEXP == "2"] <- 100
weather.df$PROPDMGEXP[weather.df$PROPDMGEXP == "3"] <- 1000
weather.df$PROPDMGEXP[weather.df$PROPDMGEXP == "4"] <- 10000
weather.df$PROPDMGEXP[weather.df$PROPDMGEXP == "5"] <- 1e+05
weather.df$PROPDMGEXP[weather.df$PROPDMGEXP == "6"] <- 1e+06
weather.df$PROPDMGEXP[weather.df$PROPDMGEXP == "7"] <- 1e+07
weather.df$PROPDMGEXP[weather.df$PROPDMGEXP == "8"] <- 1e+08

# assign 0 to invalid exponents, so they are eliminated from calculations
weather.df$PROPDMGEXP[weather.df$PROPDMGEXP == "+"] <- 0
weather.df$PROPDMGEXP[weather.df$PROPDMGEXP == "-"] <- 0
weather.df$PROPDMGEXP[weather.df$PROPDMGEXP == "?"] <- 0

# compute the property damage value
weather.df$PROPDMGEXP <- as.numeric(weather.df$PROPDMGEXP)
weather.df$PROPDMGVAL <- weather.df$PROPDMG * weather.df$PROPDMGEXP

# Replace crop exponent data
weather.df$CROPDMGEXP <- as.character(weather.df$CROPDMGEXP)
weather.df$CROPDMGEXP <- toupper(weather.df$CROPDMGEXP)
weather.df$CROPDMGEXP[weather.df$CROPDMGEXP == "K"] <- 1000
weather.df$CROPDMGEXP[weather.df$CROPDMGEXP == "M"] <- 1e+06
weather.df$CROPDMGEXP[weather.df$CROPDMGEXP == "B"] <- 1e+09
weather.df$CROPDMGEXP[weather.df$CROPDMGEXP == ""] <- 1
weather.df$CROPDMGEXP[weather.df$CROPDMGEXP == "0"] <- 1
weather.df$CROPDMGEXP[weather.df$CROPDMGEXP == "2"] <- 100

# assign 0 to invalid exponents, so they are eliminated from calculations
weather.df$CROPDMGEXP[weather.df$CROPDMGEXP == "?"] <- 0

# compute the property damage value
weather.df$CROPDMGEXP <- as.numeric(weather.df$CROPDMGEXP)
weather.df$CROPDMGVAL <- weather.df$CROPDMG * weather.df$CROPDMGEXP

#compute total damage
weather.df$TOTALDMGVAL <- weather.df$PROPDMGVAL + weather.df$CROPDMGVAL

# aggregate the data by event
fatal.byevent <- aggregate(FATALITIES ~ EVTYPE, data=weather.df, FUN=sum)
injury.byevent <- aggregate(INJURIES ~ EVTYPE, data=weather.df, FUN=sum)
propdmg.byevent <- aggregate(PROPDMGVAL ~ EVTYPE, data=weather.df, FUN=sum)
cropdmg.byevent <- aggregate(CROPDMGVAL ~ EVTYPE, data=weather.df, FUN=sum)
totaldmg.byevent <- aggregate(TOTALDMGVAL ~ EVTYPE, data=weather.df, FUN=sum)

# get top10 (plus other) events with highest fatalities
fatal.top10 <- fatal.byevent[order(-fatal.byevent$FATALITIES), ][1:10, ]
other.fatalities <- sum(fatal.byevent$FATALITIES) - sum(fatal.top10$FATALITIES)
fatal.other.df <- data.frame(EVTYPE="All Other Types", FATALITIES=other.fatalities)
fatal.top10 <- rbind(fatal.top10, fatal.other.df)

# get top10 (plus other) events with highest injuries
injury.top10 <- injury.byevent[order(-injury.byevent$INJURIES), ][1:10, ]
other.injury <- sum(injury.byevent$INJURIES) - sum(injury.top10$INJURIES)
injury.other.df <- data.frame(EVTYPE="All Other Types", INJURIES=other.injury)
injury.top10 <- rbind(injury.top10, injury.other.df)

# get top10 (plus other) events with highest property damage
propdmg.top10 <- propdmg.byevent[order(-propdmg.byevent$PROPDMGVAL), ][1:10, ]
other.propdmg <- sum(propdmg.byevent$PROPDMGVAL) - sum(propdmg.top10$PROPDMGVAL)
propdmg.other.df <- data.frame(EVTYPE="All Other Types",
                               PROPDMGVAL=other.propdmg)
propdmg.top10 <- rbind(propdmg.top10, propdmg.other.df)

# get top10 (plus other) events with highest crop damage
cropdmg.top10 <- cropdmg.byevent[order(-cropdmg.byevent$CROPDMGVAL), ][1:10, ]
other.cropdmg <- sum(cropdmg.byevent$CROPDMGVAL) - sum(cropdmg.top10$CROPDMGVAL)
cropdmg.other.df <- data.frame(EVTYPE="All Other Types",
                               CROPDMGVAL=other.cropdmg)
cropdmg.top10 <- rbind(cropdmg.top10, cropdmg.other.df)

# get top10 (plus other) events with highest total damage
totaldmg.top10 <- 
        totaldmg.byevent[order(-totaldmg.byevent$TOTALDMGVAL), ][1:10, ]
other.totaldmg <- sum(totaldmg.byevent$TOTALDMGVAL) - 
        sum(totaldmg.top10$TOTALDMGVAL)
totaldmg.other.df <- data.frame(EVTYPE="All Other Types",
                               TOTALDMGVAL=other.totaldmg)
totaldmg.top10 <- rbind(totaldmg.top10, totaldmg.other.df)

#plot fatalities & injuries
par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(fatal.top10$FATALITIES, las = 3,
        names.arg = fatal.top10$EVTYPE,
        main = "Weather Events With The Top 10 Highest Fatalities", 
        ylab = "Number of fatalities",
        col = "red")
barplot(injury.top10$INJURIES, las = 3, 
        names.arg = injury.top10$EVTYPE, 
        main = "Weather Events With the Top 10 Highest Injuries", 
        ylab = "Number of injuries", 
        col = "red")

#plot property & crop damage
par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(propdmg.top10$PROPDMGVAL/(10^9), las = 3,
        names.arg = propdmg.top10$EVTYPE, 
        main = "Top 10 Events with Greatest Property Damages", 
        ylab = "Cost of damages ($ billions)", 
        col = "red")
barplot(cropdmg.top10$CROPDMGVAL/(10^9), las = 3, 
        names.arg = cropdmg.top10$EVTYPE, 
        main = "Top 10 Events With Greatest Crop Damages", 
        ylab = "Cost of damages ($ billions)", 
        col = "red")

#plot total damage
par(mfrow = c(1, 1), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(totaldmg.top10$TOTALDMGVAL/(10^9), las = 3,
        names.arg = totaldmg.top10$EVTYPE, 
        main = "Top 10 Events with Greatest Property Damages", 
        ylab = "Cost of damages ($ billions)", 
        col = "red")