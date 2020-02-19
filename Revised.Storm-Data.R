## Firstly set Working directory to the designated folder.
getwd()
setwd("c://Users/chinks/Downloads/coursera/wk4/")
getwd()

## loading required packages for this assigment :

library(dplyr)
library(ggplot2)
## LOading and processing the data from the raw file :

Storm <- read.csv("C://Users/chinks/Downloads/coursera/wk4/repdata_data_StormData(1).csv")
head(Storm)
colnames(Storm)
summary(Storm)

## QUESTION 1: 

##  To answer the question not all the data is relevant, 
## hence narrowing down the data with the informaiton improtant to answer to question.

## a) Damage to Population Health by Storm events data:

Storm_Health <- Storm[ , c(8,23:24)]
head(Storm_Health)
dim(Storm_Health)
summary(Storm_Health)

## Aggregating the data  relating to the fatalities and injuries corresponding to the Storm events and 
## For addressing the question we will be looking at the Top 10 results.

## a) Fatalities:

Fatal <- aggregate(FATALITIES ~ EVTYPE, data = Storm_Health, sum)
head(Fatal)
tail(Fatal)

## Rearranging the data: 

Fatal <- Fatal[order(Fatal$FATALITIES, decreasing = TRUE), ]


head(Fatal,10)

## loading the data in to a new vector:

Storm_Fatal <- Fatal[1:10, ]

## looking at the Top 10 Fatalities:
Storm_Fatal

## b) Injuires

Injuries <- aggregate(INJURIES ~ EVTYPE, data = Storm_Health, sum)

head(Injuries)
## rearranging the data:

Injuries <- Injuries[order(-Injuries$INJURIES), ]

## loading the data into a new vector:
Storm_Injury <- Injuries[1:10, ]
Storm_Injury
## Creating the plot with the code optained from above:

## a) For individual FAtalities and Injuries 

par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(Storm_Fatal$FATALITIES, las = 3, names.arg = Storm_Fatal$EVTYPE, main = "FATALITIES VS Top 10 Storm Events", 
        ylab = "Number of fatalities", col = "Pink", ylim = c(0,6000))
barplot(Storm_Injury$INJURIES, las = 3, names.arg = Storm_Injury$EVTYPE, main = "Injuries VS Top 10 Storm Events", 
        ylab = "Number of injuries", col = "light blue", ylim = c(0,100000))

## b) Addressign the answer in a single plot:

head(Storm_Health)

## aggregating the data of Fatalities and Injuries correspond to the Storm events and 
## For answering the question we will be looking at the Top 10 results.

HEalth_Outcome <- aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE, data = Storm_Health, FUN = sum)

head(HEalth_Outcome)
HEalth_Outcome$People_Loss <- HEalth_Outcome$FATALITIES + HEalth_Outcome$INJURIES
head(HEalth_Outcome)
## rearranging the data:
HEalth_Outcome <- HEalth_Outcome[order(HEalth_Outcome$People_Loss, decreasing = TRUE), ]

head(HEalth_Outcome)

## Storing the top 10 results in the new vector and simplifying the data to create the plot :

Casualty <- HEalth_Outcome[1:10, ]
Casualty
colnames(Casualty)
Casualty[,-1]
x <- Casualty$EVTYPE
health <- as.matrix(t(Casualty[,-1]))
health
colnames(health) <- x
health

## Creating the plot using the code achieved from the above results:

par(mfrow= c(1,1), mar = c(12,8,4,2), cex = 0.6)
barplot(health, col = c("grey", "pink"),main = "Top 10 Harmful effects of Storm on Human Health", 
        ylim = c(0,200000), ylab = "Total number of Health Damage", cex.lab = 2, col.lab = "red",cex.axis = 0.8, cex.names = 0.9)
legend("topright",c("Fatalities","Injuries"), fill = c("grey", "pink"), bty = "n")


### QUESTION 2:

## NOw we will be looking at the data for the Economic damages caused by the Storm:

## Damages done to property and crops by Storm events:

Storm_Property <- Storm[ , c(8,25:28)]

head(Storm_Property)
colnames(Storm_Property)
summary(Storm_Property)
## Data looks a lot better now! We will now convert the PROPDMGEXP & 
## CROPDMGEXP fields to tangible numbers where H (hundreds = 10^2), K (thousands = 10^3)
## , M (millions = 10^6), and B (billions = 10^9) based on Wikipedia power of 10 table.

Storm_Property$PROPDMGEXP <- gsub("[Hh]", "2", Storm_Property$PROPDMGEXP)
Storm_Property$PROPDMGEXP <- gsub("[Kk]", "3", Storm_Property$PROPDMGEXP)
Storm_Property$PROPDMGEXP <- gsub("[Mm]", "6", Storm_Property$PROPDMGEXP)
Storm_Property$PROPDMGEXP <- gsub("[Bb]", "9", Storm_Property$PROPDMGEXP)
Storm_Property$PROPDMGEXP <- gsub("\\+", "1", Storm_Property$PROPDMGEXP)
Storm_Property$PROPDMGEXP <- gsub("\\?|\\-|\\ ", "0",  Storm_Property$PROPDMGEXP)
Storm_Property$PROPDMGEXP <- as.numeric(Storm_Property$PROPDMGEXP)

head(Storm_Property)
tail(Storm_Property)


## Applying the same process to the Crop data in the Dataset:

Storm_Property$CROPDMGEXP <- gsub("[Hh]", "2", Storm_Property$CROPDMGEXP)
Storm_Property$CROPDMGEXP <- gsub("[Kk]", "3", Storm_Property$CROPDMGEXP)
Storm_Property$CROPDMGEXP <- gsub("[Mm]", "6", Storm_Property$CROPDMGEXP)
Storm_Property$CROPDMGEXP <- gsub("[Bb]", "9", Storm_Property$CROPDMGEXP)
Storm_Property$CROPDMGEXP <- gsub("\\+", "1", Storm_Property$CROPDMGEXP)
Storm_Property$CROPDMGEXP <- gsub("\\-|\\?|\\ ", "0", Storm_Property$CROPDMGEXP)
Storm_Property$CROPDMGEXP <- as.numeric(Storm_Property$CROPDMGEXP)

Storm_Property$PROPDMGEXP[is.na(Storm_Property$PROPDMGEXP)] <- 0
Storm_Property$CROPDMGEXP[is.na(Storm_Property$CROPDMGEXP)] <- 0

Storm_Property <- mutate(Storm_Property, 
                         PROPDMGTOTAL = PROPDMG * (10 ^ PROPDMGEXP), 
                         CROPDMGTOTAL = CROPDMG * (10 ^ CROPDMGEXP))
head(Storm_Property)

## We will be answering the question in tow parts, firstly ww willbe looking at the data individually 
## at the property damage and the Crop damage and secondly looking at the data when combined.

## part 1:
## a) Property Damage: Aggrregating the data corresponding to the Storm events and for addressing 
## this question we will be looking at the Top 10 results.

St_PROPDMG <- aggregate(PROPDMGTOTAL ~ EVTYPE, data = Storm_Property, FUN = sum)
head(St_PROPDMG)
## Sorting the data to get the TOp 10 result:

St_PROPDMG <- St_PROPDMG[order(St_PROPDMG$PROPDMGTOTAL, decreasing = TRUE), ]

head(St_PROPDMG)
## Storing the Top 10 results in a new vector:

St_PROP <- St_PROPDMG[1:10, ]
St_PROP

## b) Crop Damage:Aggrregating the data corresponding to the Storm events and for addressing 
## this question we will be looking at the Top 10 results.

St_CROPDMG <- aggregate(CROPDMGTOTAL ~ EVTYPE, data = Storm_Property, FUN = sum)

# Sorting the data to get the TOp 10 result:

St_CROPDMG <- St_CROPDMG[order(St_CROPDMG$CROPDMGTOTAL, decreasing = TRUE), ]
head(St_CROPDMG)

## Storing the Top 10 results in a new vector:

St_CROP <- St_CROPDMG[1:10, ]
St_CROP
## Creating the PLot using the code achieved from the above data:

par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(St_PROP$PROPDMGTOTAL/10^9, names.arg = St_PROP$EVTYPE, col = "pink", las = 2, main = "Total Property Damage by Storm",
        ylab = "Total Damage in USD(10^9)")
barplot(St_CROP$CROPDMGTOTAL/10^9, names.arg = St_CROP$EVTYPE, col = "light green", las = 2, main = "Total CROP Damage by Storm",
        ylab = "Total Damage in USD(10^9)")


## PArt 2: Addressing the question with combine results of Economic damage:

## Aggrregating the data corresponding to the Storm events and for addressing 
## this question we will be looking at the Top 10 results.
St_Both <- aggregate(cbind(PROPDMGTOTAL, CROPDMGTOTAL) ~ EVTYPE, data = Storm_Property, FUN = sum)

head(St_Both)

St_Both$Economic <- St_Both$PROPDMGTOTAL + St_Both$CROPDMGTOTAL
head(St_Both)
## sorting the data to achieve the Top results:
St_Both <- St_Both[order(St_Both$Economic, decreasing = TRUE), ]
## Selecting the Top 10 results for answering the Question:

St_BOTH <- St_Both[1:10, ]
St_BOTH

Storm_EcoEffect <- St_BOTH[, c("EVTYPE", "Economic")]

Storm_EcoEffect
## Creating the Plot to best describe the Economic damade caused by the Weather Effects:


par(mfrow= c(1,1), mar = c(12,8,4,0))
barplot(St_BOTH$Economic/10^9, names.arg = St_BOTH$EVTYPE, col = "orange", 
        las = 2, main = "Total Property and Crop Damage by Top 10 Storm Events",
        ylim = c(0,170), ylab = "Total Damage in USD(10^9)")


