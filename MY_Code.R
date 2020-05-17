library(dplyr)
library(ggplot2)
library(reshape2)

####################################
#### ||||| 1.LOADING DATA ||||| ####
####################################

FileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
NameFile <- "StormData.csv"

if (!file.exists(NameFile)) {
  download.file(FileUrl, NameFile, mode = "wb")
}

data <- read.csv(NameFile, stringsAsFactors = FALSE)

#####################################
#### ||||| 2.CLEANING DATA ||||| ####
#####################################

Names <- c("STATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", 
"CROPDMG", "CROPDMGEXP")

subset <- data[, Names]

subset$event <- ("-")

events <- data.frame(reg = c("NADO|FUNNEL|WATERSPOUT", "THUNDER|STORM|WIND", 
 "HAIL", "FROST|FREEZ|BLIZZARD|WINTER|COLD|LOW TEMP|RECORD LOW|SNOW|ICE", 
  "HEAT|WARM|RECORD HIGH", "COSTAL STORM", "SUNAMI", "RIP CURRENT", "FLASH FLOOD|FLD|FLOOD", 
  "RIVER FLOOD|URBAN FLOOD", "TROPICAL STORM|TROPICAL", "HURRICANE", "DROUGHT", 
  "DUST STORM", "DUST DEVIL", "RAIN", "LIGHTNING"))

replace <- c("Tornado", "Thunderstorm wind", "Hail", "Cold", "Heat", "Costal Storm", 
  "Sunami", "Rip current", "Flash flood", "River flood", "Tropical Storm", 
  "Hurricane", "Drought", "Dust storm", "Dust devil", "Rain", "Ligntning")

for (i in 1:nrow(events)) {
  indexFit <- grep(events[i, "reg"], toupper(subset[, "EVTYPE"]))
  if (length(indexFit) > 0) {
    subset[indexFit, "event"] <- replace[i]
  }
}

otherIndex <- grep("-", subset[, "event"])
subset[otherIndex, "event"] <- "Other"
subset$event <- as.factor(subset$event)
subset$cropdmgvalue <- subset$cropdmg
subset$propdmgvalue <- subset$propdmg

###########################################
#### ||||| 3.PROPERTY AND DAMAGE ||||| ####
###########################################

values <- data.frame(cha = c("B", "M", "K", "H"), val = c(1e+09, 1e+06, 1000, 
                                                        100))
num <- c(1e+09, 1e+06, 1000, 100)

for (i in 1:nrow(values)) {
  # index that match the values
  j <- grep(values[i, "cha"], toupper(subset[, "CROPDMGEXP"]))
  
  if (length(j) > 0) {
    
    # Caculate the actual value
    subset[j, "cropdmgvalue"] <- num[i] * subset[j, 
                                                      "CROPDMG"]
    
  }
  
  # Same procudure for property damage
  indexProdFit <- grep(values[i, "cha"], toupper(subset[, "PROPDMGEXP"]))
  
  if (length(indexProdFit) > 0) {
    
    subset[indexProdFit, "propdmgvalue"] <- num[i] * subset[indexProdFit, 
                                                      "PROPDMG"]
  }
}

subset$ecodmgvalue <- subset$cropdmgvalue + subset$propdmgvalue
subset$pephealthdmg <- subset$INJURIES + subset$FATALITIES

###################################
#### ||||| 4.AGGREGATION ||||| ####
###################################

table <- aggregate(cbind(propdmgvalue, cropdmgvalue, INJURIES, FATALITIES) ~ event, 
               subset, FUN = sum)
tt <- melt(table, id.var = "event", variable.name = "variable")
colnames(tt) <- c("event", "damagetype", "value")

dmg <- tt[grep("FATALITIES|INJURIES", tt[, "damagetype"]), ]

##################################################
#### ||||| 5.INFLUENTS ON PEOPLE HEALTH ||||| ####
##################################################

dev.new()
par(mfcol = c(1,1))

ggplot(dmg, aes(x = reorder(event, value), y = value, fill = factor(damagetype, 
  labels = c("INJURIES", "FATALITIES")))) + geom_bar(stat = "identity") + labs(title = "Top harmful weather event for populaiton health", 
  x = "Event", y = "Damage on populaiton health (number of people)") + scale_fill_manual(values = c("#f20f0f", 
  "#000000")) + guides(fill = guide_legend(title = "Type of damage")) + 
  theme(axis.text = element_text(size = 12, colour = "#1F4178"), axis.title = element_text(size = 14, 
  colour = "#3A3E42", face = "bold"), title = element_text(size = 16, 
  colour = "#282B2E", face = "bold")) + coord_flip()

dev.copy(png, file="plot1.png", height=480, width=480,units="px",bg="transparent")
dev.off()

dmg2 <- tt[grep("cropdmgvalue|propdmgvalue", tt[, "damagetype"]), ]

#############################################
#### ||||| 5.INFLUENTS ON ECONOMIC ||||| ####
#############################################

dev.new()
par(mfcol = c(1,1))

ggplot(dmg2, aes(x = reorder(event, value), y = value, fill = factor(damagetype, 
  labels = c("Property", "Crop")))) + geom_bar(stat = "identity") + labs(title = "Top harmful weather event for economy ", 
  x = "Event", y = "Damage on economic value (US dollors)") + scale_fill_manual(values = c("#f2f40f", 
  "#407D47")) + guides(fill = guide_legend(title = "Type of damage")) +
  theme(axis.text = element_text(size = 12, colour = "#1F4178"), axis.title = element_text(size = 14, 
  colour = "#3A3E42", face = "bold"), title = element_text(size = 16, colour = "#282B2E", face = "bold")) + coord_flip()

dev.copy(png, file="plot2.png", height=480, width=480,units="px",bg="transparent")
dev.off()