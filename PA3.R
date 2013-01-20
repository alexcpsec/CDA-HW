### Programming Assignment 3

# Question 1
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11],main="Heart Attack 30-day Death Rate", xlab="30-day Death Rate")

# Question 2
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[,11] <- as.numeric(outcome[,11])
outcome[,17] <- as.numeric(outcome[,17])
outcome[,23] <- as.numeric(outcome[,23])
par(mfrow = c(1,3))
#Heart Attack
hist(outcome[,11],
     main=substitute(`Heart Attack 30-day Death Rate (`~ bar(x) == k ~`)`,
                     list(k=mean(outcome[,11], na.rm=TRUE))),
     xlab="30-day Death Rate", 
     xlim=range(outcome[,11], outcome[,17], outcome[,23], na.rm= TRUE),
     prob = TRUE)
lines(density(outcome[,11], na.rm=TRUE), lwd=2)
abline(v=median(outcome[,11], na.rm=TRUE), lwd=3)
#Heart Failure
hist(outcome[,17],
     main=substitute(`Heart Failure 30-day Death Rate (`~ bar(x) == k ~`)`,
                     list(k=mean(outcome[,17], na.rm=TRUE))),
     xlab="30-day Death Rate",
     xlim=range(outcome[,11], outcome[,17], outcome[,23], na.rm= TRUE),
     prob = TRUE)
lines(density(outcome[,17], na.rm=TRUE), lwd=2)
abline(v=median(outcome[,17], na.rm=TRUE), lwd=3)
#Pneumonia
hist(outcome[,23],
     main=substitute(`Pneumonia 30-day Death Rate (`~ bar(x) == k ~`)`,
                     list(k=mean(outcome[,23], na.rm=TRUE))),
     xlab="30-day Death Rate",
     xlim=range(outcome[,11], outcome[,17], outcome[,23], na.rm= TRUE),
     prob = TRUE)
lines(density(outcome[,23], na.rm=TRUE), lwd=2)
abline(v=median(outcome[,23], na.rm=TRUE), lwd=3)

### Question 3
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[,11] <- as.numeric(outcome[,11])

# Setting Data
boolState <- table(outcome$State) > 20
outcome2 <- subset(outcome, boolState[outcome$State])
death <- outcome2[,11]
state <- outcome2$State
stateTable <- table(outcome2$State)

# Calulating the means
stateSplit <- split(death,state)
stateMeans <- sort(sapply(stateSplit, median, na.rm = TRUE))

# Drawing the graph
par(mfrow = c(1,1))
par(las=2)
par(cex.axis=0.6)
boxplot( stateSplit[names(stateMeans)],
         main = "Heart Attack 30-day Death Rate by State",
         ylab = "30-day Death Rate",
         xaxt = "n")
axis(1, at=1:length(stateMeans),
     sprintf("%s (%d)", names(stateMeans), stateTable[names(stateMeans)]))

### Question 4

#Preparing Data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
hospital <- read.csv("hospital-data.csv", colClasses = "character")
outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")

death <- as.numeric(outcome.hospital[, 11]) ## Heart attack outcome
npatient <- as.numeric(outcome.hospital[, 15])
owner <- factor(outcome.hospital$Hospital.Ownership)

# Plotting the data
library(lattice)
xyplot(death ~ npatient | owner,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.lmline(x, y, col = 3)
       },
       xlab = "Number of Patients Seen",
       ylab = "30-day Death Rate",
       main = "Heart Attack 30-day Death Rate by Ownership")



