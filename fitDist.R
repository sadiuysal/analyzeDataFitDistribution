#run this command before running: library(xlsx)
require(xlsx)
require(graphics)
iaData <- read.xlsx ("Assignment2-Interarrival_Data.xls", sheetIndex = 1)
# Omit NA values
interarrivalTime1 <- iaData$Day.1[!is.na(iaData$Day.1)]
interarrivalTime2 <- iaData$Day.2[!is.na(iaData$Day.2)]
#1
#find critical value for K-S test to check correctness
critVal1<-1.36/sqrt(length(interarrivalTime1))
critVal2<-1.36/sqrt(length(interarrivalTime2))
#get uniform dist
uniDist <- sort(runif(length(iaData$Day.1), min = 0, max = 400))
uniDist2 <- sort(runif(length(iaData$Day.2), min = 0, max = 400))
#apply K-S test for uniformity
ks.test(iaData$Day.1,uniDist)
ks.test(iaData$Day.2,uniDist2)

## 2 - Find descriptive statistics

meanD1 <- mean(iaData$Day.1, na.rm=TRUE)
meanD2 <- mean(iaData$Day.2, na.rm=TRUE)

sdD1 <- sd(iaData$Day.1, na.rm=TRUE)
sdD2 <- sd(iaData$Day.2, na.rm=TRUE)

maxD1 <- max(interarrivalTime1)
maxD2 <- max(interarrivalTime2)

## 3 - Plot frequency histograms

#5 sec intervals
hist(interarrivalTime1, breaks=(maxD1/5+1))
hist(interarrivalTime2, breaks=(maxD2/5+1))

#10 sec intervals
hist(interarrivalTime1, breaks=(maxD1/10+1))
hist(interarrivalTime2, breaks=(maxD2/10+1))

#20 sec intervals
hist(interarrivalTime1, breaks=(maxD1/20+1))
hist(interarrivalTime2, breaks=(maxD2/20+1))


## 4 - Chi-square test

#initialize vectors
day1count <- 1:(maxD1/10+1)  #10 sec interval counts for day 1
day1probdist <- 1:(maxD1/10+1) #expected probability
day2count <- 1:(maxD2/10+1)  #10 sec interval counts for day 2
day2probdist <- 1:(maxD2/10+1) #expected probability

#count intervals for day 1 and calculate probability distribution
for (i in 1:(maxD1/10+1)) {
  day1count[i] <- 0
  day1probdist[i] <- pexp(10*(i), rate=1/meanD1) - pexp(10*(i-1), rate=1/meanD1)
  for (j in 1:length(interarrivalTime1)) {
    if (interarrivalTime1[j] < i*10) {
      if (interarrivalTime1[j] >= (i-1)*10) {
        day1count[i] <- day1count[i] + 1
      }
    }
  }
}

#count intervals for day 2 and calculate probability distribution
for (i in 1:(maxD2/10+1)) {
  day2count[i] <- 0
  day2probdist[i] <- pexp(10*(i), rate=1/meanD2) - pexp(10*(i-1), rate=1/meanD2)
  for (j in 1:length(interarrivalTime2)) {
    if (interarrivalTime2[j] < i*10) {
      if (interarrivalTime2[j] >= (i-1)*10) {
        day2count[i] <- day2count[i] + 1
      }
    }
  }
}

#Perform chi-square test at a significance level of 0.05
chisq1 <- chisq.test(day1count, p=day1probdist, rescale.p=TRUE)
chisq2 <- chisq.test(day2count, p=day2probdist, rescale.p=TRUE)
#5
#drawing qq plot for first day
qqplot(qexp(ppoints(length(iaData$Day.1))), iaData$Day.1)
# drawing qq line
qqline(iaData$Day.1, col = "steelblue", lwd = 2)
#drawing qq plot for the second day
qqplot(qexp(ppoints(length(iaData$Day.2))), iaData$Day.2)
# drawing qq line
qqline(iaData$Day.2, col = "steelblue", lwd = 2)
#6
#plotting for day 1
#find observation times  
obsT <- cumsum(iaData$Day.1)
#plot the the inter-arrival times with respect to observation times
plot(iaData$Day.1,axes=FALSE, ann=FALSE)
# Create a title with a red, bold/italic font
title(main="Int-arrival times/observation times Day1", col.main="red", font.main=4)
#setting x-axis
axis(1, at=1:length(iaData$Day.1), lab=obsT)
#setting y-axis
axis(2, las=1, at=length(iaData$Day.1)/10*0:max(interarrivalTime1))
#plotting for day 2
#find observation times  
obsT2 <- cumsum(interarrivalTime2)
#plot the the inter-arrival times with respect to observation times
plot(iaData$Day.2,axes=FALSE, ann=FALSE)
# Create a title with a red, bold/italic font
title(main="Int-arrival times/observation times Day2", col.main="red", font.main=4)
#setting x-axis
axis(1, at=1:length(interarrivalTime2), lab=obsT2)
#setting y-axis
axis(2, las=1, at=length(interarrivalTime2)/10*0:max(interarrivalTime2))
#7
#test auto-correlation for each day
acf(interarrivalTime1, lag.max =2)
acf(interarrivalTime2, lag.max =2)
   