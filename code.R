setwd("E:/AMIT_IITK/SEM7/CS724/Assignments/2")

#library for linear regression to find the best fit line
library(lmtest)

############STEP 1####################################################################

#Reading csv
ple<-read.csv("assignment2.csv")

head(ple)

###Plotting
plot(ple$LOGd,ple$ReceivedPower_dBm,main="Plot",xlab="log(d)", ylab="Received Power (dBm)")

#Finding best fit line: Intercept and Coefficient
model_1 <- lm(ReceivedPower_dBm~LOGd,data=ple)
model_1

#Best fit line
plot(ple$LOGd,ple$ReceivedPower_dBm,main="Best Fit Line",abline(lm(ReceivedPower_dBm~LOGd,data=ple)),
     xlab="log(d)", ylab="Received Power (dBm)")

#Prediction of our entries according to the best fit line
predict(model_1,ple)

#Predict received power for any distance
a<-data.frame(LOGd = 1.176091259)
predict(model_1,a)

#Error w.r.t. the best fit line
err <- ple$ReceivedPower_dBm - predict(model_1,ple)
mean(err) #mean of errors
var(err) #variance of errors

############STEP 2####################################################################

#5 more entries:
dist <- c(2,4,5,10,15)
P_r <- c(-47,-53,-58,-66,-69)

#estimated distance using the derived equation
est_dist <- 10^((-P_r-38.00)/26.96)
est_dist

#error
err2 <- dist-est_dist
mean(err2)
