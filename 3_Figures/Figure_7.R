# Script to calculate VIC CPUE Correlation and make plot



 mydata <- read.csv("VIC_CPUE_Settlement_Correlation_data.csv", header = T)

# # tests different lags (I think) - note i manually did a 1 year lag above.
x <- ccf(mydata$Predicted.settlement, mydata$CPUE, na.action = na.pass)
x
plot(x)

# Create lagged variable
library(dplyr)

mydata2 <- mutate(mydata, Predicted.settlement.2y.lag = lag(Predicted.settlement, n = 2))

mydata <- mydata2
head(mydata)
# 
plot(mydata$Predicted.settlement.2y.lag, mydata$CPUE)

fit1 <- lm(CPUE ~ (Predicted.settlement.2y.lag), data = mydata)
summary(fit1)
abline(fit1)

par(mar = c(5,5,2,5))
plot(mydata$Year, mydata$CPUE, type = "l",
     ylab = "", 
     xlab = "", yaxt= "none", xaxt="none", font = 2)
axis(side=2,las=2, xlim=c(0,3.5), font=2)
axis(side=1,las=1, xaxp  = c(1998, 2018, 20), font = 2)
par(new = T)
with(mydata, plot(Year, (Predicted.settlement.2y.lag), type = "l", lty=2,
                  col = "red", axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4, las=2, font = 2)
mtext(side = 4, line = 3, 'Lagged Relative Predicted Settlement', font = 2)
mtext(side = 2, line = 3, text = expression(bold(paste("Catch Per Unit Effort (kg shot"^" -1",")")))) # font = 2 # does not work with expression()
mtext(side = 1, line = 3, 'Year', font = 2)
legend("topleft",
       legend=c("CPUE", "Larval Settlement"),
       lty=c(1,2), col=c("black", "red"))

cor(mydata$Predicted.settlement.2y.lag, mydata$CPUE, use = "complete.obs")
cor.test(mydata$Predicted.settlement.2y.lag, mydata$CPUE)

