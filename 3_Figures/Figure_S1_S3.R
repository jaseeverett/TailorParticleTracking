# Script to demostrate Degree days growth

Days = seq(0,30, 1)

DD_18 = 18*Days
DD_22 = 22*Days
DD_26 = 26*Days

plot(Days, DD_18, type = "l", col = "blue", lwd =2, ylab="Degree Days", font.lab=2, cex.lab =1.3,
     xaxt="none", yaxt = "none")
lines(Days, DD_22, lwd =2)
lines(Days, DD_26, lwd =2, col = "red")
axis(side=2,las=2, xlim=c(0,600), font=1)
axis(side=1,las=1, xaxp  = c(0,30, 10), font = 1)
abline(h=500, lty = 3)
abline(v = (500/18), lty = 2, col = "blue")
abline(v = (500/22), lty = 2)
abline(v = (500/26), lty = 2, col = "red")
legend(0, y=450, legend = c("26 °C", "22 °C", "18 °C"), lwd = 2, col =c("red", "black", "blue"), text.font = 2)


dat <- read.csv("degree days plotting.csv")
dat <- subset(dat, degree.day <600)
plot(dat$degree.day,dat$mm, type = "l", ylab="Fish Length (mm)", 
     xlab = "Degree Days", lwd = 2, font.lab=2, cex.lab =1.3, yaxt="none")
axis(side=2,las=2, xlim=c(0,15), font=1)
abline(h = 10.7, lty = 3)
abline(v = 500, lty = 2)
