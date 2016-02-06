data(ToothGrowth)

model1 		<- lm(len ~ 1, data=ToothGrowth)
model2 		<- lm(len ~ dose - 1, data=ToothGrowth)
model3 		<- lm(len ~ dose, data=ToothGrowth)
model4 		<- lm(len ~ supp, data=ToothGrowth)
model5 		<- lm(len ~ supp + dose, data=ToothGrowth)
model6 		<- lm(len ~ supp * dose, data=ToothGrowth)
model7 		<- lm(len ~ supp + I(log(dose)), data=ToothGrowth)
model8 		<- lm(len ~ supp * I(log(dose)), data=ToothGrowth)

newdataOJ 	<- data.frame(dose=seq(0.0, 2.5, by=0.1), supp=factor(rep("OJ", 26)))
newdataVC 	<- data.frame(dose=seq(0.0, 2.5, by=0.1), supp=factor(rep("VC", 26)))

par(mfrow=c(3,3))

plot(ToothGrowth$dose, ToothGrowth$len, pch=19 + as.numeric(ToothGrowth$supp), xlim=c(0,2.5), ylim=c(0,40), 
	 main="len ~ 1", xlab="dose", ylab="len", cex.axis=0.65)
lines(newdataVC$dose, predict(model1, newdataVC), lty=1)
abline(v=0, lty=4)
legend("bottomright", horiz=TRUE, legend=c("VC", "OJ"), pch=c(21, 20), lty=c(1, 1), cex=0.6)

plot(ToothGrowth$dose, ToothGrowth$len, pch=19 + as.numeric(ToothGrowth$supp), xlim=c(0,2.5), ylim=c(0,40), 
	 main="len ~ dose - 1", xlab="dose", ylab="len", cex.axis=0.65)
lines(newdataVC$dose, predict(model2, newdataVC), lty=1)
abline(v=0, lty=4)
legend("bottomright", horiz=TRUE, legend=c("VC", "OJ"), pch=c(21, 20), lty=c(1, 1), cex=0.6)

plot(ToothGrowth$dose, ToothGrowth$len, pch=19 + as.numeric(ToothGrowth$supp), xlim=c(0,2.5), ylim=c(0,40), 
	 main="len ~ dose", xlab="dose", ylab="len", cex.axis=0.65)
lines(newdataVC$dose, predict(model3, newdataVC), lty=1)
abline(v=0, lty=4)
legend("bottomright", horiz=TRUE, legend=c("VC", "OJ"), pch=c(21, 20), lty=c(1, 1), cex=0.6)

plot(ToothGrowth$dose, ToothGrowth$len, pch=19 + as.numeric(ToothGrowth$supp), xlim=c(0,2.5), ylim=c(0,40), 
	 main="len ~ supp", xlab="dose", ylab="len", cex.axis=0.65)
lines(newdataVC$dose, predict(model4, newdataVC), lty=2)
lines(newdataOJ$dose, predict(model4, newdataOJ), lty=1)
abline(v=0, lty=4)
legend("bottomright", horiz=TRUE, legend=c("VC", "OJ"), pch=c(21, 20), lty=c(2, 1), cex=0.6)

plot(ToothGrowth$dose, ToothGrowth$len, pch=19 + as.numeric(ToothGrowth$supp), xlim=c(0,2.5), ylim=c(0,40), 
	 main = "len ~ supp + dose", xlab="dose", ylab="len", cex.axis=0.65)
lines(newdataVC$dose, predict(model5, newdataVC), lty=2)
lines(newdataOJ$dose, predict(model5, newdataOJ), lty=1)
abline(v=0, lty=4)
legend("bottomright", horiz=TRUE, legend=c("VC", "OJ"), pch=c(21, 20), lty=c(2, 1), cex=0.6)

plot(ToothGrowth$dose, ToothGrowth$len, pch=19 + as.numeric(ToothGrowth$supp), xlim=c(0,2.5), ylim=c(0,40), 
	 main = "len ~ supp * dose", xlab="dose", ylab="len", cex.axis=0.65)
lines(newdataVC$dose, predict(model6, newdataVC), lty=2)
lines(newdataOJ$dose, predict(model6, newdataOJ), lty=1)
abline(v=0, lty=4)
legend("bottomright", horiz=TRUE, legend=c("VC", "OJ"), pch=c(21, 20), lty=c(2, 1), cex=0.6)

plot(ToothGrowth$dose, ToothGrowth$len, pch=19 + as.numeric(ToothGrowth$supp), xlim=c(0,2.5), ylim=c(0,40), 
	 main = "len ~ supp + I(log(dose))", xlab="dose", ylab="len", cex.axis=0.65)
lines(newdataVC$dose, predict(model7, newdataVC), lty=2)
lines(newdataOJ$dose, predict(model7, newdataOJ), lty=1)
abline(v=0, lty=4)
legend("bottomright", horiz=TRUE, legend=c("VC", "OJ"), pch=c(21, 20), lty=c(2, 1), cex=0.6)

plot(ToothGrowth$dose, ToothGrowth$len, pch=19 + as.numeric(ToothGrowth$supp), xlim=c(0,2.5), ylim=c(0,40), 
	 main = "len ~ supp * I(log(dose))", xlab="dose", ylab="len", cex.axis=0.65)
lines(newdataVC$dose, predict(model8, newdataVC), lty=2)
lines(newdataOJ$dose, predict(model8, newdataOJ), lty=1)
abline(v=0, lty=4)
legend("bottomright", horiz=TRUE, legend=c("VC", "OJ"), pch=c(21, 20), lty=c(2, 1), cex=0.6)

par(mfrow=c(1,1))