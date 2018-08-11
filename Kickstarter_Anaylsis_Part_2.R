setwd("/Users/zachang/documents/data/data_kickstarter")
data=read.csv('DSI_kickstarterscrape_dataset.csv')
attach(data)
library(SparseM)
library(Matrix)
library(MatrixModels)
library(quantreg)

# Boxplot for Success vs Category #  

par(mfrow=c(1,1),mar=c(5,4,4,2)+0.1)
boxplot(log(pledged[status=="successful"],base=10)~category[status=='successful'],las=2,ylab="Pledged $ (Log scale)",xaxt="n",pch=20,col="skyblue",main="Boxplot of Pledged Contributions of Successful Projects by Category")
par(mar=c(6,4,4,2)+0.2)
boxplot(log(backers[status=="successful"],base=10)~category[status=='successful'],las=2,ylab="Backers (Log scale)",pch=20,col="skyblue",main="Boxplot of Backers of Successful Projects by Category")


### Quantile Regression Modelling ###

# Fit a univariate quantreg model over a range of quantiles from 0.02-0.98 #
rq.fit1<-rq(pledged~goal,tau=2:98/100)
rq.fit2<-rq(backers~goal,tau=2:98/100) 

# Plot qreg coefficients through range of quantiles #
par(mfrow=c(2,1),mar=c(5,4,4,2)+0.1)
plot(rq.fit1$coefficients[2,],type="n",xlab="Percentile",ylab="Coefficient of Goal vs Pledged")
lines(rq.fit1$coefficient[2,],lty=4,lwd=2)
plot(rq.fit2$coefficients[2,],type="n",xlab="Percentile",ylab="Coefficient of Goal vs Backers")
lines(rq.fit2$coefficient[2,],lty=4,lwd=2)

# Frequency Density Plots # 
par(mfrow=c(1,1))
plot(density(duration[status=='successful']),col="steelblue2",lwd=2,main="Frequency Densities of Campaign Duration for Successful vs Unsuccessful Campaigns")
lines(density(duration[status=='cancelled'|status=='failed']),col="orange2",lwd=2)
legend(x=70,y=0.08,legend=c('Successful','Unsuccessful'),col=c('steelblue2','orange2'),lty=1,lwd=2)

# Logistic Regression for Duration vs Success# 
data$duration.cat<-cut(duration,c(seq(0,85,by=5),max(duration))) #Bin the duration variable#
data$success=status=='succesful'

# Fitting a univariate logistic regression with duration ordinal variable #
logit.fit1<-glm(success~duration.catfamily=binomial('logit'),data=data)
summary(logit.fit1)

# Create confidence bands #
se.fit1.up<-logit.fit1$coefficients[1:length(logit.fit1$coefficients)]+1.96*summary(logit.fit1)$coefficients[1:nrow(summary(logit.fit1)$coefficients),2]
se.fit1.low<-logit.fit1$coefficients[1:length(logit.fit1$coefficients)]-1.96*summary(logit.fit1)$coefficients[1:nrow(summary(logit.fit1)$coefficients),2]

# Plot coefficient plot #
plot(logit.fit1$coefficients[1:length(logit.fit1$coefficients)],type="b", ylim=c(min(se.fit1.low)-3,max(se.fit1.up)+3),lwd=2,pch=20,cex=0.5,xaxt='n',main="Logistic Regression coefficients of Success vs Duration",xlab="Duration",ylab='Coefficients')
axis(1,at=seq(0.5,17.5,by=1),labels=seq(0,85,by=5))
lines(se.fit1.up,lty=3,col='red')
lines(se.fit1.low,lty=3,col='red')
abline(h=0,lty=2)