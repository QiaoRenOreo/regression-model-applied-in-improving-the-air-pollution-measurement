setwd("D:/2017Qiao/Module5/Assignment")
ap.cal = read.csv("M5_cal.csv", head = T, sep =";")
ap.cal$Time <- strptime(ap.cal$Time, format="%d-%m-%Y %H:%M")
ap.cal$Ref_NO2

# question1
summary(ap.cal$Time )
summary(ap.cal$Ref_NO2)
summary(ap.cal$Airbox_NO2)
summary(ap.cal$Airbox_O3 )
summary(ap.cal$Mean_temp )
summary(ap.cal$Wind_speed )
summary(ap.cal$Rel_hum )

summary(ap.cal$Wind_speed*0.1)

count_stormy_hours=0
for(i in 1:length(ap.cal$Wind_speed)+1)
{
  if (ap.cal$Wind_speed[i]*0.1>=8)
  {
    count_stormy_hours=count_stormy_hours+1
  }
}
count_stormy_hours


# question2
X11(height=7, width=7)

hist(ap.cal$Airbox_O3, nclass=30 ) 

qqnorm(ap.cal$Airbox_O3)
qqline(ap.cal$Airbox_O3)

boxplot(ap.cal$Airbox_O3)
points(1,mean(ap.cal$Airbox_O3))

mean(ap.cal$Airbox_O3)

# transform ozone data
ap.cal$log_O3 <- log(ap.cal$Airbox_O3)
hist(ap.cal$log_O3,nclass = 100) 

qqnorm(ap.cal$log_O3)
qqline(ap.cal$log_O3)

boxplot(ap.cal$log_O3)
mean(ap.cal$log_O3)
points(1,mean(ap.cal$log_O3))

#question3

X11()
par(mfrow=c(2,3))
Ref_NO2=ap.cal$Ref_NO2
plot(ap.cal$Airbox_NO2,Ref_NO2) # covariate 1
plot(ap.cal$Airbox_O3 ,Ref_NO2)  # covariate 2
plot(ap.cal$log_O3 ,Ref_NO2)  # covariate 3
plot(ap.cal$Mean_temp ,Ref_NO2)  # covariate 4
plot(ap.cal$Wind_speed ,Ref_NO2)  # covariate 5
plot(ap.cal$Rel_hum ,Ref_NO2)  # covariate 6


# question4
Ref_NO2=ap.cal$Ref_NO2

Model_Airbox_NO2.lm=lm(Ref_NO2~ap.cal$Airbox_NO2) #model 1 has run
summary(Model_Airbox_NO2.lm)

Model_Airbox_O3.lm=lm(Ref_NO2~ap.cal$Airbox_O3) #model 2 has run
summary(Model_Airbox_O3.lm)

Model_log_O3.lm=lm(Ref_NO2~ap.cal$log_O3) #model 3
summary(Model_log_O3.lm)

Model_Mean_temp.lm=lm(Ref_NO2~ap.cal$Mean_temp) #model 4
summary(Model_Mean_temp.lm)

Model_Wind_speed.lm=lm(Ref_NO2~ap.cal$Wind_speed) #model 5
summary(Model_Wind_speed.lm)

Model_Rel_hum.lm=lm(Ref_NO2~ap.cal$Rel_hum) #model 6
summary(Model_Rel_hum.lm)


# question5
round(cor(ap.cal[,2:8], use="pairwise.complete.obs"),2)

# question6
Ref_NO2=ap.cal$Ref_NO2
ap.cal$log_O3 <- log(ap.cal$Airbox_O3)
Airbox_NO2=ap.cal$Airbox_NO2 #NO2
Wind_speed= ap.cal$Wind_speed #wind
log_O3=ap.cal$log_O3 #logO3

Model_NO2_logO3.lm=lm(Ref_NO2~Airbox_NO2+log_O3) # 2 explanatory variables.
summary(Model_NO2_logO3.lm)

Model_NO2_wind.lm=lm(Ref_NO2~Airbox_NO2+Wind_speed) # 2 explanatory variables.
summary(Model_NO2_wind.lm)

Model_logO3_wind.lm=lm(Ref_NO2~log_O3+Wind_speed) # 2 explanatory variables.
summary(Model_logO3_wind.lm)

Model_3covariates.lm=lm(Ref_NO2~Airbox_NO2+log_O3+Wind_speed) # 3 explanatory variables.
summary(Model_3covariates.lm)


# question 7
mod1 <- lm(Ref_NO2~Airbox_NO2+log_O3+Wind_speed, data=ap.cal, na.action=na.exclude)

X11()
par(mfrow=c(1,2))
plot(ap.cal$Airbox_NO2, Ref_NO2) #original data
abline(lm(Ref_NO2~Airbox_NO2))
#abline(lm(Ref_NO2~Airbox_NO2+log_O3+Wind_speed))

plot(fitted(mod1), Ref_NO2) #fitted data
abline(a=0,b=1)


# question 8
# Model_3covariates.lm=lm(Ref_NO2~Airbox_NO2+log_O3+Wind_speed)
new=data.frame(Airbox_NO2=22.5,log_O3=3.95, Wind_speed = 40)
predict(Model_3covariates.lm, new, interval="prediction", level=0.95)

# my own way of calculation: estimation of model error variance
'''
SumOfResidualSquare=0
DataTotalAmount=length(ap.cal$Ref_NO2)
DataTotalAmount
DegreeOfFreedom=DataTotalAmount-2
Airbox_NO2[1]
log_O3[1]
Wind_speed[1]

for(i in 1:2)
{
  predictedY= 1.24008* Airbox_NO2[i]+(-1.50395)* log_O3[i]+ (-0.05676)* Wind_speed[i]
  observedY=Ref_NO2[i]
  residual=predictedY-observedY
  SumOfResidualSquare=SumOfResidualSquare+(residual**2)
}

SumOfResidualSquare

EstimatedModelErrorVariance=SumOfResidualSquare/DegreeOfFreedom
EstimatedModelErrorVariance
EstimatedModelErrorStandardDeviation=sqrt(EstimatedModelErrorVariance)
EstimatedModelErrorStandardDeviation

t=qt(0.975,df=DegreeOfFreedom)
'''


