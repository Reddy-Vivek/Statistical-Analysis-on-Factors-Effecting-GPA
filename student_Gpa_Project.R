load("data.Rda")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
library(MASS)
# Prerequisites
Student.Survey= read.csv("C:\\Users\\hp\\Downloads\\StudentSurvey.csv")
attach(Student.Survey)

#copying to other dataframe
sdf<-Student.Survey

#Rename columns and adding columns
transit_time<-Student.Survey$How.much.time..in.mins..does.it.take.for.you.to.come.to.college.
social_media<-Student.Survey$How.many.mins.per.day..do.you.spend.on.Facebook.++Student.Survey$How.many.mins.per.day..do.you.spend.on.Instagram.+Student.Survey$How.much.time..in.mins..per.day..do.you.spend.on.Twitter.
game<-Student.Survey$How.much.time..in.hours..per.week.do.you.spend.on.online.gaming.+Student.Survey$How.much.time..in.hours..per.week.do.you.spend.on.outdoor.sports.
sleep_time<-Student.Survey$How.many.hours.of.sleep.do.you.get.per.day.
working_hours<-Student.Survey$How.many.hours.per.week.do.you.work.
study_time_prefer<-Student.Survey$What.is.your.preferred.time.to.study.
study_place_prefer<-Student.Survey$Where.do.you.prefer.to.study.
study_hours<-Student.Survey$How.many.hours.do.you.study.per.day.
relation<-Student.Survey$How.much.you.think.your.personal.Relationships.affect.your.studies.
experience<-Student.Survey$What.is.your.total.professional.work.experience..in.months..
gpa<-Student.Survey$What.is.your.GPA.

#Removing missing values of game column and replacing with Mean
game
sum(is.na(game))
mean(game, na.rm=TRUE)
install.packages("imputeTS")
library('imputeTS')
game<-round(na_mean(game),1)
game

head(sdf)
#Creating a new dataframe
df <- data.frame(transit_time,social_media,game,sleep_time,study_hours,study_time_prefer,
                 study_place_prefer,working_hours,experience,relation,gpa)

head(df)
#Checking corelation of numerical variables
cordf <- data.frame(gpa,sleep_time,study_hours,working_hours,experience)
head(cordf)
res<- cor(cordf)
round(res,2)
install.packages("Hmisc")
library("Hmisc")
mydata.rcorr = rcorr(as.matrix(cordf))
mydata.rcorr

#Plotting scatterplots for each variable with gpa
#Transit Time
plot<- ggplot(data = df ,aes(x =transit_time, y = gpa, col=transit_time))+geom_point(stat="identity")+
  labs(x = "Transit Time", y = "GPA", title = "GPA Vs Transit Time")
plot + geom_smooth(method = "lm", se = FALSE) 


#Social media
plot<- ggplot(data = df ,aes(x =social_media, y = gpa, col=gpa))+geom_point(stat="identity")+
  labs(x = "Social Media", y = "GPA", title = "GPA Vs Social Media") 
plot + geom_smooth(method = "lm", se = FALSE) +xlim(0,200)

plot<- ggplot(data = df ,aes(x =game, y = gpa, col=game))+geom_point(stat="identity")+
  labs(x = "Gaming", y = "GPA", title = "GPA Vs Gaming Time") 
plot + geom_smooth(method = "lm", se = FALSE) + xlim(0,100)

plot<- ggplot(data = df ,aes(x =working_hours, y = gpa, col=working_hours))+geom_point(stat="identity")+
  labs(x = "Working Hours per week", y = "GPA", title = "GPA Vs Working Hours") 
plot + geom_smooth(method = "lm", se = FALSE) + xlim(0,25)



plot<- ggplot(data = df ,aes(x =study_time_prefer, y = gpa, col=study_time_prefer))+geom_point(stat="identity")+
  labs(x = "Study Time Preference", y = "GPA", title = "GPA Vs Study Time Prefer") 
plot + geom_smooth(method = "lm", se = FALSE) 

plot<- ggplot(data = df ,aes(x =sleep_time, y = gpa, col=sleep_time))+geom_point(stat="identity")+
  labs(x = "Sleep Time", y = "GPA", title = "GPA Vs Sleep Time") 
plot + geom_smooth(method = "lm", se = FALSE)

plot<- ggplot(data = df , mapping = aes(x =study_place_prefer, y = gpa,col=study_place_prefer))+geom_point()
plot

plot<- ggplot(data = df , mapping = aes(x =study_hours, y = gpa))+geom_point()
plot + geom_smooth(method = "lm", se = FALSE)

#Prev Work Exp
plot<- ggplot(data = df ,aes(x =experience, y = gpa, col=experience))+geom_point(stat="identity")+
  labs(x = "Previous Work Experience in months", y = "GPA", title = "GPA Vs Work Experience") 
plot + geom_smooth(method = "lm", se = FALSE) +xlim(0,40)

#Histogram of GPA
hist(gpa, 
     main="Histogram of GPA", 
     xlab="GPA of langara students",
     ylab = "Number of students",
     border="pink", 
     col="maroon",
     xlim=c(0,5),
     ylim=c(0,25),
     las=1, 
     breaks=20)
summary(gpa)

#Model Building
mod<-lm(gpa~transit_time)
plot(transit_time,gpa,xlim=c(0,100),ylim=c(0,5))
abline(mod)
boxplot(transit_time)
hist(transit_time,breaks=20,col='grey',xlab = 'transit time in minutes',ylab='Number of Students')
summary(mod)
summary(transit_time)

mod2<-lm(gpa~social_media)
plot(social_media,gpa,xlim=c(0,100),ylim=c(0,5))
abline(mod2)
boxplot(social_media)
hist(social_media,breaks=20,col='grey',xlab = 'Social_media time in minutes per week',ylab='Number of Students')
summary(mod2)
summary(social_media)

mod3<-lm(gpa~game)
plot(game,gpa,xlim=c(0,150),ylim=c(0,5))
abline(mod3)
hist(game,breaks=20,col='grey',xlab='Game time in minutes per week',ylab='Number of Students')
boxplot(game,xlab='Game time in minutes per week',col='grey')
summary(mod3)
summary(game)

mod4<-lm(gpa~sleep_time)
plot(sleep_time,gpa,ylim=c(1,5),xlab='Sleep Time',ylab='GPA',col='blue')
grid(nx = 5, ny = 6, col = "grey", lty = "solid",lwd = par("lwd"), equilogs = TRUE)
abline(mod4,col='red',lty='solid',lwd=2)
summary(mod4)
hist(sleep_time,breaks=8,xlab='Sleep Time',ylab='Number of Students')
boxplot(sleep_time,ylab='Number of hours')
summary(sleep_time)
sd(sleep_time)
var(sleep_time)

mod5<-lm(gpa~working_hours)
plot(working_hours,gpa,xlim=c(0,40),ylim=c(0,5),xlab='Working Hours Per Week',ylab='GPA')
abline(mod5)
summary(mod5)
hist(working_hours,breaks=10,xlab='Working Hours Per Week',ylab='Number of Students')
boxplot(working_hours,ylab='Number of Working Hours per Week')

mod6<-lm(gpa~study_time_prefer)
plot(study_time_prefer,gpa,ylim=c(1,5),xlab='Study Time',ylab='GPA')
legend(x=-3,y=7,c("sample1","sample2"),cex=.8,col=c("red","blue"),pch=c(1,2))
boxplot(study_time_prefer)
summary(study_time_prefer)
summary(mod6)

mod7<-lm(gpa~study_place_prefer)
plot(study_place_prefer,gpa,xlim=c(0,9.5),ylim=c(1,5))
boxplot(study_place_prefer)
summary(study_place_prefer)
summary(mod7)

mod8<-lm(gpa~study_hours)
plot(study_hours,gpa,ylim=c(1,6),xlab='Study Hours Per Day',ylab='GPA')
abline(mod8)
summary(mod8)
boxplot(study_hours,ylab='Number of Study Hours')
hist(study_hours,breaks=10,xlab='Study hours per day',ylab='Number of Students')

mod9<-lm(gpa~relation)
plot(relation,gpa,xlim=c(0,7),ylim=c(1,4.3))
summary(relation)
summary(mod9)

mod10<-lm(gpa~experience)
plot(experience,gpa,xlim=c(0,40),ylim=c(1,5))
abline(mod10)
boxplot(experience)
hist(experience,breaks=10)
summary(mod10)

#with interaction terms
mod12<-lm(gpa~sleep_time+study_hours+study_time_prefer+sleep_time*study_hours+sleep_time*study_time_prefer+study_hours*study_time_prefer)
summary(mod12)

mod13<-lm(gpa~sleep_time+study_hours+study_time_prefer+sleep_time*study_hours)
summary(mod13)

anova(mod12,mod13)
anova(mod12,mod14)

pred<-predict(mod13)
#without interaction
mod14<-lm(gpa~sleep_time+study_hours+study_time_prefer)
summary(mod14)

cor.test(gpa,study_hours)

car::vif(mod12)
car::vif(mod13)
car::vif(mod14)
###Checking assumptions:
### 1. Lack of fit
### 2. Equal variances
### 3. Outliers

##detecting lack of fit

df
library(ggplot2)
library(ggrepel)
library(dplyr)

residuals<-resid(mod13)
df$res<- residuals
summary(residuals)
sd(residuals) #0.4041 ---#2s = 0.8082 #95% values within 2s
residuals[(residuals >0.8082) | (residuals < -0.8082)]#values at 5,7,16,40
#filter points outside of 2s from df
highlight.df <- df %>% 
  filter((residuals >0.8082) | (residuals < -0.8082))


#plot residuals, highlight outliers, draw abline to outline regions of outliers, 
#create x&y labels and title, remove legend, add in outlier values
ggplot(data = df,mapping =  aes(x = sleep_time, y = res)) +
  geom_point() +
  geom_abline(aes(intercept = 0.8082, slope = 0)) +
  geom_abline(aes(intercept = -0.8082, slope = 0)) +
  labs(x = "Sleep time per day(in hours)", y = "Residuals", title = "Residuals VS Sleep time per day") +
  theme(legend.position = 'none') +
  geom_text_repel(aes(label=ifelse((res > 0.8082 |res < -0.8082),round(res,4),"")), hjust=1.1)

#For Study Hours
ggplot(data = df,mapping =  aes(x = study_hours, y = res)) +
  geom_point() +
  geom_abline(aes(intercept = 0.8082, slope = 0)) +
  geom_abline(aes(intercept = -0.8082, slope = 0)) +
  labs(x = "Study hours per day", y = "Residuals", title = "Residuals VS Study time per day(in hours)") +
  theme(legend.position = 'none') +
  geom_text_repel(aes(label=ifelse((res > 0.8082 |res < -0.8082),round(res,4),"")), hjust=1.1)


##check normality
ggplot(data = df) +
  aes(sample = res) +
  stat_qq() +
  stat_qq_line(color = 'blue') +
  labs(x = "Normal Scores", y = "Residuals", title = "Normal Probability Plot (Q-Q Plot)")
#some outliers observed
##identify outliers
df$stan.resid <- rstandard(mod13)
df$stan.resid[(df$stan.resid >3) | (df$stan.resid < -3)]
#-4.465

#filter points outside of 2s from df which has pred variable
highlight.stan.resid.df <- df %>% 
  filter((df$stan.resid >3) | (df$stan.resid < -3))

#plot residuals, highlight outliers, draw abline to outline regions of outliers, 
#create x&y labels and title, remove legend, add in outlier values
ggplot(data = df,aes(x = pred, y = stan.resid))+
  geom_point() +
  geom_abline(aes(intercept = 3, slope = 0)) +
  geom_abline(aes(intercept = -3, slope = 0)) +
  labs(x = "Predicted GPA", y = "Standardized Residuals", title = "Standardized Residuals VS Predicted GPA") +
  theme(legend.position = 'none') +
  geom_text_repel(aes(label=ifelse((stan.resid > 3 |stan.resid < -3),round(res,4),"")), hjust=1.1)

#remove outliers; 4 points if use 2s from residual plot; 1 point if use 3 from stand. residual plot
df.removed <- df[-c(5,7,16,40),]

#double check if really removed
length(df$How.many.hours.do.you.study.per.day.) #100
length(df.removed$How.many.hours.do.you.study.per.day.) #96

###-Re-fit model-###

refitmodel13 <- lm(gpa~sleep_time+study_hours+study_time_prefer+sleep_time*study_hours,data = df.removed)
summary(refitmodel13)
mod15<-lm(gpa~transit_time+social_media+game+sleep_time+working_hours+study_time_prefer+study_time_prefer+study_hours+relation+experience)

#colinearity check
#stepwise selection summary
ols_step_both_p(refitmodel13)
ols_step_both_p(mod12)
ols_step_both_p(mod14)
ols_step_both_p(mod15)

###Checking assumptions:
#no trend in plot, ~95% residuals within 2s of 0
#sd=0.4041
newresidual<-resid(refitmodel13)
newresidual
sd(newresidual)
#same as previous case.

##check normality
ggplot(data = df.removed) +
  aes(sample = res) +
  stat_qq() +
  stat_qq_line(color = 'blue') +
  labs(x = "Normal Scores", y = "Residuals", title = "Normal Probability Plot (Q-Q Plot) Without Outliers")
#lies close to the straight line


####--------------------------------------------
#Using final model for prection and estimation
--------------------------------------------------

#ESTIMATIOM

#estimated GPA and confidence interval when fuel sleep time =8 hrs & study time =4
#and prefer morning time to study
predict(refitmodel13, newdata= data.frame(study_hours = 3,sleep_time  = 5, study_time_prefer= 'Evening'), interval = 'confidence', level = 0.95)
##results: 2.758219, CI: 2.598059 - 2.918379


#PREDICTION

#predicted GPA and predicted interval when sleep time = 5 and study hrs =9 and study time pre = Evening
predict(refitmodel13, newdata= data.frame(study_hours = 5,sleep_time = 5, study_time_prefer= 'Evening'), interval = 'prediction', level = 0.95)
##results: 3.91147, CI: 2.936769-4.88617
predict(mod13,interval="confidence")
predict(mod13,interval="prediction")
confint(mod13,level=0.95)  
  
library(data.table)
library(olsrr)  
cor.test(gpa,study_hours)
cor.test(study_hours,sleep_time)
plot(residuals,ylab='Residuals',main='Residual plot of model')
abline(0,0)
qqnorm(residuals,main='Normal probablity plot of model')
boxplot(residuals,main='Boxplot of model')
plot(sleep_time,residuals,xlab='Sleep time per day(in hours)',ylab='Residuals')
abline(0,0,col='blue')
plot(study_hours,residuals,xlab='study time per day(in hours)',ylab='Residuals')
abline(0,0,col='blue')
install.packages('ggfortify')
library('ggfortify')
autoplot(mod13)
install.packages('car')
library('car')
crPlots(mod13)

install.packages("ggpubr")
library(ggpubr)



install.packages('MASS')
library('MASS')
step(mod12,direction='backward')
#Stepwise Regression:
#Since sample size n > number of predictors we choose backward regression here. So that the full model can be fit.

# Stepwise regression model
# Selecting variables
step(mod12, direction = "backward", nvmax=4)  

#step(mod12, direction = "forward", scope = formula(mod12))
m1 = ols_step_both_p(mod13)
m1
plot(m1)
anova(mod13)
summary(mod13)
summary(study_hours)

m2 = ols_step_all_possible(mod13)
plot(m2)

#Exporting df to csv
df <- data.frame(transit_time,social_media,game,sleep_time,study_hours,study_time_prefer,
                 study_place_prefer,working_hours,experience,relation,gpa)

write.csv(df,"C:\\Users\\hp\\Documents\\danaproject\\modifieddf.csv",row.names =TRUE)
#Exporting df to excel
install.packages("writexl")
library("writexl")
write_xlsx(df,"C:\\Users\\hp\\Documents\\danaproject\\studentdf.xlsx")
