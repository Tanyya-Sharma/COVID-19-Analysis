library(Hmisc)

data<-read.csv("/Users/tanyasharma/Desktop/COVID19_line_list_data.csv")
describe(data)

data$death_dummy<-as.integer(data$death !=0)

sum(data$death_dummy)/nrow(data) #death rate=0.05898618

#AGE- claim: people who die are older 
dead=subset(data, death_dummy==1)
alive=subset(data, death_dummy==0)
mean(dead$age, na.rm = TRUE) #68.58621
mean(alive$age, na.rm = TRUE) #48.04432
#Checking if this is statistically significant.
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99)
#Usually, if p-value<0.05, we reject null hypothesis
#here, p-value~0, so we reject the null hypothesis and conclude that this is statistically significant

#GENDER- claim: gender has no effect 
men=subset(data, gender == "male")
women=subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) #0.08477842 #8.5%
mean(women$death_dummy, na.rm = TRUE) #0.3664921 #3.7%
#Checking if this is statistically significant.
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.99)
#99% confidence: men have from 0.8% to 8.8% higher chance of dying
#p-value = 0.002<0.05, so this is statistically significant