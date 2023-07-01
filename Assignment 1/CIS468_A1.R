#Aastha Bhatt
#Read customer data csv file
Victims_Rape <- read.csv(file="2018 Victims of Rape.csv", stringsAsFactors = TRUE)
View( Victims_Rape)

#Get a listing of variables
names(Victims_Rape)

#Inspect the first 10 records
Victims_Rape[1:10,]

#Inspect a summary of all variables
summary(Victims_Rape)

#Examples of invalid values and outliers: Run the following summaries.
#Do you see anything that could be incorrect?
summary(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years)

summary(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.)


#Calculate min, max, mean, and median for total victims 
min(Victims_Rape$Total.Victims)
max(Victims_Rape$Total.Victims)
mean(Victims_Rape$Total.Victims)
median(Victims_Rape$Total.Victims)
var(Victims_Rape$Total.Victims)
sd(Victims_Rape$Total.Victims)



# Cases Reported 
min(Victims_Rape$Cases.Reported)
max(Victims_Rape$Cases.Reported)
mean(Victims_Rape$Cases.Reported)
median(Victims_Rape$Cases.Reported)
var(Victims_Rape$Cases.Reported)
sd(Victims_Rape$Cases.Reported)
total_skew1 <- (3*(mean(Victims_Rape$Cases.Reported) - median(Victims_Rape$Cases.Reported)))/sd(Victims_Rape$Cases.Reported)
total_skew1




#Child Victims - Total 
min(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)
max(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)
mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)
median(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)
var(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)
sd(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)



#Age group for (0-5)
min(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years)
max(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years)
mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years)
median(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years)
var(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years)
sd(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years)



#Age group for (6-11)
min(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above.)
max(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above.)
mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above.)
median(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above.)
var(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above.)
sd(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above.)


#Age group for (12-15)
min(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.)
max(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.)
mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.)
median(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.)
var(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.)
sd(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.)

#Age group for (16-18)
min(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above.)
max(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above.)
mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above.)
median(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above.)
var(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above.)
sd(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above.)


#Age group for (19-29)
min(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.Years)
max(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.Years)
mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.Years)
median(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.Years)
var(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.Years)
sd(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.Years)



#Age group for (30-44)
min(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years)
max(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years)
mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years)
median(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years)
var(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years)
sd(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years)



#Age group for (45-59)
min(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years)
max(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years)
mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years)
median(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years)
var(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years)
sd(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years)

#Age group for (60+)
min(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above)
max(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above)
mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above)
median(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above)
var(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above)
sd(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above)

#Total Number of Adult Women Victims 
min(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)
max(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)
mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)
median(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)
var(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)
sd(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)


length(Victims_Rape$Total.Victims)


#Calculate the skewness of total number of victims 
total_skew <- (3*(mean(Victims_Rape$Total.Victims) - median(Victims_Rape$Total.Victims)))/sd(Victims_Rape$Total.Victims)
total_skew 


#Detailed summary of categorical variable marital status
summary(Victims_Rape$Category)

summary(Victims_Rape$State.UT.)



#Finding outliers
#Using +/- 3*standard deviation
sd_total <- sd(Victims_Rape$Total.Victims)
sd_upper <- mean(Victims_Rape$Total.Victims)+(3*sd_total) #Anything up than this value is an outlier
sd_lower <- mean(Victims_Rape$Total.Victims)-(3*sd_total)
outlier_total_sd <- Victims_Rape$Total.Victims>sd_upper|Victims_Rape$Total.Victims<sd_lower
outlier_total_sd


# Producing a dot plot with sorted categories using WVPlots ClevelandDotPlot()
# if needed install WVPlots package: install.packages("WVPlots")
library(WVPlots)                                              	 
ClevelandDotPlot(Victims_Rape, "Category",               	 
                 sort = 1, color="red", title="Number of Districts per Category") +                   	
  coord_flip()+
  theme(axis.text.y = element_text(size=10)) 


#Exploring distributions/total number of victims by district with a box plot 
library (tidyverse)
library(ggplot2)
ggplot(Victims_Rape, aes(y=fct_rev(State.UT.),x = Total.Victims)) +
  geom_boxplot() +
  scale_color_brewer(palette="Set1") +
  labs(fill="State.UT.") +
  ylab("Districts") +
  xlab("Total Victims")

#Producing a density plot 
#if needed install scales package: install.packages("scales")
library(scales)                                      	
ggplot(Victims_Rape, aes(x=Total.Victims)) + geom_density() + 
  scale_x_continuous(labels=number)  

# Creating a log-scaled density plot (use when data has a large range) 
ggplot(Victims_Rape, aes(x=Total.Victims)) + 
  geom_density() + 
  scale_x_log10(breaks = c(10, 100, 1000, 10000, 100000, 1000000), labels=number) +    	 
  annotation_logticks(sides="bt", color="gray")      

# Testing for normal distribution
# Normal probability plot of total
par(mfrow=c(1,1))
qqnorm(Victims_Rape$Total.Victims, datax=TRUE, col="red", ylim=c(min(Victims_Rape$Total.Victims),max(Victims_Rape$Total.Victims)), main="Normal Q-Q Plot of Inverse Square Root weightlbs")
qqline(Victims_Rape$Total.Victims, col="blue", datax=TRUE)


# Normal probability plot of age
par(mfrow=c(1,1))
qqnorm(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above., datax=TRUE, col="red", ylim=c(min(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.),max(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.)), main="Normal Q-Q Plot of Inverse Square Root weightlbs")
qqline(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above., col="blue", datax=TRUE)











