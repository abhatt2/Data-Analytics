#Aastha Bhatt
#Read customer data csv file
Victims_Rape <- read.csv(file="Victims_Rape.csv", stringsAsFactors = TRUE)
View( Victims_Rape)

#Get a listing of variables
names(Victims_Rape)

#Inspect the first 10 records
Victims_Rape[1:10,]

#Inspect a summary of all variables
summary(Victims_Rape)

#Finding outliers 

##########################################################################################################################################
#Total Victim 
#Using +/- 3*standard deviation - MDHYA PRADESH 
sd_total <- sd(Victims_Rape$Total.Victims)
sd_upper <- mean(Victims_Rape$Total.Victims)+(3*sd_total) #Anything up than this value is an outlier
sd_lower <- mean(Victims_Rape$Total.Victims)-(3*sd_total)
outlier_total_sd <- Victims_Rape$Total.Victims>sd_upper|Victims_Rape$Total.Victims<sd_lower
outlier_total_sd

#Using z-scores - MADHYA PRADESH 
zscore_total <- (Victims_Rape$Total.Victims - mean(Victims_Rape$Total.Victims))/sd(Victims_Rape$Total.Victims)
outlier_total_zscore <- zscore_total > 3 | zscore_total < (-3)
outlier_total_zscore

#Using +/- 1.5*IQR - MADHTYA PRADESH & RAJASTAN & UTTAR PRADESH 
quartile_total <- quantile(Victims_Rape$Total.Victims, names=FALSE)
iqr_total <- quartile_total[4]-quartile_total[2]
iqr_upper <- quartile_total[4]+(1.5*iqr_total)
iqr_lower <- quartile_total[2]-(1.5*iqr_total)
outlier_total_iqr <- Victims_Rape$Total.Victims>iqr_upper|Victims_Rape$Total.Victims<iqr_lower
outlier_total_iqr

#Plotting a histogram - Total
library(ggplot2)                                    	 
ggplot(Victims_Rape, aes(x=Total.Victims)) + 
  geom_histogram(binwidth=100, fill="blue") 

library (tidyverse)
library(ggplot2)

# box plot - Total
boxplot(Victims_Rape$Total.Victims, ylab = "Total Number of Victims")
quantile(Victims_Rape$Total.Victims, probs = c(0,0.25,0.5,0.75,1))  
  
########################################################################################################################################## 
  
#Child Total Victim 
#Using +/- 3*standard deviation - MDHYA PRADESH (2841)
sd_total1 <- sd(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)
sd_upper1 <- mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)+(3*sd_total1) #Anything up than this value is an outlier
sd_lower1 <- mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)-(3*sd_total1)
outlier_total_sd1 <- Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims>sd_upper1|Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims<sd_lower1
outlier_total_sd1

#Using z-scores - MADHYA PRADESH (2841)
zscore_total <- (Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims - mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims))/sd(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)
outlier_total_zscore <- zscore_total > 3 | zscore_total < (-3)
outlier_total_zscore

#Using +/- 1.5*IQR - MADHTYA PRADESH (2841)
quartile_total <- quantile(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims, names=FALSE)
iqr_total <- quartile_total[4]-quartile_total[2]
iqr_upper <- quartile_total[4]+(1.5*iqr_total)
iqr_lower <- quartile_total[2]-(1.5*iqr_total)
outlier_total_iqr <- Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims>iqr_upper|Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims<iqr_lower
outlier_total_iqr

#Plotting a histogram - Total
library(ggplot2)                                    	 
ggplot(Victims_Rape, aes(x=Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)) + 
  geom_histogram(binwidth=100, fill="blue") 

library (tidyverse)
library(ggplot2)

# box plot - Total
boxplot(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims, ylab = "Total Number of Child Victims")
quantile(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims, probs = c(0,0.25,0.5,0.75,1))  


##########################################################################################################################################  
  
##Age group for (0-5) 
#Using +/- 3*standard deviation - Uttar PRADESH (67)
sd_total1 <- sd(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years)
sd_upper1 <- mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years)+(3*sd_total1) #Anything up than this value is an outlier
sd_lower1 <- mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years)-(3*sd_total1)
outlier_total_sd1 <- Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years>sd_upper1|Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years<sd_lower1
outlier_total_sd1

#Using z-scores - Uttar PRADESH (67)
zscore_total <- (Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years - mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years))/sd(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years)
outlier_total_zscore <- zscore_total > 3 | zscore_total < (-3)
outlier_total_zscore

#Using +/- 1.5*IQR - Andra Prdesh (16), Chhattisgarh (41), Kerala (48), MADHTYA PRADESH (54), Rajastan (17), Uttar Pradesh (67) 
quartile_total <- quantile(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years, names=FALSE)
iqr_total <- quartile_total[4]-quartile_total[2]
iqr_upper <- quartile_total[4]+(1.5*iqr_total)
iqr_lower <- quartile_total[2]-(1.5*iqr_total)
outlier_total_iqr <- Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years>iqr_upper|Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years<iqr_lower
outlier_total_iqr

#Plotting a histogram - Total
library(ggplot2)                                    	 
ggplot(Victims_Rape, aes(x=Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years)) + 
  geom_histogram(binwidth=100, fill="blue") 

library (tidyverse)
library(ggplot2)

# box plot - Total
boxplot(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years, ylab = " Number of Victims (0-5)")
quantile(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....Below.6.Years, probs = c(0,0.25,0.5,0.75,1))  

##########################################################################################################################################
  
##Age group for (6-11) 
#Using +/- 3*standard deviation - Uttar Pradesh (174)
sd_total1 <- sd(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above.)
sd_upper1 <- mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above.)+(3*sd_total1) #Anything up than this value is an outlier
sd_lower1 <- mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above.)-(3*sd_total1)
outlier_total_sd1 <- Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above.>sd_upper1|Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above.<sd_lower1
outlier_total_sd1

#Using z-scores - Uttar PRADESH (174)
zscore_total <- (Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above. - mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above.))/sd(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above.)
outlier_total_zscore <- zscore_total > 3 | zscore_total < (-3)
outlier_total_zscore

#Using +/- 1.5*IQR - Andra Prdesh (57), Chhattisgarh (80), Kerala (129), MADHTYA PRADESH (142), Rajastan (48), Uttar Pradesh (174) 
quartile_total <- quantile(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above., names=FALSE)
iqr_total <- quartile_total[4]-quartile_total[2]
iqr_upper <- quartile_total[4]+(1.5*iqr_total)
iqr_lower <- quartile_total[2]-(1.5*iqr_total)
outlier_total_iqr <- Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above.>iqr_upper|Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above.<iqr_lower
outlier_total_iqr

#Plotting a histogram - (6-11)
library(ggplot2)                                    	 
ggplot(Victims_Rape, aes(x=Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above.)) + 
  geom_histogram(binwidth=100, fill="blue") 

library (tidyverse)
library(ggplot2)

# box plot - (6-11)
boxplot(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above., ylab = " Number of Victims (6-11)")
quantile(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....6.Years...Above., probs = c(0,0.25,0.5,0.75,1))  

##########################################################################################################################################


##########################################################################################################################################

##Age group for (12-15) 
#Using +/- 3*standard deviation - Madhya Pradesh (1143)
sd_total1 <- sd(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.)
sd_upper1 <- mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.)+(3*sd_total1) #Anything up than this value is an outlier
sd_lower1 <- mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.)-(3*sd_total1)
outlier_total_sd1 <- Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.>sd_upper1|Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.<sd_lower1
outlier_total_sd1

#Using z-scores - Madhya Pradesh (1143)
zscore_total <- (Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above. - mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.))/sd(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.)
outlier_total_zscore <- zscore_total > 3 | zscore_total < (-3)
outlier_total_zscore

#Using +/- 1.5*IQR - Andra Prdesh (181), Chhattisgarh (557), Kerala (334), MADHTYA PRADESH (1143), Punjab (151), Rajastan (392), Uttar Pradesh (570) 
quartile_total <- quantile(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above., names=FALSE)
iqr_total <- quartile_total[4]-quartile_total[2]
iqr_upper <- quartile_total[4]+(1.5*iqr_total)
iqr_lower <- quartile_total[2]-(1.5*iqr_total)
outlier_total_iqr <- Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.>iqr_upper|Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.<iqr_lower
outlier_total_iqr

#Plotting a histogram - (12-15)
library(ggplot2)                                    	 
ggplot(Victims_Rape, aes(x=Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above.)) + 
  geom_histogram(binwidth=100, fill="blue") 

library (tidyverse)
library(ggplot2)

# box plot - (12-15)
boxplot(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above., ylab = " Number of Victims (12-15)")
quantile(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....12.Years...Above., probs = c(0,0.25,0.5,0.75,1))  

##########################################################################################################################################

##Age group for (16-18) 
#Using +/- 3*standard deviation - Madhya Pradesh (1502)
sd_total1 <- sd(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above.)
sd_upper1 <- mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above.)+(3*sd_total1) #Anything up than this value is an outlier
sd_lower1 <- mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above.)-(3*sd_total1)
outlier_total_sd1 <- Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above.>sd_upper1|Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above.<sd_lower1
outlier_total_sd1

#Using z-scores - Madhya Pradesh (1502)
zscore_total <- (Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above. - mean(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above.))/sd(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above.)
outlier_total_zscore <- zscore_total > 3 | zscore_total < (-3)
outlier_total_zscore

#Using +/- 1.5*IQR - Andra Prdesh (251), Chhattisgarh (541), Kerala (645), MADHTYA PRADESH (1502), Rajastan (575), Uttar Pradesh (600), Uttarkand(186)
quartile_total <- quantile(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above., names=FALSE)
iqr_total <- quartile_total[4]-quartile_total[2]
iqr_upper <- quartile_total[4]+(1.5*iqr_total)
iqr_lower <- quartile_total[2]-(1.5*iqr_total)
outlier_total_iqr <- Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above.>iqr_upper|Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above.<iqr_lower
outlier_total_iqr

#Plotting a histogram - (16-18)
library(ggplot2)                                    	 
ggplot(Victims_Rape, aes(x=Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above.)) + 
  geom_histogram(binwidth=100, fill="blue") 

library (tidyverse)
library(ggplot2)

# box plot - (16-18)
boxplot(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above., ylab = " Number of Victims (16-18)")
quantile(Victims_Rape$Child.Victims.of.Rape..Below.18.Yrs....16.Years...Above., probs = c(0,0.25,0.5,0.75,1))  

##########################################################################################################################################

##Total Adult Victims
#Using +/- 3*standard deviation - Rajastan(3305)
sd_total1 <- sd(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)
sd_upper1 <- mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)+(3*sd_total1) #Anything up than this value is an outlier
sd_lower1 <- mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)-(3*sd_total1)
outlier_total_sd1 <- Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims>sd_upper1|Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims<sd_lower1
outlier_total_sd1

#Using z-scores - Rajastan(3305)
zscore_total <- (Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims - mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims))/sd(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)
outlier_total_zscore <- zscore_total > 3 | zscore_total < (-3)
outlier_total_zscore

#Using +/- 1.5*IQR - Madhya Pradesh (2609), Rajastan (3305), Uttar Pradesh (2911)
quartile_total <- quantile(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims, names=FALSE)
iqr_total <- quartile_total[4]-quartile_total[2]
iqr_upper <- quartile_total[4]+(1.5*iqr_total)
iqr_lower <- quartile_total[2]-(1.5*iqr_total)
outlier_total_iqr <- Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims>iqr_upper|Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims<iqr_lower
outlier_total_iqr

#Plotting a histogram - Total Adult Victims
library(ggplot2)                                    	 
ggplot(Victims_Rape, aes(x=Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)) + 
  geom_histogram(binwidth=100, fill="blue") 

library (tidyverse)
library(ggplot2)

# box plot - Total Adult Victims
boxplot(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims, ylab = " Number of Victims (19-29)")
quantile(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims, probs = c(0,0.25,0.5,0.75,1))  

##########################################################################################################################################

##Age group for (19-29) 
#Using +/- 3*standard deviation - None
sd_total1 <- sd(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.Years)
sd_upper1 <- mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.Years)+(3*sd_total1) #Anything up than this value is an outlier
sd_lower1 <- mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.Years)-(3*sd_total1)
outlier_total_sd1 <- Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.Years>sd_upper1|Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.Years<sd_lower1
outlier_total_sd1

#Using z-scores - Madhya Pradesh (1502)
zscore_total <- (Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.Years - mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.Years))/sd(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.Years)
outlier_total_zscore <- zscore_total > 3 | zscore_total < (-3)
outlier_total_zscore

#Using +/- 1.5*IQR - Rajastan (2263), Uttar Pradesh (603)
quartile_total <- quantile(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.Years, names=FALSE)
iqr_total <- quartile_total[4]-quartile_total[2]
iqr_upper <- quartile_total[4]+(1.5*iqr_total)
iqr_lower <- quartile_total[2]-(1.5*iqr_total)
outlier_total_iqr <- Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.Years>iqr_upper|Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.Years<iqr_lower
outlier_total_iqr

#Plotting a histogram - (19-29)
library(ggplot2)                                    	 
ggplot(Victims_Rape, aes(x=Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30.)) + 
  geom_histogram(binwidth=100, fill="blue") 

library (tidyverse)
library(ggplot2)

# box plot - (19-29)
boxplot(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30., ylab = " Number of Victims (19-29)")
quantile(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....18.Years...Above...Below.30., probs = c(0,0.25,0.5,0.75,1))  

##########################################################################################################################################

##Age group for (30-44) 
#Using +/- 3*standard deviation - RJASTAN (912) 
sd_total1 <- sd(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years)
sd_upper1 <- mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years)+(3*sd_total1) #Anything up than this value is an outlier
sd_lower1 <- mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years)-(3*sd_total1)
outlier_total_sd1 <- Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years>sd_upper1|Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years<sd_lower1
outlier_total_sd1

#Using z-scores -  RJASTAN (912) 
zscore_total <- (Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years - mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years))/sd(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years)
outlier_total_zscore <- zscore_total > 3 | zscore_total < (-3)
outlier_total_zscore

#Using +/- 1.5*IQR - MADHYA Pradesh (725), Rajastan (912), Uttar Pradesh (603)
quartile_total <- quantile(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years, names=FALSE)
iqr_total <- quartile_total[4]-quartile_total[2]
iqr_upper <- quartile_total[4]+(1.5*iqr_total)
iqr_lower <- quartile_total[2]-(1.5*iqr_total)
outlier_total_iqr <- Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years>iqr_upper|Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years<iqr_lower
outlier_total_iqr

#Plotting a histogram - (30-44)
library(ggplot2)                                    	 
ggplot(Victims_Rape, aes(x=Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years)) + 
  geom_histogram(binwidth=100, fill="blue") 

library (tidyverse)
library(ggplot2)

# box plot - (30-44)
boxplot(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years, ylab = " Number of Victims (30-44)")
quantile(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....30.Years...Above...Below.45.Years, probs = c(0,0.25,0.5,0.75,1))  

##########################################################################################################################################

##Age group for (45-59) 
#Using +/- 3*standard deviation - RJASTAN (129) 
sd_total1 <- sd(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years)
sd_upper1 <- mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years)+(3*sd_total1) #Anything up than this value is an outlier
sd_lower1 <- mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years)-(3*sd_total1)
outlier_total_sd1 <- Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years>sd_upper1|Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years<sd_lower1
outlier_total_sd1

#Using z-scores -  RJASTAN (129) 
zscore_total <- (Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years - mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years))/sd(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years)
outlier_total_zscore <- zscore_total > 3 | zscore_total < (-3)
outlier_total_zscore

#Using +/- 1.5*IQR - Assam (103), Rajastan (129), Uttar Pradesh (70)
quartile_total <- quantile(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years, names=FALSE)
iqr_total <- quartile_total[4]-quartile_total[2]
iqr_upper <- quartile_total[4]+(1.5*iqr_total)
iqr_lower <- quartile_total[2]-(1.5*iqr_total)
outlier_total_iqr <- Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years>iqr_upper|Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years<iqr_lower
outlier_total_iqr

#Plotting a histogram - (45-59)
library(ggplot2)                                    	 
ggplot(Victims_Rape, aes(x=Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years)) + 
  geom_histogram(binwidth=100, fill="blue") 

library (tidyverse)
library(ggplot2)

# box plot - (45-59)
boxplot(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years, ylab = " Number of Victims (45-59)")
quantile(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....45.Years...Above...Below.60.Years, probs = c(0,0.25,0.5,0.75,1))  

##########################################################################################################################################

##Age group for (60+) 
#Using +/- 3*standard deviation - Kerela (14) 
sd_total1 <- sd(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above)
sd_upper1 <- mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above)+(3*sd_total1) #Anything up than this value is an outlier
sd_lower1 <- mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above)-(3*sd_total1)
outlier_total_sd1 <- Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above>sd_upper1|Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above<sd_lower1
outlier_total_sd1

#Using z-scores -  Kerela (14) 
zscore_total <- (Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above - mean(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above))/sd(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above)
outlier_total_zscore <- zscore_total > 3 | zscore_total < (-3)
outlier_total_zscore

#Using +/- 1.5*IQR - 	Chhattisgarh (6), Kerela (14), Madhya Pradesh (11), Mahrastra (11)
quartile_total <- quantile(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above, names=FALSE)
iqr_total <- quartile_total[4]-quartile_total[2]
iqr_upper <- quartile_total[4]+(1.5*iqr_total)
iqr_lower <- quartile_total[2]-(1.5*iqr_total)
outlier_total_iqr <- Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above>iqr_upper|Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above<iqr_lower
outlier_total_iqr

#Plotting a histogram - (60+)
library(ggplot2)                                    	 
ggplot(Victims_Rape, aes(x=Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above)) + 
  geom_histogram(binwidth=100, fill="blue") 

library (tidyverse)
library(ggplot2)

# box plot - (60+)
boxplot(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above, ylab = " Number of Victims (60+)")
quantile(Victims_Rape$Women.Victims.of.Rape..Above.18.Yrs....60.Years...Above, probs = c(0,0.25,0.5,0.75,1))  

##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################

#Subset auto data to only include districts that have a total number of women victims  >= 1000
totalWomen <- subset(Victims_Rape, Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims>=1000)
as.data.frame(totalWomen)
View(totalWomen)

library(ggplot2)
#Visualize the distributions of number victims over the subsets
ggp1 <- ggplot(totalWomen, aes(x=Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)) + 
  geom_density() + 
  xlim(0,6000) + 
  xlab("Total Number of Women Victims")


#Descriptive Statistics - Total Number of Adult Women Victims >=1000 
min(totalWomen$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)
max(totalWomen$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)
mean(totalWomen$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)
median(totalWomen$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)
var(totalWomen$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)
sd(totalWomen$Women.Victims.of.Rape..Above.18.Yrs....Total.Women.Adult.Victims)


#Subset auto data to only include districts that have a total number of child victims  >= 1000
totalChild <- subset(Victims_Rape,Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims>=1000)
as.data.frame(totalChild)
View(totalChild)


library(ggplot2)
#Visualize the distributions of number victims over the subsets
ggp1 <- ggplot(totalChild, aes(x=Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)) + 
  geom_density() + 
  xlim(0,6000) + 
  xlab("Total Number of Child Victims")


#Descriptive Statistics - Total Number of Child Women Victims >=1000 
min(totalChild$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)
max(totalChild$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)
mean(totalChild$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)
median(totalChild$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)
var(totalChild$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)
sd(totalChild$Child.Victims.of.Rape..Below.18.Yrs....Total.Girl.Child.Victims)


#Subset auto data to only include districts that have a total number of victims  >= 1000
total <- subset(Victims_Rape, Total.Victims>=1000)
as.data.frame(total)
View(total)

library(ggplot2)
#Visualize the distributions of number victims over the subsets
ggp1 <- ggplot(total, aes(x=Total.Victims)) + 
  geom_density() + 
  xlim(0,6000) + 
  xlab("Total Number of Victims")


#Descriptive Statistics - Total Number of Victims >=1000 
min(total$Total.Victims)
max(total$Total.Victims)
mean(total$Total.Victims)
median(total$Total.Victims)
var(total$Total.Victims)
sd(total$Total.Victims)


#Convert a categorical variable to a set of flag variables.
###########################################################
Victims_Rape$Category_1_flag[Victims_Rape$Category=="State"] <- 1 #District is ruled by State = 1 
Victims_Rape$Category_1_flag[Victims_Rape$Category!="State"] <- 0 # District is not ruled by State = 0

#Vice Versa of setting categorial variable to a set of flag variables 
Victims_Rape$Category_2_flag[Victims_Rape$Category=="Union Territory"] <- 1 #District is ruled by State = 1 
Victims_Rape$Category_2_flag[Victims_Rape$Category!="Union Territory"] <- 0 # District is not ruled by State = 0



#Convert a numeric variable to a categorical variable using binning.
#####################################################################
#Use cut() functions to convert the data into 5 (adjust this for more bins) equal width bins and convert to factors
Victims_Rape$Total.Victims<- as.factor(cut(Victims_Rape$Total.Victims,5, labels=FALSE))
#Visualize the resulting categorical variable with a bar plot
ggplot(Victims_Rape, aes(x=Total.Victims)) + 
  geom_bar()





