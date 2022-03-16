#Setting the working directory

getwd()

setwd('D:/DOCUMENTS/process documents/IUPUI/Spring 2021/Applied Statistics/Project/HF')
getwd()

#installing the required packages

#install.packages('tidyverse')
#install.packages('ggplot2')
#install.packages("ggpubr")
#install.packages("corrplot")
#install.packages("mltools")
#install.packages('fastDummies')
#install.packages('PerformanceAnalytics')
#install.packages('plotROC')
#install.packages('pROC')


library('fastDummies')

library('mltools')

library('ggplot2')
library("ggpubr")

library("plotROC")
library("corrplot")

library('pROC')

library("ISLR")

library("SmartEDA")

library('PerformanceAnalytics')

#reading the data

my_data <- read.csv('heart_failure_clinical_records_dataset.csv')

#Looking at the data

head(my_data)

tail(my_data)

#Looking for null values

sum(is.na(my_data))

mean(is.na(my_data))

descriptive <- ExpData(data=my_data,type=2, fun = c("mean", "median", "var"))
descriptive

#Looking at the summary of the data variables

summary(my_data)

sapply(my_data, sd)

#EXPLORATORY DATA ANALYSIS
#Numerical Variables

numsum <- ExpNumStat(my_data,by="A",gp=NULL,Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2,Nlim=10)
numsum

#Density plot (Univariate)

plot1 <- ExpNumViz(my_data,target=NULL, nlim=10,Page=c(3,2),sample=6)
plot1[[1]]

#frequency for all categorical independent variables

catsum <- ExpCTable(my_data,Target=NULL,margin=1,clim=10,nlim=3,round=2,bin=NULL,per=T)
catsum

#Bar plots for all categorical variables

plot4 <- ExpNumViz(my_data,target="DEATH_EVENT",type=1,nlim=3,fname=NULL,col=c("darkgreen", 'blue', 'red'),Page=c(2,2),sample=4)
plot4[[1]]

#Plotting histogram for the continous variables

attach(my_data)

plot(age, creatinine_phosphokinase, main="Age & Creatinine Phosphokinase",
     xlab="Age ", ylab="creatinine phosphokinase", pch=20, col="red", cex=1)

hist(my_data$ejection_fraction, main="Histogram for Ejection Fraction",
     xlab="Ejection fraction", border="black", 
     col="blue",xlim=c(10,80),las=1, breaks=8)

plot(age, ejection_fraction, main="Age & Ejection Fraction",
     xlab="Age ", ylab="ejection fraction", pch=21, col="blue", cex=1)

plot(age, platelets, main="Age & Platelets",
     xlab="Age ", ylab="platelets", pch=16, col="blue", cex=1)

plot(age, serum_creatinine, main="Age & Serum Creatinine",
     xlab="Age ", ylab="serum creatinine", pch=19, col="blue", cex=1)

plot(age, serum_sodium, main="Age & Serum Sodium",
     xlab="Age ", ylab="serum sodium", pch=2, col="blue", cex=1)

plot(serum_sodium, serum_creatinine, main="Serum Sodium & Serum Creatinine",
     xlab="Serum Sodium", ylab="Serum Creatinine", pch=24, col="blue", cex=1)

plot(serum_sodium, ejection_fraction, main="Serum Sodium & Ejection Fraction ",
     xlab="Serum Sodium", ylab="Ejection Fraction", pch=19, col="blue", cex=1)

plot(serum_creatinine, ejection_fraction, main="Serum Creatinine & Ejection Fraction",
     xlab="Serum Creatinine", ylab="Ejection Fraction", pch=24, col="blue", cex=1)

#Now let's check the correlation between variables to see how they are correlated.


data1 <- subset (my_data, select = -(DEATH_EVENT))
data1 <- subset (data1, select = -(time))

cc = cor(data1, method = "pearson")


#Plotting the correlation heat map

corrplot(cc, order = "hclust", hclust.method = "average", addrect = 4, tl.cex = 0.8)

############################################################################################

chart.Correlation(data1,
                  method="pearson",
                  histogram=TRUE,
                  pch=16)

et4 <- ExpCatStat(data1,Target="deatheventnum",result = "Stat",clim=10,nlim=5,bins=10,Pclass="Yes",plot=FALSE,top=20,Round=2)

et4

#QQ PLot

options(width = 150)
CData = data1
qqp <- ExpOutQQ(CData,nlim=10,fname=NULL,Page=c(3,2),sample=6)
qqp[[1]]

#Mann whiteny test

#Compute two-samples Wilcoxon test  

###The Mann–Whitney U test (or Wilcoxon rank–sum test), applied to each feature in relation 
#  to the death event target, detects whether we can reject the null hypothesis that the distribution of the each feature for the groups of samples defined by death event are the same.


# For Age
res <- wilcox.test(age ~ deatheventnum , data = data1,
                   exact = FALSE)
res


# here, the p-value is 0.0001668, which is less than 0.05 or close to zero indicating  
# that Age is strongly related to death event


#For ejection fraction
res <- wilcox.test(ejection_fraction ~ deatheventnum , data = data1,
                   exact = FALSE)
res


# here, the p-value is 7.368e-07, which is less than 0.05 or close to zero indicating  
# that ejection_fraction is strongly related to death event

# For Sex

res <- wilcox.test(sex ~ deatheventnum , data = data1,
                   exact = FALSE)
res

# here, the p-value is 0.9413, which is greater than 0.05 or close to 1 indicating  
# that sex is not related to death event.

# For Serum creatinine

res <- wilcox.test(serum_creatinine ~ deatheventnum , data = data1,
                   exact = FALSE)
res

# For Serum creatinine

res <- wilcox.test(serum_creatinine ~ deatheventnum , data = data1,
                   exact = FALSE)
res

# For Serum Sodium
res <- wilcox.test(serum_sodium ~ deatheventnum , data = data1,
                   exact = FALSE)
res


#Creating the Rank Table for features using the Wilcoxon Rank Sum Test results
Variables <- c('Sr. Creatinine','Ejection Fraction','Age', 'Sr. Sodium', 'Sex')
P_Value <- c(0.000000000158, 0.0000007368, 0.0001668, 0.0002928, 0.9413)
Rank <- (c(1, 2, 3, 4, 5))
wilcoxrank <- data.frame(Variables, P_Value, Rank)
wilcoxrank

