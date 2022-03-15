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

library('fastDummies')
library('mltools')
library('tidyverse')
library('ggplot2')
library("ggpubr")
library("corrplot")
library("plotROC")

#reading the data

my_data <- read.csv('heart_failure_clinical_records_dataset.csv')

#Looking at the data

head(my_data)
tail(my_data)

#Looking for null values

sum(is.na(my_data))
mean(is.na(my_data))

#Looking at the summary of the data variables

summary(my_data)

sapply(my_data, sd)

#Plotting histogram for the continous variables

attach(my_data)
plot(age, creatinine_phosphokinase, main="Age & Creatinine Phosphokinase",
     xlab="Age ", ylab="creatinine phosphokinase", pch=20, col="red", cex=1)

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


scatterplot(serum_creatinine ~ ejection_fraction | DEATH_EVENT, data=my_data,
            main="Serum Creatinine & Ejection Fraction ",
            xlab="Ejection Fraction", ylab="Serum Creatinine")

#The column named 'time' does not seems to make any sense for analysis,
#thus we dropped that column from the data frame

df = subset(my_data, select = -c(time))
df

#Now let's check the correlation between variables to see how they are correlated.

cc = cor(df, method = "spearman")

#Plotting the correlation heat map

corrplot(cc, order = "hclust", hclust.method = "average", addrect = 4, tl.cex = 0.7)

library(PerformanceAnalytics)

chart.Correlation(my_data,
                  method="spearman",
                  histogram=TRUE,
                  pch=16)


#one-hot encoding the categorical varibales

df <- dummy_cols(df, select_columns = c('anaemia', 'diabetes', 'high_blood_pressure', 'sex', 'smoking'), remove_selected_columns = TRUE)
df


#LOGISTIC REGRESSION

table(my_data$DEATH_EVENT)


# Create Training Data
input_death <- df[which(df$DEATH_EVENT == 1), ]  # all 1's
input_alive <- df[which(df$DEATH_EVENT == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples
input_death_training_rows <- sample(1:nrow(input_death), 0.7*nrow(input_death))  # 1's for training
input_alive_training_rows <- sample(1:nrow(input_alive), 0.7*nrow(input_alive))  # 0's for training. Pick as many 0's as 1's
training_death <- input_death[input_death_training_rows, ]  
training_alive <- input_alive[input_alive_training_rows, ]
trainingData <- rbind(training_death, training_alive)  # row bind the 1's and 0's 

# Create Test Data
test_death <- input_death[-input_death_training_rows, ]
test_alive <- input_alive[-input_alive_training_rows, ]
testData <- rbind(test_death, test_alive)  # row bind the 1's and 0's 

model.final = glm(DEATH_EVENT ~ age + creatinine_phosphokinase + ejection_fraction +
                    serum_creatinine + serum_sodium + anaemia_0 + anaemia_1 + diabetes_0 + diabetes_1
                  + high_blood_pressure_0 + high_blood_pressure_1 + sex_0 + sex_1 + smoking_0 + smoking_1, 
                  data=trainingData,
                  family = binomial(link="logit"))

predicted <- plogis(predict(model.final, testData))

summary(model.final)


plotROC(testData$DEATH_EVENT, predicted)


basicplot <- ggplot(predicted, aes(d = D, m = M1)) + geom_roc()
basicplot

