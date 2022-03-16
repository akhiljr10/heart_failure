#Heart Failure Dataset PCA and Logistic Regression

#install.packages('caret')
#install.packages('MKmisc')
#install.packages('ResourceSelection')
#install.packages('ROCR')
#install.packages('lmtest')
#install.packages('devtools')
#install_github("vqv/ggbiplot")
#install.packages('broom')
#install.packages('dplyr')
#install.packages('tidyverse')

library('tidyverse')

library('dplyr')
library('broom')

library('devtools')

library('ResourceSelection')

library('caret')

library('MKmisc')

library('ROCR')

library('lmtest')

library('ggbiplot')

getwd()

setwd('D:/DOCUMENTS/process documents/IUPUI/Spring 2021/Applied Statistics/Project/HF')
getwd()

mydata <- read.csv('heart_failure_clinical_records_dataset.csv')

mydata <- subset (mydata, select = -(time))

#We encoded the DEATH_EVENT column which was character to binary numeric variable

mydata <- subset (mydata, select = -(DEATH_EVENT))

head(mydata)

#mydata <- sapply(finaldf, as.numeric)

pca <- prcomp(mydata, center = TRUE,scale. = TRUE)

#The PCA table for Heart Failure Data Set

pca

#The PCA PLOT
# pca.plot <- autoplot(pca, data = mydata, colour = 'deatheventnum', loadings = TRUE,
#                      loadings.colour = 'red',
#                      loadings.label = TRUE, loadings.label.size = 3)
# pca.plot

#with(mydata,table(deatheventnum))

#Split the data.

Train <- createDataPartition(mydata$deatheventnum, p=0.8, list=FALSE)
training <- mydata[ Train, ]
testing <- mydata[ -Train, ]

#Fit the logistic regression model, that is a GLM using a binomial link, using the caret function train().

mod_fit <- train(deatheventnum ~ age + sex + 
                   ejection_fraction + serum_sodium + serum_creatinine, 
                 data=training, method="glm", family="binomial")

#Transform the coeficients from log-odds to odds.

exp(coef(mod_fit$finalModel))

#Predict.

predict(mod_fit, newdata=testing)

#predict(mod_fit, newdata=testing, type="prob")

#Model Evaluation and Diagnostics

#Fit two models with the R function glm().

# class(training$deatheventnum)
# training$deatheventnum <- as.factor(training$DEATH_EVENT)
# testing$deatheventnum <- as.factor(testing$DEATH_EVENT)

mod_fit_one <- glm(deatheventnum ~ age + creatinine_phosphokinase + ejection_fraction +
                     serum_creatinine + serum_sodium + anaemia + diabetes + high_blood_pressure
                   + sex + smoking, data=training, family="binomial")

anova(mod_fit_one)

mod_fit_two <- glm(deatheventnum ~ serum_creatinine + ejection_fraction
                   , data=training, family="binomial")

anova(mod_fit_two) 

anova(mod_fit_one, test ="Chisq")

anova(mod_fit_one, mod_fit_two, test ="Chisq")

#Validation of Predicted Values
#Classification Rate
#Accuracy.

pred = predict(mod_fit_one, newdata=testing)

accuracy <- table(pred, as.factor(testing[,"deatheventnum"]))
accuracy

sum(diag(accuracy))/sum(accuracy)

#Confussion Matrix.

pred = as.factor(predict(mod_fit_one, newdata=testing))

#confusionMatrix(data = pred, as.factor(testing[,'deatheventnum']))

#ROC

prob <- predict(mod_fit_two, newdata=testing, type="response")
pred <- prediction(prob, testing$deatheventnum)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)

auc <- performance(pred, measure = "auc")
class(auc)

auc <- auc@y.values[[1]]
auc

prob <- predict(mod_fit_one, newdata=testing, type="response")
pred <- prediction(prob, testing$deatheventnum)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, criterion = "MSEP", sd = TRUE)

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

#K-Fold Cross Validation
#Split the data into k folds.

ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- train(deatheventnum ~ ejection_fraction + serum_creatinine, 
                 data=mydata, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 8)

pred <- predict(mod_fit, newdata=testing)

#confusionMatrix(data=pred, testing$deatheventnum)

#Plotting AUC after K fold cross validation
prob <- predict(mod_fit, newdata=testing, type="raw")
pred <- prediction(prob, testing$deatheventnum)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, criterion = "MSEP", sd = TRUE)

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Modeling after PCA

components <- cbind(deatheventnum = mydata[, "deatheventnum"], pca$x[, 1:8]) %>%
  as.data.frame()

fit_1 <- glm(deatheventnum ~ age + ejection_fraction +
               serum_creatinine, data = mydata)

fit_2 <- glm(mydata$deatheventnum ~ components$PC1 + components$PC2 + components$PC3 + 
               components$PC4 + components$PC5 + components$PC6 + components$PC7 
             + components$PC8)

summary(fit_1)

summary(fit_2)

glance(fit_1)

glance(fit_2)

predictions <- fit_1 %>% predict(training)

data.frame(
  R2 = R2(predictions, training$deatheventnum),
  RMSE = RMSE(predictions, training$deatheventnum),
  MAE = MAE(predictions, training$deatheventnum)
)

predictions <- fit_2 %>% predict(mydata)
data.frame(
  R2 = R2(predictions, mydata$deatheventnum),
  RMSE = RMSE(predictions, mydata$deatheventnum),
  MAE = MAE(predictions, mydata$deatheventnum)
)

#Hence PC5 is not statistically significant, we dropped it from the model
fit_2 <- glm(mydata$deatheventnum ~ components$PC1 + components$PC2 + components$PC3 + 
               components$PC4 + components$PC6 + components$PC7 
             + components$PC8)


predictions <- fit_1 %>% predict(training)

data.frame(
  R2 = R2(predictions, training$deatheventnum),
  RMSE = RMSE(predictions, training$deatheventnum),
  MAE = MAE(predictions, training$deatheventnum)
)

predictions <- fit_2 %>% predict(mydata)
data.frame(
  R2 = R2(predictions, mydata$deatheventnum),
  RMSE = RMSE(predictions, mydata$deatheventnum),
  MAE = MAE(predictions, mydata$deatheventnum)
)