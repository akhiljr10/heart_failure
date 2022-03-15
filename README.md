# heart_failure
Exploratory Data Analysis of heart_failure dataset using R

The heart failure dataset (HF Dataset) used for this study was clean and did not require much preprocessing. The dataset was looked for null values, and there were none of them. The outcome variable under consideration was binary of character type, and we converted it into numerical binary variables for statistical analysis. Hence the time variable was insignificant to answer the research question; we dropped that variable from the dataset and continued with the rest of the variables for analysis and modeling. 

The data exploration of the datasets was done using the RStudio. Different packages were used for the exploration to understand the data. The package mainly used included SmartEDA. SmartEDA package is primarily used for exploratory data analysis, and the package has an advantage over other packages as it automates the process by differentiating the numerical and categorical variables. We used descriptive statistics, line graphs, and boxplots to visualize the data for better understanding. We used line graphs for visualizing the numerical variables and boxplots to visualize the categorical variables. Line graphs showed skewness of the variables, and boxplots showed the outliers in the variables. Quantile-Quantile plot (QQ plot) was used to understand the normality of the variables. Most of the variables were normally distributed.


Statistical analysis of the HF dataset was done using the RStudio. To understand which variables are having a relationship with the target variable, we performed a Pearson correlation and plotted the correlation heatmap. To better understand and confirm the relationships between the variables and the outcome, we also performed a chi-square test. The null hypothesis stated for the chi-square test was that there is no association between the variables and the target, and the alternative hypothesis was that there exists a degree of association between the variable and target, and the α-value to be considered was set to <0.05. Hence one of the study's objectives was to rank the significant features of the outcome variable; we performed a Wilcoxon rank-sum test to rank the variables according to their importance with the outcome variable.

I made the predictive models using logistic regression to predict the clinical outcomes of HF using the variables that were identified as significantly associated from the Pearsons correlation, Chi-square test, and Wilcoxon rank-sum test. In the predictive model for HF, initially, we made two models using different features as the predictors. In the first model, we used all the variables as independent variables and the clinical outcome as the dependent variable. In the second model, we used the top two variables identified by the Wilcoxon rank-sum test. Ten-fold cross-validation was also done to eliminate the chances of overfitting of the model. An area under the curve (AUC) was plotted to understand the performance of the models, and we performed the analysis of deviance to understand the measures of the discrepancy between the two models. To further improve the model and the findings of our study, we performed the principal component analysis (PCA) on the HF dataset and plotted the same. Principal components from the PCA were used as the independent variables to build another logistic regression prediction model and were compared with the initial model using various matrices to understand the performance. Based upon the significance of each variable used to build the model as obtained from the model summary, we tried removing and or replacing the non-significant variables with another significant variable and looked for the improvement in the performance of the models.
