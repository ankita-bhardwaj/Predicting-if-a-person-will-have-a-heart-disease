
hd_data <- read.csv("Cleveland_hd.csv")
head(hd_data,5)

#install.packages('tidyverse')

# creating a variable hd which is 0 when there is no disease and 1 for all levels of disease
library(tidyverse)
hd_data %>% mutate(hd = ifelse(class > 0, 1, 0))-> hd_data
hd_data %>% mutate(sex = factor(sex, levels = 0:1, labels = c('Female','Male')))-> hd_data

# Effect of sex on the heart disease? Sex is a binary variable in this dataset,
# so the appropriate test is chi-squared test
attach(hd_data)
hd_sex <- chisq.test(sex, hd)

# Does age have an effect? Age is continuous, so we use a t-test
hd_age <- t.test(age~hd)

# What about thalach? Thalach is continuous, so we use a t-test
hd_heartrate <- t.test(thalach~hd)

# similarly, all variables can be evaluated but these 3 variables 
#were very significant in detecting the heart disease of a person.
print(hd_sex)
print(hd_age)
print(hd_heartrate)

# Recoding hd to be labelled
hd_data%>% mutate(hd_labelled = ifelse(hd == 0, "No Disease", "Disease")) -> hd_data

# plotting the above three variables to see their significance visually.
ggplot(data = hd_data, aes(x = hd_labelled,y = age)) + geom_boxplot()

ggplot(data = hd_data,aes(x=hd_labelled,fill=sex)) + geom_bar(position="fill") + ylab("Sex %")

ggplot(data = hd_data, aes(x = hd_labelled,y = thalach)) + geom_boxplot()
# All three variables are significant and hence next step is running a logistic regression on them.

model <- glm(data = hd_data, hd~age+sex+thalach, family="binomial")

summary(model)

# getting the predicted probability
pred_prob <- predict(model,hd_data, type = "response")

#creating a threshold. Can be done using CV but here could get it manually as was close to 0.5.
hd_data$pred_hd <- ifelse(pred_prob>=0.45,1,0)

# Predicting for one data point
newdata <- data.frame(age = 45, sex = "Female", thalach = 150)

# predict probability for this new case and print out the predicted value
p_new <- predict(model,newdata, type = "response")
p_new

# Next step is evaluating the model based on some scores.
#install.packages('Metrics')
library(Metrics)

# calculate auc, accuracy, clasification error
auc <- auc(hd_data$hd,hd_data$pred_hd)
accuracy <- accuracy(hd_data$hd,hd_data$pred_hd)
classification_error <- ce(hd_data$hd,hd_data$pred_hd)

print(paste("AUC=", auc))
print(paste("Accuracy=", accuracy))
print(paste("Classification Error=", classification_error))

# confusion matrix
table(hd_data$hd,hd_data$pred_hd, dnn=c('True Status','Predicted Status')) # confusion matrix

# we are able to predict the accuracy of chances of a person to have a heart disease with 70% which can be significant in medical industry.