rm(list=ls())
setwd('/Users/divyasharma/Documents/Edwisor/Project_2')
getwd()
library(readxl)
df <- read_excel("Absenteeism_at_work_Project.xls")
View(df)
str(df)
summary(df)

#Missing-Value Analysis
missing_val = data.frame(sapply(df, function(x){sum(is.na(x))}))
print(missing_val)
rm(missing_val)

#Install Packages
x = c("forcats","dplyr","ggplot2","plyr","DataExplorer","grid","ggthemes","gridExtra","factoextra","FactoMineR")
install.packages(x)
rm(x)
y = c("DMwR","rpart","MASS","usdm","corrgram","Metrics")
install.packages(y)
rm(y)

#KNN Imputation
library(DMwR)
df <- as.data.frame(df)
df = knnImputation(df, k = 5)
View(df)
#rm(df)
#r <- unique(df$Work.load.Average.day)

#Find unique values
#gunique = unique(df$`Absenteeism time in hours`)
#len <- length(gunique)
#rm(un)
#symnum(cor(df))
df <- data.frame(lapply(df, function(x){round(x, digits = 0)}))
View(df)

#Convert numeric Categorical variables to factor to plot 

# df$Education <- as.factor(df$Education)
# df$Disciplinary.failure <- as.factor(df$Disciplinary.failure)
# df$Social.drinker <- as.factor(df$Social.drinker)
# df$Social.smoker <- as.factor(df$Social.smoker)
# df$Pet <- as.factor(df$Pet)
# df$Day.of.the.week <- as.factor(df$Day.of.the.week)
# df$Seasons <- as.factor(df$Seasons)
# df$Son <- as.factor(df$Son)

# #Data-Visualization
# library(forcats)
# library(dplyr)
# library(ggplot2)
# library(plyr)
# library(DataExplorer)
# library(ggthemes)
# library(grid)
# library(gridExtra)
# library(factoextra)
# library(FactoMineR)
# 
# p <- ggplot(df, aes(x = Pet, fill = Pet)) + geom_bar() 
# s <- ggplot(df, aes(x = Son, fill = Son)) + geom_bar()
# t <- ggplot(df, aes(x = Seasons, fill = Seasons )) + geom_bar()
# r <- ggplot(df, aes(x = Education, fill = Education)) + geom_bar()
# u <- ggplot(df, aes(x = Disciplinary.failure, fill = Disciplinary.failure)) + geom_bar()
# v <- ggplot(df, aes(x = Social.drinker, fill = Social.drinker)) + geom_bar()
# w <- ggplot(df, aes(x = Social.smoker, fill = Social.smoker)) + geom_bar() 
# x <- ggplot(df, aes(x = Day.of.the.week, fill = Day.of.the.week)) + geom_bar() 
# z <- ggplot(df, aes(x =  df$Hit.target, y =  df$Absenteeism.time.in.hours)) + geom_point()      
# grid.arrange(z, nrow=1)
# grid.arrange(p,s, nrow=1)
# grid.arrange(t,r, nrow=1)
# grid.arrange(u,v, nrow=1)
# grid.arrange(w,x, nrow=1)
# 
# hist(df$Reason.for.absence, col='yellow')
#df$Transportation.expense <- log10(df$Transportation.expense)
#cor(x=df$Transportation.expense, y= df$Absenteeism.time.in.hours, method="pearson")

#sp<-ggplot(df, aes(x=Absenteeism.time.in.hours, y=df$Month.of.absence, color=Reason.for.absence)) + geom_point()
#sp

# plot(df$Absenteeism.time.in.hours, df$Body.mass.index,
#      main = "Scatter plot",
#      xlab = "Absenteeism", ylab = "Height", 
#      col = c("dark green", "red", "orange"))


##Normalization
for(i in 1:ncol(df)){
  print(i)
  df[,i]=(df[,i]-min(df[,i]))/(max(df[,i]-min(df[,i])))
}

#VIF Test
library(usdm)
vif(df[,-19])
vifcor(df[,-19], th = 0.9)

#Correlation plot
library(corrgram)
numeric_index = sapply(df,is.numeric)
corrgram(df[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Dimension Reduction
#df = subset(df, select = -c(Service.time,Weight,Seasons))

#Decision-Tree

library(rpart)
library(MASS)
set.seed(123)
train_index = sample(1:nrow(df), 0.8 * nrow(df))
train = df[train_index,]
test = df[-train_index,]

#rpart for regression
fit = rpart(Absenteeism.time.in.hours ~ ., data = train, method = "anova")

#Predict for new test cases
predictions_DT = predict(fit, test[,-21])

#MAPE
#calculate MAPE
#MAPE = function(y, yhat){
 # mean(abs((y - yhat)/y))
#}

#MAPE(test[,19], predictions_DT)

#Calculate RMSE
library(Metrics)
rmse(test[,21],predictions_DT)

#Error rate: 10.44
#Accuracy: 89.56

## Chi-squared Test of Independence
#print(chisq.test(table(df$Social.drinker,df$Social.smoker)))


#run regression model
library(DMwR)
set.seed(123)
lm_model = lm(Absenteeism.time.in.hours ~., data = train)

#Summary of the model
summary(lm_model)
#install.packages("kableExtra")
#library(kableExtra)
#kable(anova(lm_model), booktabs = T)
#df = subset(df, select = -c(Work.load.Average.day))


#Predict
predictions_LR = predict(lm_model, test[,1:20])

#Calculate RMSE
library(Metrics)
rmse(test[,21],predictions_LR)

#Error Rate: 10.454
#Accuracy: 89.546


#KNN Imputation
library(class)
#Predict Test Data
KNN_Predictions = knn(train[,1:20], test[,1:20], train$Absenteeism.time.in.hours,
                      k=5)
KNN_Predictions <- as.numeric(KNN_Predictions)
rmse(test[,21],KNN_Predictions)

#With Normalisation
#Error: 5.88
#Accuracy: 94.12

