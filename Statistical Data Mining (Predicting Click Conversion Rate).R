R code for the analysis:
rm(list = ls())
library(rio)
df = import("Final SDM Project File.xlsx")
View(df)
str(df)
table(df$CURRENCY)
table(df$BRAND)
table(df$SHOP_LOCATION)
table(df$CATEGORY)
#Check for NULLs
colSums(is.na(df))
#review rating, review count, sold, historical sold, Liked count has NULLs
#show_count has lot of NULLS so dropping them from analysis
df$SHOW_DISCOUNT = NULL
#removing NULLs
df <- df[complete.cases(df),]
colSums(is.na(df))
#subset the data by removing sold = 0 and view_count = 0 from dataset
df = subset(df, df$SOLD!=0 | df$VIEW_COUNT!=0)
df = subset(df, df$DEPARTMENT == "Men Clothes")
table(df$DEPARTMENT)
View(df)
#factors
df$CATEGORY = factor(df$CATEGORY)
df$SHOP_LOCATION = factor(df$SHOP_LOCATION)
df$DEPARTMENT = factor(df$DEPARTMENT)
df$CURRENCY = factor(df$CURRENCY)
#controlling for time, we need to extract year and month from create time variable
df$CREATE_TIME_YEAR = format(df$CREATE_TIME, "%Y")
df$CREATE_TIME_YEAR = as.factor(df$CREATE_TIME_YEAR)
df$CREATE_TIME_MONTH = format(df$CREATE_TIME, "%b")
df$CREATE_TIME_MONTH = as.factor(df$CREATE_TIME_MONTH)
#Predictors
#Sale price
#review rating
#review count
#historical sold
#stock
#discount %
#liked_count
#Create_year
#category
#response variable - conversion
df$conversion = df$SOLD/df$VIEW_COUNT
#conversion rate in %
df$conv_rate = df$conversion*100
df$conv_rate = round(df$conv_rate,0)
summary(df$conv_rate)
#sampling data
set.seed(12)
index = sample(1:nrow(df), size = 0.5*nrow(df), replace = F)
df_sample = df[index,]
dim(df_sample)
#exploratory data analysis
#histogram of conversion rate
hist(df_sample$conv_rate, col = "red", probability = T ,
     main = "Hist of conv_rate")
hist(log(df_sample$conv_rate), col = "red", probability = T,
     main = "Hist of log(conv_rate)")
table(df$CATEGORY)
table(df$DEPARTMENT)
table(df$CREATE_TIME_YEAR)
#data visualization
library(lattice)
bwplot(~df_sample$conversion | df_sample$CATEGORY)
boxplot(df_sample$conv_rate ~ df_sample$CATEGORY,NAMES=NULL,
        xlab = "categories", ylab = "Conversion rate")
bwplot(~df_sample$conv_rate | df_sample$CREATE_TIME_YEAR, xlab = "conversion rate")
#check for correlations
df_num = c("SALE_PRICE","REVIEW_RATING","REVIEW_COUNT","LIKED_COUNT",
           "DISCOUNT","conv_rate")
library(PerformanceAnalytics)
chart.Correlation(df_sample[,df_num])
## Models
summary(df$conv_rate)
#Conversion rate is a censored data
#Using tobit model
#Since DV: Conversion is sold/impression which is count/count
# a percentage, therefore, we can use OLS regression
#base model - linear model withh all predictors
lm1 = lm(conv_rate ~ SALE_PRICE + REVIEW_RATING + REVIEW_COUNT + LIKED_COUNT +
           HISTORICAL_SOLD + STOCK + DISCOUNT + CATEGORY + CREATE_TIME_YEAR,
         data = df_sample)
summary(lm1)
#tobit model
library(AER)
tobit1 = tobit(conv_rate ~ SALE_PRICE + REVIEW_RATING + REVIEW_COUNT +
                 LIKED_COUNT + HISTORICAL_SOLD + DISCOUNT + STOCK + CREATE_TIME_YEAR + CATEGORY,
               left = 0, right = 100, data = df_sample)
summary(tobit1)
AIC(tobit1)
tobit2 = tobit(conv_rate ~ SALE_PRICE+ STOCK + DISCOUNT*REVIEW_RATING + REVIEW_COUNT +
                 CATEGORY*LIKED_COUNT + HISTORICAL_SOLD + CATEGORY*REVIEW_RATING
               + CREATE_TIME_YEAR, left = 0, right = 100,
               data = df_sample)
summary(tobit2)
AIC(tobit2)
library(stargazer)
stargazer(lm1, tobit1, tobit2, type = "text", out = "out.txt", single.row = TRUE)
#interaction between stock and price to check when price is low
#and stock is high - does conversion improve?
#how distinct categories affect conversion with discount and higher review rating respectively
#assumptions
#multicollinearity
round(vif(tobit1),2)
#high multi collinearity between historical sold and revieq_count
#Independence
library(car)
durbinWatsonTest(residuals(tobit2))
#no sign of auto correlation - PASS
#Heteroscedasticity
residuals <- residuals(tobit2)
fitted_values <- fitted(tobit2)
plot(fitted_values, residuals, pch = 1,xlab = "Predicted values", ylab = "Residuals")
abline(0,0,col = "red",lwd = 3)
#Normality
qqnorm(residuals, pch = 19, main = "Normality Plot of Conversion rate residuals")
qqline(residuals, lwd =3, col = "red")