setwd("D:/Analytics Complete/R/R Case studies/R Case study Linear Regression")

library(readxl)
require(readxl)
Customer_dbase= readxl::read_excel("Linear Regression Case.xlsx" , sheet=1)
Data_dictionary= readxl::read_excel("Linear Regression Case.xlsx" , sheet=2)

View(head(Customer_dbase))
View(tail(Customer_dbase))

datatypes= data.frame(sapply(Customer_dbase, class))


summary(Customer_dbase)


library(writexl)
require(writexl)
library(dplyr)
require(dplyr)

write_xlsx(datatypes, path= "D:/Analytics Complete/R/R Case studies/R Case study Linear Regression/datatypes.xlsx",col_names= TRUE, use_zip64 = FALSE, format_headers = TRUE)
## Removing unwanted categorical variables 
Customer_dbase$custid = NULL


Customer_dbase$birthmonth= NULL

Customer_dbase$carditems = NULL

Customer_dbase$card2items = NULL

Customer_dbase =as.data.frame(Customer_dbase)
class(Customer_dbase)
datatypes= data.frame(sapply(Customer_dbase, class))



### Creating function outlier

outlier_function= function(x){
  UC= quantile(x , p=0.95, na.rm = T)
  LC = quantile(x, p=0.05 , na.rm=T)
  
  x= ifelse(x>UC , UC , x)
  x = ifelse(x<LC , LC , x)
  return(x)
  
}

## create missing value treatemnt function 

missing_function = function(x){
  x[is.na(x)] = mean(x , na.rm=T)
  return(x)
}


## treat outliers and then missing values in customer_database 
Outlier_treated = as.data.frame(sapply(Customer_dbase , FUN =outlier_function))
missing_and_outlier_treated = as.data.frame(sapply(Outlier_treated , FUN =missing_function))

Numeric_customer_database = missing_and_outlier_treated


View(Numeric_customer_database)
View(Outlier_treated)


### FEATURE REDUCTION #
library(caret)

require(caret)

colnames(Numeric_customer_database)
features <- dplyr::select(Numeric_customer_database, -TotalCardSpent)
Y_var <- dplyr::select(Numeric_customer_database, TotalCardSpent)

features <- data.matrix(features)
Y_var <- data.matrix(Y_var)


View(features)

# Performing RFE #
##################

modelF <- lm(TotalCardSpent~.,data=Numeric_customer_database)

set.seed(0)

# WARNING : Time Consuming Step (approx 1-2 mins)
results_rfe <- rfe(features,Y_var,size=c(1:129), rfeControl=rfeControl(functions = lmFuncs))

results_rfe

# Finding the top 10 vars
RFE_top10 <- update(results_rfe,features,Y_var,size = 10)
RFE_top10[["bestVar"]]

# output variables from RFE method 

#  "wireless"   "lnwiremon"  "equip"      "tollfree"   "lnequipmon" "lntollmon"  "lncardmon"  "marital"    "callcard"   "lntollten" 



# Performing Step wise Regression #
###################################

# Creating a full and a empty model
modelF <- lm(TotalCardSpent~.,data=Numeric_customer_database)
modelF

modelnull <- lm(TotalCardSpent~1,data=Numeric_customer_database)
modelnull

# Running stepwise Regression
stepwise <- step(modelnull, scope = list(upper=modelF), data=Numeric_customer_database,  direction="both")

stepwise
# Output Result (Included Vars) from stepwise method 
# Step:  AIC=33832.92

# 
# TotalCardSpent ~ cardspent...79 + card2spent + inccat + carbuy + 
#  pets + commutemotorcycle + pets_freshfish + creddebt + cardbenefit + 
#  polcontrib + churn + hometype + vote + response_01 + callid + 
# tollmon + cardspent...42 + cardtenurecat + cardtenure



# LASSO METHOD #
#########

# Initalising a lasso regression model
lasso = train(TotalCardSpent~.,
              data=Numeric_customer_database,method='glmnet',
              trControl = trainControl(method="none"),
              tuneGrid=expand.grid(alpha=1,lambda=0.09))

pred_lasso = predict(lasso,newdata = subset(Numeric_customer_database,select = c(1:129)))

coef(lasso$finalModel, s = lasso$bestTune$lambda)

# output variables from Lasso method 
#  cardspent...79 
# card2spent       


colnames(Numeric_customer_database)

## now combining vriables from all 3 lists removng duplicates ( RFE, Stepwise and Lasso to get final list of reduced variables for modelling)



Final_Vars =c("equip" , "lnequipmon" , "wireless" , "lnwiremon" , "tollfree" , "lntollmon" , "lncardmon" , "marital" , "lntollten" , "callcard" ,
              "cardspent...79" , "pets" , "polcontrib" , "card2spent" , "inccat" , "carbuy" , "commutemotorcycle" , "pets_freshfish",
              "creddebt" , "cardbenefit" , "churn" , "hometype" , "vote" , "response_01" , "callid" , "tollmon" , "cardspent...42" ,
              "cardtenurecat" , "cardtenure" , "TotalCardSpent") 


## reduced 30 variables Dataframe for furtehr iteration and modelling

Customer__reducevar <- Numeric_customer_database[, Final_Vars]
View(Customer__reducevar )
length(Customer__reducevar)


# Assumptions Check (for Linear Regression)

## normal distribution of Y variable

hist(Y_var)

install.packages("moments")
require(moments)

skewness(Y_var)

kurtosis(Y_var)


# 2 - Check for correlations between y and x variables 

cor_mat<-data.frame(cor(Customer__reducevar))




write.csv(cor_mat, "cor_mat.csv")

## Correlation based analysis : below 18 variables are final with corrleation with Y above  0.3)



Final_Vars2 =c( "TotalCardSpent" , "equip" , "lnequipmon" , "wireless" , "lnwiremon" , "tollfree" , "lntollmon" , "lntollten" , "callcard", 
                "cardspent...79" , "card2spent" , "inccat" , "vote" , "callid" , "tollmon" , "cardspent...42" , "cardtenurecat" , "cardtenure")




## reduced 18 variables Dataframe for further iteration and modelling

Customer__reducevar2 <- Numeric_customer_database[, Final_Vars2]
View(Customer__reducevar2 )
length(Customer__reducevar2)


## Analysis : final reduced data set has 17 columns now 

########  DATA SPLITTING    

samp<-sample(1:nrow(Customer__reducevar2), floor(nrow(Customer__reducevar2)*0.7))

dev<-Customer__reducevar2[samp,]
val<-Customer__reducevar2[-samp,]

nrow(Customer__reducevar2)
nrow(dev)
nrow(val)

colnames(Customer__reducevar2)

#  LINEAR REGRESSION ON FINAL DATASET :  (Customer__reducevar2)
# 1st iteration 

fit<- lm(TotalCardSpent~equip+lnequipmon+wireless+lnwiremon+tollfree+lntollmon+lntollten+    
           callcard+cardspent...79+card2spent+inccat+vote+callid+tollmon+      
           cardspent...42+cardtenurecat+cardtenure  , data=Customer__reducevar2)

summary(fit)


# 2nd iteration - Removing wireless as it has high p value


fit_2<- lm(TotalCardSpent~equip+lnequipmon+lnwiremon+tollfree+lntollmon+lntollten+    
             callcard+cardspent...79+card2spent+inccat+vote+callid+tollmon+      
             cardspent...42+cardtenurecat+cardtenure  , data=Customer__reducevar2)

summary(fit_2)



# 3 rd iteration - Removing lnequipmon as it has high p value


fit_3<- lm(TotalCardSpent~equip+lnwiremon+tollfree+lntollmon+lntollten+    
             callcard+cardspent...79+card2spent+inccat+vote+callid+tollmon+      
             cardspent...42+cardtenurecat+cardtenure  , data=Customer__reducevar2)

summary(fit_3)




# 4th iteration - Removing vote as it has high p value


fit_4<- lm(TotalCardSpent~equip+lnwiremon+tollfree+lntollmon+lntollten+    
             cardspent...79+card2spent+inccat+vote+callid+tollmon+      
             cardspent...42+cardtenurecat+cardtenure  , data=Customer__reducevar2)

summary(fit_4)


# 5th iteration - Removing equip  as it has high p value


fit_5<- lm(TotalCardSpent~lnwiremon+tollfree+lntollmon+lntollten+    
             cardspent...79+card2spent+inccat+vote+callid+tollmon+      
             cardspent...42+cardtenurecat+cardtenure  , data=Customer__reducevar2)

summary(fit_5)





# 6th iteration - Removing lnwiremon  as it has high p value


fit_6<- lm(TotalCardSpent~+tollfree+lntollmon+lntollten+    
             cardspent...79+card2spent+inccat+vote+callid+tollmon+      
             cardspent...42+cardtenurecat+cardtenure  , data=Customer__reducevar2)

summary(fit_6)






# 7th iteration - Removing lntollten  as it has high p value


fit_7<- lm(TotalCardSpent~+tollfree+lntollmon+    
             cardspent...79+card2spent+inccat+vote+callid+tollmon+      
             cardspent...42+cardtenurecat+cardtenure  , data=Customer__reducevar2)

summary(fit_7)





# 8th iteration - Removing Vote  as it has high p value


fit_8<- lm(TotalCardSpent~+tollfree+lntollmon+    
             cardspent...79+card2spent+inccat+callid+tollmon+      
             cardspent...42+cardtenurecat+cardtenure  , data=Customer__reducevar2)

summary(fit_8)





# 9th iteration - Removing Vote  as it has high p value


fit_9<- lm(TotalCardSpent~+tollfree+lntollmon+    
             cardspent...79+card2spent+inccat+callid+tollmon+      
             cardspent...42+cardtenurecat+cardtenure  , data=Customer__reducevar2)

summary(fit_9)



#### VIF for checking multi collinearity amngst X variables using VIF 


require(car)

modelfull= lm(TotalCardSpent~. , data = Customer__reducevar2)
car::vif(modelfull)
## anaylsis that few variable has VIF more than 5 hence we will remove them one by one . 
## 1) tollfree   2) lntollmon  3)tollmon   

#### remove tollmon as vif value is 314 which is highest 

model_new1 <- lm(TotalCardSpent~+tollfree+lntollmon+    
                   cardspent...79+card2spent+inccat+callid+      
                   cardspent...42+cardtenurecat+cardtenure  , data=Customer__reducevar2)

car::vif(model_new1)
# now all vif values are less than 5 , hence this means no major multi colinearity amongst X variables 


############# validate the model with predict function 


dev1<-data.frame(cbind(dev,pred=predict(model_new1, newdata=dev)))

val1<-data.frame(cbind(val,pred=predict(model_new1, newdata=val)))

View(dev1)

########################## calucating MAPE, RMSe for development and validation datasets


require(Metrics)

Metrics::mape(dev1$TotalCardSpent , dev1$pred)

Metrics::mape(val1$TotalCardSpent , val1$pred)

Metrics::rmse(dev1$TotalCardSpent , dev1$pred)
Metrics::rmse(val1$TotalCardSpent , val1$pred)

Metrics::rmsle(dev1$TotalCardSpent , dev1$pred)
Metrics::rmsle(val1$TotalCardSpent , val1$pred)

cor(dev1$TotalCardSpent , dev1$pred , method = "pearson")
cor(val1$TotalCardSpent , val1$pred , method = "pearson")


## decile analysis for validation dataset

dev11 = dev1

declocations = quantile(dev11$pred, seq(0.1,0.9,0.1))
declocations

dev11$decile = findInterval(dev11$pred , c(-Inf, declocations, Inf))

View(dev11)

## confirm decile by checking summary and cross Freq fnction 

summary(dev11$decile)
xtabs(~decile,dev11)

## Group data by decile and calculate avergae Y and Predicted Y


colnames(dev11)
dev12= dev11[ ,   c("TotalCardSpent" , "pred"     ,      "decile" ) ]



require(sqldf)
dev_DA <- sqldf("select decile, count(decile) as count, avg(pred) as avg_pred_Y,   avg(TotalCardSpent) as avg_ActualY
                    from dev12
                    group by decile
                    order by decile desc")


writexl::write_xlsx(dev_DA , "decileanalysis_dev_dataset.xlsx")




##### ## decile analysis for validation dataset

val11 = val1

declocations2 = quantile(val11$pred, seq(0.1,0.9,0.1))
declocations2

val11$decile= findInterval(val11$pred , c(-Inf, declocations2, Inf))

View(val11)

## confirm decile by checking summary and cross Freq fnction 

summary(val11$decile)
xtabs(~decile,val11)

## Group data by decile and calculate avergae actual Y and Predicted Y

colnames(val11)
val12= val11[ ,   c("TotalCardSpent" , "pred"     ,      "decile" ) ]


require(sqldf)
val_DA <- sqldf("select decile, count(decile) as count, avg(pred) as avg_pred_Y,   avg(TotalCardSpent) as avg_ActualY
                    from val12
                    group by decile
                    order by decile desc")




writexl::write_xlsx(val_DA , "decileanalysis_validation_dataset.xlsx")

# calculate R2 using user defined function 
rsq <- function (x, y){
  p=cor(x, y) ^ 2
  return(p)
  
  
}


rsq(dev1$TotalCardSpent , dev1$pred)
rsq(val1$TotalCardSpent , val1$pred)

# RMSE is 30.07 for dev data and 28.17 for validation data 
# MAPE is 0.02 for dev data and 0.02 for validation data 
# RMSLE is 0.05 for dev data and 0.04 for validation data 
# R2 is 0.987 for dev data and 0.989 for validation data 
# corerleation between actual y and predicted y is 99.03 %
