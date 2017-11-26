library(tidyverse)
library(xray)

setwd("~/Desktop/kaggle house prices//")

###################### Load the data
train <- read_csv('train.csv')
test <- read_csv('test.csv')
###################### Explore the data

# Let's conbine them together 
train <- train %>% mutate(train_test = 'train') 
test <- test %>% mutate(SalePrice = NA, train_test = 'test')
train_test <- train %>% bind_rows(test)

# move to factor variables

# get list of variables 
var_list <- names(train_test)[! names(train_test) %in% c('Id', 'SalePrice', 'train_test')]
length(var_list)


# lets explore variability 
summary(train_test %>% select(!!!var_list))

# lets explore anomalies 2-5 
xray::anomalies(train_test[,2:5])  
xray::distributions(train_test[,2:5])
xray::distributions(data.frame(y = log(train_test$LotArea)))

# Create log from LotArea
train_test$LotArea_log <- log(train_test$LotArea)

# MSZoning
train_test$MSSubClass <- as.factor(train_test$MSSubClass)
train_test$is_residential <- as.integer(train_test$MSZoning %in% c('FV', 'RH', 'RL', 'RP', 'RM'))
xray::distributions(data.frame(y = train_test$MSZoning))
xray::distributions(data.frame(y = log(train_test$is_residential)))
table(train_test$is_residential, useNA = 'always')


# lets explore anomalies 6-9
xray::anomalies(train_test[,6:9])  
xray::distributions(train_test[,6:9])

# Fix Alley 
train_test$Alley <- as.character(train_test$Alley)
train_test$Alley[is.na(train_test$Alley)] <- 'No Alley'

train %>% group_by(LotShape) %>% summarise(SalePrice = mean(SalePrice), n = n())
train_test$is_regular_shape <- as.integer(!train_test$LotShape %in% c(' IR1', 'IR2', 'IR3'))
xray::distributions(data.frame(y = train_test$is_regular_shape))

train_test %>% group_by(is_regular_shape) %>% summarise(SalePrice = mean(SalePrice, na.rm = T), n = n())

# lets explore anomalies 10-13
xray::anomalies(train_test[,10:13])  
xray::distributions(train_test[,10:13])

train %>% group_by(LandSlope) %>% summarise(SalePrice = mean(SalePrice), n = n())

train_test$LandSlope_int <- NA
train_test$LandSlope_int[train_test$LandSlope == 'Gtl'] <- 1
train_test$LandSlope_int[train_test$LandSlope == 'Mod'] <- 2
train_test$LandSlope_int[train_test$LandSlope == 'Sev'] <- 3
table(train_test$LandSlope_int, useNA = 'always')

# lets explore anomalies 14-17
xray::anomalies(train_test[,14:17])  
xray::distributions(train_test[,14:17])

train_test %>% group_by(Condition2) %>% summarise(SalePrice = mean(SalePrice, na.rm = T), n = n())


# lets explore anomalies 18-21
xray::anomalies(train_test[,18:21])  
xray::distributions(train_test[,18:21])
train_test$YearRemodAdd_lag_log <- log(max(train_test$YearRemodAdd)+1 - train_test$YearRemodAdd)

# lets explore anomalies 22-25
xray::anomalies(train_test[,22:25])  
xray::distributions(train_test[,22:25])
train %>% group_by(Exterior1st) %>% summarise(SalePrice = mean(SalePrice, na.rm = T), n = n()) %>% arrange(-n)

# lets explore anomalies 26-29
xray::anomalies(train_test[,26:29])  
xray::distributions(train_test[,26:29])

train_test$MasVnrArea[is.na(train_test$MasVnrArea)] <- 0
train_test$MasVnrType[is.na(train_test$MasVnrType)] <- 'None'

train_test$isMasVnrArea <- ifelse(train_test$MasVnrArea == 0, 0, 1)


table(train_test$isMasVnrArea)

#ExterQual

train_test$ExterQual_int <- 0
train_test$ExterQual_int[train_test$ExterQual == 'Ex'] <- 5
train_test$ExterQual_int[train_test$ExterQual == 'Gd'] <- 4
train_test$ExterQual_int[train_test$ExterQual == 'TA'] <- 3
train_test$ExterQual_int[train_test$ExterQual == 'Fa'] <- 2
train_test$ExterQual_int[train_test$ExterQual == 'Po'] <- 1




#ExterCond

train_test$ExterCond_int <- 0
train_test$ExterCond_int[train_test$ExterCond == 'Ex'] <- 5
train_test$ExterCond_int[train_test$ExterCond == 'Gd'] <- 4
train_test$ExterCond_int[train_test$ExterCond == 'TA'] <- 3
train_test$ExterCond_int[train_test$ExterCond == 'Fa'] <- 2
train_test$ExterCond_int[train_test$ExterCond == 'Po'] <- 1

# lets explore anomalies 30-33
xray::anomalies(train_test[,30:33])  
xray::distributions(train_test[,30:33])


#BsmtQual
train_test$is_Bsmt <- ifelse(is.na(train$BsmtQual), 0,1)
train_test$BsmtQual_int <- 0
train_test$BsmtQual_int[train_test$BsmtQual == 'Ex'] <- 5
train_test$BsmtQual_int[train_test$BsmtQual == 'Gd'] <- 4
train_test$BsmtQual_int[train_test$BsmtQual == 'TA'] <- 3
train_test$BsmtQual_int[train_test$BsmtQual == 'Fa'] <- 2
train_test$BsmtQual_int[train_test$BsmtQual == 'Po'] <- 1
train_test %>% group_by(BsmtQual_int) %>% summarise(SalePrice = mean(SalePrice, na.rm = T), n = n()) %>% arrange(-n)


#BsmtCond
train_test$BsmtCond_int <- 0
train_test$BsmtCond_int[train_test$BsmtCond == 'Ex'] <- 5
train_test$BsmtCond_int[train_test$BsmtCond == 'Gd'] <- 4
train_test$BsmtCond_int[train_test$BsmtCond == 'TA'] <- 3
train_test$BsmtCond_int[train_test$BsmtCond == 'Fa'] <- 2
train_test$BsmtCond_int[train_test$BsmtCond == 'Po'] <- 1

# BsmtExposure
train_test$BsmtExposure_int <- 0
train_test$BsmtExposure_int[train_test$BsmtExposure == 'Gd'] <- 4
train_test$BsmtExposure_int[train_test$BsmtExposure == 'Av'] <- 3
train_test$BsmtExposure_int[train_test$BsmtExposure == 'Mn'] <- 2
train_test$BsmtExposure_int[train_test$BsmtExposure == 'No'] <- 1


# lets explore anomalies 34-37
xray::anomalies(train_test[,34:37])  
xray::distributions(train_test[,34:37])

train_test$BsmtFinSF2[is.na(train_test$BsmtFinSF2)] <- 0
train_test$BsmtFinSF1[is.na(train_test$BsmtFinSF1)] <- 0

train_test$BsmtFinType2[is.na(train_test$BsmtFinType2)] <- 'No info'
train_test$BsmtFinType1[is.na(train_test$BsmtFinType1)] <- 'No info'

# lets explore anomalies 38-41
xray::anomalies(train_test[,38:41])  
xray::distributions(train_test[,38:41])

train_test$BsmtUnfSF[is.na(train_test$BsmtUnfSF)] <- mean(train_test$BsmtUnfSF, na.rm = TRUE)
train_test$TotalBsmtSF[is.na(train_test$TotalBsmtSF)] <- mean(train_test$TotalBsmtSF, na.rm = TRUE)

table(train_test$Heating)



# lets explore anomalies 42-45
xray::anomalies(train_test[,42:45])  
xray::distributions(train_test[,42:45])
names(train_test)[names(train_test) == '1stFlrSF'] <- 'FirstFlrSF'
names(train_test)[names(train_test) == '2ndFlrSF'] <- 'SecondFlrSF'
names(train_test)[names(train_test) == '3SsnPorch'] <- 'ThreeSsnPorch'

# lets explore anomalies 46-49
xray::anomalies(train_test[,46:49])  
xray::distributions(train_test[,46:49])

train_test$BsmtHalfBath[is.na(train_test$BsmtHalfBath)] <- 0
train_test$BsmtFullBath[is.na(train_test$BsmtFullBath)] <- 0

# lets explore anomalies 50-53
xray::anomalies(train_test[,50:53])  
xray::distributions(train_test[,50:53])


# lets explore anomalies 54-57
xray::anomalies(train_test[,54:57])  
xray::distributions(train_test[,54:57])

#KitchenQual
train_test$KitchenQual_int <- 0
train_test$KitchenQual_int[train_test$KitchenQual == 'Ex'] <- 5
train_test$KitchenQual_int[train_test$KitchenQual == 'Gd'] <- 4
train_test$KitchenQual_int[train_test$KitchenQual == 'TA'] <- 3
train_test$KitchenQual_int[train_test$KitchenQual == 'Fa'] <- 2
train_test$KitchenQual_int[train_test$KitchenQual == 'Po'] <- 1


train_test$Functional_int <- 8
train_test$Functional_int[train_test$KitchenQual == 'Typ'] <- 8
train_test$Functional_int[train_test$KitchenQual == 'Min1'] <- 7
train_test$Functional_int[train_test$KitchenQual == 'Min2'] <- 6
train_test$Functional_int[train_test$KitchenQual == 'Mod'] <- 5
train_test$Functional_int[train_test$KitchenQual == 'Maj1'] <- 4
train_test$Functional_int[train_test$KitchenQual == 'Maj2'] <- 3
train_test$Functional_int[train_test$KitchenQual == 'Sev'] <- 2
train_test$Functional_int[train_test$KitchenQual == 'Sal'] <- 0


# lets explore anomalies 58-61
xray::anomalies(train_test[,58:61])  
xray::distributions(train_test[,58:61])

#FireplaceQu
train_test$FireplaceQu_int <- 0
train_test$FireplaceQu_int[train_test$FireplaceQu == 'Ex'] <- 5
train_test$FireplaceQu_int[train_test$FireplaceQu == 'Gd'] <- 4
train_test$FireplaceQu_int[train_test$FireplaceQu == 'TA'] <- 3
train_test$FireplaceQu_int[train_test$FireplaceQu == 'Fa'] <- 2
train_test$FireplaceQu_int[train_test$FireplaceQu == 'Po'] <- 1

train_test$FireplaceQu_int_sq <- 0
train_test$FireplaceQu_int_sq[train_test$FireplaceQu == 'Ex'] <- 5^2
train_test$FireplaceQu_int_sq[train_test$FireplaceQu == 'Gd'] <- 4^2
train_test$FireplaceQu_int_sq[train_test$FireplaceQu == 'TA'] <- 3^2
train_test$FireplaceQu_int_sq[train_test$FireplaceQu == 'Fa'] <- 2^2
train_test$FireplaceQu_int_sq[train_test$FireplaceQu == 'Po'] <- 1^2

train_test$is_Garage <- !is.na(train_test$GarageFinish) 
train_test$GarageFinish[is.na(train_test$GarageFinish)] <- 'No garage'
train_test$GarageType[is.na(train_test$GarageType)] <- 'No garage'
train_test$GarageYrBlt[is.na(train_test$GarageYrBlt)] <- 1900


# lets explore anomalies 62-65
xray::anomalies(train_test[,62:65])  
xray::distributions(train_test[,62:65])



#GarageQual
train_test$GarageQual_int <- 0
train_test$GarageQual_int[train_test$GarageQual == 'Ex'] <- 5
train_test$GarageQual_int[train_test$GarageQual == 'Gd'] <- 4
train_test$GarageQual_int[train_test$GarageQual == 'TA'] <- 3
train_test$GarageQual_int[train_test$GarageQual == 'Fa'] <- 2
train_test$GarageQual_int[train_test$GarageQual == 'Po'] <- 1


#GarageCond
train_test$GarageCond_int <- 0
train_test$GarageCond_int[train_test$GarageCond == 'Ex'] <- 5
train_test$GarageCond_int[train_test$GarageCond == 'Gd'] <- 4
train_test$GarageCond_int[train_test$GarageCond == 'TA'] <- 3
train_test$GarageCond_int[train_test$GarageCond == 'Fa'] <- 2
train_test$GarageCond_int[train_test$GarageCond == 'Po'] <- 1

train_test$GarageCars[is.na(train_test$GarageCars)] <- 0
train_test$GarageArea[is.na(train_test$GarageArea)] <- 0


# lets explore anomalies 66-69
xray::anomalies(train_test[,66:69])  
xray::distributions(train_test[,66:69])

train_test$is_EnclosedPorch <- ifelse(train_test$EnclosedPorch == 0, 0,1)
train_test$is_OpenPorch <- ifelse(train_test$OpenPorch == 0, 0,1)
train_test$is_Deck <- ifelse(train_test$WoodDeckSF == 0, 0,1)

# lets explore anomalies 70-73
xray::anomalies(train_test[,70:73])  
xray::distributions(train_test[,70:73])

table(train_test$is_Pool)
train_test$is_Pool <- ifelse(train_test$PoolArea == 0, 0,1)

# lets explore anomalies 74-77
xray::anomalies(train_test[,74:77])  
xray::distributions(train_test[,74:77])

train_test$MiscFeature[is.na(train_test$MiscFeature)] <- 'None'

# lets explore anomalies 78-81
xray::anomalies(train_test[,78:81])  
xray::distributions(train_test[,78:81])
train_test$SaleType[is.na(train_test$SaleType)] <- 'WD'

train_test$MSZoning[is.na(train_test$MSZoning)] <- 'RL'

train_test$Fence[is.na(train_test$Fence)] <-  'No Fence'

train_test$Exterior1st[is.na(train_test$Exterior1st)] <-  'VinylSd'
train_test$Exterior2nd[is.na(train_test$Exterior2nd)] <-  'VinylSd'
train_test$Electrical[is.na(train_test$Electrical)] <-  'SBrkr'


train_test$MSSubClass <- factor(train_test$MSSubClass)



train_test$LotFrontage[is.na(train_test$LotFrontage)] <- mean(train_test$LotFrontage, na.rm = TRUE)


############# Neighborhood - should be the most predicting variable ever - need to check later
############# Exterior1st - need to fill/predict one variable
############# Exterior2nd - need to fill/predict one variable 

############# MasVnrArea - need to fill/predict one variable
############# MasVnrType - need to fill/predict one variable 




train_test <- train_test %>% select(-BsmtQual)
train_test <- train_test %>% select(-BsmtCond)
train_test <- train_test %>% select(-BsmtExposure)
#train_test <- train_test %>% select(-Heating)
train_test <- train_test %>% select(-KitchenQual)
train_test <- train_test %>% select(-Functional)
train_test <- train_test %>% select(-FireplaceQu)

train_test <- train_test %>% select(-GarageQual)
train_test <- train_test %>% select(-GarageCond)

train_test <- train_test %>% select(-PoolQC)


###### Now lets explore each variable




xray::anomalies(train_test %>% select(YearRemodAdd))  
xray::distributions(train_test %>% select(YearRemodAdd_lag_log))
table(train_test$YearRemodAdd_lag_log)

na_df <- train_test %>% summarise_each(funs(sum(is.na(.) | is.infinite(.) | is.nan(.)))) 
no_na <- names(train_test)[colSums(na_df)==0]
with_na <- names(train_test)[colSums(na_df)>0]
formula <- paste('SalePrice ~', paste(no_na[no_na != 'Id'], collapse = ' + '))
write_file(formula, path = 'formula.txt')
train_test$SalePrice[train_test$Id >= 1461] <- NA


############# Now lets run some regression
# Random forest 
library(randomForest)

# Create final df
#train_test <- train %>% full_join(test)
train_test <- train_test %>% mutate_if(is.character, factor )
original_SalePrice <- train_test$SalePrice
train_test$SalePrice <- log(original_SalePrice)
train_test$SF <- train_test$FirstFlrSF + train_test$SecondFlrSF
train_df <- train_test %>% filter(!is.na(SalePrice))
test_df <- train_test %>% filter(is.na(SalePrice))


    rf <- randomForest(SalePrice ~ MSSubClass + MSZoning + LotFrontage + LotArea + Street + Alley + LotShape + LandContour + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + Heating + HeatingQC + CentralAir + Electrical + FirstFlrSF + SecondFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Fireplaces + GarageType + GarageYrBlt + GarageFinish + GarageCars + GarageArea + PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch + ThreeSsnPorch + ScreenPorch + PoolArea + Fence + MiscFeature + MiscVal  + SaleType + SaleCondition + train_test + LotArea_log + is_residential + is_regular_shape + LandSlope_int + YearRemodAdd_lag_log + isMasVnrArea + ExterQual_int + ExterCond_int + BsmtQual_int + BsmtCond_int + BsmtExposure_int + KitchenQual_int + Functional_int + FireplaceQu_int + FireplaceQu_int_sq + is_Garage + GarageQual_int + GarageCond_int + is_EnclosedPorch + is_Deck + is_Pool + SF, data = train_df, importance = TRUE, ntree=1000, mtry=7)

which.min(rf$mse)

# Impression
imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp


new_formula <- paste('SalePrice ~', paste(rownames(imp)[1:20], collapse = ' + '))
write_file(new_formula, path = 'new_formula.txt')

# rf <- randomForest(SalePrice ~ GrLivArea + Neighborhood + MSSubClass + OverallQual + FirstFlrSF + SecondFlrSF + TotalBsmtSF + GarageArea + GarageCars + BsmtFinType1 + LotArea_log + LotArea + BsmtFinSF1 + GarageType + ExterQual + ExterQual_int + FireplaceQu_int + FireplaceQu_int_sq + YearBuilt + KitchenQual_int, data = train_df, importance = TRUE, ntree=1000)

plot(rf)
varImpPlot(rf)
# Prediction
predict <- predict(rf,test_df)


final_result <- test_df %>% select(Id) %>% mutate(SalePrice = predict)

dim(test_df[is.na(predict),])
write_csv(final_result, path = 'dennis_submit.csv')


# try library(party)
library(party)
fit <- cforest(SalePrice ~ MSSubClass + MSZoning + LotFrontage + LotArea + Street + Alley + LotShape + LandContour + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + Heating + HeatingQC + CentralAir + Electrical + FirstFlrSF + SecondFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Fireplaces + GarageType + GarageYrBlt + GarageFinish + GarageCars + GarageArea + PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch + ThreeSsnPorch + ScreenPorch + PoolArea + Fence + MiscFeature + MiscVal + MoSold + YrSold + SaleType + SaleCondition + train_test + LotArea_log + is_residential + is_regular_shape + LandSlope_int + YearRemodAdd_lag_log + isMasVnrArea + ExterQual_int + ExterCond_int + BsmtQual_int + BsmtCond_int + BsmtExposure_int + KitchenQual_int + Functional_int + FireplaceQu_int + FireplaceQu_int_sq + is_Garage + GarageQual_int + GarageCond_int + is_EnclosedPorch + is_Deck + is_Pool,
               data = train_df, 
               controls=cforest_unbiased(ntree=2000, mtry=5))

Prediction <- predict(fit, test_df, OOB=TRUE)

final_result <- test_df %>% select(Id) %>% mutate(SalePrice = Prediction)

dim(test_df[is.na(predict),])
write_csv(final_result, path = 'dennis_submit.csv')


# Try using caret

# Create model with default paramters
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "RMSE"
set.seed(seed)
mtry <- round(sqrt(ncol(train_df)-1))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(SalePrice~GrLivArea, data=train_df, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_default)
plot(rf_default)
