library(tidyverse)
library(xray)
library(randomForest)
library(timeDate)



setwd("~/Desktop/kaggle house prices//")

###################### Load the data
train <- read_csv('train.csv')
test <- read_csv('test.csv')
###################### Explore the data


# Let's conbine them together 
train <- train %>% mutate(train_test = 1) 
test <- test %>% mutate(SalePrice = NA, train_test = 0)
train_test <- train %>% bind_rows(test)


# Transform some integer variables into factors 
train_test <- train_test %>% 
    mutate(MSSubClass =  factor(MSSubClass), 
           MoSold = factor(MoSold), 
           YrSold = factor(YrSold))


# Fix some NA's 
train_test <- train_test %>% 
    replace_na(list(Alley = 'No Alley', 
                    MasVnrArea = 0,
                    MasVnrType = 'None', 
                    BsmtQual = 'No Basement', 
                    BsmtCond = 'No Basement', 
                    BsmtExposure = 'No Basement', 
                    BsmtFinSF2 = 0,
                    BsmtFinSF1 = 0, 
                    BsmtFinType1 = 'No info', 
                    BsmtFinType2 = 'No info', 
                    TotalBsmtSF = 0, 
                    BsmtUnfSF = 0, 
                    BsmtHalfBath = 0, 
                    BsmtFullBath = 0, 
                    GarageFinish = 'No garage', 
                    GarageType = 'No garage', 
                    GarageQual = 'No garage', 
                    GarageCond = 'No garage', 
                    GarageYrBlt =  mean(train_test$GarageYrBlt, na.rm = TRUE), 
                    GarageCars = 0, 
                    GarageArea = 0, 
                    KitchenQual = 'NA', 
                    Functional = 'NA', 
                    FireplaceQu = 'NA', 
                    MiscFeature = 'None', 
                    MiscFeature = 'None', 
                    MiscFeature = 'None', 
                    SaleType = 'WD', 
                    MSZoning = 'RL', 
                    Fence = 'No Fence', 
                    Exterior1st = 'VinylSd', 
                    Exterior2nd = 'VinylSd', 
                    Electrical = 'SBrkr', 
                    LotFrontage = 0, 
                    PoolQC = 'NA'
                    )) %>% 
    mutate(GarageYrBlt = ifelse(GarageYrBlt>2010, median(GarageYrBlt, na.rm = TRUE),GarageYrBlt), 
           is_EnclosedPorch = ifelse(EnclosedPorch == 0, 0,1), 
           is_OpenPorch = ifelse(OpenPorchSF == 0, 0,1), 
           is_WoodDeck = ifelse(WoodDeckSF == 0, 0, 1), 
           is_Pool = ifelse(PoolArea == 0, 0, 1) , 
           is_LotFrontage = ifelse(LotFrontage == 0, 0, 1) 
    ) %>% # Change some names - dont like column names starting with number 
    mutate(FirstFlrSF = `1stFlrSF`, SecondFlrSF = `2ndFlrSF`, ThreeSsnPorch = `3SsnPorch`) %>% 
    select(-`1stFlrSF`, -`2ndFlrSF`, -`3SsnPorch`) 

# Combine Condition1-2 
Cond1_dummy <- train_test %>% select(Id, Condition1) %>% mutate(is_true = 1) %>% spread(key = Condition1, value = is_true, fill = 0) %>% gather(key = Condition, value = 'is_true', 2:(length(unique(train_test$Condition1))+1)) %>% arrange(Id)

Cond2_dummy <- train_test %>% select(Id, Condition1) %>% mutate(is_true = 1) %>% spread(key = Condition1, value = is_true, fill = 0) %>% gather(key = Condition, value = 'is_true', 2:(length(unique(train_test$Condition1))+1)) %>% arrange(Id)

Cond_dummy <- Cond1_dummy %>% bind_rows(Cond2_dummy) %>% group_by(Id, Condition) %>%
    summarise(is_true = sum(is_true))  %>% mutate(is_true = ifelse(is_true > 0, 1,0)) %>% arrange(-is_true) %>% spread(Condition, value = is_true, fill = 0)
    
train_test <- train_test %>% select(-Condition1, -Condition2) %>% left_join(Cond_dummy)

# Combine Condition1-2 
Cond1_dummy <- train_test %>% select(Id, Condition1) %>% mutate(is_true = 1) %>% spread(key = Condition1, value = is_true, fill = 0) %>% gather(key = Condition, value = 'is_true', 2:(length(unique(train_test$Condition1))+1)) %>% arrange(Id)

Cond2_dummy <- train_test %>% select(Id, Condition1) %>% mutate(is_true = 1) %>% spread(key = Condition1, value = is_true, fill = 0) %>% gather(key = Condition, value = 'is_true', 2:(length(unique(train_test$Condition1))+1)) %>% arrange(Id)

Cond_dummy <- Cond1_dummy %>% bind_rows(Cond2_dummy) %>% group_by(Id, Condition) %>%
    summarise(is_true = sum(is_true))  %>% mutate(is_true = ifelse(is_true > 0, 1,0)) %>% arrange(-is_true) %>% spread(Condition, value = is_true, fill = 0)

train_test <- train_test %>% select(-Condition1, -Condition2) %>% left_join(Cond_dummy)

# Exterior1st and Exterior2nd
Ext1_dummy <- train_test %>% select(Id, Exterior1st) %>% mutate(is_true = 1) %>% spread(key = Exterior1st, value = is_true, fill = 0) %>% gather(key = Exterior, value = 'is_true', 2:(length(unique(train_test$Exterior1st))+1)) %>% arrange(Id)

Ext2_dummy <- train_test %>% select(Id, Exterior2nd) %>% mutate(is_true = 1) %>% spread(key = Exterior2nd, value = is_true, fill = 0) %>% gather(key = Exterior, value = 'is_true', 2:(length(unique(train_test$Exterior2nd))+1)) %>% arrange(Id)

Exterior_dummy <- Ext1_dummy %>% bind_rows(Ext2_dummy) %>% group_by(Id, Exterior) %>%
    summarise(is_true = sum(is_true))  %>% mutate(is_true = ifelse(is_true > 0, 1,0)) %>% arrange(-is_true) %>% spread(Exterior, value = is_true, fill = 0)

train_test <- train_test %>% select(-Exterior1st, -Exterior2nd) %>% left_join(Exterior_dummy)


# Model some new variables 

train_test <- train_test %>% 
    mutate(is_Remod = as.integer(YearRemodAdd == YearBuilt),
           YearBuilt_age = max(YearBuilt)-YearBuilt,
           YearRemodAdd_age = max(YearRemodAdd)-YearRemodAdd,
           GarageYrBlt_age = max(GarageYrBlt)-GarageYrBlt, 
           YearBuilt_bucket = factor(round((order(YearBuilt)/max(order(YearBuilt))/0.2))),
           YearRemodAdd_bucket = factor(round((order(YearRemodAdd)/max(order(YearRemodAdd))/0.2))),
           GarageYrBlt_bucket = factor(round((order(GarageYrBlt)/max(order(GarageYrBlt))/0.2)))
    ) %>% select(-YearBuilt, -YearRemodAdd, -GarageYrBlt, -Utilities) # Utilities are useless - they have only one value 

# Transform all non-numeric columns to factors
train_test <- train_test %>% mutate_if(is.character, factor )
str(train_test)

# Separate numeric columns and convert then to logs
train_test_numeric <- train_test %>% 
     select_if(
         funs(is.numeric(.) & max(as.integer(.), na.rm = T)>1 & !is.factor(.))
         ) %>% select(-Id)

# Convert all numerics to logs
train_test_numeric_log <- train_test_numeric 
#%>% mutate(SalePrice_log = log(SalePrice)) %>% 
#    select(-SalePrice)
 train_test_numeric_log[,] <- log(train_test_numeric[,]+1)
 names(train_test_numeric_log) <- paste0(names(train_test_numeric_log), '_log')

# Create dummy variables 
train_test_non_numeric <- train_test %>% select(-one_of(names(train_test_numeric))) %>% select(-Id, -train_test)

library(caret)
train_test_non_numeric_dummy <- data.frame(predict(dummyVars(" ~ .", data = train_test_non_numeric), newdata = train_test_non_numeric))


# Now construct data sets back 
train_test_log <- train_test %>% 
    select(Id, train_test) %>% 
    bind_cols(train_test_numeric_log) %>% 
    bind_cols(train_test_non_numeric_dummy)

train_test_original <- train_test %>% 
    select(Id, train_test) %>% 
    bind_cols(train_test_numeric) %>% 
    bind_cols(train_test_non_numeric_dummy)


# Locate columns with lack of values in test 
#test_useless_vars <- names(train_test_original %>% filter(train_test == 0) %>% select_if(function(x) length(unique(x))==1) %>% select(-SalePrice, -train_test))

#train_useless_vars <- names(train_test_original %>% filter(train_test == 1) %>% select_if(function(x) length(unique(x))==1) %>% select(-train_test))

#Remove them 
#train_test_log <- train_test_log %>% select(-one_of(train_useless_vars)) %>% select(-one_of(test_useless_vars))

# train_test_original <- train_test_original %>% select(-one_of(train_useless_vars)) %>% select(-one_of(test_useless_vars))

# Final test for NA's in variables 
na_df <- train_test_original %>% summarise_each(funs(sum(is.na(.) | is.infinite(.) | is.nan(.)))) 
with_na <- names(train_test_original)[colSums(na_df)>0]
with_na



# Prepare the final df's 

train_df <- train_test_log %>% filter(!is.na(SalePrice_log)) %>% select(-Id) 
test_df <- train_test_log %>% filter(is.na(SalePrice_log)) %>% select(-Id) 

train_df_exp <- train_test_original %>% filter(!is.na(SalePrice)) %>% select(-Id) 
test_df_exp <- train_test_original %>% filter(is.na(SalePrice)) %>% select(-Id) 


X <- as.matrix(train_df %>% select( -SalePrice_log))
Y <- train_df$SalePrice_log
X_test <- as.matrix(test_df %>% select( -SalePrice_log))


 X_exp <- as.matrix(train_df_exp %>% select( -SalePrice))
 X_test_exp <- as.matrix(test_df_exp %>% select( -SalePrice))

# Now LM 
library(glmnet)

lambda <- 10^seq(10, -2, length = 100)

# Now lets try Lasso
cv.out <- cv.glmnet(X, Y, alpha = 1)
bestlam <- cv.out$lambda.min
bestlam

lasso.mod <- glmnet(X, Y, alpha = 1, lambda = lambda)
print(lasso.mod)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = X)

mean((lasso.pred-Y)^2)

# calculate residuals 
resid <- exp(Y)-exp(lasso.pred)


train_df <- train_df %>% select(-SalePrice_log) %>% mutate(resid = resid)
train_df_exp <- train_df_exp %>% select(-SalePrice) %>% mutate(resid = resid)

#rf <- randomForest(resid ~ . ,data = train_df, importance = TRUE,  ntree=500)
rf <- randomForest(resid ~ . ,data = train_df_exp, importance = TRUE,  ntree=500)

which.min(rf$mse)

# Impression
imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp
View(imp %>% add_rownames())
rf

plot(rf)

predict_resid_train <- predict(rf,train_df_exp)
mean((log(predict_resid_train+exp(lasso.pred))-Y)^2)


#predict_lasso <- exp(predict(lasso.mod, s = bestlam, newx = X_test))
predict_lasso <- exp(predict(lasso.mod, s = bestlam, newx = X_test))

predict_resid <- predict(rf,  test_df_exp)

predict <- predict_lasso+predict_resid


final_result_lasso <- train_test %>% filter(is.na(SalePrice)) %>% select(Id) %>% mutate(Id = as.integer(Id), SalePrice = round(predict))


#final_result_lasso$SalePrice

#dim(test_df[is.na(predict),])
final_result_lasso$SalePrice
write_csv(final_result_lasso, path = 'dennis_submit_lasso_rf.csv')


# Now XGboost 

# 
# xgbResult <- xgboost(params=list(max_depth=4,
#                                  eta=shrinkage,
#                                  gamma=0.0,
#                                  colsample_bytree=0.2,
#                                  min_child_weight=1.5, n_estimators = 7200, learning_rate=0.01,reg_alpha=0.9,reg_lambda=0.6,subsample=0.2,seed=42,silent=1 ),
#                      data=train_df,
#                      nrounds=numTrees,
#                      objective="binary:logistic",
#                      eval_metric="error")
# 
# testPreds <- predict(xgbResult, dtest)

# Try double lasso 

# Now lets try Lasso

cv.out <- cv.glmnet(X, Y, alpha = 1)
bestlam <- cv.out$lambda.min


lasso.mod <- glmnet(X, Y, alpha = 1, lambda = lambda)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = X)

mean((lasso.pred-Y)^2)

# calculate residuals 
resid <- exp(Y)-exp(lasso.pred)
resid_adj <- max(abs(resid))

train_df2 <- train_df %>% select(-SalePrice_log) %>% mutate(resid_log = log(resid+resid_adj+1))
test_df2 <- test_df %>% select(-SalePrice_log) 

X2 <- as.matrix(train_df2 %>% select( -resid_log))
Y2 <- train_df2$resid_log
X2_test <- as.matrix(test_df2 )


cv.out2 <- cv.glmnet(X2, Y2, alpha = 1)
bestlam2 <- cv.out$lambda.min


lasso.mod2 <- glmnet(X2, Y2, alpha = 1, lambda = lambda)
lasso.pred2 <- predict(lasso.mod2, s = bestlam2, newx = X2)

mean((lasso.pred2-Y2)^2)

predict_lasso_train <- exp(predict(lasso.mod, s = bestlam, newx = X))
predict_lasso2_train <- exp(predict(lasso.mod2, s = bestlam2, newx = X2))-resid_adj-1
predict_train <- predict_lasso_train+predict_lasso2_train
mean((log(predict_train)-Y)^2)

resid_2 <- exp(Y) - predict_train




train_df_exp3 <- train_df_exp %>% select(-SalePrice) %>% mutate(resid = resid_2)
test_df_exp3 <- test_df_exp %>% select(-SalePrice) 

rf2 <- randomForest(resid ~ . ,data = train_df_exp3, importance = TRUE,  ntree=500)

which.min(rf2$mse)

# Impression
imp <- as.data.frame(sort(importance(rf2)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp
#View(imp %>% add_rownames())
rf2

plot(rf2)

predict_resid_train_3 <- predict(rf2,train_df_exp3)

mean(((predict_resid_train_3+predict_train)-exp(Y))^2)




predict_lasso <- exp(predict(lasso.mod, s = bestlam, newx = X_test))
predict_lasso2 <- exp(predict(lasso.mod2, s = bestlam2, newx = X2_test))-resid_adj-1
predict_rf <- predict(rf,  test_df_exp3)

predict <- predict_lasso+predict_lasso2+predict_rf


final_result_dbl_lasso <- train_test %>% filter(is.na(SalePrice)) %>% select(Id) %>% mutate(Id = as.integer(Id), SalePrice = round(predict))


#final_result_lasso$SalePrice

#dim(test_df[is.na(predict),])
final_result_dbl_lasso$SalePrice
write_csv(final_result_dbl_lasso, path = 'dennis_submit_double_lasso.csv')
