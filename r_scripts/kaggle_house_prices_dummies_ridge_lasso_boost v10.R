library(tidyverse)
library(xray)
library(randomForest)
library(timeDate)
library(glmnet)



setwd("~/Desktop/kaggle house prices//")

###################### Load the data
train <- read_csv('train.csv')
test <- read_csv('test.csv')
###################### Explore the data


# Let's conbine them together 
train <- train %>% mutate(train_test = 1) %>% filter(Id != 524)
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

# 
# # Combine Condition1-2
# Cond1_dummy <- train_test %>% select(Id, Condition1) %>% mutate(is_true = 1) %>% spread(key = Condition1, value = is_true, fill = 0) %>% gather(key = Condition, value = 'is_true', 2:(length(unique(train_test$Condition1))+1)) %>% arrange(Id)
# 
# Cond2_dummy <- train_test %>% select(Id, Condition1) %>% mutate(is_true = 1) %>% spread(key = Condition1, value = is_true, fill = 0) %>% gather(key = Condition, value = 'is_true', 2:(length(unique(train_test$Condition1))+1)) %>% arrange(Id)
# 
# Cond_dummy <- Cond1_dummy %>% bind_rows(Cond2_dummy) %>% group_by(Id, Condition) %>%
#     summarise(is_true = sum(is_true))  %>% mutate(is_true = ifelse(is_true > 0, 1,0)) %>% arrange(-is_true) %>% spread(Condition, value = is_true, fill = 0)
# 
# train_test <- train_test %>% select(-Condition1, -Condition2) %>% left_join(Cond_dummy)
# 
# # Exterior1st and Exterior2nd
# Ext1_dummy <- train_test %>% select(Id, Exterior1st) %>% mutate(is_true = 1) %>% spread(key = Exterior1st, value = is_true, fill = 0) %>% gather(key = Exterior, value = 'is_true', 2:(length(unique(train_test$Exterior1st))+1)) %>% arrange(Id)
# 
# Ext2_dummy <- train_test %>% select(Id, Exterior2nd) %>% mutate(is_true = 1) %>% spread(key = Exterior2nd, value = is_true, fill = 0) %>% gather(key = Exterior, value = 'is_true', 2:(length(unique(train_test$Exterior2nd))+1)) %>% arrange(Id)
# 
# Exterior_dummy <- Ext1_dummy %>% bind_rows(Ext2_dummy) %>% group_by(Id, Exterior) %>%
#     summarise(is_true = sum(is_true))  %>% mutate(is_true = ifelse(is_true > 0, 1,0)) %>% arrange(-is_true) %>% spread(Exterior, value = is_true, fill = 0)
# 
# train_test <- train_test %>% select(-Exterior1st, -Exterior2nd) %>% left_join(Exterior_dummy)


# Model some new variables 

train_test <- train_test %>% 
    mutate(is_Remod = as.integer(YearRemodAdd == YearBuilt),
           YearBuilt_age = max(YearBuilt)-YearBuilt,
           YearRemodAdd_age = max(YearRemodAdd)-YearRemodAdd,
           GarageYrBlt_age = max(GarageYrBlt)-GarageYrBlt, 
           YearBuilt_bucket = factor(round((order(YearBuilt)/max(order(YearBuilt))/0.2))),
           YearRemodAdd_bucket = factor(round((order(YearRemodAdd)/max(order(YearRemodAdd))/0.2))),
           GarageYrBlt_bucket = factor(round((order(GarageYrBlt)/max(order(GarageYrBlt))/0.2)))
    ) %>% select(-YearBuilt, -YearRemodAdd, -GarageYrBlt, -Utilities)  # Utilities are useless - they have only one value 

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
# names(train_test_numeric_log) <- paste0(names(train_test_numeric_log), '_log')

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


# Final test for NA's in variables 
na_df <- train_test_original %>% 
    summarise_each(funs(sum(is.na(.) | is.infinite(.) | is.nan(.)))) 
with_na <- names(train_test_original)[colSums(na_df)>0]
with_na



# Prepare the final df's 

train_df_log <- train_test_log %>% filter(!is.na(SalePrice)) %>% select(-Id) 
test_df_log <- train_test_log %>% filter(is.na(SalePrice)) %>% select(-Id) 

train_df_exp <- train_test_original %>% filter(!is.na(SalePrice)) %>% select(-Id) 
test_df_exp <- train_test_original %>% filter(is.na(SalePrice)) %>% select(-Id) 


fit_lasso <- function(train_df, test_df) {
Y <- train_df$SalePrice
X <- as.matrix(train_df %>% select( -SalePrice))
X_test <- as.matrix(test_df %>% select( -SalePrice))

# Now LM 

# Set range of lambda values 
lambda <- 10^seq(10, -2, length = 100)

# Now lets try Lasso
cv.out <- cv.glmnet(X, Y, alpha = 1)
bestlam <- cv.out$lambda.min
bestlam

lasso.mod <- glmnet(X, Y, alpha = 1, lambda = lambda)
coef(lasso.mod)
lasso.pred.train <- predict(lasso.mod, s = bestlam, newx = X)
lasso.pred.test <- predict(lasso.mod, s = bestlam, newx = X_test)

small.lambda.index <- which(cv.out$lambda == bestlam)
small.lambda.betas <- cv.out$glmnet.fit$beta[, small.lambda.index]
small.lambda.betas[order(-small.lambda.betas)[1:10]]

print(mean((lasso.pred.train-Y)^2))


return(list(lasso.pred.train, lasso.pred.test))
}

# collect two predictions

# log data model
lasso.pred_log.list <- fit_lasso(train_df_log, test_df_log)
lasso.pred_log.train <- exp(lasso.pred_log.list[[1]])
lasso.pred_log.test <- exp(lasso.pred_log.list[[2]])

#original data model
lasso.pred.list.exp <- fit_lasso(train_df_exp, test_df_exp)
lasso.pred_exp.train <- lasso.pred.list.exp[[1]]
lasso.pred_exp.test <- lasso.pred.list.exp[[2]]

# now fit model into lm 
lm.exp.log.train.data = data.frame(SalePrice = train_df_exp$SalePrice, lasso.pred_log = lasso.pred_log.train, lasso.pred_exp = lasso.pred_exp.train)

lm.exp.log.test.data = data.frame(SalePrice = test_df_exp$SalePrice, lasso.pred_log = lasso.pred_log.test, lasso.pred_exp = lasso.pred_exp.test)

plot(lasso.pred_log.train~lasso.pred_exp.train)


# Lets do the same with Lasso / Ridge 

# Set range of lambda values 
lambda <- 10^seq(10, -2, length = 100)

# Now lets try Lasso
X_full <- as.matrix(lm.exp.log.train.data %>% filter(!is.na(SalePrice)) %>% select(-SalePrice))
X_full_test <- as.matrix(lm.exp.log.test.data %>% filter(is.na(SalePrice)) %>% select(-SalePrice))
Y_full <- (lm.exp.log.train.data %>%  filter(!is.na(SalePrice)))$SalePrice
cv.out.full <- cv.glmnet(X_full, Y_full)
bestlam.full <- cv.out.full$lambda.min
bestlam.full

lasso.mod.full <- glmnet(X_full, Y_full,  lambda = bestlam.full)
coef(lasso.mod.full)
lasso.pred.train.full <-  lasso.pred_log.train # predict(lasso.mod.full, s = bestlam.full, newx = X_full)
lasso.pred.test.full <- lasso.pred_log.test # predict(lasso.mod.full, s = bestlam.full, newx = X_full_test)

mean((lasso.pred.train.full-train_df_exp$SalePrice)^2)
mean((lasso.pred_exp.train-train_df_exp$SalePrice)^2)

# calculate residuals 
resid <- train_df_exp$SalePrice-lasso.pred.train.full
mean(((resid))^2)

# Now fit the rendom forest 
# First define optimal number of trees
rf <- randomForest(resid ~ . ,data = train_test %>% filter(train_test == 1) %>% select(-SalePrice, -train_test) %>% mutate(resid = resid, lasso_pred = lasso.pred.train.full), importance = TRUE,  ntree=500)

which.min(rf$mse)
# 
# rf <- randomForest(resid ~ . ,data = train_df_exp %>% select(-SalePrice), importance = TRUE,  ntree=which.min(rf_test$mse))

# Impression
imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp
print(tbl_df((imp %>% add_rownames())))
#rf
length(rf$mse)

plot(rf)

predict_resid_train <- predict(rf,train_test %>% filter(train_test == 1) %>% select(-SalePrice, -train_test)%>% mutate( lasso_pred = lasso.pred.train.full) )

# Comparing MSEs

mean((lasso.pred.train.log-train_df_exp$SalePrice)^2)
mean((lasso.pred.train.exp-train_df_exp$SalePrice)^2)
mean((lasso.pred.train.full-train_df_exp$SalePrice)^2)
mean((predict_resid_train-resid)^2)
mean((predict_resid_train+lasso.pred.train.full-train_df_exp$SalePrice)^2)

delta = predict_resid_train+lasso.pred.train.full-train_df_exp$SalePrice
price = train_df_exp$SalePrice
options("scipen"=10)
plot(price~delta)
which.max(delta)



hist(delta)
# Now calculate test 
predict_resid_test <- predict(rf,train_test %>% filter(train_test == 0) %>% select(-SalePrice, -train_test)%>% mutate(lasso_pred = lasso.pred.test.full) )


# Final predict 
predict <- lasso.pred.test.full+predict_resid_test


final_result_lasso <- train_test %>% filter(is.na(SalePrice)) %>% select(Id) %>% mutate(Id = as.integer(Id), SalePrice = round(predict))


#final_result_lasso$SalePrice

#dim(test_df[is.na(predict),])
hist(final_result_lasso$SalePrice)
write_csv(final_result_lasso, path = 'dennis_submit_lasso_lm_rf.csv')

