library(tidyverse)
library(xray)
library(randomForest)
library(timeDate)
library(glmnet)
library(psych)

# CONVERT QUALITY TO CONT Variables! 


setwd("~/Desktop/kaggle house prices//")

###################### Load the data
train <- read_csv('train.csv')
test <- read_csv('test.csv')
###################### Explore the data


# Let's conbine them together 
train <- train %>% mutate(train = 1) %>% filter(Id != '524', Id != '1299')
test <- test %>% mutate(SalePrice = NA, train = 0)
train_test <- train %>% bind_rows(test)
#train_test <- train_test %>% filter(Id != '524', Id != '1299')

train_test$BedroomAbvGr_factor <- factor(train_test$BedroomAbvGr)

#train_test$OverallQual <- factor(train_test$OverallQual)
#train_test <- train_test %>% select(-GarageCars)
    
# Transform some integer variables into factors 
train_test <- train_test %>% 
    mutate(MSSubClass =  factor(MSSubClass), 
MoSold = factor(MoSold), 
YrSold_factor = factor(YrSold))

train_test$GarageYrBlt[train_test$GarageYrBlt == 2207] <- 2007


# train_test$NeighRich[train_test$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 'Top'
# train_test$NeighRich[!train_test$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 'Middle'
# train_test$NeighRich[train_test$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 'Bottom'

#train_test$NeighRich_Year <- factor(paste(train_test$Neighborhood, train_test$YrSold))
# Overall Quality - should we use it linear?

ggplot(train_test %>% filter(train == 1) , aes(x=factor(OverallQual), y=SalePrice))+
           geom_boxplot() + labs(x='Overall Quality') +
           scale_y_continuous(breaks= seq(0, 800000, by=100000))

ggplot(train_test %>% filter(train == 1) , aes(x=factor(OverallQual), y=SalePrice))+
    geom_boxplot() + labs(x='Overall Quality') +
    scale_y_continuous(breaks= seq(0, 800000, by=100000))


train_test_neib_sq_f_price <- train_test %>% filter(train == 1) %>% group_by(Neighborhood, YrSold) %>% summarize(price_sq_f = sum(SalePrice)/sum(TotalBsmtSF+GrLivArea), 
                                                                                                                 price =   mean(SalePrice)                                                                                                  ) %>% 
    arrange(YrSold, -price_sq_f)

ggplot(train_test_neib_sq_f_price, aes(x = YrSold, y = price_sq_f, group = Neighborhood, color = Neighborhood))+geom_line(stat = 'identity')


ggplot(train_test_neib_sq_f_price, aes(x = YrSold, y = price, group = Neighborhood, color = Neighborhood))+geom_line(stat = 'identity')


# Fix some NA's 
train_test <- train_test %>% 
    mutate(is_new_Garage = as.integer(abs(GarageYrBlt-YearBuilt)<1)) %>% 
    mutate(is_Bsmt_unfinished = as.integer((BsmtUnfSF/TotalBsmtSF)>0.5)) %>% 
    replace_na(list(Alley = 'No Alley', 
                    is_Bsmt_unfinished = 0, 
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
                   GarageYrBlt =  1979, #median(train_test$GarageYrBlt, na.rm = TRUE), 
                    GarageType = 'No garage', 
                    GarageQual = 'No garage', 
                    GarageCond = 'No garage', 
                    
                    GarageCars = 0, 
                    GarageArea = 0, 
                    KitchenQual = 'NA', 
                    Functional = 'NA', 
                    FireplaceQu = 'NA', 
                    MiscFeature = 'None', 
                    SaleType = 'WD', 
                    MSZoning = 'RL', 
                    Fence = 'No Fence', 
                    Exterior1st = 'VinylSd', 
                    Exterior2nd = 'VinylSd', 
                    Electrical = 'SBrkr', 
                    LotFrontage = 0, 
                    PoolQC = 'NA', 
                is_new_Garage = 0)) %>% 
    #mutate(  SF = GrLivArea+TotalBsmtSF)%>% 
    mutate(GarageYrBlt = ifelse(GarageYrBlt>YrSold, YrSold,GarageYrBlt), 
           YearBuilt = ifelse(YearBuilt>YrSold, YrSold,YearBuilt),
           YearRemodAdd = ifelse(YearRemodAdd>YrSold, YrSold,YearRemodAdd),
           is_EnclosedPorch = ifelse(EnclosedPorch == 0, 0,1), 
          # is_NeighRich = ifelse(Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge'), 1,0), 
           

           is_OpenPorch = ifelse(OpenPorchSF == 0, 0,1), 
           is_WoodDeck = ifelse(WoodDeckSF == 0, 0, 1), 
           is_Pool = ifelse(PoolArea == 0, 0, 1) , 
           is_LotFrontage = ifelse(LotFrontage == 0, 0, 1) 
    ) %>% # Change some names - dont like column names starting with number 
    mutate(FirstFlrSF = `1stFlrSF`, SecondFlrSF = `2ndFlrSF`, ThreeSsnPorch = `3SsnPorch`) %>% 
    select(-`1stFlrSF`, -`2ndFlrSF`, -`3SsnPorch`, TotalBsmtSF)  #%>% 
    #%>% 
    
    
   #a mutate(YrSold = YrSold-min(YrSold))
#train_test <- train_test %>% mutate(TotalSqFeet = GrLivArea +  TotalBsmtSF) #, NeighRich = NA)

# Transform Miscfeatures 
 
train_test_misc <- train_test %>% select(Id, MiscFeature, MiscVal) %>% group_by(Id) %>% 
    mutate(MiscVal = ifelse(MiscFeature == 'None', 0, MiscVal)) %>% 
    spread(key = MiscFeature, value = MiscVal, fill = 0) %>% ungroup() %>% select(-None, -Id)

train_test <- train_test %>% select(-MiscFeature, -MiscVal) %>% bind_cols(train_test_misc)

# Create Year + Neighborhood

Yr_Nb <- train_test %>% group_by(Id) %>% summarise(Neighborhood_YrSold = factor(paste(Neighborhood, YrSold, sep = '_'))) 


Yr_Nb_dummy <- train_test %>% 
    group_by(Id) %>% 
    summarise(Neighborhood_YrSold = factor(paste(Neighborhood, YrSold, sep = '_')))  %>% ungroup() %>% mutate(is_true = 1) %>% spread(key = Neighborhood_YrSold, value = is_true, fill = 0) %>% gather(key = Neighborhood_YrSold, value = 'is_true', 2:2) %>% arrange(Id) %>% select(-Neighborhood_YrSold)


Yr_Nb_slope <- train_test %>% filter(train == 1) %>% 
    mutate(SalePrice_SF = SalePrice/(GrLivArea+TotalBsmtSF)) %>% 
    select(Id, SalePrice,Neighborhood,  YrSold) %>% 
    mutate(Neighborhood = factor(Neighborhood)) %>% 
    group_by( Neighborhood) %>% 
    do(tidy(lm(SalePrice ~ YrSold, .)))

table(Yr_Nb_slope$term)
Yr_Nb_slope_slope <- Yr_Nb_slope %>% filter(term == 'YrSold')
Yr_Nb_ic <- Yr_Nb_slope %>% filter(term == '(Intercept)')

Yr_Nb_slope_final  <- Yr_Nb_slope_slope %>% select(Neighborhood, slope_NB = estimate ) %>% ungroup() %>% 
    mutate(slope_NB = slope_NB -min(slope_NB)+1)

Yr_Nb_ic_final  <- Yr_Nb_ic %>% select(Neighborhood, ic = estimate ) %>% ungroup() %>% 
    mutate(slope_NB = ic -min(ic)+1)

train_test <- train_test %>% left_join(Yr_Nb_slope_final)

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
    mutate(is_not_Remod = as.integer(YearRemodAdd == YearBuilt),
           is_new = as.integer(abs(YrSold-YearBuilt)<1),
           YearBuilt_age = YrSold-YearBuilt+1,
           YearRemodAdd_age = YrSold-YearRemodAdd+1,
           GarageYrBlt_age = YrSold-GarageYrBlt+1, 
           YearBuilt_bucket = factor(
               round(
                   (
                       (YearBuilt_age-min(YearBuilt_age))/(max(YearBuilt_age)-min(YearBuilt_age)))/0.25
                    )
                     ),
           YearRemodAdd_bucket = factor(round(((YearRemodAdd_age-min(YearRemodAdd_age))/
                                                   (max(YearRemodAdd_age)-min(YearRemodAdd_age)))/0.25))
           
          # GarageYrBlt_bucket = factor(round((order(GarageYrBlt)/max(order(GarageYrBlt))/0.2)))
    ) %>% select( -YearRemodAdd, -GarageYrBlt, -Utilities, -YrSold)  # Utilities are useless - they have only one value 
#


# Transform all non-numeric columns to factors
train_test <- train_test %>% mutate_if(is.character, factor )
str(train_test)

# k = ncol(train_test[!sapply(train_test,  is.factor)])
# for(i in (1:ncol(train_test))[!sapply(train_test,  is.factor)]){
#     if ((names(train_test)[i] != 'Id')) {
#     if (abs(skew(train_test[,i]))>1 ) {
#         train_test[,i] <- log(train_test[,i] +1)
#         k = k-1
#     }
#     }
# }
# k

# Separate numeric columns and convert then to logs
train_test_numeric <- train_test %>% 
     select_if(
         funs(is.numeric(.) & length(unique(.)) > 2 & max(as.integer(.), na.rm = T)>1 & !is.factor(.) )
         ) %>% select(-Id)


# Convert all numerics to logs
train_test_numeric_skew <- train_test_numeric #%>% select_if(funs(abs(skew(.))>0.9 )) 
names(train_test_numeric_skew)
names(train_test_numeric)

train_test_numeric_skew_log <- train_test_numeric_skew %>% 
    mutate_all(funs(log(1 + .)))
names(train_test_numeric_skew_log) <- paste0(names(train_test_numeric_skew_log), '_log') 
train_test_numeric_skew_log <- train_test_numeric_skew_log %>% mutate(SalePrice = SalePrice_log) %>% select(-SalePrice_log)

train_test_numeric_non_skew <- train_test_numeric %>% select(-SalePrice) #%>% 
#    select_if(funs(abs(skew(.))<=1 )) 


train_test_numeric_already_dummy <- train_test %>% 
    select_if(
        funs(is.numeric(.) 
             & max(as.integer(.), na.rm = T)==1 
             & min(as.integer(.), na.rm = T)==0 )
    ) %>% select(-train)

# Create dummy variables 
train_test_non_numeric <- train_test %>% select(-one_of(names(train_test_numeric))) %>% select(-Id, -train) %>% select(-one_of(names(train_test_numeric_already_dummy)))

library(caret)
train_test_non_numeric_dummy <- data.frame(predict(dummyVars(" ~ .", data = train_test_non_numeric), newdata = train_test_non_numeric))
#View(train_test_non_numeric)

# Now construct data sets back 
train_test_log <- train_test %>% 
    select(Id, train) %>% 
    bind_cols(train_test_numeric_skew_log) %>% 
    bind_cols(train_test_numeric_non_skew) %>%
    bind_cols(train_test_numeric_already_dummy) %>% 
    bind_cols(train_test_non_numeric_dummy)# %>% 
  #  bind_cols(Yr_Nb_dummy %>% select(-Id))

train_test_non_dummy <- 
    train_test %>% 
    select(Id, train) %>% 
    bind_cols(train_test_numeric_skew) %>% 
    bind_cols(train_test_numeric_non_skew) %>%
    bind_cols(train_test_numeric_already_dummy) %>% 
    bind_cols(train_test_non_numeric) # %>% 
  #  bind_cols(Yr_Nb_dummy %>% select(-Id))

# Final test for NA's in variables 
na_df <- train_test_log %>% 
    summarise_each(funs(sum(is.na(.) | is.infinite(.) | is.nan(.)))) 
with_na <- names(train_test_log)[colSums(na_df)>0]
with_na


# Prepare the final df's 
train_df_log <- train_test_log %>% filter(!is.na(SalePrice)) %>% select(-Id) 
test_df_log <- train_test_log %>% filter(is.na(SalePrice)) %>% select(-Id) 

Y <- train_df_log$SalePrice
X <- as.matrix(train_df_log %>% select( -SalePrice, -train))
X_test <- as.matrix(test_df_log %>% select( -SalePrice, -train))

# Now lets fit Lasso LM 

# Set range of lambda values 
lambda <- 10^seq(5, -4, length = 100)

# Now lets try Lasso
cv.out <- cv.glmnet(X, Y, alpha = 1, lambda = lambda)
bestlam <- cv.out$lambda.min
bestlam

lasso.mod <- glmnet(X, Y, alpha = 1, lambda = bestlam)
B <- as.matrix(coef(lasso.mod))
lasso_index <- B[-1,1] > 0
sum(lasso_index)

lasso.pred_log.train <- exp(predict(lasso.mod, s = bestlam, newx = X))
lasso.pred_log.test <- exp(predict(lasso.mod, s = bestlam, newx = X_test))

print(mean((lasso.pred_log.train-exp(Y))^2))




# calculate residuals 
resid <- train$SalePrice-lasso.pred_log.train

mean(((resid))^2)
hist(resid)

names(train_test_non_dummy) <- make.names(names(train_test_non_dummy))

train_test_non_dummy <- train_test_non_dummy %>% 
    mutate(SalePrice = 0) #%>% select(-NeighRich_Year)
# Now fit the random forest 
rf <- randomForest(resid ~ . ,train_test_non_dummy %>% filter(train == 1)  %>% mutate(resid = resid, lasso_pred = lasso.pred_log.train) %>% select(-SalePrice, -train) , importance = TRUE,  ntree=1000 )

which.min(rf$mse)
plot(rf)
# Importance
imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp
print(tbl_df((imp %>% add_rownames())))
rf
length(rf$mse)
View(imp %>% add_rownames())
plot(rf)

predict_resid_train <- predict(rf,train_test_non_dummy %>% filter(train == 1)  %>% mutate(lasso_pred = lasso.pred_log.train) %>% select(-SalePrice, -train))


# Comparing MSEs

mean((lasso.pred_log.train-train$SalePrice)^2)
mean((predict_resid_train-resid)^2)
mean((predict_resid_train+lasso.pred_log.train-train$SalePrice)^2)
predict_train <- predict_resid_train+lasso.pred_log.train
train_check <- train %>% mutate(predict_train = predict_train, delta = SalePrice - predict_train, delta_abs = abs(delta)) #%>% arrange(-delta_abs)
View(train_check)
plot(predict_train~train$SalePrice)

delta = predict_resid_train+lasso.pred_log.train-train$SalePrice
price = train$SalePrice
options("scipen"=10)
plot(price~delta)
which.max(delta)



hist(delta)
# Now calculate test 
predict_resid_test <- predict(rf,train_test_non_dummy %>% filter(train == 0) %>% select(-SalePrice, -train)%>% mutate(lasso_pred = lasso.pred_log.test) )


# Final predict 

predict <- lasso.pred_log.test+predict_resid_test


final_result_lasso <- train_test %>% filter(is.na(SalePrice)) %>% select(Id) %>% mutate(Id = as.integer(Id), SalePrice = round(predict))

final_result_lasso_all <- train_test %>% filter(is.na(SalePrice)) %>% mutate(Id = as.integer(Id), SalePrice_final_predict = round(predict), predict_resid_test = predict_resid_test)


#final_result_lasso$SalePrice

#dim(test_df[is.na(predict),])
hist(final_result_lasso$SalePrice)
hist(train$SalePrice)

write_csv(final_result_lasso, path = 'dennis_submit_lasso_lm_rf.csv')

