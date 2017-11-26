fill_random_forest <- function(var_name, df) {
    names(df)[names(df)==var_name] = 'Outcome'
    rf <- randomForest(Outcome ~ MSSubClass + LotArea + Street + LotShape + LandContour + LotConfig + LandSlope + Neighborhood + Condition1 +
                           #Condition2 + 
                           BldgType + 
                           #HouseStyle + 
                           OverallQual + OverallCond +
                           YearBuilt + YearRemodAdd + RoofStyle + 
                           
                           #RoofMatl + 
                           ExterQual + 
                           ExterCond + 
                           Foundation + 
                           #Heating + 
                           
                           HeatingQC + CentralAir + LowQualFinSF + 
                           GrLivArea + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Fireplaces + PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch +  ScreenPorch + PoolArea + MiscVal + MoSold + YrSold + SaleCondition + is_residential + ExterQual_int + ExterCond_int + is_Bsmt + BsmtQual_int + BsmtCond_int + BsmtExposure_int + KitchenQual_int + FireplaceQu_int + GarageQual_int + GarageCond_int + PoolQC_int 
                       , data = df %>% filter(!is.na(Outcome)), importance = TRUE, ntree=100)
    which.min(rf$mse)
    
    # Impression
    imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
    names(imp) <- "% Inc MSE"
    imp
    
    # Prediction
    predict <- predict(rf,df %>% filter(is.na(Outcome)))
    
    df_new <- df
df_new$Outcome[is.na(df_new$Outcome)] <- predict
df_new$Outcome
    }
