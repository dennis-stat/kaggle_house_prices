# Fit quick tree
# Now fit the random forest 
rf_test <- randomForest(SalePrice ~ . ,train_test_log_stan %>% filter(train_test == 1) %>% select(-train_test) , importance = TRUE,  ntree=50)

which.min(rf_test$mse)
# 
# rf <- randomForest(resid ~ . ,data = train_df_exp %>% select(-SalePrice), importance = TRUE,  ntree=which.min(rf_test$mse))

# Importance
imp <- as.data.frame(sort(importance(rf_test)[,1],decreasing = TRUE),optional = T)
names(imp) <- c('perc_inc_mse') 
imp <- tbl_df((imp %>% rownames_to_column())) %>% select(var = rowname, perc_inc_mse)
imp <- imp %>% filter(var != 'GarageCars') %>% top_n(30)
View(imp)
print(imp)
ggplot(imp, aes(x = var, y = perc_inc_mse))+geom_bar(stat = "identity")
rf
length(rf$mse)

plot(rf)

   
stan_code_intercept <- "
data {
int<lower=1> K;
int<lower=1> J;
int<lower=1> N;

matrix[N,K] x;
vector[N] y;
int zipcode[N];
}

parameters {
real<lower=0> sigma;
real<lower=0> sigma_a;
real mu;
vector[J] a;
vector<lower=0>[K] beta;
real mu_a;
}
transformed parameters{
real mu_adj;
vector[J] a_adj;

mu_adj = mu + mean(a);
a_adj = a - mean(a);
}

model {

a~normal(mu_a,sigma_a);
mu_a ~ normal(0,100);

y~normal( mu_adj + a_adj[zipcode]  + x*beta,sigma);
}

generated quantities {
vector[N] y_hat;
y_hat = mu_adj + a_adj[zipcode] + x*beta;
}
"
train_test_log_stan <- train_test_log %>% scale(center = TRUE, scale = FALSE) %>% 
    as.data.frame() %>% tbl_df() %>% 
    mutate(Id = train_test_log$Id, train_test = train_test_log$train_test, SalePrice = train_test_log$SalePrice)

train_df_log_stan <- train_test_log_stan %>% filter(!is.na(SalePrice)) %>% select(-Id) 
test_df_log_stan <- train_test_log_stan %>% filter(is.na(SalePrice)) %>% select(-Id) 

y <- log(train$SalePrice)
X <- as.matrix(as.matrix(train_df_log_stan %>% select( -SalePrice, -train_test)))
X_test <- as.matrix(as.matrix(test_df_log_stan %>% select( -SalePrice, -train_test)))

B <- as.matrix(coef(lasso.mod))
lasso_index <- B[-1,1] 
lasso_index[grepl(x = names(lasso_index), pattern = 'Neighborhood')] <- FALSE
sum(lasso_index)
X_stan_train <- X[,as.vector(lasso_index)]
dim(X_stan_train)
X_stan_test <- X_test[,as.vector(lasso_index)]

dim(X_stan_train)
neighborhood_factor <- as.factor(train_test$Neighborhood)
neighborhood <- as.numeric(neighborhood_factor)

neighborhood_train <- neighborhood[train_test$train_test == 1]
neighborhood_test <- neighborhood[train_test$train_test == 0]

N <- length(neighborhood_train)
K <- dim(X_stan_train)[2]
J = length(unique(neighborhood_train))
dat <- list(
            K = K,
            J = J,
            N = N, 
            x = X_stan_train,
            y = y,
            zipcode = neighborhood_train)

# library(shinystan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit <- stan(model_code = stan_code_intercept, model_name = "house",
            data = dat, iter = 1200,warmup=200,  chains = 3 ,
            verbose = FALSE)

y_hat <- get_posterior_mean(fit,pars="y_hat")
sum((y_hat[,4]-mean(y))^2)/sum((y-mean(y))^2)
traceplot(fit,pars="mu_adj")

residual = y-y_hat[,3]
plot(y_hat[,3],residual)
hist(residual)


mean((residual)^2)

residual_exp = exp(y)-exp(y_hat[,3])
mean((residual_exp)^2)
resid_stan_exp <- train$SalePrice-exp(y_hat[,3])


a_adj <- get_posterior_mean(fit,pars="a_adj")
zip_price <- cbind(a_adj,levels(neighborhood_factor), 1:length(levels(neighborhood_factor)))
colnames(zip_price) <- c("chain1","chain2","both_chains","zipcode","index_zipcode")
zip_price <- as.data.frame(zip_price)
ggplot(zip_price,aes(factor(zipcode),chain1)) + geom_bar(stat="identity",fill='blue') 
beta <- get_posterior_mean(fit,pars="beta")[,3]

mu_adj <- get_posterior_mean(fit,pars="mu_adj")[,5]
a_adj <- get_posterior_mean(fit,pars="a_adj")[,5]
beta <- get_posterior_mean(fit,pars="beta")[,5]

y_hat_train <- mu_adj + a_adj[neighborhood_train] +X_stan_train %*% beta
y_hat_train-y_hat[,5]
# test

y_hat_test <- mu_adj + a_adj[neighborhood_test] + X_stan_test %*% beta
y_hat_test_exp <- exp(y_hat_test)
hist(y_hat_test_exp)

##############
#############
############

# First define optimal number of trees
rf <- randomForest(resid ~ . ,data = train_test %>% filter(train_test == 1) %>% select(-SalePrice, -train_test) %>% mutate(resid = resid_stan_exp, lasso_pred = exp(y_hat_train)), importance = TRUE,  ntree=500)

which.min(rf$mse)
# 
# rf <- randomForest(resid ~ . ,data = train_df_exp %>% select(-SalePrice), importance = TRUE,  ntree=which.min(rf_test$mse))

# Impression
imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp
print(tbl_df((imp %>% add_rownames())))
rf
length(rf$mse)

plot(rf)

predict_resid_train <- predict(rf,data = train_test %>% filter(train_test == 1) %>% select(-SalePrice, -train_test) %>% mutate(resid = resid_stan_exp, lasso_pred = exp(y_hat_train)) )
predict_resid_test <- predict(rf,data = train_test %>% filter(train_test == 0) %>% select(-SalePrice, -train_test) %>% mutate( lasso_pred = exp(y_hat_test)) )

# Comparing MSEs

mean((exp(y_hat_train)-train$SalePrice)^2)
mean((predict_resid_train-resid_stan_exp)^2)
mean((predict_resid_train+exp(y_hat_train)-train$SalePrice)^2)


# Final predict 
predict <- exp(y_hat_test)+predict_resid_test


final_result_lasso <- train_test %>% filter(is.na(SalePrice)) %>% select(Id) %>% mutate(Id = as.integer(Id), SalePrice = round(predict))


#final_result_lasso$SalePrice

#dim(test_df[is.na(predict),])
hist(final_result_lasso$SalePrice)
write_csv(final_result_lasso, path = 'dennis_submit_lasso_lm_rf.csv')

