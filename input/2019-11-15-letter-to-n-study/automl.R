# Step 1 -----
# required packages
# install vip from github repo: devtools::install_github("koalaverse/vip")
library(lime)       # ML local interpretation
library(vip)        # ML global interpretation
library(pdp)        # ML global interpretation
library(ggplot2)    # visualization pkg leveraged by above packages
library(caret)      # ML model building
library(h2o)        # ML model building

n_seed = 30116

# initialize h2o
h2o.init()
## 
## H2O is not running yet, starting it now...
## 
## Note:  In case of errors look at the following log files:
##     /var/folders/ws/qs4y2bnx1xs_4y9t0zbdjsvh0000gn/T//RtmpIqxdOK/h2o_bradboehmke_started_from_r.out
##     /var/folders/ws/qs4y2bnx1xs_4y9t0zbdjsvh0000gn/T//RtmpIqxdOK/h2o_bradboehmke_started_from_r.err
## 
## 
## Starting H2O JVM and connecting: .. Connection successful!
## 
## R is connected to the H2O cluster: 
##     H2O cluster uptime:         2 seconds 469 milliseconds 
##     H2O cluster timezone:       America/New_York 
##     H2O data parsing timezone:  UTC 
##     H2O cluster version:        3.18.0.11 
##     H2O cluster version age:    15 days  
##     H2O cluster name:           H2O_started_from_R_bradboehmke_tnu907 
##     H2O cluster total nodes:    1 
##     H2O cluster total memory:   1.78 GB 
##     H2O cluster total cores:    4 
##     H2O cluster allowed cores:  4 
##     H2O cluster healthy:        TRUE 
##     H2O Connection ip:          localhost 
##     H2O Connection port:        54321 
##     H2O Connection proxy:       NA 
##     H2O Internal Security:      FALSE 
##     H2O API Extensions:         XGBoost, Algos, AutoML, Core V3, Core V4 
##     R Version:                  R version 3.5.0 (2018-04-23)
h2o.no_progress()
h2o.removeAll()

# Script 1 -----
# 

# create data sets
#mlResData <- resDiffData[,c(1,4:63,66)]
mlResData <- resData[,c(1,38,40,41,42,15,10,11,56,58,60,61,62,63,66)]
mlResData$runData <- as.factor(mlResData$runData)

target = "runData"
features <- setdiff(colnames(mlResData),target)

h_ds = as.h2o(mlResData)

# Split Train/Test
h_split = h2o.splitFrame(h_ds, ratios = 0.75, seed = n_seed)
h_train = h_split[[1]] # 75% for modelling
h_test = h_split[[2]] # 25% for evaluation

x_train <- as.data.frame(h_train)
x_test <- as.data.frame(h_test)[,-1]

# Train a Default H2O GBM model
model_gbm = h2o.gbm(x = features,
                    y = target,
                    training_frame = h_train,
                    model_id = "gbm_default_class",
                    seed = n_seed)
print(model_gbm)


# Train multiple H2O models with H2O AutoML
# Stacked Ensembles will be created from those H2O models
# You tell H2O ...
#     1) how much time you have and/or 
#     2) how many models do you want
# Note: H2O deep learning algo on multi-core is stochastic
model_automl = h2o.automl(x = features,
                          y = target,
                          training_frame = h_train,
                          nfolds = 5,               # Cross-Validation
                          max_runtime_secs = 600,   # Max time
                          max_models = 5,         # Max no. of models
                          stopping_metric = "RMSE", # Metric to optimize
                          project_name = "automl_class",
                          exclude_algos = NULL,     # If you want to exclude any algo 
                          seed = n_seed)

model_automl@leaderboard
# H2O: Model Leader
# Best Model (either an individual model or a stacked ensemble)
model_automl@leader

# Default GBM Model
h2o.auc(h2o.performance(model_gbm, newdata = h_test))
# Best model from AutoML
h2o.auc(h2o.performance(model_automl@leader, newdata = h_test)) # higher AUC = better

yhat_test = h2o.predict(model_automl@leader, h_test)
head(yhat_test)

# Save model to disk
# h2o.saveModel(object = model_automl@leader, 
#               path = "./models/",
#               force = TRUE)


explainer = lime::lime(x = as.data.frame(h_train[, features]),
                       model = model_automl@leader)

# Create explanations
explanations = lime::explain(x = x_test[sample(1:nrow(x_test),8),],
                             explainer = explainer,
                             n_permutations = 5000,
                             feature_select = "lasso_path",
                             dist_fun = "manhattan",
                             kernel_width = 3,
                             n_labels = 1,    # Binary classification
                             n_features = 5) # Look top x features
lime::plot_features(explanations)
plot_explanations(explanations)

# Sort explanations by feature weight
explanations = 
  explanations[order(explanations$feature_weight, decreasing = TRUE),]
# Print Table
print(explanations)
