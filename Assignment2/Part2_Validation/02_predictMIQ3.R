# Define libraries and options for the cleaning process

packages <- c("readr", "tidyr", "stringr", "stargazer", "Hmisc", "ggplot2", 'extrafont', 'ggpubr', 'fixest','lmtest','sandwich','gbm','stargazer', 'caret','grid','rattle','ranger','pdp')

# Install packages if not already installed
 for (package in packages) {
   if (!require(package, character.only = TRUE)) {
     install.packages(package)
     library(package, character.only = TRUE)
   }
 }

library(dplyr)
library(readr)
library(ggplot2)
library(extrafont)
library(ggpubr)
library(fixest)
library(lmtest)
library(sandwich)
library(gbm)
library(stargazer)
library(caret)
library(grid)
library(rattle)
library(ranger)
library(pdp)

options(digits = 3)

# Define folders and source functions

#setwd("C:/Users/Castelli/Desktop/WU_exams/CEU_WiSe25/Machine_Learning/Assignment2/")

#source("C:/Users/Castelli/DataAnalysis3/da_case_studies/ch00-tech-prep/theme_bg.R")
#source("C:/Users/Castelli/DataAnalysis3/da_case_studies/ch00-tech-prep/da_helper_functions.R")

# Read Database ----

# read db
# MIQ3_db <- read_rds("MIQ3_db_clean.rds")
MIQ3_db <- read_rds('https://osf.io/euhvs/download')
names(MIQ3_db)
length(unique(MIQ3_db$id)) # unique ID for each observation


# Prediction ---- 

## 1. OLS ----

# Define variables for regression
# OLS1: basic var
# OLS2: basic var + host & review feature
# OLS3: basic var + host & review feature + amenities dummies 
# OLS4: basic var + host & review feature + amenities dummies + neighbourhood dummies

# basic variables
# ref category bedrooms: 0-studio flat
# ref category room time: entire property
basic_var <- c('accommodates' ,  'bathrooms' ,  'bedrooms_f_1Broom' , 'bedrooms_f_2Broom','bedrooms_f_3Broom' , 'bedrooms_f_4Broom' ,'`bedrooms_f_5+Broom`' , '`room_type_f_Private room`' ,  '`room_type_f_Shared room`' )
# host & review
host_review_var <- c("d_miss_resp_time" ,  "host_resptime_n" ,  "d_miss_superhost" ,  "host_is_superhost" ,  "availability_30" ,  "number_of_reviews" ,  "minimum_nights" ,  "maximum_nights")
vars_ols2 <- c(basic_var, host_review_var)
# amenities dummies 
amenities_var <- names(MIQ3_db)[136:144]
vars_ols3 <- c(basic_var, host_review_var, amenities_var)
# neighbourhood dummies
neigh_var <- names(MIQ3_db)[c(43:129)]
vars_ols4 <- c(basic_var, host_review_var, amenities_var, neigh_var)

# translate into formula objects                   
formula_ols1  <- paste("price_ln ~", paste(basic_var, collapse = " + "))
formula_ols1  <- as.formula(formula_ols1 )

formula_ols2  <- paste("price_ln ~", paste(vars_ols2, collapse = " + "))
formula_ols2  <- as.formula(formula_ols2)

formula_ols3  <- paste("price_ln ~", paste(vars_ols3, collapse = " + "))
formula_ols3  <- as.formula(formula_ols3)

formula_ols4  <- paste("price_ln ~", paste(vars_ols4, collapse = " + "))
formula_ols4  <- as.formula(formula_ols4)


# Fit the linear model - whole MIQ3_db
OLS1_MIQ3 <- lm(formula_ols1 , data = MIQ3_db)
OLS2_MIQ3 <- lm(formula_ols2 , data = MIQ3_db)
OLS3_MIQ3 <- lm(formula_ols3 , data = MIQ3_db)
OLS4_MIQ3 <- lm(formula_ols4 , data = MIQ3_db)

# Regression results
#stargazer_r(list(OLS1_MIQ3, OLS2_MIQ3, OLS3_MIQ3, OLS4_MIQ3), float=F, se = 'robust', digits=2, dep.var.caption = "Dep. var: ln price", keep.stat = c("rsq","n"), no.space = T, omit = "^neighbourhood_cleansed", add.lines = list(c("Neighbourhood", "No", "No", "No", "Yes")))
stargazer(OLS1_MIQ3, OLS2_MIQ3, OLS3_MIQ3, OLS4_MIQ3, type = 'text',  omit = "^neighbourhood_cleansed", add.lines = list(c("Neighbourhood", "No", "No", "No", "Yes")))

# Comparison model statistics
# evaluation of the models
models <- c("OLS1_MIQ3", "OLS2_MIQ3", "OLS3_MIQ3", "OLS4_MIQ3")
AIC <- c()
BIC <- c()
RMSE <- c()
RSquared <- c()
regr <- c()
k <- c()

# Get for all models
for ( i in 1:length(models)){
  AIC[i] <- AIC(get(models[i]))
  BIC[i] <- BIC(get(models[i]))
  RMSE[i] <- RMSE(predict(get(models[i])), get(models[i])$model$price_ln)
  RSquared[i] <-summary(get(models[i]))$r.squared
  regr[[i]] <- coeftest(get(models[i]), vcov = sandwich)
  k[i] <- get(models[i])$rank -1
}

# create summary table for all models (training and test)
eval <- data.frame(models, k, RSquared, RMSE, BIC)
eval <- eval %>%
  #mutate(models_train = paste0("(",gsub("reg","",models),")")) %>%
  rename(Model = models, "R-squared" = RSquared, "RMSE" = RMSE, "N predictors" = k)

# OLS LINE HORSE TAB ---- 
stargazer(eval, summary = F, digits=2, float = F, no.space = T, type = 'text')
# Model 4 confirmed as best one

## 2. LASSO ----

# Use best OLS model 4 
# Set lasso tuning parameters
train_control <- trainControl(method = "cv", number = 5)
tune_grid <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

set.seed(1234)
lasso_model_MIQ3 <- caret::train(formula_ols4,
                                  data = MIQ3_db,
                                  method = "glmnet",
                                  preProcess = c("center", "scale"),
                                  trControl = train_control,
                                  tuneGrid = tune_grid,
                                  na.action=na.exclude)
print(lasso_model_MIQ3$bestTune$lambda)

lasso_coeffs_MIQ3 <- coef(lasso_model_MIQ3$finalModel, lasso_model_MIQ3$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") 

print(lasso_coeffs_MIQ3)

# RMSE Lasso
# filter LASSO coeffs > 0
lasso_coeffs_MIQ3_nz <- lasso_coeffs_MIQ3 %>%
  filter(s1!=0)

print(nrow(lasso_coeffs_MIQ3_nz))
# LASSO VARS ---- 
stargazer(lasso_coeffs_MIQ3_nz ,summary = F, type = 'text')

# Evaluate model. CV error:
lasso_cv_rmse_MIQ3 <- lasso_model_MIQ3$results %>%
  filter(lambda == lasso_model_MIQ3$bestTune$lambda) %>%
  dplyr::select(RMSE)
# LASSO LINE HORSE TAB ---- 
print(paste('Lasso RMSE Training set:' , round(lasso_cv_rmse_MIQ3[1, 1], 2), sep = ' ' ))

## 3. RANDOM FOREST ----
# from ch14 Data Analysis book, let's add additional interactions
# among amenities
X1  <- c("accommodates*bathrooms_f",  "room_type_f*d_sharebath",
         "room_type_f*d_am_petsallowed")
# with boroughs
X2  <- c("room_type_f*neighbourhood_f", "room_type_f*neighbourhood_f",
         "accommodates*neighbourhood_f" )

predictors_1rf <- c(basic_var)
predictors_2rf <- c(basic_var, host_review_var, amenities_var, neigh_var)
predictors_INTERACTrf <- c(basic_var, host_review_var, amenities_var, neigh_var, X1,X2)

# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)


# RF1
# set tuning parameters (nr predictors: 9 )
tune_grid <- expand.grid(
  .mtry = c(3,4,5), # m is about the sqrt of number of variables 
  .splitrule = "variance",
  .min.node.size = c(20, 50, 100 ) #
)

# model 1
set.seed(1234)

rf_modelMIQ3_1 <- train(
  formula(paste0("price_ln ~", paste0(predictors_1rf, collapse = " + "))),
  data = MIQ3_db,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tune_grid,
  importance = "impurity"
)


rf_modelMIQ3_1

# RF2
# set tuning pars (nr predictors 113)
tune_grid <- expand.grid(
  .mtry = c(8, 10, 12),
  .splitrule = "variance",
  .min.node.size = c(20, 50, 100)
)

set.seed(1234)

# first tried on a sub-sample of 5k observations

rf_modelMIQ3_2 <- train(
  formula(paste0("price_ln ~", paste0(predictors_2rf, collapse = " + "))),
  data = MIQ3_db,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tune_grid,
  importance = "impurity"
)


rf_modelMIQ3_2


# RF3 - interaction model
# set tuning pars (nr predictors 119)
tune_grid <- expand.grid(
  .mtry = c(8, 10, 12),
  .splitrule = "variance",
  .min.node.size = c(15, 20, 50, 100)
)

set.seed(1234)

# first tried on a sub-sample of 5k observations
rf_modelMIQ3_3 <- train(
  formula(paste0("price_ln ~", paste0(predictors_INTERACTrf, collapse = " + "))),
  data = MIQ3_db,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tune_grid,
  importance = "impurity"
)


rf_modelMIQ3_3

# evaluate random forests 

results_rf <- resamples(
  list(
    model_1  = rf_modelMIQ3_1,
    model_2  = rf_modelMIQ3_2,
    model_3 = rf_modelMIQ3_3
    
  )
)


summary(results_rf)


## 4. BOOSTING GBM basic ----

gbm_grid <-  expand.grid(interaction.depth = c(1, 5, 10), # complexity of the tree
                         n.trees = (4:10)*50, # number of iterations, i.e. trees
                         shrinkage = 0.1, # learning rate: how quickly the algorithm adapts
                         n.minobsinnode = 20 # the minimum number of training set samples in a node to commence splitting
)

set.seed(1234)
gbm_modelMIQ3 <- train(formula(paste0("price_ln ~", paste0(predictors_2rf, collapse = " + "))),
                   data = MIQ3_db,
                   method = "gbm",
                   trControl = train_control,
                   verbose = FALSE,
                   tuneGrid = gbm_grid)

gbm_modelMIQ3

## 5. BOOST GBM broad - additional model ----

# the next will be in final model, loads of tuning
gbm_grid2 <-  expand.grid(interaction.depth = c(1, 3, 5, 7, 9), # complexity of the tree
                          n.trees = (4:10)*50, # number of iterations, i.e. trees
                          shrinkage = c(0.05, 0.1, 0.15, 0.2), # learning rate: how quickly the algorithm adapts
                          n.minobsinnode = c(5,10,20) # the minimum number of training set samples in a node to commence splitting
)


set.seed(1234)

gbm_modelMIQ3_2 <- train(formula(paste0("price_ln ~", paste0(predictors_2rf, collapse = " + "))),
                    data = MIQ3_db,
                    method = "gbm",
                    trControl = train_control,
                    verbose = FALSE,
                    tuneGrid = gbm_grid2)

gbm_modelMIQ3_2

# HORSE RACE Table ----

final_models <-  list("Random forest (smaller model M3)" = rf_modelMIQ3_1,
                      "Random forest M4" = rf_modelMIQ3_2,
                       "Random forest M4 + interact" = rf_modelMIQ3_3,
                      "GBM (basic tuning) M4"  = gbm_modelMIQ3,
                      "GBM (broad tuning) M4" = gbm_modelMIQ3_2)
                          
results <- resamples(final_models) %>% summary()

# Model selection is carried out on this CV RMSE

horsetab <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")

horsetab_MIQ3_OLS <- eval$RMSE[eval$Model == 'OLS4_MIQ3']
horsetab_MIQ3_OLS <- as.data.frame(horsetab_train_OLS)
row.names(horsetab_MIQ3_OLS) <- 'OLS M4'
colnames(horsetab_MIQ3_OLS) <- "CV RMSE"

horsetab_MIQ3_LASSO <- round(lasso_cv_rmse_MIQ3[1, 1], 3)
horsetab_MIQ3_LASSO <- as.data.frame(horsetab_MIQ3_LASSO)
row.names(horsetab_MIQ3_LASSO) <- 'LASSO M4'
colnames(horsetab_MIQ3_LASSO) <-  "CV RMSE"

horsetab_addon <- rbind(horsetab_MIQ3_OLS, horsetab_MIQ3_LASSO)

horsetab <- rbind(horsetab_addon ,horsetab)

stargazer(horsetab, summary = F, type = 'text')

# Best Model : fitted versus actual values (y-yhat)

MIQ3_db$predicted_g1 <- predict(gbm_modelMIQ3, newdata = MIQ3_db)
MIQ3_db$predicted_g2 <- predict(gbm_modelMIQ3_2, newdata = MIQ3_db)

# Scatterplot of Actual vs Predicted Prices MIQ3
MIQ3_GBM_p <-  ggplot(MIQ3_db, aes(y = price_ln)) +
  geom_point(aes(x = predicted_g2, color = "GBM broad" ), alpha = 0.6) +  # First scatter points
  geom_point(aes(x = predicted_g1, color = "GBM basic"), alpha = 0.2) +  # Second scatter point
  geom_abline(slope = 1, intercept = 0, color = "darkred", linetype = "dashed") +  # Reference Line
  scale_color_manual(name = "Models", 
                     values = c("GBM broad" = "orange2", "GBM basic" = "purple3")) +
  labs(title = "Actual vs Predicted Prices MIQ3",
       x = "Predicted Prices (ln)",
       y = "Actual Prices (ln)") +
  theme_minimal()+
  theme( legend.text = element_text(family = "Garamond"),
         legend.title = element_text(family = "Garamond"),
         axis.text.x = element_text(family = "Garamond", face = "bold"),
         axis.title.x = element_text(family = "Garamond"),
         axis.text.y = element_text(family = "Garamond", face = "bold"),
         axis.title.y = element_text(family = "Garamond"),
         plot.title = element_text(family = "Garamond")) 

ggsave( 'GBM_baseMIQ3.png', plot = BGQ3_GBM_p, width = 6 , height = 6)

