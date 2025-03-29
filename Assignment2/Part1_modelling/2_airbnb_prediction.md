Building a prediction model on house prices - Prediction
================
Chiara Castelli

Scope of this script is to build 5 different predictive models from the
01_airbnb_datacleaning :

1.  OLS  
2.  LASSO  
3.  Random Forest  
4.  GBM Boosting basic tuning
5.  GBM Boosting broad tuning

The final section will compare these models and select the best one,
which will be validated in the upcoming scripts on two ‘live’ datasets:
MIQ32024 and BGQ32024.The corresponding codes are available in the
folder **Part2_Validity** .

# Read Database

## Training and test sets

Before moving on, we split the database into training set (70%) and test
set (30%).

## Prediction

### 1. OLS

We define different model to test as follows:

- M1: basic var
- M2: basic var + host & review feature
- M3: basic var + host & review feature + amenities dummies
- M4: basic var + host & review feature + amenities dummies +
  neighbourhood dummies

Reference categories: No Bedrooms - studio flat (Bedroom), Entire
property (Room type)

    ## 
    ## ========================================================================
    ##                                         Dep. var: ln price              
    ##                            ---------------------------------------------
    ##                                              price_ln                   
    ##                               (1)        (2)         (3)         (4)    
    ## ------------------------------------------------------------------------
    ## accommodates                 0.06       0.07        0.07        0.07    
    ##                            (0.01)*** (0.005)***  (0.005)***  (0.005)*** 
    ## bathrooms                    0.31       0.30        0.30        0.25    
    ##                            (0.02)***  (0.02)***   (0.02)***   (0.02)*** 
    ## bedrooms_f_1Broom            0.13       0.10        0.10        0.12    
    ##                            (0.02)***  (0.02)***   (0.02)***   (0.02)*** 
    ## bedrooms_f_2Broom            0.25       0.24        0.23        0.25    
    ##                            (0.03)***  (0.03)***   (0.03)***   (0.02)*** 
    ## bedrooms_f_3Broom            0.43       0.44        0.42        0.46    
    ##                            (0.05)***  (0.05)***   (0.05)***   (0.04)*** 
    ## bedrooms_f_4Broom            0.57       0.59        0.54        0.59    
    ##                            (0.10)***  (0.10)***   (0.10)***   (0.10)*** 
    ## `bedrooms_f_5+Broom`         0.39       0.36        0.31        0.43    
    ##                            (0.16)**   (0.14)**    (0.15)**    (0.15)*** 
    ## `room_type_f_Private room`   -0.33      -0.36       -0.31       -0.24   
    ##                            (0.02)***  (0.02)***   (0.02)***   (0.02)*** 
    ## `room_type_f_Shared room`    -0.62      -0.72       -0.62       -0.55   
    ##                            (0.10)***  (0.08)***   (0.09)***   (0.09)*** 
    ## d_miss_resp_time                        0.36        0.34        0.37    
    ##                                       (0.02)***   (0.02)***   (0.02)*** 
    ## host_resptime_n                         0.03        0.03        0.04    
    ##                                       (0.01)***   (0.01)***   (0.01)*** 
    ## d_miss_superhost                        -0.12       -0.15       -0.16   
    ##                                        (0.18)      (0.19)      (0.18)   
    ## host_is_superhost                       -0.01       0.002       0.01    
    ##                                        (0.01)      (0.01)      (0.01)   
    ## availability_30                         0.01        0.01        0.01    
    ##                                      (0.001)***  (0.001)***  (0.001)*** 
    ## number_of_reviews                      -0.001      -0.001      -0.001   
    ##                                      (0.0001)*** (0.0001)*** (0.0001)***
    ## minimum_nights                         -0.003      -0.003      -0.003   
    ##                                      (0.001)***  (0.001)***  (0.001)*** 
    ## maximum_nights                         0.0001      0.0001      0.0001   
    ##                                      (0.0000)*** (0.0000)*** (0.0000)***
    ## d_sharebath                                         -0.14       -0.12   
    ##                                                   (0.03)***   (0.02)*** 
    ## d_am_selfcheckin                                    0.01        0.001   
    ##                                                    (0.01)      (0.01)   
    ## d_am_locker                                         -0.05       -0.05   
    ##                                                   (0.01)***   (0.01)*** 
    ## d_am_housekeepingserv                               0.03        0.04    
    ##                                                   (0.01)**    (0.01)*** 
    ## d_am_fastwifi                                       0.01        0.03    
    ##                                                    (0.01)      (0.01)*  
    ## d_am_outdoor                                        -0.01       0.02    
    ##                                                    (0.01)      (0.01)*  
    ## d_am_petsallowed                                    -0.03       -0.01   
    ##                                                   (0.01)**     (0.01)   
    ## d_am_kitchen                                        -0.14       -0.08   
    ##                                                   (0.02)***   (0.02)*** 
    ## d_am_essentials                                     -0.08       -0.09   
    ##                                                   (0.01)***   (0.01)*** 
    ## Constant                     4.10       3.86        4.08        3.63    
    ##                            (0.03)***  (0.03)***   (0.04)***   (0.06)*** 
    ## ------------------------------------------------------------------------
    ## Neighbourhood                 No         No          No          Yes    
    ## Observations                14,876     14,876      14,876      14,876   
    ## R2                           0.21       0.30        0.31        0.40    
    ## ========================================================================
    ## Note:                                        *p<0.1; **p<0.05; ***p<0.01
    ##                                    Robust standard errors in parentheses

After running the linear regression model under different specification,
we can compare the RMSE obtain at each run on the training set:

    ## 
    ## ==================================================
    ##     Model    N predictors R-squared RMSE    BIC   
    ## --------------------------------------------------
    ## 1 OLS1_train      9         0.21    0.58 25,981.00
    ## 2 OLS2_train      17        0.30    0.54 24,291.00
    ## 3 OLS3_train      26        0.31    0.54 24,206.00
    ## 4 OLS4_train     113        0.40    0.50 22,765.00
    ## --------------------------------------------------

As shown in the above Table, Model 4 has lowest RMSE and BIC, despite
the significant number of predictors compared to the other ones.

We can check if the same conclusion is derived in the Test (or Holdout)
set, by looking at the following Table:

    ## 
    ## ===============================================================================
    ##   Model R-squared_train RMSE_train BIC_train R-squared_test RMSE_test BIC_test 
    ## -------------------------------------------------------------------------------
    ## 1 OLS1       0.21          0.58    25,981.00      0.19        0.58    11,349.00
    ## 2 OLS2       0.30          0.54    24,291.00      0.27        0.55    10,750.00
    ## 3 OLS3       0.31          0.54    24,206.00      0.28        0.55    10,769.00
    ## 4 OLS4       0.40          0.50    22,765.00      0.38        0.51    10,554.00
    ## -------------------------------------------------------------------------------

Same results as before, Model 4 provides the best statistics in terms of
BIC and RMSE. As a final step in the decision process, let’s perform a
Cross-Validation (CV) excercise on both training and test set.

#### Cross-validation

We first compute CV separately for the training and test set, then
compare RMSE across models and sets.

That is, we start from the training set:

    ## 
    ## ======================================
    ##   CV Train Model1 Model2 Model3 Model4
    ## --------------------------------------
    ## 1  Fold1    0.57   0.54   0.54   0.50 
    ## 2  Fold2    0.58   0.55   0.54   0.52 
    ## 3  Fold3    0.58   0.55   0.55   0.51 
    ## 4  Fold4    0.58   0.55   0.55   0.51 
    ## 5  Fold5    0.57   0.53   0.53   0.49 
    ## 6 Avg RMSE  0.58   0.55   0.55   0.51 
    ## --------------------------------------

And continue with the test set:

    ## 
    ## ======================================
    ##   CV Test  Model1 Model2 Model3 Model4
    ## --------------------------------------
    ## 1  Fold1    0.63   0.59   0.59   0.56 
    ## 2  Fold2    0.57   0.54   0.53   0.51 
    ## 3  Fold3    0.53   0.50   0.50   0.46 
    ## 4  Fold4    0.58   0.57   0.57   0.54 
    ## 5  Fold5    0.62   0.60   0.60   0.57 
    ## 6 Avg RMSE  0.58   0.55   0.55   0.52 
    ## --------------------------------------

*Training - Test comparison:*

    ## 
    ## ========================================
    ##     CV RMSE  Model1 Model2 Model3 Model4
    ## ----------------------------------------
    ## 6  Train set  0.58   0.55   0.55   0.51 
    ## 61 Test set   0.58   0.55   0.55   0.52 
    ## ----------------------------------------

In both cases, Model 4 is preferred. Hence, we can move on and evaluate
the prediction made with M4.

#### Prediction

*Prediction interval at 80% and 90% confidence level* (New observation)

    ## 
    ## =============================================
    ##             Model in logs Convertion to level
    ## ---------------------------------------------
    ## Predicted       4.55            108.00       
    ## PI_low 80%      3.90             55.90       
    ## PI_high 80%     5.21            207.00       
    ## PI_low 90%      3.71             46.50       
    ## PI_high 90%     5.39            249.00       
    ## ---------------------------------------------

Prediction ranges seem quite wide.

*Prediction versus actual (y-yhat))* (test set)

![](2_airbnb_prediction_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

The model seems to be under-predict very high prices (i.e. y axis \> 7)

### 2. LASSO

Moving to the Lasso specification, we can test our best model M4 under
OLS and see how many predictors remain significant.

    ## 
    ## ======================================
    ##              variable             s1  
    ## --------------------------------------
    ## 1          (Intercept)          4.770 
    ## 2          accommodates         0.126 
    ## 3           bathrooms           0.109 
    ## 4       bedrooms_f_3Broom       0.0004
    ## 5   `room_type_f_Private room`  -0.033
    ## 6        d_miss_resp_time       0.062 
    ## 7        availability_30        0.075 
    ## 8       number_of_reviews       -0.017
    ## 9          d_sharebath          -0.031
    ## 10         d_am_locker          -0.001
    ## 11 neighbourhood_cleansed_BRERA 0.043 
    ## 12 neighbourhood_cleansed_DUOMO 0.069 
    ## --------------------------------------

    ## [1] "Lasso RMSE Training set: 0.55"

RMSE LASSO M4: 0.56 versus OLS M4:0.51. NCOEFF LASSO M4: 11 versus OLS
M4: 111.

Lasso looses only a bit of goodness in fitting the model, while it
shrinks down the nr. of coefficients significantly. Interestingly,
*d_miss_resp_time* appears significant as in OLS M4. This may indicate
that there is no randomness in misreporting the response time, so we
will check if its significance persists in the other predictive models.

### 3. RANDOM FOREST

Random Forest creates many decision trees during its training phase,
where each tree is trained on a random sub sample (i.e. the bootstrap
technique). Importantly, instead of using all features, each tree
considers only a random subset of features at each split, which is
decided by the analyst. In this excercise, our set of parameters will
range from 3 to 12, while the node size will range between 20 to 100
observations.

As for the selection of the predictors, we start from the OLS M3 and M4
specification to be tested in a RF setting, while additionally including
some interactions terms not tested in the OLS, to check whether they
emerge as significant. Specifically, we focus on interaction *among
amenities*, as well as *between property features and location*.

After performing the different RF specifications on the *training set*,
we can check the models performance in the below table:

``` r
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
```

Where Model 2 (wich corresponds to OLS M4 set of predictors) is chosen
as best RF model. Within this specification, the lowest RMSE is found
when the nr of parameters is set to 12 and at the lowest node size of 20
(might be indicate the presence of overfitting).

We can test the goodness of the selected model on our test set by first
running the same RF model, and then looking at the resulting RMSE
distribution within different predictors categories, as shown in the
following Table:

#### Subsample performance on test set: RMSE-to-mean(y) ratio

Here, with the exception of the category ‘shared room’, the overall
stability in the RMSE-to-price ratios indicates a good model
performance.

We can now have a look at some graphics to help interpret our RF model.

#### Variable Importance Plots Train Set - Top 11

We can start by looking at the singular most important predictors
(i.e. ungrouped categorical variables) to check, for instance, the
difference of results with Lasso.

We can notice that these variables corresponds to the Lasso results,
with the addition of the information on minimum and maximum days of
stay. Furthermore, the two very central neighbourhoods of **Brera** and
**Duomo** appear as the most important locations, as together account
for ~8%.

We can further aggregate the categorical variables (i.e. neighbourhood,
room type and bedroom/ property size):

An important limitation of this plot lies in the fact that the group
importance is given by the direct sum of all individual importance
values of each category-dummy. In this way however, the raw sum could
underestimate the importance of the overall qualitative variable due to
the existing collinearity across dummies. Unfortunately, due to time
constraints, the suggested Python code is not translated into R for this
assignment so that we should treat these results with caution. Having
this acknowledgment in mind, we can still notice that the neighbourhood
information seems to play a very important role in Airbnb pricing.

Moreover, in addition to the visualization of the mostly relevant
predictors for our RF model, we still don’t know their magnitude. Are
the results of OLS and Lasso confirmed by the RF excerice?

To answer to this question, we can have a look at some Partial
Dependence Plots (PDP) plotted in the next section.

#### Partial Dependence Plots Train Set

Overall, we can say that both magnitude and functional forms are similar
to those included in the OLS.

## 4. BOOSTING

The last model family we try considers Boosting algorithms. Like in
Random Forests, the idea behing boosting is to still build multiple
decision trees, but with a key difference: instead of growing trees
independently, it builds them sequentially.

This is performed in order to improve the prediction fit at each new
tree, by giving more weight to observations that were poorly predicted
in the previous round. This iterative process helps improve overall
model accuracy and reduces bias, and it is usually preferred among other
prediction models together with Random Forest algorithms.

In this application, we start by including all predictors of our
preferred RF model, and run the specification in a Boosting model that
considers two versions of the Gradient boosting machine (GBM) algorithm,
where additional complexity is added in the search of the tuning
parameters in the second Boosting model.

### 4.1 GBM basic

### 4.2 GBM broad

## RF versus GBM most relevant features

We can compare the relevance of the predictors by looking at the
Importance Plot under RF and GBM broad boosting models. As shown in the
plot below, the set of top 11 predictors is identical, with minor
changes in the relative importances.

This shows a convergence in the identification of the most relevant
variable for our price analysis. We can conclude by having a look at the
horse race table in the following section.

## HORSES RACE: Models comparison and final discussion

The final decision on which model to choose is done by looking at the
comparison across RMSE computed under different model specifications.

In particular, the following table shows a comparison across model
performance considering the training set:

Where the lowest RMSE is given by the Boosting GBM model, with almost no
difference between basic a broad tuning models.

Hence, we can look at the corresponding table considering the test set:

After a second RMSE evaluation on the test set, we select the basic GBM
as our workhorse model. Despite considering 111 predictors, a subset of
11 key variables is identified including property characteristics
(i.e. guest capacity, property size, number of bathrooms, and Airbnb
availability in terms of minimum/maximum days and availability over the
next 30 days), the number of reviews, the type of room offered
(e.g. entire property vs. single private rooms), and their location,
with a premium effect observed for properties in Brera and Duomo.

To validate the selection of this model, we will apply the same model
selection routine to the other available datasets - i.e. MIQ3 and BGQ3 -
to determine whether both the GBM specification and the identified key
variables remain the most relevant predictors for Airbnb property price.
