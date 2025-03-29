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
    ## accommodates                 0.07       0.07        0.07        0.07    
    ##                            (0.01)*** (0.005)***  (0.005)***  (0.005)*** 
    ## bathrooms                    0.29       0.28        0.28        0.23    
    ##                            (0.02)***  (0.02)***   (0.02)***   (0.02)*** 
    ## bedrooms_f_1Broom            0.13       0.10        0.09        0.12    
    ##                            (0.02)***  (0.02)***   (0.02)***   (0.02)*** 
    ## bedrooms_f_2Broom            0.26       0.25        0.24        0.26    
    ##                            (0.03)***  (0.03)***   (0.03)***   (0.02)*** 
    ## bedrooms_f_3Broom            0.44       0.45        0.44        0.47    
    ##                            (0.05)***  (0.05)***   (0.05)***   (0.05)*** 
    ## bedrooms_f_4Broom            0.48       0.51        0.47        0.51    
    ##                            (0.09)***  (0.08)***   (0.08)***   (0.08)*** 
    ## `bedrooms_f_5+Broom`         0.62       0.55        0.48        0.57    
    ##                            (0.14)***  (0.13)***   (0.14)***   (0.14)*** 
    ## `room_type_f_Private room`   -0.31      -0.35       -0.32       -0.26   
    ##                            (0.02)***  (0.02)***   (0.02)***   (0.02)*** 
    ## `room_type_f_Shared room`    -0.54      -0.65       -0.56       -0.47   
    ##                            (0.10)***  (0.09)***   (0.09)***   (0.09)*** 
    ## d_miss_resp_time                        0.35        0.33        0.36    
    ##                                       (0.02)***   (0.02)***   (0.02)*** 
    ## host_resptime_n                         0.03        0.02        0.03    
    ##                                       (0.01)***   (0.01)***   (0.01)*** 
    ## d_miss_superhost                        -0.13       -0.17       -0.15   
    ##                                        (0.17)      (0.18)      (0.17)   
    ## host_is_superhost                       -0.01       0.002       0.01    
    ##                                        (0.01)      (0.01)      (0.01)   
    ## availability_30                         0.01        0.01        0.01    
    ##                                      (0.001)***  (0.001)***  (0.001)*** 
    ## number_of_reviews                      -0.001      -0.001      -0.001   
    ##                                      (0.0001)*** (0.0001)*** (0.0001)***
    ## minimum_nights                         -0.004      -0.004      -0.004   
    ##                                      (0.001)***  (0.001)***  (0.001)*** 
    ## maximum_nights                         0.0001      0.0001      0.0001   
    ##                                      (0.0000)*** (0.0000)*** (0.0000)***
    ## d_sharebath                                         -0.13       -0.11   
    ##                                                   (0.03)***   (0.02)*** 
    ## d_am_selfcheckin                                   0.0000       -0.01   
    ##                                                    (0.01)      (0.01)   
    ## d_am_locker                                         -0.03       -0.03   
    ##                                                   (0.01)**    (0.01)**  
    ## d_am_housekeepingserv                               0.03        0.03    
    ##                                                   (0.01)**    (0.01)**  
    ## d_am_fastwifi                                       0.001       0.02    
    ##                                                    (0.02)      (0.01)   
    ## d_am_outdoor                                        -0.01       0.01    
    ##                                                    (0.01)      (0.01)*  
    ## d_am_petsallowed                                    -0.02      -0.001   
    ##                                                    (0.01)*     (0.01)   
    ## d_am_kitchen                                        -0.15       -0.09   
    ##                                                   (0.02)***   (0.02)*** 
    ## d_am_essentials                                     -0.09       -0.09   
    ##                                                   (0.01)***   (0.01)*** 
    ## Constant                     4.11       3.90        4.14        3.63    
    ##                            (0.03)***  (0.03)***   (0.04)***   (0.06)*** 
    ## ------------------------------------------------------------------------
    ## Neighbourhood                 No         No          No          Yes    
    ## Observations                14,876     14,876      14,876      14,876   
    ## R2                           0.20       0.29        0.30        0.40    
    ## ========================================================================
    ## Note:                                        *p<0.1; **p<0.05; ***p<0.01
    ##                                    Robust standard errors in parentheses

After running the linear regression model under different specification,
we can compare the RMSE obtain at each run on the training set:

    ## 
    ## ==================================================
    ##     Model    N predictors R-squared RMSE    BIC   
    ## --------------------------------------------------
    ## 1 OLS1_train      9         0.20    0.58 26,054.00
    ## 2 OLS2_train      17        0.29    0.54 24,328.00
    ## 3 OLS3_train      26        0.30    0.54 24,243.00
    ## 4 OLS4_train     112        0.40    0.50 22,757.00
    ## --------------------------------------------------

As shown in the above Table, Model 4 has lowest RMSE and BIC, despite
the significant number of predictors compared to the other ones.

We can check if the same conclusion is derived in the Test (or Holdout)
set, by looking at the following Table:

    ## 
    ## ===============================================================================
    ##   Model R-squared_train RMSE_train BIC_train R-squared_test RMSE_test BIC_test 
    ## -------------------------------------------------------------------------------
    ## 1 OLS1       0.20          0.58    26,054.00      0.21        0.58    11,267.00
    ## 2 OLS2       0.29          0.54    24,328.00      0.29        0.55    10,664.00
    ## 3 OLS3       0.30          0.54    24,243.00      0.29        0.55    10,670.00
    ## 4 OLS4       0.40          0.50    22,757.00      0.39        0.51    10,476.00
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
    ## 1  Fold1    0.57   0.54   0.54   0.49 
    ## 2  Fold2    0.60   0.57   0.56   0.53 
    ## 3  Fold3    0.57   0.54   0.53   0.50 
    ## 4  Fold4    0.56   0.53   0.52   0.50 
    ## 5  Fold5    0.59   0.56   0.56   0.51 
    ## 6 Avg RMSE  0.58   0.54   0.54   0.50 
    ## --------------------------------------

And continue with the test set:

    ## 
    ## ======================================
    ##   CV Test  Model1 Model2 Model3 Model4
    ## --------------------------------------
    ## 1  Fold1    0.56   0.54   0.54   0.50 
    ## 2  Fold2    0.58   0.54   0.54   0.50 
    ## 3  Fold3    0.60   0.58   0.57   0.54 
    ## 4  Fold4    0.59   0.57   0.56   0.53 
    ## 5  Fold5    0.58   0.55   0.55   0.52 
    ## 6 Avg RMSE  0.58   0.56   0.55   0.52 
    ## --------------------------------------

*Training - Test comparison:*

    ## 
    ## ========================================
    ##     CV RMSE  Model1 Model2 Model3 Model4
    ## ----------------------------------------
    ## 6  Train set  0.58   0.54   0.54   0.50 
    ## 61 Test set   0.58   0.56   0.55   0.52 
    ## ----------------------------------------

In both cases, Model 4 is preferred. Hence, we can move on and evaluate
the prediction made with M4.

#### Prediction

*Prediction interval at 80% and 90% confidence level* (New observation)

    ## 
    ## =============================================
    ##             Model in logs Convertion to level
    ## ---------------------------------------------
    ## Predicted       4.59            112.00       
    ## PI_low 80%      3.94             58.10       
    ## PI_high 80%     5.24            215.00       
    ## PI_low 90%      3.75             48.20       
    ## PI_high 90%     5.43            259.00       
    ## ---------------------------------------------

Prediction ranges seem quite wide.

*Prediction versus actual (y-yhat))* (test set)

![](https://raw.githubusercontent.com/ccastelli1994/DataAnalysis3/main/Assignment2/Part1_modelling/Plots/unnamed-chunk-13-1.png)

The model seems to be under-predict very high prices (i.e. y axis \> 7)

### 2. LASSO

Moving to the Lasso specification, we can test our best model M4 under
OLS and see how many predictors remain significant.

    ## 
    ## ========================================
    ##              variable              s1   
    ## ----------------------------------------
    ## 1          (Intercept)           4.770  
    ## 2          accommodates          0.131  
    ## 3           bathrooms            0.101  
    ## 4       bedrooms_f_1Broom       -0.00005
    ## 5   `room_type_f_Private room`   -0.036 
    ## 6        d_miss_resp_time        0.060  
    ## 7        availability_30         0.076  
    ## 8       number_of_reviews        -0.017 
    ## 9          d_sharebath           -0.026 
    ## 10 neighbourhood_cleansed_BRERA  0.045  
    ## 11 neighbourhood_cleansed_DUOMO  0.065  
    ## ----------------------------------------

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

===========================================
      Var.1     RMSE  Mean.price RMSE.price
-------------------------------------------
1   Nr people                              
2   large apt   0.509   4.970      0.102   
3   small apt   0.432   4.610      0.094   
4   Room Type                              
5  Private room 0.455   4.370      0.104   
6  Shared room  0.706   4.010      0.176   
7    Borough                               
8     BRERA     0.504   5.210      0.097   
9     DUOMO     0.427   5.210      0.082   
10     All      0.468   4.770      0.098   
-------------------------------------------

Here, with the exception of the category ‘shared room’, the overall
stability in the RMSE-to-price ratios indicates a good model
performance.

We can now have a look at some graphics to help interpret our RF model.

#### Variable Importance Plots Train Set - Top 11

We can start by looking at the singular most important predictors
(i.e. ungrouped categorical variables) to check, for instance, the
difference of results with Lasso.

![](https://raw.githubusercontent.com/ccastelli1994/DataAnalysis3/main/Assignment2/Part1_modelling/Plots/rf_model_2_var_imp_plot_b.png)

We can notice that these variables corresponds to the Lasso results,
with the addition of the information on minimum and maximum days of
stay. Furthermore, the two very central neighbourhoods of **Brera** and
**Duomo** appear as the most important locations, as together account
for ~8%.

We can further aggregate the categorical variables (i.e. neighbourhood,
room type and bedroom/ property size):

![](https://raw.githubusercontent.com/ccastelli1994/DataAnalysis3/main/Assignment2/Part1_modelling/Plots/rf_model_2_var_imp_grouped_plot.png)

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

![](https://raw.githubusercontent.com/ccastelli1994/DataAnalysis3/main/Assignment2/Part1_modelling/Plots/pdp_numeric.png)

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


![](https://raw.githubusercontent.com/ccastelli1994/DataAnalysis3/main/Assignment2/Part1_modelling/Plots/rf_gbm_compare.png)


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

|-------------------------------------------|--------------|---|---|---|
|                                           | CV RMSE      |   |   |   |
|     OLS   M4                              |     0.511    |   |   |   |
|     LASSO   M4                            |     0.554    |   |   |   |
|     Random   forest (smaller model M3)    |     0.584    |   |   |   |
|     Random   forest M4                    |     0.496    |   |   |   |
|     Random   forest M4 + interact         |     0.512    |   |   |   |
|     GBM   (basic tuning)                  |     0.484    |   |   |   |
|     GBM   (broad tuning)                  |     0.483    |   |   |   |
|-------------------------------------------|--------------|---|---|---|

Where the lowest RMSE is given by the Boosting GBM model, with almost no
difference between basic a broad tuning models.

Hence, we can look at the corresponding table considering the test set:

=============================================
                                 Holdout RMSE
---------------------------------------------
OLS M4                              0.490    
Random forest (smaller model M3)    0.558    
Random forest M4                    0.469    
Random forest M4 + interact         0.488    
GBM (basic tuning)                  0.434    
GBM (broad tuning)                  0.442    
---------------------------------------------

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
