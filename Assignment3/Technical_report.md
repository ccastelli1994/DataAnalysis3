Fast growing firms
================
Chiara Castelli

# Data cleaning and labelling

We start with a first overview of the database and a quick exclusion of
selected variables with high share of missing values. Additionally, we
shrink the sample to the period 2010-2015, in accordance to our scope of
analysis, and create a complete (unbalanced) panel of selected firms.

``` r
summary(data)

min(data$begin)
max(data$end)
# panel from 2005 to 2016
table(data$year)
# shrink sample to the 2010-15 period 
data <- data[(data$year >= 2010 & data$year <= 2015),] 

# check if financials are annual
which(substr(data$begin,1,4)!= substr(data$end,1,4))
data[which(substr(data$begin,1,4)!= substr(data$end,1,4)),] # fore these companies, fin year starts in Oct

# drop variables with many NAs
data <- data %>%  select(-c(COGS, finished_prod, net_dom_sales, net_exp_sales, wages)) 
# drop companies with an exit date
data <- data[is.na(data$exit_year),] 

# create complete panel adding comp_id-fin_year when missing
data <- data %>% complete(year, comp_id)
```

## Growth measure

We start our data routine from our target variable, namely firm growth.
As standard in the economics literature (World Bank, 2015), we consider
as main growth measure the difference in sales among two years. In
particular, we start by including both 1-year change as well as 2-years
change according to data availability, as shown in the code below:

``` r
# 2 measures of fast growth: 1-year and 2-year change
# we use sales as measure of growth - standard in the literature

# Look at distribution of obs first
table(data$year[!is.na(data$sales)]) 
# 2015, 2014 and 2013 top 3 coverage. Hence we define 2 growth measures:
# 1-y consecutive change: 2014 -> 2015 
# 2-y consecutive change: 2013 -> 2015
data <- data[(data$year >= 2013),] 

# From Gabor's Chpt.17 case study: growth measures
summary(data$sales)
# keep positive sales only
data <- data[data$sales >= 0,]

data <- data %>%
  mutate(ln_sales = ifelse(sales > 0, log(sales), 0))

data <- data %>%
  group_by(comp_id) %>%
  mutate(d1_sales_log = ln_sales - Lag(ln_sales, 1),
         d2_sales_log = ln_sales - Lag(ln_sales, 2) ) %>%
  ungroup()

data <- data[!is.na(data$comp_id),]

# replace growth = 0 for new firms 
data <- data %>%
  mutate(age = (year - founded_year) %>% ifelse(. < 0, 0, .),
         d1_sales_log = ifelse(age <= 1, 0, d1_sales_log),
         d2_sales_log = ifelse(age <= 2, 0, d2_sales_log))

data <- data %>%
  mutate(d1_sales_log = ifelse(!is.finite(d1_sales_log), NA, d1_sales_log),
         d2_sales_log = ifelse(!is.finite(d2_sales_log), NA, d2_sales_log))

# check for missing values in the final vars
data %>% select( sales, ln_sales, d1_sales_log, d2_sales_log) %>%  summarise(across(everything(), ~ sum(is.na(.)), .names = "na_count_{.col}"))
# and drop any missing obs 
data <- data %>% filter(if_all(all_of(c("d2_sales_log", "d1_sales_log")), ~ !is.na(.)))
```

## Firms characteristics

We move to the cleaning and labeling of other relevant firm features for
the analysis, following Gabor’s code on Chapt 17. In particular, we will
start from the recoding certain industry codes, and then move to the
computation of financial indicators.

- Industry recoding

``` r
sort(unique(data$ind2))

# change some industry category codes
data <- data %>%
  mutate(ind2_cat = ind2 %>%
           ifelse(. > 56, 60, .)  %>%
           ifelse(. < 26, 20, .) %>%
           ifelse(. < 55 & . > 35, 40, .) %>%
           ifelse(. == 31, 30, .) %>%
           ifelse(is.na(.), 99, .)
           )


table(data$ind2_cat)
# and create a industry factor variable

data <- data %>%
  mutate(ind2_cat = factor(ind2_cat, levels = sort(unique(data$ind2_cat))))
```

### Financial/Economic indicators

#### Business ratios

Based on a review of the literature in economics and business studies,
we include the following pieces of information from the Balance sheet
and Profit/Loss statements that are identified as growth drivers:

*Balance sheet info*, in particular:

- tangible and intangible assets, as well as inventories, can reflect
  both firm growth potential and production capacity (Hall, 1993) var
  names: **intang_assets, tang_assets, fixed_assets, inventories**
- liquidity and leverage indicators that are important for understanding
  how firm financial constraints (Berry 2013) var names: **liq_assets,
  curr_assets, curr_liab, noncurr_liab (estimate)**
- firm capitalisation, in terms of equity and shared capital, to
  consider the internal financial capabilities (Rajan & Zingales, 1998)
  var names: **share_eq, subscribed_cap**

*P/L statement*, in particular:

- non-operational and operational profitability, as firms with higher
  profits are more likely to reinvest and expand, thus grow (Penrose,
  1959). var names: **extra_exp, extra_inc, extra_profit_loss,
  inc_bef_tax, profit_loss_year**

- variables related to the cost structure, such as in Labor and material
  inputs, which reflect the investment in resources that can contribute
  to grow (Hall, 1987). var names: **material_exp, personnel_exp**

Furthermore, we devide all the above variables for firm size (expressed
as **total assets**, estimate), in order to gain a more realistic size
of the relative impact of these variables on each individual growth.

``` r
# Financial variables, create ratios

# assets can't be negative. Change them to 0 and add a flag.
data <-data  %>%
  mutate(flag_asset_problem=ifelse(intang_assets<0 | curr_assets<0 | fixed_assets<0,1,0  ))
table(data$flag_asset_problem)

data <- data %>%
  mutate(intang_assets = ifelse(intang_assets < 0, 0, intang_assets),
         curr_assets = ifelse(curr_assets < 0, 0, curr_assets),
         fixed_assets = ifelse(fixed_assets < 0, 0, fixed_assets))

# generate total assets
data <- data %>%
  mutate(total_assets_bs = intang_assets + curr_assets + fixed_assets)
summary(data$total_assets_bs)

# impute non-curr liab
data <- data %>%
  mutate(noncurr_liab = total_assets_bs - curr_liab - share_eq) 

# define Balance sheet variables
bs_names <- c("intang_assets", "curr_liab", "noncurr_liab", "fixed_assets", "liq_assets", "curr_assets", "share_eq", "subscribed_cap", "tang_assets")
# define P/L variables
pl_names <- c("extra_exp","extra_inc", "extra_profit_loss", "inc_bef_tax" ,"inventories",
              "material_exp", "profit_loss_year", "personnel_exp")

# divide all pl_names & bl_ names by sales and create new column for it
data <- data %>%
  mutate_at(vars(pl_names), funs("pl"=./sales))
data <- data %>%
  mutate_at(vars(bs_names), funs("bs"=ifelse(total_assets_bs == 0, 0, ./total_assets_bs)))

data <- data %>%
  mutate(across(matches("(bs|pl)$"), ~ replace(., !is.finite(.), NA)))

# create dummy if company registered loss
data$d_profit <- ifelse(data$profit_loss_year > 0, 1,0)
```

As a final step, we flag anomalies & impute missing observations

#### Other characteristics

*Demographics and location* : following the extensive literature on firm
survival and growth, we consider age in its quadratic form (Brudlerl &
Schussler, 1990; Mahmood, 2000), as well as certain location factor that
affect firms’ access to resources, markets, and talent (Gennaioli et
al., 2013). var names **age, urban_m, region_m**

*Managerial characteristics*: in particular, CEOs demographics are
considered (e.g. gender and age) to include the potential differences in
the leadrship style and their impact on firm performace (Bertrand &
Schoar, 2003; Adams & Ferreira, 2009; Yim, 2013) var names:
**foreign_management, gender, ceo_count, ceo_age/ ceo_young**

``` r
basic <- c('ind2_cat', 'age',  'intang_assets', 'tang_assets' , "curr_liab", "noncurr_liab")
# Impute missing 
# basic
data %>% select(basic) %>% describe()
# impute age, drop rest
data <- data %>%  
  mutate(
  flag_miss_age = ifelse(is.na(age),1,0),
  age = ifelse(is.na(age),mean(age, na.rm = T), age),
  age2 = age^2)

ceo_extrainfo <- c('foreign_management','gender', 'ceo_age', 'urban_m','region_m')

# Create foreign_management dummy
data <- data %>%
  mutate(foreign_management = ifelse(foreign >= 0.5, 1,0))
# create factors
data <- data %>%
  mutate(urban_m = factor(urban_m, levels = c(1,2,3)),
         ind2_cat = factor(ind2_cat, levels = sort(unique(data$ind2_cat))))

# impute variables 
# CEO age
data <- data %>%
  mutate(ceo_age = year-birth_year,
         flag_low_ceo_age = as.numeric(ceo_age < 25 & !is.na(ceo_age)),
         flag_high_ceo_age = as.numeric(ceo_age > 75 & !is.na(ceo_age)),
         flag_miss_ceo_age = as.numeric(is.na(ceo_age)))

data <- data %>%
  mutate(ceo_age = ifelse(ceo_age < 25, 25, ceo_age) %>%
           ifelse(. > 75, 75, .) %>%
           ifelse(is.na(.), mean(., na.rm = TRUE), .),
         ceo_young = as.numeric(ceo_age < 40))

data %>% select(ceo_extrainfo) %>% describe()
data <- data %>%  
  mutate(
  flag_miss_gender = ifelse(is.na(personnel_exp),1,0),
  gender = ifelse(is.na(gender), get_mode(gender), gender),
  flag_miss_foreign_management = ifelse(is.na(foreign_management),1,0),
  foreign_management = ifelse(is.na(foreign_management),mean(foreign_management, na.rm = T), foreign_management),
  flag_miss_region_m = ifelse(is.na(region_m),1,0),
  region_m = ifelse(is.na(region_m),get_mode(region_m), region_m))

# recode last couple of vars and create into factors
data <- data %>%
  mutate(age2 = age^2,
         gender_m = factor(gender, levels = c("female", "male", "mix")),
         m_region_loc = factor(region_m, levels = c("Central", "East", "West")))


# drop obs having NAs in any of these 3 main var groups
data <- data %>% filter(if_all(all_of(c(basic, ceo_extrainfo, pl_names, bs_names)), ~ !is.na(.)))

# finally, keep companies at year 2015 (so that d1 and d2 are changes wrt previous years)
data <- data[data$year == 2015,]
```

We are left with ~25k firms (some of them are new, so growth rate is set
to 0)

# Descriptive visulisation

## Growth rate

We first compare our two different measures of growth, namely 1-year and
2-years rates of change:

![](Technical_report_files/figure-gfm/unnamed-chunk-8-1.png)<!-- --> The
two plots look very similar, so we zoom in into the two distributions
using boxplots:

![](Technical_report_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> As
we are interested in fast growing firms, the 2-y change seems to have
more variability towards the top of the distribution. Hence, we decide
to keep this measure as the preferred one.

As the next step, we create two variables of *fast growth* by
highlighting firms at the *top 3%* and *top 5%* of the growth
distribution.

In addition, as the outcoming shares of top-to-total firms are very low
with respect to the overall sample size, we decide to *further shrink*
our sample and *exclude* firms below 2 years old (as we consider 2-ys
changes rates) and *keep* only positive growth rates, as our goal is to
determine those who are growing more than the others.

This brings us to a *final sample of 11k companies*, of which ~7% are in
the Top3%, and ~11% are in the Top5% of growth distribution. Due to the
low rate of Top 3% firms, we decide to base the analysis solely on the
Top 5% category.

## Numeric variables

### Age

![](Technical_report_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
Here the linear fit seems already adequate to describe the growth-age
relationship however, in order to remain consistent with the literature
the quadratic form is kept in the analysis

### Lowess on Dummy Profits

![](Technical_report_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

It seems to have an overall convex shape. We can further zoom in to the
top distribution (i.e. Top5% category)

![](Technical_report_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Overall positive and linear, meaning that registering profits is
correlated with fast growth.

### Correlation plot

#### Balance sheet information

![](Technical_report_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
Overall, no big influence (age, tang assets, shared equite and liquidity
most relevant)

#### P/L variables

![](Technical_report_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->
Again no bg influence (personnel and material expenses, income before
tax and profit/loss of the year - interestingly all at negative values)

#### CEO and management info

![](Technical_report_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## Factors

### Location

![](Technical_report_files/figure-gfm/unnamed-chunk-18-1.png)<!-- --> No
significant variability

- CEO demographics

![](Technical_report_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

Young CEOs emerge as potential canditats for fast growing firms.

# Prediction Top 5%

## Logit and Lasso

Starting from the Logit regression, we create a set of models to test by
including additional variables at every round:

- M1: firm demographics, location and industry at 2-digits
- M2: M1 + management info
- M3: M3 + balance sheet info
- M4: M3 + P/L info
- M5: M5 + errors and missing flags

The results of the various Logit specifications are available at the below Table, where Model 5 returns lowest AIC value.
As this result consider the overall sample, we can now test its validity by using Cross-Validation (CV) on Train and Holdout sets.
<!-- -->

   
    ## =========================================================================================================================================================================================================================================================================================================
    ##                                                                                                                                               Dep. var: Probability of being a Top5%                                                                                                                     
    ##                          --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
       ##                                                      (1)                                                      (2)                                                  (3)                                                  (4)                                                  (5)                         
    ## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ## age                                                -0.19***                                                 -0.18***                                             -0.17***                                             -0.17***                                             -0.17***                      
    ##                                                     (0.02)                                                   (0.02)                                               (0.02)                                               (0.02)                                               (0.02)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## age2                                               0.005***                                                 0.004***                                             0.004***                                             0.004***                                             0.004***                      
    ##                                                    (0.001)                                                  (0.001)                                              (0.001)                                              (0.001)                                              (0.001)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## region_mEast                                       -0.0002                                                   -0.02                                                -0.01                                                -0.01                                                 0.01                        
    ##                                                     (0.08)                                                   (0.08)                                               (0.08)                                               (0.08)                                               (0.09)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## region_mWest                                        -0.15                                                    -0.15                                                -0.12                                                -0.10                                                -0.10                        
    ##                                                     (0.10)                                                   (0.10)                                               (0.10)                                               (0.10)                                               (0.10)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## urban_m2                                             0.04                                                     0.03                                                 0.05                                                 0.05                                                 0.06                        
    ##                                                     (0.09)                                                   (0.09)                                               (0.09)                                               (0.09)                                               (0.09)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## urban_m3                                             0.01                                                     0.02                                                 0.05                                                 0.05                                                 0.06                        
    ##                                                     (0.08)                                                   (0.08)                                               (0.08)                                               (0.09)                                               (0.09)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## foreign_management                                                                                          0.35***                                              0.38***                                              0.37***                                              0.39***                       
    ##                                                                                                              (0.09)                                               (0.09)                                               (0.09)                                               (0.09)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## ceo_count                                                                                                   -0.34***                                             -0.33***                                             -0.33***                                             -0.31***                      
    ##                                                                                                              (0.09)                                               (0.09)                                               (0.09)                                               (0.09)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## ceo_young                                                                                                   0.30***                                              0.29***                                              0.30***                                              0.30***                       
    ##                                                                                                              (0.07)                                               (0.07)                                               (0.07)                                               (0.07)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## gender_mmale                                                                                                 -0.02                                                -0.01                                                0.003                                                 0.02                        
    ##                                                                                                              (0.08)                                               (0.08)                                               (0.08)                                               (0.08)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## gender_mmix                                                                                                  -0.18                                                -0.17                                                -0.18                                                -0.17                        
    ##                                                                                                              (0.16)                                               (0.16)                                               (0.16)                                               (0.16)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## liq_assets_bs                                                                                                                                                    0.39***                                              0.46***                                              0.51***                       
    ##                                                                                                                                                                   (0.12)                                               (0.12)                                               (0.12)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## tang_assets_bs                                                                                                                                                   -0.38***                                             -0.38***                                             -0.32***                      
    ##                                                                                                                                                                   (0.12)                                               (0.12)                                               (0.12)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## intang_assets_bs                                                                                                                                                  -0.50                                                -0.50                                                -0.29                        
    ##                                                                                                                                                                   (0.90)                                               (0.91)                                               (0.90)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## share_eq_bs                                                                                                                                                       0.0001                                               0.0002                                               0.0002                       
    ##                                                                                                                                                                  (0.0002)                                             (0.0003)                                             (0.0003)                      
    ##                                                                                                                                                                                                                                                                                                          
    ## noncurr_liab_bs                                                                                                                                                  -0.0003                                               -0.001                                               -0.001                       
    ##                                                                                                                                                                  (0.002)                                              (0.002)                                              (0.002)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## personnel_exp_pl                                                                                                                                                                                                       0.16**                                               0.14*                        
    ##                                                                                                                                                                                                                        (0.07)                                               (0.07)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## material_exp_pl                                                                                                                                                                                                       0.20***                                              0.19***                       
    ##                                                                                                                                                                                                                        (0.05)                                               (0.04)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## profit_loss_year_pl                                                                                                                                                                                                   -0.06***                                             -0.05***                      
    ##                                                                                                                                                                                                                        (0.02)                                               (0.02)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## liq_assets_flag_error                                                                                                                                                                                                                                                        1.33                        
    ##                                                                                                                                                                                                                                                                             (1.36)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## tang_assets_flag_error                                                                                                                                                                                                                                                      -11.63                       
    ##                                                                                                                                                                                                                                                                            (535.41)                      
    ##                                                                                                                                                                                                                                                                                                          
    ## intang_assets_flag_error                                                                                                                                                                                                                                                                                 
    ##                                                                                                                                                                                                                                                                                                          
    ##                                                                                                                                                                                                                                                                                                          
    ## share_eq_flag_error                                                                                                                                                                                                                                                        0.30***                       
    ##                                                                                                                                                                                                                                                                             (0.07)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## noncurr_liab_flag_error                                                                                                                                                                                                                                                     0.18**                       
    ##                                                                                                                                                                                                                                                                             (0.07)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## flag_miss_age                                                                                                                                                                                                                                                                                            
    ##                                                                                                                                                                                                                                                                                                          
    ##                                                                                                                                                                                                                                                                                                          
    ## flag_asset_problem                                                                                                                                                                                                                                                          -11.99                       
    ##                                                                                                                                                                                                                                                                            (358.43)                      
    ##                                                                                                                                                                                                                                                                                                          
    ## flag_miss_region_m                                                                                                                                                                                                                                                           0.28                        
    ##                                                                                                                                                                                                                                                                             (0.55)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## personnel_exp_flag_error                                                                                                                                                                                                                                                    -10.47                       
    ##                                                                                                                                                                                                                                                                            (378.37)                      
    ##                                                                                                                                                                                                                                                                                                          
    ## material_exp_flag_error                                                                                                                                                                                                                                                                                  
    ##                                                                                                                                                                                                                                                                                                          
    ##                                                                                                                                                                                                                                                                                                          
    ## flag_miss_personnel_exp                                                                                                                                                                                                                                                                                  
    ##                                                                                                                                                                                                                                                                                                          
    ##                                                                                                                                                                                                                                                                                                          
    ## Constant                                            -0.67                                                    -0.48                                                -0.52                                                -0.80                                                -1.07                        
    ##                                                     (0.63)                                                   (0.65)                                               (0.65)                                               (0.65)                                               (0.66)                       
    ##                                                                                                                                                                                                                                                                                                          
    ## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ## Industry 2digit                                      Yes                                                      Yes                                                  Yes                                                  Yes                                                  Yes                         
    ## Observations                                        11,045                                                   11,045                                               11,045                                               11,045                                               11,045                       
    ## Akaike Inf. Crit.                                  7,496.18                                                 7,444.48                                             7,413.25                                             7,312.69                                             7,301.75                      
    ## =========================================================================================================================================================================================================================================================================================================
    ## Note:                                                                                                                                                                                                                                                                         *p<0.1; **p<0.05; ***p<0.01



### Train versus Holdout

#### CV 5-folds

``` r
# separate datasets 

set.seed(13505)

train_indices <- as.integer(createDataPartition(data$d2_top5, p = 0.7, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

# 5 fold cross-validation
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)
```

We first estimate the best model using the Train set, also including
Lasso as an alternative:

``` r
# Train Logit & Lasso  

# Logit
logit_model_vars <- list("var1" = demo_location, "var2" = var_2, "var3" = var_3, "var4" = var_4, "var5" = var_5)

CV_RMSE_folds <- list()
logit_models <- list()

for (model_name in names(logit_model_vars)) {

  features <- logit_model_vars[[model_name]]

  set.seed(13505)
  glm_model <- train(
    formula(paste0("d2_top5 ~", paste0(features, collapse = " + "))),
    method = "glm",
    data = data_train,
    family = binomial,
    trControl = train_control
  )

  logit_models[[model_name]] <- glm_model
  # Calculate RMSE on test for each fold
  CV_RMSE_folds[[model_name]] <- glm_model$resample[,c("Resample", "RMSE")]

}

# augment magnitude of variables before performing lasso
#data_train_lasso <- data_train 

#data_train_lasso[bl_info] <- data_train_lasso[bl_info]*10^3
#data_train_lasso[pl_info] <- data_train_lasso[pl_info]*10^3

# Lasso 

lambda <- 10^seq(-2, -4, length = 10)
grid <- expand.grid("alpha" = 1, lambda = lambda)

set.seed(13505)

  logit_lasso_model <- train(
    formula(paste0("d2_top5 ~", paste0(var_5, collapse = " + "))),
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    family = "binomial",
    trControl = train_control,
    tuneGrid = grid,
    na.action=na.exclude
  )

tuned_logit_lasso_model <- logit_lasso_model$finalModel
best_lambda <- logit_lasso_model$bestTune$lambda
logit_models[["LASSO"]] <- logit_lasso_model
lasso_coeffs <- as.matrix(coef(tuned_logit_lasso_model, best_lambda))

CV_RMSE_folds[["LASSO"]] <- logit_lasso_model$resample[,c("Resample", "RMSE")]
```

As next step, we calculate the RMSE and the ROC curve, which will guide
us in the decision of the best model among the 5 Logit and Lasso, to
then apply it to the Holdout set. In order to do so, we consider first a
case where no loss function is included, namely there is no penalty
towards false negative and positive predictions, then we will introduce
it.

#### Model selection

##### 1. No loss function

We first calculate RMSE and ROC-Curve Area (AUC) for each CV-fold Train,
considering all 5 logit models and LASSO.

Thus, we pick our preferred model based on the lowest RMSE and greatest
AUC values:

    ##       Number.of.predictors CV.RMSE CV.AUC
    ## var1                    18   0.308  0.654
    ## var2                    23   0.307  0.671
    ## var3                    28   0.308  0.675
    ## var4                    31   0.305  0.689
    ## var5                    42   0.305  0.692
    ## LASSO                   10   0.308  0.654

Logit M5 shows the best diagnostics, although Logit M4 closely follows
behind. We choose Logit M5 as the best candidate to try on the *Holdout
set*.

    ## [1] "RMSE on holdout set M5: 0.319"

    ## [1] "AUC on holdout set M5: 0.653"

Both RMSE and AUC values are sufficiently close to those of the Train
set. We can visualise graphically the corresponding ROC-Curve:

![](Technical_report_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

###### Classification Table

In absence of a Loss function, we translate the predicted probabilities
into binary classes “Top 5%”/“Not Top 5%” by deciding a classification
threshold. Then, we compute the Classification Tables, or confusion
matrices.

For the sake of comparison, we can use the default threshold of 0.5 as a
first reference:

    ##             Reference
    ## Prediction   Not top 5% Top 5%
    ##   Not top 5%       2904    388
    ##   Top 5%              7     14

As a second comparison, we can take the sample mean predicted
probability as threshold:

    ## [1] " the mean predicted prob. is: 0.11"

    ##             Reference
    ## Prediction   Not top 5% Top 5%
    ##   Not top 5%       1914    164
    ##   Top 5%            997    238

The mean predicted probability (0.11) is remarkably lower than the
default (0.5). Thus, by lowering the classification threshold, we are
pushing for more predictions of 1 values (i.e. top 5% class) than
before, as shown in the second row of the table above.

We can also compare the related classification statistics in the table
below:

| Model | Logit Default | Logit No Loss | 
|-------|---------------|---------------|
|Threshold| 0.50        | 0.11          |
|Accuracy | 0.88        | 0.65          |
|Sensitivity | 0.04     | 0.59          | 
|Specificity | 0.99     | 0.66          | 

where

- Accuracy =(TP+TN)/N (i.e., the proportion of correct predictions)
- Sensitivity = TP / (TP+FN) (i.e. the proportion of correct prediction
  for the positive class)
- Specificity = TN/(TN+FP)(i.e. the proportion of correct prediction for
  the negative class)

As noted earlier, lowering the classification threshold from 0.5 to 0.11
increases the predictions of the positive class, and thus the
sensitivity statistic. However, this automatically reduces the negative
class (specificity) and shows the existence of a trade-off between the
two measures.

##### 2. With loss function

We now repeat the evaluation procedure by introducing a loss function.
That is, we explicitly define the cost of a false negative
classification compared to a false positive. Given that the scope of the
analysis is to identify high growth companies, we decide to give more
weight to the potential mis-prediction of the top 5% in the not top 5%
class (i.e. the negative class) by assigning a ratio of 10:1 compared to
a false positive. This means that for every Top 5% missed, we lose 10
times the value of a Not Top 5% miss.

``` r
FP=1
FN=10
cost = FN/FP

# proportion of cases in the population (n.cases/(n.controls+n.cases))
prevelance = sum(data_train$d2_top5)/length(data_train$d2_top5)
```

Once defined the loss function, we can check whether this helps in
improving our predictions by searching the threshold that minimises th
E\[Loss\].

    ##       Avg.of.optimal.thresholds Threshold.for.Fold5 Avg.expected.loss
    ## var1                 0.09304137          0.06956362         0.7325461
    ## var2                 0.09165050          0.09816125         0.6932250
    ## var3                 0.08673419          0.09046760         0.6866319
    ## var4                 0.09411350          0.09363950         0.6796514
    ## var5                 0.08528789          0.08966601         0.6597316
    ## LASSO                0.08890120          0.09252364         0.7046318
    ##       Expected.loss.for.Fold5
    ## var1                0.7425614
    ## var2                0.6798189
    ## var3                0.6869340
    ## var4                0.6752911
    ## var5                0.6235446
    ## LASSO               0.6804010

With the introduction of a Loss function, we need to adjust the initial
threshold computed with Lasso, as it relies on the Logit scala and not
on the Probability scale. To do this,we convert the computed initial
value through the following formula:

$$\frac{1}{1+ exp{-init.threshold}}$$

The results are shown in the Table above, where to the average threshold
and expected loss computed over the 5CV folds, we show the value of the
last 5th fold for comparison. Again, Logit M5 returns the lowest
Expected Losses with an average threshold of 0.085, which is chosen as
best candidate to try on the Holdout set.

###### Classification Table

    ## [1] "Expected loss on Holdout set : 0.764"

    ##             Reference
    ## Prediction   Not top 5% Top 5%
    ##   Not top 5%       1520    114
    ##   Top 5%           1391    288

After a CV exercise on the smaller sample, the average expected loss
gets worse and now reaches 0.76.

Again, we can compare the classification statistics wrt to the previous
cases, as shown in the table below:

| Model | Logit Default | Logit No Loss | Logit Loss |
|-------|---------------|---------------|------------|
|Threshold| 0.50        | 0.11          | 0.085      |
|Accuracy | 0.88        | 0.65          | 0.54       | 
|Sensitivity | 0.04     | 0.59          | 0.71       | 
|Specificity | 0.99     | 0.66          | 0.53       | 

As expected, the inclusion of a penalty in the mis-prediction of the top
5% class (i.e. false negatives) significantly improves the correct
prediction of the top 5% class compared to the previous thresholds. To
further improve sensitivity, one could iterate the classification for
thresholds over the range (0.85-0.11) and check when the sensitivity
share is maximised.

###### Calibration curve

Finally, we can plot a visual representation of the fitted versus actual
values (i.e. y-y hat), analogous to the continuous case:

![](Technical_report_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

## RF Probability (with loss function)

We now move on to the last family of prediction models used in the
analysis, and compute a Random Forest Probability Prediction model
augmented by the same Loss Function defined in the previous section
(i.e. FN:FP = 10:1).

### Train set - M5 variables

    ##     CV.RMSE    CV.AUC Avg.of.optimal.thresholds Threshold.for.Fold5
    ## 1 0.2836597 0.8247216                 0.1245044           0.1455174
    ##   Avg.expected.loss Expected.loss.for.Fold5
    ## 1         0.4790507               0.4754204

Using RF further improves the model performance, either in terms of
lower RMSE as well as greater AUC. The outcoming threshold is 0.124
(higher than the one found for the Logit with Loss Function). Let’s test
the performance on the Holdout set

### Holdout set - M5 variables

    ## [1] "The Holdout set RMSE is : 0.299"

    ## [1] "The Holdout set AUC is : 0.814"

    ## [1] "The Holdout set Expected Loss is : 0.536"

Even for the Holdout set, better performance than in the Logit case. The
RF is confirmed as the best fit, with an AUC of 0.81.

In ordert to discover which predictors are the most relevant in building
the the classification trees, we can look at t he following Importance
Plot.

### Importance Plot

![](Technical_report_files/figure-gfm/unnamed-chunk-36-1.png)<!-- --> In
light of this plot, a comparison with thwe Logit M5 model can be made: -
Both models highlight the relevance of liquidity, tangible assets,
personnel expenses, material expenses, and profit/loss as influential
factors.

- Additionally, the Random Forest model identifies equity and
  non-current liabilities as important variables, thus capturing a
  broader range of financial indicators compared to Logit M5.

- With respect to firm demographics and management variables, the
  quadratic effect of firm age is confirmed in both the Logit M5 and the
  Random Forest model. Interestingly, the urban variable becomes
  significant, although its effect remains marginal in magnitude. Among
  management characteristics, foreign management, number of CEOs, and
  the presence of a young CEO also show marginal significance,
  suggesting some nuanced influence on firm growth.

Overall, we can say that both the Logit and RF models suggest a primary
importance of financial and economic indicators as key predictors of
firm growth.

# Summary results

    ##                Number.of.predictors   CV.RMSE    CV.AUC CV.threshold
    ## Logit M1                         18 0.3079775 0.6542494   0.09304137
    ## Logit M2                         23 0.3074968 0.6713788   0.09165050
    ## Logit M3                         28 0.3078720 0.6745415   0.08673419
    ## Logit M4                         31 0.3047171 0.6887460   0.09411350
    ## Logit M5                         42 0.3047837 0.6919572   0.08528789
    ## Logit LASSO                      10 0.3081739 0.6542332   0.08890120
    ## RF probability                   17 0.2836597 0.8247216   0.12450442
    ##                CV.expected.Loss
    ## Logit M1              0.7325461
    ## Logit M2              0.6932250
    ## Logit M3              0.6866319
    ## Logit M4              0.6796514
    ## Logit M5              0.6597316
    ## Logit LASSO           0.7046318
    ## RF probability        0.4790507

The Random Forest neatly outperforms the other models, and it is choosen
as our best candidate. We can compute the final classification table,
using the indicated threshold of 0.124.

And the corresponding prediction measureas:

| Model | Logit Default | Logit No Loss | Logit Loss | RF Loss |
|-------|---------------|---------------|------------|---------|
|Threshold| 0.50        | 0.11          | 0.085      | 0.124   |
|Accuracy | 0.88        | 0.65          | 0.54       | 0.54    |
|Sensitivity | 0.04     | 0.59          | 0.71       | 0.72    | 
|Specificity | 0.99     | 0.66          | 0.53       | 0.52    |


where results for the RF model are very similar to the previous Logit
with the Loss Function, but with the additional benefit of imporving
both RMSE and AUC values.

### Extra: RF Classification prediction

As a comparison to the Random Forest, we also include a classification
model with the same loss function. The resulting model shows an increase
in expected loss, indicating no improvement in the prediction of
high-growth firms, as indicated below:

``` r
# Use predicted classes to calculate expected loss based on loss fn
fp <- sum(data_holdout$rf_f_prediction_class == "top5" & data_holdout$d2_top5_f == "Not top 5%")
fn <- sum(data_holdout$rf_f_prediction_class == "no_top5" & data_holdout$d2_top5_f == "Top 5%")

exp_loss_rf_c <- (fp*FP + fn*FN)/length(data_holdout$d2_top5)

print(paste('The expected loss of the Houldout set with the RF Classification model is: ', round(exp_loss_rf_c,3), sep = ''))
```

    ## [1] "The expected loss of the Houldout set with the RF Classification model is: 1.07"

# Service versus Manufacturing (RF with loss function)

As last part of the analysis, we compare the model performance of our
best candidate on two firm categories in the dataset: manufacturing
(i.e. NACE industry codes from 10 t0 34) and services (i.e. codes from
41-Construction- onwards).

``` r
data$d2_top5_f <- factor(data$d2_top5_f, levels = c("Not top 5%","Top 5%"), labels = c("no_top5", "top5"))

dat_manu = data[data$ind2 >= 10 & data$ind2 <= 34, ]
dat_serv = data[data$ind2 >= 41 , ]


# 5 fold cross-validation

train_control <- trainControl(
  method = "cv",
  n = 5,
  classProbs = TRUE, # same as probability = TRUE in ranger
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)
train_control$verboseIter <- TRUE

tune_grid <- expand.grid(
  .mtry = c(5, 6, 7),
  .splitrule = "gini",
  .min.node.size = c(10, 15)
)
```

## Summary

While the computed AUC remains relatively stable across the two
categories, the optimal threshold differs significantly, with the
Service industry driving it up to 0.125. This drift might be due to the
predominance of Service industry in the database, it has more than
double the number of observations compared to the Manufacturing
industry.

As a final step, we can look at the importance plots related to the two
industries in the figure below, where a consistency of the set of
predictors confirms the result of the main analysis.

# References

Adams, R. B., & Ferreira, D. (2009). Women in the boardroom and their
impact on governance and performance. Journal of financial economics,
94(2), 291-309.

Berry, H. (2013). When do firms divest foreign operations?. Organization
Science, 24(1), 246-261.

Bertrand, M., & Schoar, A. (2003). Managing with style: The effect of
managers on firm policies. The Quarterly journal of economics, 118(4),
1169-1208.

Bruderl, J., & Schussler, R. (1990). Organizational mortality: The
liabilities of newness and adolescence. Administrative Science
Quarterly, 35(3), 530–547.

Gennaioli, N., La Porta, R., Lopez-de-Silanes, F., & Shleifer, A.
(2013). Human capital and regional development. The Quarterly journal of
economics, 128(1), 105-164.

Hall, B. H. (1993). The stock market’s valuation of R&D investment
during the 1980’s. The American Economic Review, 83(2), 259-264.

Mahmood, T. (2000). Survival of newly founded business: A log-logistic
model approach. Small Business Economics, 14, 223–237.

Penrose (1959), The Theory of the Growth of the Firm. Oxford university
press.

Rajan, R., & Zingales, L. (1996). Financial dependence and growth.

Yim, S. (2013). The acquisitiveness of youth: CEO age and acquisition
behavior. Journal of financial economics, 108(1), 250-273.
