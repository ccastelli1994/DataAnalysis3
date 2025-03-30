
# Testing prediction on other later dates/cities

After the selection of the GBM broad model for our main database MIQ1, we test its goodness of fit on other two databases:
- MIQ3 (![ScriptMIQ3](https://github.com/ccastelli1994/DataAnalysis3/blob/main/Assignment2/Part2_Validation/02_predictMIQ3.R))
- BGQ3 (![ScriptBGQ3](https://github.com/ccastelli1994/DataAnalysis3/blob/main/Assignment2/Part2_Validation/02_predictBGQ3.R))

Unfortunately, the initial number of observations in BGQ3 (3.3k) shrinks to ~1.2k after the data cleaning process, as we decide to keep properties only in the city of Bergamo and not in the surrounding area.

Then, the entire model selection process performed for the main database MIQ1 is repeated, and we summarise results in the following section. 
First, we can compare the final CV RMSE in a Horse-Race Table, where to the MIQ1 Holdout set (6.3k) we compared the entire observations contained in MIQ3 (~21k) and BGQ3(~1.5k) :

| Model CV RMSE                   |MIQ1 Test| MIQ3  |  BGQ3 | 
|---------------------------------|---------|-------|-------|
|  OLS M4                         | 0.490   | 0.515 | 0.395 |
|LASSO M4                         | 0.539   | 0.569 | 0.423 |
|Random forest (smaller model M3) | 0.558   | 0.606 | 0.419 |
|Random forest M4                 | 0.469   | 0.513 | 0.395 |
|Random forest M4 + interact      | 0.488   | 0.530 | 0.394 |
|GBM (basic tuning) M4            | **0.434** | 0.497 | 0.398 |
|GBM (broad tuning) M4            | 0.442   | **0.494** | **0.392** |

Easily we can notice that across the different samples, the GBM algorithms return the lowest RMSE, hence they should be preferred.
Finally, a graphic visualisation of both GBM fits can be plotted by looking at the corresponding fitted-vs-actual values:

## MIQ3
![](https://github.com/ccastelli1994/DataAnalysis3/blob/main/Assignment2/Part1_modelling/Plots/GBM_baseMIQ3.png)

## BGQ3
![](https://github.com/ccastelli1994/DataAnalysis3/blob/main/Assignment2/Part1_modelling/Plots/GBM_baseBGQ3.png)

At a first glance, the majority of predicted values seem to gravitate around the 45-degree line, indicating good prediction power. 
However, as noticed already in the main dataset, values towards the top price distribution (log scale) of the two live sets are predicted with less accurancy than the other values, flagging for the presence of outliers that are hard to model. 
The log transformation may further amplify this underestimation, and data trimming should be considered in future applications to improve the prediction power.
In addition, prediction on prices in level terms may also be considered as alternative model.


