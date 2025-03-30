
# Testing prediction on other later dates/cities

After the selection of the GBM broad model for our main database MIQ1, we test its goodness of fit on other two databases:
- MIQ3
- BGQ3 (![Script](https://github.com/ccastelli1994/DataAnalysis3/blob/main/Assignment2/Part2_Validation/02_predictBGQ3.R) )

By repeating the entire model selection process performed for the main database MIQ1. 

The results are summirised in the final Horse-Race Table below:

| Model CV RMSE                   | MIQ3  |  BGQ3 | 
|---------------------------------|-------|-------|
|  OLS M4                         | 0. | 0.395 |
|LASSO M4                         | 0. | 0.423 |
|Random forest (smaller model M3) | 0. | 0.419 |
|Random forest M4                 | 0. | 0.395 |
|Random forest M4 + interact      | 0. | 0.394 |
|GBM (basic tuning) M4            | 0. | 0.398 |
|GBM (broad tuning) M4            | 0. | 0.392 |

where in both new samples, the GBM algorithms perform best. Hence, we can have a look at the corresponding fitted-to-actual y values:

![](https://github.com/ccastelli1994/DataAnalysis3/blob/main/Assignment2/Part1_modelling/Plots/GBM_baseBGQ3.png)

