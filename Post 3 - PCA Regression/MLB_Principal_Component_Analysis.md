Using Pricipal Component Regression to Predict WAR From 2018 MLB Data
================
David Swets
December 11, 2018

Since advanced stats have been all the rage in baseball since "Moneyball" first came out, let's attempt to determine the most significant predictor of WAR of American League position players using data from the 2018 season. In this era of advanced metrics, WAR (Wins Above Replacement) has become possibly the most influential statistic used to evaluate the overall value of an MLB player. This data comes from Baseball Reference (<https://www.baseball-reference.com/leagues/AL/2018-standard-batting.shtml>) and the file used for this analysis can also be found here on this GitHub page. Because WAR is calculated slightly differently depending on the source, just to clarify, we will be using Baseball Reference's calculation of WAR for this analysis.

Let's start by loading in the required packages and reading in the two data sets. To get the WAR information, we'll need to read in this additional data set and join the two together; however, we'll only keep the "Name" and "WAR" columns from the second data set, as the rest of the columns are unnecessary or even potentially duplicates of data we already have.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(QuantPsyc)
```

    ## Loading required package: boot

    ## Loading required package: MASS

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    ## 
    ## Attaching package: 'QuantPsyc'

    ## The following object is masked from 'package:base':
    ## 
    ##     norm

``` r
AL <- read.delim("al_standard.txt", sep = ",")


WAR <- read.delim("al_war.txt", sep = ",")

# pulling just the Name and WAR columns
WAR <- WAR[, c("Name", "WAR")]
```

Now, we want to join them together to get a WAR value for each player, and we'll also remove incomplete cases.

``` r
full_AL <- inner_join(AL, WAR, by = "Name")

# removing rows with NA's
full_AL <- na.omit(full_AL)
```

Let's start by predicting WAR using a few traditional baseball stats. We can't use all of the variables we have because of the obvious problems we'd run into from including redundant variables such as OBP (On Base Percentage), SLG (Slugging Percentage), and OPS (which is equal to OBP + Slugging Pct), for example. Let's keep this one simple using some very straightforward predictors: Age, At-Bats, Runs, Hits, Home Runs, Runs Batted In, Stolen Bases, and Strike Outs.

``` r
AL_num <- full_AL[, unlist(lapply(full_AL, is.numeric))]


m1 <- lm(WAR ~ Age + AB + R + H + HR + RBI + SB + SO, data = AL_num)
summary(m1)
```

    ## 
    ## Call:
    ## lm(formula = WAR ~ Age + AB + R + H + HR + RBI + SB + SO, data = AL_num)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5847 -0.2454  0.0125  0.1460  3.5933 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.1548510  0.2245011  -0.690  0.49068    
    ## Age          0.0054722  0.0079198   0.691  0.48993    
    ## AB          -0.0211134  0.0013819 -15.279  < 2e-16 ***
    ## R            0.0616204  0.0056952  10.820  < 2e-16 ***
    ## H            0.0578708  0.0050702  11.414  < 2e-16 ***
    ## HR           0.0397146  0.0128100   3.100  0.00205 ** 
    ## RBI          0.0012626  0.0059364   0.213  0.83166    
    ## SB           0.0410562  0.0076893   5.339 1.44e-07 ***
    ## SO           0.0009271  0.0020436   0.454  0.65029    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6519 on 478 degrees of freedom
    ## Multiple R-squared:  0.8221, Adjusted R-squared:  0.8192 
    ## F-statistic: 276.2 on 8 and 478 DF,  p-value: < 2.2e-16

``` r
# calculating RMSE
sqrt(mean(residuals(m1)^2))
```

    ## [1] 0.6458068

``` r
# standardized betas
lm.beta(m1)
```

    ##         Age          AB           R           H          HR         RBI 
    ##  0.01375030 -2.64109456  1.12832075  1.91598195  0.22875437  0.02267739 
    ##          SB          SO 
    ##  0.15351467  0.02842674

We see from this analysis that At-Bats, Runs, Hits, Home Runs, and Stolen Bases are all significant predictors of WAR. This model produces an R^2 of .8221 and an RMSE of .6458. Looking at the standardized betas, we see that At-Bats actually has the most significant impact on WAR, and a negative impact at that. We'll retain only the significant predictors for the next model.

``` r
# keeping only the significant variables

m2 <- lm(WAR ~ AB + R + H + HR + SB, data = AL_num)
summary(m2)
```

    ## 
    ## Call:
    ## lm(formula = WAR ~ AB + R + H + HR + SB, data = AL_num)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6114 -0.2399  0.0216  0.1322  3.5823 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.0009203  0.0411983  -0.022    0.982    
    ## AB          -0.0206384  0.0009850 -20.952  < 2e-16 ***
    ## R            0.0613907  0.0056443  10.877  < 2e-16 ***
    ## H            0.0572687  0.0040793  14.039  < 2e-16 ***
    ## HR           0.0426587  0.0083916   5.083 5.32e-07 ***
    ## SB           0.0401619  0.0074554   5.387 1.12e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6503 on 481 degrees of freedom
    ## Multiple R-squared:  0.8219, Adjusted R-squared:   0.82 
    ## F-statistic: 443.9 on 5 and 481 DF,  p-value: < 2.2e-16

``` r
# calculating RMSE for m2
sqrt(mean(residuals(m2)^2))
```

    ## [1] 0.6462327

This cleaner model has an R^2 value of .8219 and an RMSE of .6462. Only slightly different than the model before and much more parsimonious!

Now, let's calculate the first 2 principal components of all of our quantitative variables (except WAR, of course). Once we have these values, we'll plug these into a new model as predictors and see how our model changes.

``` r
al.pca <- prcomp(AL_num[,1:26], center = TRUE,scale. = TRUE)
summary(al.pca)
```

    ## Importance of components:
    ##                           PC1    PC2     PC3     PC4     PC5     PC6
    ## Standard deviation     3.8518 1.8515 1.36419 1.02936 0.99232 0.89734
    ## Proportion of Variance 0.5706 0.1318 0.07158 0.04075 0.03787 0.03097
    ## Cumulative Proportion  0.5706 0.7025 0.77406 0.81482 0.85269 0.88366
    ##                            PC7     PC8     PC9   PC10    PC11   PC12
    ## Standard deviation     0.75696 0.70409 0.65362 0.5837 0.53067 0.4327
    ## Proportion of Variance 0.02204 0.01907 0.01643 0.0131 0.01083 0.0072
    ## Cumulative Proportion  0.90570 0.92477 0.94120 0.9543 0.96513 0.9723
    ##                           PC13    PC14    PC15    PC16    PC17    PC18
    ## Standard deviation     0.41537 0.37964 0.34916 0.29620 0.27238 0.21775
    ## Proportion of Variance 0.00664 0.00554 0.00469 0.00337 0.00285 0.00182
    ## Cumulative Proportion  0.97897 0.98451 0.98920 0.99258 0.99543 0.99726
    ##                           PC19   PC20    PC21    PC22    PC23      PC24
    ## Standard deviation     0.17203 0.1444 0.11662 0.08096 0.02742 0.0009887
    ## Proportion of Variance 0.00114 0.0008 0.00052 0.00025 0.00003 0.0000000
    ## Cumulative Proportion  0.99839 0.9992 0.99972 0.99997 1.00000 1.0000000
    ##                             PC25      PC26
    ## Standard deviation     0.0009595 1.711e-15
    ## Proportion of Variance 0.0000000 0.000e+00
    ## Cumulative Proportion  1.0000000 1.000e+00

``` r
# keeping only the first 2 PCS
pcs = data.frame("pc1" = al.pca$x[,1], "pc2" = al.pca$x[,2]) 

al_pca_join <- data.frame(AL_num, pcs)
```

Now, let's build the model

``` r
# model using 1st two principal components as predictors

m3 <- lm(WAR ~ pc1 + pc2, data= al_pca_join)

summary(m3)
```

    ## 
    ## Call:
    ## lm(formula = WAR ~ pc1 + pc2, data = al_pca_join)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6690 -0.3990  0.1639  0.3926  7.0391 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.68439    0.04659  14.688   <2e-16 ***
    ## pc1         -0.29327    0.01211 -24.219   <2e-16 ***
    ## pc2          0.07753    0.02519   3.078   0.0022 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.028 on 484 degrees of freedom
    ## Multiple R-squared:  0.5519, Adjusted R-squared:   0.55 
    ## F-statistic:   298 on 2 and 484 DF,  p-value: < 2.2e-16

``` r
# calculating RMSE for m2
sqrt(mean(residuals(m3)^2))
```

    ## [1] 1.02508

We can see here that our new R^2 value has dropped slightly to .5856 and the RMSE has increased to .9858. However, as we saw from the pca summary above, the first 2 principal components only explain about 70% of the variation in the overall data. While this is actually quite a lot considering that that these 2 dimensions explain about 70% of what was explained in 26 dimensions, there's little reason to think that using just these two predictors would perform better than the model we built above. Therefore, while typically very effective, in this case, using the first 2 principal components as predictors resulted in a slightly lesser performing model than the straightforward regression model produced above.
