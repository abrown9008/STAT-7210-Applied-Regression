## STAT 7210 Variable Selection/Model Building R Code ##

library(tidyverse)

## Fit Penguins Model to Obtain Model Adequacy Metrics ##

penguins <- palmerpenguins::penguins |>
  na.omit()

pmod <- lm(body_mass_g~flipper_length_mm+bill_length_mm,
           data=penguins)

## We can obtain R2, Adjusted R2, & MSE by: ##

library(broom)

pmod |>
  glance()

## Note, sigma = sqrt(MSE), sometimes called RMSE ##

## Mallows's Cp ##

library(olsrr)

red_mod <- lm(body_mass_g~flipper_length_mm,data=penguins)

ols_mallows_cp(red_mod,pmod)

## PRESS ##

ols_press(pmod)

## AIC ##

ols_aic(pmod)

## BIC ##

## Schwartz ##

ols_sbc(pmod)

## Sawa compares models, like Mallow's Cp ##

ols_sbic(red_mod,pmod)

## Read in the Cement Data ##

library(readxl)

cement <- read_xlsx("Cement.xlsx")

## Summary ##

cement |>
  glimpse()

## Fit Full Model ##

cmod <- lm(y~.,data=cement)

## All Possible Regressions ##

k <- ols_step_all_possible(cmod)

plot(k)

results_df <- k$result

## Biggest Adj R2? ##

results_df |>
  dplyr::filter(adjr == max(adjr))

## Index 11...include x1 x2 and x4 ##

## Lowest MSE ##

results_df |>
  dplyr::filter(msep == min(msep))

## Index 11...include x1 x2 and x4 ##

## Lowest Cp ##

results_df |>
  dplyr::filter(cp == min(cp))

## Index 5...include x1 and x2 ##

## Lowest AIC ##

results_df |>
  dplyr::filter(aic == min(aic))

## Index 11...include x1 x2 and x4 ##

## Schwartz BIC ##

results_df |>
  dplyr::filter(sbc == min(sbc))

## Index 5...include x1 and x2 ##

## Note, by default, SAS uses Sawa BIC ##

## Sawa BIC ##

results_df |>
  dplyr::filter(sbic == min(sbic))

## Index 5...include x1 and x2 ##

## We have 3 in favor of model 11 and 3 in favor of model 5 ##
## So which do we choose? In my opinion, here's where parsimony comes 
## into play. Plus the differences between the metrics are fairly 
## negligible. ##

## Fit Model 5 ##

new_mod <- lm(y~x1+x2,data=cement)

## Check Assumptions ##

## Normality ##

library(ggpubr)

new_mod$residuals |>
  ggqqplot()

ks.test(rstandard(new_mod),'pnorm')

## Constant Variance ##

ggplot() + geom_point(aes(fitted(new_mod),rstudent(new_mod))) +
  theme_classic()

library(lmtest)

new_mod |>
  bptest()

## VIF ##

new_mod |>
  ols_vif_tol()

## Everything looks good! Let's look at our model summary! ##

## Omnibus ##

new_mod |>
  glance()

## Individual Coefficients ##

new_mod |>
  tidy(conf.int=T)

## Let's try again with the penguins data ##

## Fit Full Model ##

## First Select Variables ##

p1 <- penguins |>
  dplyr::select(body_mass_g,species,bill_length_mm,bill_depth_mm,
                flipper_length_mm,sex)

fpmod <- lm(body_mass_g~.,data=p1)

## All Possible Regressions ##

r <- ols_step_all_possible(fpmod)

p_df <- r$result

## Biggest Adj R2 ##

p_df |>
  dplyr::filter(adjr == max(adjr))

## Index 31...species, bill length, bill depth, flipper length & sex ##

## Smallest MSE ##

p_df |>
  dplyr::filter(msep == min(msep))

## Index 31...species, bill length, bill depth, flipper length & sex ##

## Smallest Mallows's Cp ##

p_df |>
  dplyr::filter(cp == min(cp))

## Index 31...species, bill length, bill depth, flipper length & sex ##

## Smallest AIC ##

p_df |>
  dplyr::filter(aic == min(aic))

## Index 31...species, bill length, bill depth, flipper length & sex ##

## Smallest Schwartz BIC ##

p_df |>
  dplyr::filter(sbc == min(sbc))

## Index 31...species, bill length, bill depth, flipper length & sex ##

## Smallest Sawa BIC ##

p_df |>
  dplyr::filter(sbic == min(sbic))

## Index 31...species, bill length, bill depth, flipper length & sex ##

## So it looks like Index 31 is our best fitting model!

## Check Assumptions ##

## Normality ##

fpmod$residuals |>
  ggqqplot()

ks.test(rstudent(fpmod),'pnorm')

## Constant Variance ##

ggplot() + geom_point(aes(fitted(fpmod),rstudent(fpmod))) +
  theme_classic()

fpmod |>
  bptest()

## VIF ##

fpmod |>
  ols_vif_tol()

## Check out the results ##

## Omnibus ##

fpmod |>
  glance()

## Individual Coefficients ##

fpmod |>
  tidy(conf.int=T)

## Forward Selection using Cement Data ##

fc <- ols_step_forward_aic(cmod)

plot(fc)

fc

## While the x1, x2, x4 model is the "best," the graphs show
## that only small improvements are made to our indices with
## the inclusion of x2. The model with x1 & x4 is nearly as good.
## Notice that this is close to what we found with all possible 
## regressions, but not exactly the same. ##

## Backward Selection ##

bc <- ols_step_backward_aic(cmod)

plot(bc)

## The plots indicate step 1. Let's see what the method says ##

bc

## It only wanted x3 removed leaving us with x1, x2, & x4 ##

## Stepwise Regression ##

sc <- ols_step_both_aic(cmod)

plot(sc)

## The plots would indicate step 2 is our model. Let's see! ##

sc

## The method suggests x1, x2 & x4. Our result is similar to what
## we've previously obtained. But remember, this is not generally
## the case. ##

## We can go through another example using the penguin data! ##

## Forward Selection ##

fp <- ols_step_forward_aic(fpmod)

plot(fp)

## The "elbow" indicates step 3 ##

fp

## Backward Selection ##

bp <- ols_step_backward_aic(fpmod)

plot(bp)

## The blank plots indicate that...

bp

## Stepwise Selection ##

sp <- ols_step_both_aic(fpmod)

plot(sp)

## The elbow indicates step 3. Let's see! ##

sp

## It suggests the same model as forward selection ##

## Lasso ##

library(glmnet)

## Let's try this with the penguins data ##

## Define Response ##

y <- penguins$body_mass_g

## Define Predictors ##

X <- data.matrix(p1[,c("species","bill_length_mm","bill_depth_mm",
                       "flipper_length_mm","sex")])

lasso_mod <- cv.glmnet(x=X,y=y,alpha=1)

## What is the best value of our tuning parameter that
## minimizes MSE? ##

best_lambda <- lasso_mod$lambda.min

best_lambda

## Visualize ##

plot(lasso_mod)

## Obtain Coefficients of Best Model ##

best_model <- glmnet(x=X,y=y,alpha=1,lambda=best_lambda)

coef(best_model)

## So Lasso also suggests using the full model! ##

## However, let's compare the coefficient estimates to that of regular
## least-squares regression: ##

coef(fpmod)

## Notice something interesting: our categorical predictors are coded
## differently with glmnet than they are with regular lm ##

## Adjusted R2? ##

var_y <- var(y)

rz <- predict.glmnet(best_model,newx=X,s=best_lambda)

MSE <- sum((y - rz)^2)/(nrow(p1)-6)

1 - MSE/var_y