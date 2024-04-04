## Ridge Regression & Lasso Code ##

library(tidyverse)
library(glmnet)

## Append Credit Data & fit model ##

credit <- ISLR::Credit

credit_mod <- lm(Balance~Income+Limit+Rating+Student,
                 data=credit)

## Check Assumptions ##

## Normality ##

library(ggpubr)

credit_mod$residuals |>
  ggqqplot()

ks.test(rstudent(credit_mod),"pnorm")

## Normality doesn't seem too good ##

## Constant Variance ##

library(moderndive)

credit_mod |>
  get_regression_points() |>
  mutate(`Studentized Residuals` = rstudent(credit_mod)) |>
  ggplot(aes(x=`Balance_hat`,y=`Studentized Residuals`)) + 
  geom_point() + 
  geom_hline(yintercept = 3, color='red') +
  geom_hline(yintercept = -3, color='red') +
  geom_hline(yintercept = 0, color='black',
             linetype='dashed') +
  labs(y="Studentized Residuals",
       x="Fitted Values") +
  theme_classic()

library(lmtest)

credit_mod |>
  bptest()

## Constant Variance is also for sure not met ##

## Multicollinearity ##

library(olsrr)

credit_mod |>
  ols_vif_tol()

## Pretty severe violations. Can we observe this in
## our coefficient estimates? ##

library(broom)

credit_mod |>
  tidy()

## We can for sure see it in Income...we have a negative
## coefficient here, but: ##

cor(credit$Income,credit$Balance)

## A positive correlation. This is a telltale sign of 
## inflated variance causing imprecise estimates ##

## Let's see if Ridge Regression can help us! ##

library(MASS)

ridge <- lm.ridge(Balance~Income+Limit+Rating+Student,
                  data=credit,lambda=seq(0.01,100,0.01))

plot(ridge)

## We can also estimate the value of lambda using the select function 
## within MASS: ##

MASS::select(ridge)

## HKB and L-W are different methods for estimating lambda ##

## GCV (general cross validation) is a slightly different method for estimating 
## the location of lambda ##

## Let's use the GCV estimator lambda = 1.08 ##

lambda <- 1.08

## Refit Ridge ##

refit_ridge <- lm.ridge(Balance~Income+Limit+Rating+Student,
                  data=credit,lambda=lambda)

## Get Coefficients ##

round(coef(refit_ridge),4)

##