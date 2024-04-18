## STAT 7210 Validation Techniques R Code ##

library(tidyverse)

## Load up Penguin Data. Let's say we want to predict body mass
## using flipper length and species. ##

penguins <- na.omit(palmerpenguins::penguins)

## Split the Data into a 80/20 Training/Testing Set ##

library(rsample)

p_split <- initial_split(penguins,prop=0.80)

train_data <- training(p_split)

test_data <- testing(p_split)

## Fit Model using training set ##

pmod <- lm(body_mass_g~flipper_length_mm+species,data=train_data)

## Evaluate Model ##

## Normality Assumption ##

library(ggpubr)

pmod$residuals |>
  ggqqplot()

ks.test(rstudent(pmod),'pnorm')

## Okay looks good!! ##

## Constant Variance Assumption ##

ggplot() + geom_point(aes(fitted(pmod),rstudent(pmod))) +
  theme_classic()

library(lmtest)

pmod |>
  bptest()

## VIF ##

library(olsrr)

pmod |>
  ols_vif_tol()

## Okay, all assumptions are met! Let's check out the results ##

library(broom)

pmod |>
  glance()

pmod |>
  tidy(conf.int=T)

## Assuming this makes sense to us as penguin experts,
## let's now validate this model using the testing set ##

## Obtain Predicted Values for Testing Set ##

pv <- tibble(Prediction = predict(pmod,newdata=test_data),
             Observed = test_data$body_mass_g)

## Graphically evaluate the predicted & observed values ##

pv |>
  ggplot(aes(Prediction,Observed)) + 
  geom_point() +
  labs(x = "Predicted Values",
       y = "Observed Values") + 
  theme_classic()

## A more perfect, positive linear relationship indicates a 
## more accurate model ##

## Mean Square Error ##

MSE <- sum((pv$Observed - pv$Prediction)^2)/nrow(pv)

RMSE <- sqrt(MSE)

c(MSE,RMSE)

## MAE ##

MAE <- sum(abs(pv$Observed - pv$Prediction))/nrow(pv)

MAE

## LOOCV ##

library(caret)

ctrl2 <- trainControl(method = "LOOCV")

## Fit the original Penguin Model and use LOOCV to evaluate performance ##

loo_mod <- train(body_mass_g~flipper_length_mm+species, 
                data = penguins, method = "lm", trControl = ctrl2)

print(loo_mod)

## What is the resulting model?? ##

loo_mod$finalModel |>
  glance()

loo_mod$finalModel |>
  tidy(con.int=T)

## k-fold cross validation ##

k <- 5

ctrl <- trainControl(method = "cv", number = k)

## Fit the original Penguin Model and use k-fold CV to evaluate performance ##

model <- train(body_mass_g~flipper_length_mm+species, 
               data = penguins, method = "lm", trControl = ctrl)

## View Summary of k-fold CV ##

print(model)

## Okay, so what's the resulting model? ##

model$finalModel |>
  glance()

model$finalModel |>
  tidy(conf.int=T)

## If you want to/need to, you can also go through and do all
## of the assumption checking as we've done throughout the term! ##

## Normality ##

model$finalModel$residuals |>
  ggqqplot()

ks.test(rstudent(model$finalModel),'pnorm')

## Constant Variance ##

ggplot() + geom_point(aes(predict(model$finalModel),
                          rstudent(model$finalModel))) +
  theme_classic()

bptest(model$finalModel)

## VIF ##

model$finalModel |>
  ols_vif_tol()

## Let's do another example using the ISLR::Auto data ##

## Let's fit a model predicting mpg using displacement, weight, 
## & horsepower and validate it using both LOOCV & k-folds with k = 5 ##

## LOOCV ##

install.packages('ISLR')

autocntrl <- trainControl(method='LOOCV')

LOOCV_mod <- train(mpg~displacement+weight+horsepower,
                   data=ISLR::Auto,method='lm',trControl=autocntrl)

## View a Summary ##

print(LOOCV_mod)

## Evaluate Assumptions ##

## Normality ##

LOOCV_mod$finalModel$residuals |>
  ggqqplot()

ks.test(rstudent(LOOCV_mod$finalModel),
        'pnorm')

## Constant Variance ##

ggplot() + 
  geom_point(aes(predict(LOOCV_mod$finalModel),rstudent(LOOCV_mod$finalModel))) +
  theme_classic()

bptest(LOOCV_mod$finalModel)

## VIF ##

ols_vif_tol(LOOCV_mod$finalModel)

## Check out omnibus & individual coefficient results ##

LOOCV_mod$finalModel |>
  glance()

LOOCV_mod$finalModel |>
  tidy(conf.int=T)

## Try it again with k-fold ##

autocntrl2 <- trainControl(method='cv',number=5)

k_mod <- train(mpg~displacement+weight+horsepower,
                   data=ISLR::Auto,method='lm',trControl=autocntrl2)

## View a Summary ##

print(k_mod)

## Evaluate Assumptions ##

## Normality ##

k_mod$finalModel$residuals |>
  ggqqplot()

ks.test(rstudent(k_mod$finalModel),
        'pnorm')

## Constant Variance ##

ggplot() + 
  geom_point(aes(predict(k_mod$finalModel),rstudent(k_mod$finalModel))) +
  theme_classic()

bptest(k_mod$finalModel)

## VIF ##

ols_vif_tol(k_mod$finalModel)

## Check out omnibus & individual coefficient results ##

k_mod$finalModel |>
  glance()

k_mod$finalModel |>
  tidy(conf.int=T)
