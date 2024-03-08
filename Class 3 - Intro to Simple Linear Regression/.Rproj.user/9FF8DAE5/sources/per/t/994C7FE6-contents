## STAT 7210 Simple Linear Regression R Code ##

library(tidyverse)
library(tidymodels)

## Read in Baseball Data ##

library(readxl)

baseball <- read_xlsx("Baseball Data.xlsx")

## Integrity Check ##

baseball |>
  glimpse()

## Build & Interpret Scatterplot ##

baseball |>
  ggplot(aes(x = R, y = Wins)) + geom_point() +
  labs(x = "Runs Scored", 
       y = "Regular Season Wins",
       title = "Relationship Between Runs Scored and Regular Season Wins",
       subtitle = "MLB 2009 - 2019 Seasons") + 
  theme_classic()

## Obtain Summary Statistics for Baseball Data ##

library(rstatix)

baseball |>
  select(Wins,R) |> 
  get_summary_stats(type="common")

## Get & Interpret Correlation Coefficient ##

cor(baseball$Wins,baseball$R)

## Fit Simple Linear Regression Model ##

## Traditional Method ##

trad_mod <- lm(Wins~R,data=baseball)

coef(trad_mod)

trad_mod |>
  tidy() |>
  select(term,estimate)

## Tidymodels Approach ##

library(broom)

tidy_mod <- linear_reg() |>
  set_engine("lm") |>
  fit(Wins~R,data=baseball)

tidy_mod |>
  tidy() |>
  select(term,estimate)

## Another Example Using the mtcars data ##

## Fit a line between MPG and Rear Axle Ratio ##

## We want to predict MPG ##

## Start with a scatterplot ##

data('mtcars')

## Inspect ##

mtcars |>
  glimpse()

## Data Visualization ##

mtcars |>
  ggplot(aes(x = drat, y = mpg)) + geom_point() +
  geom_smooth(method='lm',se=F) +
  labs(x = "Rear Axle Ratio",
       y = "Miles Per Gallon",
       title = "Relationship Between MPG & Rear Axle Ratio",
       subtitle = "Sample of 1973-74 Vehicles") + theme_classic()

## Interpret this plot using the four questions ##

## What's the correlation & how is it interpreted? ##

cor(mtcars$drat,mtcars$mpg)

## Obtain Estimates for Beta0 & Beta1. How are they interpreted? ##

car_mod <- lm(mpg~drat,data=mtcars)

car_mod |>
  tidy() |>
  select(term,estimate)

tidycar_mod <- linear_reg() |>
  set_engine("lm") |>
  fit(mpg~drat,data=mtcars)

tidycar_mod |>
  tidy() |>
  select(term,estimate)

## Calculating Test Statistics for Regression Coefficients ##

## Baseball Data ##

## Using trad_mod ##

trad_mod |>
  tidy()

## Using tidy_mod ##

tidy_mod |>
  tidy()

## Another Example Using the mtcars data ##

car_mod |>
  tidy()

tidycar_mod |>
  tidy()

car_mod |>
  glance()

## Get Confidence Intervals for Betas ##

## Baseball ##

## Traditional Method ##

trad_mod |>
  confint()

## Tidy Method ##

tidy_mod |>
  tidy(conf.int=T) |>
  select(term,conf.low,conf.high)

## Cars ##

## Traditional Method ##

car_mod |>
  confint()

## Tidy Method ##

car_mod |>
  tidy(conf.int=T) |>
  select(term,conf.low,conf.high)

## Plot the Fitted Values ##

baseball |>
  ggplot(aes(R,Wins)) +
  geom_point() +
  geom_smooth(method="lm",fill='red') + 
  labs(x = "Runs",
       y = "Regular Season Wins",
       title = "MLB Wins v Runs Fitted Model") + 
  theme_classic()

## Extract y-hats ##

## Baseball Example ##

fitted(trad_mod)

install.packages('moderndive')

library(moderndive)

trad_mod |>
  get_regression_points()

## Obtain R^2 for our models ##

trad_mod |>
  glance() |>
  select(r.squared)

tidy_mod |>
  glance() |>
  select(r.squared)

car_mod |>
  glance() |>
  select(r.squared)
