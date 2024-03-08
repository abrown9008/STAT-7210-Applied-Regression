## STAT 7210 - Multicollinearity Code ##

library(tidyverse)

## Attach the Cars dataset ##

mtcars <- datasets::mtcars

## Fit Model using drat, qsec & wt
## to predict mpg: ##

car_mod <- lm(mpg~drat+qsec+wt,data=mtcars)

## Calculating VIF ##

library(olsrr)

car_mod |>
  ols_vif_tol()

## Okay cool! So none of the predictors exhibit 
## too much multicollinearity according to VIF ##

## Condition Indices ##

car_mod |>
  ols_eigen_cindex() |>
  select(Eigenvalue,`Condition Index`)

car_mod |>
  tidy()

## One above 30...maybe a problem ##

## Condition Number ##

car_mod |>
  ols_eigen_cindex() |>
  filter(`Condition Index` == max(`Condition Index`)) |>
  select(`Condition Index`)

## Let's go through another example ##

## Read in Acetylene Data ##

library(readxl)

acetylene <- read_xlsx("Acetylene.xlsx")

acetylene |>
  glimpse()

## Conversion is the % of a particular chemical being converted to acetlyne
## t is the reactor temperature 
## h is the mole ratio 
## c is the contact time ##

## Create New Variables ##

acetylene <- acetylene |>
  mutate(th = t*h,
         tc = t*c,
         hc = h*c,
         t2 = t^2,
         h2 = h^2,
         c2 = c^2)

## Set up model ##

mod <- lm(Conversion ~., data=acetylene)

mod |>
  tidy()

## Check out these standard errors...pretty big!! ##

## Find Pairwise Correlations ##

library(GGally)

acetylene |>
  ggcorr() +
  labs(title = "Heatmap Correlation Matrix",
       subtitle = "For Acetylene Data") +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

## VIF ##

options(scipen=999)

mod |>
  ols_vif_tol()

## Some pretty bad ones! ##

## Condition Indices ##

mod |>
  ols_eigen_cindex() |>
  select(Eigenvalue,`Condition Index`)

## Condition Number ##

mod |>
  ols_eigen_cindex() |>
  filter(`Condition Index` == max(`Condition Index`)) |>
  select(`Condition Index`)

## Clearly, we have a severe problem that needs to be addressed. ##