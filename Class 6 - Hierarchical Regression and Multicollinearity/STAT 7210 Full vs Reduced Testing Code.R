## STAT 7210 - Full vs Reduced Testing ##

library(tidyverse)

## Read in Data ##

library(readxl)

dt <- read_xlsx("Delivery Time Data.xlsx")

## Integrity Check ##

dt |>
  glimpse()

## Suppose we want to predict the amount of time (in minutes)
## a delivery takes using the distance the storefront
## is from the parking lot (in feet) ##

## Fit a SLR using Distance ##

m1 <- lm(`Delivery Time`~Distance,data=dt)

## Omnibus Test ##

library(broom)

m1 |>
  glance()

## Individual Coefficient Test ##

m1 |>
  tidy()

## Extracting R2 & Adjusted R2 ##

m1 |>
  glance() |>
  select(r.squared,adj.r.squared)

## Now suppose we want to include the number of cases
## being delivered as this would seem to also be relevant
## in determining delivery time: ##

m2 <- lm(`Delivery Time`~`Number of Cases` + Distance,data=dt)

## Omnibus Test ##

m2 |>
  glance()

## Individual Coefficient Test ##

m2 |>
  tidy()

## Extracting R2 & Adjusted R2 ##

m2 |>
  glance() |>
  select(r.squared,adj.r.squared)

## Here, we can clearly see a big jump in R2/Adj R2 from
## m1 to m2. To determine whether this jump is statistically
## meaningful, we can perform our full v reduced test to 
## assess the added contribution of number of cases to the
## model which only included distance ##

anova(m1,m2) |>
  tidy()

## So we can see that the inclusion of Distance substantially
## improves model fit! ##

## Attach the Cars dataset ##

mtcars <- datasets::mtcars
data('mtcars')

## Full vs Reduced Hypothesis Test (also called hierarchical regression) ##

## Is a model which predicts mpg using drat, qsec, and wt a significantly better
## fit than a model which only uses drat? ##

full_mod <- lm(mpg~drat+qsec+wt,data=mtcars)

reduced_mod <- lm(mpg~drat,data=mtcars)

anova(reduced_mod,full_mod) |>
  tidy()

## Compare adj R^2 ##

full_mod |>
  glance() |>
  select(adj.r.squared)

reduced_mod |>
  glance() |>
  select(adj.r.squared)

## Let's now do another example using the penguin data. Is including bill length
## and flipper length in a model already containing bill depth to predict body mass
## preferable to the SLR model? ##

penguins <- palmerpenguins::penguins

full_p_mod <- lm(body_mass_g~bill_depth_mm+bill_length_mm+flipper_length_mm,
                 data=penguins)

reduced_p_mod <- lm(body_mass_g~bill_depth_mm,data=penguins)

anova(reduced_p_mod,full_p_mod) |>
  tidy()

full_p_mod |>
  glance() |>
  select(adj.r.squared)

reduced_p_mod |>
  glance() |>
  select(adj.r.squared)
