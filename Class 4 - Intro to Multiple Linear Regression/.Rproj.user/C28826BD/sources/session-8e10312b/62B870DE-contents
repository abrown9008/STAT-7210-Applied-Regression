## STAT 7210 - Multiple Linear Regression R Code ## 

## Part I ##

library(tidyverse)
library(tidymodels)

## Matrix & Vector Operations in R ##

## Multiplication ##

A <- matrix(c(2,3,
              0,-1,
              -2,5),byrow=T,ncol=2,nrow=3)

B <- matrix(c(3,-2,
              0,1),byrow=T,ncol=2,nrow=2)

A
B

A%*%B

## Transpose ##

At <- t(A)

At

## Inverse ##

A <- matrix(c(2,15,
              9,-8),byrow=T,ncol=2)

solve(A)

## Identity Matrix ##

round(solve(A)%*%A)

## Advertising Example ##

## Suppose we want to predict our organizations sales using
## ad expenses for TV ads, radio ads, and newspaper ads? ##

## Read in Data ##

library(readxl)

sales <- read_xlsx("Advertising.xlsx")

## Integrity Check ##

sales |>
  glimpse()

## Graphical Exploratory Analysis ##

## Old School R Scatterplot Matrix ##

pairs(sales)

## Old School R Correlation Matrix ##

cor(sales)

## Correlation Heat Map ##

library(GGally)

sales |>
  ggcorr() +
  labs(title = "Heatmap Correlation Matrix",
       subtitle = "For Sales Data") +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

## Combo Plot ##

sales |>
  ggpairs() +
  labs(title = "More Informative Scatterplot Matrix",
       subtitle = "For Sales Data") + 
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))

## Build a Model for Sales Data ##

library(broom)

mod <- lm(Sales~TV+Radio+Newspaper, data=sales)

mod |>
  tidy()  

## Test for the Significance of the Model ##

mod |>
  glance()

## Penguin Data Example ##

## Let's say we want to use bill length and depth as well as flipper length to 
## predict a penguin's body mass ##

penguins <- palmerpenguins::penguins

## Exploratory Analysis ##

sub_penguins <- penguins |>
  select(body_mass_g,flipper_length_mm,
         bill_length_mm,bill_depth_mm)

cor(sub_penguins,use="na.or.complete")

sub_penguins |>
  ggpairs() +
  labs(title = "Scatterplot Matrix & Density Plots",
       subtitle = "For Penguin Data") + 
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))

sub_penguins |>
  ggcorr() +
  labs(title = "Heatmap Correlation Matrix",
       subtitle = "For Penguin Data") +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

## Fit Model -- Perform Test ##

pen_mod <- lm(body_mass_g~bill_length_mm+bill_depth_mm+
              flipper_length_mm,data=sub_penguins)

pen_mod |> 
  glance()

## Individual Coefficient Marginal Tests - Sales Data ##
## With 95% CI's for beta-hats ##

mod |>
  tidy(conf.int = T, 
       conf.level = 0.95)

## Penguin Data ##

pen_mod |>
  tidy(conf.int = T, 
       conf.level = 0.95)

## CI for Mean Response of Sales Data ##

library(moderndive)

mod |> 
  predict(interval = 'confidence')

mod |>
  get_regression_points()

## Fit = y-hat ##
