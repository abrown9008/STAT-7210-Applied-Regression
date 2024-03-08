## STAT 7210 Assumption Checking and Residual Diagnostics ##

library(tidyverse)

## Load Boston Dataset ##

boston <- MASS::Boston

## Fit Linear Regression using rm & ptratio to predict medv ##

boston_mod <- lm(medv~rm+ptratio,data=boston)

## View Plot of Standardized Residuals ##

library(olsrr)

boston_mod |>
  ols_plot_resid_stand()

## NOte, the above function plots lines at +/- 2. 
## Regardless, we can see lots of values exceeding +3
## which would indicate that they are potential outliers ##

## To extract the rows containing potential outliers,
## we can use: ##

library(moderndive)

boston_mod |>
  get_regression_points() |>
  mutate(`Standardized Residual` = rstandard(boston_mod)) |>
  filter(abs(`Standardized Residual`) > 3)

## What about the studentized residuals? ##

## Obtain Plot of Studentized Residuals ##

## Not a good way I have found to do this so we 
## can do it manually using ggplot2: ##

library(ggrepel)

boston_mod |>
  get_regression_points() |>
  mutate(`Studentized Residual` = rstudent(boston_mod),
         Label = if_else(abs(`Studentized Residual`) > 3, as.character(ID), "")
         ) |>
  ggplot(aes(x=ID,y=`Studentized Residual`)) +
  geom_hline(yintercept=3,color='red') +
  geom_hline(yintercept=-3,color='red') +
  geom_hline(yintercept=0,color='black') +
  geom_point(color='blue') +
  geom_label_repel(aes(label = Label),color='maroon') +
  geom_segment(aes(xend=ID,yend=0),color='blue') +
  labs(title = "Studentized Residuals Plot") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.50))

## Then, obviously to obtain the outliers according to the
## studentized residuals, we can use: ##

boston_mod |>
  get_regression_points() |>
  mutate(`Studentized Residual` = rstudent(boston_mod)) |>
  filter(abs(`Studentized Residual`) > 3)

## Notice the studentized residuals are greater than the standardized residuals...why? ##

## Obtain PRESS Residuals ##

library(qpcR)

boston_mod |>
  get_regression_points() |>
  mutate(`PRESS Residuals` = PRESS(boston_mod,verbose=F)$residuals) |>
  arrange(desc(abs(`PRESS Residuals`))) |>
  head(10)

## These PRESS residuals aren't standardized so while we can plot them,
## looking at the biggest values can be an indicator of influential points ##


## Leverage Points -- The hii's ##

## Obtain the hii's ##

boston_mod |>
  get_regression_points() |>
  mutate(Hii = hatvalues(boston_mod))

## Since p = 3, our cutoff will be:

co <- (2*3)/nrow(boston)

## Leverage points, according to the hii's, will be: ##

boston_mod |>
  get_regression_points() |>
  mutate(Hii = hatvalues(boston_mod)) |>
  filter(Hii > co) |>
  arrange(desc(Hii))

## We have lots of points which could be considered leverage!! ##

## Visually: ##

boston_mod |>
  get_regression_points() |>
  mutate(Hii = hatvalues(boston_mod),
         Label = if_else(Hii > co, as.character(ID),"")) |>
  ggplot(aes(x=ID,y=Hii)) +
  geom_hline(yintercept=co,color='red') +
  geom_point(color='blue') +
  geom_segment(aes(xend=ID,yend=0),color='blue') +
  geom_label_repel(aes(label = Label),color='maroon',
                   max.overlaps = 25) +
  labs(title = "Hii Plot") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.50))

## Influential Points ##

## Cook's D - Plot ##

boston_mod |>
  ols_plot_cooksd_chart()

## The cutoff point used by Cook's D is typically 4/n ##

## Cook's D - Points ##

boston_mod |>
  get_regression_points() |>
  mutate(`Cook's D` = cooks.distance(boston_mod)) |>
  filter(`Cook's D` > 4/nrow(boston)) |>
  arrange(desc(`Cook's D`))

## DFBETAS - Plot ##

boston_mod |>
  ols_plot_dfbetas()

## DFBETAS - Points ##

boston_mod |>
  get_regression_points() |>
  bind_cols(dfbetas(boston_mod)) |>
  rename(rm = rm...3,
         ptratio = ptratio...4,
         `DFBETAS Intercept` = `(Intercept)`,
         `DFBETAS rm` = rm...8,
         `DFBETAS ptratio` = ptratio...9) |>
  filter(abs(`DFBETAS Intercept`) > 2/sqrt(nrow(boston)) |
           abs(`DFBETAS rm`) > 2/sqrt(nrow(boston)) |
           abs(`DFBETAS ptratio`) > 2/sqrt(nrow(boston)))

## DFFITS - Plot ##

boston_mod |>
  ols_plot_dffits()

## DFFITS - Points ##

boston_mod |>
  get_regression_points() |>
  mutate(DFFITS = dffits(boston_mod)) |>
  filter(abs(DFFITS) > 2*sqrt(3/nrow(boston)))

## COVRATIO - Plot ##

## Calculate two cutoffs ##

pco <- 1 + 3*3/nrow(boston)

nco <- 1 - 3*3/nrow(boston)

boston_mod |>
  get_regression_points() |>
  mutate(COVRATIO = covratio(boston_mod),
         Label = if_else(COVRATIO > pco | COVRATIO < nco,
                         as.character(ID),"")
         )|>
  ggplot(aes(x=ID,y=COVRATIO)) +
  geom_line(aes(y=pco),color='red') +
  geom_line(aes(y=nco),color='red') +
  geom_line(aes(y=1),color='red') +
  geom_point(color='blue') +
  geom_segment(aes(xend=ID,yend=1),color='blue') +
  geom_label_repel(aes(label = Label),color='maroon',
                   max.overlaps = 25) +
  labs(x="Observation Number",
       y="COVRATIO") +
  theme_classic()

## Assumption Checking ##

## Normality - QQ Plot ##

library(ggpubr)

boston_mod$residuals |>
  ggqqplot()

## Looks like some strong non-normality! ##

## Normality - Testing Method ##

library(nortest)

## Kolmogorov-Smirnov ##

ks.test(rstudent(boston_mod),"pnorm")

## Since p < 0.05, this indicates evidence of non-normality ##

## Shapiro-Wilk ##

shapiro.test(rstudent(boston_mod))

## Since p < 0.05, this indicates evidence of non-normality ##

## Anderson-Darling ##

ad.test(rstudent(boston_mod))

## Since p < 0.05, this indicates evidence of non-normality ##

## Cramer-von Mises ##

cvm.test(rstudent(boston_mod))

## Since p < 0.05, this indicates evidence of non-normality ##

## Lilliefors Test ##

lillie.test(rstudent(boston_mod))

## Since p < 0.05, this indicates evidence of non-normality ##

## Constant Variance - Visual Method ##

boston_mod |>
  get_regression_points() |>
  mutate(`Studentized Residuals` = rstudent(boston_mod)) |>
  ggplot(aes(x = medv_hat, y = `Studentized Residuals`)) +
  geom_point() +
  geom_hline(yintercept = 3, color='red') +
  geom_hline(yintercept = -3, color='red') +
  geom_hline(yintercept = 0, color='black') +
  theme_classic()

## We have a random blob in the middle but we have some weird
## points above +3 with a negative slope. Likely indicates
## non-constant variance but not too bad ##

## Constant Variance - Breusch Pagan Test ##

library(lmtest)

boston_mod |>
  bptest()

## Since p > 0.05, maybe the constant variance assumption is okay!! ##

