## STAT 7210 - Transformations Code ##

library(tidyverse)

## Read in Energy Data ##

library(readxl)

energy <- read_xlsx("Energy.xlsx")

## Integrity Check ##

energy |>
  glimpse()

## Fit Regression Model to Predict Demand using Usage ##

## Check Scatterplot ##

energy |>
  ggplot(aes(`kWh Usage`,`kW Demand`)) + geom_point() +
  labs(y="kW Demand",
       x="kWh Usage") +
  theme_classic()

## Looks Good, Right? ##

mod <- lm(`kW Demand` ~ `kWh Usage`,data=energy)

## Check Normality Assumption ##

library(ggpubr)

mod$residuals |>
  ggqqplot()

ks.test(rstudent(mod),"pnorm")

## Check Constant Variance Assumption ##

## Plot Studentized Residuals against Fitted Values ##

library(moderndive)

mod |>
  get_regression_points() |>
  mutate(`Studentized Residuals` = rstudent(mod)) |>
  ggplot(aes(x=`kW Demand_hat`,y=`Studentized Residuals`)) + 
  geom_point() + 
  geom_hline(yintercept = 3, color='red') +
  geom_hline(yintercept = -3, color='red') +
  geom_hline(yintercept = 0, color='black',
             linetype='dashed') +
  labs(y="Studentized Residuals",
       x="Fitted Values") +
  theme_classic()

library(lmtest)

bptest(mod)

## Since kW Demand is a sort of count (e.g., Poisson-ish), maybe the sqrt transformation
## will help correct the variance issue ##

energy <- energy |>
  mutate(new_y = sqrt(`kW Demand`))

## Go through the process again ##

## Check Scatterplot ##

energy |>
  ggplot(aes(`kWh Usage`,new_y)) + geom_point() +
  labs(x="kW Usage",
       y="Transformed kW Demand") +
  theme_classic()

## Pretty Decent Overall!! ##

mod1 <- lm(new_y ~ `kWh Usage`,data=energy)

## Check Normality Assumption ##

mod1$residuals |>
  ggqqplot()

ks.test(residuals(mod1),"pnorm")

## Check Constant Variance Assumption ##

## Plot Studentized Residuals against Fitted Values ##

mod1 |>
  get_regression_points() |>
  mutate(`Studentized Residuals` = rstudent(mod1)) |>
  ggplot(aes(x=new_y_hat,y=`Studentized Residuals`)) + 
  geom_point() + 
  geom_hline(yintercept = 3, color='red') +
  geom_hline(yintercept = -3, color='red') +
  geom_hline(yintercept = 0, color='black',
             linetype='dashed') +
  labs(y="Studentized Residuals",
       x="Fitted Values") +
  theme_classic()

bptest(mod1)

## Results ##

library(broom)

mod1 |>
  glance()

mod1 |>
  tidy(conf.int=T)

## Box Cox Transformation on Energy Data ##

library(MASS)

bc <- boxcox(mod)

bc_df <- tibble(lambda = bc$x,
                LL = bc$y)

bc_df |>
  filter(LL == max(LL)) |>
  dplyr::select(lambda) |>
  as.numeric() -> lambda

lambda

## Do the Transformation and Refit the Model ##

energy$bc_y <- (energy$`kW Demand`^lambda - 1)/lambda

trans_mod <- lm(bc_y~`kWh Usage`,data=energy)

## Check Normality Assumption ##

trans_mod$residuals |>
  ggqqplot()

ks.test(rstudent(trans_mod),"pnorm")

## Check Constant Variance Assumption ##

## Plot Studentized Residuals against Fitted Values ##

trans_mod |>
  get_regression_points() |>
  mutate(`Studentized Residuals` = rstudent(trans_mod)) |>
  ggplot(aes(x=bc_y_hat,y=`Studentized Residuals`)) + 
  geom_point() + 
  geom_hline(yintercept = 3, color='red') +
  geom_hline(yintercept = -3, color='red') +
  geom_hline(yintercept = 0, color='black',
             linetype='dashed') +
  labs(y="Studentized Residuals",
       x="Fitted Values") +
  theme_classic()

bptest(trans_mod)

trans_mod |>
  glance()

trans_mod |>
  tidy(conf.int=T)

## Another Example Using the Delivery Time Data ##

dt <- read_xlsx("Delivery Time Data.xlsx")

dt_mod <- lm(`Delivery Time` ~ `Number of Cases` + Distance, data=dt)

## Check Assumptions ##

## Normality ##

dt_mod$residuals |>
  ggqqplot()

ks.test(rstudent(dt_mod),"pnorm")

## Constant Variance ##

dt_mod |>
  get_regression_points() |>
  mutate(`Studentized Residuals` = rstudent(dt_mod)) |>
  ggplot(aes(x=`Delivery Time_hat`,y=`Studentized Residuals`)) + 
  geom_point() + 
  geom_hline(yintercept = 3, color='red') +
  geom_hline(yintercept = -3, color='red') +
  geom_hline(yintercept = 0, color='black',
             linetype='dashed') +
  labs(y="Studentized Residuals",
       x="Fitted Values") +
  theme_classic()

bptest(dt_mod)

## Box-Cox Transformation ##

bc1 <- boxcox(dt$`Delivery Time`~dt$`Number of Cases`+dt$Distance)

bc1_df <- tibble(lambda = bc1$x,
                 LL = bc1$y)

bc1_df |>
  filter(LL == max(LL)) |>
  dplyr::select(lambda) |>
  as.numeric() -> lambda1

lambda1

## Do the Transformation and Refit the Model ##

dt$bc_y <- (dt$`Delivery Time`^lambda - 1)/lambda

trans_mod1 <- lm(bc_y~`Number of Cases` + Distance,data=dt)

## Now recheck assumptions ##

## Normality ##

trans_mod1$residuals |>
  ggqqplot()

ks.test(rstudent(trans_mod1),"pnorm")

## Constant Variance ##

trans_mod1 |>
  get_regression_points() |>
  mutate(`Studentized Residuals` = rstudent(trans_mod1)) |>
  ggplot(aes(x=bc_y_hat,y=`Studentized Residuals`)) + 
  geom_point() + 
  geom_hline(yintercept = 3, color='red') +
  geom_hline(yintercept = -3, color='red') +
  geom_hline(yintercept = 0, color='black',
             linetype='dashed') +
  labs(y="Studentized Residuals",
       x="Fitted Values") +
  theme_classic()

bptest(trans_mod1)

trans_mod1 |>
  glance()

trans_mod1 |>
  tidy(conf.int=T)

## Linearize the Equation ##

## Read in Windmill Data ##

windmill <- read_xlsx("Windmill.xlsx")

## Integrity Check ##

windmill |>
  glimpse()

## Check Linearity Assumption ##

windmill |>
  ggplot(aes(`Wind Velocity`,`DC Output`)) + 
  geom_point() +
  labs(x="Wind Velocity",
       y="DC Output") + 
  theme_classic()

## Not Bad at first blush, but it does look a bit square root-ish ##

## Fit Model ##

windmill_mod <- lm(`DC Output`~`Wind Velocity`,data=windmill)

## Check Assumptions ##

## Normality ##

windmill_mod$residuals |>
  ggqqplot()

ks.test(rstudent(windmill_mod),"pnorm")

## Constant Variance ##

windmill_mod |>
  get_regression_points() |>
  mutate(`Studentized Residuals` = rstudent(windmill_mod)) |>
  ggplot(aes(x=`DC Output_hat`,y=`Studentized Residuals`)) + 
  geom_point() + 
  geom_hline(yintercept = 3, color='red') +
  geom_hline(yintercept = -3, color='red') +
  geom_hline(yintercept = 0, color='black',
             linetype='dashed') +
  labs(y="Studentized Residuals",
       x="Fitted Values") +
  theme_classic()

bptest(windmill_mod)

## BP is okay (could be small sample size) but the graph shows real issues ##

## A VST may work, but we could also try a LT as it looks like from the 
## scatterplot that as wind velocity increases, the total amount of DC Output 
## approaches a limit, which makes sense. When functions have an upper asymptote,
## one suggested transformation is to invert either the response or explanatory
## variable (sometimes both). Let's just invert wind velocity and see what happens. ##

windmill$inverted_x <- windmill$`Wind Velocity`**(-1)

wm_bc <- boxcox(windmill_mod,lambda=c(-5,5))

wm_bc_df <- tibble(lambda = wm_bc$x,
                   LL = wm_bc$y)

wm_bc_df |>
  filter(LL == max(LL)) |>
  dplyr::select(lambda) |>
  as.numeric() -> lambda2
  
## Check Linearity Assumption ##

windmill |>
  ggplot(aes(inverted_x,`DC Output`)) + 
  geom_point() +
  labs(x="Inverse Wind Velocity",
       y="DC Output") + 
  theme_classic()

## Fit Model ##

windmill_mod1 <- lm(`DC Output`~inverted_x,data=windmill)

## Check Assumptions ##

## Normality ##

windmill_mod1$residuals |>
  ggqqplot()

ks.test(rstudent(windmill_mod1),"pnorm")

## Constant Variance ##

windmill_mod1 |>
  get_regression_points() |>
  mutate(`Studentized Residuals` = rstudent(windmill_mod1)) |>
  ggplot(aes(x=`DC Output_hat`,y=`Studentized Residuals`)) + 
  geom_point() + 
  geom_hline(yintercept = 3, color='red') +
  geom_hline(yintercept = -3, color='red') +
  geom_hline(yintercept = 0, color='black',
             linetype='dashed') +
  labs(y="Studentized Residuals",
       x="Fitted Values") +
  theme_classic()

bptest(windmill_mod1)

windmill_mod1 |>
  glance()

windmill_mod1 |>
  tidy(conf.int=T)
