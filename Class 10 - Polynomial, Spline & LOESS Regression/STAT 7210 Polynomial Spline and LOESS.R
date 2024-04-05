## STAT 7210 - Polynomial, Spline, and LOESS Regression ##

library(tidyverse)

## Polynomial Regression ##

## Read in the hardwood data ##

library(readxl)

hardwood <- read_xlsx("Hardwood.xlsx")

hardwood |>
  glimpse()

## Generate Scatterplot ##

hardwood |>
  ggplot(aes(`Hardwood Concentration`,`Tensile Strength`)) + 
  geom_point() +
  geom_smooth(method='lm',se=F) +
  theme_classic()

ex <- lm(`Tensile Strength`~.,data=hardwood)

## Evaluate Normality ##

## QQ-Plot ##

library(ggpubr)

ex$residuals |>
  ggqqplot()

ks.test(rstudent(ex),'pnorm')

## Constant Variance ##

ggplot() + geom_point(aes(x=fitted(ex),
                          y=rstudent(ex))) +
  theme_classic()

library(lmtest)

ex |>
  bptest()

## Okay this is pretty clearly a non-linear relationship. Maybe we try a second
## order polynomial model? ##

mod <- lm(`Tensile Strength` ~ poly(`Hardwood Concentration`,degree=2),
          data=hardwood)

## Check the assumptions ##

## Normality ##

library(ggpubr)

mod$residuals |>
  ggqqplot()

ks.test(rstudent(mod),'pnorm')

## Normality seems reasonable! ##

## Constant Variance ##

library(moderndive)

ggplot() + 
  geom_point(aes(x=fitted(mod),y=rstudent(mod))) + 
  geom_hline(yintercept = 3, color='red') +
  geom_hline(yintercept = -3, color='red') +
  geom_hline(yintercept = 0, color='black',
             linetype='dashed') +
  labs(y="Studentized Residuals",
       x="Fitted Values") +
  theme_classic()

library(lmtest)

mod |>
  bptest()

## Not great, but not bad ##

## Let's examine the omnibus F test ##

library(broom)

mod |>
  glance()

## Now, the individual coefficient tests ##

mod |>
  tidy(conf.int = T)

## We can see the squared term is significant! 
## let's check out how the model fits, visually ##

hardwood |>
  ggplot(aes(`Hardwood Concentration`,`Tensile Strength`)) + 
  geom_point() +
  geom_smooth(method='lm',se=F,color='purple') +
  geom_line(aes(y=fitted(mod)),color='red') +
  theme_classic()

## We can see here that the introduction of the squared term
## substantially improves the fit of the model over the SLR
## model ##

## Spline Regression in R ##

## Read in Car Data ##

cars <- read_xlsx("Car Data.xlsx")

cars |>
  glimpse()

## Check out the Scatterplot ##

cars |>
  ggplot(aes(`Job Tenure`,`Cars Sold`)) + 
  geom_point() + 
  theme_classic()

## Okay, pretty clear we have a non-linear relationship. Let's try a second order
## polynomial model ##

cars_mod <- lm(`Cars Sold` ~ poly(`Job Tenure`,degree=2),
               data=cars)

## Check the Assumptions as usual ##

## Normality ##

cars_mod$residuals |>
  ggqqplot()

ks.test(rstudent(cars_mod),'pnorm')

## Constant Variance ##

ggplot() + 
  geom_point(aes(fitted(cars_mod),rstudent(cars_mod))) +  
  geom_hline(yintercept = 3, color='red') +
  geom_hline(yintercept = -3, color='red') +
  geom_hline(yintercept = 0, color='black',
             linetype='dashed') +
  labs(y="Studentized Residuals",
       x="Fitted Values") +
  theme_classic()

cars_mod |>
  bptest()

## Overall, pretty good on both counts!! ##

## Visualize model fit ##

cars |>
  ggplot(aes(`Job Tenure`,`Cars Sold`)) + 
  geom_point() +
  geom_smooth(method='lm',se=F,color='purple') +
  geom_line(aes(y=fitted(cars_mod)),color='red') +
  theme_classic()

## Again, we can see a second order polynomial is 
## better the SLR. But what if we tried splines? ##

## It looks like maybe 350 is a good knot? Let's try! ##

library(lspline)

s_mod <- lm(`Cars Sold` ~ lspline(`Job Tenure`,knots=350),
            data=cars)

## Check Assumptions ##

## Normality ##

s_mod$residuals |>
  ggqqplot()

ks.test(rstudent(s_mod),'pnorm')

## Looks good! What about constant variance? ##

ggplot() +
  geom_point(aes(x=fitted(s_mod),y=rstudent(s_mod))) + 
  geom_hline(yintercept = 3, color='red') +
  geom_hline(yintercept = -3, color='red') +
  geom_hline(yintercept = 0, color='black',
             linetype='dashed') +
  labs(y="Studentized Residuals",
       x="Fitted Values") +
  theme_classic()

s_mod |>
  bptest()

## Also looks pretty good! Let's now evaluate the omnibus F test ##

s_mod |>
  glance()

s_mod |>
  tidy(conf.int = T)

## Comparing Linear Splines to Polynomial Regression: ##

cars |>
  ggplot(aes(`Job Tenure`,`Cars Sold`)) + 
  geom_point() +
  geom_smooth(method='lm',se=F,color='purple') +
  geom_line(aes(y=fitted(cars_mod)),color='red') +
  geom_line(aes(y=fitted(s_mod)),color='steelblue') +
  theme_classic()

## Compare Adj R2 ##

cars_mod |>
  glance() ## 0.843 ##

s_mod |>
  glance() ## 0.893 ##

## LOESS ##

## Let's first try s = 0.50 ##

s <- 0.50

## Note, loess function doesn't like names
## with spaces in them. So we can fix this
## by: 

cars <- cars |>
  mutate(New_Y = `Cars Sold`,
         New_X = `Job Tenure`)

l50_mod <- loess(New_Y~New_X,span=s,
                 data=cars)

## Let's evaluate the model visually: ##

cars |>
  ggplot(aes(`Job Tenure`,`Cars Sold`)) + 
  geom_point() +
  geom_smooth(method='lm',se=F,color='purple') +
  geom_line(aes(y=fitted(cars_mod)),color='red') +
  geom_line(aes(y=fitted(s_mod)),color='steelblue') +
  geom_line(aes(y=fitted(l50_mod)),color='saddlebrown') +
  theme_classic()

## For LOESS regression, testing coefficients really
## isn't possible nor the goal. We can obtain SSE:

loess_sse <- sum(l50_mod$residuals^2)

## SST is constant for all models:

sst <- var(cars$New_Y)*(nrow(cars)-1)

## So R2 for LOESS is: ##

1 - loess_sse/sst

## So overall comparison: ##

## Simple Linear Regression: ##

lm(New_Y~New_X,data=cars) |>
  glance() |>
  select(r.squared)

## Polynomial Regression: ##

cars_mod |>
  glance() |>
  select(r.squared)

## Splines Regression: ##

s_mod |>
  glance() |>
  select(r.squared)

## LOESS: ##

1 - loess_sse/sst

## Append the Boston Data ##

boston <- MASS::Boston

## medv ~ rm ##

boston |>
  ggplot(aes(x=rm,y=medv)) +
  geom_point() +
  geom_smooth(method='lm',se=F,color='purple') +
  theme_classic()

s1 <- 0.1
s2 <- 0.2
s3 <- 0.3
s4 <- 0.4
s5 <- 0.5

l1 <- loess(medv~rm,data=boston,span=s1)
l2 <- loess(medv~rm,data=boston,span=s2)
l3 <- loess(medv~rm,data=boston,span=s3)
l4 <- loess(medv~rm,data=boston,span=s4)
l5 <- loess(medv~rm,data=boston,span=s5)

boston |>
  ggplot(aes(x=rm,y=medv)) +
  geom_point() +
  geom_smooth(method='lm',se=F,color='purple') +
  geom_line(aes(y=fitted(l1)),color='blue') +
  geom_line(aes(y=fitted(l2)),color='red') +
  geom_line(aes(y=fitted(l3)),color='green') +
  geom_line(aes(y=fitted(l4)),color='steelblue') +
  geom_line(aes(y=fitted(l5)),color='saddlebrown') +
  theme_classic()
