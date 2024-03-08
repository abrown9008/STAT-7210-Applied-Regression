## STAT 7210 Indicator Variables R Code ##

library(tidyverse)

## Read in Tool Wear Data -- In this example, suppose we want to
## predict the useful life of a tool (in hours) used for cutting wood on
## a spinning lathe using the RPMs of the lathe as well as 
## a tool type (suppose we have two tool types, A & B) ##

library(readxl)

tool <- read_xlsx("Tool Wear Data.xlsx")

tool |>
  glimpse()

## Let's just see what's going on between RPMs & Hours 
## using a scatterplot ##

tool |>
  ggplot(aes(RPM,Hours)) + geom_point(aes(color=`Tool Type`)) + 
  theme_classic()

## Okay, here, we can sort of see two different models ##

## Fit the model...note, by default R chooses the "0" level to
## be the first alphabetically ##

mod <- lm(Hours~RPM+`Tool Type`,data=tool)

## Looks good! Let's check out the model! ##

library(broom)

mod |>
  glance()

mod |>
  tidy(conf.int = T,
       conf.level = 0.95)

## Tool type is significant meaning that the grouping we observed
## in the scatterplot could be attributed to the tool type. In other
## words, the difference between the intercepts is significant! ##

## Visualize the model graphically: ##

library(moderndive)

tool |>
  ggplot(aes(RPM,Hours)) + geom_point(aes(color=`Tool Type`)) +
  geom_smooth(method = 'lm', se = F, color = 'steelblue') +
  geom_parallel_slopes(aes(color=`Tool Type`),se=F) + theme_classic()

## As mentioned, if we want a specific level of our categorical predictor
## to be the "0" or "reference" group, we can do this in R quite easily: ##

tool$`Tool Type` <- relevel(factor(tool$`Tool Type`),ref="B")

lm(Hours~RPM+`Tool Type`,data=tool) |>
  tidy()

## Let's do another example using the penguin data. Let's predict body mass
## using flipper length as well as species ##

## Check Relationship Between Body Mass & Flipper Length ##

penguins <- palmerpenguins::penguins

penguins |>
  ggplot(aes(flipper_length_mm,body_mass_g)) + geom_point(aes(color=species)) +
  theme_classic()

## Pretty strong, positive, linear relationship, but there's a weird
## gap. Maybe species can help account for this. ##

## See which species will be reference group ##

penguins |>
  select(species) |>
  distinct()

## So Adelie will be the reference as it occurs first alphabetically ##

p_mod <- lm(body_mass_g~flipper_length_mm+species,data=penguins)

p_mod |>
  glance()

p_mod |>
  tidy()

## Visualize the Relationship ##

penguins |>
  ggplot(aes(flipper_length_mm,body_mass_g)) + geom_point(aes(color=species)) +
  geom_parallel_slopes(aes(color=species),se=F) +
  theme_classic()
  
## Assessing a Difference in Slopes ##

## Tool Data ##

## Fit the Model ##

t_mod <- lm(Hours~RPM+`Tool Type`+RPM*`Tool Type`,data=tool)

## Check it out! ##

t_mod |>
  glance()

t_mod |>
  tidy()

tool |>
  ggplot(aes(RPM,Hours)) + geom_point(aes(color=`Tool Type`)) +
  geom_smooth(aes(color=`Tool Type`),method='lm',se=F) +
  theme_classic()

## The results here would indicate that there isn't a difference 
## between the slopes ##

## Penguins Example ##

p_mod1 <- lm(body_mass_g~flipper_length_mm+species+
               flipper_length_mm*species,data=penguins)

## Are there significantly different slopes between penguin species? ##

p_mod1 |>
  glance()

p_mod1 |>
  tidy(conf.int=T)

## Let's visualize this! ##

penguins |>
  ggplot(aes(flipper_length_mm,body_mass_g)) + geom_point(aes(color=species)) +
  geom_smooth(aes(color=species),method='lm',se=F) +
  theme_classic()

## Comparing one-way ANOVA to regression approach to ANOVA using Penguin Data ##

## Suppose we want to compare body mass across the levels of penguin species ##

## One-Way ANOVA ##

anova_mod <- aov(body_mass_g~species,data=penguins)

anova_mod |>
  tidy()

## Post Hoc Test ##

anova_mod |>
  TukeyHSD() |>
  tidy()

## Get Estimated Coefficients from ANOVA model ##

model.tables(anova_mod,"means",digits=4)

## Double Check ##

library(rstatix)

penguins |>
  group_by(species) |>
  get_summary_stats(body_mass_g,type="mean")

## Okay now let's try it using the regression method ##

## Recall, by default Adelie will be the reference group ##

reg_mod <- lm(body_mass_g~species,data=penguins)

reg_mod |>
  tidy()

## b0 is the mean of Adelie ##

c(coef(reg_mod)[1],
  model.tables(anova_mod,"means",digits=4)$tables$species[1])

## b0 + b1 is the mean of Chinstrap ##

c(coef(reg_mod)[1] + coef(reg_mod)[2],
  model.tables(anova_mod,"means",digits=4)$tables$species[2])

## b0 + b2 is the mean of Gentoo ##

c(coef(reg_mod)[1] + coef(reg_mod)[3],
  model.tables(anova_mod,"means",digits=4)$tables$species[3])

## So both approaches are giving the same information ##
