## STAT 7210 Introduction R Code ##

library(tidyverse)

## Read in Baseball Data ##

library(readxl)

baseball <- read_xlsx("Baseball Data.xlsx")

## Integrity Check ##

baseball |>
  glimpse()

## Build Scatterplot ##

baseball |>
  ggplot(aes(x = R, y = Wins)) + geom_point() +
  labs(x = "Runs Scored", 
       y = "Regular Season Wins",
       title = "Relationship Between Runs Scored and Regular Season Wins",
       subtitle = "MLB 2009 - 2019 Seasons")

## Another Example Using Palmer's Penguins ##

install.packages('palmerpenguins')

penguins <- palmerpenguins::penguins

## Plot the relationship between bill length & depth ##

penguins |>
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) + geom_point() +
  labs(x = "Bill Depth (in mm)", 
       y = "Bill Length (in mm)",
       title = "Relationship Between Penguin Bill Length and Depth") +
  theme_classic()

## Correlation Test ##

## Baseball - Traditional Method ##

cor.test(baseball$Wins,baseball$R)

## Penguins - Traditional Method ##

cor.test(penguins$bill_depth_mm,penguins$bill_length_mm)

## Baseball - Rstatix Method ##

library(rstatix)

baseball |>
  cor_test(Wins,R)

## Penguins - Rstatix Method ##

penguins |>
  cor_test(bill_depth_mm,bill_length_mm)

## Correlation Matrix ##

## Select Subset of Columns ##

baseball_sub <- baseball |>
  select(Wins,HR,RBI,SB,CS,BB,SO,BA)

cor(baseball_sub)

## A bit tough to read (Could output to Excel using writexl::write_xlsx) ##

## Something I like better ##

library(GGally)

ggcorr(baseball_sub) +
  ggtitle("Visualizing Correlation Matrix as a Heatmap") +
  theme(plot.title=element_text(hjust=0.5))

