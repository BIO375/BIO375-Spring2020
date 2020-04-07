### Lecture 1-way ANOVA I

# Clean up the working environment
rm(list = ls())

### Install and load packages ####

if(!require(Rmisc)){install.packages("multcomp")}
if(!require(DescTools)){install.packages("nlme")}
if(!require(summarytools)){install.packages("ggfortify")}
if(!require(tidyverse)){install.packages("tidyverse")}

# Check for updates
tidyverse_update()

### Scenario 1. Pig weights ####
# To make sure everyone is working with the same original file, I am going to 
# create a datafile longhand
pigs <-read_csv("obs, feed, weight
                  1, 1, 60
                  2, 1, 55
                  3, 1, 65
                  4, 2, 70
                  5, 2, 65
                  6, 2, 75
                  7, 3, 75
                  8, 3, 85
                  9, 3, 80", 
                col_types = cols(
                    feed = col_character()) )

# Generate summary statistics that we will need for sum of squares
summ_wgt <- pigs %>%
  group_by(feed) %>%
  summarise(mean_wgt = mean(weight),
            sd_wgt = sd(weight),
            n = n())
grand_mean_wgt <- mean(pigs$weight)

model_pigs <- lm(weight~feed, data = pigs)
anova(model_pigs)
### Scenario 2.  Diatom diversity

diatoms <- read_csv("zinc,diversity
back,2.27
back,1.7
back,2.05
back,1.98
back,2.2
back,1.53
back,0.76
back,1.89
high,1.25
high,1.15
high,0.63
high,1.04
high,1.9
high,1.88
high,0.85
high,1.43
high,1.37
low,1.4
low,2.18
low,1.83
low,1.88
low,2.1
low,2.38
low,2.83
low,1.66
medium,1.62
medium,2.19
medium,2.1
medium,2.06
medium,2.02
medium,1.94
medium,1.75
medium,0.8
medium,0.98")

summ_div <- diatoms %>%
  group_by(zinc) %>%
  summarise(mean_dia = mean(diversity),
            n_dia = n())
grand_mean_div = mean(diatoms$diversity)

model_div <- lm(diversity~zinc, data = diatoms)
anova(model_div)
