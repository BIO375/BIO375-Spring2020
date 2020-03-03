# Clean up the working environment
rm(list = ls())

### Install and load packages ####

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}

# Check for updates
tidyverse_update()

dolphin <- read_csv("datasets/abd/chapter11/chap11q16DolphinsClockwise.csv")
dolphin_summary <- dolphin %>%
  summarise(n=n(),
            mean_CW = mean(percentClockwise),
            sd_CW = sd(percentClockwise),
            med_CW = median(percentClockwise))

koala <- read_csv("datasets/abd/chapter11/chap11q17KoalaBellows.csv")
ggplot(koala)+
  geom_histogram(aes(femalePreference), binwidth = 2)
ggplot(koala)+
  geom_boxplot(aes("", femalePreference))+
  stat_summary(aes(x = "", y = femalePreference), 
               fun.y=mean, 
               colour="darkred", 
               geom="point", 
               shape=18, 
               size=3)
summ_koala <- koala %>%
  summarise(n = n(),
            mean_pref = mean(femalePreference),
            sd_pref = sd(femalePreference),
            se_pref = sd(femalePreference)/sqrt(n()))
t <- summ_koala$mean_pref/summ_koala$se_pref

            