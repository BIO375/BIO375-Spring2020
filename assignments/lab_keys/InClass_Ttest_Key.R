### Four t-test scenarios given in class, due Monday 2020-03-09

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

### Scenario 1. Birth rates ####
# To make sure everyone is working with the same original file, I am going to 
# create a datafile longhand
births <-read_csv("county, birth_1982, birth_2000
                  afgh, 48, 44
                  alge, 44, 37
                  arge, 24, 20
                  aust, 12, 12
                  bang, 49, 42
                  barb, 17, 18
                  belg, 12, 12
                  boli, 42, 35
                  braz, 31, 26
                  cana, 15, 14
                  chil, 24, 21
                  colo, 28, 27
                  cost, 31, 28
                  cuba, 16, 18
                  czec, 15, 14")

# When you are performing a paired t-test, you need to clearly define how you 
# are calculating the difference, because it will matter when you interpret the
# results, especially with a one-tailed test!  In other words a test of diff = (A - B)
# is not the exact same as a test of diff = (B-A).  

# Below, I have diff = birth_2000 -birth_1982, which means the alternate hypothesis
# for the onesided test is true mean difference < 0.

births <- births %>%
  mutate(diff = birth_2000 - birth_1982)

ggplot(births) +
  geom_histogram(aes(diff), binwidth = 1)
ggplot(births) +
  geom_boxplot(aes(x = "", y = diff))+
  stat_summary(aes(x = "", y = diff), 
               fun.y=mean, 
               colour="blue", 
               geom="point", 
               shape=21, 
               size=3)

ggplot(births)+
  geom_qq(aes(sample = diff))

summ_births <- births %>%
  summarise(mean_y = mean(diff),
            sd_y = sd(diff),
            se_y = sd_y/sqrt(n()),
            n_y = n())


tbirth = summ_births$mean_y/summ_births$se_y

tcrit = qt(0.95)

### Scenario 2 ##################
data01 <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
data01 <- data01 %>%
  slice(-105)

summ_lizard <- data01 %>%
  group_by(Survival) %>%
  summarise(mean_y = mean(squamosalHornLength),
            sd_y = sd(squamosalHornLength),
            se_y = sd_y/sqrt(n()),
            n_y = n())

ggplot(data01) +
  geom_histogram(aes(squamosalHornLength), binwidth = 1)+
  facet_wrap(~Survival)
ggplot(data01) +
  geom_boxplot(aes(x = Survival, y = squamosalHornLength))
ggplot(data01)+
  geom_qq(aes(sample = squamosalHornLength, color = Survival)) 
ratio <-(max(summ_lizard$sd_y))/(min(summ_lizard$sd_y))

