# Exam 1
# Script to generate plots and answer questions

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

### Salmon mass ####
# Read in salmon mass data
salmon <- read_csv("datasets/abd/chapter02/chap02f2_5SalmonBodySize.csv", 
                   col_types = cols(sex = col_character()))

# Calculate summary statistics for massKg
summary_massKg <- salmon %>%
  summarise(mean_massKg = mean(massKg),
            median_massKg = median(massKg),
            IQR_massKg = IQR(massKg),
            sd_massKg = sd(massKg),
            var_massKg = var(massKg))

# Boxplot of salmon weight in kg
ggplot(data = salmon)+
  geom_boxplot(aes(x = "", y = massKg), notch = TRUE)+
  stat_summary(aes(x = "", y = massKg), 
               fun.y=mean, 
               colour="darkred", 
               geom="point", 
               shape=18, 
               size=3)

ggplot(data = salmon)+
  geom_boxplot(aes(x = "", y = massKg), notch = TRUE)+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

### Inbreeding in Spanish royalty ####

# This creates the dataframe and names it inbreeding
inbreeding <- tribble(
  ~Parents, ~F,~PostnatalSurvival,
  #--/--/--
  "PhilipII-ElizabethValois", 0.01,1.00,
  "PhilipI-JoannaI", 0.04,1.00,
  "Ferdinand-ElizabethCastile", 0.04,1.00,
  "PhilipIV-ElizabethBourbon", 0.05, 0.50,
  "PhilipIII-MargaretAustria", 0.12, 0.63,
  "CharlesI-IsabellaPortugal", 0.12, 0.60,
  "PhillipII-AnnaAustria", 0.22, 0.20,
  "PhilipIV-MarianaAustria", 0.25,0.40
)

# Calculate summary statistics for massKg
summary_F <- inbreeding %>%
  summarise(mean_F = mean(F),
            median_F = median(F),
            IQR_F = IQR(F),
            sd_F = sd(F),
            var_F = var(F))

# Boxplot of inbreeding weight in kg
ggplot(data = inbreeding)+
  geom_boxplot(aes(x = "", y = F), notch = TRUE)+
  stat_summary(aes(x = "", y = F), 
               fun.y=mean, 
               colour="darkred", 
               geom="point", 
               shape=18, 
               size=3)


### Family size ####

Original histogram difficult to read
family <- tibble(
  y = c(
    rep(1,11),
    rep(2,24),
    rep(3,25),
    rep(4,40)
  )
)

ggplot(family) +
  geom_histogram(aes(NumberChildren), binwidth = 1)

discrete <- tribble(
  ~x, ~y,
  2, 1/36,
  3, 2/36,
  4, 3/36,
  5, 4/36,
  6, 5/36,
  7, 6/36,
  8, 5/36,
  9, 4/36,
  10, 3/36,
  11, 2/36,
  12, 1/36
)

discrete <- discrete %>%
  factor(x, levels = unique(x))

ggplot(data = discrete)+
  geom_col(mapping = aes(x=x, y=y))+
  coord_cartesian(ylim = c(0,0.25))
