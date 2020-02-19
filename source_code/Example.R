### Example script for an Exam

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

# Import cichlid data, a grouped variable
cichlid <- read_csv("datasets/abd/chapter12/chap12q09Cichlids.csv")

cichlid_summary01 <- cichlid %>%
  group_by(genotype) %>%
  summarise(n_preference = n(),
            mean_preference = mean(preference),
            median_preference = median(preference),
            sd_preference = sd(preference),
            IQR_preference = IQR(preference),
            var_preference = var(preference),
            se_preference = sd(preference)/sqrt(n()))

# Histogram cichlid data
ggplot(cichlid) +
  geom_histogram(aes(preference), binwidth = .1)+
  facet_wrap(~genotype)


