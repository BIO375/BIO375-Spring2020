### Linear regression lecture example

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

library(ggfortify)
library(broom)
library(tidyverse)

tidyverse_update()
christ <- read_csv("datasets/quinn/chpt5/christ.csv")

# * LAKE - name of the North American lake from which observations were collected
# * AREA - lake shoreline area
# * CABIN - density of cabins along the shoreline
# * RIP.DENS - density (no. km^-1) of riparian trees along shoreline
# * RIP.BASA - basal area (m^2 km^-1) of riparian trees along the shoreline
# * CWD.DENS - density (no. km^-1) of course woody debris (>5cm diam.) in nearshore water 	
# * CWD.BASA - basal area (m^2 km^-1) of course woody debris (>5cm diam.) along the shoreline
# * L10CABIN - Log10 transformation of CABIN
# * LCWD.BAS - Log10 transformation of CWD.BASA
# * RESID1 - residuals from linear regression of CWD.BASA against RIP.DENS
# * PREDICT1 - predicted CWD.BASA from linear regression of CWD.BASA against RIP.DENS
# * RESID2 - residuals from linear regression of CWD.BASA against CABIN
# * PREDICT2 - predicted CWD.BASA from linear regression of CWD.BASA against CABIN
# * RESID3 - residuals from linear regression of CWD.BASA against L10CABIN
# * PREDICT3 - predicted CWD.BASA from linear regression of CWD.BASA against L10CABIN
# * RESID4 - residuals from linear regression of LCWD.BAS against L10CABIN
# * PREDICT4 - predicted LCWD.BAS from linear regression of LCWD.BAS against L10CABIN

ggplot(data = christ) +
  geom_point(mapping = aes(x = RIP.DENS, y = CWD.BASA),
             colour = "firebrick", size = 2)+
  xlim(0,2200)+
  ylim(-100,200)+
  theme_bw()+
  labs( x = "Density of riparian trees", y = "Basal area of coarse woody debris")

model_lake <- lm(CWD.BASA ~ RIP.DENS, data = christ)
summary(model_lake)
anova(model_lake)

ggplot(data = christ, aes(x = RIP.DENS, y = CWD.BASA)) +
  geom_point(colour = "firebrick", size = 2) +
  geom_smooth(method = "lm", level=0.95, fullrange = TRUE) +
  theme_bw() +
  xlim(0,2200)+
  ylim(-100,200)+
  labs( x = "Density of riparian trees", y = "Basal area of coarse woody debris")
