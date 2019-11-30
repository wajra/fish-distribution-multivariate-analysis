# In this file we'll perform an exploratory analysis on the
# geofiltered value 
# Written for R Version 3.6.1


library(tidyverse)
library(MASS)

# Read in the data
bsb_data <- read.csv("data/black_sea_bass_nw_stock_filtered.csv")
# bsb_data <- bsb_data[!(bsb_data$season=='Summer'), ]
# bsb_data$season <- factor(bsb_data$season)
# bsb_data <- bsb_data %>% filter(season %in% c("Spring","Fall"))
lda_formula <- formula(season ~ SBT.seasonal + SST.seasonal.mean + SBT.min + SBT.max + SST.max + rugosity + GRAINSIZE)
sp_lda <- lda(lda_formula, data = bsb_data)
lda_values <- predict(sp_lda)

lda_df <- data.frame(lda_values$x[,1], lda_values$x[,2], bsb_data$season)
# Rename the columns
columns <- c("LD1", "LD2","season")
colnames(lda_df) <- columns

# The actual plotting
ggplot(lda_df, aes(x=LD1, y=LD2)) +
  geom_point(aes(color = factor(season)))

# Performing a Bartlett's test on the data
# First let's test for SBT.seasonal
# bartlett.test(count ~ spray, data = InsectSprays)

plot(SBT.seasonal ~ season, data = bsb_data)

bsb_data <- bsb_data %>% filter(season %in% c("Spring","Fall"))
bsb_data$season <- factor(bsb_data$season)
bartlett.test(SBT.seasonal ~ season, data = bsb_data)




