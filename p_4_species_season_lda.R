# In this file we'll perform an exploratory analysis on the
# geofiltered values for the Northern Atlantic stocks of
# black sea bass, summer flounder, and scup
# Written for R Version 3.6.1


library(tidyverse)
library(MASS)

# Read in the data
sp_obs_data <- read.csv("data/all_species_north_atlantic_stocks.csv")

# Re-check to see if the data is structured as required
# How many unique species are in the dataset?
print(unique(sp_obs_data$sppocean))
# The answer is 3

sp_obs_data[, "season"] <- NA
sp_obs_data$season[sp_obs_data$month %in% c(2,3,4,5)] <- 'Spring'
sp_obs_data$season[sp_obs_data$month %in% c(6,7,8)] <- 'Summer'
sp_obs_data$season[sp_obs_data$month %in% c(9,10,11,12)] <- 'Fall'

sp_obs_spring <- sp_obs_data %>% filter(season=='Spring')
# sp_obs_spring <- sp_obs_spring %>% drop_na()

lda_formula <- formula(sppocean ~ SBT.seasonal + SST.seasonal.mean + SBT.min + SBT.max + SST.max + rugosity + GRAINSIZE)
sp_lda <- lda(lda_formula, data = sp_obs_spring)
lda_values <- predict(sp_lda)

lda_df <- data.frame(lda_values$x[,1], lda_values$x[,2], sp_obs_spring$sppocean)
# Rename the columns
columns <- c("LD1", "LD2","sppocean")
colnames(lda_df) <- columns

# The actual plotting
ggplot(lda_df, aes(x=LD1, y=LD2)) +
  geom_point(aes(color = factor(sppocean)))

# For Fall

sp_obs_fall <- sp_obs_data %>% filter(season=='Fall')
lda_formula <- formula(sppocean ~ SBT.seasonal + SST.seasonal.mean + SBT.min + SBT.max + SST.max + rugosity + GRAINSIZE + GRAVEL + SAND + MUD)
sp_lda <- lda(lda_formula, data = sp_obs_fall)
lda_values <- predict(sp_lda)

lda_df <- data.frame(lda_values$x[,1], lda_values$x[,2], sp_obs_fall$sppocean)
# Rename the columns
columns <- c("LD1", "LD2","sppocean")
colnames(lda_df) <- columns

# The actual plotting
ggplot(lda_df, aes(x=LD1, y=LD2)) +
  geom_point(aes(color = factor(sppocean)))
