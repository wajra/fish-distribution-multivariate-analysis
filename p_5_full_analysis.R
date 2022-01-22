# In this file we'll perform the full analysis on the
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

# Assigning seasons to the observations based on the month
sp_obs_data[, "season"] <- NA
# February to June is Spring
sp_obs_data$season[sp_obs_data$month %in% c(2,3,4,5)] <- 'Spring'
# June to September is Summer
sp_obs_data$season[sp_obs_data$month %in% c(6,7,8)] <- 'Summer'
# September to January is Fall
sp_obs_data$season[sp_obs_data$month %in% c(9,10,11,12)] <- 'Fall'

# Assigning common names to the species
# First an empty column
sp_obs_data[, "spp_name"] <- NA
sp_obs_data$spp_name[sp_obs_data$sppocean == 'centropristis striata_Atl'] <- 'black sea bass'
sp_obs_data$spp_name[sp_obs_data$sppocean == 'paralichthys dentatus_Atl'] <- 'summer flounder'
sp_obs_data$spp_name[sp_obs_data$sppocean == 'stenotomus chrysops_Atl'] <- 'scup'

# We must first run a Bartlett's test for this

# First let's begin discriminator analysis for species between seasons
# LDA formula for seasons
seasons_lda_formula <- formula(season ~ SBT.seasonal + SST.seasonal.mean + SBT.min + SBT.max + SST.max + rugosity + GRAINSIZE)

# 1. Black sea bass
# Filter black sea bass observations
sp_obs_bsb <- sp_obs_data %>% filter(spp_name == 'black sea bass')

# Derive the discriminant functions
sp_lda_bsb <- lda(seasons_lda_formula, data = sp_obs_bsb)
# Apply the discriminant functions to the data to get DF scores
lda_values_bsb <- predict(sp_lda_bsb)

lda_df_bsb <- data.frame(lda_values_bsb$x[,1], lda_values_bsb$x[,2], sp_obs_bsb$season)
# Rename the columns
columns <- c("LD1", "LD2","season")
colnames(lda_df_bsb) <- columns

# Plot the discriminant functions
ggplot(lda_df_bsb, aes(x=LD1, y=LD2)) +
  geom_point(aes(color = factor(season))) +
  ggtitle("Discriminant analysis of black seas bass in different seasons")


# 2. Scup
sp_obs_scp <- sp_obs_data %>% filter(spp_name == 'scup')

# Derive the discriminant functions
sp_lda_scp <- lda(seasons_lda_formula, data = sp_obs_scp)
# Apply the discriminant functions to the data to get DF scores
lda_values_scp <- predict(sp_lda_scp)

lda_df_scp <- data.frame(lda_values_scp$x[,1], lda_values_scp$x[,2], sp_obs_scp$season)
# Rename the columns
columns <- c("LD1", "LD2","season")
colnames(lda_df_scp) <- columns

# Plot the discriminant functions
ggplot(lda_df_scp, aes(x=LD1, y=LD2)) +
  geom_point(aes(color = factor(season))) +
  ggtitle("Discriminant analysis of scup in different seasons")


# 3. Summer flounder
sp_obs_sfl <- sp_obs_data %>% filter(spp_name == 'summer flounder')

# Derive the discriminant functions
sp_lda_sfl <- lda(seasons_lda_formula, data = sp_obs_sfl)
# Apply the discriminant functions to the data to get DF scores
lda_values_sfl <- predict(sp_lda_sfl)

lda_df_sfl <- data.frame(lda_values_sfl$x[,1], lda_values_sfl$x[,2], sp_obs_sfl$season)
# Rename the columns
columns <- c("LD1", "LD2","season")
colnames(lda_df_sfl) <- columns

# Plot the discriminant functions
ggplot(lda_df_sfl, aes(x=LD1, y=LD2)) +
  geom_point(aes(color = factor(season))) +
  ggtitle("Discriminant analysis of summer flounder in different seasons")


# It seems that the discriminant functions are doing a good job of discriminating between seasons
# for individual species

# Now let's see if they can discrminate between the species in a single season
# My guess is probably not, but we'll see
# Formula for species
species_lda_formula <- formula(spp_name ~ SBT.seasonal + SST.seasonal.mean + SBT.min + SBT.max + SST.max + rugosity + GRAINSIZE)

# First let's go for fall
# Filter the season
sp_obs_fall <- sp_obs_data %>% filter(season=='Fall')
# Derive the discriminant functions
sp_lda_fall <- lda(species_lda_formula, data = sp_obs_fall)
# Apply the discriminant functions to the data to get DF scores
lda_values_fall <- predict(sp_lda_fall)

lda_df_fall <- data.frame(lda_values_fall$x[,1], lda_values_fall$x[,2], sp_obs_fall$spp_name)
# Rename the columns
columns <- c("LD1", "LD2","spp_name")
colnames(lda_df_fall) <- columns

# Plot the discriminant functions
ggplot(lda_df_fall, aes(x=LD1, y=LD2)) +
  geom_point(aes(color = factor(spp_name))) +
  ggtitle("Discriminant analysis of the three species in fall")

# Next let's go for spring
sp_obs_spring <- sp_obs_data %>% filter(season=='Spring')
# Derive the discriminant functions
sp_lda_spring <- lda(species_lda_formula, data = sp_obs_spring)
# Apply the discriminant functions to the data to get DF scores
lda_values_spring <- predict(sp_lda_spring)

lda_df_spring <- data.frame(lda_values_spring$x[,1], lda_values_spring$x[,2], sp_obs_spring$spp_name)
# Rename the columns
columns <- c("LD1", "LD2","spp_name")
colnames(lda_df_spring) <- columns

# Plot the discriminant functions
ggplot(lda_df_spring, aes(x=LD1, y=LD2)) +
  geom_point(aes(color = factor(spp_name))) +
  ggtitle("Discriminant analysis of the three species in spring")

# As can be seen from the plots, the fact that these species share similar habitats throughout the year
# is well reflected graphically through discrminant analysis


# The next stage will be adding a GLM
