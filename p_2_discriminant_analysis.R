# In this file we'll perform an exploratory LDA on the output
# from 'p_1_overview.R'
# Written for R Version 3.6.1


library(tidyverse)
library(MASS)

# Read in the data
sp_obs_data <- read.csv("data/data_for_lda.csv")

# Re-check to see if the data is structured as required
# How many unique species are in the dataset?
print(unique(sp_obs_data$sppocean))
# The answer is 3

# First let's do an overall LDA on the dataset
# This will probably mean meaningless. But let's see
lda_formula <- formula(sppocean ~ SBT.seasonal + SST.seasonal.mean + SBT.min + SBT.max + SST.max + rugosity + GRAINSIZE)
sp_lda <- lda(lda_formula, data = sp_obs_data)
lda_values <- predict(sp_lda)
# Now let's plot them and see
# We have to get the discriminant values into a dataframe for this
lda_df <- data.frame(lda_values$x[,1], lda_values$x[,2], sp_obs_data$sppocean)
# Rename the columns
columns <- c("LD1", "LD2", "species")
colnames(lda_df) <- columns

# The actual plotting
ggplot(lda_df, aes(x=LD1, y=LD2)) +
  geom_point(aes(color = factor(species)))
# As you can see, when we consider all the seasons we cannot really
# discriminate between the species
# So let's start splitting up the data by season
# We have 3 seasons
get_season <- function(x){
  if (x %in% c(3,4,5)){
    return ('Spring')
  }
  else if (x %in% c(6,7,8)){
    return ('Summer')
  }
  else if (x %in% c(9,10,11,12))
    return ('Fall')
  
}

# season_list <- purrr::map(sp_obs_data$month, get_season)
