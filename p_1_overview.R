# In this file, we'll read the data in and filter it
# The data is in the 'data/' folder
# Written for R version 3.6.1

library(dplyr)

# Load the data
# This is the dataset named 'hauls'
# It has all the information for individual hauls
# in surveys
load('data/master_hauls_March7_2017.RData')
# This is the dataset names 'dat'
# It has all the information for individual species
# per haul
load('data/dat_selectedspp_Feb_1_2017.Rdata')

# Species of interest
# Black sea bass – Centropristis striata
# Summer flounder - Paralichthys dentatus
# Tautog - Tautoga onitis
# Scup (Porgy) - Stenotomus chrysops

# All species names in the 'dat' file are appended
# with a regional marker
# Example - centropristis striata_Atl
# Because it's from the Atlantic Ocean

# Filtering out only the needed species
# Species that we are interested in
rec_species <- c('centropristis striata_Atl', 
                 'paralichthys dentatus_Atl',
                 'tautoga onitis_Atl',
                 'stenotomus chrysops_Atl')
# Filtering the 'dat' dataframe
# sp_data <- dat[dat$sppocean %in% rec_species, ]
sp_data <- dat %>% filter(sppocean %in% rec_species)

# Now let's group the 'sp_data' dataframe and see how many
# observations we have for each species
grouped_sp_data <- sp_data %>% group_by(sppocean)
# Now let's summarize some of the data
print(sp_data %>% count(sppocean))

# Remaining to do
# Merge 'hauls' and 'sp_data'
# Identify the continuous variables
# Conduct preliminary LDA on the data

dat <- dat[!(dat$wtcpue == 0 & dat$region == 'DFO_SoGulf'),] 
dat$wtcpue[dat$wtcpue == 0] <- 0.0002
sp_data$logwtcpue <- log(sp_data$wtcpue)

# Select only several relevant columns
sp_data <- sp_data %>% select(haulid, sppocean, Freq, wtcpue, logwtcpue)

# Now we'll merge it with hauls
# We'll use inner join
sp_haul_data <- left_join(sp_data, hauls, by='haulid')

# 
sp_not_null <- sp_haul_data %>% filter(!is.na(year))

write.csv(sp_not_null, file="data/data_for_lda.csv")