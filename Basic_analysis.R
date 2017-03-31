# R Script to analyse Traumatic brain injury patient's data
# Author: Deepak Sharma (computer Programmer, JPNATC -AIIMS New Delhi)
# Start Date 31 March 2017

# Import libraries --------------------------------------------------------

require(XLConnect)
require(graphics)
require(plyr)

# Importing Traumatic Brain Injury data -----------------------------------

getwd()
tbi <- loadWorkbook("TBI.xlsx")
tbi_gen <- readWorksheet(tbi, sheet = "ECONOMIC", header = TRUE)

# Setiing common parameters fr - First row, lr - Last row
fr <- 1
lr <- nrow(tbi_gen)

# General Analysis --------------------------------------------------------

# Number of expired TBI patients

no_expired <- count(is.na(tbi_gen$PFU_DIRECT_COST[fr:lr]))[2,2]
death_percent <- no_expired/lr
noquote(paste("Out of ",lr, " patients (with traumatic brain injuries) ", death_percent * 100, "% were expired.", sep=""))
