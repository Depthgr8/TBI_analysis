# Script to analyse Traumatic brain injury patient's data
# Author: Deepak Sharma (computer Programmer, JPNATC -AIIMS New Delhi)
# Start Date 7 March 2017

# Import libraries --------------------------------------------------------

require(XLConnect)
require(readODS)
require(graphics)
require(plyr)
library(Hmisc)

# Importing Traumatic Brain Injury data -----------------------------------

tbi <- loadWorkbook("TBI.xlsx")
tbi_eco <- readWorksheet(tbi, sheet = "ECONOMIC", header = TRUE)

# Setiing common parameters fr - First row, lr - Last row
fr <- 1
lr <- 128

# General Analysis --------------------------------------------------------

# Number of expired TBI patients

no_expired <- count(is.na(tbi_eco$PFU_DIRECT_COST[fr:lr]))[2,2]
noquote(paste("Number of expired patients : ", no_expired, sep=""))

# Pre follow up Analysis ---------------------------------------------------

# ================ Pre follow up Direct cost =======================

pfu_direct_cost <- tbi_eco$PFU_DIRECT_COST[fr:lr]
pfu_direct_cost <- strtoi(pfu_direct_cost)
pfu_dc_temp_table = table(pfu_direct_cost)
pfu_dc_d_frame = as.data.frame(pfu_dc_temp_table)
colnames(pfu_dc_d_frame) <- c("PFU Direct cost", "Cases")
pfu_dc_d_frame
x = as.numeric(as.vector(pfu_dc_d_frame[,1]))
y = pfu_dc_d_frame[,2]
dc_codes <- list("0 ~ 5,000 INR","5,000 ~ 10,000 INR","10,000 ~ 15,000 INR","15,000 ~ 20,000 INR")
plot(x,y,main = "Direct cost involved in the care of TBI patients before follow-up",
     xlab="PFU Direct cost codes", ylab="Cases",
     xlim=c(0,6), ylim=c(0,60),pch=16, col="blue")
text(x, y, row.names(pfu_dc_d_frame$Cases), cex=1, pos=1, col="blue",labels = dc_codes)
abline(h=seq(0,40,5),lty=5,col="lightblue")
abline(v=seq(0,5,1),lty=5,col="lightblue")

# ================ Pre follow up Indirect cost =======================

pfu_indirect_cost <- tbi_eco$PFU_INDIRECT_COST[fr:lr]
pfu_indirect_cost <- strtoi(pfu_indirect_cost)
pfu_idc_temp_table = table(pfu_indirect_cost)
pfu_idc_d_frame = as.data.frame(pfu_idc_temp_table)
colnames(pfu_idc_d_frame) <- c("PFU Indirect cost", "Cases")
pfu_idc_d_frame
x = as.numeric(as.vector(pfu_idc_d_frame[,1]))
y = pfu_idc_d_frame[,2]
idc_codes <- list("Nil","0 ~ 5,000 INR","5,000 ~ 10,000 INR","10,000 ~ 15,000 INR","15,000 ~ 20,000 INR")
plot(x,y,main = "Indirect cost up involved in the care of TBI patients before follow-up",
     xlab="PFU indirect cost codes", ylab="Cases",
     xlim=c(0,6), ylim=c(0,60),pch=16, col="blue")
text(x, y, row.names(pfu_idc_d_frame$Cases), cex=1, pos=1, col="blue",labels = idc_codes)
abline(h=seq(0,40,5),lty=5,col="lightblue")
abline(v=seq(0,5,1),lty=5,col="lightblue")

# ================ Pre follow up Total expenditure =======================

pfu_total_expenditure <- tbi_eco$PFU_TOTAL_EXPENDITURE[fr:lr]
pfu_total_expenditure <- strtoi(pfu_total_expenditure)
pfu_te_temp_table = table(pfu_total_expenditure)
pfu_te_d_frame = as.data.frame(pfu_te_temp_table)
colnames(pfu_te_d_frame) <- c("PFU Total expenditure", "Cases")
pfu_te_d_frame
x = as.numeric(as.vector(pfu_te_d_frame[,1]))
y = pfu_te_d_frame[,2]
te_codes <- list("0 ~ 20,000 INR","20,000 ~ 40,000 INR","40,000 ~ 60,000 INR","Above 60,000 INR")
plot(x,y,main = "Total expenditure involved in the care of TBI patients before follow-up",
     xlab="PFU total expenditure codes", ylab="Cases",
     xlim=c(0,6), ylim=c(0,60),pch=16, col="blue")
text(x, y, row.names(pfu_te_d_frame$Cases), cex=1, pos=1, col="blue",labels = te_codes)
abline(h=seq(0,40,5),lty=5,col="lightblue")
abline(v=seq(0,5,1),lty=5,col="lightblue")


# Follow up analysis ------------------------------------------------------
# ================ 1st follow up Direct cost =======================

direct_cost_fu1 <- tbi_eco$FU1_DIRECT.COST[fr:lr]
direct_cost_fu1 <- strtoi(direct_cost_fu1)
dc_temp_table = table(direct_cost_fu1)
dc_d_frame = as.data.frame(dc_temp_table)
colnames(dc_d_frame) <- c("Direct cost", "Cases")
dc_d_frame
x = as.numeric(dc_d_frame[,1])
y = dc_d_frame[,2]
dc_codes <- list("Nil","Between 0 and 10,000 INR","Between 20,000 and 30,000 INR","Above 30,000 INR")
plot(x,y,main = "Analysis of direct cost involved in the care of TBI patients",
     xlab="Direct cost codes", ylab="Cases",
     xlim=c(min(x), max(x)), ylim=c(min(y), max(y)),pch=16, col="blue")
text(x, y, row.names(dc_d_frame$Cases), cex=1, pos=2, col="blue",labels = dc_codes)
abline(h=seq(0,40,5),lty=5,col="lightblue")
abline(v=seq(0,5,1),lty=5,col="lightblue")

# ================ 2nd follow up Direct cost =======================

direct_cost_fu2 <- tbi_eco$FU2_DIRECT.COST[fr:lr]
direct_cost_fu2 <- strtoi(direct_cost_fu2)
dc_temp_table = table(direct_cost_fu2)
dc_d_frame = as.data.frame(dc_temp_table)
colnames(dc_d_frame) <- c("Direct cost", "Cases")
dc_d_frame
x = as.numeric(dc_d_frame[,1])
y = dc_d_frame[,2]
dc_codes <- list("Nil","Between 0 and 10,000 INR","Between 20,000 and 30,000 INR","Above 30,000 INR")
plot(x,y,main = "Analysis of direct cost involved in the care of TBI patients",
     xlab="Direct cost codes", ylab="Cases",
     xlim=c(min(x), max(x)), ylim=c(min(y), max(y)),pch=16, col="blue")
text(x, y, row.names(dc_d_frame$Cases), cex=1, pos=2, col="blue",labels = dc_codes)
abline(h=seq(0,40,5),lty=5,col="lightblue")
abline(v=seq(0,5,1),lty=5,col="lightblue")

# ================ 3rd follow up Direct cost =======================

direct_cost_fu3 <- tbi_eco$FU3_DIRECT.COST[fr:lr]
direct_cost_fu3 <- strtoi(direct_cost_fu3)
dc_temp_table = table(direct_cost_fu3)
dc_d_frame = as.data.frame(dc_temp_table)
colnames(dc_d_frame) <- c("Direct cost", "Cases")
dc_d_frame
x = as.numeric(dc_d_frame[,1])
y = dc_d_frame[,2]
dc_codes <- list("Nil","Between 0 and 10,000 INR","Between 20,000 and 30,000 INR","Above 30,000 INR")
plot(x,y,main = "Analysis of direct cost involved in the care of TBI patients",
     xlab="Direct cost codes", ylab="Cases",
     xlim=c(min(x), max(x)), ylim=c(min(y), max(y)),pch=16, col="blue")
text(x, y, row.names(dc_d_frame$Cases), cex=1, pos=2, col="blue",labels = dc_codes)
abline(h=seq(0,40,5),lty=5,col="lightblue")
abline(v=seq(0,5,1),lty=5,col="lightblue")

# ================ 4th follow up Direct cost =======================

direct_cost_fu4 <- tbi_eco$FU4_DIRECT.COST[fr:lr]
direct_cost_fu4 <- strtoi(direct_cost_fu4)
dc_temp_table = table(direct_cost_fu4)
dc_d_frame = as.data.frame(dc_temp_table)
colnames(dc_d_frame) <- c("Direct cost", "Cases")
dc_d_frame
x = as.numeric(dc_d_frame[,1])
y = dc_d_frame[,2]
dc_codes <- list("Nil","Between 0 and 10,000 INR","Between 20,000 and 30,000 INR","Above 30,000 INR")
plot(x,y,main = "Analysis of direct cost involved in the care of TBI patients",
     xlab="Direct cost codes", ylab="Cases",
     xlim=c(min(x), max(x)), ylim=c(min(y), max(y)),pch=16, col="blue")
text(x, y, row.names(dc_d_frame$Cases), cex=1, pos=2, col="blue",labels = dc_codes)
abline(h=seq(0,40,5),lty=5,col="lightblue")
abline(v=seq(0,5,1),lty=5,col="lightblue")

# ================ 5th follow up Direct cost =======================

direct_cost_fu5 <- tbi_eco$FU5_DIRECT.COST[fr:lr]
direct_cost_fu5 <- strtoi(direct_cost_fu5)
dc_temp_table = table(direct_cost_fu5)
dc_d_frame = as.data.frame(dc_temp_table)
colnames(dc_d_frame) <- c("Direct cost", "Cases")
dc_d_frame
x = as.numeric(dc_d_frame[,1])
y = dc_d_frame[,2]
dc_codes <- list("Nil","Between 0 and 10,000 INR","Between 20,000 and 30,000 INR","Above 30,000 INR")
plot(x,y,main = "Analysis of direct cost involved in the care of TBI patients",
     xlab="Direct cost codes", ylab="Cases",
     xlim=c(min(x), max(x)), ylim=c(min(y), max(y)),pch=16, col="blue")
text(x, y, row.names(dc_d_frame$Cases), cex=1, pos=2, col="blue",labels = dc_codes)
abline(h=seq(0,40,5),lty=5,col="lightblue")
abline(v=seq(0,5,1),lty=5,col="lightblue")

# ================ 1st follow up Indirect cost =======================

indirect_cost_fu1 <- tbi_eco$FU1_INDIRECT.COST[fr:lr]
indirect_cost_fu1 <- strtoi(indirect_cost_fu1)
idc_temp_table = table(indirect_cost_fu1)
idc_d_frame = as.data.frame(idc_temp_table)
colnames(idc_d_frame) <- c("Indirect cost", "Cases")
idc_d_frame
x = as.numeric(as.vector(idc_d_frame[,1]))
y = idc_d_frame[,2]
idc_codes <- list("Nil","Between 0 and 10,000 INR","Between 10,000 to 20,000 INR","Between 20,000 and 30,000 INR","Above 30,000 INR")
plot(x,y,main = "Analysis of indirect cost involved in the care of TBI patients",
     xlab="Indirect cost codes", ylab="Cases",
     xlim=c(min(x), max(x)), ylim=c(min(y), max(y)),pch=16, col="blue")
text(x, y, row.names(idc_d_frame$Cases), cex=1, pos=2, col="blue",labels = idc_codes)
abline(h=seq(0,70,5),lty=5,col="lightblue")
abline(v=seq(0,5,1),lty=5,col="lightblue")

# ================ 2nd follow up Indirect cost =======================

indirect_cost_fu2 <- tbi_eco$FU2_INDIRECT.COST[fr:lr]
indirect_cost_fu2 <- strtoi(indirect_cost_fu2)
idc_temp_table = table(indirect_cost_fu2)
idc_d_frame = as.data.frame(idc_temp_table)
colnames(idc_d_frame) <- c("Indirect cost", "Cases")
idc_d_frame
x = as.numeric(as.vector(idc_d_frame[,1]))
y = idc_d_frame[,2]
idc_codes <- list("Nil","Between 0 and 10,000 INR","Between 10,000 to 20,000 INR","Between 20,000 and 30,000 INR","Above 30,000 INR")
plot(x,y,main = "Analysis of indirect cost involved in the care of TBI patients",
     xlab="Indirect cost codes", ylab="Cases",
     xlim=c(min(x), max(x)), ylim=c(min(y), max(y)),pch=16, col="blue")
text(x, y, row.names(idc_d_frame$Cases), cex=1, pos=2, col="blue",labels = idc_codes)
abline(h=seq(0,70,5),lty=5,col="lightblue")
abline(v=seq(0,5,1),lty=5,col="lightblue")

# ================ 3rd follow up Indirect cost =======================

indirect_cost_fu3 <- tbi_eco$FU3_INDIRECT.COST[fr:lr]
indirect_cost_fu3 <- strtoi(indirect_cost_fu3)
idc_temp_table = table(indirect_cost_fu3)
idc_d_frame = as.data.frame(idc_temp_table)
colnames(idc_d_frame) <- c("Indirect cost", "Cases")
idc_d_frame
x = as.numeric(as.vector(idc_d_frame[,1]))
y = idc_d_frame[,2]
idc_codes <- list("Nil","Between 0 and 10,000 INR","Between 10,000 to 20,000 INR","Between 20,000 and 30,000 INR","Above 30,000 INR")
plot(x,y,main = "Analysis of indirect cost involved in the care of TBI patients",
     xlab="Indirect cost codes", ylab="Cases",
     xlim=c(min(x), max(x)), ylim=c(min(y), max(y)),pch=16, col="blue")
text(x, y, row.names(idc_d_frame$Cases), cex=1, pos=2, col="blue",labels = idc_codes)
abline(h=seq(0,70,5),lty=5,col="lightblue")
abline(v=seq(0,5,1),lty=5,col="lightblue")

# ================ 4th follow up Indirect cost =======================

indirect_cost_fu4 <- tbi_eco$FU4_INDIRECT.COST[fr:lr]
indirect_cost_fu4 <- strtoi(indirect_cost_fu4)
idc_temp_table = table(indirect_cost_fu4)
idc_d_frame = as.data.frame(idc_temp_table)
colnames(idc_d_frame) <- c("Indirect cost", "Cases")
idc_d_frame
x = as.numeric(as.vector(idc_d_frame[,1]))
y = idc_d_frame[,2]
idc_codes <- list("Nil","Between 0 and 10,000 INR","Between 10,000 to 20,000 INR","Between 20,000 and 30,000 INR","Above 30,000 INR")
plot(x,y,main = "Analysis of indirect cost involved in the care of TBI patients",
     xlab="Indirect cost codes", ylab="Cases",
     xlim=c(min(x), max(x)), ylim=c(min(y), max(y)),pch=16, col="blue")
text(x, y, row.names(idc_d_frame$Cases), cex=1, pos=2, col="blue",labels = idc_codes)
abline(h=seq(0,70,5),lty=5,col="lightblue")
abline(v=seq(0,5,1),lty=5,col="lightblue")

# ================ 5th follow up Indirect cost =======================

indirect_cost_fu5 <- tbi_eco$FU5_INDIRECT.COST[fr:lr]
indirect_cost_fu5 <- strtoi(indirect_cost_fu5)
idc_temp_table = table(indirect_cost_fu5)
idc_d_frame = as.data.frame(idc_temp_table)
colnames(idc_d_frame) <- c("Indirect cost", "Cases")
idc_d_frame
x = as.numeric(as.vector(idc_d_frame[,1]))
y = idc_d_frame[,2]
idc_codes <- list("Nil","Between 0 and 10,000 INR","Between 10,000 to 20,000 INR","Between 20,000 and 30,000 INR","Above 30,000 INR")
plot(x,y,main = "Analysis of indirect cost involved in the care of TBI patients",
     xlab="Indirect cost codes", ylab="Cases",
     xlim=c(min(x), max(x)), ylim=c(min(y), max(y)),pch=16, col="blue")
text(x, y, row.names(idc_d_frame$Cases), cex=1, pos=2, col="blue",labels = idc_codes)
abline(h=seq(0,70,5),lty=5,col="lightblue")
abline(v=seq(0,5,1),lty=5,col="lightblue")

# ================ Analysis of Quality of life of TBI patients =======================


#   Results:      One twenty eight patients and their families were interviewed. On average, patients with severe, moderate and minor TBI incurred direct costs at USD 2,365, USD 1,390 and USD 849, with time lost for normal activities averaging 54 weeks, 26 weeks and 17 weeks and years lived with disability (YLD) of 0.46, 0.25 and 0.15 year, respectively.
#   Conclusion:   All three component costs of TBI were high; the direct cost accounted for the largest proportion, with costs rising with the severity of TBI. The results suggest that the burden of TBI can be catastrophic for families because of high direct costs, significant time off work for patients and caregivers, and impact on heal
