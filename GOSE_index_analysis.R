# R Script to analyse Traumatic brain injury patient's GOSE index
# Author: Deepak Sharma (computer Programmer, JPNATC -AIIMS New Delhi)
# Start Date 31 March 2017

# Import libraries --------------------------------------------------------

require(XLConnect)
require(graphics)
require(plyr)

# Importing Traumatic Brain Injury data -----------------------------------

getwd()
tbi <- loadWorkbook("TBI.xlsx")
tbi_gose <- readWorksheet(tbi, sheet = "GOSE", header = TRUE)

# GOSE analysis -----------------------------------------------------------

fr <- 1
lr <- 128

# Follow up 1 -------------------------------------------------------------

fu_gose <- strtoi(tbi_gose$FU1_GI[fr:lr])
fu_gose_frame = as.data.frame(table(fu_gose))
colnames(fu_gose_frame) <- c("Gose Index", "Cases")
fu_gose_frame
x = as.numeric(as.vector(fu_gose_frame[,1]))
y = fu_gose_frame[,2]
gose_codes <- list("")
png(filename="~/AIIMS work/TBI_analysis/plots/gose/gose_fu1.png",width = 1200, height = 800)
par(mfrow=c(2,1))
plot(x,y,main = "Gose index for follow up 1",type='o',
     xlab="Gose index", ylab="Cases",
     xlim=c(min(x)-1, max(x)+1), ylim=c(min(y)-10, max(y)+10),pch=16, col="blue")
abline(h=seq(0,40,1),lty=5,col="lightgray")
abline(v=seq(0,30,1),lty=5,col="lightgray")
text(x, y, row.names(fu_gose_frame$Cases), cex=1.02, pos=3, col="blue",labels = gose_codes)
pie(y,x)
dev.off()

# Follow up 2 -------------------------------------------------------------

fu_gose <- strtoi(tbi_gose$FU2_GI[fr:lr])
fu_gose_frame = as.data.frame(table(fu_gose))
colnames(fu_gose_frame) <- c("Gose Index", "Cases")
fu_gose_frame
x = as.numeric(as.vector(fu_gose_frame[,1]))
y = fu_gose_frame[,2]
gose_codes <- list("")
png(filename="~/AIIMS work/TBI_analysis/plots/gose/gose_fu2.png",width = 1200, height = 800)
par(mfrow=c(2,1))
plot(x,y,main = "Gose index for follow up 2",type='o',
     xlab="Gose index", ylab="Cases",
     xlim=c(min(x)-1, max(x)+1), ylim=c(min(y)-10, max(y)+10),pch=16, col="blue")
abline(h=seq(0,40,1),lty=5,col="lightgray")
abline(v=seq(0,30,1),lty=5,col="lightgray")
text(x, y, row.names(fu_gose_frame$Cases), cex=1.02, pos=3, col="blue",labels = gose_codes)
pie(y,x)
dev.off()

# Follow up 3 -------------------------------------------------------------

fu_gose <- strtoi(tbi_gose$FU3_GI[fr:lr])
fu_gose_frame = as.data.frame(table(fu_gose))
colnames(fu_gose_frame) <- c("Gose Index", "Cases")
fu_gose_frame
x = as.numeric(as.vector(fu_gose_frame[,1]))
y = fu_gose_frame[,2]
gose_codes <- list("")
png(filename="~/AIIMS work/TBI_analysis/plots/gose/gose_fu3.png",width = 1200, height = 800)
par(mfrow=c(2,1))
plot(x,y,main = "Gose index for follow up 3",type='o',
     xlab="Gose index", ylab="Cases",
     xlim=c(min(x)-1, max(x)+1), ylim=c(min(y)-10, max(y)+10),pch=16, col="blue")
abline(h=seq(0,40,1),lty=5,col="lightgray")
abline(v=seq(0,30,1),lty=5,col="lightgray")
text(x, y, row.names(fu_gose_frame$Cases), cex=1.02, pos=3, col="blue",labels = gose_codes)
pie(y,x)
dev.off()

# Follow up 4 -------------------------------------------------------------

fu_gose <- strtoi(tbi_gose$FU4_GI[fr:lr])
fu_gose_frame = as.data.frame(table(fu_gose))
colnames(fu_gose_frame) <- c("Gose Index", "Cases")
fu_gose_frame
x = as.numeric(as.vector(fu_gose_frame[,1]))
y = fu_gose_frame[,2]
gose_codes <- list("")
png(filename="~/AIIMS work/TBI_analysis/plots/gose/gose_fu4.png",width = 1200, height = 800)
par(mfrow=c(2,1))
plot(x,y,main = "Gose index for follow up 4",type='o',
     xlab="Gose index", ylab="Cases",
     xlim=c(min(x)-1, max(x)+1), ylim=c(min(y)-10, max(y)+10),pch=16, col="blue")
abline(h=seq(0,40,1),lty=5,col="lightgray")
abline(v=seq(0,30,1),lty=5,col="lightgray")
text(x, y, row.names(fu_gose_frame$Cases), cex=1.02, pos=3, col="blue",labels = gose_codes)
pie(y,x)
dev.off()

# Follow up 5 -------------------------------------------------------------

fu_gose <- strtoi(tbi_gose$FU5_GI[fr:lr])
fu_gose_frame = as.data.frame(table(fu_gose))
colnames(fu_gose_frame) <- c("Gose Index", "Cases")
fu_gose_frame
x = as.numeric(as.vector(fu_gose_frame[,1]))
y = fu_gose_frame[,2]
gose_codes <- list("")
png(filename="~/AIIMS work/TBI_analysis/plots/gose/gose_fu5.png",width = 1200, height = 800)
par(mfrow=c(2,1))
plot(x,y,main = "Gose index for follow up 5",type='o',
     xlab="Gose index", ylab="Cases",
     xlim=c(min(x)-1, max(x)+1), ylim=c(min(y)-10, max(y)+10),pch=16, col="blue")
abline(h=seq(0,40,1),lty=5,col="lightgray")
abline(v=seq(0,30,1),lty=5,col="lightgray")
text(x, y, row.names(fu_gose_frame$Cases), cex=1.02, pos=3, col="blue",labels = gose_codes)
pie(y,x)
dev.off()

# Follow up 6 -------------------------------------------------------------

fu_gose <- strtoi(tbi_gose$FU6_GI[fr:lr])
fu_gose_frame = as.data.frame(table(fu_gose))
colnames(fu_gose_frame) <- c("Gose Index", "Cases")
fu_gose_frame
x = as.numeric(as.vector(fu_gose_frame[,1]))
y = fu_gose_frame[,2]
gose_codes <- list("")
png(filename="~/AIIMS work/TBI_analysis/plots/gose/gose_fu6.png",width = 1200, height = 800)
par(mfrow=c(2,1))
plot(x,y,main = "Gose index for follow up 6",type='o',
     xlab="Gose index", ylab="Cases",
     xlim=c(min(x)-1, max(x)+1), ylim=c(min(y)-10, max(y)+10),pch=16, col="blue")
abline(h=seq(0,40,1),lty=5,col="lightgray")
abline(v=seq(0,30,1),lty=5,col="lightgray")
text(x, y, row.names(fu_gose_frame$Cases), cex=1.02, pos=3, col="blue",labels = gose_codes)
pie(y,x)
dev.off()