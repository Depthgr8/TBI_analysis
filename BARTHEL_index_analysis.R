# R Script to analyse Traumatic brain injury patient's BARTHEL index
# Author: Deepak Sharma (computer Programmer, JPNATC -AIIMS New Delhi)
# Start Date 31 March 2017

# Import libraries --------------------------------------------------------

require(XLConnect)
require(graphics)
require(plyr)

# Importing Traumatic Brain Injury data -----------------------------------

getwd()
tbi <- loadWorkbook("TBI.xlsx")
tbi_barthel <- readWorksheet(tbi, sheet = "BARTHEL", header = TRUE)


# GOSE analysis -----------------------------------------------------------

fr <- 1
lr <- 128

# Follow up 1 -------------------------------------------------------------

fu_barthel <- strtoi(tbi_barthel$FU1_BI[fr:lr])
fu_barthel_frame = as.data.frame(table(fu_barthel))
colnames(fu_barthel_frame) <- c("Barthel Index", "Cases")
fu_barthel_frame
x = as.numeric(as.vector(fu_barthel_frame[,1]))
y = fu_barthel_frame[,2]
barthel_codes <- list("")
png(filename="~/AIIMS work/TBI_analysis/plots/barthel/barthel_fu1.png",width = 1200, height = 800)
par(mfrow=c(2,1))
plot(x,y,main = "Barthel index for follow up 1",type='o',
     xlab="Barthel index", ylab="Cases",
     xlim=c(min(x)-1, max(x)+1), ylim=c(min(y)-10, max(y)+10),pch=16, col="blue")
abline(h=seq(0,40,1),lty=5,col="lightgray")
abline(v=seq(0,30,1),lty=5,col="lightgray")
text(x, y, row.names(fu_barthel_frame$Cases), cex=1.02, pos=3, col="blue",labels = barthel_codes)
pie(y,x)
dev.off()

# Follow up 2 -------------------------------------------------------------

fu_barthel <- strtoi(tbi_barthel$FU2_BI[fr:lr])
fu_barthel_frame = as.data.frame(table(fu_barthel))
colnames(fu_barthel_frame) <- c("Barthel Index", "Cases")
fu_barthel_frame
x = as.numeric(as.vector(fu_barthel_frame[,1]))
y = fu_barthel_frame[,2]
barthel_codes <- list("")
png(filename="~/AIIMS work/TBI_analysis/plots/barthel/barthel_fu2.png",width = 1200, height = 800)
par(mfrow=c(2,1))
plot(x,y,main = "Barthel index for follow up 2",type='o',
     xlab="Barthel index", ylab="Cases",
     xlim=c(min(x)-1, max(x)+1), ylim=c(min(y)-10, max(y)+10),pch=16, col="blue")
abline(h=seq(0,40,1),lty=5,col="lightgray")
abline(v=seq(0,30,1),lty=5,col="lightgray")
text(x, y, row.names(fu_barthel_frame$Cases), cex=1.02, pos=3, col="blue",labels = barthel_codes)
pie(y,x)
dev.off()

# Follow up 3 -------------------------------------------------------------

fu_barthel <- strtoi(tbi_barthel$FU3_BI[fr:lr])
fu_barthel_frame = as.data.frame(table(fu_barthel))
colnames(fu_barthel_frame) <- c("Barthel Index", "Cases")
fu_barthel_frame
x = as.numeric(as.vector(fu_barthel_frame[,1]))
y = fu_barthel_frame[,2]
barthel_codes <- list("")
png(filename="~/AIIMS work/TBI_analysis/plots/barthel/barthel_fu3.png",width = 1200, height = 800)
par(mfrow=c(2,1))
plot(x,y,main = "Barthel index for follow up 3",type='o',
     xlab="Barthel index", ylab="Cases",
     xlim=c(min(x)-1, max(x)+1), ylim=c(min(y)-10, max(y)+10),pch=16, col="blue")
abline(h=seq(0,40,1),lty=5,col="lightgray")
abline(v=seq(0,30,1),lty=5,col="lightgray")
text(x, y, row.names(fu_barthel_frame$Cases), cex=1.02, pos=3, col="blue",labels = barthel_codes)
pie(y,x)
dev.off()

# Follow up 4 -------------------------------------------------------------

fu_barthel <- strtoi(tbi_barthel$FU4_BI[fr:lr])
fu_barthel_frame = as.data.frame(table(fu_barthel))
colnames(fu_barthel_frame) <- c("Barthel Index", "Cases")
fu_barthel_frame
x = as.numeric(as.vector(fu_barthel_frame[,1]))
y = fu_barthel_frame[,2]
barthel_codes <- list("")
png(filename="~/AIIMS work/TBI_analysis/plots/barthel/barthel_fu4.png",width = 1200, height = 800)
par(mfrow=c(2,1))
plot(x,y,main = "Barthel index for follow up 4",type='o',
     xlab="Barthel index", ylab="Cases",
     xlim=c(min(x)-1, max(x)+1), ylim=c(min(y)-10, max(y)+10),pch=16, col="blue")
abline(h=seq(0,40,1),lty=5,col="lightgray")
abline(v=seq(0,30,1),lty=5,col="lightgray")
text(x, y, row.names(fu_barthel_frame$Cases), cex=1.02, pos=3, col="blue",labels = barthel_codes)
pie(y,x)
dev.off()

# Follow up 5 -------------------------------------------------------------

fu_barthel <- strtoi(tbi_barthel$FU5_BI[fr:lr])
fu_barthel_frame = as.data.frame(table(fu_barthel))
colnames(fu_barthel_frame) <- c("Barthel Index", "Cases")
fu_barthel_frame
x = as.numeric(as.vector(fu_barthel_frame[,1]))
y = fu_barthel_frame[,2]
barthel_codes <- list("")
png(filename="~/AIIMS work/TBI_analysis/plots/barthel/barthel_fu5.png",width = 1200, height = 800)
par(mfrow=c(2,1))
plot(x,y,main = "Barthel index for follow up 5",type='o',
     xlab="Barthel index", ylab="Cases",
     xlim=c(min(x)-1, max(x)+1), ylim=c(min(y)-10, max(y)+10),pch=16, col="blue")
abline(h=seq(0,40,1),lty=5,col="lightgray")
abline(v=seq(0,30,1),lty=5,col="lightgray")
text(x, y, row.names(fu_barthel_frame$Cases), cex=1.02, pos=3, col="blue",labels = barthel_codes)
pie(y,x)
dev.off()

# Follow up 6 -------------------------------------------------------------

fu_barthel <- strtoi(tbi_barthel$FU6_BI[fr:lr])
fu_barthel_frame = as.data.frame(table(fu_barthel))
colnames(fu_barthel_frame) <- c("Barthel Index", "Cases")
fu_barthel_frame
x = as.numeric(as.vector(fu_barthel_frame[,1]))
y = fu_barthel_frame[,2]
barthel_codes <- list("")
png(filename="~/AIIMS work/TBI_analysis/plots/barthel/barthel_fu6.png",width = 1200, height = 800)
par(mfrow=c(2,1))
plot(x,y,main = "Barthel index for follow up 6",type='o',
     xlab="Barthel index", ylab="Cases",
     xlim=c(min(x)-1, max(x)+1), ylim=c(min(y)-10, max(y)+10),pch=16, col="blue")
abline(h=seq(0,40,1),lty=5,col="lightgray")
abline(v=seq(0,30,1),lty=5,col="lightgray")
text(x, y, row.names(fu_barthel_frame$Cases), cex=1.02, pos=3, col="blue",labels = barthel_codes)
pie(y,x)
dev.off()

# Follow up 2 -------------------------------------------------------------

fu_barthel <- strtoi(tbi_barthel$FU2_BI[fr:lr])
fu_barthel_frame = as.data.frame(table(fu_barthel))
colnames(fu_barthel_frame) <- c("Barthel Index", "Cases")
fu_barthel_frame
x = as.numeric(as.vector(fu_barthel_frame[,1]))
y = fu_barthel_frame[,2]
barthel_codes <- list("")
png(filename="~/AIIMS work/TBI_analysis/plots/barthel/barthel_fu2.png",width = 1200, height = 800)
par(mfrow=c(2,1))
plot(x,y,main = "Barthel index for follow up 2",type='o',
     xlab="Barthel index", ylab="Cases",
     xlim=c(min(x)-1, max(x)+1), ylim=c(min(y)-10, max(y)+10),pch=16, col="blue")
abline(h=seq(0,40,1),lty=5,col="lightgray")
abline(v=seq(0,30,1),lty=5,col="lightgray")
text(x, y, row.names(fu_barthel_frame$Cases), cex=1.02, pos=3, col="blue",labels = barthel_codes)
pie(y,x)
dev.off()