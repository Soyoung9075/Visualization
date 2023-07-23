
library(tidyverse)
library(readxl)

setwd("G:/내 드라이브/취업/메디크로/plot")

############### Female #############
df <- read_excel("cifplot.xlsx", sheet = "female")

######### female no adjustment#########
# color 버전
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence, type = "l", col = 'blue',
     xlab = "Year", ylab = "Cumulative Incidence", main = "Cumulative Incidence for Female", lwd = 1.5,
     cex.axis=0.8)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence, col = 'red', lwd = 1.5)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), col = c("blue", "red"), 
       lty = c(1,1),lwd = c(1.2, 1.2),cex = 0.8)

# 점선버전1
png("female_ver1.png",width=609,height=482,res=100)
range <- c(min(df$Incidence) ,max(df$Incidence))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence", main = "Cumulative Incidence for Female", lwd = 1.5,
     cex.axis=0.8, ylim = range)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8)
text(x = 8.5, y = 0.002,                # First text
     "Grey's Test p=0.0019", cex = 0.8)
dev.off()

# 점선버전2
png("female_ver2.png",width=609,height=482,res=100)
range <- c(min(df$Incidence) ,max(df$Incidence))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence", main = "Cumulative Incidence for Female", lwd = 1.5,
     cex.axis=0.8, ylim = range)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8, ncol = 2)
text(x = 1.2, y = max(df$Incidence)-0.005,                # First text
     "Grey's Test p=0.0019", cex = 0.8)
dev.off()

######### female after adjustment ########

# 버전1
png("female_ver1.png",width=654,height=482,res=100)
par(mar = c(5, 5, 3, 3))
range <- c(min(df$Incidence_adj) ,max(df$Incidence_adj))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence_adj, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence (100,000 PY)", 
     main = "Cumulative Incidence for Female", lwd = 1.5,
     cex.axis=0.8, cex.lab=0.9, ylim = range, yaxt = "n")
options(scipen = 10, digits = 2)
seq <- seq(min(df$Incidence_adj) ,max(df$Incidence_adj), by = 1000)
axis(2, at = seq,labels = format(seq, big.mark = ","))
par(cex.axis = 0.8)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence_adj,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8)
text(x = 8.5, y = 100,            
     "Grey's Test : p=0.0019", cex = 0.8)
dev.off()

# 버전2
png("female_ver2.png",width=654,height=482,res=100)
par(mar = c(5, 5, 3, 3))
range <- c(min(df$Incidence_adj) ,max(df$Incidence_adj))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence_adj, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence (100,000 PY)", 
     main = "Cumulative Incidence for Female", lwd = 1.5,
     cex.axis=0.8, cex.lab=0.9, ylim = range, yaxt = "n")
options(scipen = 10, digits = 2)
seq <- seq(min(df$Incidence_adj) ,max(df$Incidence_adj), by = 1000)
axis(2, at = seq,labels = format(seq, big.mark = ","))
par(cex.axis = 0.8)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence_adj,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8, ncol = 2)
text(x = 1.2, y = max(df$Incidence_adj)*0.89,                # First text
     "Grey's Test : p=0.0019", cex = 0.8)
dev.off()

############ male ###############
df <- read_excel("cifplot.xlsx", sheet = "male")

######### male no adjustment ########
# 점선버전1
png("male_ver1.png",width=609,height=482,res=100)
range <- c(min(df$Incidence) ,max(df$Incidence))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence", main = "Cumulative Incidence for Male", lwd = 1.5,
     cex.axis=0.8, ylim = range)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8)
text(x = 8.5, y = 0.002,                # First text
     "Grey's Test : p <.0001", cex = 0.8)
dev.off()

# 점선버전2
png("male_ver2.png",width=609,height=482,res=100)
range <- c(min(df$Incidence) ,max(df$Incidence))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence", main = "Cumulative Incidence for Male", lwd = 1.5,
     cex.axis=0.8, ylim = range)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8, ncol = 2)
text(x = 1.2, y = max(df$Incidence)-0.005,                # First text
     "Grey's Test : p <.0001", cex = 0.8)
dev.off()

########### male after adjustment #############

png("male_ver1.png",width=654,height=482,res=100)
par(mar = c(5, 5, 3, 3))
range <- c(min(df$Incidence_adj) ,max(df$Incidence_adj))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence_adj, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence (100,000 PY)", 
     main = "Cumulative Incidence for Male", lwd = 1.5,
     cex.axis=0.8, cex.lab=0.9, ylim = range, yaxt = "n")
options(scipen = 10, digits = 2)
seq <- seq(min(df$Incidence_adj) ,max(df$Incidence_adj), by = 1000)
axis(2, at = seq,labels = format(seq, big.mark = ","))
par(cex.axis = 0.8)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence_adj,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8)
text(x = 8.5, y = 100,            
     "Grey's Test : p <.0001", cex = 0.8)
dev.off()

# 버전2
png("male_ver2.png",width=654,height=482,res=100)
par(mar = c(5, 5, 3, 3))
range <- c(min(df$Incidence_adj) ,max(df$Incidence_adj))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence_adj, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence (100,000 PY)", 
     main = "Cumulative Incidence for Male", lwd = 1.5,
     cex.axis=0.8, cex.lab=0.9, ylim = range, yaxt = "n")
options(scipen = 10, digits = 2)
seq <- seq(min(df$Incidence_adj) ,max(df$Incidence_adj), by = 1000)
axis(2, at = seq,labels = format(seq, big.mark = ","))
par(cex.axis = 0.8)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence_adj,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8, ncol = 2)
text(x = 1.2, y = max(df$Incidence_adj)*0.89,                # First text
     "Grey's Test : p <.0001", cex = 0.8)
dev.off()



############ AGE_UNDER_60 ###############
df <- read_excel("cifplot.xlsx", sheet = "age_under_60")

######### age_under_60 no adjustment ########
# 점선버전1
png("age_under_60_ver1.png",width=609,height=482,res=100)
range <- c(min(df$Incidence) ,max(df$Incidence))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence", main = "Cumulative Incidence for Age under 60", lwd = 1.5,
     cex.axis=0.8, ylim = range)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8)
text(x = 8.5, y = 0.002,                # First text
     "Grey's Test : p <.0001", cex = 0.8)
dev.off()

# 점선버전2
png("age_under_60_ver2.png",width=609,height=482,res=100)
range <- c(min(df$Incidence) ,max(df$Incidence))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence", main = "Cumulative Incidence for Age under 60", lwd = 1.5,
     cex.axis=0.8, ylim = range)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8, ncol = 2)
text(x = 1.2, y = max(df$Incidence)-0.0035,                # First text
     "Grey's Test : p <.0001", cex = 0.8)
dev.off()

########### age_under_60 after adjustment #############
# 버전1
png("age_under_60_ver1.png",width=654,height=482,res=100)
par(mar = c(5, 5, 3, 3))
range <- c(min(df$Incidence_adj) ,max(df$Incidence_adj))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence_adj, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence (100,000 PY)", 
     main = "Cumulative Incidence for Age under 60", lwd = 1.5,
     cex.axis=0.8, cex.lab=0.9, ylim = range, yaxt = "n")
options(scipen = 10, digits = 2)
seq <- seq(min(df$Incidence_adj) ,max(df$Incidence_adj), by = 1000)
axis(2, at = seq,labels = format(seq, big.mark = ","))
par(cex.axis = 0.8)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence_adj,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8)
text(x = 8.5, y = 100,            
     "Grey's Test : p <.0001", cex = 0.8)
dev.off()

# 버전2
png("age_under_60_ver2.png",width=654,height=482,res=100)
par(mar = c(5, 5, 3, 3))
range <- c(min(df$Incidence_adj) ,max(df$Incidence_adj))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence_adj, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence (100,000 PY)", 
     main = "Cumulative Incidence for Age under 60", lwd = 1.5,
     cex.axis=0.8, cex.lab=0.9, ylim = range, yaxt = "n")
options(scipen = 10, digits = 2)
seq <- seq(min(df$Incidence_adj) ,max(df$Incidence_adj), by = 1000)
axis(2, at = seq,labels = format(seq, big.mark = ","))
par(cex.axis = 0.8)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence_adj,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8, ncol = 2)
text(x = 1.2, y = max(df$Incidence_adj)*0.89,                # First text
     "Grey's Test : p <.0001", cex = 0.8)
dev.off()

############ AGE_60 #############
df <- read_excel("cifplot.xlsx", sheet = "age_60")

# 버전1
png("age_60_ver1.png",width=654,height=482,res=100)
par(mar = c(5, 5, 3, 3))
range <- c(min(df$Incidence_adj) ,max(df$Incidence_adj))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence_adj, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence (100,000 PY)", 
     main = "Cumulative Incidence for Age 60-69", lwd = 1.5,
     cex.axis=0.8, cex.lab=0.9, ylim = range, yaxt = "n")
options(scipen = 10, digits = 2)
seq <- seq(min(df$Incidence_adj) ,max(df$Incidence_adj), by = 1000)
axis(2, at = seq,labels = format(seq, big.mark = ","))
par(cex.axis = 0.8)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence_adj,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8)
text(x = 8.5, y = 100,            
     "Grey's Test : p=0.0009", cex = 0.8)
dev.off()

# 버전2
png("age_60_ver2.png",width=654,height=482,res=100)
par(mar = c(5, 5, 3, 3))
range <- c(min(df$Incidence_adj) ,max(df$Incidence_adj))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence_adj, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence (100,000 PY)", 
     main = "Cumulative Incidence for Age 60-69", lwd = 1.5,
     cex.axis=0.8, cex.lab=0.9, ylim = range, yaxt = "n")
options(scipen = 10, digits = 2)
seq <- seq(min(df$Incidence_adj) ,max(df$Incidence_adj), by = 1000)
axis(2, at = seq,labels = format(seq, big.mark = ","))
par(cex.axis = 0.8)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence_adj,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8, ncol = 2)
text(x = 1.2, y = max(df$Incidence_adj)*0.89,                # First text
     "Grey's Test : p=0.0009", cex = 0.8)
dev.off()


############ AGE_over_70 #############
df <- read_excel("cifplot.xlsx", sheet = "age_over_70")
df

# 버전1
png("age_over_70_ver1.png",width=654,height=482,res=100)
par(mar = c(5, 5, 3, 3))
range <- c(min(df$Incidence_adj) ,max(df$Incidence_adj))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence_adj, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence (100,000 PY)", 
     main = "Cumulative Incidence for Age 70 and above", lwd = 1.5,
     cex.axis=0.8, cex.lab=0.9, ylim = range, yaxt = "n")
options(scipen = 10, digits = 2)
seq <- seq(min(df$Incidence_adj) ,max(df$Incidence_adj), by = 1000)
axis(2, at = seq,labels = format(seq, big.mark = ","))
par(cex.axis = 0.8)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence_adj,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8)
text(x = 8.5, y = 100,            
     "Grey's Test : p=0.1541", cex = 0.8)
dev.off()

# 버전2
png("age_over_70_ver2.png",width=654,height=482,res=100)
par(mar = c(5, 5, 3, 3))
range <- c(min(df$Incidence_adj) ,max(df$Incidence_adj))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence_adj, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence (100,000 PY)", 
     main = "Cumulative Incidence for Age 70 and above", lwd = 1.5,
     cex.axis=0.8, cex.lab=0.9, ylim = range, yaxt = "n")
options(scipen = 10, digits = 2)
seq <- seq(min(df$Incidence_adj) ,max(df$Incidence_adj), by = 1000)
axis(2, at = seq,labels = format(seq, big.mark = ","))
par(cex.axis = 0.8)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence_adj,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8, ncol = 2)
text(x = 1.2, y = max(df$Incidence_adj)*0.89,                # First text
     "Grey's Test : p=0.1541", cex = 0.8)
dev.off()

############ BE_Y ###############
df <- read_excel("cifplot.xlsx", sheet = "BE_Y")

# 버전1
png("BE_ver1.png",width=654,height=482,res=100)
par(mar = c(5, 5, 3, 3))
range <- c(min(df$Incidence_adj) ,max(df$Incidence_adj))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence_adj, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence (100,000 PY)", 
     main = "Cumulative Incidence for BE", lwd = 1.5,
     cex.axis=0.8, cex.lab=0.9, ylim = range, yaxt = "n")
options(scipen = 10, digits = 2)
seq <- seq(min(df$Incidence_adj) ,max(df$Incidence_adj), by = 1000)
axis(2, at = seq,labels = format(seq, big.mark = ","))
par(cex.axis = 0.8)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence_adj,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8)
text(x = 8.5, y = 100,            
     "Grey's Test : p <.0001", cex = 0.8)
dev.off()

# 버전2
png("BE_ver2.png",width=654,height=482,res=100)
par(mar = c(5, 5, 3, 3))
range <- c(min(df$Incidence_adj) ,max(df$Incidence_adj))
plot(df[df$IBD == "N",]$year, df[df$IBD == "N",]$Incidence_adj, type = "l", lty = 2,
     xlab = "Year", ylab = "Cumulative Incidence (100,000 PY)", 
     main = "Cumulative Incidence for BE", lwd = 1.5,
     cex.axis=0.8, cex.lab=0.9, ylim = range, yaxt = "n")
options(scipen = 10, digits = 2)
seq <- seq(min(df$Incidence_adj) ,max(df$Incidence_adj), by = 1000)
axis(2, at = seq,labels = format(seq, big.mark = ","))
par(cex.axis = 0.8)
lines(df[df$IBD == "Y",]$year, df[df$IBD == "Y",]$Incidence_adj,lwd = 1.5, lty = 1)
legend(x = "topleft", legend = c("IBD = N", "IBD = Y"), 
       lty = c(2,1),lwd = c(1.2, 1.2),cex = 0.8, ncol = 2)
text(x = 1.2, y = max(df$Incidence_adj)*0.89,                # First text
     "Grey's Test : p <.0001", cex = 0.8)
dev.off()

