# Beaudrot et al. Ecology manuscript "Mixed-species groups of Serengeti grazers: a test of the stress gradient hypothesis"
# Purpose: Create main text Figure 2 and supplementary figures  
# Author: Lydia Beaudrot

rm(list=ls())
library(fields)

# Load data and format for figures
obs2 <- read.csv(file="obs2.csv") # observations per 16 day NDVI sampling bin
# obs2 <- read.csv(file="obs2_excluding_nocturnal.csv") # produces results in Appendix that exclude nocturnal observations
combo <- paste0(obs2$bin_val,".",obs2$Var2.1)
obs2 <- data.frame(obs2, combo)
obs2.assoc <- obs2[duplicated(obs2$combo)==FALSE,]
obs2.poly <- obs2.assoc[obs2.assoc$Var2.1==1,]
obs2.mono <- obs2.assoc[obs2.assoc$Var2.1==0,]

ndvi <- read.csv(file="ndvi.csv") # camera trap specific ndvi values for each 16 day ndvi sampling bin
rownames(ndvi) <- ndvi[,1]
ndvi <- ndvi[,-1]
avg.ndvi <- colMeans(as.matrix(ndvi))


# Plotting helper functions
plot.obs <- function(species,ylab,ylim){
  plot(obs2$Freq[obs2$Var2==species] ~ obs2$bin_val[obs2$Var2==species], 
       type="l", xlab="", ylab=ylab, las=0, ylim=ylim)
  lines(obs2$bin_val[obs2$Var2==species], obs2$Freq[obs2$Var2==species])
  points(obs2$bin_val[obs2$Var2==species], obs2$Freq[obs2$Var2==species], pch=16, cex=0.8)
}

add.species <- function(species, color){
  lines(obs2$bin_val[obs2$Var2==species], obs2$Freq[obs2$Var2==species], col=color)
  points(obs2$bin_val[obs2$Var2==species], obs2$Freq[obs2$Var2==species], pch=16, cex=0.8, col=color)
}

add.assoc <- function(color){
  lines(obs2.poly$bin_val, obs2.poly$Freq.1, col=color)
  points(obs2.poly$bin_val, obs2.poly$Freq.1, pch=16, cex=0.8, col=color)
}


######## Figure 2 Main Text #######
# Extract month
Month <- as.numeric(substr(obs2.mono$Start, 6, 7))
dry <- ifelse(Month==6| Month==7 | Month==8 | Month==9 | Month==10 | Month==11, "Dry", "Wet")
obs2.mono <- data.frame(obs2.mono, Month, dry)

# Objects for creating NDVI shading
y <- (avg.ndvi*0.0001)*4000
x <- 1:length(avg.ndvi)
x.poly <- c(x, x[183], x[1])
y.poly <- c(y, 0, 0)

# Plot figure showing variation in grazer observations (mono and polyspecific), NDVI, and season over time
plot(obs2.mono$Freq.1 ~ obs2.mono$bin_val,  las=1, ylab="Number of observations", xlab="", cex=0.01)
# add gray shading for wet season: 26-33, 45-56, 68-79, 91-102, 113-124
# NB plot excluding nocturnal data used 2915 as 4th coordinate in rect()
rect(26, -40, 33, 3935, col="gray", density=-100, border=NA) 
rect(45, -40, 56, 3935, col="gray", density=-100, border=NA) # rect(xleft, ybottom, xright, ytop...)
rect(68, -40, 79, 3935, col="gray", density=-100, border=NA)
rect(91, -40, 102, 3935, col="gray", density=-100, border=NA)
rect(113, -40, 124, 3935, col="gray", density=-100, border=NA)
polygon(x.poly, y.poly, col="green4", border=NA) # add NDVI in green polygon
lines(obs2.mono$bin_val, (obs2.poly$Freq.1/obs2.mono$Freq.1)*2915, col="white", lwd=2, lty=1) # Note 2915 value is the max ylim value for scaling % of obs
points(obs2.mono$Freq.1 ~ obs2.mono$bin_val, pch=17, cex=0.8, col="black") # add monospecific observations
lines(obs2.mono$bin_val, obs2.mono$Freq.1, col="black")
add.assoc("blue") # add polyspecific observations



####### Figure S1 ########
# Plot variation in observations of each focal grazer species over time for supplementary figure
# NB Notation for plot.obs requires quotes for species and ylab; vector for ylim e.g., plot.obs("wildebeest", "Wildebeest observations", c(0,1500))
par(mar=c(1, 1, 1, 1))
set.panel(2,3)
plot.obs("wildebeest", "Wildebeest observations", c(0,2000))
plot.obs("zebra", "Zebra observations", c(0,2000))
plot.obs("gazelleThomsons", "Thomsons gazelle observations", c(0,2000))
plot.obs("buffalo", "Buffalo observations", c(0,2000))
plot.obs("topi", "Topi observations", c(0,2000))
plot.obs("hartebeest", "Hartebeest observations", c(0,2000))
set.panel()


####### FIGURE S2 ######
# Examine temporal autocorrelation in proportion of observations that are mixed-species groups
temp <- obs2.poly$Freq.1/obs2.mono$Freq.1
acf(temp, main="Proportion of observations with mixed-species groups")


####### FIGURE S3 ######
# Create appendix figure showing relationships between predictor variables
set.panel(3,2)
par(mar=c(3,4,3,1))

# Load CT level covariates
new_covs <- read.csv(file="new_covs.csv")

# Plot differences in distance to kopjes between habitats
boxplot(new_covs$KOP.DIST.M ~ new_covs$BINHAB, las=1, ylab="Distance to kopjes (m)")
wilcox.test(new_covs$KOP.DIST.M ~ new_covs$BINHAB)

# Plot difference in grazer observations between seasons
boxplot(obs2.mono$Freq.1 ~ obs2.mono$dry, las=1, ylab="Number of grazer observations", xlab="Season")
wilcox.test(obs2.mono$Freq.1 ~ obs2.mono$dry)

# Plot differences in lion density between habitats in wet season
boxplot(new_covs$lion.encount.wet ~ new_covs$BINHAB, ylim=c(0,2.5), las=1, ylab="Lion density in wet season")
wilcox.test(new_covs$lion.encount.wet ~ new_covs$BINHAB)
cor(new_covs$lion.encount.wet, new_covs$lion.encount.dry)

# Extract dry season and wet season NDVI values per camera trap for boxplot for supplementary figure
drywet <- t(ndvi)
bin <- rownames(drywet)
bin2 <- sub("X","", bin)
bin3 <- gsub(".","-", bin2, fixed=TRUE, perl=FALSE)
rownames(drywet) <- bin3
drywet2 <- drywet[match(as.character(obs2.mono$Start), rownames(drywet)),]
boxplot(drywet2*0.0001 ~ obs2.mono$dry, las=1, ylab="NDVI")
wilcox.test(drywet2*0.0001 ~ obs2.mono$dry)

# Plot differences in lion density between habitats in dry season
boxplot(new_covs$lion.encount.dry ~ new_covs$BINHAB, ylim=c(0,2.5), las=1, ylab="Lion density in dry season")
t.test(new_covs$lion.encount.dry ~ new_covs$BINHAB)
wilcox.test(new_covs$lion.encount.dry ~ new_covs$BINHAB)

set.panel()



