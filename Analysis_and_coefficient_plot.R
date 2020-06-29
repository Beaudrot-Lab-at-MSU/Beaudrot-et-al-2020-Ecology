# Beaudrot et al. Ecology manuscript "Mixed-species groups of Serengeti grazers: a test of the stress gradient hypothesis"
# Purpose: Analysis of mixed-species associations in Serengeti grazers and Figure 3
# Author: Lydia Beaudrot

# Load packages
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(lme4)

# Load formatted data
grazers <- read.csv(file="grazers.csv")
# grazers <- read.csv(file="grazers_excluding_nocturnal.csv) # produces results in Appendix


# Definitions of variables:
# assoc: observations of two species observed by a camera trap simultaneously are coded as associations with a 1
# grazer species: buffalo, Thomson's gazelle, hartebeest, topi, wildebeest, zebra

########### Output table for SUMMARY STATISTICS
grazers_table <- table(grazers$species, grazers$MixedSpp2)
#write.csv(grazers_table, file="Grazers_table_summary.csv")

########### GOODNESS OF FIT TESTS to determine whether the number of observed associations per species differs significantly from what would be expected by chance based on the number of observations
source(file="g.test.R")

# Data include nocturnal observations
mono_obs <- c(6397, 32176, 5754, 2068, 27941, 38648)
#poly_obs <- c(38, 444, 148, 50, 1668, 1956)
poly_obs <- c(19, 222, 74, 25, 834, 978) # [divide poly obs by 2 so that they are independent rather than duplicate observations]

# Data exclude nocturnal observations
#mono_obs <- c(4411, 25475, 4464, 1625, 20602, 26651) # Number of monospecific observations per species from grazers_table
#poly_obs <- c(33, 397, 125, 45, 1387, 1635) # Number of polyspecific observations per species from grazers_table

total_obs <- data.frame(mono_obs, poly_obs, 
                        row.names=c("Buffalo", "Thomsons gazelle", "Hartebeest", 
                                            "Topi", "Wildebeest", "Zebra"))
g.test(total_obs)
g.test(total_obs[c(2,5,6),]) # migrating species
g.test(total_obs[c(1,3,4),]) # resident species

########### MODELLING: Model polyspecific association probability as a function of food and predation for grazers

model <- glmer(assoc ~ season + BINHAB +  scale(encount_risk) + scale(KOP.DIST.M) + scale(NDVI) + (1|SiteID), family=binomial, data=grazers, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(model)

# Subset data to observations within 1000 M from a kopje and re-run model
kops <- grazers[grazers$KOP.DIST.M < 1000,]
table(kops$species, kops$MixedSpp2)
colSums(table(kops$species, kops$MixedSpp2))
model2 <- glmer(assoc ~ season + BINHAB +  scale(encount_risk) + scale(KOP.DIST.M) + scale(NDVI) + (1|SiteID), family=binomial, data=kops, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(model2)

### Calculate odds ratios 

hold <- coef(model) # For all observations
head(exp(hold[[1]])) # for positive coefficients
1/head(exp(hold[[1]])) # for negative coefficients

hold2 <- coef(model2) # For observations restricted to 1000 M within kopjes
head(exp(hold2[[1]])) # for positive coefficients
1/head(exp(hold2[[1]])) # for negative coefficients

# Create coefficient plot for Figure 3

plot_model(model, colors="black", vline.color="gray50",
           axis.labels=c("Distance to Kopjes", "Lion density", "Wet season", "NDVI", "Woodland habitat"), 
           order.terms=c(2, 5, 1, 3, 4), axis.lim=c(0.9, 1.2),
           title="") +
  theme(panel.grid.minor = element_line(colour="white"), 
        panel.background = element_rect(fill="white", colour="gray50"))



