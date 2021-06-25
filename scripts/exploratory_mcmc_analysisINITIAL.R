library(MCMCglmm)
library(lme4)

# set wkdir
file_path = "/Users/sebastian/Documents/Uni/Sheffield (MSc)/2. Semester/Research Project/msc_dissertation_project"
setwd(file_path)

library("here")

# from raw data generate disaggregated data for logistic regression via disaggregate.py
dat = read.csv(here("data", "crowdstorm_disaggregated.csv"))

#if we're on windows or mac you need to force the bias variables into numeric
dat$meanIAT <- as.numeric(as.character(dat$meanIAT))
dat$meanExp <- as.numeric(as.character(dat$meanExp))

# Remove NA skintone data
dat <- dat[!is.na(dat$skintone), ]

#center skintone ratings (MUST remove nans first)
AvSkin <- mean(dat$skintone)
dat$skintone <- dat$skintone - AvSkin

# Remove those individuals on whom we do not have position information
NoPos <- which(dat$position == "")
dat <- dat[-NoPos, ]

# Remove those where referee has unknown bias
dat <- dat[!is.na(dat$meanIAT),]
dat <- dat[!is.na(dat$meanExp),]


#full - takes 8925 seconds to run = 148 minutes = nearly three hours
#brief - takes 118 seconds
#ptm <- proc.time() #start the clock!
#proc.time() - ptm #stop the clock!
#saveRDS(M1, "M1model.rds")
#load using: M1 <- readRDS("M1model.rds")

# - - - using allreds var, simple effect of skintone
# Two Random effects, EQUIVALENT TO lmer(redCards~ (1|player) + (1|refNum) + rater1,family=binomial,data=subdata[!is.na(subdata$rater1),])
prior = list(R = list(V = 1, fix=1),
             G = list(G1 = list(V = 1,nu = 0.002), G2 = list(V = 1,nu = 0.002)))
M2 = MCMCglmm(allreds ~1 + skintone,
              random = ~player + refNum,
              data = dat,family = "categorical", prior = prior, verbose = F,
              burnin = 1000, nitt = 10000, thin = 1)
saveRDS(M2, "M2model.rds")
summary(M2)

# - - - with bias measures
prior= list(R = list(V = 1, fix=1), G = list(G1 = list(V = 1,nu = 0.002),G2 = list(V = 1,nu = 0.002)))
M5 = MCMCglmm(allreds~ 1+ skintone*meanIAT, random = ~player+refNum,data=dat[(!is.na(dat$skintone) & !is.na(dat$meanIAT)),],family="categorical",prior=prior,verbose=F, burnin=1000,nitt=10000,thin=1)
saveRDS(M5, "M5model.rds")

# - - - with bias measures
prior= list(R = list(V = 1, fix=1), G = list(G1 = list(V = 1,nu = 0.002),G2 = list(V = 1,nu = 0.002)))
M5x = MCMCglmm(allreds~ 1+ skintone*meanExp, random = ~player+refNum,data=dat[(!is.na(dat$skintone) & !is.na(dat$meanIAT)),],family="categorical",prior=prior,verbose=F, burnin=1000,nitt=10000,thin=1)
saveRDS(M5x, "M5xmodel.rds")

# - - - interaction with position
prior= list(R = list(V = 1, fix=1), G = list(G1 = list(V = 1,nu = 0.002),G2 = list(V = 1,nu = 0.002)))
M5_p = MCMCglmm(allreds~ 1+ skintone*meanExp*position, random = ~player+refNum,data=dat[(!is.na(dat$skintone) & !is.na(dat$meanIAT)),],family="categorical",prior=prior,verbose=F, burnin=1000,nitt=10000,thin=1)
saveRDS(M5_p, "M5_pmodel.rds")

# no inteaction of skintone with position.
M5_p2 = MCMCglmm(allreds~ 1+ skintone*position, random = ~player+refNum,data=dat[(!is.na(dat$skintone) & !is.na(dat$meanIAT)),],family="categorical",prior=prior,verbose=F, burnin=1000,nitt=10000,thin=1)
saveRDS(M5_p2, "M5_p2model.rds")



# - - - interaction with bias and skintone and with position
prior= list(R = list(R1 = list(V = 1, fix=1), R2 = list(V = 1, fix=1)), G = list(G1 = list(V = 1,nu = 0.002),G2 = list(V = 1,nu = 0.002)))
M6 = MCMCglmm(allreds~ 1+ skintone*meanIAT + position, random = ~player+refNum,data=dat[(!is.na(dat$skintone) & !is.na(dat$meanIAT)),],family="categorical",prior=prior,verbose=F, burnin=100,nitt=1000,thin=1)

### --------------------- Results

M2 <- readRDS("M2model.rds")

#exp the log odds to get the estimate and CI

#implicit
M5i <- readRDS("M5model.rds")

#explicit

M5x <- readRDS("M5xmodel.rds")


# ------------------------

#- subsample for refs from majority 4 countries
subdata = dat[((dat$refCountry ==3) | (dat$refCountry ==7) | (dat$refCountry ==8) | (dat$refCountry ==44)),]  

prior= list(R = list(V = 1, fix=1), G = list(G1 = list(V = 1,nu = 0.002),G2 = list(V = 1,nu = 0.002)))
M2sub = MCMCglmm(allreds~ 1+ skintone, random = ~player + refNum,data=subdata,family="categorical",prior=prior,verbose=F, burnin=1000,nitt=10000,thin=1)
saveRDS(M2sub, "M2submodel.rds")
summary(M2sub)

prior= list(R = list(V = 1, fix=1), G = list(G1 = list(V = 1,nu = 0.002),G2 = list(V = 1,nu = 0.002)))
M5isub = MCMCglmm(allreds~ 1+ skintone*meanIAT, random = ~player + refNum,data=subdata,family="categorical",prior=prior,verbose=F, burnin=1000,nitt=10000,thin=1)
saveRDS(M5isub, "M5isubmodel.rds")
summary(M5isub)
