#install.packages("MCMCglmm")

library(MCMCglmm)
library(lme4)

# setwd("~/Desktop/Dropbox/university/red_card") #tom's laptop
setwd("C:/Users/tom/Dropbox/university/red_card") #tom's desktop

source("TJHPreProcessFuncs.R")

#from raw data generate disaggregated data for logistic regression via disaggregate.py
dat = read.csv("crowdstorm_disaggregated.csv")

# Select only the columns we will use in the analysis
dat <- dat[,c("playerShort", "position", "redCards", "refNum", "meanIAT", "meanExp", "skintone", "allreds", "leagueCountry","refCount","allredsStrict")]

#if we're on windows or mac you need to force the bias variables into numeric
dat$meanIAT <- as.numeric(as.character(dat$meanIAT))
dat$meanExp <- as.numeric(as.character(dat$meanExp))

# Remove NA skintone data
dat <- dat[!is.na(dat$skintone),]

#remove refs present in <22 dyads
#reduces 373067 obs. by 16958
dat <- dat[dat$refCount>21,]

# Remove those individuals on whom we do not have position information
NoPos <- which(dat$position == "")
dat <- dat[-NoPos, ]

# Remove those where referee has unknown bias
dat <- dat[!is.na(dat$meanIAT),]
dat <- dat[!is.na(dat$meanExp),]

# n = 335537 games

# See what the current structure of the data is
str(dat)

# Find out how many observations we have
ndat <- nrow(dat)

# Now create another variable which splits individuals into GK, Defender, Midfielder, Attacker
GeneralPos <- rep(NA, ndat)
for(i in 1:ndat) {
  GeneralPos[i] <- FindGeneralPos(as.character(dat$position[i]))
}


# variable centering

# nb NOT used in creating final data set - 
# - stopped using this because skintone recoded in revised dataset
##center skintone ratings (MUST remove nans first)
AvSkin <- mean(dat$skintone)
centSkinTone <- dat$skintone - AvSkin

# Now implicit racism in country of referee
AvImpBias <- mean(dat$meanIAT)
centImpBias <- dat$meanIAT - AvImpBias 

# Now explicit racism in country of referee
AvExpBias <- mean(dat$meanExp)
centExpBias <- dat$meanExp - AvExpBias

# Create a dataframe containing all of these variables
## uncomment this line to have analysis on strict red cards only (ie no double yellows). Results are the same
#Finaldat <- data.frame(Player = as.factor(dat$playerShort), GeneralPos = as.factor(GeneralPos), SpecificPos = as.factor(dat$position), FacSkinTone = as.factor(dat$skintone), ContSkinTone = as.numeric(dat$skintone), NoWhite = (dat$skintone > 1), ImpBias = as.numeric(centImpBias), ExpBias = as.numeric(centExpBias), Ref = as.factor(dat$refNum), Red = dat$redCards, allreds = as.numeric(dat$allredsStrict), league = as.factor(dat$leagueCountry))
Finaldat <- data.frame(Player = as.factor(dat$playerShort), GeneralPos = as.factor(GeneralPos), SpecificPos = as.factor(dat$position), FacSkinTone = as.factor(dat$skintone), ContSkinTone = as.numeric(dat$skintone), NoWhite = (dat$skintone > 1), ImpBias = as.numeric(centImpBias), ExpBias = as.numeric(centExpBias), Ref = as.factor(dat$refNum), Red = dat$redCards, allreds = as.numeric(dat$allreds), league = as.factor(dat$leagueCountry))

# uncommment this line to use centered skintone variable (makes little difference)
#Finaldat <- data.frame(Player = as.factor(dat$playerShort), GeneralPos = as.factor(GeneralPos), SpecificPos = as.factor(dat$position), FacSkinTone = as.factor(dat$skintone), ContSkinTone = as.numeric(centSkinTone), NoWhite = (dat$skintone > 1), ImpBias = as.numeric(centImpBias), ExpBias = as.numeric(centExpBias), Ref = as.factor(dat$refNum), Red = dat$redCards, allreds = as.numeric(dat$allreds), league = as.factor(dat$leagueCountry))

#for model selection see TJHAllRedsAnalysis.R
#FootballFinal4i.glmm <- glmer(allreds ~ ContSkinTone*ImpBias + league + SpecificPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)
FootballFinal4.glmm <- glmer(allreds ~ ContSkinTone*ImpBias + ContSkinTone*ExpBias + league + SpecificPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)

saveRDS(FootballFinal4.glmm, "FootballFinal4.glmm_model.rds")
#FootballFinal4.glmm <- readRDS("FootballFinal4.glmm_model.rds")

summary(FootballFinal4.glmm)

## For the skin tone main effect
PointEst <- fixef(FootballFinal4.glmm)
Pointse <- sqrt(diag(vcov(FootballFinal4.glmm)))
CIUpper <- PointEst + 1.96*Pointse
CIlower <- PointEst - 1.96*Pointse

# Now for the odds ratio i.e. exponentiate
exp(PointEst)
exp(CIlower)
exp(CIUpper)

#skintone: 1.31 [1.10, 1.56]
#skintone * implicit bias 0.00 [0.00, 2.33]
#skintone * explicit bias 1.84 [0.49, 6.85]

#skintone: 1.32 [1.11, 1.58]
#skintone * implicit bias 0.00 [0.00, 1.53]
#skintone * explicit bias 1.83 [0.31, 6.41]


#now lets do this the baysian way - takes longer

#full - takes 8925 seconds to run = 148 minutes = nearly three hours
#brief - takes 173 seconds on tom's laptop
#ptm <- proc.time() #start the clock!
#proc.time() - ptm #stop the clock!
#saveRDS(M1, "M1model.rds")
#load using: M1 <- readRDS("M1model.rds")

# Two Random effects, EQUIVALENT TO lmer(redCards~ (1|player) + (1|refNum) + rater1,family=binomial,data=subdata[!is.na(subdata$rater1),])
# - - - interaction with bias and skintone and with position

ptm <- proc.time() #start the clock!
prior= list(R = list(R1 = list(V = 1, fix=1), R2 = list(V = 1, fix=1),R3 = list(V = 1, fix=1),R4 = list(V = 1, fix=1)), G = list(G1 = list(V = 1,nu = 0.002),G2 = list(V = 1,nu = 0.002)))
M6 = MCMCglmm(allreds~ 1+ ContSkinTone*ImpBias + ContSkinTone*ExpBias + league + SpecificPos, random = ~Player+Ref,data=Finaldat,family="categorical",prior=prior,verbose=F, burnin=1000,nitt=10000,thin=1)
proc.time() - ptm #stop the clock!

saveRDS(M6, "M6model.rds")
M6 <- readRDS("M6model.rds")
summary(M6)

#ContSkinTone                      0.274744   0.108338   0.459123 
#ContSkinTone:ImpBias             -6.299596 -14.289650   0.425546    
#ContSkinTone:ExpBias              0.602204  -1.181566   1.857206    

#skintone: 1.32 [1.11, 1.58]
#skintone * implicit bias 0.00 [0.00, 1.53]
#skintone * explicit bias 1.83 [0.31, 6.41]

### --------------------- Results

##Frequentist
#skintone: 1.31 [1.10, 1.56]
#skintone * implicit bias 0.00 [0.00, 2.33]
#skintone * explicit bias 1.84 [0.49, 6.85]

##Bayesian
#skintone: 1.32 [1.11, 1.58]
#skintone * implicit bias 0.00 [0.00, 1.53]
#skintone * explicit bias 1.83 [0.31, 6.41]


