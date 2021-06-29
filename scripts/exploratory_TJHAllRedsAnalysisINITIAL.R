# This version will work on the full set of data and analyse the allreds column (accumulated yellow and red cards)

library("lme4")

file_path = "/Users/sebastian/Documents/Uni/Sheffield (MSc)/2. Semester/Research Project/msc_dissertation_project"
setwd(file_path)

library("here")

source(here("scripts", "TJHPreProcessFuncs.R"))


# Read in complete data set 
dat = read.csv(here("data", "crowdstorm_disaggregated.csv"), header = TRUE, sep = ",")

# Select only the columns we will use in the analysis
dat <- dat[, c("playerShort", "position", "redCards", "refNum", "meanIAT", "meanExp", "skintone", "allreds", "leagueCountry")]

# Each row corresponds to a single football match and the important columns are:
# player - the id of the player (FACTOR VARIABLE)
# redCards - whether the payer got a red card in that game (0/1 VARIABLE)
# refNum - the identifier of the referee (FACTOR VARIABLE) 
# position - the position played by the individual

# See what the current structure of the data is
str(dat)

# Clean up the data to remove those individuals who we do not have required information on

# Remove NA skintone data
dat <- dat[!is.na(dat$skintone), ]

# Remove those individuals on whom we do not have position information
NoPos <- which(dat$position == "")
dat <- dat[-NoPos, ]

# Remove those where referee has unknown bias
dat <- dat[!is.na(dat$meanIAT), ]
dat <- dat[!is.na(dat$meanExp), ]

# Remove those where the league country in unknown
dat <- dat[!is.na(dat$leagueCountry), ]

# Find out how many observations we have
ndat <- nrow(dat) # cleaning retained 82.34% of the data


# Now create another variable which splits individuals into GK, Defender, Midfielder, Attacker
GeneralPos <- rep(NA, ndat)

for(i in 1:ndat) {
  GeneralPos[i] <- FindGeneralPos(as.character(dat$position[i]))
}

## Centre variables to reduce potential numerical issues

# Firstly we want to center the SkinTone
AvSkin <- mean(dat$skintone)
centSkinTone <- dat$skintone - AvSkin
#### We will also want to include an interaction term between the referee racism and player colour so include these terms centred

# Now implicit racism in country of referee
AvImpBias <- mean(dat$meanIAT)
centImpBias <- dat$meanIAT - AvImpBias 
 
# Now explicit racism in country of referee
AvExpBias <- mean(dat$meanExp)
centExpBias <- dat$meanExp - AvExpBias

# Create a dataframe containing all of these variables
Finaldat <- data.frame(Player = as.factor(dat$playerShort),
                       GeneralPos = as.factor(GeneralPos),
                       SpecificPos = as.factor(dat$position),
                       FacSkinTone = as.factor(dat$skintone),
                       ContSkinTone = as.numeric(centSkinTone),
                       NoWhite = (dat$skintone > 1),
                       ImpBias = as.numeric(centImpBias),
                       ExpBias = as.numeric(centExpBias),
                       Ref = as.factor(dat$refNum),
                       Red = dat$redCards, allreds = as.numeric(dat$allreds),
                       league = as.factor(dat$leagueCountry))

# Do a bit of cleaning up
rm(dat, GeneralPos)

# To get the model to run you have to choose nAGQ = 0 (i.e. not Laplace approximation).
# This is a quicker version with less exact approximation

######################################################
## I checked a variety of models - each model takes about 20 mins to fit 
## (all have as random effect Ref and Player)
# To compare we use the AIC criteria. A lower AIC score is better.


### Initially do not include the bias score of the referee

# Model 1 - Red ~ Indicator(Dark) + GeneralPos
NoRefRacFootball1.glmm <- glmer(allreds ~ NoWhite + GeneralPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)
# Model 2 - Red ~ Indicator(Dark) + SpecificPos
NoRefRacFootball2.glmm <- glmer(allreds ~ NoWhite + SpecificPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)
# Model 3 - Red ~ factor(SkinTone) + GeneralPos
NoRefRacFootball3.glmm <- glmer(allreds ~ as.factor(FacSkinTone) + GeneralPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)
# Model 4 -  Red ~ factor(SkinTone) + SpecificPos
NoRefRacFootball4.glmm <- glmer(allreds ~ as.factor(FacSkinTone) + SpecificPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)
# Model 5 -  Red ~ continuous(SkinTone) + GeneralPos
NoRefRacFootball5.glmm <- glmer(allreds ~ ContSkinTone + GeneralPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)
# Model 6 -  Red ~ continuous(SkinTone) + SpecificPos
NoRefRacFootball6.glmm <- glmer(allreds ~ ContSkinTone + SpecificPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)

# Now include the bias score of the referee in an interaction with Skin Tone
# Model 7 -  Red ~ continuous(SkinTone)*continuous(RefRacism) + SpecificPos 
RefRacFootball7.glmm <- glmer(allreds ~ ContSkinTone*ExpBias + SpecificPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)
RefRacFootball8.glmm <- glmer(allreds ~ ContSkinTone*ImpBias + SpecificPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)
RefRacFootball9.glmm <- glmer(allreds ~ ContSkinTone + ImpBias + ExpBias + SpecificPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)
RefRacFootball10.glmm <- glmer(allreds ~ ContSkinTone + ImpBias + SpecificPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)
RefRacFootball11.glmm <- glmer(allreds ~ ContSkinTone + ExpBias + SpecificPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)
RefRacFootball12.glmm <- glmer(allreds ~ ContSkinTone*ImpBias + ContSkinTone*ExpBias + SpecificPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)

# Now include a league term (first with an interaction)
FootballFinal.glmm <- glmer(allreds ~ ContSkinTone*ImpBias + ContSkinTone*league + SpecificPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)

FootballFinal2.glmm <- glmer(allreds ~ ContSkinTone*ImpBias + league + SpecificPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)

FootballFinal3.glmm <- glmer(allreds ~ ContSkinTone*ImpBias + ContSkinTone*ExpBias + ContSkinTone*league + SpecificPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)

FootballFinal4.glmm <- glmer(allreds ~ ContSkinTone*ImpBias + ContSkinTone*ExpBias + league + SpecificPos + (1|Ref) + (1|Player), data = Finaldat, family = "binomial", nAGQ = 0)


##### We can now look at the AIC scores and output to select a model
summary(NoRefRacFootball1.glmm)
summary(NoRefRacFootball2.glmm)
summary(NoRefRacFootball3.glmm)
summary(NoRefRacFootball4.glmm)
summary(NoRefRacFootball5.glmm)
summary(NoRefRacFootball6.glmm)
summary(RefRacFootball7.glmm)
summary(RefRacFootball8.glmm)
summary(RefRacFootball9.glmm)
summary(RefRacFootball10.glmm)
summary(RefRacFootball11.glmm)
summary(RefRacFootball12.glmm)

summary(FootballFinal.glmm)
summary(FootballFinal2.glmm)
summary(FootballFinal3.glmm)
summary(FootballFinal4.glmm)

AIC(NoRefRacFootball1.glmm)
AIC(NoRefRacFootball2.glmm)
AIC(NoRefRacFootball3.glmm)
AIC(NoRefRacFootball4.glmm)
AIC(NoRefRacFootball5.glmm)
AIC(NoRefRacFootball6.glmm)
AIC(RefRacFootball7.glmm)
AIC(RefRacFootball8.glmm)
AIC(RefRacFootball9.glmm)
AIC(RefRacFootball10.glmm)
AIC(RefRacFootball11.glmm)
AIC(RefRacFootball12.glmm)

AIC(FootballFinal.glmm)
AIC(FootballFinal2.glmm)
AIC(FootballFinal3.glmm)
AIC(FootballFinal4.glmm)

## According to the AIC the best of these models (which include the required interactions to answer the questions posed) is FootballFinal4.glmm

# We will use this model to answer the questions and find CI for the main effects of skintone; the interaction between meanIAT and skintone; and the main effect of meanIAT score 

## For the skin tone main effect
PointEst <- fixef(FootballFinal4.glmm)
Pointse <- sqrt(diag(vcov(FootballFinal4.glmm)))
CIUpper <- PointEst + 1.96*Pointse
CIlower <- PointEst - 1.96*Pointse

# Now for the odds ratio i.e. exponentiate
exp(PointEst)
exp(CIUpper)
exp(CIlower)

