library("lme4")
library("here")

# from raw data generate disaggregated data for logistic regression via 2_data_disaggregate.py
dat = read.csv(here("data", "2_disaggregated_data.csv"))

# Select only the columns we will use in the analysis
dat <- dat[, c("playerShort", "position", "redCards", "refNum", "meanIAT", "meanExp", "skintone", "allreds", "leagueCountry","refCount","allredsStrict")]

#if we're on windows or mac you need to force the bias variables into numeric
dat$meanIAT <- as.numeric(as.character(dat$meanIAT))
dat$meanExp <- as.numeric(as.character(dat$meanExp))

# Remove NA skintone data
dat <- dat[!is.na(dat$skintone), ]

#remove refs present in <22 dyads
#reduces 373067 obs. by 16958
dat <- dat[dat$refCount > 21, ]

# Remove those individuals on whom we do not have position information
NoPos <- which(dat$position == "")
dat <- dat[-NoPos, ]

# Remove those where referee has unknown bias
dat <- dat[!is.na(dat$meanIAT), ]
dat <- dat[!is.na(dat$meanExp), ]

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
# uncomment this line to have analysis on strict red cards only (ie no double yellows). Results are the same.
# Finaldat <- data.frame(Player = as.factor(dat$playerShort),
#                        GeneralPos = as.factor(GeneralPos),
#                        SpecificPos = as.factor(dat$position),
#                        FacSkinTone = as.factor(dat$skintone),
#                        ContSkinTone = as.numeric(dat$skintone),
#                        NoWhite = (dat$skintone > 1),
#                        ImpBias = as.numeric(centImpBias),
#                        ExpBias = as.numeric(centExpBias),
#                        Ref = as.factor(dat$refNum),
#                        Red = dat$redCards,
#                        allreds = as.numeric(dat$allredsStrict),
#                        league = as.factor(dat$leagueCountry))

Finaldat <- data.frame(Player = as.factor(dat$playerShort),
                       GeneralPos = as.factor(GeneralPos),
                       SpecificPos = as.factor(dat$position),
                       FacSkinTone = as.factor(dat$skintone),
                       ContSkinTone = as.numeric(dat$skintone),
                       NoWhite = (dat$skintone > 1),
                       ImpBias = as.numeric(centImpBias),
                       ExpBias = as.numeric(centExpBias),
                       Ref = as.factor(dat$refNum),
                       Red = dat$redCards,
                       allreds = as.numeric(dat$allreds),
                       league = as.factor(dat$leagueCountry))

write.csv(Finaldat, here("data", "3_prepared_data.csv"), row.names = FALSE)

# uncommment this line to use centered skintone variable (makes little difference).
# Finaldat <- data.frame(Player = as.factor(dat$playerShort),
#                        GeneralPos = as.factor(GeneralPos),
#                        SpecificPos = as.factor(dat$position),
#                        FacSkinTone = as.factor(dat$skintone),
#                        ContSkinTone = as.numeric(centSkinTone),
#                        NoWhite = (dat$skintone > 1),
#                        ImpBias = as.numeric(centImpBias),
#                        ExpBias = as.numeric(centExpBias),
#                        Ref = as.factor(dat$refNum),
#                        Red = dat$redCards,
#                        allreds = as.numeric(dat$allreds),
#                        league = as.factor(dat$leagueCountry))

# for model selection see TJHAllRedsAnalysis.R
# FootballFinal4i.glmm <- glmer(allreds ~ ContSkinTone*ImpBias +
#                                 league + SpecificPos +
#                                 (1|Ref) + (1|Player),
#                               data = Finaldat, family = "binomial", nAGQ = 0)

