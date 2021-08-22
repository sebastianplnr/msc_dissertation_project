library("here")
library("data.table")
library("tidyverse")
library("plyr")
library("lme4")

# From raw data generate disaggregated data for logistic regression via 2_data_disaggregate.py
dat = data.frame(fread(here::here("data", "2_disaggregated_data.csv")))

# Silberzahn et al. 2018 list all covariates among them are two that need to be calculated: the cumulated number of cards received by a player and assigned by a referee. (More information in the codebook)
dat = dat %>% group_by(player) %>% mutate(player_all_cards_received = sum(yellowCards + yellowReds + redCards))
dat = dat %>% group_by(refNum) %>% mutate(ref_all_cards_assigned = sum(yellowCards + yellowReds + redCards))

# Select only the columns we will use in the analysis
# Adding all used covariates mentioned in Silberzahn et al. (2018) incl. main variables
dat <- dat[, c("playerShort", "height", "weight", "birthday", "position",
               "club", "leagueCountry",
               "games",  "goals", "victories", "ties", 
               "yellowCards", "yellowReds", "redCards", "allreds", "allredsStrict", "player_all_cards_received",
               "refNum", "refCountry", "refCount", "ref_all_cards_assigned",
               "meanIAT", "meanExp", "skintone")]

# If we're on windows or mac you need to force the bias variables into numeric
dat$meanIAT <- as.numeric(as.character(dat$meanIAT))
dat$meanExp <- as.numeric(as.character(dat$meanExp))

# Remove NA skintone data
dat <- dat[!is.na(dat$skintone), ]

# Remove refs present in <22 dyads
# Reduces 373067 obs. by 16958
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

# find out how many observations we have
ndat <- nrow(dat)

# Now create another variable which splits individuals into GK, Defender, Midfielder, Attacker
FindGeneralPos <- function(Pos) {
  if(Pos == "Goalkeeper") return("GK")
  if((Pos == "Center Back") | (Pos == "Left Fullback") | (Pos == "Right Fullback")) return("Def")
  if((Pos == "Center Midfielder") | (Pos == "Defensive Midfielder") | (Pos == "Left Midfielder") | (Pos == "Right Midfielder") | (Pos == "Attacking Midfielder")) return("Mid")
  if((Pos == "Center Forward") | (Pos == "Left Winger") | (Pos == "Right Winger")) return("Att")
  
  # otherwise stop as unknown position  
  stop("Unknown Position")
  return(NA)
}

# Create column for GeneralPos
GeneralPos <- rep(NA, ndat)

# Fill column with GeneralPos
for(i in 1:ndat) {
  GeneralPos[i] <- FindGeneralPos(as.character(dat$position[i]))
}

# Variable centring
## Implicit racism in country of referee
AvImpBias <- mean(dat$meanIAT)
centImpBias <- dat$meanIAT - AvImpBias 

## Explicit racism in country of referee
AvExpBias <- mean(dat$meanExp)
centExpBias <- dat$meanExp - AvExpBias

# Calculate the players' age in years
calc_age_years = function(date, date_format) {
  birthday = as.Date(date, date_format)
  today = Sys.Date()
  age_year = round((today - birthday) / 365.25, digits = 1)
  return(age_year)
}

# Define variables for final dataset
Finaldat <- data.frame(player = as.factor(dat$playerShort),
                       height_cm = as.numeric(dat$height),
                       weight_kg = as.numeric(dat$weight),
                       age_yrs = as.numeric(calc_age_years(dat$birthday, "%d.%m.%Y")),
                       # general_pos = as.factor(GeneralPos),
                       specific_pos = as.factor(dat$position),
                       club = as.factor(dat$club),
                       league_country = as.factor(dat$leagueCountry),
                       games = as.numeric(dat$games),
                       goals = as.numeric(dat$goals),
                       victories = as.numeric(dat$victories),
                       ties = as.numeric(dat$ties),
                       # yellow_cards = as.numeric(dat$yellowCards),
                       # yellow_red_cards = as.numeric(dat$yellowReds),
                       # red_cards = as.numeric(dat$redCards),
                       all_reds = as.numeric(dat$allreds),
                       player_cards_received = as.numeric(dat$player_all_cards_received),
                       ref = as.factor(dat$refNum),
                       ref_country = as.factor(dat$refCountry),
                       ref_cards_assigned = as.numeric(dat$ref_all_cards_assigned),
                       # ref_games = as.numeric(dat$refCount),
                       # skin_tone_fct = as.factor(dat$skintone),
                       skin_tone_num = as.numeric(dat$skintone),
                       # no_white = (dat$skintone > 1),
                       imp_bias = as.numeric(centImpBias),
                       exp_bias = as.numeric(centExpBias))


# Save as csv
write.csv(Finaldat, here::here("data", "3_prepared_data.csv"), row.names = FALSE)


# For model selection see TJHAllRedsAnalysis.R
# FootballFinal4i.glmm <- glmer(allreds ~ ContSkinTone*ImpBias +
#                                 league + SpecificPos +
#                                 (1|Ref) + (1|Player),
#                               data = Finaldat, family = "binomial", nAGQ = 0)