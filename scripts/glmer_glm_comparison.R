library(MCMCglmm)
library(lme4)

file_path = "/Users/sebastian/Documents/Uni/Sheffield (MSc)/2. Semester/Research Project/msc_dissertation_project"
setwd(file_path)

library("here")

dat = read.csv(here("data", "final_data_set.csv"))

# based on the redcard_analysisFINAL.R script of the orignal analysis (Team 23)
dat$Player = as.factor(dat$Player)
dat$GeneralPos = as.factor(dat$GeneralPos)
dat$SpecificPos = as.factor(dat$SpecificPos)
dat$FacSkinTone = as.factor(dat$FacSkinTone)
dat$ContSkinTone = as.numeric(dat$ContSkinTone)
# NoWhite as is
dat$ImpBias = as.numeric(dat$ImpBias)
dat$ExpBias = as.numeric(dat$ExpBias)
dat$Ref = as.factor(dat$Ref)
# Red as is
dat$allreds = as.numeric(dat$allreds)
dat$league = as.factor(dat$league)


# generalised mixed-effect model (original name: "FootballFinal4.glmm")
model_start_glmer1 = Sys.time()
glmer1 = glmer(allreds ~ ContSkinTone*ImpBias + ContSkinTone*ExpBias +
                           league + SpecificPos +
                           (1|Ref) + (1|Player),
                         data = dat, family = "binomial", nAGQ = 0)
model_stop_glmer1 = Sys.time()
model_time_glmer1 = model_stop_glmer1 - model_start_glmer1
print(model_time_glmer1)
# Tom's XPS = ... (26.9s?)


# generalised linear model
model_start_glm1 = Sys.time()
glm1 = glm(allreds ~ ContSkinTone*ImpBias + ContSkinTone*ExpBias +
                       league + SpecificPos + Ref + Player,
                     data = dat, family = "binomial")
model_stop_glm1 = Sys.time()
model_time = model_stop_glm1 - model_start_glm1
print(model_time_glm1)
#Tom's XPS = ... (26.9s?)

summary(glmer1)
summary(glm1)
