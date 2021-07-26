# WARNING: The following line of code will remove all objects from the workspace. 
#          Please consider saving your workspace.
rm(list=ls()) 


#................................# Set parameters #..................................#

# This script can be run in its entirety to reproduce the specification curve analysis for one population at a time. 
# Please read the comments in this section to understand what each parameter does.
# After setting the parameters for the first time, we recommend reading the WARNINGS on lines 1 and 56 before
# sourcing the entire script or running the "Prepare R Session" section.

#... Set seed for random number generation
set.seed(999999) # set to 999999 to reproduce the results reported in the paper


#...........................................# Prepare R Session #...............................................#

# WARNING: All currently loaded packages will be detached and packages needed 
#          to run this script will be loaded. 
#          Missing packages will be downloaded and installed.
#          Also, the working directory will be automatically set to the location of this script.
#          If you did not alter the structure of the folder containing this script and the data,
#          in most cases you should be able to run this script without any manual adjustments 
#          in a current version of RStudio (tested in RStudio Version 1.1.453 and newer releases running 
#          R Version 3.5.1 and newer releases).


# Detach packages
if(!is.null(sessionInfo()$otherPkgs)) {
  invisible(lapply(paste("package:", names(sessionInfo()$otherPkgs), sep = ""), 
                   detach, character.only = TRUE, unload = TRUE))
}

# Download and install missing packages
requiredPackages = c("psych", "plotrix", "rstudioapi", "DescTools", # packages needed?
                     "here", "data.table", "tidyverse", "lme4", "parallel", "pbmcapply", "PupillometryR")

missingPackages = requiredPackages[!requiredPackages %in% installed.packages()[ , "Package"]]

if(length(missingPackages)) {
  install.packages(missingPackages)
}

# Load required packages
invisible(lapply(requiredPackages, require, character.only = TRUE))

# Source script containing functions required to run the master script
# source(here("scripts", "my_functions.R"))

# Load data
dat = data.frame(fread(here("data", "3_prepared_data.csv")))


#..........................................................................................................#
#..........................................................................................................#
#..................................... Specification Curve Analysis .......................................#
#..........................................................................................................#
#..........................................................................................................#


#...........................................# Preparation #................................................#

#...# Prepare variables needed for specification curve analysis
# Full variable names
variables = c("Name", "Height (in cm)", "Weight (in kg)", "Age (in years)", "Specific position", "Club", "League country", "Number of games", "Number of goals", "Number of victories", "Yellow cards", "Yellow/red cards", "Red cards", "all_reds",  "Referee ID", "Referee country", "Referee number of games", "Implicit bias", "Explicit bias")

# Dependent variables
dv = c("all_reds", "red_cards", "yellow_red_cards", "yellow_cards")

# Independent variables
iv = c("(1|player)", "height_cm", "weight_kg", "age_yrs", "specific_pos", "club", "league_country", "games", "goals", "victories", "(1|ref)", "ref_country", "ref_games", "imp_bias", "exp_bias")

# All variables needed to conduct specification curve analysis
var.all = c(dv, iv)

# create list including variables
varList = list()
varList[["variables"]] = variables
varList[["dv"]] = dv
varList[["iv"]] = iv
varList[["var.all"]] = var.all


#.................. Prepare specifications: Create data frame from all combinations of covariates .........#
# Create TRUE/FALSE matrix for every combination of covariates
specifications = expand.grid(rep(list(0:1), length(iv))) == TRUE

# Set column names
colnames(specifications) = iv

# Create a list included formula for every (standardised) covariate combination
all_models = apply(specifications, 1, function(x) paste(c("all_reds ~ skin_tone_num", iv[x]), collapse = " + "))

# Identify random effects
all_models_index = grepl("(1|", all_models, fixed = TRUE)

# Divide formulas based on including random effects
formula_ranef = all_models[all_models_index]
formula_ef = all_models[!all_models_index]

# Sample from all model specifications
n_sample = 10

ranef_sample =  sample(formula_ranef, n_sample, replace = FALSE)
ef_sample = sample(formula_ef, n_sample, replace = FALSE)

# preparing parallel processing
num_cores = detectCores() - 2 # keep one core of other tasks

# Run models corresponding to their specification as glmer or glm. Parallel processing to decrease running time.
all_models_glmer_results = pbmclapply(ranef_sample, mc.cores = num_cores, function(x) glmer(x, data = dat, family = "binomial", nAGQ = 0))
all_models_glm_results = pbmclapply(ef_sample, mc.cores = num_cores, function(x) glm(x, data = dat, family = "binomial"))

# lapply doesn't keep the name, hence, re-assigning them
names(all_models_glmer_results) = ranef_sample
names(all_models_glm_results) = ef_sample

# Extract estimates from models. Glmer and glm have different output structures, hence, separate functions are applied.
# Thought their only difference is "fixef" vs. "coef" to extract the estimates.
extract_fixef = function(x) {
  estimate = c()
  se = c()
  
  for(i in 1:length(x)) {
    estimate[i] = fixef(x[[i]])["skin_tone_num"]
    se[i] = sqrt(vcov(x[[i]])["skin_tone_num", "skin_tone_num"])
  }
  
  results_df = data.frame(cbind(names(x), estimate, se))
  results_df$estimate = as.numeric(results_df$estimate)
  results_df$se = as.numeric(results_df$se)
  
  return(results_df)
} # extract estimates from glmer


extract_ef = function(x) {
  estimate = c()
  se = c()
  
  for(i in 1:length(x)) {
    estimate[i] = coef(x[[i]])["skin_tone_num"]
    se[i] = sqrt(vcov(x[[i]])["skin_tone_num", "skin_tone_num"])
  }
  
  results_df = data.frame(cbind(names(x), estimate, se))
  results_df$estimate = as.numeric(results_df$estimate)
  results_df$se = as.numeric(results_df$se)
  
  return(results_df)
} # extract estimates from glm


effects_glmer = extract_fixef(all_models_glmer_results)
effects_glm = extract_ef(all_models_glm_results)

effects_df = rbind(effects_glmer, effects_glm)

# Calculate confidence intervals (CI)
z = 1.96 # 95% CI

effects_df$ci_lower = with(effects_df, estimate - z*se)
effects_df$ci_upper = with(effects_df, estimate + z*se)

# Calculate odds-ratios i.e., exponentiate
effects_df$estimate_oddsratio = with(effects_df, exp(estimate))
effects_df$ci_lower_oddsratio = with(effects_df, exp(ci_lower))
effects_df$ci_upper_oddsratio = with(effects_df, exp(ci_upper))


#............................................# Rain cloud plot #...........................................#

effects_df %>%
  
  ggplot(aes(x = "", y = estimate_oddsratio)) +
  
  geom_flat_violin(aes(fill = ""),
                   position = position_nudge(x = 0.1, y = 0),
                   adjust = 1.5,
                   trim = FALSE,
                   alpha = 0.5, colour = NA) +
  
  geom_point(aes(x = 0.6, # preventing the points from overlapping with the box plot
                 y = estimate_oddsratio),
             position = position_jitter(width = .03, seed = 123), size = 1, shape = 20) +
  
  geom_boxplot(aes(x = 0.851,
                   y = estimate_oddsratio),
               alpha = 0.5,
               width = 0.1,
               colour = "black") +
  
  geom_hline(yintercept = 1.31, 
             linetype = "dashed", 
             color = "black") +
  
  coord_flip() + theme_classic() + 
  
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  
  labs(y = "Skin tone odds ratio")