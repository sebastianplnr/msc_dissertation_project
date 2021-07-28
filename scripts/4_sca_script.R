# WARNING: The following line of code will remove all objects from the workspace. 
#          Please consider saving your workspace.
rm(list = ls()) 


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
                     "here", "data.table", "tidyverse", "lme4", "parallel", "PupillometryR")

missingPackages = requiredPackages[!requiredPackages %in% installed.packages()[ , "Package"]]

if(length(missingPackages)) {
  install.packages(missingPackages)
}


# Load required packages
invisible(lapply(requiredPackages, require, character.only = TRUE))


# Source script containing functions required to run the master script
# source(here("scripts", "my_functions.R"))

# Load data
dat = data.frame(fread(here::here("data", "3_prepared_data.csv")))


#..........................................................................................................#
#..........................................................................................................#
#..................................... Specification Curve Analysis .......................................#
#..........................................................................................................#
#..........................................................................................................#


#...........................................# Preparation #................................................#

#...# Prepare variables needed for specification curve analysis
# Full variable names
variables = c("All red cards",
              "Skin tone (num)", "Implicit bias", "Explicit bias",
              "Player position", "Player’s height (in cm)", "Player’s weight (in kg)", "Player’s league country", "Player’s age (in years)", "Goals scored by player", "Player’s club", "Referee’s country", "Referee", "Player’s number of victories", "Number of cards received by player", "Player", "Number of cards awarded by referee", "Number of draws", "Number games")


# Dependent variables
dv = c("all_reds")


# "Base variable"
base_var = c("skin_tone_num", "imp_bias", "exp_bias")


# Independent variables
co_var = c("as.factor(specific_pos)", "height_cm", "weight_kg", "as.factor(league_country)", "age_yrs", "goals", "as.factor(club)", "as.factor(ref_country)", "(1|ref)", "victories", "player_cards_received", "(1|player)", "ref_cards_assigned", "ties", "games")


# All variables needed to conduct specification curve analysis
var_all = c(dv, base_var, co_var)


# create list including variables
varList = list()
varList[["variables"]] = variables
varList[["dv"]] = dv
varList[["base_var"]] = base_var
varList[["co_var"]] = co_var
varList[["var_all"]] = var_all


#.................. Prepare specifications: Create data frame from all combinations of covariates .........#
# Create TRUE/FALSE matrix for every combination of covariates
specifications = data.frame(expand.grid(rep(list(0:1), length(co_var))) == TRUE)


# Set column names
colnames(specifications) = co_var


# Add dependent variable and "base variable"
all_reds = c(rep(TRUE, nrow(specifications)))
skin_tone_num = c(rep(TRUE, nrow(specifications)))
imp_bias = c(rep(TRUE, nrow(specifications)))
exp_bias = c(rep(TRUE, nrow(specifications)))

specifications = add_column(specifications, all_reds, .before = co_var[1])
specifications = add_column(specifications, skin_tone_num, .after = "all_reds") # ".after" to make sure the previous column addition worked
specifications = add_column(specifications, imp_bias, .after = "skin_tone_num")
specifications = add_column(specifications, exp_bias, .after = "imp_bias")


# Create base model formula
base_model = paste(dv[1], " ~ ", base_var[1], "*", base_var[2], " + ", base_var[1], "*", base_var[3], sep = "")


# Create a list including a formula for every covariate combination
all_models = apply(specifications[ , (length(dv) + length(base_var) + 1):ncol(specifications)], # starting from first covariate as the first columns are dependent and base variables
                   1, # loop through rows
                   function(x) paste(c(base_model, co_var[x]), collapse = " + ")) # create formulas


# adding the formulas are a dedicated columns to the specification data frame
specifications$formula = all_models 


# Identify random effects
all_models_index = grepl("(1|", all_models, fixed = TRUE)


# Divide formulas based on including random effects
formula_ranef = all_models[all_models_index]
formula_ef = all_models[!all_models_index]


# Function extracting estimates, se, z-values and p-value from models.
# Use as wrapper function around the models to extract relevant statistics only and reducing the output size.
extract_statistics = function(model, var_estimate) {
  
  estimate = c()
  se = c()
  z_value = c()
  p_value = c()
  
  estimate = model$coefficients[var_estimate, "Estimate"]
  se = model$coefficients[var_estimate, "Std. Error"]
  z_value = model$coefficients[var_estimate, "z value"]
  p_value = model$coefficients[var_estimate, "Pr(>|z|)"]
  
  results_df = data.frame(cbind(estimate, se, z_value, p_value))
  results_df$estimate = as.numeric(results_df$estimate)
  results_df$se = as.numeric(results_df$se)
  results_df$z_value = as.numeric(results_df$z_value)
  results_df$p_value = as.numeric(results_df$p_value)
  
  return(results_df)
}


# Sample from all model specifications for a "minimum viable product" to work with
n_sample = 2

ranef_sample =  sample(formula_ranef, n_sample, replace = FALSE)
ef_sample = sample(formula_ef, n_sample, replace = FALSE)


# Preparing parallel processing
num_cores = detectCores() - 2 # Not using all cores due to working memory limitations


# Run models corresponding to their specification as glmer or glm. Parallel processing to decrease running time.
# Calling summary to reduce the object size and save storage. Wrap model call within the above-define function.
all_models_glmer_results = mclapply(ranef_sample,
                                    mc.cores = num_cores,
                                    function(x) extract_statistics(summary(glmer(x, data = dat, family = "binomial", nAGQ = 0)), "skin_tone_num"))

all_models_glm_results = mclapply(ef_sample,
                                  mc.cores = num_cores,
                                  function(x) extract_statistics(summary(glm(x, data = dat, family = "binomial")), "skin_tone_num"))


# lapply doesn't keep the name, hence, re-assigning them
names(all_models_glmer_results) = ranef_sample
names(all_models_glm_results) = ef_sample


# Converting list to data frame while retaining item names
statistics_glmer = as.data.frame(do.call(rbind, all_models_glmer_results))
statistics_glm = as.data.frame(do.call(rbind, all_models_glm_results))

df = rbind(statistics_glmer, statistics_glm)


# Set significant level (alpha). Calculate confidence intervals (CI)
alpha_level = 0.05
z = 1.96 # 95% CI

df$below_alpha = with(df, ifelse(p_value < alpha_level, TRUE, FALSE))

df$ci_lower = with(df, estimate - z*se)
df$ci_upper = with(df, estimate + z*se)


# Calculate odds-ratios i.e., exponentiate
df$estimate_oddsratio = with(df, exp(estimate))
df$ci_lower_oddsratio = with(df, exp(ci_lower))
df$ci_upper_oddsratio = with(df, exp(ci_upper))


#............................................# Rain cloud plot #...........................................#

df %>%
  
  ggplot(aes(x = "", y = estimate_oddsratio)) +
  
  geom_flat_violin(aes(fill = ""),
                   position = position_nudge(x = 0.1, y = 0),
                   adjust = 1.5,
                   trim = FALSE,
                   alpha = 0.5, colour = NA) +
  
  geom_point(aes(x = 0.6, # preventing the points from overlapping with the box plot
                 y = estimate_oddsratio,
                 colour = below_alpha),
             position = position_jitter(width = .03, seed = 123), size = 1, shape = 20) +
  
  geom_boxplot(aes(x = 0.851,
                   y = estimate_oddsratio),
               alpha = 0.5,
               width = 0.1,
               colour = "black") +
  
  geom_hline(yintercept = 1.31, 
             linetype = "dashed", 
             color = "black") +
  
  coord_flip() + theme_classic() + labs(y = "Skin tone odds ratio") +
  
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
