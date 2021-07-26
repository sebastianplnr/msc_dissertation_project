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

# "Base variable"
base_var = c("skin_tone_num")

# Independent variables
iv = c("(1|player)", "height_cm", "weight_kg", "age_yrs", "specific_pos", "club", "league_country", "games", "goals", "victories", "(1|ref)", "ref_country", "ref_games", "imp_bias", "exp_bias")

# All variables needed to conduct specification curve analysis
var.all = c(dv, iv)

# create list including variables
varList = list()
varList[["variables"]] = variables
varList[["dv"]] = dv
varList[["base_var"]] = base_var
varList[["iv"]] = iv
varList[["var.all"]] = var.all


#.................. Prepare specifications: Create data frame from all combinations of covariates .........#
# Create TRUE/FALSE matrix for every combination of covariates
specifications = data.frame(expand.grid(rep(list(0:1), length(iv))) == TRUE)

# Set column names
colnames(specifications) = iv

# Add dependent variable and "base variable"
all_reds = c(rep(TRUE, nrow(specifications)))
skin_tone_num = c(rep(TRUE, nrow(specifications)))

specifications = add_column(specifications, all_reds, .before = iv[1])
specifications = add_column(specifications, skin_tone_num, .before = iv[1])

# Create a list included formula for every covariate combination
base_model = paste(dv[1], "~", base_var)

all_models = apply(specifications[3:ncol(specifications)], # starting from [, 3] as the first two columns being dv and base_var
                   1, # loop through rows
                   function(x) paste(c(base_model, iv[x]), collapse = " + ")) # create formulas

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
num_cores = detectCores() - 2 # Not using all cores due to working memory limitations

# Run models corresponding to their specification as glmer or glm. Parallel processing to decrease running time.
all_models_glmer_results = pbmclapply(ranef_sample, mc.cores = num_cores, function(x) glmer(x, data = dat, family = "binomial", nAGQ = 0))
all_models_glm_results = pbmclapply(ef_sample, mc.cores = num_cores, function(x) glm(x, data = dat, family = "binomial"))

# lapply doesn't keep the name, hence, re-assigning them
names(all_models_glmer_results) = ranef_sample
names(all_models_glm_results) = ef_sample

# Extract estimates from models
extract_estimates = function(model_list, var_estimate) {
  
  model_summary = list()
  estimate = c()
  se = c()
  z_value = c()
  p_value = c()
  
  for(i in 1:length(model_list)) {
    model_summary[[i]] = summary(model_list[[i]])
    
    estimate[i] = model_summary[[i]]$coefficients[var_estimate, "Estimate"]
    se[i] = model_summary[[i]]$coefficients[var_estimate, "Std. Error"]
    z_value[i] = model_summary[[i]]$coefficients[var_estimate, "z value"]
    p_value[i] = model_summary[[i]]$coefficients[var_estimate, "Pr(>|z|)"]
    
  }
  
  model_formula = names(model_list)
  
  results_df = data.frame(cbind(model_formula, estimate, se, z_value, p_value))
  results_df$estimate = as.numeric(results_df$estimate)
  results_df$se = as.numeric(results_df$se)
  results_df$z_value = as.numeric(results_df$z_value)
  results_df$p_value = as.numeric(results_df$p_value)
  
  return(results_df)
}

effects_glmer = extract_estimates(all_models_glmer_results, "skin_tone_num")
effects_glm = extract_estimates(all_models_glm_results, "skin_tone_num")

effects_df = rbind(effects_glmer, effects_glm)

# In case of remote processing: remove models from environment after extracting results due to taking up to much space for file transferring
# rm(all_models_glmer_results)
# rm(all_models_glm_results)

# Set significant level (alpha). Calculate confidence intervals (CI)
alpha_level = 0.05
z = 1.96 # 95% CI

effects_df$below_alpha = with(effects_df, ifelse(p_value < alpha_level, TRUE, FALSE))

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
