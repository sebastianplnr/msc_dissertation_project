# WARNING: The following line of code will remove all objects from the workspace. 
#          Please consider saving your workspace.
rm(list = ls())


#...............................................# Set parameters #..............................................#

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
requiredPackages = c(# "psych", "plotrix", "rstudioapi", "DescTools", # packages needed?
                     "here", "data.table", "tidyverse", "lme4", "parallel", "PupillometryR")

missingPackages = requiredPackages[!requiredPackages %in% installed.packages()[ , "Package"]]

if(length(missingPackages)) {
  install.packages(missingPackages)
}


# Load required packages
invisible(lapply(requiredPackages, require, character.only = TRUE))


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
variables = c("All red cards", # dv
              "Skin tone (num)", "Implicit bias", "Explicit bias", # base var
              "Player position", "Player’s height (in cm)", "Player’s weight (in kg)", "Player’s league country", "Player’s age (in years)", "Goals scored by player", "Player’s club", "Referee’s country", "Referee", "Player’s number of victories", "Number of cards received by player", "Player", "Number of cards awarded by referee", "Number of draws", "Number games") # covar


# Dependent variable
dv = c("all_reds")


# Base variables
base_var = c("skin_tone_num", "imp_bias", "exp_bias")


# Covariates
covar = c("as.factor(specific_pos)", "height_cm", "weight_kg", "as.factor(league_country)", "age_yrs", "goals", "as.factor(club)", "as.factor(ref_country)", "(1|ref)", "victories", "player_cards_received", "(1|player)", "ref_cards_assigned", "ties", "games")


# All variables needed to conduct multiverse analysis
var_all = c(dv, base_var, covar)


# create list including variables
varList = list()
varList[["variables"]] = variables
varList[["dv"]] = dv
varList[["base_var"]] = base_var
varList[["covar"]] = covar
varList[["var_all"]] = var_all



#.................. Prepare specifications: Create data frame from all combinations of covariates .........#
# Create TRUE/FALSE matrix for every combination of covariates
specifications = data.frame(expand.grid(rep(list(0:1), length(covar))) == TRUE)


# Set column names
colnames(specifications) = covar


# Add dependent variable and "base variable"
all_reds = c(rep(TRUE, nrow(specifications)))
skin_tone_num = c(rep(TRUE, nrow(specifications)))
imp_bias = c(rep(TRUE, nrow(specifications)))
exp_bias = c(rep(TRUE, nrow(specifications)))

specifications = add_column(specifications, all_reds, .before = covar[1])
specifications = add_column(specifications, skin_tone_num, .after = "all_reds") # ".after" to make sure the previous column addition worked
specifications = add_column(specifications, imp_bias, .after = "skin_tone_num")
specifications = add_column(specifications, exp_bias, .after = "imp_bias")


# Create base model formula
base_model = paste(dv[1], " ~ ", base_var[1], "*", base_var[2], " + ", base_var[1], "*", base_var[3], sep = "")


# Create a list including a formula for every covariate combination
all_models = apply(specifications[ , (length(dv) + length(base_var) + 1):ncol(specifications)], # starting from first covariate as the first columns are dependent and base variables
                   1, # loop through rows
                   function(x) paste(c(base_model, covar[x]), collapse = " + ")) # create formulas


# Adding the formulas are a dedicated columns to the specification data frame
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
n_sample = 100

ranef_sample =  sample(formula_ranef, n_sample, replace = FALSE)
ef_sample = sample(formula_ef, n_sample, replace = FALSE)


# Preparing parallel processing
num_cores = detectCores() - 2 # Not using all cores due to working memory limitations


# Run models corresponding to their specification as glmer or glm. Parallel processing to decrease running time.
# Calling summary to generate a homogeneous outcome structure which needed for subsetting. Wrap model call within the above-define function.
all_models_glmer_results = mclapply(ranef_sample,
                                    mc.cores = num_cores,
                                    function(x) extract_statistics(summary(glmer(x, data = dat, family = "binomial", nAGQ = 0)), "skin_tone_num"))

all_models_glm_results = mclapply(ef_sample,
                                  mc.cores = num_cores,
                                  function(x) extract_statistics(summary(glm(x, data = dat, family = "binomial")), "skin_tone_num"))


# lapply doesn't keep the name, hence, re-assigning them
names(all_models_glmer_results) = ranef_sample
names(all_models_glm_results) = ef_sample


# Converting list to data frame while retaining item names and combining to one data frame
statistics_glmer = data.frame(do.call(rbind, all_models_glmer_results))
statistics_glm = data.frame(do.call(rbind, all_models_glm_results))

df = rbind(statistics_glmer, statistics_glm)


# Converting row names to formula column and 
df$formula = rownames(df)
rownames(df) = NULL


# Set significant level (alpha). Calculate confidence intervals (CI)
alpha_level = 0.05
z = 1.96 # 95% CI

df$below_alpha = with(df, ifelse(p_value < alpha_level, "Significant", "Non-significant"))

df$ci_lower = with(df, estimate - z*se)
df$ci_upper = with(df, estimate + z*se)


# Calculate odds-ratios i.e., exponentiate
df$estimate_oddsratio = with(df, exp(estimate))
df$ci_lower_oddsratio = with(df, exp(ci_lower))
df$ci_upper_oddsratio = with(df, exp(ci_upper))

write.csv(df, here::here("data", "4_model_outcome_data.csv"), row.names = FALSE)


#............................................# Rain cloud plot #...........................................#

vibration_of_effect = df %>%
  
  filter(estimate_oddsratio > 0 & estimate_oddsratio <= 1.5) %>% 
  
  ggplot(aes(x = "", y = estimate_oddsratio)) +
  
  geom_flat_violin(aes(fill = ""), trim = FALSE, colour = "dark grey", fill = "dark grey") +
  
  geom_point(aes(x = 0.6, y = estimate_oddsratio, colour = below_alpha),
             position = position_jitter(width = .03, seed = 123), size = 3, shape = 20, alpha = 0.6, stroke = 0) +
  
  geom_boxplot(aes(x = 0.81, y = estimate_oddsratio), alpha = 0.5, width = 0.1, colour = "black") +
  
  geom_hline(yintercept = 1.31, linetype = "dashed", color = "black") +
  
  annotate("text", x = 1.58, y = 1.375, label = "Silberzahn et al. (2018) - Median OR", color="black") +
  
  scale_color_manual(values = c("red", "black")) +
  
  scale_y_continuous(name = "Odds ratio", breaks = c(seq(1.0, 1.5, 0.05)), limits = c(1, 1.5), expand = c(0, 0)) +
  
  labs(title = "Vibration of effect due to covariate specification", subtitle = "Odds ratio (OR), n = 200") +
  
  coord_flip() + theme_classic() +
  
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
        legend.position = "top",
        legend.justification = "left",
        legend.title.align = 1,
        legend.text = element_text(size = 11),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.margin = margin(b = -0.735, unit = "cm"),
        legend.box.margin = margin(b = -0.735, unit = "cm"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic")) +
  
  guides(colour = guide_legend(override.aes = list(alpha = 1), reverse = TRUE))

vibration_of_effect

ggsave(here::here("figures", "vibration_of_effect.png"), vibration_of_effect, width = 11, height = 5)


#............................................# Covariates effects #.........................................#

# Build data frame that includes the specifications and the analysis results
analysed_specifications = inner_join(specifications, df, by = "formula")
analysed_specifications = analysed_specifications %>% filter(estimate_oddsratio < 2 & estimate_oddsratio <= 1.5) # Excluding the zero cases makes a big difference: excluded R^2 = 0.9734, included R^2 = 0.3852
analysed_specifications = analysed_specifications[ , c(2:19, 28)] # select variable of interest i.e., covariates and dv

colnames(analysed_specifications) = c("skin_tone_num", "imp_bias", "exp_bias", "specific_pos", "height_cm", "weight_kg", "league_country", "age_yrs", "goals", "club", "ref_country", "ref", "victories", "player_cards_received", "player", "ref_cards_assigned", "ties", "games", "estimate_oddsratio")


# Run linear model to get the specified effects of each covariate (included yes/no)
impact_mod = lm(estimate_oddsratio ~ factor(specific_pos) + factor(height_cm) + factor(weight_kg) + factor(league_country) + factor(age_yrs) + factor(goals) + factor(club) + factor(ref_country) + factor(ref) + factor(victories) + factor(player_cards_received) + factor(player) + factor(ref_cards_assigned) + factor(ties) + factor(games),
                data = analysed_specifications)
impact_summary = summary(impact_mod)


# Extract data from model and save as data frame
impact_coef = data.frame(impact_summary$coefficients)[c(-1), 1]
impact_se = data.frame(impact_summary$coefficients)[c(-1), 2]
impact_pvalue = data.frame(impact_summary$coefficients)[c(-1), 4]
impact_names = c("Position", "Height", "Weight", "Lg. country", "Age", "Goals", "Club", "Ref country", "Ref", "Victories", "Cards rec.", "Player", "Cards assig.", "Ties", "Games")

impact_df = data.frame(cbind(impact_names,
                             impact_coef,
                             impact_se,
                             impact_pvalue))

impact_df$ci_lower = with(impact_df, as.numeric(impact_coef) - z*as.numeric(impact_se))
impact_df$ci_upper = with(impact_df, as.numeric(impact_coef) + z*as.numeric(impact_se))

impact_df$estimate_oddsratio = with(impact_df, round(exp(as.numeric(impact_coef)), digits = 5))
impact_df$ci_lower_oddsratio = with(impact_df, exp(ci_lower))
impact_df$ci_upper_oddsratio = with(impact_df, exp(ci_upper))

impact_df$below_alpha = with(impact_df, ifelse(as.numeric(impact_pvalue) < 0.05, "Significant", "Non-significant"))

write.csv(impact_df, here::here("data", "4_covariate_effect_data.csv"), row.names = FALSE)


# Visualise the specified effect of covariates
covariate_effects = impact_df %>%
  
  ggplot(aes(x = reorder(x = impact_names, X = estimate_oddsratio), estimate_oddsratio)) +
  
  geom_point(aes(color = below_alpha)) +
  
  geom_hline(yintercept = 1) +
  
  geom_errorbar(aes(ymin = ci_lower_oddsratio, ymax = ci_upper_oddsratio, color = below_alpha), width = 0.1) +
  
  scale_color_manual(values = c("black", "red")) +
  
  scale_y_continuous(name = "Odds ratio", breaks = c(seq(0.85, 1.1, 0.05)), limits = c(0.85, 1.1)) +
  
  labs(title = "Specified effect of covatiates", subtitle = "Odds ratio (OR), n = 200", x = "Covariate") +
  
  theme_classic() +
  
  theme(panel.grid.major.y = element_line(colour = "grey"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
        legend.position = "top",
        legend.justification = "left",
        legend.title.align = 1,
        legend.text = element_text(size = 11),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.margin = margin(l = 0.1, b = -0.719, unit = "cm"),
        legend.box.margin = margin(l = 0.1, b = -0.719, unit = "cm"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"))

covariate_effects

ggsave(here::here("figures", "covariate_effects.png"), covariate_effects, width = 11, height = 5)