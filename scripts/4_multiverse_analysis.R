# DISCLAIMER: The running times of the models included in this script are extremely long (14 to 18 hours).
# If you do not want to wait this long, you can load the models output, "4_model_outcomes_data.csv", in line 240,
# please uncomment that line. Please only refrain from running the section "Wrapper function, running models and output wrangling",
# line 152 to 233, all other code is required to be run.


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
requiredPackages = c("here", "data.table", "tidyverse", "lme4", "parallel", "pbmcapply", "PupillometryR", "cowplot")

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



#......................... Prepare specifications: create formulas and draw random sample .................#
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


# Sample from all model specifications accounting for the proportion of random effects to non-random effects in the formulas
n_sample = 1000
prop_ranef = length(all_models[all_models_index]) / (length(all_models[!all_models_index]) + length(all_models[all_models_index]))
prop_ef = length(all_models[!all_models_index]) / (length(all_models[!all_models_index]) + length(all_models[all_models_index]))

ranef_sample =  sample(formula_ranef, n_sample*prop_ranef, replace = FALSE)
ef_sample = sample(formula_ef, n_sample*prop_ef, replace = FALSE)



#........................# Wrapper function, running models and output wrangling #.........................#

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


# Preparing parallel processing
num_cores = detectCores() - 2 # Not using all cores due to working memory limitations


# Run models corresponding to their specification as glmer or glm. Parallel processing to decrease running time.
# Calling summary to generate a homogeneous outcome structure which needed for subsetting. Wrap model call within the above-define function.
all_models_glmer_results = pbmclapply(ranef_sample,
                                      mc.cores = num_cores,
                                      function(x) extract_statistics(summary(glmer(x, data = dat, family = "binomial", nAGQ = 0)), "skin_tone_num"))

all_models_glm_results = pbmclapply(ef_sample,
                                    mc.cores = num_cores,
                                    function(x) extract_statistics(summary(glm(x, data = dat, family = "binomial")), "skin_tone_num"))


# lapply doesn't keep the name, hence, re-assigning them
names(all_models_glmer_results) = ranef_sample
names(all_models_glm_results) = ef_sample


# Converting list to data frame while retaining item names
statistics_glmer = data.frame(do.call(rbind, all_models_glmer_results))
statistics_glm = data.frame(do.call(rbind, all_models_glm_results))


# Combining to one data frame
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


# Calculate proportion of significant effects
sum(df$below_alpha == "Significant") / nrow(df) # = 89.7%


# Save the models output data
write.csv(df, here::here("data", "4_model_outcomes_data.csv"), row.names = FALSE)



#...............................# Process model output of visualisations #.................................#

# In case models, were not run, load their outcomes
# df = read.csv(here::here("data", "4_model_outcomes_data.csv"))


# Build data frame that includes the specifications and the analysis results
analysed_specifications = inner_join(specifications, df, by = "formula")


# Joining results in 5 missing cases, which are identified. Apparently due to a "1" at the formulas' end.
# No apparent systematic error. N = 5, hence, deemed negligible. 
indexing_missing_cases = df$formula %in% analysed_specifications$formula
df[!indexing_missing_cases, ]


# Summary statistics show OR higher than 150; those are excluded as outliers
unsually_high_odds_ratio = df %>% filter(estimate_oddsratio > 100)


# Excluding the seven outliers
analysed_specifications = analysed_specifications %>% filter(estimate_oddsratio < 100)


# Assigning significance labels
## Alpha and z have been defined earlier, but in case that section has not been run, it's redefined here
alpha_level = 0.05
z = 1.96 # 95% CI
analysed_specifications$below_alpha = with(analysed_specifications, ifelse(p_value < alpha_level, "Significant", "Non-significant"))


# Selecting relevant columns for a better overview (covariates and "estimate_oddsratio")
analysed_specifications = analysed_specifications[ , c(base_var, covar, "p_value", "below_alpha", "estimate_oddsratio", "ci_lower_oddsratio", "ci_upper_oddsratio")]


# The original variable names contain variable class e.g., "factor(specific_pos)". Those are removed by assigning "clean" names.
colnames(analysed_specifications) = c("skin_tone_num", "imp_bias", "exp_bias", "specific_pos", "height_cm", "weight_kg", "league_country", "age_yrs", "goals", "club", "ref_country", "ref", "victories", "player_cards_received", "player", "ref_cards_assigned", "ties", "games", "p_value", "below_alpha", "estimate_oddsratio", "ci_lower_oddsratio", "ci_upper_oddsratio")


# Save as csv
write.csv(analysed_specifications, here::here("data", "4_model_outcomes_specifications_merged.csv"), row.names = FALSE)


#............................................# Rain cloud plot #...........................................#

results_space_plot = analysed_specifications %>%
  filter(estimate_oddsratio < 4) %>% 
  ggplot(aes(x = "", y = estimate_oddsratio)) +
  geom_flat_violin(aes(fill = ""), trim = FALSE, colour = "dark grey", fill = "dark grey") +
  geom_point(aes(x = 0.6, y = estimate_oddsratio, colour = below_alpha),
             position = position_jitter(width = .03, seed = 123), size = 3, shape = 20, alpha = 0.5, stroke = 0) +
  geom_boxplot(aes(x = 0.81, y = estimate_oddsratio), alpha = 0.5, width = 0.1, colour = "black") +
  geom_hline(yintercept = 1.31, linetype = "dashed", color = "black") +
  annotate("text", x = 1.58, y = 1.375, label = "Silberzahn et al. (2018) - Median OR", color = "black") +
  scale_color_manual(values = c("red", "black")) +
  scale_y_continuous(name = "Odds ratio", breaks = c(seq(1.0, 1.5, 0.05)), limits = c(0.99, 1.51), expand = c(0, 0)) +
  labs(title = "Results space defined through covariate specification",
       subtitle = paste0("N = ", nrow(analysed_specifications), ", Odds ratio (OR)", collapse = "")) +
  coord_flip() +
  theme_classic() +
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
        legend.margin = margin(b = -0.73, unit = "cm"),
        legend.box.margin = margin(b = -0.73, unit = "cm"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1), reverse = TRUE))

results_space_plot

ggsave(here::here("figures", "results_space_plot.png"), results_space_plot, width = 11, height = 5)


#............................................# Covariates effects #.........................................#

# Run linear model to get the specified effects of each covariate (factor levels: included yes/no)
impact_mod = lm(estimate_oddsratio ~ factor(specific_pos) + factor(height_cm) + factor(weight_kg) + factor(league_country) + factor(age_yrs) + factor(goals) + factor(club) + factor(ref_country) + factor(ref) + factor(victories) + factor(player_cards_received) + factor(player) + factor(ref_cards_assigned) + factor(ties) + factor(games),
                data = analysed_specifications)
impact_summary = summary(impact_mod)


# Extract data from model (without the intercept) and save as data frame
impact_coef = data.frame(impact_summary$coefficients)[c(-1), "Estimate"]
impact_se = data.frame(impact_summary$coefficients)[c(-1), "Std..Error"]
impact_pvalue = data.frame(impact_summary$coefficients)[c(-1), "Pr...t.."]
impact_names = c("Position", "Height", "Weight", "League", "Age", "Goals", "Club", "Ref. country", "Referee", "Victories", "Cards rec.", "Player", "Cards assig.", "Ties", "Games")

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


#.........................................# Covariate effects plot #.......................................#

# Order covariate effects low to high
impact_df = impact_df[order(impact_df$estimate_oddsratio), ]

covariate_effects_plot = impact_df %>%
  ggplot(aes(x = reorder(x = impact_names, X = as.numeric(impact_coef)), y = as.numeric(impact_coef))) +
  geom_point(aes(color = factor(below_alpha))) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = below_alpha), width = 0.1) +
  scale_color_manual(values = c("red", "black")) +
  scale_y_continuous(name = "Estimates\n",
                     labels = scales::comma_format(accuracy = 0.05), breaks = c(seq(-0.15, 0.15, 0.05)), limits = c(-0.15, 0.15)) +
  labs(title = "Specified covariate effects",
       subtitle = paste0("N = ", nrow(analysed_specifications), ", 95% CI", collapse = ""), x = "Covariates") +
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
        plot.subtitle = element_text(face = "italic")) +
  guides(colour = guide_legend(reverse = TRUE))

covariate_effects_plot

ggsave(here::here("figures", "covariate_effects_plot.png"), covariate_effects_plot, width = 11, height = 5)


#................................................# SCA plot #..............................................#
# In order to built the SCA plot, the dataset containing all specifications and the relevant statistics
# first need to be transformed into a long format to be able to create the specification table.
# Moreover, the specifications need an identifier to be able to identically sort the top and bottom plot
# (otherwise the entire SCA plot would not make sense). The sorting depends on the odds ratios from lowest to highest.
# Finally, despite being presented in a different order in the thesis, the SCA plot has to be created last as e.g.,
# order of covariates in the bottom table depended on the outcome of the specified covariate effects analysis.


# Order by odds ratio, low to high and assign identifier
analysed_specifications = analysed_specifications[order(analysed_specifications$estimate_oddsratio), ]
analysed_specifications$id = factor(seq(1:nrow(analysed_specifications)))


# Preparing the bottom part of the SCA plot by transforming wide to long
## Subset covariates and relevant statistics
prepare_long_df = analysed_specifications %>% select(specific_pos:games, id)

## transform wide to long
long_df = melt(data.table(prepare_long_df), "id", variable.names = "covariate")
long_df = long_df[long_df$value == TRUE, c("id", "variable")]

## statistics are still missing (odds ratio and significance)
id_oddsratio_below_alpha = analysed_specifications %>% select(id, estimate_oddsratio, below_alpha, p_value)

## Adding (or joining) the statistics to the long data frame
long_df = data.frame(left_join(long_df, id_oddsratio_below_alpha, by = "id"))


# Assign proper names for visualisation
long_df$variable = factor(long_df$variable,
                           levels = c("club", "victories", "weight_kg", "player_cards_received", "ties", "league_country", "specific_pos", "age_yrs", "ref", "ref_cards_assigned", "goals", "height_cm", "games", "ref_country", "player"),
                           labels = c("Club", "Victories", "Weight", "Cards rec.", "Ties", "League", "Position", "Age", "Referee", "Cards assig.", "Goals", "Height", "Games", "Ref. country", "Player"))


# Covariates should be sorted based in their impact
impact_df = impact_df[order(impact_df$estimate_oddsratio), ]
ordered_covar = impact_df$impact_names


# Build specification curve plot
top = analysed_specifications %>%
  filter(estimate_oddsratio < 4) %>% 
  ggplot(aes(as.numeric(id), estimate_oddsratio)) +
  geom_point(aes(colour = below_alpha), size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = ci_lower_oddsratio, ymax = ci_upper_oddsratio), alpha = 0.3) +
  geom_hline(yintercept = 1, color = "black") +
  scale_color_manual(values = c("red", "black")) +
  scale_x_discrete(name = "", expand = c(0.01, 0)) +
  scale_y_continuous(name = "Odds ratio", breaks = c(seq(0.9, 1.7, 0.1)), limits = c(0.89, 1.71), expand = c(0, 0)) +
  labs(title = "Specification curve",
       subtitle = paste0("N = ", nrow(analysed_specifications), ", 95% CI", collapse = "")) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(colour = "grey"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        plot.margin = unit(c(0.5, 1, 0, 1), "cm"),
        legend.position = "top",
        legend.title.align = 1,
        legend.text = element_text(size = 11),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.margin = margin(t = -0.5, b = -0.2, unit = "cm"),
        legend.box.margin = margin(t = -0.5, b = -0.2, unit = "cm"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic", margin = margin(0, 0, 20, 0))) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1), reverse = TRUE))


bottom = long_df %>%
  ggplot(aes(x = id, y = factor(variable, levels = ordered_covar))) +
  geom_tile(aes(fill = below_alpha), width = 0.5, height = 0.5, color = "white") +
  scale_fill_manual(values = c("red", "black")) +
  scale_x_discrete(name = "Specifications", expand = c(0.01, 0)) +
  scale_y_discrete(name = "Covariates", expand = c(0.03, 0)) +
  theme_classic() +
  theme(legend.position = "none", 
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0, 1, 0.5, 1), "cm"))

sca_plot = plot_grid(top, bottom, ncol = 1, align = "v")
sca_plot

ggsave(here::here("figures", "sca_plot.png"), sca_plot, width = 10, height = 6)


#..............................................# Volcano plot #............................................#

latent_structure_plot = analysed_specifications %>% 
  filter(estimate_oddsratio < 4) %>% 
  ggplot(aes(estimate_oddsratio, log10(p_value))) +
  geom_point(aes(colour = below_alpha), alpha = 0.3) +
  scale_x_continuous(name = "Odds ratios", breaks = c(seq(1.0, 1.5, 0.05)), limits = c(0.99, 1.51), expand = c(0, 0)) +
  scale_y_continuous(name = "Log10 p-value") +
  scale_color_manual(values = c("red", "black")) +
  labs(title = "Systemtic latent structures",
       subtitle = paste0("N = ", nrow(analysed_specifications), collapse = "")) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "grey"),
        plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
        legend.position = "top",
        legend.justification = "right",
        legend.title.align = 1,
        legend.text = element_text(size = 10),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.margin = margin(t = -0.6, b = -0.2, unit = "cm"),
        legend.box.margin = margin(t = -0.6, b = -0.2, unit = "cm"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic", margin = margin(0, 0, 20, 0)),
        text = element_text(size = 10)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1), reverse = TRUE))

latent_structure_plot

ggsave(here::here("figures", "latent_structure_plot.png"), latent_structure_plot, width = 10, height = 6)