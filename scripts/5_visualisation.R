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
                     "here", "data.table", "tidyverse", "PupillometryR")

missingPackages = requiredPackages[!requiredPackages %in% installed.packages()[ , "Package"]]

if(length(missingPackages)) {
  install.packages(missingPackages)
}


# Load required packages
invisible(lapply(requiredPackages, require, character.only = TRUE))


# Load data
dat_models = data.frame(fread(here::here("data", "4_model_outcome_data.csv")))
dat_covariates = data.frame(fread(here::here("data", "4_covariate_effect_data.csv")))


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


#.........................................# Covariate Effects plot #.......................................#

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