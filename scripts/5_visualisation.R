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


# Adjusting class for proper visualisation
dat_models$below_alpha = factor(dat_models$below_alpha)

dat_covariates$impact_names = factor(dat_covariates$impact_names)
dat_covariates$below_alpha = factor(dat_covariates$below_alpha)


#............................................# Rain cloud plot #...........................................#

vibration_of_effect = dat_models %>%
  filter(estimate_oddsratio > 0 & estimate_oddsratio <= 4) %>% 
  ggplot(aes(x = "", y = estimate_oddsratio)) +
  geom_flat_violin(aes(fill = ""), trim = FALSE, colour = "dark grey", fill = "dark grey") +
  geom_point(aes(x = 0.6, y = estimate_oddsratio, colour = below_alpha),
             position = position_jitter(width = .03, seed = 123), size = 3, shape = 20, alpha = 0.5, stroke = 0) +
  geom_boxplot(aes(x = 0.81, y = estimate_oddsratio), alpha = 0.5, width = 0.1, colour = "black") +
  geom_hline(yintercept = 1.31, linetype = "dashed", color = "black") +
  annotate("text", x = 1.58, y = 1.375, label = "Silberzahn et al. (2018) - Median OR", color = "black") +
  scale_color_manual(values = c("red", "black")) +
  scale_y_continuous(name = "Odds ratio", breaks = c(seq(1.0, 1.5, 0.05)), limits = c(1, 1.5), expand = c(0, 0)) +
  labs(title = "Vibration of effect due to covariate specification", subtitle = "N = 1,000; Odds ratio (OR)") +
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

vibration_of_effect

ggsave(here::here("figures", "vibration_of_effect.png"), vibration_of_effect, width = 11, height = 5)


#.........................................# Covariate Effects plot #.......................................#

dat_covariates = dat_covariates[order(dat_covariates$estimate_oddsratio), ]

covariate_effects = dat_covariates %>%
  ggplot(aes(x = reorder(x = impact_names, X = impact_coef), impact_coef)) +
  geom_point(aes(color = below_alpha)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = below_alpha), width = 0.1) +
  scale_color_manual(values = c("red", "black")) +
  scale_y_continuous(name = "Estimates\n", labels = scales::comma_format(accuracy = 0.05), breaks = c(seq(-0.15, 0.15, 0.05)), limits = c(-0.15, 0.15)) +
  labs(title = "Specified effect of covatiates", subtitle = "N = 1,000; 95% CI", x = "Covariate") +
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

covariate_effects

ggsave(here::here("figures", "covariate_effects.png"), covariate_effects, width = 11, height = 5)


#................................................# SCA plot #..............................................#

analysed_specifications = analysed_specifications[order(analysed_specifications$estimate_oddsratio), ] # order by odds ratio, low to high
analysed_specifications$id = factor(seq(1:nrow(analysed_specifications))) # assign identifier

# Preparing specification matrix i.e., the bottom part of the SCA plot
prepare_long_df = analysed_specifications[ , c(4:18, 20, 21, 24)] # Subset covariates and statistics
prepare_long_df[prepare_long_df == FALSE] = NA # FALSE ro NA to better subset
long_df = melt(setDT(prepare_long_df), id.vars = c("id", "below_alpha"), variable.name = c("estimate_oddsratio")) # Wide to long format
long_df = long_df[complete.cases(long_df), ] # Exclude NA cases
long_df = long_df[!grepl("estimate_oddsratio", long_df$estimate_oddsratio), ] # Exclude outcome variable
colnames(long_df) = c("id", "below_alpha", "covariate", "x")
long_df = long_df[ , c("id", "below_alpha", "covariate")] # Exclude value column

impact_df = impact_df[order(impact_df$estimate_oddsratio), ] # order covariate effects low to high

long_df$covariate = factor(long_df$covariate,
                           levels = c("club", "victories", "weight_kg", "player_cards_received", "ties", "league_country", "specific_pos", "age_yrs", "ref", "ref_cards_assigned", "goals", "height_cm", "games", "ref_country", "player"),
                           labels = c("Club", "Victories", "Weight", "Cards rec.", "Ties", "Lg. country", "Position", "Age", "Ref", "Cards assig.", "Goals", "Height", "Games", "Ref country", "Player"))


# build plot
top = analysed_specifications %>% 
  filter(id != "1") %>% 
  ggplot(aes(as.numeric(id), estimate_oddsratio)) +
  geom_point(aes(colour = below_alpha), size = 1, alpha = 0.5) +
  geom_ribbon(aes(ymin = ci_lower_oddsratio, ymax = ci_upper_oddsratio), alpha = 0.3) +
  geom_hline(yintercept = 1, color = "black") +
  scale_color_manual(values = c("red", "black")) +
  scale_x_discrete(name = "", expand = c(0.01, 0)) +
  scale_y_continuous(name = "Odds ratio", breaks = c(seq(0.9, 1.7, 0.1)), limits = c(0.9, 1.7), expand = c(0, 0)) +
  labs(title = "Results of the specification-curve analysis", subtitle = "N = 1,000; 95% CI") +
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
  filter(id != "1") %>%
  ggplot(aes(x = id, y = covariate)) +
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
        plot.margin = unit(c(0, 1, 0.5, 1), "cm"),)

sca_plot = plot_grid(top, bottom, ncol = 1, align = "v")
sca_plot

ggsave(here::here("figures", "sca_plot.png"), sca_plot, width = 10, height = 6)


###
median_sca = median(analysed_specifications$estimate_oddsratio)
median_silberzahn = 1.31

ecdf_calculate = ecdf(analysed_specifications$estimate_oddsratio)

ecdf_sca_median = ecdf_calculate(median_sca)
ecdf_silberzahn_median = ecdf_calculate(ecdf_calculate)


analysed_specifications %>%
  filter(estimate_oddsratio < 4) %>% 
  ggplot(aes(estimate_oddsratio)) +
  stat_ecdf(geom = "smooth", pad = FALSE, colour = "black") +
  
  geom_segment(aes(x = median_silberzahn, y = 0, xend = median_silberzahn, yend = ecdf_silberzahn_median), color = "red") +
  geom_segment(aes(x = 1, y = ecdf_silberzahn_median, xend = median_silberzahn, yend = ecdf_silberzahn_median), color = "red") +
  
  geom_segment(aes(x = median_sca, y = 0, xend = median_sca, yend = ecdf_sca_median), color = "red") +
  geom_segment(aes(x = 1, y = ecdf_sca_median, xend = median_sca, yend = ecdf_sca_median), color = "red") +
  
  scale_x_continuous(name = "Odds ratio", breaks = c(seq(1, 1.4, 0.05)), limits = c(1, 1.4), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "light grey"))
