# WARNING: The following line of code will remove all objects from the workspace. 
#          Please consider saving your workspace.
rm(list=ls()) 


#................................# Set parameters #..................................#

# This script can be run in its entirety to reproduce the specification curve analysis for one population at a time. 
# Please read the comments in this section to understand what each parameter does.
# After setting the parameters for the first time, we recommend reading the WARNINGS on lines 1 and 56 before
# sourcing the entire script or running the "Prepare R Session" section.


#... Set number of analyses with shuffled data
R <- 1
# This determines how many times the original data will be shuffled in the permutation test
# To reproduce our results, set R to 1000
# For quick, approximate calculations, please set R to, e.g., 5 or 10



#... Define which group of datasets to load
whichData <- "default"
# Please choose one of the following options:
# default: sub-sample mean differences removed (sample mean residualization)
# keyDem: controlling for key demographic variables (SES ladder, age, gender). In the chosen datasets, these variables were partialled out from all contact and social change items.



#... Define population for which specification curve analysis will be conducted
dataset <- "AG"
# In the line above, please indicate for which population the specification curve analysis should be conducted
# Possible options:
# AG       (ethnic majorities)
# HE       (cis-heterosexuals)
# DG       (ethnic minorities)
# SM       (LGBTIQ+ individuals)

# AG_short (ethnic majorities w/o quantity of direct contact and quantity of indirect outgroup friends)
# HE_short (cis-heterosexuals w/o quantity of direct contact and quantity of indirect outgroup friends)
# DG_short (ethnic minorities w/o quantity of direct contact and quantity of indirect outgroup friends)



#... Save specification curve as PDF
pdf <- F    # If FALSE, the plot is displayed in R. If TRUE, the plot is saved as a PDF in your working directory.


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
if(!is.null(sessionInfo()$otherPkgs)){
  invisible(lapply(paste('package:', names(sessionInfo()$otherPkgs), sep = ""), 
                   detach, character.only = T, unload = T))
}

# Download and install missing packages
requiredPackages <- c("psych", "plotrix", "rstudioapi", "DescTools")
missingPackages <- requiredPackages[!requiredPackages %in% installed.packages()[ ,"Package"]]
if(length(missingPackages)){
  install.packages(missingPackages)
}

# Load required packages
invisible(lapply(requiredPackages, require, character.only = T))

# Set working directory to script location (might only work in RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Source script containing functions required to run the master script
source('Functions.R')

# Load data (residualized) with scale variables created in the scale construction script

if(whichData == "default"){
  load("data/CSc_agdataSpec.RData")
  load("data/CSc_hedataSpec.RData")
  load("data/CSc_dgdataSpec.RData")
  load("data/CSc_smdataSpec.RData")
}

if(whichData == "keyDem"){
  load("data/CSc_agdataSpec_key.RData")
  load("data/CSc_hedataSpec_key.RData")
  load("data/CSc_dgdataSpec_key.RData")
  load("data/CSc_smdataSpec_key.RData") 
}


#..........................................................................................................#
#..........................................................................................................#
#......................... Specification Curve Analysis (residualized variables) ..........................#
#..........................................................................................................#
#..........................................................................................................#


#......................................# Preparation #...........................................#

#...# Prepare variables needed for specification curve analysis
varList <- specVars(dataset)

# Full variable names
variables <- varList[["variables"]]

# Support for social change
dv <- varList[["dv"]]

# Intergroup contact variables
iv <- varList[["iv"]]

# Other variables: Attention check 
var.other <- varList[["var.other"]]

# All variables needed to conduct specification curve analysis
var.all <- varList[["var.all"]]


#...# Prepare specified dataset for specification curve analysis 
if(dataset %in% c("AG", "AG_short")){
  dataSpec <- agdataRes
}
if(dataset %in% c("HE", "HE_short")){
  dataSpec <- hedataRes
}
if(dataset %in% c("DG", "DG_short")){
  dataSpec <- dgdataRes
}
if(dataset %in% c("SM")){
  dataSpec <- smdataRes
}

# Select support for social change and intergroup contact variables
data.sel <- dataSpec[paste0(var.all[1:(length(dv) + length(iv))], "_NM")] # Select 

# Center and scale intergroup contact and support for social change variables
data.sel[] <- scale(data.sel[]) 

# Add attention check variable
data.sel$ATTCHK <- dataSpec$ATTCHK

# Set column names to names in var.all
colnames(data.sel) <- var.all


#.......................................................# Specification curve analysis #............................................................#

#.................. Prepare specifications: Create data frame from all combinations of support for social change and intergroup contact variables
specifications <- expand.grid(which(var.all %in% dv),  # Indices of support for social change variables: 1-5
                              which(var.all %in% iv))  # Indices of intergroup contact variables: 6-13 (LGBTIQ+ individuals: 6-11)
names(specifications) <- c("dv","iv")                  # Set column names


#.................. Original dataset: Bivariate correlations, confidence intervals (90%, 95%), meta-regression of effect sizes
# Pearson
x <- new_calc_sc(data = data.sel, 
                 specifications = specifications)
saveSpec(dataset, whichData, x) # file names: e.g., agx.RData (AG), dgx_short.RData (DG_short), agx_key.RData (controlling for key demographic variables)


#...# Descriptives

# Number of model specifications
length(dv)*length(iv)*2*2 # Support for social change variables, intergroup contact variables, two binary analytic decisions

# Effect sizes (bivariate correlations)
effectSize <- psych::describe(x$effect)

# Number of model specifications with positive effects
pos.spec <- length(x$effect[x$effect>0])

# Number of model specifications with negative effects
neg.spec <- length(x$effect[x$effect<0])


#... Check: Spearman rank-order correlation
x.spear <- new_calc_sc(data = data.sel, 
                       meth = "spearman", 
                       specifications = specifications)

# Proportion of specifications which remain significantly negative using spearman (90%; 95%)
mean(x.spear$p[x$effect < 0 & x$p < .1] < .1);mean(x.spear$p[x$effect < 0 & x$p < .05] < .05)

# Proportion of specifications which remain significantly positive using spearman (90%; 95%)
mean(x.spear$p[x$effect > 0 & x$p < .1] < .1);mean(x.spear$p[x$effect > 0 & x$p < .05] < .05)


#.................. R shuffled datasets: Bivariate correlations, confidence intervals (90%, 95%), meta-regression of effect sizes
xR <- new_calc_sc(data = data.sel, 
                  specifications = specifications, 
                  shuffle = TRUE, 
                  repetitions = R)
xR.spear <- new_calc_sc(data = data.sel, 
                        meth = "spearman",
                        specifications = specifications, 
                        shuffle = TRUE, 
                        repetitions = R)


#.................. Joint significance test (Table 2 in the main paper)

# One-sided significance, positive direction (as predicted for ethnic majorities and cis-heterosexuals)
joint.one_pos <- jointSig(numsig = "numsig90pos") # Smallest testable p value: p < 1/R. 
joint.one_pos.spear <- jointSig(numsig = "numsig90pos", orig = x.spear, shuf = xR.spear)
# Explanation of the output:
# sigOriginal: Number of significant model specifications in the original dataset
# sigShuffled: Number of significant model specifications in the shuffled datasets
# moreSigAbs: Number of shuffled datasets with at least as many significant model specifications as the original dataset
# pValue: p-value of the joint significance test

# One-sided significance, negative direction (as predicted for ethnic minorities and LGBTIQ+ individuals)
joint.one_neg <- jointSig(numsig = "numsig90neg")
joint.one_neg.spear <- jointSig(numsig = "numsig90neg", orig = x.spear, shuf = xR.spear)

# Two-sided significance
joint.two <- jointSig(numsig = "numsig95")
joint.two.spear <- jointSig(numsig = "numsig95", orig = x.spear, shuf = xR.spear)


#................... False Discovery Rate (see Benjamini & Yekutieli, 2001)

# Adjust p-values for multiple comparisons
x$pBY <- p.adjust(x$p, "BY")
x.spear$pBY <- p.adjust(x.spear$p, "BY")

# p < .1 (one-sided test for preregistered hypotheses)
BY.one <- BY.adj(x, .1, dataset)
BY.one.spear <- BY.adj(x.spear, .1, dataset)

# p < .05
BY.two <- BY.adj(x, .05, dataset)
BY.two.spear <- BY.adj(x.spear, .05, dataset)

# Proportion of specifications in the predicted direction which remain significant with Benjamini-Yekutieli procedure (90%; 95%)
BY.one[[1]][1,1]/BY.one[[1]][1,2]; BY.two[[1]][1,1]/BY.two[[1]][1,2]
# mean(x$pBY[x$effect > 0 & x$p < .1] < .1);mean(x$pBY[x$effect > 0 & x$p < .05] < .05) # Check for HE, AG
# mean(x$pBY[x$effect < 0 & x$p < .1] < .1);mean(x$pBY[x$effect < 0 & x$p < .05] < .05) # Check for SM, DG
BY.one.spear[[1]][1,1]/BY.one.spear[[1]][1,2]; BY.two.spear[[1]][1,1]/BY.two.spear[[1]][1,2]

# Proportion of specifications in the opposite direction which remain significant with Benjamini-Yekutieli procedure (90%; 95%) (90%; 95%)
BY.one[[2]][1,1]/BY.one[[2]][1,2]; BY.two[[2]][1,1]/BY.two[[2]][1,2]
BY.one.spear[[2]][1,1]/BY.one.spear[[2]][1,2]; BY.two.spear[[2]][1,1]/BY.two.spear[[2]][1,2]


#... just specifications with WRKSO
WRKSO.ADJ <- list(p = x$p[x$combos$WRKSO == 1], 
                  pBY = x$pBY[x$combos$WRKSO == 1], 
                  effect = x$effect[x$combos$WRKSO == 1],
                  numspec = sum(x$combos$WRKSO == 1))
WRKSO.ADJ.spear <- list(p = x.spear$p[x.spear$combos$WRKSO == 1], 
                        pBY = x.spear$pBY[x.spear$combos$WRKSO == 1], 
                        effect = x.spear$effect[x.spear$combos$WRKSO == 1],
                        numspec = sum(x.spear$combos$WRKSO == 1))
# p < .1
BY.one.wrkso <- BY.adj(WRKSO.ADJ, .1, dataset)
BY.one.wrkso.spear <- BY.adj(WRKSO.ADJ.spear, .1, dataset)

# p < .05
BY.two.wrkso <- BY.adj(WRKSO.ADJ, .05, dataset)
BY.two.wrkso.spear <- BY.adj(WRKSO.ADJ.spear, .05, dataset)

# Proportion of specifications in the predicted direction which remain significant with Benjamini-Yekutieli procedure (90%; 95%)
BY.one.wrkso[[1]][1,1]/BY.one.wrkso[[1]][1,2]; BY.two.wrkso[[1]][1,1]/BY.two.wrkso[[1]][1,2]
BY.one.wrkso.spear[[1]][1,1]/BY.one.wrkso.spear[[1]][1,2]; BY.two.wrkso.spear[[1]][1,1]/BY.two.wrkso.spear[[1]][1,2]

# Proportion of specifications in the opposite direction which remain significant with Benjamini-Yekutieli procedure (90%; 95%) (90%; 95%)
BY.one.wrkso[[2]][1,1]/BY.one.wrkso[[2]][1,2]; BY.two.wrkso[[2]][1,1]/BY.two.wrkso[[2]][1,2]
BY.one.wrkso.spear[[2]][1,1]/BY.one.wrkso.spear[[2]][1,2]; BY.two.wrkso.spear[[2]][1,1]/BY.two.wrkso.spear[[2]][1,2]


#..................... Meta-regression

#...# Effect coding: Deviations from the grand mean within a specification factor 

# Original dataset
ose <- x$spec.effects[2:(length(x$spec.effects)-2)]
ose.spear <- x.spear$spec.effects[2:(length(x.spear$spec.effects)-2)] 
c(sort(ose[1:length(dv)], decreasing = T), sort(ose[(length(dv)+1):(length(dv)+length(iv))], decreasing = TRUE))
c(sort(ose.spear[1:length(dv)], decreasing = T), sort(ose.spear[(length(dv)+1):(length(dv)+length(iv))], decreasing = TRUE))


# New (shuffled) datasets
nse <- do.call(rbind.data.frame, xR$spec.effects)[2:(length(x$spec.effects)-2)] 
names(nse) <- names(x$spec.effects)[2:(length(x$spec.effects)-2)]

nse.spear <- do.call(rbind.data.frame, xR.spear$spec.effects)[2:(length(x.spear$spec.effects)-2)]
names(nse.spear) <- names(nse)


#......................................................# Specification curve figure #.....................................................#

# Set order of specification factors, 
# set order of measures within specification factors (from largest to smallest deviation from grand mean effect)
decreasing <- c(order(ose[1:length(dv)], decreasing = TRUE),    # Support for social change variables
                order(ose[(length(dv)+1):(length(dv)+length(iv))], decreasing = TRUE)+length(dv),   # Intergroup contact variables
                (length(dv)+length(iv)+1):length(ose)) # Analytic decisions (binary)


#.................. Plot specification curve
sc_plot(x = x$effect,
        px = 2 * (x$sig90neg + x$sig90pos) - (x$sig95neg + x$sig95pos),
        specifications = x$combos,
        ci1 = x$ci95,
        ci2 = x$ci90,
        names = paste0(variables, paste0("  (", sprintf("%.2f", ose), ")")),
        data = dataset,   # This argument is used to generate the correct plot title; we recommend not adjusting it 
        saveaspdf = pdf,    # If FALSE, the plot is displayed in R. If TRUE, the plot is saved as a PDF in your working directory.
        file = paste0("specification_curve_", dataset), # Name of the PDF (if saveaspdf = TRUE)
        resid = whichData
        )



#..........................................................................................................#
#..........................................................................................................#
#......................................... Supplementary Material  ........................................#
#..........................................................................................................#
#..........................................................................................................#

#...........................# Table S8: Results from Meta-Regression: Deviations from the Grand-Mean #...............................#
#                                      (Conduct meta shuffle to calculate p-values) 

# Calculate how likely it is that an intergroup contact or support for social change variable
# produces a deviation from the grand mean at least as large in the original dataset by chance.
results.metashuffle <- metaShuffle(ose, nse, dv, iv) # p-values
results.metashuffle.spear <- metaShuffle(ose.spear, nse.spear, dv, iv)
# Smallest testable p-values: 1/number of shuffled datasets



#...........................# Supplementary Table9: Numbers and Proportions of Significant Correlations #............................#
BY.adj(x, .1, dataset)
BY.adj(x, .05, dataset)



#...........................# Supplementary Table10: Numbers and Proportions of Significant Correlations for Specifications with Working in Solidarity #............................#
WRKSO.ADJ <- list(p = x$p[x$combos$WRKSO == 1],
                  pBY = x$pBY[x$combos$WRKSO == 1],
                  effect = x$effect[x$combos$WRKSO == 1],
                  numspec = sum(x$combos$WRKSO == 1))
BY.adj(WRKSO.ADJ, .1, dataset)
BY.adj(WRKSO.ADJ, .05, dataset)



#...........................# Table S11: Explained Variance and Cross-validation #............................#

# load results from specification curve analysis
if(whichData == "default"){
  load("data/agx.RData")
  load("data/dgx.RData")
  load("data/hex.RData")
  load("data/smx.RData")
  load("data/agx_short.RData")
  load("data/dgx_short.RData")
  load("data/hex_short.RData")
  load("data/smx_short.RData") # identical with smx.RData
}
if(whichData == "keyDem"){
  load("data/agx_key.RData")
  load("data/dgx_key.RData")
  load("data/hex_key.RData")
  load("data/smx_key.RData")
  load("data/agx_short_key.RData")
  load("data/dgx_short_key.RData")
  load("data/hex_short_key.RData")
  load("data/smx_short_key.RData")
}


#...# Meta-regression

# long datasets including quantity of contact and quantity of indirect OG friends (for all populations except LGBTIQ+)
dg_model_l <- metaReg(data = dgx, short = FALSE) 
ag_model_l <- metaReg(agx, F)
he_model_l <- metaReg(hex, F)
# summary(he_model_l) # use summary() to inspect individual regression models (reference variables: WRKSO, CQUA)

# short datasets not including quantity of contact and quantity of indirect OG friends (for all populations)
dg_model_s <- metaReg(data = dgx_short, short = TRUE)
sm_model_s <- metaReg(smx, T)
ag_model_s <- metaReg(agx_short, T)
he_model_s <- metaReg(hex_short, T)
# summary(he_model_s) # use summary() to inspect individual regression models (reference variables: WRKSO, CQUAL)


#...# Explained Variance and Cross-validation

# Prepare lists of data and models
allData <- list(dg_dat = dgx$xdata, ag_dat = agx$xdata, he_dat = hex$xdata, 
                dg_dat_s = dgx_short$xdata, ag_dat_s = agx_short$xdata, he_dat_s = hex_short$xdata, sm_dat_s = smx$xdata)
allMod <- list(dg_mod = dg_model_l, ag_mod = ag_model_l, he_mod = he_model_l, 
               dg_mod_s = dg_model_s, ag_mod_s = ag_model_s, he_mod_s = he_model_s, sm_mod_s = sm_model_s)


# Calculate cross-validation and print Table S11
tableS11 <- crossVal(dataList = allData, modList = allMod)


#..........................................................................................................#
#..........................................................................................................#
#......................................... Print key results ..............................................#
#..........................................................................................................#
#..........................................................................................................#

# In this section, nothing new is calculated.
# Instead, some key results are printed to the console.

# Explanation of the output:

# Dataset: 
# Dataset for which the specification curve analysis was conducted
# default: sub-sample mean residualization
# keyDem: controlling for key demographic variables

# Participants:
# Number of participants in the selected dataset

# Model Specifications:
# Number of model specifications

# Effect Sizes (Bivariate Correlations): 
# Descriptive statistics of the bivariate correlations in the selected dataset

# Joint Significance Test:
# Results reported in Table 2 in the main article
# Significant correlations: Number of model specifications with significant correlations in the predicted direction (original dataset)
# p-value: p-value of the joint significance test (smallest testable p value: p < 1/R)

# Meta-Regression:
# Results reported in Table S8
# b = unstandardized regression coefficient, p = p-value from the meta shuffle

# Benjamini-Yekutieli:
# Results reported in Table S9
# p < .1: one-sided test, p < .05: two-sided test
# Predicted direction: significant specifications in the predicted direction
# Opposite direction: significant effects in the opposite direction
# adjusted, raw: results based on p-values adjusted for multiple comparisons and raw p-values
# number: number of specifications with significant results in the predicted direction
# proportion: proportion of specificications with significant results in the predicted direction

# Cross-Validation:
# Results reported in Table S11
keyResults()

# Save key results as .txt
cat(capture.output(keyResults()), file = paste0("keyResults", dataset, whichData, "_", R, ".txt"), sep = "\n")

