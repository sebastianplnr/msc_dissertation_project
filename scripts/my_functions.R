#..........................................................................................................#
#..........................................................................................................#
#................................ Specification Curve Analysis ............................................#
#..........................................................................................................#
#..........................................................................................................#

#...# Prepare variables needed for specification curve analysis
specVars <- function(whichData) {
  varList <- list()
  
  # Full variable names / descriptions (variables)
  varList[["variables"]] <- c("Working in solidarity",           
                              "Support empowering policies",
                              "Raising IG awareness",
                              "High cost collective action",
                              "Low cost collective action",
                              "Quantity of contact",
                              "Positive contact",
                              "Absence of negative contact",
                              "Number of OG friends",
                              "Frequency of meeting OG friends",
                              "Quantity of indirect OG friends",
                              "Positive indirect contact",
                              "Absence of negative indirect contact",
                              "Exclusion of attention check failures",
                              "Exclusion of statistical outliers")
  
  # Support for social change variables (dv)
  varList[["dv"]] <- c("WRKSO","SEMP","IDISC", "WILAC_D","WILAC_E")
  
  # Intergroup contact variables (iv)
  varList[["iv"]] <- c("CQUA", "CQUAL", "NEG_R", "NFRI","FFRI", "EXTC_CQUA", "EXTC_CQUAL", "EXTC_NEG_R") 
  
  # Other variables: Attention check (var.other)
  varList[["var.other"]] <- c("ATTCHK")  
  
  # Adapt "variables" and "iv" for datasets without quantity of contact and quantity of indirect OG friends 
  # (SM, HE_short, AG_short, DG_short)
  if (whichData %in% c("SM", "HE_short", "AG_short", "DG_short")){
    varList[["variables"]] <- varList[["variables"]][!varList[["variables"]] %in% c("Quantity of contact", "Quantity of indirect OG friends")]
    varList[["iv"]] <- varList[["iv"]][!varList[["iv"]] %in% c("CQUA", "EXTC_CQUA")]
  } 
  
  # All variables needed to conduct specification curve analysis
  varList[["var.all"]] <- c(varList[["dv"]], varList[["iv"]], varList[["var.other"]])
  
  # Return list
  return(varList)
} # delete not needed


#...# Calculate mean bivariate correlations
calc_mean_r <- function(data, meth = NULL){
  if (meth == "spearman") output <- as.numeric(c(SpearmanRho(data[,1], data[,2]),
                                                 SpearmanRho(data[,1], data[,2], conf.level = .95)[c(2, 3)],
                                                 SpearmanRho(data[,1], data[,2], conf.level = .90)[c(2, 3)],
                                                 suppressWarnings(cor.test(data[, 1], data[, 2], method = "spearman")$p.value))) # suppress warning re exact p-values 
  else output <- c(as.numeric(cor.test(data[, 1] ,data[, 2])$estimate),
                   cor.test(data[, 1], data[, 2])$conf.int[c(1, 2)],
                   cor.test(data[, 1], data[, 2], conf.level = .90)$conf.int[c(1, 2)],
                   cor.test(data[, 1], data[, 2])$p.value)
  return(output)
} # deleted not needed


#...# Remove outliers (+- 3*IQR)
remove_outliers <- function(data, columns) # deleted not needed
 {
   outliers <- matrix(FALSE, ncol = length(columns), nrow = nrow(data))
   for (col in columns)
   {
     # calculate boxplot
     box <- boxplot(data[, col], plot = FALSE)
     
     # extract quartiles and interquartile range
     q <- box$stats[c(2, 4)]
     iqr <- diff(q)
     
     # determine outliers
     if (iqr > 0) outliers[, col] <- data[, col] <= q[1] - 3 * iqr | data[,col] >= q[2] + 3 * iqr
   }
   
   # which rows do not contain outliers?
   no_outliers <- apply(outliers,1,function(x) all(!x | is.na(x)))
   data.no_outliers <- data[no_outliers, ]
   return(data.no_outliers)
 }


#...# Calculate main (and interaction) effects of variable inclusion (effect-coded)
coef_func <- function(xdata, dv, iv){
  
  # separate reference category from dvs
  dv_ref <- dv[1]
  dv <- dv[-1]
  
  # separate reference category from ivs
  iv_ref <- iv[1]
  iv <- iv[-1]  
  
  # effect variable
  ev <- names(xdata)[1]
  
  # other variables
  ov <- names(xdata)[!names(xdata) %in% c(ev, dv_ref, dv, iv_ref, iv)]
  
  
  # Formula1: Main effects
  formula1 <- as.formula(paste(ev,
                               "~ (",
                               paste(dv, collapse = "+"),
                               ") + (",
                               paste(iv, collapse = "+"),
                               ") +",
                               paste(ov, collapse = "+")))
  
  # Formula2: Main effects and interaction
  formula2 <- as.formula(paste(ev,
                               "~ (",
                               paste(dv, collapse = "+"),
                               ") * (",
                               paste(iv, collapse = "+"),
                               ") +",
                               paste(ov, collapse = "+")))
  
  # regression models  
  mod1 <- lm(formula1, data = xdata)
  mod2 <- lm(formula2, data = xdata)
  
  modcof <- c(mod1$coef,summary(mod1)$r.squared,summary(mod2)$r.squared)
  modcof <- c(as.numeric(modcof[1]), # intercept
              sum(as.numeric(modcof[dv]*(-1))), as.numeric(modcof[dv]), # dependent variables
              sum(as.numeric(modcof[iv]*(-1))), as.numeric(modcof[iv]), # independent variables 
              as.numeric(modcof[ov]), # attchk and no_outliers 
              as.numeric(modcof[length(modcof) - 1]), as.numeric(modcof[length(modcof)])) # r.squared
  names(modcof) <- c(ev, dv_ref, dv, iv_ref, iv, ov, "mod1.r.squared","mod2.r.squared")
  return(list(coef = modcof,
              formula1 = formula1,
              formula2 = formula2)) 
}



#...# Specification curve analysis: main function 
new_calc_sc <- function(data, specifications, shuffle = FALSE, repetitions = 1,  meth = "pearson")
{
  
  if (!shuffle) repetitions <- 1
  
  res.list <- list()
  
  for (repetition in 1:repetitions)
  {
    # generate code table for all combinations of variables (specification)
    nvar <- ncol(data) 
    combos <- as.data.frame(matrix(NA, ncol = nvar, nrow = nrow(specifications)))
    res <- list()
    
    # Shuffle
    if (shuffle){
      Y<-data[, 1:5]                 # Separate block of dependent variables from
      X<-data[, 6:ncol(data)]        # block of predictor variables,
      X<-X[order(rnorm(nrow(X))), ]  # randomly reorder the rows of the predictor values,
      tempdat<-cbind(Y, X)           # put dependent and predictor variables together again.
    } else {
      data
    } 
    
    for (i in 1:nrow(specifications))
    {
      # initialize binary vector
      x <- vector(mode = "numeric", length = ncol(combos))
      
      # set flags for selected variables
      x[as.numeric(specifications[i, ])] <- 1
      
      # generate temporary dataframe for codes
      m <- as.data.frame(matrix(rep(x, 4), ncol = ncol(combos), byrow = TRUE))
      
      # add flags for attention check and outlier specifications
      m[, ncol(m)-1] <-  c(0, 1, 0, 1)      # attchk       
      m[, ncol(m)] <- c(0, 0, 1, 1)     # no_outliers 
      
      # place into code table
      combos[((i - 1)*4 + 1):(i*4), ] <- m
      
      # get model coefficients for specification
      tempdatsub <- tempdat[, as.numeric(specifications[i, 1:2])]  
      tempdatsub <- cbind(tempdatsub, tempdat$ATTCHK)
      names(tempdatsub)[3] <- c("ATTCHK")
      
      
      # remove lines with any vars=NA from tempdat
      tempdatsub <- tempdatsub[complete.cases(tempdatsub), ]
      
      
      # data from participants who passed the attention check
      tempdatsub.attchk <- tempdatsub[tempdatsub$ATTCHK == 1,]
      
      
      # data without outliers
      tempdatsub.no_outliers <- remove_outliers(tempdatsub, columns = c(1:2))
      
      
      # data from participants who passed the attention check without outliers
      tempdatsub.attchk.no_outliers <- remove_outliers(tempdatsub.attchk, columns = c(1:2))
      
      # calculate correlations and confidence intervals
      res[[((i-1)*4 + 1)]] <- calc_mean_r(tempdatsub, meth = meth)
      res[[((i-1)*4 + 2)]] <- calc_mean_r(tempdatsub.attchk, meth = meth)
      res[[((i-1)*4 + 3)]] <- calc_mean_r(tempdatsub.no_outliers, meth = meth)
      res[[((i-1)*4 + 4)]] <- calc_mean_r(tempdatsub.attchk.no_outliers, meth = meth)
      
    }  
    
    # set column names
    names(combos) <- names(data)
    names(combos)[length(names(combos))] <- "NO_OUTLIERS"
    
    # significance (ci1 (95%) does not contain 0)
    ci95 <- lapply(res, function(x) x[2:3])
    ci90 <- lapply(res, function(x) x[4:5])
    sig95neg <- unlist(lapply(ci95, function(x) (x[1] < 0 & x[2] < 0)))
    sig95pos <- unlist(lapply(ci95, function(x) (x[1] > 0 & x[2] > 0)))
    sig90neg <- unlist(lapply(ci90, function(x) (x[1] < 0 & x[2] < 0)))
    sig90pos <- unlist(lapply(ci90, function(x) (x[1] > 0 & x[2] > 0)))
    p <- sapply(res, function(x) x[6])
    
    # build effect-coded combos
    effectcodes <- combos
    e1 <- min(specifications$dv)
    e2 <- min(specifications$iv)
    l1 <- length(unique(specifications$dv))
    l2 <- length(unique(specifications$iv))
    effectcodes[effectcodes[, e1] == 1, unique(specifications$dv)[unique(specifications$dv) != e1]] <- (-1) # dv variables
    effectcodes[effectcodes[, e2] == 1, unique(specifications$iv)[unique(specifications$iv) != e2]] <- (-1) # iv variables
    effectcodes[effectcodes[, l1 + l2 + 1] == 0, l1 + l2 + 1] <- (-1) # attention check
    effectcodes[effectcodes[, l1 + l2 + 2] == 0, l1 + l2 + 2] <- (-1) # no outliers
    
    x.effect <- unlist(lapply(res,function(x) x[1]))
    
    xdata <- data.frame(x.effect, effectcodes)
    
    # get effects of variable inclusion in specification
    spec.res <- coef_func(xdata,
                          dv = names(data[unique(specifications$dv)]),
                          iv = names(data[unique(specifications$iv)]))
    spec.effects <- spec.res$coef
    formula1 <- spec.res$formula1
    formula2 <- spec.res$formula2
    
    
    # Fill res.list with results
    res.list$effect[[repetition]] <- x.effect
    res.list$spec.effects[[repetition]] <- spec.effects
    res.list$formula1[[repetition]] <- formula1
    res.list$formula2[[repetition]] <- formula2
    res.list$xdata[[repetition]] <- xdata
    res.list$ci95[[repetition]] <- ci95
    res.list$ci90[[repetition]] <- ci90
    res.list$SE[[repetition]] <- NA
    res.list$sig95neg[[repetition]] <- sig95neg
    res.list$sig95pos[[repetition]] <- sig95pos
    res.list$sig90neg[[repetition]] <- sig90neg
    res.list$sig90pos[[repetition]] <- sig90pos
    res.list$numsig95neg[[repetition]] <- sum(res.list$sig95neg[[repetition]])
    res.list$numsig95pos[[repetition]] <- sum(res.list$sig95pos[[repetition]])
    res.list$numsig90neg[[repetition]] <- sum(res.list$sig90neg[[repetition]])
    res.list$numsig90pos[[repetition]] <- sum(res.list$sig90pos[[repetition]])
    res.list$numsig95[[repetition]] <- res.list$numsig95neg[[repetition]] + res.list$numsig95pos[[repetition]]
    res.list$numsig90[[repetition]] <- res.list$numsig90neg[[repetition]] + res.list$numsig90pos[[repetition]]
    if (shuffle) print(paste("Repetition", repetition, "of", repetitions))
  }    
  
  # Return results for shuffled datasets
  if (shuffle)
  {
    return(res.list)
    
    # Prepare and return results for original dataset
  } else
  {
    
    res <- res.list$effect[[1]]
    ci95 <- res.list$ci95[[1]]
    ci90 <- res.list$ci90[[1]]
    sig95neg <- res.list$sig95neg[[1]]
    sig95pos <- res.list$sig95pos[[1]]
    sig90neg <- res.list$sig90neg[[1]]
    sig90pos <- res.list$sig90pos[[1]]
    
    
    # set column names
    names(combos) <- names(data)
    names(combos)[length(names(combos))] <- "NO_OUTLIERS"
    
    
    sfactors <- as.data.frame(matrix(0, ncol = 2, nrow = 4*nrow(specifications)))
    names(sfactors) <- paste0("Var",c(1:2))
    sfactors$Var1 <- as.factor(names(combos)[rep(specifications$dv, each = 4)])
    sfactors$Var2 <- as.factor(names(combos)[rep(specifications$iv, each = 4)])
    sfactors <- cbind(sfactors, as.factor(combos$ATTCHK), as.factor(combos$NO_OUTLIERS))
    names(sfactors)[c(3, 4)] <- c("ATTCHK", "NO_OUTLIERS")
    
    # Fill result with results
    result <- list()
    result$data <- data
    result$effect <- res
    result$p <- p
    result$spec.effects <- res.list$spec.effects[[1]]
    result$formula1 <- res.list$formula1[[1]]
    result$formula2 <- res.list$formula2[[1]]
    result$xdata <- res.list$xdata[[1]]
    result$sig95neg <- sig95neg
    result$sig95pos <- sig95pos
    result$sig90neg <- sig90neg
    result$sig90pos <- sig90pos
    result$numsig95neg <- sum(sig95neg)
    result$numsig95pos <- sum(sig95pos)
    result$numsig90neg <- sum(sig90neg)
    result$numsig90pos <- sum(sig90pos)
    result$numsig95 <- sum(sig95neg) + sum(sig95pos)
    result$numsig90 <- sum(sig90neg) + sum(sig90pos)
    result$ci95 <- ci95
    result$ci90 <- ci90
    result$numspec <- length(sig95neg)
    result$specifications <- specifications
    result$sfactors <- sfactors
    result$combos <- combos
    return(result)
  }
} 


#...# Save spec curve results of original dataset (x) as RData file
saveSpec <- function(data, resid, results){
  if(resid == "default"){
    if (data == "AG"){
      agx <- results
      save(agx, file = "data/agx.RData")}
    if (data == "DG"){
      dgx <- results
      save(dgx, file = "data/dgx.RData")}
    if (data == "HE"){
      hex <- results
      save(hex, file = "data/hex.RData")}
    if (data == "SM"){
      smx = smx_short <- results
      save(smx, file = "data/smx.RData")
      save(smx_short, file = "data/smx_short.RData")}
    if (data == "AG_short"){
      agx_short <- results
      save(agx_short, file = "data/agx_short.RData")}
    if (data == "DG_short"){
      dgx_short <- results
      save(dgx_short, file = "data/dgx_short.RData")}
    if (data == "HE_short"){
      hex_short <- results
      save(hex_short, file = "data/hex_short.RData")}
  }
  if(resid == "keyDem"){
    if (data == "AG"){
      agx <- results
      save(agx, file = "data/agx_key.RData")}
    if (data == "DG"){
      dgx <- results
      save(dgx, file = "data/dgx_key.RData")}
    if (data == "HE"){
      hex <- results
      save(hex, file = "data/hex_key.RData")}
    if (data == "SM"){
      smx = smx_short <- results
      save(smx, file = "data/smx_key.RData")
      save(smx_short, file = "data/smx_short_key.RData")}
    if (data == "AG_short"){
      agx_short <- results
      save(agx_short, file = "data/agx_short_key.RData")}
    if (data == "DG_short"){
      dgx_short <- results
      save(dgx_short, file = "data/dgx_short_key.RData")}
    if (data == "HE_short"){
      hex_short <- results
      save(hex_short, file = "data/hex_short_key.RData")}
  }
}


#...# Joint significance test
jointSig <- function(numsig, orig = x, shuf = xR) {
  sigOriginal <- orig[[numsig]]   # Number of significant model specifications in the original dataset
  sigShuffled <- shuf[[numsig]]   # Number of significant model specifications in the shuffled datasets
  moreSigAbs <- sum(sigShuffled >= sigOriginal) # Number of shuffled datasets with more significant model specifications than the original dataset
  pValue <- moreSigAbs / R # Proportion of shuffled datasets with more significant model specifications than the original dataset
  pValue <- ifelse(pValue == 0, paste("<", 1/R), pValue)
  jointList <- list(sigOriginal, sigShuffled, moreSigAbs, pValue)
  names(jointList) <- c("sigOriginal", "sigShuffled", "moreSigAbs", "pValue")
  return(jointList)
}


#...# Specification curve plot
sc_plot <- function(x, 
                    px, 
                    specifications, 
                    ci1=NA, 
                    ci2=NA, 
                    col = c("red","lightsalmon"),
                    legend = TRUE,
                    legendx = NA,
                    legendy = NA,
                    barspace = 0.4,       # space between bars (as a fraction of bar width)
                    margins = c(1,12,1,1),
                    names = NULL,
                    sorted = TRUE,
                    lines = c(length(dv), length(dv) + length(iv), length(dv) + length(iv) + 1),
                    lineseq = decreasing,
                    linesleft = -90,      # left end of separator lines
                    gridlines = TRUE,     # draw horizontal lines at y-ticks
                    p.level = .1,         # not used for confidence intervals
                    text.offset = 3,      # distance of variable names from y-axis
                    pointlinestop = -0.5, # y-position of first dashboard line
                    pointlinespace = .09, # distance between dashboard lines
                    tcex = .8,            # text size
                    pcex = 0.2,           # size of effect marks
                    lcex = 0.9,           # size of legend text
                    acex = .8,            # text size for y-axis ticks
                    x.offset = 3,         # distance of dashboard from y-axis (minimum 1)
                    topmargin = 0,        # we recommend leaving this value at 0
                    bottommargin = .2,
                    ylim = c(-.4, .9),    # default = NA
                    ylab = "Correlation Coefficient",
                    data = NA,
                    saveaspdf = FALSE,
                    file = "sc_plot",
                    resid)
{
  
  if (saveaspdf){
    if(resid == "keyDem"){
      file <- paste0(file, "_key")
    }
    pdf(file=paste0(file, ".pdf"), width = 12, height = 8)
  } 
  
  if (!any(is.na(margins))) par(mar = margins)
  
  if (is.null(names)) names <- names(specifications)
  
  if (is.na(pointlinestop)) 
  {
    pointlinestop <- round(min(unlist(ci1))*1.1,2) 
  }
  
  if (is.na(ylab)) ylab = ""
  
  n.spec <- ncol(specifications)
  
  specifications.plot <- specifications; if (sorted) specifications.plot <- specifications[order(x),] 
  
  ci1.u <- unlist(lapply(ci1, function(x) x[2])); if (sorted) ci1.u <- ci1.u[order(x)]
  ci1.l <- unlist(lapply(ci1, function(x) x[1])); if (sorted) ci1.l <- ci1.l[order(x)]
  ci2.u <- unlist(lapply(ci2, function(x) x[2])); if (sorted) ci2.u <- ci2.u[order(x)]
  ci2.l <- unlist(lapply(ci2, function(x) x[1])); if (sorted) ci2.l <- ci2.l[order(x)]
  
  x.plot <- x; if (sorted) x.plot <- x[order(x)]
  px.plot <- px; if (sorted) px.plot <- px.plot[order(x)]
  
  ylim[is.na(ylim)] <- 0
  adjusted.ylim = c(pointlinestop - (n.spec*pointlinespace) - bottommargin, round(max(c(ci1.u, max(ylim, na.rm = TRUE))), 1) + topmargin + .05)
  
  # Set plot title
  if (data == "AG"){
    plot.title <- "Figure 1A: Ethnic Majorities"
  } else if (data == "HE"){
    plot.title <- "Figure 1B: Cis-Heterosexuals"
  } else if (data == "DG"){
    plot.title <- "Figure 2A: Ethnic Minorities"
  } else if (data == "SM"){
    plot.title <- "Figure 2B: LGBTIQ+ Individuals"
  } else if (data == "AG_short"){
    plot.title <- "Ethnic Majorities (short)"
  } else if (data == "HE_short"){
    plot.title <- "Cis-Heterosexuals (short)"
  } else if (data == "DG_short"){
    plot.title <- "Ethnic Minorities (short)"
  } else {
    plot.title <- "Specification Curve (unspecified)"
  }
  
  if(resid == "keyDem"){
    plot.title <- paste(plot.title, "(keyDem)")
  }
  
  
  bar.pos <<- barplot(c(rep(0, x.offset), ci1.u * (ci1.u >= 0)), col = par("bg"), ylim = adjusted.ylim,
                      space = barspace, axes = FALSE, border = NA, offset = 0, main = plot.title)
  
  bar.width <<- bar.pos[2] - bar.pos[1] - barspace
  bars.x <- bar.pos + x.offset * (bar.pos[2] - bar.pos[1])
  for (bar in (1:length(bar.pos)))
  {
    # ci 1
    polygon(c(bars.x[bar] - (bar.width/2), bars.x[bar] + (bar.width/2), bars.x[bar] + (bar.width/2), bars.x[bar] - (bar.width/2)),
            c(ci1.u[bar], ci1.u[bar], ci1.l[bar], ci1.l[bar]),
            col = col[1],
            border = NA)
    
    # ci 2
    polygon(c(bars.x[bar] - (bar.width/2), bars.x[bar] + (bar.width/2), bars.x[bar] + (bar.width/2), bars.x[bar] - (bar.width/2)),
            c(ci2.u[bar], ci2.u[bar], ci2.l[bar], ci2.l[bar]),
            col = col[2],
            border = NA)
  }
  
  # mark effects with circles
  if (is.na(pcex)) pcex <- 0.7*bar.width
  points(bar.pos[-c(1:x.offset)], x.plot, cex = pcex) 
  
  # ticks for y-axis
  ylim <- c(round(min(c(ci1.l,min(ylim, na.rm = TRUE))), 1), round(max(c(ci1.u, max(ylim, na.rm = TRUE))), 1) + topmargin)
  ticks = c(seq(from = ylim[1], to = ylim[2], by = .1))  
  ticks = round(ticks, 1)
  
  # draw y-axis
  ylabels <- ticks
  ylabels[ylabels == 0] <- paste(ylab, ylabels[ylabels == 0], sep = "       ") # Workaround to label the y-Axis at a predictable position
  axis(2, at = ticks, labels = ylabels, cex.axis = acex, las = 1, pos = 0)
  
  # draw horizontal line at y = 0 (x-axis)
  linesright <- bar.pos[length(bar.pos)] + bar.width
  lines(x = c(0, linesright), y = c(0, 0), xpd = FALSE)
  
  # draw horizontal lines at y-ticks
  if (gridlines) for (tick in ticks) lines(x = c(0, linesright), y = c(tick, tick), xpd = FALSE, lty = "dotted", lwd = .25)
  
  # BarCode
  if (any(is.na(lineseq))) lineseq <- 1:n.spec
  
  for (line in 1:n.spec)
  {
    colvect <- c(rep(0, x.offset), c("black", col)[px.plot + 1])
    colvect[c(rep(0, x.offset), specifications.plot[, lineseq[line]]) == 0] <- 0
    barplot(c(rep(0, x.offset), rep(pointlinespace*.6, nrow(specifications.plot))), col = colvect,
            space = barspace, axes = FALSE, border = NA, offset = pointlinestop - line * pointlinespace, add = TRUE)
    text(text.offset, pointlinestop - (line - .3)  * pointlinespace, names[lineseq[line]], cex = tcex, pos = 2, xpd = TRUE)
  }
  
  if (is.na (linesleft)) linesleft <- 0
  if (any(!is.na(lines)))
  {
    for (linepos in lines) lines(x = c(linesleft, linesright),
                                 y = rep(pointlinestop - (linepos + 2*pointlinespace) * pointlinespace, 2), lty = "dotted", xpd = TRUE)
  }
  
  # Legend
  if (legend)
  {
    if (all(is.na(c(legendx,legendy)))) 
    {
      legend(x = "top",
             legend = c("95% CI","90% CI"),
             fill = c(col),
             bty = "o",
             cex = lcex,
             horiz = TRUE,
             box.col = par("bg"))
    } else
    {  
      if (is.na(legendx)) legendx <- par("usr")[1]
      if (is.na(legendy)) legendy <- ylim[2]
      legend(x = legendx,
             y = legendy,
             legend = c("95% CI","90% CI"),
             fill = c(col),
             bty = "o",
             xpd = FALSE,
             cex = lcex,
             box.col = par("bg")
      )
    }
    legend(x = bar.pos[length(bar.pos)],
           y = (pointlinestop-(n.spec)*pointlinespace),
           legend = c("sig.: two-tailed", "sig.: one-tailed", "non-significant"),
           fill = c(col,1),
           bty = "n",
           cex = lcex,
           xjust = 1,
           horiz = TRUE)
  }
  
  
  # Number of model specifications
  textbox(x = c(0,bar.pos[length(bar.pos)]) ,
          y = -.45,
          textlist = paste(nrow(specifications), "model specifications"),
          cex = .8,
          justify = "r",
          box = TRUE,
          border = "white",
          fill = "white")
  
  # Finalize save on request
  if (saveaspdf){ 
    dev.off()
  }
}



#..........................................................................................................#
#..........................................................................................................#
#......................................... Supplementary Material  ........................................#
#..........................................................................................................#
#..........................................................................................................#


#...# Meta shuffle

# Calculate how likely it is that a intergroup contact or support for social change variable
# produces a deviation from the grand mean at least as large in the original dataset by chance.
metaShuffle <- function(original, shuffled, change, contact) {
  
  # Define ranges of variables belonging to non-binary specification factors
  varN <- length(change)+length(contact)
  rangeDV <- 1:length(change) # Support for social change
  rangeIV <- (length(change)+1):varN # Intergroup contact
  
  # Prepare vector for p-values
  results.metashuffle <- rep(NA, varN)
  names(results.metashuffle) <- names(original)[1:varN]
  
  # Calculate p-values
  for(i in 1:varN){
    results.metashuffle[i] <- mean(abs(shuffled[ ,i]) >= abs(original[i])) 
  }
  
  results.metashuffle[rangeDV][results.metashuffle[rangeDV] == 0] <- paste("<", 1/(nrow(shuffled)))
  results.metashuffle[rangeIV][results.metashuffle[rangeIV] == 0] <- paste("<", 1/(nrow(shuffled)))
  
  return(results.metashuffle)
}


#...# Meta regression (only main effects)
metaReg <- function(data, short) {
  if(short) 
  {
    model <- "x.effect ~ (SEMP + IDISC + WILAC_D + WILAC_E) + (NEG_R + NFRI + FFRI + EXTC_CQUAL + EXTC_NEG_R) + (ATTCHK + NO_OUTLIERS)" 
    # Without CQUA, EXTC_CQUA (reference variables: WRKSO, CQUAL)
  }
  else
  {
    model <- "x.effect ~ (SEMP + IDISC + WILAC_D + WILAC_E) + (CQUAL + NEG_R + NFRI + FFRI + EXTC_CQUA + EXTC_CQUAL + EXTC_NEG_R) + (ATTCHK + NO_OUTLIERS)" 
    # (reference variables: WRKSO, CQUA)
  }
  result <- lm(model, data$xdata)
  return(result)
}


#...# Explained Variance and Cross-validation
crossVal <- function(dataList, modList) {
  #... Prepare matrix
  table_s11 <- matrix(nrow = 4, ncol = 4)
  colnames(table_s11) <- c('Data - DG', 'Data - AG', 'Data - HE', 'Data - SM')
  rownames(table_s11) <- c('Model - DG', 'Model - AG', 'Model - HE', 'Model - SM') 
  
  
  #... AG, DG, HE (long models, long data)
  for (i in 1:3) {
    table_s11[-4, i] <- sapply(modList[1:3], function(x) cor(predict(x, newdata = dataList[[i]]), dataList[[i]]$x.effect))
  }
  
  
  #... SM (short models, short data)
  
  # Data - SM
  table_s11[ ,4] <- sapply(modList[4:7], function(x) cor(predict(x, newdata = dataList[[7]]), dataList[[7]]$x.effect))
  
  # Model - SM
  table_s11[4, ] <- sapply(dataList[4:7], function(x) cor(predict(modList[[7]], newdata = x), x$x.effect))
  
  
  #... Prepare and return table s11
  
  # Reorder matrix
  table_s11 <- table_s11[c(1, 4, 2, 3), c(1, 4, 2, 3)]
  
  #... Final Table S11
  table_s11 <- round(table_s11, 2)
  return(table_s11)
}



#..........................................................................................................#
#..........................................................................................................#
#......................................... Print key results ..............................................#
#..........................................................................................................#
#..........................................................................................................#

keyResults <- function(set = dataset) {
  results <- list()
  
  # Selected dataset
  results$Dataset <- paste(set, whichData)
  
  # Number of participants in selected dataset
  results$Participants <- paste0("N = ", nrow(data.sel))
  
  # Number of model specifications
  results$'Model Specifications' <- x$numspec
  
  # Descriptive statistics: Effect sizes (bivariate correlations) in the selected dataset
  results$'Effect Sizes (Bivariate Correlations)' <- effectSize
  
  # Warning if R != 1000
  if(R != 1000){
    results$WARNING <- "Please set R to 1000 to receive correct p-values (joint significance test, meta-regression). "
    results$WARNING <- paste0(results$WARNING, "Currently, R is set to ", R, ". ", "So, the smallest calculable p-value in our permutation tests is p < ", 1/R, " (p < 1/R).")
  }
  
  # Joint significance test (Table 2 in the main article)
  if(dataset %in% c("SM", "DG", "DG_short")){
    # One-sided significance, negative direction (as predicted for ethnic minorities and LGBTIQ+ individuals)
    joint <- joint.one_neg[c(1,4)]
  } else if (dataset %in% c("HE", "AG", "HE_short", "AG_short")){
    # One-sided significance, positive direction (as predicted for ethnic majorities and cis-heterosexuals)
    joint <- joint.one_pos[c(1,4)] 
  } 
  joint <- t(as.matrix(joint))
  colnames(joint) <- c("Significant Correlations", "p-value")
  results$'Joint Significance Test' <- joint
  
  # Table S8: Results from meta-regression: Deviations from the grand mean
  metaReg <- rbind(round(ose[1:(length(ose)-2)], 2), results.metashuffle)
  rownames(metaReg) <- c("b", "p")
  results$'Meta-Regression' <- metaReg
  
  # Table S9: Multiple comparisons: Benjamini-Yekutieli
  results$'Benjamini-Yekutieli (p < .1)' <- BY.adj(x, .1, dataset)
  results$'Benjamini-Yekutieli (p < .05)' <- BY.adj(x, .05, dataset)
  
  # # Benjamini-Yekutieli: Specifications with WRKSO (Table S10)
  # WRKSO.ADJ <- list(p = x$p[x$combos$WRKSO == 1], 
  #                   pBY = x$pBY[x$combos$WRKSO == 1], 
  #                   effect = x$effect[x$combos$WRKSO == 1],
  #                   numspec = sum(x$combos$WRKSO == 1))
  # # p < .1
  # results$'Benjamini-Yekutieli WRKSO (p < .1)' <- BY.adj(WRKSO.ADJ, .1, dataset)
  # 
  # # p < .05
  # results$'Benjamini-Yekutieli WRKSO (p < .05)' <- BY.adj(WRKSO.ADJ, .05, dataset)
  
  # Table S11: Cross-Validation
  results$'Cross-Validation' <- tableS11
  
  # Return key results
  return(results)
}


#................................................... Additional Functions .................................#


#... Benjamini-Yekutieli correction
BY.adj <- function(res, p, whichdata, WRKO = F) {
  
  # DG, SM
  if(grepl("DG",whichdata) | grepl("SM",whichdata)){
    
    # predicted direction
    raw.sum <- sum(res$p[res$effect < 0] < p)
    raw.mean <- sum(res$p[res$effect < 0] < p)/res$numspec
    
    BY.sum <- sum(res$effect < 0 & res$pBY < p)
    BY.mean <- mean(res$effect < 0 & res$pBY < p)
    
    # opposite direction
    raw.sum.opp <- sum(res$p[res$effect > 0] < p)
    raw.mean.opp <- sum(res$p[res$effect > 0] < p)/res$numspec
    
    BY.sum.opp <- sum(res$effect > 0 & res$pBY < p)
    BY.mean.opp <- mean(res$effect > 0 & res$pBY < p)
    
    # AG, HE
  } else {
    
    # predicted direction
    raw.sum <- sum(res$p[res$effect > 0] < p)
    raw.mean <- sum(res$p[res$effect > 0] < p)/res$numspec
    
    BY.sum <- sum(res$effect > 0 & res$pBY < p)
    BY.mean <- mean(res$effect > 0 & res$pBY < p)
    
    # opposite direction
    raw.sum.opp <- sum(res$p[res$effect < 0] < p)
    raw.mean.opp <- sum(res$p[res$effect < 0] < p)/res$numspec
    
    BY.sum.opp <- sum(res$effect < 0 & res$pBY < p)
    BY.mean.opp <- mean(res$effect < 0 & res$pBY < p)
  }
  
  BY.mat <- matrix(c(BY.sum, BY.mean, raw.sum, raw.mean), nrow = 2, ncol = 2)
  colnames(BY.mat) <- c("adjusted", "raw")
  rownames(BY.mat) <- c("number", "proportion")
  
  BY.mat.opp <- matrix(c(BY.sum.opp, BY.mean.opp, raw.sum.opp, raw.mean.opp), nrow = 2, ncol = 2)
  colnames(BY.mat.opp) <- c("adjusted", "raw")
  rownames(BY.mat.opp) <- c("number", "proportion")
  
  results <- list("predicted" = BY.mat, "opposite" = BY.mat.opp)
  return(results)
  
}
