# This function can be used to save diagnostic plots created by autoplot() in ggfortify
# There currently is a bug where you cannot save the diagnostic plots
# See see https://github.com/sinhrks/ggfortify/issues/98 for bug in autoplot
ggsave.ggmultiplot <- function(plot, file=NULL, height=4, width=6, units="in", dpi=300){
  #browser()
  require(grid)
  png(file=file, height=height, width=width, units=units, res=dpi)
  grid::grid.draw(plot)
  dev.off()
  invisible()
}


# Create residual and other diagnostic plots from lmer() objects.
sf.autoplot.lmer <- function(model, ..., which=TRUE, mfrow=c(2,2)){
  # which = TRUE implies select all plots; specify a vector if only want some of the plots
  require(broom.mixed)
  require(ggplot2) 
  require(grid)
  require(gridExtra)
  require(lattice)
  require(plyr)
  
  ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
    # Create Caterpillar plots
    # Refer to http://stackoverflow.com/questions/13847936/in-r-plotting-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot
    # We modified it to access the name of the random effect for use in the plots
    # http://stackoverflow.com/questions/9950144/access-lapply-index-names-inside-fun
    
    require(ggplot2)
    f <- function(i, allre) {
      re_name <- names(allre)[i] # name of the random effect
      x   <- allre[[i]]
      pv   <- attr(x, "postVar")
      cols <- 1:(dim(pv)[1])
      se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
      ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
      pDf  <- data.frame(y=unlist(x)[ord],
                         ci=1.96*se[ord],
                         nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                         ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                         ind=gl(ncol(x), nrow(x), labels=names(x)))
      
      if(QQ) {  ## normal QQ-plot
        p <- ggplot(pDf, aes(nQQ, y))
        p <- p + facet_wrap(~ ind, scales="free")
        p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
      } else {  ## caterpillar dotplot
        p <- ggplot(pDf, aes(ID, y)) + coord_flip()
        if(likeDotplot) {  ## imitate dotplot()  - same scales for random effects
          p <- p + facet_wrap(~ ind)
        } else {           ## different scales for random effects
          p <- p + facet_grid(ind ~ ., scales="free_y")
        }
        p <- p + xlab("Levels") + ylab("Random effects") + ggtitle(paste("Caterpillar Plot of ", re_name))
      }
      
      p <- p + theme(legend.position="none")
      p <- p + geom_hline(yintercept=0)
      p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
      p <- p + geom_point(aes(size=1.2), colour="blue") 
      return(p)
    }
    res<- lapply(seq_along(re), f, allre=re)
    names(res) <- names(re)
    res
  }
  
  
  df <- broom::augment(model)
  #df <- fortify(model)
  df <- cbind(df, rows=1:nrow(df))
  
  # residuals vs fitted
  g1 <- ggplot(df, aes(x=.fitted, y=.resid)) +
    geom_point()  +
    geom_smooth(se=FALSE) +
    geom_hline(yintercept=0, linetype=2, size=.2) +
    scale_x_continuous("Fitted Values") +
    scale_y_continuous("Residual") +
    ggtitle("Residuals vs Fitted")
  
  # normal qq on residuals
  a <- quantile(df$.resid, c(0.25, 0.75))
  b <- qnorm(c(0.25, 0.75))
  slope <- diff(a)/diff(b)
  int <- a[1] - slope * b[1]
  g2 <- ggplot(df, aes(sample=.resid)) +
    stat_qq() +
    geom_abline(slope=slope, intercept=int) +
    xlab("Theoretical Quantiles") +
    ylab("Residuals") +
    ggtitle("Normal Q-Q on residuals")
  
  # caterpillar plots on the all of the random effects
  cat_plot <- ggCaterpillar( ranef(model, condVar=TRUE),  QQ=FALSE, likeDotplot=FALSE)
  
  plots <- list(g1=g1, g2=g2)
  cat_names <- names(cat_plot)
  l_ply(cat_names, function(name){
    # add the caterpiller plots to the list of plots
    #browser()
    plots <<- c(plots, cat_plot[name])
  })
  plots.subset <- plots[which]
  plots.subset$ncol <- mfrow[2]
  #plots.subset$nrow <- mfrow[1]
  # browser()
  gridplots <- do.call(arrangeGrob, plots.subset)
  gridplots  # return the final object
}


### power function for linear regression

# Simple Linear regression Power function that incorporates process and sampling error
# While this is commonly used for trend analysis where the X variable is time, it can
# also be used for any regression problem.
# Autocorrelation is not accounted for in this power analysis.

# 2014-11-21 CJS Removed Ivalue from the call as not needed
# 2014-06-24 CJS First Edition for web

# This function computes the power for a simple linear regression design that allows
# for process and sampling error
#
# The information we need is:
#     Alpha level (usually .05 or .10)
#     Variance components (these were obtained from the an analysis of previous data)
#        Process.SD  - standard deviation of the process variation over the X value
#        Sampling.SD - standard deviation of the sampling variation at each X value
#     Trend   - slope for which the power to detect is to be estimated
#     X       - vector of X values. These can be in any order. Multiple X values
#               indicated multiple measurements at each X value.


# The computations are based on the 
#    Stroup, W. W. (1999)
#    Mixed model procedures to assess power, precision, and sample size in the design of experiments.
# paper where "dummy" data is generated and "analyzed" and the resulting F-statistics etc
# provide information needed to compute the power

library(lme4)


#-----------------------------------------------

slr.power.stroup <- function(Trend, Xvalues, Process.SD, Sampling.SD, alpha=0.05){
# This computes the power of a simple linear regression that potentially includes 
# process and sampling error. Arguments are defined above

# There are 3 cases to consider
# (a) Process.SD >0, Sampling.SD >0, replicates at some X values
#      This is the classical regression model with process error and sampling error.
#      Because there are multiple measurements at some X values, it is possible to
#      fit a model of the form
#        lmer( Y ~ X + (1|XF), data=blah)
#      where XF are the X values treated as a factor.
#      In this case, the df for testing a hypothesis about the slope is
#      approximately equal to the
#          number of unique X values - 2 (essentially, you analze the averages)
#      The power refers to the ability to detect a non-zero slope of the lmer model.
#
# (b) Process.SD >0, Sampling.SD >0, NO replicates at any X value
#     This is quite common where an estimate of a population parameter is obtained
#     each year with a measure of precision (SE of each estimate). The sampling SD
#     is essentially the average of the SE. Process error is obatined by subtracting
#     the average SE from the residual sd after fitting a mean (or simple slope)
#     In this case, all that need be done is fit a 
#           lm( Y ~ X, data=blah)
#     as then the resiudal sd is the (proper) mixture of process and sampling SD
#     The df for hypothesis tests about the slope is again approximately equal to 
#           number of unique X values - 2
#     If you want to investigate the impact of increasing effort in each year, treat
#     the current average se as obtained from a "unit of effort". So if you take two 
#     measurement at an X value, this is (approximately) equivalent to doubling the effort.
#
# (c) Process.SD = 0, Sampling.SD >0, any set of X values (with or without replicates
#     at an paticular X values)
#     This is a classical simple linear regression with data points scattered about
#     the regression line and all points are independent of all other points,i.e.
#          lm(Y ~ X)
#     This is a VERY strong assumption and typically not valid when testing for 
#     trends over time where it is VERY common to have a process error that corresponds to
#     year specific effects over which you typically do not have any control.
#     The df for testing hypothese about the slope is number of data points - 2.
      
  # Total sample size
  n <- length(Xvalues)
  # Compute the mean response (before adding process o sampling error)
  mu <- 0 + Trend*(Xvalues-min(Xvalues))
  
  # Create the various design matrices
  # Fixed effects
  X  <- cbind(1, Xvalues)
  XF <- model.matrix(~ -1 +as.factor(Xvalues))

  # We solve for the regression estimates using weighted least squares
  # based on the variance-covariance matrix for the data
  V <- diag(Sampling.SD^2,n,n) + XF %*% t(XF)*Process.SD^2 

  # Get fixed effects and fixed effects varcovar matrix
  beta <- solve(t(X)%*%solve(V)%*%X) %*% t(X)%*%solve(V)%*%mu

# the vector to extract the slope coefficient
  K <- c(0,1)

#  calculate the non-centrality parameter, and then the power
  ncp <- as.numeric(t(t(K)%*%beta)%*%solve(t(K)%*%solve(t(X)%*%solve(V)%*%X)%*%K)%*%(t(K)%*%beta))

# What is the denominator df for the hypothesis test. See notes above
  dfdenom <- length(unique(Xvalues))-2 # approximation to df for slope is number of unique X values -2 
  if(Process.SD ==0){ dfdenom = length(Xvalues)-2}

  Fcrit <- qf(1-alpha, 1, dfdenom)
  power.2s <- 1 - pf(Fcrit, 1, dfdenom,ncp)

#  Compute the one-sided power, i.e. to detect the change in one direction only
#  Because I don't know which direction of interest, the one-sided power is 
#  computed in both directions.
#
  Tcrit <- qt(1-alpha,dfdenom)
  power.1s.a <- 1-pt(Tcrit,dfdenom,sqrt(ncp))
  power.1s.b <- pt(-Tcrit,dfdenom,sqrt(ncp))

  data.frame(alpha=alpha, 
            Trend=Trend, 
            Process.SD=Process.SD, Sampling.SD=Sampling.SD,
            Beta0=beta[1], Beta1=beta[2],
            dfdenom=dfdenom, ncp=ncp, Fcrit=Fcrit, power.2s=power.2s,
            Tcrit=Tcrit, power.1s.a=power.1s.a, power.1s.b=power.1s.b)
}



