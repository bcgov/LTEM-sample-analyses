sf.autoplot.lmer <- function(model, ..., which=TRUE, mfrow=c(2,2)){
  # which = TRUE implies select all plots; specify a vector if only want some of the plots
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
        if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
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
  
  
  df <- fortify(model)
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

