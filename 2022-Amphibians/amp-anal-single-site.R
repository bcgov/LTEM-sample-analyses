





























#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#  Analysis of the visual survey protocol

# Look at # adults 
# We need to assume that detection probabilities are consistent over time and space
# There is only one monitoring station at Alice Lake

dim(amph.df)
amph.v.df <- amph.df[ amph.df$Survey.Type=='VI',]
dim(amph.v.df)

# Count the total number of egg masses seen by species
adults.count.by.species <- plyr::ddply(amph.v.df, c("Study.Area.Name","Transect.Label","Year","Species"), summarize,
                       n.adults=sum(Life.Stage=='AD'))
adults.count.by.species

# Count the total number of egg masses seen for ALL species
adults.count.all.species <- plyr::ddply(amph.v.df, c("Study.Area.Name","Transect.Label","Year","Species"), summarize,
                       n.adults=sum(Life.Stage=='AD'))
adults.count.all.species <- plyr::ddply(amph.v.df, c("Study.Area.Name","Transect.Label","Year"), summarize,
                       n.adults=sum(Life.Stage=='AD'))
adults.count.all.species$Species <- 'ALL.species'

adults.count <- rbind(adults.count.by.species, adults.count.all.species)




# Need to impute 0 values for species not seen in a year


# check out records where adults count not identified
select <- adults.count$Species %in% c("AMPHIBION","Unidentifed") # Notice type in unidentified
adults.count[ select,]

adults.count <- adults.count[ !select, ] 


# Create a record for each species x each year
complete.species.year <- expand.grid(Study.Area.Name=unique(adults.count$Study.Area.Name),
                                     Transect.Label =unique(adults.count$Transect.Label),
                                     Species=unique(adults.count$Species),
                                     Year   =unique(adults.count$Year,  stringsAsFactors=FALSE))
adults.count <- merge(complete.species.year, adults.count, all.x=TRUE)

# replace all NA by 0
adults.count$n.adults[ is.na(adults.count$n.adults)] <- 0

# check that every species is given in each year
xtabs(~Species+Year, data=adults.count, exclude=NULL, na.action=na.pass)




# Section of code used for testing multiple transects#
#adults.count2 <- adults.count
#adults.count2$Transect.Label ='xx'
#adults.count2$n.adults <- rpois(1, 1+adults.count$n.adults)
#adults.count2
#adults.count <- rbind(adults.count, adults.count2)

# Make a preliminary plot of total count by date
# Make a preliminary plot of total count by date
prelim.adult.plot <- ggplot(data=adults.count, aes(x=Year, y=n.adults, color=Transect.Label, linetype=Transect.Label))+
   ggtitle("Amphibian adult count data")+
   ylab("Amphibian adults")+
   geom_point(position=position_dodge(width=.5))+
   geom_line( position=position_dodge(width=.5))+
   facet_wrap(~interaction(Study.Area.Name,Species), ncol=2, scales="free_y")+
   scale_x_continuous(breaks=min(adults.count$Year,na.rm=TRUE):max(adults.count$Year,na.rm=TRUE))
prelim.adult.plot 
ggplot2::ggsave(plot=prelim.adult.plot, 
       file=paste(file.prefix,'-plot-prelim-adult.png',sep=""),
       h=6, w=6, units="in",dpi=300)


# This is a regression analysis with Year as the trend variable 
# In this case, there is only one transect, so it is not necessary to have the Transect.Label in the model
# We also don't need to model process (year specific effects) because there is only 1 transect

# Fit a linear regression for each species 
fits<- dlply(adults.count, "Species", function(adults.count){
   cat("\n\n\n *** Starting analysis for species ", as.character(adults.count$Species[1]), "\n")
   if(length(unique(adults.count$Transect.Label))>1){
      adults.count$Transect.LabelF <- factor(adults.count$Transect.Label)
      adults.count$YearF           <- factor(adults.count$Year)
      adult.fit <- lmerTest::lmer(log(n.adults) ~ Year + (1|Transect.LabelF) + (1|YearF) , data=adults.count)
      print(anova(adult.fit, ddf='kenward-roger'))
      print(summary(adult.fit))
      print(VarCorr(adult.fit))
   }
   if(length(unique(adults.count$Transect.Label))==1){
       # with only 1 transect, not necessary to put random effects effect in the model
       adult.fit <- glm(n.adults ~  Year , data=adults.count, family=quasipoisson)
       print(Anova(adult.fit, test="F", type=3))
       print(summary(adult.fit))
   }
   list(Species=adults.count$Species[1], fit=adult.fit)
})


# Get the model diagnostic plots
l_ply(fits, function(x){
    cat("\n\n\n *** Diagnostic plot for species ", as.character(x$Species), "\n")
   if(length(unique(x$fit$data$Transect.Label))>1){
      diag.plot <- sf.autoplot.lmer(x$fit)  # residual and other diagnostic plots
      plot(diag.plot)
   }
   if(length(unique(x$fit$data$Transect.Label))==1){
      diag.plot <- autoplot(x$fit)  # residual and other diagnostic plots
      show(diag.plot)
   }
   ggplot2:: ggsave(plot=diag.plot, 
          file=paste(file.prefix,"-adult-residual-plot-",x$Species,".png",sep=""),
          h=6, w=6, units="in", dpi=300)
})

l_ply(fits, function(x){
   cat("\n\n\n *** Check for autocorrelation for species ", as.character(x$Species), "\n")
   adults.count <- x$fit$data
   # check for autocorrelation
   if(length(unique(adults.count$Transect.Label))>1){
      adults.count$resid <- log(adults.count$n.adults) - predict(x$fit, newdata=adults.count, re.form=~0)
   }
   #browser()
   if(length(unique(adults.count$Transect.Label))==1){
      adults.count$resid <- log(adults.count$n.adults+.1) - predict(x$fit, newdata=adults.count, type="link")
   }
   mean.resid <- plyr::ddply(adults.count, "Year", summarize, mean.resid=mean(resid))
   resid.fit <- lm( mean.resid ~ 1, data=mean.resid)
   dwres1 <- car::durbinWatsonTest(resid.fit)
   print(dwres1)
   dwres2 <- lmtest::dwtest(resid.fit)
   print(dwres2)
})


# extract a table of statistics for each study area x species combiations
adult.slopes <- ldply(fits, function(x){
  adults.count <- x$fit$data
  adult.fit       <- x$fit
  if(length(unique(adults.count$Transect.Label))==1){
     adult.slopes <- data.frame(
        Study.Area.Name =adults.count$Study.Area.Name[1],
        slope    = coef(adult.fit)["Year"],
        slope.se = sqrt(diag(vcov(adult.fit)))["Year"],
        p.value  = summary(adult.fit)$coefficients[row.names(summary(adult.fit)$coefficients)=="Year"  ,"Pr(>|t|)"],
        stringsAsFactors=FALSE
     )
   }
   if(length(unique(adults.count$Transect.Label))>1){
     adult.slopes <- data.frame(
          Study.Area.Name =adults.count$Study.Area.Name[1],
          slope           = fixef(adult.fit)["Year"],
          slope.se        = summary(adult.fit)$coefficients["Year","Pr(>|t|)"],
          p.value         = summary(adult.fit)$coefficients[row.names(summary(adult.fit)$coefficients)=="Year"  ,"Pr(>|t|)"], 
          stringsAsFactors=FALSE)
   }
   adult.slopes
})
adult.slopes


# compute the fitted values from the model
adult.fitted <- ldply(fits, function(x){
   adults.count <- x$fit$data
   adult.fit       <- x$fit
   Transect.Label <- unique(as.character(adults.count$Transect.Label))
   newdata <- expand.grid(Year=seq(min(adults.count$Year, na.rm=TRUE),max(adults.count$Year, na.rm=TRUE), .1),
                          Transect.LabelF=Transect.Label)
   newdata$Transect.Label <-as.character(newdata$Transect.LabelF)
   if(length(unique(adults.count$Transect.Label))==1){
      newdata$pred.mean <- predict(adult.fit, newdata=newdata,type="response")
   }
   if(length(unique(adults.count$Transect.Label))>1){
      newdata$pred.mean <- exp(predict(adult.fit, newdata=newdata,type="response", re.form=~0))
   }
   # we now must average over all of the transect labels
   adult.fitted <- plyr::ddply(newdata, c("Year","Transect.Label"), plyr::summarize, pred.mean=mean(pred.mean))
   adult.fitted$Study.Area.Name <- adults.count$Study.Area.Name
   adult.fitted
})
adult.fitted


# Plot with trend line 
adult.plot.summary <- ggplot2::ggplot(data=adults.count,
                                    aes(x=Year, y=n.adults))+
   ggtitle("Amphibian adults count ")+
   ylab("Amphibian adults Count")+
   geom_point(size=3, aes(color=Transect.Label))+
   facet_wrap(~interaction(Study.Area.Name,Species), ncol=2, scales="free_y")+
   geom_line(data=adult.fitted, aes(x=Year,y=pred.mean))+
   scale_x_continuous(breaks=min(adults.count$Year,na.rm=TRUE):max(adults.count$Year,na.rm=TRUE))+
   geom_text(data=adult.slopes, aes(x=min(adults.count$Year, na.rm=TRUE), y=max(adults.count$n.adults, na.rm=TRUE)), 
             label=paste("Slope (on log scale) : ",round(adult.slopes$slope,2), 
                         " ( SE "  ,round(adult.slopes$slope.se,2),")",
                         " \np :"    ,round(adult.slopes$p.value,3)),
                         hjust="left",vjust="top")+
   theme(legend.position="top")
adult.plot.summary
ggsave(plot=adult.plot.summary, 
       file=paste(file.prefix,'-adult-plot-summary.png',sep=""),
       h=6, w=6, units="in", dpi=300)

