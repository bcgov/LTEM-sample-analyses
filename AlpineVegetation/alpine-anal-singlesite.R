# This script will demonstrate how to analyze the ALPINE data collected as 
# part of the LongTerm Ecological Monitoring Initiative

# Notice that this example only has 1 years of data. We simulate multiple years to illustrate
# how to do a trend analysis.
#
#
# Only one study area at time can only be analyzed with a script. 
#
# This was programmed by Carl James Schwarz, Statistics and Actuarial Science, SFU
# cschwarz@stat.sfu.ca
#
# 2017-02-28 First Edition

# Summary of Protocol
#     “Permanent transects are established from just below present treeline 
#     to just above the transition from vegetation to rock (or ridgeline, whichever comes first).
#     A 50cm X 50cm quadrat is sampled at regular intervals along the transect.
#
#     The % foliar cover by species is recorded in each quadrat.”


# load libraries
library(car)       # for testing for autocorrelation (2 libraries needed - see dwtest)
library(ggfortify) # for residual and other diagnostic plot
library(ggplot2)   # for plotting
library(lmtest)    # for testing for autocorrelation
library(plyr)      # for group processing
library(readxl)    # for opening the Excel spreadsheets and reading off them
library(reshape2)  # for melting and casting
library(lmerTest)  # for the linear mixed modelling
library(stringr)   # string handling (like case conversion)

# Load some common functions
source("../CommonFiles/common.functions.R")


cat("\n\n ***** Vegetation Analysis *****  \n\n")

# get the data from the Excel work.books.
# we put the list of work books here, including the file type (xls or xlsx).

work.books.csv <- textConnection(
"file.name
General Survey Babine alpine_2013.xlsm
")

work.books <- read.csv(work.books.csv, as.is=TRUE, strip.white=TRUE, header=TRUE)
cat("File names with the data \n")
work.books

# read each workbook and put all of the data together into one big data frame
veg.df <- plyr::ddply(work.books, "file.name", function(x){
  file.name <- file.path("Data", x$file.name)
  data <- readxl::read_excel(file.name, sheet="General Survey")
  data
})


# I will add some "fake data" based on the above data by randomly sampling from the above 
# and changing the dates to 2014 and 2015
set.seed(234234)
veg.2017 <- veg.df[ sample(1:nrow(veg.df), nrow(veg.df), replace=TRUE),]
veg.2017$Date <- as.Date( paste("2017-", format(veg.df$Date, "%m-%d")))
veg.2021 <- veg.df[ sample(1:nrow(veg.df), nrow(veg.df), replace=TRUE),]
veg.2021$Date <- as.Date( paste("2021-", format(veg.df$Date, "%m-%d")))

veg.df <- rbind(veg.df, veg.2017, veg.2021)
# combined species that were sampled twice
names(veg.df) <- make.names(names(veg.df))
veg.df <- plyr::ddply(veg.df, c("Study.Area.Name","Transect.Label","Date","Comments","Species"), plyr::summarize,
                      Foliar.cover....=sum(Foliar.cover...., na.rm=TRUE))


#------------ Data Editing -----------
# fix up variable names in the data.frame.
# Variable names in R must start with a letter and contain letters or number or _. 
# Blanks in variable names are not normally allowed. Blanks will be replaced by . (period)
cat("\nOriginal variable names in data frame\n")
names(veg.df)

names(veg.df) <- make.names(names(veg.df))

cat("\nCorrected variable names of data frame\n")
names(veg.df)



# Convert dates to R date format
xtabs(~Date, data=veg.df, exclude=NULL, na.action=na.pass)  # check the date formats. Make sure that all yyyy-mm-dd
veg.df$Date <- as.Date(veg.df$Date, "%d-%b-%y", tz="UTC")
veg.df$Year <- as.numeric(format(veg.df$Date, "%Y"))



# Check that the Study Area Name is the same across all years
# Look at the output from the xtabs() to see if there are multiple spellings 
# of the same Study.Area.Name.

# We will convert the Study.Area.Name to Proper Case.
veg.df$Study.Area.Name <- stringr::str_to_title(veg.df$Study.Area.Name)
xtabs(~Study.Area.Name,      data=veg.df, exclude=NULL, na.action=na.pass)
xtabs(~Study.Area.Name+Year, data=veg.df, exclude=NULL, na.action=na.pass)


# Check the Transect.Labels for typos
xtabs(~Study.Area.Name+Transect.Label, data=veg.df, exclude=NULL, na.action=na.pass)

# When is each transect measured?
xtabs(~Transect.Label+Year, data=veg.df, exclude=NULL, na.action=na.pass)
xtabs(~Date+Transect.Label, data=veg.df, exclude=NULL, na.action=na.pass)


# Check the plot codes
# In the Babine example, this is in the comment field. We need to extract
veg.df$Plot <- paste(veg.df$Comments, ';', sep="")
veg.df$Plot <- substr(veg.df$Plot, 1, -1+regexpr(";",veg.df$Plot, fixed=TRUE))
xtabs(~Transect.Label+Plot, data=veg.df, exclude=NULL, na.action=na.pass)

# Make the plot codes a combination of transect number and plot code
veg.df$Plot <- interaction(veg.df$Transect.Label, veg.df$Plot)
xtabs(~Transect.Label+Plot, data=veg.df, exclude=NULL, na.action=na.pass)


# Check the Species code to make sure that they are all ok
xtabs(~Species, data=veg.df, exclude=NULL, na.action=na.pass)
xtabs(~Species+Year, data=veg.df, exclude=NULL, na.action=na.pass)

# Check the % cover to make sure that they are sensible
xtabs(~Foliar.cover...., data=veg.df, exclude=NULL, na.action=na.pass)


# Exclude all species code = NULL which is usually rock and other junk
dim(veg.df)
veg.df <- veg.df[ !veg.df$Species %in% c("NULL"),]
dim(veg.df)
xtabs(~Species, data=veg.df, exclude=NULL, na.action=na.pass)

# prefix for plot and other files created
file.prefix <- make.names(veg.df$Study.Area.Name[1])
file.prefix <- gsub(".", '-', file.prefix, fixed=TRUE) # convert spaces to -
file.prefix <- file.path("Plots",file.prefix)

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#  Analysis of mean total cover

# Compute the total cover for each plot in each transect
cover <- plyr::ddply(veg.df, c("Study.Area.Name","Year","Transect.Label","Plot"), plyr::summarize,
                          cover=sum(Foliar.cover....))

# Compute the average total cover for each transect so I can plot these over time
cover.transect <- plyr::ddply(cover, c("Study.Area.Name","Year","Transect.Label"), plyr::summarize,
                          cover=mean(cover, na.rm=TRUE))
cover.transect

# Make a preliminary plot of cover by years

prelim.cover.plot <- ggplot(data=cover, aes(x=Year, y=cover, color=Transect.Label, linetype=Transect.Label))+
   ggtitle("Mean total cover")+
   ylab("Total % cover on the plots")+
   geom_point(position=position_dodge(width=.2))+
   geom_line(data=cover.transect)+
   scale_x_continuous(breaks=min(cover$Year, na.rm=TRUE):max(cover$Year, na.rm=TRUE))+
   facet_wrap(~Study.Area.Name, ncol=1)
prelim.cover.plot 
ggsave(plot=prelim.cover.plot, 
       file=paste(file.prefix,'-cover-plot-prelim.png',sep=""),
       h=6, w=6, units="in",dpi=300)


# This is a regression analysis with Year as the trend variable and Transect as a random effects.
# We need to account for the same transect (and plots) being measured over time.
# Because this is a linear mixed model and the because the total cover is typically large enough, no transformation
# is needed.

# define the YearF effect for process error (year specific effcts)
cover.transect$YearF           <- factor(cover.transect$Year)
cover.transect$Transect.LabelF <- factor(cover.transect$Transect.Label)
cover.transect

cover.fit.lmer <-  lmerTest::lmer(cover ~ Year + (1|Transect.LabelF) + (1|YearF), data=cover.transect)
anova(cover.fit.lmer,dfm='Kenward-Roger')
summary(cover.fit.lmer)
VarCorr(cover.fit.lmer)

# Look at the residual plot 
diag.plot <- sf.autoplot.lmer(cover.fit.lmer)  # residual and other diagnostic plots
plot(diag.plot)
ggplot2::ggsave(plot=diag.plot, 
                file=paste(file.prefix,"-cover-residual-lmer-plot.png",sep=""),
                h=6, w=6, units="in", dpi=300)

# check for autocorrelation - look at the average residual over time
cover.transect$resid <- cover.transect$cover - predict(cover.fit.lmer, newdata=cover.transect, re.form=~0)
mean.resid <- plyr::ddply(cover.transect, "Year", summarize, mean.resid=mean(resid))
resid.fit <- lm( mean.resid ~ 1, data=mean.resid)
dwres1 <- car::durbinWatsonTest(resid.fit)
dwres1
dwres2 <- lmtest::dwtest(resid.fit)
dwres2


# extract the slope
cover.slopes <- data.frame(
       Study.Area.Name =cover.transect$Study.Area.Name[1],
       slope           = fixef(cover.fit.lmer)["Year"],
       slope.se        = summary(cover.fit.lmer)$coefficients["Year","Pr(>|t|)"],
       p.value         = summary(cover.fit.lmer)$coefficients[row.names(summary(cover.fit.lmer)$coefficients)=="Year"  ,"Pr(>|t|)"], 
       #r2             = summary(cover.fit.lmer)$r.squared,  # not defined for mixed effect models
       stringsAsFactors=FALSE)
cover.slopes


# compute the fitted values from the model
cover.fitted <- data.frame(
                 Study.Area.Name=cover.transect$Study.Area.Name[1],
                 Year=seq(min(cover$Year, na.rm=TRUE),max(cover$Year, na.rm=TRUE), .1),
                 stringsAsFactors=FALSE)
cover.fitted$pred.mean <- predict(cover.fit.lmer, newdata=cover.fitted,type="response", re.form=~0)
head(cover.fitted)

# Plot with trend line 
cover.plot.summary <- ggplot2::ggplot(data=cover,
                                    aes(x=Year, y=cover))+
   ggtitle("Total Species Cover")+
   ylab("Total % cover")+
   geom_point(size=3, aes(color=Transect.Label),position=position_dodge(w=0.2))+
   geom_line(data=cover.fitted, aes(x=Year,y=pred.mean))+
   facet_wrap(~Study.Area.Name, ncol=1, scales="free" )+
   scale_x_continuous(breaks=min(cover$Year, na.rm=TRUE):max(cover$Year,na.rm=TRUE))+
   geom_text(data=cover.slopes, aes(x=min(cover$Year, na.rm=TRUE), y=max(cover$cover, na.rm=TRUE)), 
             label=paste("Slope : ",round(cover.slopes$slope,2), 
                         " ( SE "  ,round(cover.slopes$slope.se,2),")",
                         " p :"    ,round(cover.slopes$p.value,3)),
                         hjust="left")
cover.plot.summary
ggsave(plot=cover.plot.summary, 
       file=paste(file.prefix,'-cover-plot-summary-lmer.png',sep=""),
       h=6, w=6, units="in", dpi=300)



##### if lmer() does not converge, try an approximate analysis on the overall averages 


# Compute the average total cover for each transect so I can plot these over time
cover.avg <- plyr::ddply(cover.transect, c("Study.Area.Name","Year"), plyr::summarize,
                          cover=mean(cover, na.rm=TRUE))
cover.avg

# Make a preliminary plot of average cover by years

prelim.cover.plot.avg <- ggplot(data=cover.avg, aes(x=Year, y=cover))+
   ggtitle("Mean total cover - averaged over all transects in a year")+
   ylab("Mean Total % cover on the plots")+
   geom_point(position=position_dodge(width=.2))+
   geom_smooth(method="lm", se=FALSE)+
   scale_x_continuous(breaks=min(cover$Year, na.rm=TRUE):max(cover$Year, na.rm=TRUE))+
   facet_wrap(~Study.Area.Name, ncol=1)
prelim.cover.plot.avg 
ggsave(plot=prelim.cover.plot.avg, 
       file=paste(file.prefix,'-cover-plot-prelim-avg.png',sep=""),
       h=6, w=6, units="in",dpi=300)


# This is a simple regression analysis with Year as the trend variable 

cover.fit.avg <-  lm(cover ~ Year, data=cover.avg)
anova(cover.fit.avg)
summary(cover.fit.avg)

# Look at the residual plot 
diag.plot <- autoplot(cover.fit.avg)  # residual and other diagnostic plots
show(diag.plot)
ggplot2::ggsave(plot=diag.plot, 
                file=paste(file.prefix,"-cover-residual-avg-plot.png",sep=""),
                h=6, w=6, units="in", dpi=300)

# check for autocorrelation - look at the average residual over time
cover.avg$resid <- cover.avg$cover - predict(cover.fit.avg, newdata=cover.avg)
mean.resid <- plyr::ddply(cover.avg, "Year", summarize, mean.resid=mean(resid))
resid.fit <- lm( mean.resid ~ 1, data=mean.resid)
dwres1 <- car::durbinWatsonTest(resid.fit)
dwres1
dwres2 <- lmtest::dwtest(resid.fit)
dwres2


# extract the slope
cover.slopes.avg <- data.frame(
       Study.Area.Name =cover.transect$Study.Area.Name[1],
       slope           = coef(cover.fit.avg)["Year"],
       slope.se        = summary(cover.fit.avg)$coefficients["Year","Pr(>|t|)"],
       p.value         = summary(cover.fit.avg)$coefficients[row.names(summary(cover.fit.avg)$coefficients)=="Year"  ,"Pr(>|t|)"], 
       r2              = summary(cover.fit.avg)$r.squared, 
       stringsAsFactors=FALSE)
cover.slopes.avg


# compute the fitted values from the model
cover.fitted.avg <- data.frame(
                 Study.Area.Name=cover.transect$Study.Area.Name[1],
                 Year=seq(min(cover$Year, na.rm=TRUE),max(cover$Year, na.rm=TRUE), .1),
                 stringsAsFactors=FALSE)
cover.fitted.avg$pred.mean <- predict(cover.fit.avg, newdata=cover.fitted,type="response")
head(cover.fitted.avg)

# Plot with trend line 
cover.plot.summary.avg <- ggplot2::ggplot(data=cover.avg,
                                    aes(x=Year, y=cover))+
   ggtitle("Total Species Cover")+
   ylab("Mean Total % cover")+
   geom_point(size=3,position=position_dodge(w=0.2))+
   geom_line(data=cover.fitted.avg, aes(x=Year,y=pred.mean))+
   facet_wrap(~Study.Area.Name, ncol=1, scales="free" )+
   scale_x_continuous(breaks=min(cover$Year, na.rm=TRUE):max(cover$Year,na.rm=TRUE))+
   geom_text(data=cover.slopes.avg, aes(x=min(cover$Year, na.rm=TRUE), y=max(cover.avg$cover, na.rm=TRUE)), 
             label=paste("Slope : ",round(cover.slopes.avg$slope,2), 
                         " ( SE "  ,round(cover.slopes.avg$slope.se,2),")",
                         " p :"    ,round(cover.slopes.avg$p.value,3)),
                         hjust="left")
cover.plot.summary.avg
ggsave(plot=cover.plot.summary.avg, 
       file=paste(file.prefix,'-cover-plot-summary-avg.png',sep=""),
       h=6, w=6, units="in", dpi=300)





#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#  Analysis of mean plot-level species richness

# Compute the species richness for each plot in each transect
richness <- plyr::ddply(veg.df, c("Study.Area.Name","Year","Transect.Label","Plot"), plyr::summarize,
                          richness=length(Species))

# Compute the average richness for each transect so I can plot these over time
richness.transect <- plyr::ddply(richness, c("Study.Area.Name","Year","Transect.Label"), plyr::summarize,
                          richness=mean(richness, na.rm=TRUE))

# Make a preliminary plot of richness by years

prelim.richness.plot <- ggplot(data=richness, aes(x=Year, y=richness, color=Transect.Label, linetype=Transect.Label))+
   ggtitle("Species richness")+
   ylab("Species richness")+
   geom_point(position=position_dodge(width=.2))+
   geom_line(data=richness.transect)+
   scale_x_continuous(breaks=min(richness$Year, na.rm=TRUE):max(richness$Year, na.rm=TRUE))+
   facet_wrap(~Study.Area.Name, ncol=1)
prelim.richness.plot 
ggsave(plot=prelim.richness.plot, 
       file=paste(file.prefix,'-richness-plot-prelim.png',sep=""),
       h=6, w=6, units="in",dpi=300)


# This is a regression analysis with Year as the trend variable and Transect as a random effects.
# We need to account for the same transect (and plots) being measured over time.
# Because this is a linear mixed model and the because the total richness is typically large enough, no transformation
# is needed.

# define the YearF effect for process error (year specific effcts)
richness.transect$YearF           <- factor(richness.transect$Year)
richness.transect$Transect.LabelF <- factor(richness.transect$Transect.Label)
richness.transect

richness.fit.lmer <-  lmerTest::lmer(richness ~ Year + (1|Transect.LabelF) + (1|YearF), data=richness.transect)
anova(richness.fit.lmer,dfm='Kenward-Roger')
summary(richness.fit.lmer)
VarCorr(richness.fit.lmer)

# Look at the residual plot 
diag.plot <- sf.autoplot.lmer(richness.fit.lmer)  # residual and other diagnostic plots
plot(diag.plot)
ggplot2::ggsave(plot=diag.plot, 
                file=paste(file.prefix,"-richness-residual-lmer-plot.png",sep=""),
                h=6, w=6, units="in", dpi=300)

# check for autocorrelation - look at the average residual over time
richness.transect$resid <- richness.transect$richness - predict(richness.fit.lmer, newdata=richness.transect, re.form=~0)
mean.resid <- plyr::ddply(richness.transect, "Year", summarize, mean.resid=mean(resid))
resid.fit <- lm( mean.resid ~ 1, data=mean.resid)
dwres1 <- car::durbinWatsonTest(resid.fit)
dwres1
dwres2 <- lmtest::dwtest(resid.fit)
dwres2


# extract the slope
richness.slopes <- data.frame(
       Study.Area.Name =richness.transect$Study.Area.Name[1],
       slope           = fixef(richness.fit.lmer)["Year"],
       slope.se        = summary(richness.fit.lmer)$coefficients["Year","Pr(>|t|)"],
       p.value         = summary(richness.fit.lmer)$coefficients[row.names(summary(richness.fit.lmer)$coefficients)=="Year"  ,"Pr(>|t|)"], 
       #r2             = summary(richness.fit.lmer)$r.squared,  # not defined for mixed effect models
       stringsAsFactors=FALSE)
richness.slopes


# compute the fitted values from the model
richness.fitted <- data.frame(
                 Study.Area.Name=richness.transect$Study.Area.Name[1],
                 Year=seq(min(richness$Year, na.rm=TRUE),max(richness$Year, na.rm=TRUE), .1),
                 stringsAsFactors=FALSE)
richness.fitted$pred.mean <- predict(richness.fit.lmer, newdata=richness.fitted,type="response", re.form=~0)
head(richness.fitted)

# Plot with trend line 
richness.plot.summary <- ggplot2::ggplot(data=richness,
                                    aes(x=Year, y=richness))+
   ggtitle("Total Species richness")+
   ylab("Total % richness")+
   geom_point(size=3, aes(color=Transect.Label),position=position_dodge(w=0.2))+
   geom_line(data=richness.fitted, aes(x=Year,y=pred.mean))+
   facet_wrap(~Study.Area.Name, ncol=1, scales="free" )+
   scale_x_continuous(breaks=min(richness$Year, na.rm=TRUE):max(richness$Year,na.rm=TRUE))+
   geom_text(data=richness.slopes, aes(x=min(richness$Year, na.rm=TRUE), y=max(richness$richness, na.rm=TRUE)), 
             label=paste("Slope : ",round(richness.slopes$slope,2), 
                         " ( SE "  ,round(richness.slopes$slope.se,2),")",
                         " p :"    ,round(richness.slopes$p.value,3)),
                         hjust="left")
richness.plot.summary
ggsave(plot=richness.plot.summary, 
       file=paste(file.prefix,'-richness-plot-summary-lmer.png',sep=""),
       h=6, w=6, units="in", dpi=300)



#####  if lmer() does not converge, try an approximate analysis on the overall averages 


# Compute the average total richness for each transect so I can plot these over time
richness.avg <- plyr::ddply(richness.transect, c("Study.Area.Name","Year"), plyr::summarize,
                          richness=mean(richness, na.rm=TRUE))
richness.avg

# Make a preliminary plot of average richness by years

prelim.richness.plot.avg <- ggplot(data=richness.avg, aes(x=Year, y=richness))+
   ggtitle("Mean total richness - averaged over all transects in a year")+
   ylab("Mean Total % richness on the plots")+
   geom_point(position=position_dodge(width=.2))+
   geom_smooth(method="lm", se=FALSE)+
   scale_x_continuous(breaks=min(richness$Year, na.rm=TRUE):max(richness$Year, na.rm=TRUE))+
   facet_wrap(~Study.Area.Name, ncol=1)
prelim.richness.plot.avg 
ggsave(plot=prelim.richness.plot, 
       file=paste(file.prefix,'-richness-plot-prelim-avg.png', sep=""),
       h=6, w=6, units="in",dpi=300)


# This is a simple regression analysis with Year as the trend variable 

richness.fit.avg <-  lm(richness ~ Year, data=richness.avg)
anova(richness.fit.avg)
summary(richness.fit.avg)

# Look at the residual plot 
diag.plot <- autoplot(richness.fit.avg)  # residual and other diagnostic plots
show(diag.plot)
ggplot2::ggsave(plot=diag.plot, 
                file=paste(file.prefix,"-richness-residual-avg-plot.png", sep=""),
                h=6, w=6, units="in", dpi=300)

# check for autocorrelation - look at the average residual over time
richness.avg$resid <- richness.avg$richness - predict(richness.fit.avg, newdata=richness.avg)
mean.resid <- plyr::ddply(richness.avg, "Year", summarize, mean.resid=mean(resid))
resid.fit <- lm( mean.resid ~ 1, data=mean.resid)
dwres1 <- car::durbinWatsonTest(resid.fit)
dwres1
dwres2 <- lmtest::dwtest(resid.fit)
dwres2


# extract the slope
richness.slopes.avg <- data.frame(
       Study.Area.Name =richness.transect$Study.Area.Name[1],
       slope           = coef(richness.fit.avg)["Year"],
       slope.se        = summary(richness.fit.avg)$coefficients["Year","Pr(>|t|)"],
       p.value         = summary(richness.fit.avg)$coefficients[row.names(summary(richness.fit.avg)$coefficients)=="Year"  ,"Pr(>|t|)"], 
       r2              = summary(richness.fit.avg)$r.squared, 
       stringsAsFactors=FALSE)
richness.slopes.avg


# compute the fitted values from the model
richness.fitted.avg <- data.frame(
                 Study.Area.Name=richness.transect$Study.Area.Name[1],
                 Year=seq(min(richness$Year, na.rm=TRUE),max(richness$Year, na.rm=TRUE), .1),
                 stringsAsFactors=FALSE)
richness.fitted.avg$pred.mean <- predict(richness.fit.avg, newdata=richness.fitted,type="response")
head(richness.fitted.avg)

# Plot with trend line 
richness.plot.summary.avg <- ggplot2::ggplot(data=richness.avg,
                                    aes(x=Year, y=richness))+
   ggtitle("Total Species richness")+
   ylab("Mean Total % richness")+
   geom_point(size=3,position=position_dodge(w=0.2))+
   geom_line(data=richness.fitted.avg, aes(x=Year,y=pred.mean))+
   facet_wrap(~Study.Area.Name, ncol=1, scales="free" )+
   scale_x_continuous(breaks=min(richness$Year, na.rm=TRUE):max(richness$Year,na.rm=TRUE))+
   geom_text(data=richness.slopes.avg, aes(x=min(richness$Year, na.rm=TRUE), y=max(richness.avg$richness, na.rm=TRUE)), 
             label=paste("Slope : ",round(richness.slopes.avg$slope,2), 
                         " ( SE "  ,round(richness.slopes.avg$slope.se,2),")",
                         " p :"    ,round(richness.slopes.avg$p.value,3)),
                         hjust="left")
richness.plot.summary.avg
ggsave(plot=richness.plot.summary.avg, 
       file=paste(file.prefix,'-richness-plot-summary-avg.png',sep=""),
       h=6, w=6, units="in", dpi=300)




#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# 
#  Construct diversity profiles over time 

# Functions to compute the diversity profile
###########################################
#  Measuring species diversity using the entropy measures of Leinster and Cobbold (2012)
#  Code taken from http://jonlefcheck.net/2012/10/23/diversity-as-effective-numbers/


div.profile <- function(community, Z=diag(length(community))){
  # Compute profile scores for a vector of community abundances
  # See the Leinster and Cobbold (2012) paper
  
  # Input data
  #   community - vector of species counts in same order as Z matrix
  #   Z         - similarity matrix for each species vs other species
  #               default is a diagonal matrix in which every species is treated
  #               as functionally distinct
  #   Both community and Z must be ordered in the same way
  #   PerCover  - the percent cover (response variable) for which diversity matrix measured
    temp <- community >0
    community.red <- community[ temp]
    Z.red <- Z[temp,temp]
    p <- community.red/sum(community.red)
    ddply(data.frame(q=c(seq(0,.1,.01),seq(.2,5,.1))), "q", function(q, p){
      if(abs(q-1)>.05) { diversity <- sum(p*(Z.red%*%p)^(q$q-1))^(1/(1-q$q))}
      if(abs(q-1)<=.05){ diversity <- exp(-sum(p*log(Z.red%*%p), na.rm=TRUE))}
      names(diversity) <- 'diversity'
      diversity
    }, p=p)  
  }
  
# We construct the diversity profile for each transect in each year by summing
# the % cover over all the plots in a transect year
cover.transect <- plyr::ddply(veg.df, c("Study.Area.Name","Year","Transect.Label","Species"),
                              plyr::summarize,  cover=sum(Foliar.cover...., na.rm=TRUE))
head(cover.transect)

# We need to make each transect have cover for ALL possible species in the overall database

cover.transect <- plyr::ddply(cover.transect, c("Study.Area.Name"), function (x){
   # extract all possible combination of year and species and transect and expand each transect
   trans.sp.year <- expand.grid(Study.Area.Name=unique(x$Study.Area.Name),
                                Species        =unique(x$Species),
                                Year           =unique(x$Year),
                                Transect.Label =unique(x$Transect.Label), stringsAsFactors=FALSE)
   x <- merge(x, trans.sp.year, all=TRUE)
   x$cover[ is.na(x$cover)] <- 0
   x
})

# Sort the data by species
cover.transect <- cover.transect[ order(cover.transect$Study.Area.Name,cover.transect$Year,
                                        cover.transect$Transect.Label,
                                        cover.transect$Species),]

# We dont have information on the Z matrix (species similarity) so we will
# use the default which is diagonal.
profiles <- ddply(cover.transect, c("Study.Area.Name","Year","Transect.Label"), function(x){
     cat('Profile computatations for ', unlist( x[1,]), "\n")
     profile <- div.profile(x$cover)
     profile
})


# Plot the profiles
plot.profile <- ggplot2::ggplot(data=profiles, aes(x=q, y=diversity, color=Transect.Label, linetype=factor(Year) ))+
  ggtitle("Diversity profiles")+
  ylab("Effective number of species")+
  xlab("q")+
  geom_line()+
  facet_wrap(~Study.Area.Name, ncol=1)+
  scale_linetype_discrete(name="Year")
plot.profile
ggsave(plot=plot.profile, 
       file=paste(file.prefix,'-diversity-profiles.png',sep=""),
       h=6, w=6, units="in", dpi=300)


# Extract the value of effective number of species for q=2 and do the analysis
diversity.transect <- profiles[ profiles$q==2.0,]
diversity.transect
temp <- diversity.transect
temp$diversity <- round(temp$diversity, 1)
print(temp, row.names=FALSE)

# Make a preliminary plot of diversity by years

prelim.diversity.plot <- ggplot(data=diversity.transect, aes(x=Year, y=diversity, color=Transect.Label, linetype=Transect.Label))+
   ggtitle(paste("Effective number of species at q =", format(diversity.transect$q[1],nsmall=2),sep=""))+
   ylab("Effective number of species")+
   geom_point(position=position_dodge(width=.2))+
   geom_line(data=diversity.transect)+
   scale_x_continuous(breaks=min(diversity.transect$Year, na.rm=TRUE):max(diversity.transect$Year, na.rm=TRUE))+
   facet_wrap(~Study.Area.Name, ncol=1)
prelim.diversity.plot 
ggsave(plot=prelim.diversity.plot, 
       file=paste(file.prefix,'-diversity-plot-prelim.png',sep=""),
       h=6, w=6, units="in",dpi=300)


# This is a regression analysis with Year as the trend variable and Transect as a random effects.
# We need to account for the same transect (and plots) being measured over time.
# Because this is a linear mixed model and the because the total diversity is typically large enough, no transformation
# is needed.

# define the YearF effect for process error (year specific effcts)
diversity.transect$YearF           <- factor(diversity.transect$Year)
diversity.transect$Transect.LabelF <- factor(diversity.transect$Transect.Label)
diversity.transect

diversity.fit.lmer <-  lmerTest::lmer(diversity ~ Year + (1|Transect.LabelF) + (1|YearF), data=diversity.transect)
anova(diversity.fit.lmer,dfm='Kenward-Roger')
summary(diversity.fit.lmer)
VarCorr(diversity.fit.lmer)

# Look at the residual plot 
diag.plot <- sf.autoplot.lmer(diversity.fit.lmer)  # residual and other diagnostic plots
plot(diag.plot)
ggplot2::ggsave(plot=diag.plot, 
                file=paste(file.prefix,"-diversity-residual-lmer-plot.png",sep=""),
                h=6, w=6, units="in", dpi=300)

# check for autocorrelation - look at the average residual over time
diversity.transect$resid <- diversity.transect$diversity - predict(diversity.fit.lmer, newdata=diversity.transect, re.form=~0)
mean.resid <- plyr::ddply(diversity.transect, "Year", summarize, mean.resid=mean(resid))
resid.fit <- lm( mean.resid ~ 1, data=mean.resid)
dwres1 <- car::durbinWatsonTest(resid.fit)
dwres1
dwres2 <- lmtest::dwtest(resid.fit)
dwres2


# extract the slope
diversity.slopes <- data.frame(
       Study.Area.Name =diversity.transect$Study.Area.Name[1],
       slope           = fixef(diversity.fit.lmer)["Year"],
       slope.se        = summary(diversity.fit.lmer)$coefficients["Year","Pr(>|t|)"],
       p.value         = summary(diversity.fit.lmer)$coefficients[row.names(summary(diversity.fit.lmer)$coefficients)=="Year"  ,"Pr(>|t|)"], 
       #r2             = summary(diversity.fit.lmer)$r.squared,  # not defined for mixed effect models
       stringsAsFactors=FALSE)
diversity.slopes


# compute the fitted values from the model
diversity.fitted <- data.frame(
                 Study.Area.Name=diversity.transect$Study.Area.Name[1],
                 Year=seq(min(diversity.transect$Year, na.rm=TRUE),max(diversity.transect$Year, na.rm=TRUE), .1),
                 stringsAsFactors=FALSE)
diversity.fitted$pred.mean <- predict(diversity.fit.lmer, newdata=diversity.fitted,type="response", re.form=~0)
head(diversity.fitted)

# Plot with trend line 
diversity.plot.summary <- ggplot2::ggplot(data=diversity.transect,
                                    aes(x=Year, y=diversity))+
   ggtitle(paste("Effective number of species at q =", format(diversity.transect$q[1],nsmall=2),sep=""))+
   ylab("Effective number of species")+
   geom_point(size=3, aes(color=Transect.Label),position=position_dodge(w=0.2))+
   geom_line(data=diversity.fitted, aes(x=Year,y=pred.mean))+
   facet_wrap(~Study.Area.Name, ncol=1, scales="free" )+
   scale_x_continuous(breaks=min(diversity.transect$Year, na.rm=TRUE):max(diversity.transect$Year,na.rm=TRUE))+
   geom_text(data=diversity.slopes, aes(x=min(diversity.transect$Year, na.rm=TRUE), y=max(diversity.transect$diversity, na.rm=TRUE)), 
             label=paste("Slope : ",round(diversity.slopes$slope,2), 
                         " ( SE "  ,round(diversity.slopes$slope.se,2),")",
                         " p :"    ,round(diversity.slopes$p.value,3)),
                         hjust="left")
diversity.plot.summary
ggsave(plot=diversity.plot.summary, 
       file=paste(file.prefix,'-diversity-plot-summary-lmer.png',sep=""),
       h=6, w=6, units="in", dpi=300)



##### if lmer() does not converge, try an approximate analysis on the overall averages 


# Compute the average total diversity for each transect so I can plot these over time
diversity.avg <- plyr::ddply(diversity.transect, c("Study.Area.Name","Year"), plyr::summarize,
                          diversity=mean(diversity, na.rm=TRUE))
diversity.avg

# Make a preliminary plot of average diversity by years

prelim.diversity.plot.avg <- ggplot(data=diversity.avg, aes(x=Year, y=diversity))+
   ggtitle("Mean effective number of species - averaged over all transects in a year")+
   ylab("Mean effective number of species")+
   geom_point(position=position_dodge(width=.2))+
   geom_smooth(method="lm", se=FALSE)+
   scale_x_continuous(breaks=min(diversity.transect$Year, na.rm=TRUE):max(diversity.transect$Year, na.rm=TRUE))+
   facet_wrap(~Study.Area.Name, ncol=1)
prelim.diversity.plot.avg 
ggsave(plot=prelim.diversity.plot, 
       file=paste(file.prefix,'-diversity-plot-prelim-avg.png',sep=""),
       h=6, w=6, units="in",dpi=300)


# This is a simple regression analysis with Year as the trend variable 

diversity.fit.avg <-  lm(diversity ~ Year, data=diversity.avg)
anova(diversity.fit.avg)
summary(diversity.fit.avg)

# Look at the residual plot 
diag.plot <- autoplot(diversity.fit.avg)  # residual and other diagnostic plots
show(diag.plot)
ggplot2::ggsave(plot=diag.plot, 
                file=paste(file.prefix,"-diversity-residual-avg-plot.png",sep=""),
                h=6, w=6, units="in", dpi=300)

# check for autocorrelation - look at the average residual over time
diversity.avg$resid <- diversity.avg$diversity - predict(diversity.fit.avg, newdata=diversity.avg)
mean.resid <- plyr::ddply(diversity.avg, "Year", summarize, mean.resid=mean(resid))
resid.fit <- lm( mean.resid ~ 1, data=mean.resid)
dwres1 <- car::durbinWatsonTest(resid.fit)
dwres1
dwres2 <- lmtest::dwtest(resid.fit)
dwres2


# extract the slope
diversity.slopes.avg <- data.frame(
       Study.Area.Name =diversity.transect$Study.Area.Name[1],
       slope           = coef(diversity.fit.avg)["Year"],
       slope.se        = summary(diversity.fit.avg)$coefficients["Year","Pr(>|t|)"],
       p.value         = summary(diversity.fit.avg)$coefficients[row.names(summary(diversity.fit.avg)$coefficients)=="Year"  ,"Pr(>|t|)"], 
       r2              = summary(diversity.fit.avg)$r.squared, 
       stringsAsFactors=FALSE)
diversity.slopes.avg


# compute the fitted values from the model
diversity.fitted.avg <- data.frame(
                 Study.Area.Name=diversity.transect$Study.Area.Name[1],
                 Year=seq(min(diversity.transect$Year, na.rm=TRUE),max(diversity.transect$Year, na.rm=TRUE), .1),
                 stringsAsFactors=FALSE)
diversity.fitted.avg$pred.mean <- predict(diversity.fit.avg, newdata=diversity.fitted,type="response")
head(diversity.fitted.avg)

# Plot with trend line 
diversity.plot.summary.avg <- ggplot2::ggplot(data=diversity.avg,
                                    aes(x=Year, y=diversity))+
   ggtitle(paste("Effective number of species at q =", format(diversity.transect$q[1],nsmall=2),sep=""))+
   ylab("Mean effective number of species")+
   geom_point(size=3,position=position_dodge(w=0.2))+
   geom_line(data=diversity.fitted.avg, aes(x=Year,y=pred.mean))+
   facet_wrap(~Study.Area.Name, ncol=1, scales="free" )+
   scale_x_continuous(breaks=min(diversity.transect$Year, na.rm=TRUE):max(diversity.transect$Year,na.rm=TRUE))+
   geom_text(data=diversity.slopes.avg, aes(x=min(diversity.transect$Year, na.rm=TRUE), y=max(diversity.avg$diversity, na.rm=TRUE)), 
             label=paste("Slope : ",round(diversity.slopes.avg$slope,2), 
                         " ( SE "  ,round(diversity.slopes.avg$slope.se,2),")",
                         " p :"    ,round(diversity.slopes.avg$p.value,3)),
                         hjust="left")
diversity.plot.summary.avg
ggsave(plot=diversity.plot.summary.avg, 
       file=paste(file.prefix,'-diversity-plot-summary-avg.png',sep=""),
       h=6, w=6, units="in", dpi=300)



