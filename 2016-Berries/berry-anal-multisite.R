# This script will demonstrate how to analyze the Soapberry data collected as 
# part of the LongTerm Ecological Monitoring Initiative to see if the trends
# are the same at multiple Survey Areas.
#
# At least two study areas are needed for this script, each with at least 3 years of data.
#
# This was programmed by Carl James Schwarz, Statistics and Actuarial Science, SFU
# cschwarz@stat.sfu.ca
#
# 2017-02-28 First Edition

# Summary of Protocol
#   We count the number of berries produced on the exact same stems 
#   of soapberry bushes each year to give an index of soapberry production. 
#   No attempt to measure the total biomass production of soapberries per hectare.
#
#   Choose an area rich in soapberries is located for permanent monitoring. 
#   Choose 10 robust plants. Mark 2 branches on each plant for sampling.
#   The number of berries on each branch is recorded.
#   The branch diameter (mm) is also recorded
#
#   A sample of 25-50 ripe berries is selected and measured for average mass.


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


cat("\n\n ***** Soap Berry Analysis - Multisite *****  \n\n")

# get the data from the Excel work.books.
# we put the list of work books here, including the file type (xls or xlsx).
# You can put multiple stations here because the station information is included on the raw data

work.books.csv <- textConnection(
"file.name
EskersPark2013.csv
EskersPark2014.csv
EskersPark2015.csv
EskersPark2016-simulated.csv
Berries_SchoenLake_2013.csv
Berries_SchoenLake_2014.csv
Berries_SchoenLake_2015-simulated.csv
Berries_SchoenLake_2016-simulated.csv
")

work.books <- read.csv(work.books.csv, as.is=TRUE, strip.white=TRUE, header=TRUE)
cat("File names with the data \n")
work.books


# read each workbook and put all of the data together into one big data frame
soap.df <- plyr::ddply(work.books, "file.name", function(x){
   cat("Reading in workbook :", x$file.name, "\n")
   #data <- readxl::read_excel(x$file.name, sheet="General Survey")
   file.name <- file.path("Data", x$file.name)
   data <- read.csv(file.name, header=TRUE, as.is=TRUE, strip.white=TRUE)
   data
})


#------------ Data Editing -----------
# fix up variable names in the data.frame.
# Variable names in R must start with a letter and contain letters or number or _. 
# Blanks in variable names are not normally allowed. Blanks will be replaced by . (period)
cat("\nOriginal variable names in data frame\n")
names(soap.df)

names(soap.df) <- make.names(names(soap.df))

cat("\nCorrected variable names of data frame\n")
names(soap.df)

# Convert dates to R date format
xtabs(~Date, data=soap.df, exclude=NULL, na.action=na.pass)  # check the date formats. Make sure that all yyyy-mm-dd
soap.df$Date <- as.Date(soap.df$Date, "%d-%b-%y")
soap.df$Year <- as.numeric(format(soap.df$Date, "%Y"))
xtabs(~Date+Year, data=soap.df, exclude=NULL, na.action=na.pass)  # check the date formats. Make sure that all yyyy-mm-dd


# Check that the Study Area Name is the same across all years
# Look at the output from the xtabs() to see if there are multiple spellings 
# of the same Study.Area.Name.

# We will convert the Study.Area.Name to Proper Case.
soap.df$Study.Area.Name <- stringr::str_to_title(soap.df$Study.Area.Name)
xtabs(~Study.Area.Name+Year, data=soap.df, exclude=NULL, na.action=na.pass)


# Check the Species code to make sure that all the same
# This isn't used anywhere in the analysis but is useful to know
xtabs(~Species, data=soap.df, exclude=NULL, na.action=na.pass)

# Look at Sample Station Label and extract the bush and stem numbers
#   The Bush number is the string up to the - (dash)
#   The Stem number is full string. We need to use the combination of bush and 
#       stem number of identify stems because plan01stem01 is a different stem than
#       plan02stem01.
# 
xtabs(~Sample.Station.Label+Year, data=soap.df, exclude=NULL, na.action=na.pass)

soap.df$Bush <- substr(soap.df$Sample.Station.Label, 1, -1+regexpr('-',soap.df$Sample.Station.Label, fixed=TRUE))
soap.df$Stem <- soap.df$Sample.Station.Label

# check the extraction
xtabs(~Sample.Station.Label+Bush, data=soap.df, exclude=NULL, na.action=na.pass)

# The sample size for the berry weight is given in the N.for.Weight field
# Note that even though this field is replicated for all observations, there is ONLY one
# measurement of weight taken for the entire year.
xtabs(~Year+N.for.Weight, data=soap.df, exclude=NULL, na.action=na.pass)

# check other comments. You may need to adjust the data to account for 
# problems in the data. 
# For example, if the comment is "heavily browsed" make sure that 
#   the Berry.count is NA and not 0
xtabs(~Comments+Berry.count, data=soap.df, exclude=NULL, na.action=na.pass)
xtabs(~Comments+Year,        data=soap.df, exclude=NULL, na.action=na.pass)

# check the berry count. These need to be numeric
xtabs(~Year+Berry.count, data=soap.df, exclude=NULL, na.action=na.pass)

soap.df$Berry.count <- as.numeric(soap.df$Berry.count)
xtabs(~Year+Berry.count, data=soap.df, exclude=NULL, na.action=na.pass)

file.prefix <- file.path("Plots","multisite")

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#  Analysis of the mean weight of berries. Is the trend the same over time.

# Look at mean weight of berries over time
# Remember there is only one number per year so we need to extract from the database.
# We take the mean for each year. We also take the SD. If this is >0 then the
# data in the database is not consistent

berry.weight <- plyr::ddply(soap.df, c("Study.Area.Name","Year"), plyr::summarize, 
                            Mean.weight   =mean(Average.weight..gms., na.rm=TRUE),
                            Mean.weight.sd=sd(  Average.weight..gms., na.rm=TRUE),
                            N.Mean.weight =mean(N.for.Weight, na.rm=TRUE))

# check to see if any of the Mean.weight.sd >0 indicating that the data is not consistent on the sheets
berry.weight[ berry.weight$Mean.weight.sd >0,]

# listing of data
berry.weight

prelim.plot.weight <- ggplot(data=berry.weight, aes(x=Year, y=Mean.weight, 
                                               color=Study.Area.Name, shape=Study.Area.Name))+
   ggtitle("Berry mean weight")+
   ylab("Berry mean weight (g)")+
   geom_point(position=position_dodge(width=.1))+
   geom_line( position=position_dodge(width=.1))
#   facet_wrap(~Study.Area.Name, ncol=1)
prelim.plot.weight 
ggsave(plot=prelim.plot.weight, 
       file=paste(file.prefix,'-weight-plot-prelim.png',sep=""), 
       h=4, w=6, units="in",dpi=300)


# Fit a ANCOVA linear trend through the data and check for evidence of a trend.
# We fit both a parallel and non-parallel slope model
# We need to account for potential process error (year specific effects)
berry.weight$Study.Area.NameF <- factor(berry.weight$Study.Area.Name)
berry.weight$YearF            <- factor(berry.weight$Year)


# fit the non-parallel slope model
weight.np.fit <- lmerTest::lmer(Mean.weight ~ Year + Study.Area.NameF + Year:Study.Area.NameF +
                                  (1|YearF), data=berry.weight)
anova(weight.np.fit,ddf='kenward-roger')
summary(weight.np.fit)$coefficients
VarCorr(weight.np.fit)

# fit the parallel slope model
weight.p.fit <- lmerTest::lmer(Mean.weight ~ Year + Study.Area.NameF  +
                                 (1|YearF), data=berry.weight)
anova(weight.p.fit,ddf='kenward-roger')
summary(weight.p.fit)$coefficients
VarCorr(weight.p.fit)

weight.all.fits <- list( list(model='Parallel',     fit=weight.p.fit),
                         list(model='Non-parallel', fit=weight.np.fit))


# which model is preferable?
anova(weight.p.fit, weight.np.fit, test='F')


# Extract the slope from both models
weight.slopes <- plyr::ldply(weight.all.fits, function (x){
   fit.lsmo <- emmeans::emtrends(x$fit, 'Study.Area.NameF', var='Year' )
   slopes <- emmeans::cld(fit.lsmo)
   slopes$model <- x$model
   print(slopes)
   slopes
})
weight.slopes


# Look at the residual plots and save them to the directory
l_ply(weight.all.fits, function(x){
   diag.plot <- sf.autoplot.lmer(x$fit)  # residual and other diagnostic plots
   plot(diag.plot)
   ggsave(plot=diag.plot, 
          file=paste(file.prefix,"-weight-residual-plot-",x$model,".png",sep=""),
          h=6, w=6, units="in", dpi=300)
})


# check for autocorrelation - not implemented for ANCOVA models


# compute the fitted values from the model for both the parallel and non-parallel models
weight.fitted <- plyr::ldply(weight.all.fits, function(x,berry.weight){
   # generate sequence of year values to get fitted values to draw the lines
   sl <- unique(berry.weight[, c("Study.Area.Name"),drop=FALSE])
   newdata <- plyr::ddply(sl, c("Study.Area.Name"), function(x){
       newdata <- data.frame(Study.Area.Name=x$Study.Area.Name,
                             Year=seq(min(berry.weight$Year, na.rm=TRUE),
                                      max(berry.weight$Year, na.rm=TRUE), .1))
       newdata
   })     
   newdata$Study.Area.NameF <- factor(newdata$Study.Area.Name)
   newdata$pred.mean <- predict(x$fit, newdata=newdata, type="response", re.form=~0)
   # we now must average over all of the transect labels
   newdata <- plyr::ddply(newdata, c("Study.Area.Name","Year"), summarize, pred.mean=mean(pred.mean))
   newdata$model <- x$model
   newdata
},berry.weight=berry.weight)
head(weight.fitted)


# Plot both models with the fitted lines
plyr::d_ply(weight.fitted, "model", function(x, berry.weight, weight.slopes){
   weight.slopes <- weight.slopes[ weight.slopes$model == x$model[1],]
   weight.plot.summary <- ggplot2::ggplot(data=berry.weight,
                                    aes(x=Year, y=Mean.weight))+
     ggtitle(paste("Mean berry weight with  ",x$model[1],' model',sep=""))+
     ylab("Mean weight (g)")+
     geom_point(size=3, aes(color=Study.Area.Name))+
     geom_line(data=x, aes(y=pred.mean, linetype=Study.Area.Name))+
     scale_x_continuous(breaks=min(berry.weight$Year,na.rm=TRUE):max(berry.weight$Year,na.rm=TRUE))+
     geom_text(data=weight.slopes, aes(x=min(berry.weight$Year, na.rm=TRUE), y=max(berry.weight$Mean.weight, na.rm=TRUE)), 
             label=paste(paste("Slope : ",format(round(weight.slopes$Year.trend,2),nsmall=2), 
                         " ( SE "  , format(round(weight.slopes$SE,2),nsmall=2),")",
                         " :",weight.slopes$Study.Area.NameF,sep=""),collapse="\n"),
                         hjust="left")
   plot(weight.plot.summary)
   ggsave(plot=weight.plot.summary, 
          file=paste(file.prefix,'-weight-plot-summary-',x$model[1],'.png',sep=""), 
          h=6, w=6, units="in", dpi=300)
}, berry.weight=berry.weight, weight.slopes=weight.slopes)



#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#  Analysis of the stem diameter.
#  Each stem is measured on each bush in each year.
#  We need to account for the repeated measurements of each stem/bush combination over time
#  using a linear mixed models. 
#  We need to account for year process error. Essentially, you need to get one number
#  per year for the analysis (the mean), but we need to account for the repeated measurements

dim(soap.df)
stem.diam <- soap.df[ !is.na(soap.df$Stem.diameter..mm.),]
dim(stem.diam)

# Compute a simple mean for each study area in each year
stem.diam.mean <- plyr::ddply(stem.diam, c("Study.Area.Name","Year"), plyr::summarize,
                              mean.diam=mean(Stem.diameter..mm.,na.rm=TRUE))

prelim.plot.stem <- ggplot2::ggplot(data=stem.diam, aes(x=Year, y=Stem.diameter..mm., 
                             color=Study.Area.Name))+
   ggtitle("Stem Diameter")+
   ylab("Stem Diameter")+
   geom_point(position=position_dodge(width=.1))+
   geom_line(data=stem.diam.mean, aes(y=mean.diam), position=position_dodge(width=.1))
#   facet_wrap(~Study.Area.Name, ncol=1)
prelim.plot.stem
ggsave(plot=prelim.plot.stem,
       file=paste(file.prefix,'-stem-plot-prelim.png',sep=""), 
       h=4, w=6, units="in",dpi=300)

# Fit a ANCOVA linear trend through the data and check for evidence of a trend.
# We fit both a parallel and non-parallel slope model
# We need to account for potential process error (year specific effects)
stem.diam$Study.Area.NameF <- factor(stem.diam$Study.Area.Name)
stem.diam$BushF            <- factor(stem.diam$Bush)
stem.diam$StemF            <- factor(stem.diam$Stem)
stem.diam$YearF            <- factor(stem.diam$Year)


# fit the non-parallel slope model
stem.np.fit <- lmerTest::lmer(Stem.diameter..mm. ~ Year + Study.Area.NameF + Year:Study.Area.NameF 
                              + (1|YearF) + (1|StemF) + (1|YearF:Study.Area.NameF), data=stem.diam)
anova(stem.np.fit,ddf='kenward-roger')
summary(stem.np.fit)$coefficients
VarCorr(stem.np.fit)

# fit the parallel slope model
stem.p.fit <- lmerTest::lmer(Stem.diameter..mm. ~ Year + Study.Area.NameF  
                             + (1|YearF) + (1|StemF) + (1|YearF:Study.Area.NameF), data=stem.diam)
anova(stem.p.fit,ddf='kenward-roger')
summary(stem.p.fit)$coefficients
VarCorr(stem.p.fit)

stem.all.fits <- list( list(model='Parallel',     fit=stem.p.fit),
                         list(model='Non-parallel', fit=stem.np.fit))


# which model is preferable?
anova(stem.p.fit, stem.np.fit, test='F')


# Extract the slope from both models
stem.slopes <- plyr::ldply(stem.all.fits, function (x){
   fit.lsmo <- emmeans::emtrends(x$fit, 'Study.Area.NameF', var='Year' )
   slopes <- emmeans::cld(fit.lsmo)
   slopes$model <- x$model
   print(slopes)
   slopes
})
stem.slopes


# Look at the residual plots and save them to the directory
l_ply(stem.all.fits, function(x){
   diag.plot <- sf.autoplot.lmer(x$fit)  # residual and other diagnostic plots
   plot(diag.plot)
   ggsave(plot=diag.plot, 
          file=paste(file.prefix,"-stem-residual-plot-",x$model,".png",sep=""),
          h=6, w=6, units="in", dpi=300)
})


# check for autocorrelation - not implemented for ANCOVA models


# compute the fitted values from the model for both the parallel and non-parallel models
stem.fitted <- plyr::ldply(stem.all.fits, function(x,stem.diam){
   # generate sequence of year values to get fitted values to draw the lines
   sl <- unique(stem.diam[, c("Study.Area.Name"),drop=FALSE])
   newdata <- plyr::ddply(sl, c("Study.Area.Name"), function(x){
       newdata <- data.frame(Study.Area.Name=x$Study.Area.Name,
                             Year=seq(min(stem.diam$Year, na.rm=TRUE),
                                      max(stem.diam$Year, na.rm=TRUE), .1))
       newdata
   })     
   newdata$Study.Area.NameF <- factor(newdata$Study.Area.Name)
   newdata$pred.mean <- predict(x$fit, newdata=newdata, type="response", re.form=~0)
   # we now must average over all of the transect labels
   newdata <- plyr::ddply(newdata, c("Study.Area.Name","Year"), summarize, pred.mean=mean(pred.mean))
   newdata$model <- x$model
   newdata
},stem.diam=stem.diam)
head(stem.fitted)


# Plot both models with the fitted lines
plyr::d_ply(stem.fitted, "model", function(x, stem.diam, stem.slopes){
   stem.slopes <- stem.slopes[ stem.slopes$model == x$model[1],]
   stem.plot.summary <- ggplot2::ggplot(data=stem.diam,
                                    aes(x=Year, y=Stem.diameter..mm.))+
     ggtitle(paste("Mean berry stem with  ",x$model[1],' model',sep=""))+
     ylab("Mean stem (g)")+
     geom_point(size=3, aes(color=Study.Area.Name), position=position_dodge(w=.2))+
     geom_line(data=x, aes(y=pred.mean, linetype=Study.Area.Name))+
     scale_x_continuous(breaks=min(stem.diam$Year,na.rm=TRUE):max(stem.diam$Year,na.rm=TRUE))+
     geom_text(data=stem.slopes, aes(x=min(stem.diam$Year, na.rm=TRUE), y=max(stem.diam$Stem.diameter..mm., na.rm=TRUE)), 
             label=paste(paste("Slope : ",format(round(stem.slopes$Year.trend,2),nsmall=2), 
                         " ( SE "  , format(round(stem.slopes$SE,2),nsmall=2),")",
                         " :",stem.slopes$Study.Area.NameF,sep=""),collapse="\n"),
                         hjust="left")
   plot(stem.plot.summary)
   ggsave(plot=stem.plot.summary, 
          file=paste(file.prefix, '-stem-plot-summary-',x$model[1],'.png',sep=""), 
          h=6, w=6, units="in", dpi=300)
}, stem.diam=stem.diam, stem.slopes=stem.slopes)





#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#  Analysis of the berry counts
#  Each stem is measured on each bush in each year.
#  We need to account for the repeated measurements of each stem/bush combination over time
#  using a linear mixed models.
#  The counts are large enough that we can use a normal theory model on log(count+.5) rather than a Poisson regression.
#  We analyze on the log-scale to account for the increasing variation over time
#  We need to account for year process error. Essentially, you need to get one number
#  per year for the analysis (the mean), but we need to account for the repeated measurements

range(soap.df$Berry.count, na.rm=TRUE)

dim(soap.df)
berry.count <- soap.df[ !is.na(soap.df$Berry.count),]
dim(berry.count)

# Fit a ANCOVA linear trend through the data and check for evidence of a trend.
# We fit both a parallel and non-parallel slope model
# We need to account for potential process error (year specific effects)
berry.count$Study.Area.NameF <- factor(berry.count$Study.Area.Name)
berry.count$BushF            <- factor(berry.count$Bush)
berry.count$StemF            <- factor(berry.count$Stem)
berry.count$YearF            <- factor(berry.count$Year)


# fit the non-parallel slope model
berry.np.fit <- lmerTest::lmer(log(Berry.count+.5) ~ Year + Study.Area.NameF + Year:Study.Area.NameF 
                              + (1|YearF) + (1|StemF) +(1|YearF:Study.Area.NameF), data=berry.count)
anova(berry.np.fit,ddf='kenward-roger')
summary(berry.np.fit)$coefficients
VarCorr(berry.np.fit)

# fit the parallel slope model
berry.p.fit <- lmerTest::lmer(log(Berry.count+.5) ~ Year + Study.Area.NameF  
                              + (1|YearF) + (1|StemF) +(1|YearF:Study.Area.NameF), data=berry.count)
anova(berry.p.fit,ddf='kenward-roger')
summary(berry.p.fit)$coefficients
VarCorr(berry.p.fit)

berry.all.fits <- list( list(model='Parallel',     fit=berry.p.fit),
                         list(model='Non-parallel', fit=berry.np.fit))


# which model is preferable?
anova(berry.p.fit, berry.np.fit, test='F')


# Extract the slope from both models
berry.slopes <- plyr::ldply(berry.all.fits, function (x){
   fit.lsmo <- emmeans::emtrends(x$fit, 'Study.Area.NameF', var='Year' )
   slopes <- emmeans::cld(fit.lsmo)
   slopes$model <- x$model
   print(slopes)
   slopes
})
berry.slopes


# Look at the residual plots and save them to the directory
l_ply(berry.all.fits, function(x){
   diag.plot <- sf.autoplot.lmer(x$fit)  # residual and other diagnostic plots
   plot(diag.plot)
   ggsave(plot=diag.plot, 
          file=paste(file.prefix,"-berry-residual-plot-",x$model,".png",sep=""),
          h=6, w=6, units="in", dpi=300)
})


# check for autocorrelation - not implemented for ANCOVA models


# compute the fitted values from the model for both the parallel and non-parallel models
berry.fitted <- plyr::ldply(berry.all.fits, function(x,berry.count){
   # generate sequence of year values to get fitted values to draw the lines
   sl <- unique(berry.count[, c("Study.Area.Name"),drop=FALSE])
   newdata <- plyr::ddply(sl, c("Study.Area.Name"), function(x){
       newdata <- data.frame(Study.Area.Name=x$Study.Area.Name,
                             Year=seq(min(berry.count$Year, na.rm=TRUE),
                                      max(berry.count$Year, na.rm=TRUE), .1))
       newdata
   })     
   newdata$Study.Area.NameF <- factor(newdata$Study.Area.Name)
   newdata$pred.mean <- exp(predict(x$fit, newdata=newdata, type="response", re.form=~0))
   # we now must average over all of the transect labels
   newdata <- plyr::ddply(newdata, c("Study.Area.Name","Year"), summarize, pred.mean=mean(pred.mean))
   newdata$model <- x$model
   newdata
},berry.count=berry.count)
head(berry.fitted)


# Plot both models with the fitted lines
plyr::d_ply(berry.fitted, "model", function(x, berry.count, berry.slopes){
   berry.slopes <- berry.slopes[ berry.slopes$model == x$model[1],]
   berry.plot.summary <- ggplot2::ggplot(data=berry.count,
                                    aes(x=Year, y=Berry.count))+
     ggtitle(paste("Mean berry count with  ",x$model[1],' model',sep=""))+
     ylab("Mean berry coount")+
     geom_point(size=3, aes(color=Study.Area.Name), position=position_dodge(w=.2))+
     geom_line(data=x, aes(y=pred.mean, linetype=Study.Area.Name))+
     scale_x_continuous(breaks=min(berry.count$Year,na.rm=TRUE):max(berry.count$Year,na.rm=TRUE))+
     geom_text(data=berry.slopes, aes(x=min(berry.count$Year, na.rm=TRUE), y=max(berry.count$Berry.count, na.rm=TRUE)), 
             label=paste(paste("Slope (on log scale): ",format(round(berry.slopes$Year.trend,2),nsmall=2), 
                         " ( SE "  , format(round(berry.slopes$SE,2),nsmall=2),")",
                         " :",berry.slopes$Study.Area.NameF,sep=""),collapse="\n"),
                         hjust="left")
   plot(berry.plot.summary)
   ggsave(plot=berry.plot.summary, 
          file=paste(file.prefix,'-berry-plot-summary',x$model[1],'.png',sep=""), 
          h=6, w=6, units="in", dpi=300)
}, berry.count=berry.count, berry.slopes=berry.slopes)

