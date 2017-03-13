# This script will demonstrate how to analyze the amphibian data collected as 
# part of the LongTerm Ecological Monitoring Initiative

#
# Only one study area at time can only be analyzed with a script. 
#
# This was programmed by Carl James Schwarz, Statistics and Actuarial Science, SFU
# cschwarz@stat.sfu.ca
#
# 2017-02-28 First Edition

# Summary of calling protocol
#    Define a survey transect along or through your wetland. … 
#    Surveyors visit the monitoring site(s) in spring and listen for calling males, 
#    recording the species and approximate number of each. 
#    Repeat surveys increase the probability that species will be detected.”

# Summary of visual protocol
#    Surveyors monitor breeding site(s) during the active season (spring to fall),
#    walking the shoreline of a wetland recording all species and life stages 
#    encountered. These include egg masses, tadpoles and adults. 
#    Repeat surveys increase the probability that species will be detected.”



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


cat("\n\n ***** Amphibian Analysis - Single Site Analysis *****  \n\n")

# get the data from the Excel work.books.
# we put the list of work books here, including the file type (xls or xlsx).
# You can put multiple stations here because the station information is included on the raw data

work.books.csv <- textConnection(
"file.name
General Survey Using Transects-amphibians_AliceLake2013.xls
General Survey Using Transects-amphibians_AliceLake2014.xls
General Survey Using Transects-amphibians_AliceLake2015.xls
")

work.books <- read.csv(work.books.csv, as.is=TRUE, strip.white=TRUE, header=TRUE)
cat("File names with the data \n")
work.books

# read each workbook and put all of the data together into one big data frame
amph.df <- plyr::ddply(work.books, "file.name", function(x){
  file.name <- file.path("Data",x$file.name)
  data <- readxl::read_excel(file.name, sheet="General Survey")
  data
})


#------------ Data Editing -----------
# fix up variable names in the data.frame.
# Variable names in R must start with a letter and contain letters or number or _. 
# Blanks in variable names are not normally allowed. Blanks will be replaced by . (period)
cat("\nOriginal variable names in data frame\n")
names(amph.df)

names(amph.df) <- make.names(names(amph.df))

cat("\nCorrected variable names of data frame\n")
names(amph.df)



# Convert dates to R date format
xtabs(~Date, data=amph.df, exclude=NULL, na.action=na.pass)  # check the date formats. Make sure that all yyyy-mm-dd
amph.df$Date <- as.Date(amph.df$Date, "%d-%b-%y", tz="UTC")
amph.df$Year <- as.numeric(format(amph.df$Date, "%Y"))



# Check that the Study Area Name is the same across all years
# Look at the output from the xtabs() to see if there are multiple spellings 
# of the same Study.Area.Name.

# We will convert the Study.Area.Name to Proper Case.
amph.df$Study.Area.Name <- stringr::str_to_title(amph.df$Study.Area.Name)
xtabs(~Study.Area.Name,      data=amph.df, exclude=NULL, na.action=na.pass)
xtabs(~Study.Area.Name+Year, data=amph.df, exclude=NULL, na.action=na.pass)


# Check the Transect.Labels for typos
xtabs(~Study.Area.Name+Transect.Label, data=amph.df, exclude=NULL, na.action=na.pass)
xtabs(~Transect.Label+Year,            data=amph.df, exclude=NULL, na.action=na.pass)

# When is each station measured?
xtabs(~Transect.Label+Year, data=amph.df, exclude=NULL, na.action=na.pass)
xtabs(~Date+Transect.Label, data=amph.df, exclude=NULL, na.action=na.pass)


# Check the Species code to make sure that they are all ok
# This isn't used anywhere in the analysis but is useful to know
xtabs(~Species, data=amph.df, exclude=NULL, na.action=na.pass)
xtabs(~Species+Year, data=amph.df, exclude=NULL, na.action=na.pass)


# Check the survey type fiels
xtabs(~Year+Survey.Type,  data=amph.df, exclude=NULL, na.action=na.pass)

# Check the Life Stage Code
xtabs(~Year+Life.Stage,  data=amph.df, exclude=NULL, na.action=na.pass)
xtabs(~Survey.Type+Life.Stage, data=amph.df, exclude=NULL, na.action=na.pass)

# list the adult records
amph.df[ amph.df$Life.Stage=="AD",c("Study.Area.Name","Date","Survey.Type","Life.Stage","Count")]

# Get the file prefix
file.prefix <- make.names(amph.df$Study.Area.Name[1])
file.prefix <- gsub(".", '-', file.prefix, fixed=TRUE) # convert blanks to -
file.prefix <- file.path("Plots", file.prefix)


#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#  Analysis of the calliing data

# Not possible at this time. See document.



#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#  Analysis of the visual survey protocol

# Look at # egg masses 
# We need to assume that detection probabilities are consistent over time and space
# There is only one monitoring station at Alice Lake so we don't need to model transect or year-specific
# process error. But I've generalized the code so that it also works with more than one station.

dim(amph.df)
amph.v.df <- amph.df[ amph.df$Survey.Type=='VI',]
dim(amph.v.df)

# Count the total number of egg masses seen
eggmass.count <- plyr::ddply(amph.v.df, c("Study.Area.Name","Transect.Label","Year"), summarize,
                       n.eggmass=sum(Life.Stage=='EG'))
eggmass.count

# Section of code used for testing multiple transects#
#eggmass.count2 <- eggmass.count
#eggmass.count2$Transect.Label ='xx'
#eggmass.count2$n.eggmass <- rpois(1, eggmass.count$n.eggmass)
#eggmass.count2
#eggmass.count <- rbind(eggmass.count, eggmass.count2)

# Make a preliminary plot of total count by date
prelim.egg.plot <- ggplot(data=eggmass.count, aes(x=Year, y=n.eggmass, color=Transect.Label, linetype=Transect.Label))+
   ggtitle("Amphibian egg mass count data")+
   ylab("Amphibian egg masses")+
   geom_point(position=position_dodge(width=.5))+
   geom_line( position=position_dodge(width=.5))+
   facet_wrap(~Study.Area.Name, ncol=1)+
  scale_x_continuous(breaks=min(eggmass.count$Year,na.rm=TRUE):max(eggmass.count$Year,na.rm=TRUE))
prelim.egg.plot 
ggsave(plot=prelim.egg.plot, 
       file=paste(file.prefix,'-plot-prelim-egg.png',sep=""),
       h=6, w=6, units="in",dpi=300)


# This is a regression analysis with Year as the trend variable 
# In this case, there is only one transect, so it is not necessary to have the Transect.Label in the model
# We also don't need to model process (year specific effects) because there is only 1 transect


if(length(unique(eggmass.count$Transect.Label))>1){
      eggmass.count$Transect.LabelF <- factor(eggmass.count$Transect.Label)
      eggmass.count$YearF           <- factor(eggmass.count$Year)
      egg.fit <- lmerTest::lmer(log(n.eggmass) ~ Year + (1|Transect.LabelF) + (1|YearF) , data=eggmass.count)
      print(anova(egg.fit, ddf='kenward-roger'))
      print(summary(egg.fit))
      print(VarCorr(egg.fit))
}
if(length(unique(eggmass.count$Transect.Label))==1){
    # with only 1 transect, not necessary to put random effects effect in the model
    egg.fit <- glm(n.eggmass ~  Year , data=eggmass.count, family=quasipoisson)
    print(Anova(egg.fit, test="F", type=3))
    print(summary(egg.fit))
}

# Get the model diagnostic plots
if(length(unique(eggmass.count$Transect.Label))>1){
   diag.plot <- sf.autoplot.lmer(egg.fit)  # residual and other diagnostic plots
   plot(diag.plot)
}
if(length(unique(eggmass.count$Transect.Label))==1){
   diag.plot <- autoplot(egg.fit)  # residual and other diagnostic plots
   show(diag.plot)
}
ggsave(plot=diag.plot, 
          file=paste(file.prefix,"-egg-residual-plot.png",sep=""),
          h=6, w=6, units="in", dpi=300)


# check for autocorrelation
if(length(unique(eggmass.count$Transect.Label))>1){
  eggmass.count$resid <- log(eggmass.count$n.eggmass) - predict(egg.fit, newdata=eggmass.count, re.form=~0)
}
if(length(unique(eggmass.count$Transect.Label))==1){
  eggmass.count$resid <- log(eggmass.count$n.eggmass) - predict(egg.fit, newdata=eggmass.count, type="link")
}
mean.resid <- plyr::ddply(eggmass.count, "Year", summarize, mean.resid=mean(resid))
resid.fit <- lm( mean.resid ~ 1, data=mean.resid)
dwres1 <- car::durbinWatsonTest(resid.fit)
dwres1
dwres2 <- lmtest::dwtest(resid.fit)
dwres2


# extract a table of statistics for each study area
if(length(unique(eggmass.count$Transect.Label))==1){
  egg.slopes <- data.frame(
     Study.Area.Name =eggmass.count$Study.Area.Name[1],
     slope    = coef(egg.fit)["Year"],
     slope.se =- sqrt(diag(vcov(egg.fit)))["Year"],
     p.value  =- summary(egg.fit)$coefficients[row.names(summary(egg.fit)$coefficients)=="Year"  ,"Pr(>|t|)"],
     stringsAsFactors=FALSE
  )
}
if(length(unique(eggmass.count$Transect.Label))>1){
  egg.slopes <- data.frame(
       Study.Area.Name =eggmass.count$Study.Area.Name[1],
       slope           = fixef(egg.fit)["Year"],
       slope.se        = summary(egg.fit)$coefficients["Year","Pr(>|t|)"],
       p.value         = summary(egg.fit)$coefficients[row.names(summary(egg.fit)$coefficients)=="Year"  ,"Pr(>|t|)"], 
       stringsAsFactors=FALSE)
}
egg.slopes


# compute the fitted values from the model
Transect.Label <- unique(as.character(eggmass.count$Transect.Label))
newdata <- expand.grid(Year=seq(min(eggmass.count$Year, na.rm=TRUE),max(eggmass.count$Year, na.rm=TRUE), .1),
                       Transect.LabelF=Transect.Label)
newdata$Transect.Label <-as.character(newdata$Transect.LabelF)
if(length(unique(eggmass.count$Transect.Label))==1){
   newdata$pred.mean <- predict(egg.fit, newdata=newdata,type="response")
}
if(length(unique(eggmass.count$Transect.Label))>1){
   newdata$pred.mean <- exp(predict(egg.fit, newdata=newdata,type="response", re.form=~0))
}
# we now must average over all of the transect labels
egg.fitted <- plyr::ddply(newdata, c("Year","Transect.Label"), plyr::summarize, pred.mean=mean(pred.mean))
egg.fitted


# Plot with trend line 
egg.plot.summary <- ggplot2::ggplot(data=eggmass.count,
                                    aes(x=Year, y=n.eggmass))+
   ggtitle("Amphibian eggmass count ")+
   ylab("Amphibian Eggmass Count")+
   geom_point(size=3, aes(color=Transect.Label))+
   geom_line(data=egg.fitted, aes(x=Year,y=pred.mean))+
   facet_wrap(~Study.Area.Name, ncol=1, scales="free" )+
   scale_x_continuous(breaks=2010:2020)+
   geom_text(data=egg.slopes, aes(x=min(eggmass.count$Year, na.rm=TRUE), y=max(eggmass.count$n.eggmass, na.rm=TRUE)), 
             label=paste("Slope (on log scale) : ",round(egg.slopes$slope,2), 
                         " ( SE "  ,round(egg.slopes$slope.se,2),")",
                         " p :"    ,round(egg.slopes$p.value,3)),
                         hjust="left")
egg.plot.summary
ggsave(plot=egg.plot.summary, 
       file=paste(file.prefix,'-egg-plot-summary.png',sep=""),
       h=6, w=6, units="in", dpi=300)





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

# Create an occupany record for each visit by species using visual record only
adults.count <- plyr::ddply(amph.v.df, c("Study.Area.Name","Transect.Label","Year"), summarize,
                             n.adults=sum(Life.Stage=='AD'))
adults.count


# Section of code used for testing multiple transects#
#adults.count2 <- adults.count
#adults.count2$Transect.Label ='xx'
#adults.count2$n.adults <- rpois(1, 1+adults.count$n.adults)
#adults.count2
#adults.count <- rbind(adults.count, adults.count2)

# Make a preliminary plot of total count by date
prelim.adult.plot <- ggplot(data=adults.count, aes(x=Year, y=n.adults, color=Transect.Label, linetype=Transect.Label))+
   ggtitle("Amphibian adult counts")+
   ylab("Amphibian adult counts")+
   geom_point(position=position_dodge(width=.5))+
   geom_line( position=position_dodge(width=.5))+
   facet_wrap(~Study.Area.Name, ncol=1)+
   scale_x_continuous(breaks=min(adults.count$Year,na.rm=TRUE):max(adults.count$Year,na.rm=TRUE))
prelim.adult.plot 
ggsave(plot=prelim.adult.plot, 
       file=paste(file.prefix,'-plot-prelim-adult.png',sep=""),
       h=6, w=6, units="in",dpi=300)


# This is a regression analysis with Year as the trend variable 
# In this case, there is only one transect, so it is not necessary to have the Transect.Label in the model
# We also don't need to model process (year specific effects) because there is only 1 transect


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

# Get the model diagnostic plots
if(length(unique(adults.count$Transect.Label))>1){
   diag.plot <- sf.autoplot.lmer(adult.fit)  # residual and other diagnostic plots
   plot(diag.plot)
}
if(length(unique(adults.count$Transect.Label))==1){
   diag.plot <- autoplot(adult.fit)  # residual and other diagnostic plots
   show(diag.plot)
}
ggsave(plot=diag.plot, 
          file=paste(file.prefix,"-adult-residual-plot.png",sep=""),
          h=6, w=6, units="in", dpi=300)


# check for autocorrelation
if(length(unique(adults.count$Transect.Label))>1){
  adults.count$resid <- log(adults.count$n.adults) - predict(adult.fit, newdata=adults.count, re.form=~0)
}
if(length(unique(adults.count$Transect.Label))==1){
  adults.count$resid <- log(adults.count$n.adults) - predict(adult.fit, newdata=adults.count, type="link")
}
mean.resid <- plyr::ddply(adults.count, "Year", summarize, mean.resid=mean(resid))
resid.fit <- lm( mean.resid ~ 1, data=mean.resid)
dwres1 <- car::durbinWatsonTest(resid.fit)
dwres1
dwres2 <- lmtest::dwtest(resid.fit)
dwres2


# extract a table of statistics for each study area
if(length(unique(adults.count$Transect.Label))==1){
  adult.slopes <- data.frame(
     Study.Area.Name =adults.count$Study.Area.Name[1],
     slope    = coef(adult.fit)["Year"],
     slope.se =- sqrt(diag(vcov(adult.fit)))["Year"],
     p.value  =- summary(adult.fit)$coefficients[row.names(summary(adult.fit)$coefficients)=="Year"  ,"Pr(>|t|)"],
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


# compute the fitted values from the model
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
adult.fitted


# Plot with trend line 
adult.plot.summary <- ggplot2::ggplot(data=adults.count,
                                    aes(x=Year, y=n.adults))+
   ggtitle("Amphibian adults count ")+
   ylab("Amphibian adult Count")+
   geom_point(size=3, aes(color=Transect.Label))+
   geom_line(data=adult.fitted, aes(x=Year,y=pred.mean))+
   facet_wrap(~Study.Area.Name, ncol=1, scales="free" )+
   scale_x_continuous(breaks=2010:2020)+
   geom_text(data=adult.slopes, aes(x=min(adults.count$Year, na.rm=TRUE), y=max(adults.count$n.adults, na.rm=TRUE)), 
             label=paste("Slope (on log scale) : ",round(adult.slopes$slope,2), 
                         " ( SE "  ,round(adult.slopes$slope.se,2),")",
                         " p :"    ,round(adult.slopes$p.value,3)),
                         hjust="left")
adult.plot.summary
ggsave(plot=adult.plot.summary, 
       file=paste(file.prefix,'-adult-plot-summary.png',sep=""),
       h=6, w=6, units="in", dpi=300)



