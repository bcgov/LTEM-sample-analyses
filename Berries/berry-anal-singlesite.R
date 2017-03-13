# This script will demonstrate how to analyze the Soapberry data collected as 
# part of the LongTerm Ecological Monitoring Initiative
#
# Only one study area at time can only be analyzed with this script. 
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


cat("\n\n ***** Soap Berry Analysis - Single Site Analysis *****  \n\n")

# get the data from the Excel work.books.
# we put the list of work books here, including the file type (xls or xlsx).
# You can put multiple stations here because the station information is included on the raw data

work.books.csv <- textConnection(
"file.name
EskersPark2013.csv
EskersPark2014.csv
EskersPark2015.csv
")

work.books <- read.csv(work.books.csv, as.is=TRUE, strip.white=TRUE, header=TRUE)
cat("File names with the data \n")
work.books


# read each workbook and put all of the data together into one big data frame
# There is some sort of problem with the 2015 excel file so I'm using csv her.
soap.df <- plyr::ddply(work.books, "file.name", function(x){
   cat("Reading in workboosk :", x$file.name, "\n")
   #data <- readxl::read_excel(x$file.name, sheet="General Survey")
   file.name <- file.path("Data",x$file.name)
   data <- read.csv(file.name, as.is=TRUE, strip.white=TRUE, header=TRUE)
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


# Check that the Study Area Name is the same across all years
# Look at the output from the xtabs() to see if there are multiple spellings 
# of the same Study.Area.Name.

# We will convert the Study.Area.Name to Proper Case.
soap.df$Study.Area.Name <- stringr::str_to_title(soap.df$Study.Area.Name)
xtabs(~Study.Area.Name, data=soap.df, exclude=NULL, na.action=na.pass)

# Convert dates to R date format
xtabs(~Date, data=soap.df, exclude=NULL, na.action=na.pass)  # check the date formats. Make sure that all yyyy-mm-dd
soap.df$Date <- as.Date(soap.df$Date, "%d-%b-%y")
soap.df$Year <- as.numeric(format(soap.df$Date, "%Y"))
xtabs(~Date+Year, data=soap.df, exclude=NULL, na.action=na.pass)  # check the date formats. Make sure that all yyyy-mm-dd

# Check the Species code to make sure that all the same
# This isn't used anywhere in the analysis but is useful to know
xtabs(~Species+Year, data=soap.df, exclude=NULL, na.action=na.pass)

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

# Get the file prefix
file.prefix <- make.names(soap.df$Study.Area.Name[1])
file.prefix <- gsub(".", '-', file.prefix, fixed=TRUE) # convert . to -
file.prefix <- file.path("Plots", file.prefix)



#----------------------------------------------------------------------------------------
#  Analysis of the mean weight of berries.

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

# Fit a linear trend through the data and check for evidence of a trend.
weight.fit <- lm(Mean.weight ~ Year, data=berry.weight)
anova(weight.fit)
summary(weight.fit)$coefficients
 

# Look at the residual plots and save them to the directory
diag.plot <- autoplot(weight.fit)  # residual and other diagnostic plots
show(diag.plot)
ggsave(plot=diag.plot, 
       file=paste(file.prefix,"-weight-residual-plot.png",sep=""),
       h=6, w=6, units="in", dpi=300)


# check for autocorrelation
dwres1 <- car::durbinWatsonTest(weight.fit)
dwres1
dwres2 <- lmtest::dwtest(weight.fit)
dwres2


# extract a table of slopes 
weight.slopes <- data.frame(
  Study.Area.Name=berry.weight$Study.Area.Name[1],
  slope    = coef(weight.fit)["Year"],
  slope.se = sqrt(diag(vcov(weight.fit)))["Year"],
  p.value  = summary(weight.fit)$coefficients[row.names(summary(weight.fit)$coefficients)=="Year"  ,"Pr(>|t|)"], 
  r2       = summary(weight.fit)$r.squared,
  stringsAsFactors=FALSE)
weight.slopes

# Plot with trend line with a separate plot for each StudyArea
weight.plot.summary <- ggplot2::ggplot(data=berry.weight,
                                    aes(x=Year, y=Mean.weight))+
   ggtitle("Mean berry weight ")+
   ylab("Mean weight (g)")+
   geom_point(size=3)+
   geom_smooth(method="lm", se=FALSE)+
   facet_wrap(~Study.Area.Name, ncol=2, scales="free" )+
   scale_x_continuous(breaks=2010:2020)+
   geom_text(data=weight.slopes, aes(x=min(berry.weight$Year, na.rm=TRUE), y=max(berry.weight$Mean.weight, na.rm=TRUE)), 
             label=paste("Slope : ",round(weight.slopes$slope,2), 
                         " ( SE "  ,round(weight.slopes$slope.se,2),")",
                         " p :"    ,round(weight.slopes$p.value,3)),
                         hjust="left")
weight.plot.summary
ggsave(plot=weight.plot.summary, 
       file=paste(file.prefix,'-weight-plot-summary.png',sep=""),
       h=6, w=6, units="in", dpi=300)



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

soap.df$BushF <- factor(soap.df$Bush)
soap.df$StemF <- factor(soap.df$Stem)
soap.df$YearF<- factor(soap.df$Year)  # This represents process error 

stem.diam <- soap.df[!is.na(soap.df$Stem.diameter..mm.),]
  
# do the fit
stem.fit <- lmerTest::lmer(Stem.diameter..mm. ~ Year + (1|YearF) + (1|BushF) + (1|StemF), data=stem.diam)
anova(stem.fit, dfm="Kenward-Roger")
summary(stem.fit)$coefficients
VarCorr(stem.fit)


# Look at the residual plots and save  to the directory
diag.plot <- sf.autoplot.lmer(  stem.fit)  # residual and other diagnostic plots
plot(diag.plot)
ggsave(plot=diag.plot, 
       file=paste(file.prefix,"-stem-residual-plot.png",sep=""),
       h=6, w=6, units="in", dpi=300)


# test for autocorrelation by finding the average residual for each year
stem.diam$resid <- stem.diam$Stem.diameter..mm. - predict(stem.fit, newdata=stem.diam, re.form=~0)
mean.resid <- plyr::ddply(stem.diam, "Year", summarize, mean.resid=mean(resid))
resid.fit <- lm( mean.resid ~ 1, data=mean.resid)
dwres1 <- car::durbinWatsonTest(resid.fit)
dwres1
dwres2 <- lmtest::dwtest(resid.fit)
dwres2


# extract a table of statistics for each study area
stem.slopes <- data.frame(
  Study.Area.Name = stem.diam$Study.Area.Name[1],
  slope           = fixef(  stem.fit)["Year"],
  slope.se        = sqrt(diag(vcov(  stem.fit)))[names(fixef(  stem.fit))=="Year"],
  p.value         = summary(  stem.fit)$coefficients[row.names(summary(  stem.fit)$coefficients)=="Year"  ,"Pr(>|t|)"], 
  stringsAsFactors=FALSE)
stem.slopes

# compute the fitted values from the model
stem.fitted <- data.frame(
                 Study.Area.Name=stem.diam$Study.Area.Name[1],
                 Year=seq(min(stem.diam$Year, na.rm=TRUE),max(stem.diam$Year, na.rm=TRUE), .1),
                 stringsAsFactors=FALSE)
stem.fitted$pred.mean <- predict(stem.fit, newdata=stem.fitted, type="response", re.form=~0)
head(stem.fitted)


# Plot with trend line with a separate plot for each StudyArea
# We add in the observed mean for each year to look at autocorrrelation overtime
stem.yearly.mean <- plyr::ddply(stem.diam, c("Study.Area.Name","Year"), plyr::summarize,
                                mean.stem=mean(Stem.diameter..mm., na.rm=TRUE))
stem.yearly.mean



stem.plot.summary <- ggplot2::ggplot(data=soap.df,
                                    aes(x=Year, y=Stem.diameter..mm.))+
   ggtitle("Stem Diameter ")+
   ylab("Stem diameter (mm)")+
   geom_point(size=1, aes(color=Bush), position=position_dodge(w=.1))+
   geom_point(data=stem.yearly.mean, aes(y=mean.stem), shape="X", size=4)+
   geom_line(data=stem.fitted, aes(y=pred.mean))+
   facet_wrap(~Study.Area.Name, ncol=2, scales='free')+
   scale_x_continuous(breaks=min(stem.diam$Year,na.rm=TRUE):max(stem.diam$Year,na.rm=TRUE))+
   geom_text(data=stem.slopes, aes(x=min(soap.df$Year, na.rm=TRUE), y=max(soap.df$Stem.diameter..mm., na.rm=TRUE)), 
             label=paste("Slope : ",round(stem.slopes$slope,2), 
                         " ( SE "  ,round(stem.slopes$slope.se,2),")",
                         " p :"    ,round(stem.slopes$p.value,3)),
                         hjust="left")
stem.plot.summary
ggsave(plot=stem.plot.summary, 
       file=paste(file.prefix,'-stem-plot-summary.png',sep=""),
       h=6, w=6, units="in", dpi=300)





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

soap.df$BushF <- factor(soap.df$Bush)
soap.df$StemF <- factor(soap.df$Stem)
soap.df$YearF<- factor(soap.df$Year)  # This represents process error 

# remove missing values
count.df <- soap.df[ !is.na(soap.df$Berry.count),]

count.fit <- lmerTest::lmer(log(Berry.count+.5) ~ Year + (1|Year)+  (1|Bush) + (1|Stem), data=count.df)
anova(count.fit, dfm="Kenward-Roger")
summary(count.fit)$coefficients
VarCorr(count.fit)


# Look at the residual plots and save them to the directory
diag.plot <- sf.autoplot.lmer(count.fit)  # residual and other diagnostic plots
plot(diag.plot)
ggsave(plot=diag.plot, 
       file=paste(file.prefix,"-count-residual-plot.png",sep=""),
       h=6, w=6, units="in", dpi=300)


# check for autocorreclation 
count.df$resid <- log(count.df$Berry.count+.5) - predict(count.fit, newdata=count.df, re.form=~0)
mean.resid <- plyr::ddply(count.df, "Year", summarize, mean.resid=mean(resid))
resid.fit <- lm( mean.resid ~ 1, data=mean.resid)
dwres1 <- car::durbinWatsonTest(resid.fit)
dwres1
dwres2 <- lmtest::dwtest(resid.fit)
dwres2



# extract a table of the slopes
count.slopes <- data.frame(
       Study.Area.Name = count.df$Study.Area.Name[1],
       slope           = fixef(count.fit)["Year"],
       slope.se        = summary(count.fit)$coefficients["Year","Pr(>|t|)"],
       p.value         = summary(count.fit)$coefficients[row.names(summary(count.fit)$coefficients)=="Year"  ,"Pr(>|t|)"], 
       #r2             = summary(count.fit)$r.squared,  # not defined for mixed effect models
       stringsAsFactors=FALSE)
count.slopes

# Plot with trend line with a separate plot for each StudyArea
count.yearly.mean <- plyr::ddply(count.df, c("Study.Area.Name","Year"), plyr::summarize,
                                mean.count=mean(Berry.count, na.rm=TRUE))
count.yearly.mean

# compute the fitted values from the model
# The model was run on the log(average count), so we need to back transform
count.fitted <- data.frame(
                 Study.Area.Name=count.df$Study.Area.Name[1],
                 Year=seq(min(count.df$Year, na.rm=TRUE),max(count.df$Year, na.rm=TRUE), .1),
                 stringsAsFactors=FALSE)
count.fitted$pred.mean <- exp(predict(count.fit, newdata=count.fitted,type="response", re.form=~0))
head(count.fitted)



# make some summary plots
count.plot.summary <- ggplot2::ggplot(data=count.df,
                                    aes(x=Year, y=Berry.count))+
   ggtitle("Berry Count ")+
   ylab("log(Berry Count + .5)")+
   geom_point(size=1, aes(color=Bush), position=position_dodge(w=.1))+
   geom_point(data=count.yearly.mean, aes(y=mean.count), shape="X", size=4)+
   geom_line(data=count.fitted, aes(y=pred.mean))+
   facet_wrap(~Study.Area.Name, ncol=2, scales='free')+
   scale_x_continuous(breaks=min(count.df$Year):max(count.df$Year, na.rm=TRUE))+
   geom_text(data=count.slopes, aes(x=min(soap.df$Year,na.rm=TRUE), y=max(soap.df$Berry.count,na.rm=TRUE)), 
             label=paste("Slope (on log scale) : ",round(count.slopes$slope,2), 
                         " ( SE "  ,round(count.slopes$slope.se,2),")",
                         " p :"    ,round(count.slopes$p.value,3)),
                         hjust="left")
count.plot.summary
ggsave(plot=count.plot.summary, 
       file=paste(file.prefix,'-count-plot-summary.png',sep=""),
       h=6, w=6, units="in", dpi=300)



