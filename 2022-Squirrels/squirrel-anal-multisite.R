# This script will demonstrate how to analyze the Squirrel data collected as 
# part of the LongTerm Ecological Monitoring Initiative in multiple study areas
# to see if the trends are common over time.
#
# At least two study areas needs to be analyzed here. Enter the sheets in 
# the appropriate section of the code.
#
# This was programmed by Carl James Schwarz, Statistics and Actuarial Science, SFU
# cschwarz@stat.sfu.ca
#
# 2017-02-28 First Edition

# Summary of Protocol
#    Red squirrels regularly emit an audible rattle, 
#    especially when their territories are invaded. 
#    This protocol involves walking a transect (a section of a trail) 
#    and recording the location of rattles heard along the way. 
#
#    Locate as many transects in a given area as possible (up to 5). 
#    Sample them annually but in a different order each year. 
#    Sampling involves walking a defined segment of trail and recording 
#    squirrel rattles or chattering.


# load libraries
library(car)       # for testing for autocorrelation (2 libraries needed - see dwtest)
library(ggfortify) # for residual and other diagnostic plot
library(ggplot2)   # for plotting
library(lmtest)    # for testing for autocorrelation
library(plyr)      # for group processing
library(readxl)    # for opening the Excel spreadsheets and reading off them
library(reshape2)  # for melting and casting
library(emmeans)   # for the comparing the slopes among study areas
library(stringr)   # string handling (like case conversion)
library(lmerTest)  # mixed linear models

# Load some common functions
source("../CommonFiles/common.functions.R")


cat("\n\n ***** Squirrel Analysis - Multi Site *****  \n\n")

# get the data from the Excel work.books.
# we put the list of work books here, including the file type (xls or xlsx).
# You can put multiple stations here because the station information is included on the raw data

work.books.csv <- textConnection(
"file.name
General Survey-squirrels_PWC_2013.xls
General Survey-squirrels_PWC_2014.xls
General Survey-squirrels_PWC_2015.xls
ESKERS_SQUIRREL_2013.xls
ESKERS_SQUIRREL_2014.xls
ESKERS_SQUIRREL_2015-simulated.xls")

work.books <- read.csv(work.books.csv, as.is=TRUE, strip.white=TRUE, header=TRUE)
cat("File names with the data \n")
work.books


# read the transect and survey information from each workbook and put together into a list
# we need a list here because we need to check (later) that all transects are run on all dates.
# we extract the year from the General survey worksheet and add it to the transect data
squirrel.list <- plyr::dlply(work.books, "file.name", function (x){
    cat("Reading data for ", x$file.name, "\n")
    file.name <- file.path("Data", x$file.name)
    transects <- readxl::read_excel(file.name, sheet="Transect Information")
    squirrels <- readxl::read_excel(file.name, sheet="General Survey")
    squirrels$Date  <- as.Date(squirrels$Date, "%d-%b-%y", tz="UTC")
    squirrels$Year  <- as.numeric(format(squirrels$Date, "%Y") )
    transects$Year  <- squirrels$Year[1]
    list(transects=transects, squirrels=squirrels)
})


# paste all of the transect information together and paste all of the general survey information together
transect.df <- plyr::ldply(squirrel.list, function (x){x$transects})
squirrel.df <- plyr::ldply(squirrel.list, function (x){x$squirrels})


#------------ Data Editing -----------
# fix up variable names in the data.frames.
# Variable names in R must start with a letter and contain letters or number or _. 
# Blanks in variable names are not normally allowed. Blanks will be replaced by . (period)
cat("\nOriginal variable names in squirrels data file\n")
names(squirrel.df)

names(squirrel.df) <- make.names(names(squirrel.df))

cat("\nCorrected variable names of data frame\n")
names(squirrel.df)


cat("\nOriginal variable names in transects data frame\n")
names(transect.df)

names(transect.df) <- make.names(names(transect.df))

cat("\nCorrected variable names of transect data frame\n")
names(transect.df)



# Check that the Study Area Name is the same across all years
# Look at the output from the xtabs() to see if there are multiple spellings 
# of the same Study.Area.Name.

# We will convert the Study.Area.Name to Proper Case.
squirrel.df$Study.Area.Name <- stringr::str_to_title(squirrel.df$Study.Area.Name)
xtabs(~Study.Area.Name+Year, data=squirrel.df, exclude=NULL, na.action=na.pass)

transect.df$Study.Area.Name <- stringr::str_to_title(transect.df$Study.Area.Name)
xtabs(~Study.Area.Name+Year, data=transect.df, exclude=NULL, na.action=na.pass)


# Check the dates and year codes to R date format
# Notice that we already converted to R date format for the squirrel.df when we read in the data
xtabs(~Date, data=squirrel.df, exclude=NULL, na.action=na.pass)  # check the date formats. Make sure that all yyyy-mm-dd

xtabs(~Year, data=squirrel.df, exclude=NULL, na.action=na.pass)
xtabs(~Year, data=transect.df, exclude=NULL, na.action=na.pass)


# Check the Transect code to make sure that all the same
# This isn't used anywhere in the analysis but is useful to know
xtabs(~Transect.Label+Year+Study.Area.Name, data=squirrel.df, exclude=NULL, na.action=na.pass)
xtabs(~Transect.Label+Year+Study.Area.Name, data=transect.df, exclude=NULL, na.action=na.pass)


# Check the Species code to make sure that all the same
# This isn't used anywhere in the analysis but is useful to know
xtabs(~Species+Year, data=squirrel.df, exclude=NULL, na.action=na.pass)


# Check the Detection type. We are only interested in CA (calls) - check for upper case everywhere
xtabs(~Detect.Type+Year+Study.Area.Name, data=squirrel.df, exclude=NULL, na.action=na.pass)
squirrel.df <- squirrel.df[ squirrel.df$Detect.Type == "CA",]  # select only calls
xtabs(~Detect.Type+Year+Study.Area.Name, data=squirrel.df, exclude=NULL, na.action=na.pass)


# Summarize the total number of calls to the Year-Date-Transect level
count.transect <- plyr::ddply(squirrel.df, c("Study.Area.Name","Year","Date","Transect.Label"), 
                              plyr::summarize, n.calls=length(Date))
xtabs(~n.calls+Year, data=count.transect)  # Notice that no 0's are present


# Impute 0 values. Create a list of all transect x dates for each year of the study
unique.transect <- unique(transect.df[,c("Study.Area.Name","Year","Transect.Label")])
unique.date     <- unique(squirrel.df[,c("Study.Area.Name","Year","Date")])

# create the combination of transects and dates for Study-area year combination
transect.date.set <- plyr::ddply(unique.transect, c("Study.Area.Name","Year"), function(x, unique.date){
    # Extract the dates for this study area - year combination
    dates <- unique.date[ x$Study.Area.Name[1] == unique.date$Study.Area.Name & 
                          x$Year[1]            == unique.date$Year, "Date" ]
    transect.date <- expand.grid(Transect.Label=x$Transect.Label,
                                 Date          =dates, stringsAsFactors=FALSE)
    transect.date$Study.Area.Name <- x$Study.Area.Name[1]
    transect.date
}, unique.date=unique.date)
head(transect.date.set)

# match up the expanded set with the actual data. Missing values will be generate which will
# be converted to zero
dim(count.transect)
count.transect <- merge(count.transect, transect.date.set, all=TRUE)
dim(count.transect)

# which date/transect combinations were missing
cat("Missing transect data on the following date --- check your data\n")
count.transect[ is.na(count.transect$n.calls),]

# Impute a value of 0 for the total calls
count.transect$n.calls[ is.na(count.transect$n.calls)] <- 0
count.transect[ is.na(count.transect$n.calls),]

# finaly summary table
xtabs(n.calls~Transect.Label+Date+Study.Area.Name, data=count.transect, exclude=NULL, na.action=na.pass)
xtabs(~Transect.Label+Date+Study.Area.Name, data=count.transect, exclude=NULL, na.action=na.pass)


# Summarize the imputed data to one number per year per transect
count.transect <- plyr::ddply(count.transect, c("Study.Area.Name","Year","Transect.Label"), 
                              plyr::summarize, n.calls=mean(n.calls))
count.transect

# reduce the size of the label for purcell
count.transect$Study.Area.Name[ grepl('Purcell', count.transect$Study.Area.Name)] <- "PWC"

# We need to make transect labels specific to the study area
count.transect$Transect.Label <- interaction(count.transect$Transect.Label, count.transect$Study.Area.Name)
head(count.transect)


file.prefix <- file.path("Plots","multisite")


#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#  Analysis of the number of calls over time.

# Make a preliminary plot

prelim.plot <- ggplot(data=count.transect, aes(x=Year, y=log(n.calls), 
                                               color=Transect.Label, shape=Study.Area.Name))+
   ggtitle("Squirrel count data")+
   ylab("log(mean count/transect")+
   geom_point(position=position_dodge(width=.1))+
   geom_line( position=position_dodge(width=.1))+
   facet_wrap(~Study.Area.Name, ncol=1)
prelim.plot 
ggsave(plot=prelim.plot, 
       file=paste(file.prefix, '-count-plot-prelim.png',sep=""),
       h=6, w=6, units="in",dpi=300)


# This is a ANCOVA analysis with Year as the trend variable and Study.Area.Name as the factor.
# We need to account for the same transect being measured over time and process error

# Fit the non-parallel slope model
count.transect$Transect.LabelF  <- factor(count.transect$Transect.Label)
count.transect$Study.Area.NameF <- factor(count.transect$Study.Area.Name)
count.transect$YearF            <- factor(count.transect$Year)
count.fit.np <- lmerTest::lmer(log(n.calls) ~  Year + Study.Area.NameF + Year:Study.Area.NameF + 
                                 (1|Transect.LabelF) + (1|YearF) +(1|YearF:Study.Area.NameF), 
                 data=count.transect)
anova(count.fit.np, ddf='Kenward-Roger')
summary(count.fit.np)

# Fit the model with parallel slopes
count.fit.p <- lmerTest::lmer(log(n.calls) ~  Year + Study.Area.NameF +
                                (1|Transect.LabelF) + (1|YearF) +(1|YearF:Study.Area.NameF), 
                 data=count.transect)
anova(count.fit.p, df='Kenward-Roger')
summary(count.fit.p)


# which model is preferable?
anova(count.fit.p, count.fit.np)


# Extract the slopes from both models
count.all.fits <- list(list(model='Parallel',     fit=count.fit.p), 
                       list(model='Non-parallel', fit=count.fit.np))

count.slopes <- plyr::ldply(count.all.fits, function (x){
   fit.emmo <- emmeans::emtrends(x$fit, 'Study.Area.NameF', var='Year' )
   slopes <- emmeans::cld(fit.emmo)
   slopes$model <- x$model
   print(slopes)
   slopes
})
count.slopes


# Look at the residual plots for both models and save them to the directory
l_ply(count.all.fits, function (x){
  diag.plot <- sf.autoplot.lmer(x$fit)  # residual and other diagnostic plots
  plot(diag.plot)
  ggsave(plot=diag.plot, 
         file=paste(file.prefix,"-count-residual-plot-",x$model,".png",sep=""),
         h=6, w=6, units="in", dpi=300)
})



# check for autocorrelation - don't forget that analysis was done on the log-scale
count.transect$resid <- log(count.transect$n.calls) - predict(count.fit.np, newdata=count.transect, re.form=~0)
mean.resid <- plyr::ddply(count.transect, "Year", summarize, mean.resid=mean(resid))
resid.fit <- lm( mean.resid ~ 1, data=mean.resid)
dwres1 <- car::durbinWatsonTest(resid.fit)
dwres1
dwres2 <- lmtest::dwtest(resid.fit)
dwres2

# compute the fitted values from the model for both the parallel and non-parallel models
# don't forget to back-transform the resposne
count.fitted <- plyr::ldply(count.all.fits, function(x,count.transect){
   # generate sequence of year values to get fitted values to draw the lines
   sl <- unique(count.transect[, c("Study.Area.Name","Transect.Label")])
   newdata <- plyr::ddply(sl, c("Study.Area.Name","Transect.Label"), function(x){
       newdata <- data.frame(Study.Area.Name=x$Study.Area.Name,
                             Transect.Label =x$Transect.Label,
                             Year=seq(min(count.transect$Year, na.rm=TRUE),max(count.transect$Year, na.rm=TRUE), .1))
       newdata
   })     
   newdata$Study.Area.NameF <- factor(newdata$Study.Area.Name)
   newdata$Transect.LabelF  <- factor(newdata$Transect.Label)
   newdata$pred.mean <- exp(predict(x$fit, newdata=newdata, type="response", re.form=~0))
   # we now must average over all of the transect labels
   newdata <- plyr::ddply(newdata, c("Study.Area.Name","Year"), summarize, pred.mean=mean(pred.mean))
   newdata$model <- x$model
   newdata
},count.transect=count.transect)
head(count.fitted)


# Plot with trend lines for each model
plyr::d_ply(count.fitted, "model", function(x, count.transect, count.slopes){
   count.slopes <- count.slopes[ count.slopes$model == x$model[1],]
   count.plot.summary <- ggplot2::ggplot(data=count.transect,
                                    aes(x=Year, y=n.calls))+
     ggtitle(paste("Squirrel count with  ",x$model[1],' model',sep=""))+
     ylab("Squirrel Count")+
     geom_point(size=3, aes(color=Transect.Label, shape=Study.Area.Name), position=position_dodge(w=.1))+
     geom_line(data=x, aes(y=pred.mean, linetype=Study.Area.Name))+
     scale_x_continuous(breaks=min(count.transect$Year,na.rm=TRUE):max(count.transect$Year,na.rm=TRUE))+
     geom_text(data=count.slopes, aes(x=min(count.transect$Year, na.rm=TRUE), y=max(count.transect$n.calls, na.rm=TRUE)), 
             label=paste(paste("Slope (on log scale) : ",format(round(count.slopes$Year.trend,2),nsmall=2), 
                         " ( SE "  , format(round(count.slopes$SE,2),nsmall=2),")",
                         ":",count.slopes$Study.Area.NameF,sep=""),collapse="\n"),
                         hjust="left")
   plot(count.plot.summary)
   ggsave(plot=count.plot.summary, 
          file=paste(file.prefix,'-count-plot-summary-',x$model[1],'.png',sep=""),
          h=6, w=6, units="in", dpi=300)
}, count.transect=count.transect, count.slopes=count.slopes)

