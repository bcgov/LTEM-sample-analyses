# This script will demonstrate how to analyze the waterfowl data collected as 
# part of the LongTerm Ecological Monitoring Initiative

# Notice that this example only has 2 years of data. We simulate a third year to illustrate
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
#    Establish observation points at enough sites 
#    to provide views of the entire survey area. … 
#
#    Choose a date after which migrants are mostly gone. 
#    Count on 3 separate occasions during a 2-3 week period and keep highest count.


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


cat("\n\n ***** Waterfowl Analysis - Single Site *****  \n\n")

# get the data from the Excel work.books.
# we put the list of work books here, including the file type (xls or xlsx).
# You can put multiple stations here because the station information is included on the raw data

work.books.csv <- textConnection(
"file.name
General Survey Using Sample Stations_waterfowl_YPB2013.xls
General Survey Using Sample Stations_waterfowl_YPB2014.xls
")

work.books <- read.csv(work.books.csv, as.is=TRUE, strip.white=TRUE, header=TRUE)
cat("File names with the data \n")
work.books

# read each workbook and put all of the data together into one big data frame
waterfowl.df <- plyr::ddply(work.books, "file.name", function(x){
  cat("Reading from file ", x$file.name, "\n")
  file.name <- file.path("Data",x$file.name)
  data <- readxl::read_excel(file.name, sheet="General Survey")
  data
})

# I will add some "fake data" based on the above data by randomly sampling from the above and changing the dates to 2015
# rather than 2014
set.seed(234234)
waterfowl.2015 <- waterfowl.df[ sample(1:nrow(waterfowl.df), 40),]
waterfowl.2015$Date <- as.Date( paste("2015-", format(waterfowl.2015$Date, "%m-%d")))
waterfowl.df <- rbind(waterfowl.df, waterfowl.2015)


#------------ Data Editing -----------
# fix up variable names in the data.frame.
# Variable names in R must start with a letter and contain letters or number or _. 
# Blanks in variable names are not normally allowed. Blanks will be replaced by . (period)
cat("\nOriginal variable names in data frame\n")
names(waterfowl.df)

names(waterfowl.df) <- make.names(names(waterfowl.df))

cat("\nCorrected variable names of data frame\n")
names(waterfowl.df)



# Convert dates to R date format
xtabs(~Date, data=waterfowl.df, exclude=NULL, na.action=na.pass)  # check the date formats. Make sure that all yyyy-mm-dd
waterfowl.df$Date <- as.Date(waterfowl.df$Date, "%d-%b-%y", tz="UTC")
waterfowl.df$Year <- as.numeric(format(waterfowl.df$Date, "%Y"))



# Check that the Study Area Name is the same across all years
# Look at the output from the xtabs() to see if there are multiple spellings 
# of the same Study.Area.Name.

# We will convert the Study.Area.Name to Proper Case.
waterfowl.df$Study.Area.Name <- stringr::str_to_title(waterfowl.df$Study.Area.Name)
xtabs(~Study.Area.Name,      data=waterfowl.df, exclude=NULL, na.action=na.pass)
xtabs(~Study.Area.Name+Year, data=waterfowl.df, exclude=NULL, na.action=na.pass)


# Check the Sample.Station.Labels for typos
xtabs(~Study.Area.Name+Sample.Station.Label, data=waterfowl.df, exclude=NULL, na.action=na.pass)

# When is each station measured?
xtabs(~Sample.Station.Label+Year, data=waterfowl.df, exclude=NULL, na.action=na.pass)
xtabs(~Date+Sample.Station.Label, data=waterfowl.df, exclude=NULL, na.action=na.pass)



# Check the Species code to make sure that they are all ok
# This isn't used anywhere in the analysis but is useful to know
xtabs(~Species, data=waterfowl.df, exclude=NULL, na.action=na.pass)
xtabs(~Species+Year, data=waterfowl.df, exclude=NULL, na.action=na.pass)

# Get the file prefix
file.prefix <- make.names(waterfowl.df$Study.Area.Name[1])
file.prefix <- gsub(".", '-', file.prefix, fixed=TRUE) # convert . to -
file.prefix <- file.path("Plots",file.prefix)



#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#  Analysis of the number of maximum (total) count by month.

# First aggregate the data by date over all species
count.date <- plyr::ddply(waterfowl.df, c("Study.Area.Name","Year","Date","Sample.Station.Label"), 
                          plyr::summarize,
                          total.count=sum(Count, na.rm=TRUE))

# Make a preliminary plot of total count by date

prelim.plot <- ggplot(data=count.date, aes(x=Date, y=log(total.count+.5), color=Sample.Station.Label, linetype=Sample.Station.Label))+
   ggtitle("Waterfowl (total) count data")+
   geom_point(position=position_dodge(width=.5))+
   geom_line( position=position_dodge(width=.5))+
   facet_wrap(~Study.Area.Name, ncol=1)
prelim.plot 
ggsave(plot=prelim.plot, 
       file=paste(file.prefix,'-plot-prelim.png',sep=""),
       h=6, w=6, units="in",dpi=300)


# Find the maximum count each month (Nov/Dec/Jan/Feb only)
count.date$Month  <- as.numeric(format(count.date$Date, "%m"))
count.max <- plyr::ddply(count.date, c("Study.Area.Name","Sample.Station.Label","Year","Month"), 
                         plyr::summarize, 
                         max.count=max(total.count, na.rm=TRUE))
count.max$Year.Month <- count.max$Year+((count.max$Month-.5)/12)     # this is the approximate midpoint of each month
count.max <- count.max[ count.max$Month %in% c(1:2,11:12),]  # only retain N/D/J/F data

# Make a preliminary plot of the maximum 
prelim.max.plot <- ggplot(data=count.max, aes(x=Year.Month, y=log(max.count+.5), color=Sample.Station.Label, linetype=Sample.Station.Label))+
  ggtitle("Waterfowl (max) count in month")+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks=min(count.max$Year):(1+max(count.max$Year)))+
  facet_wrap(~Study.Area.Name, ncol=1)
prelim.max.plot 
ggsave(plot=prelim.max.plot, 
       file=paste(file.prefix,'-plot-prelim-max.png',sep=""),
       h=6, w=6, units="in",dpi=300)



# This is a regression analysis with Year as the trend variable and Month as a seasonal effect.
# We need to account for the same stations being measured over time and for year specific effects (process error).
# Because of the small counts, we will try a Poisson model (allowsing for possible overdispersion
# It would be nice to fit a generalized linear mixed model, but there are convergence problems


count.max$MonthF                <- factor(count.max$Month)
count.max$Sample.Station.LabelF <- factor(count.max$Sample.Station.Label)
count.max$YearF                 <- factor(count.max$Year)

count.max.lmer <- lmerTest::lmer(log(max.count+.5) ~ Year + MonthF +(1|Sample.Station.LabelF) +(1|YearF), data=count.max)

anova(count.max.lmer, ddf="kenward-roger")
summary(count.max.lmer)
VarCorr(count.max.lmer)



# Look at the residual plot and save  to the directory
diag.plot <- sf.autoplot.lmer(count.max.lmer)  # residual and other diagnostic plots
plot(diag.plot)
ggsave(plot=diag.plot, 
       file=paste(file.prefix,"-count-residual-plot.png",sep=""),
       h=6, w=6, units="in", dpi=300)


# check for autocorreclation 
count.max$resid <- log(count.max$max.count+.5) - predict(count.max.lmer, newdata=count.max, re.form=~0)
mean.resid <- plyr::ddply(count.max, "Year", summarize, mean.resid=mean(resid))
resid.fit <- lm( mean.resid ~ 1, data=mean.resid)
dwres1 <- car::durbinWatsonTest(resid.fit)
dwres1
dwres2 <- lmtest::dwtest(resid.fit)
dwres2



# extract a table of the slopes
count.slopes <- data.frame(
       Study.Area.Name =count.max$Study.Area.Name[1],
       slope           = fixef(count.max.lmer)["Year"],
       slope.se        = summary(count.max.lmer)$coefficients["Year","Pr(>|t|)"],
       p.value         = summary(count.max.lmer)$coefficients[row.names(summary(count.max.lmer)$coefficients)=="Year"  ,"Pr(>|t|)"], 
       #r2             = summary(count.max.lmer)$r.squared,  # not defined for mixed effect models
       stringsAsFactors=FALSE)
count.slopes


# compute the fitted values from the model
# The model was run on the log(average count), so we need to back transform
count.fitted <- expand.grid(
                 Study.Area.Name=count.max$Study.Area.Name[1],
                 Year=seq(min(count.max$Year, na.rm=TRUE),1+max(count.max$Year, na.rm=TRUE), .1),
                 MonthF=factor(c(1:2,11:12)),
                 stringsAsFactors=FALSE)
count.fitted$pred.mean <- exp(predict(count.max.lmer, newdata=count.fitted,type="response", re.form=~0))
head(count.fitted)



# Plot with trend line with a separate plot for each StudyArea
count.plot.summary <- ggplot2::ggplot(data=count.max,
                                    aes(x=Year.Month, y=max.count))+
   ggtitle("Waterfowl counts ")+
   ylab("Maximum Waterfowl Count in month")+
   geom_point(size=3, aes(color=MonthF))+
   geom_line(data=count.fitted, aes(x=Year,y=pred.mean, color=MonthF))+
   facet_wrap(~Study.Area.Name, ncol=1, scales="free" )+
   scale_x_continuous(breaks=min(count.max$Year,na.rm=TRUE):max(count.max$Year,na.rm=TRUE))+
   geom_text(data=count.slopes, aes(x=min(count.max$Year, na.rm=TRUE), y=max(count.max$max.count, na.rm=TRUE)), 
             label=paste("Slope (on log scale) : ",round(count.slopes$slope,2), 
                         " ( SE "  ,round(count.slopes$slope.se,2),")",
                         " p :"    ,round(count.slopes$p.value,3)),
                         hjust="left")
count.plot.summary
ggsave(plot=count.plot.summary, 
       file=paste(file.prefix,'-count-plot-summary.png',sep=""),
       h=6, w=6, units="in", dpi=300)




