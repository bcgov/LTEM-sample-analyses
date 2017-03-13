# Illustrative example of fitting trends with a complex design

# Fixed monitoring sites are established in a monitoring site. 
# These fixed monitoring stations are repeated measured over time.
# Multiple measurements on each station are taken in each year.

set.seed(32343344)

library(ggplot2)   # for plotting
library(plyr)      # split-apply-combine paradigm
library(reshape2)  # for melting
library(lmerTest)  # for a linear mixed model with p-values
library(lsmeans)   # for comparing the trends


# Create some fake data to illustrate the fitting concepts

gen.study.data<- function(Study.Area,
                           nTransects, nMeasurements,  nYears, 
                          sdTransects,sdMeasurements, sdYears,
                          beta0, beta1){
  # Generate the random effects
  eff.Process   <- rnorm(nYears,     mean=0, sd=sdYears)
  eff.Transect  <- rnorm(nTransects, mean=0, sd=sdTransects)
  eff.Measurement<-rnorm(nYears*nTransects*nMeasurements, mean=0, sd=sdMeasurements)

  sim.data <- expand.grid(Study.Area=Study.Area, 
                          Year=1:nYears, 
                          Transect=1:nTransects, 
                          Measurement=1:nMeasurements, stringsAsFactors=FALSE)
  sim.data$Y <- beta0+ beta1*sim.data$Year  # generate the base trend
  sim.data$Y <- sim.data$Y + eff.Process [sim.data$Year]
  sim.data$Y <- sim.data$Y + eff.Transect[sim.data$Transect]
  sim.data$Y <- sim.data$Y + eff.Measurement
  sim.data
}

sdTransects    <- 4
sdMeasurements <- 2
sdYears        <- 3

beta0 <- 20
beta1 <- -0.5

sim.data.A <- gen.study.data(Study.Area='A',
                              nTransects=2,  nMeasurements=3,  nYears=10,
                             sdTransects=sdTransects, sdMeasurements=sdMeasurements, sdYears=sdYears,
                             beta0=beta0, beta1=beta1)



# Create factors
sim.data.A$TransectF <- factor(sim.data.A$Transect)
sim.data.A$YearF     <- factor(sim.data.A$Year)

# Compute the average of each transect in each year
transect.mean <- plyr::ddply(sim.data.A, c("Year","Transect"), plyr::summarize, mean.Y = mean(Y))

# Initial plot
plot.prelim <- ggplot2::ggplot(data=sim.data.A, aes(x=Year, y=Y, color=Transect, linetype=Transect))+
  ggtitle("Illustration of the various sources of variation")+
  geom_point(size=2, position=position_dodge(w=0.1))+
  geom_line(data=transect.mean, aes(y=mean.Y))+
  geom_smooth(method="lm", se=FALSE, color="blue", aes(group=1))+
  geom_point(data=transect.mean, aes(y=mean.Y), shape="X", size=5, color="black")+
  scale_x_continuous(breaks=1:10)+
  ylim(c(5,25))
plot.prelim

ggplot2::ggsave(plot=plot.prelim, 
                file="trend-demo-prelim-plot.png",
                h=4, w=6, units="in", dpi=300)

# do a naive fit
naive.fit <- lm(Y ~ Year, data=sim.data.A)
anova(naive.fit)
summary(naive.fit)

# do a fit on the averages
sim.data.avg <- plyr::ddply(sim.data.A, c("Year"), summarize, mean.Y = mean(Y))
plot.avg <- ggplot2::ggplot(data=sim.data.avg, aes(x=Year, y=mean.Y))+
  ggtitle("Illustration of analysis on average of each year's data")+
  geom_point(size=2, position=position_dodge(w=0.1))+
  geom_smooth(method="lm", se=FALSE, color="blue", aes(group=1))+
  scale_x_continuous(breaks=1:10)+
  ylim(c(5,25))
plot.avg

ggplot2::ggsave(plot=plot.avg,
                file="trend-demo-prelim-plot-avg.png",
                h=4, w=6, units="in", dpi=300)


avg.fit <- lm(mean.Y ~ Year, data=sim.data.avg)
anova(avg.fit)
summary(avg.fit)


# do a fit on the individual values accounting for random effects
sim.data.A$TransectF <- factor(sim.data.A$Transect)
sim.data.A$YearF     <- factor(sim.data.A$Year)

re.fit <- lmerTest::lmer(Y~ Year + (1|TransectF) + (1|YearF), data=sim.data.A)
anova(re.fit, dfm='kenward-roger')
summary(re.fit)
VarCorr(re.fit)


#------------------------------------------------------------
# Make a plot showing the key assumption of regression analysis:
sim.data.A$Y2 <- beta0+ beta1*sim.data.A$Year  # generate the base trend
sim.data.A$Y2 <- sim.data.A$Y2 + rnorm(nrow(sim.data.A), mean=0, sd=sqrt(sdMeasurements^2+sdTransects^2+sdMeasurements^2))


transect.mean2 <- plyr::ddply(sim.data.A, c("Year","Transect"), summarize, mean.Y2 = mean(Y2))

plot.prelim2 <- ggplot2::ggplot(data=sim.data.A, aes(x=Year, y=Y2, color=Transect, linetype=Transect))+
  ggtitle("Illustration of data that meets the regression assumptions")+
  geom_point(size=2, position=position_dodge(w=0.1))+
  geom_line(data=transect.mean2, aes(y=mean.Y2))+
  geom_smooth(method="lm", se=FALSE, color="blue", aes(group=1))+
  geom_point(data=transect.mean2, aes(y=mean.Y2), shape="X", size=5, color="black")+
  scale_x_continuous(breaks=1:10)
plot.prelim2

ggplot2::ggsave(plot=plot.prelim2, 
                file="trend-demo-prelim-plot2.png",
                h=4, w=6, units="in", dpi=300)

#--------------------------------------------------
#
# Proportional change per years

P.effect <- .75

prop.data <- data.frame(Year=1:10) 
prop.data$Response <- 100 * P.effect^(prop.data$Year-1)
prop.data$logResponse <- log(prop.data$Response)

prop.data.melt <- reshape2::melt(prop.data,
                            id.var="Year",
                            variable.name="Response",
                            value.name="value")
response.plot <- ggplot(data=prop.data.melt, aes(x=Year, y=value))+
   ggtitle("Responses with and without log() transform")+
   geom_point()+
   geom_line()+
   facet_wrap(~Response, scales="free", ncol=1)
response.plot
ggplot2::ggsave(plot=response.plot, file='plot-response-transform.png',
                h=6, w=6, units="in", dpi=300)


#------------------------------------------------
# ANCOVA

set.seed(97937344)

gen.study.data2<- function(nStudy.Areas,
                           nTransects, nMeasurements,  nYears, 
                          sdTransects,sdMeasurements, sdYears,
                          beta0, beta1){
  # Generate the random effects
  eff.Process   <- rnorm(nYears,     mean=0, sd=sdYears)
  eff.Transect  <- rnorm(nTransects*nStudy.Areas,   mean=0, sd=sdTransects)
  eff.Measurement<-rnorm(nStudy.Areas*nYears*nTransects*nMeasurements, mean=0, sd=sdMeasurements)

  sim.data <- expand.grid(Study.Area=1:nStudy.Areas, 
                          Year=1:nYears, 
                          Transect=1:nTransects, 
                          Measurement=1:nMeasurements, stringsAsFactors=FALSE)
  sim.data$Y <- beta0[sim.data$Study.Area]+ beta1[sim.data$Study.Area]*sim.data$Year  # generate the base trend
  sim.data$Y <- sim.data$Y + eff.Process [sim.data$Year]
  sim.data$Y <- sim.data$Y + eff.Transect[(sim.data$Study.Area-1)*nTransects + sim.data$Transect]
  sim.data$Y <- sim.data$Y + eff.Measurement
  sim.data
}


# Create some fake data to illustrate the fitting concepts

sim.data.B <- gen.study.data2(nStudy.Areas=2,
                              nTransects=2,  nMeasurements=3,  nYears=10,
                             sdTransects=sdTransects, sdMeasurements=sdMeasurements, sdYears=sdYears,
                             beta0=c(20,30), beta1=c(-0.5,-0.7))


# Create factors - we need to distinguish between transects with the same label in different study areas
sim.data.B$Study.Area <- c("A","B")[sim.data.B$Study.Area]
sim.data.B$TransectF <- factor(interaction(sim.data.B$Study.Area,sim.data.B$Transect))
sim.data.B$YearF     <- factor(sim.data.B$Year)


# Compute the average of each transect in each year
transect.mean <- plyr::ddply(sim.data.B, c("Study.Area","Year","YearF","Transect","TransectF"), summarize, mean.Y = mean(Y))


# Initial plot
plot.prelim.multisite <- ggplot2::ggplot(data=sim.data.B, aes(x=Year, y=Y, color=Study.Area, linetype=TransectF))+
  ggtitle("Illustration of the various sources of variation for multi-site studies")+
  geom_point(size=2, position=position_dodge(w=0.1))+
  geom_line(data=transect.mean, aes(y=mean.Y))+
  geom_smooth(method="lm", se=FALSE, color="blue", aes(group=Study.Area))+
  geom_point(data=transect.mean, aes(y=mean.Y), shape="X", size=5, color="black")+
  scale_x_continuous(breaks=1:10)
  #ylim(c(5,25))
plot.prelim.multisite

ggplot2::ggsave(plot=plot.prelim.multisite, 
                file="trend-demo-prelim-plot-multisite.png",
                h=4, w=6, units="in", dpi=300)


# do a fit on the averages
sim.data.avg <- plyr::ddply(sim.data.B, c("Study.Area","Year"), summarize, mean.Y = mean(Y))
plot.avg.multisite <- ggplot2::ggplot(data=sim.data.avg, aes(x=Year, y=mean.Y,color=Study.Area))+
  ggtitle("Illustration of multisite analysis on average of each year's data")+
  geom_point(size=2, position=position_dodge(w=0.1))+
  geom_smooth(method="lm", se=FALSE, aes(group=Study.Area))+
  scale_x_continuous(breaks=1:10)
plot.avg.multisite

ggplot2::ggsave(plot=plot.avg.multisite,
                file="trend-demo-prelim-plot-multisite-avg.png",
                h=4, w=6, units="in", dpi=300)

sim.data.avg$Study.AreaF <- factor(sim.data.avg$Study.Area)
sim.data.avg$YearF       <- factor(sim.data.avg$Year)

avg.fit <- lmerTest::lmer(mean.Y ~ Year + Study.AreaF+ Year:Study.AreaF + (1|YearF),
                          data=sim.data.avg)
anova(avg.fit, ddf='kenward-roger')
summary(avg.fit)
VarCorr(avg.fit)

# Get the individual slopes and the cld
avg.fit.lsmo <- lsmeans::lstrends(avg.fit, "Study.AreaF", var="Year")
lsmeans::cld(avg.fit.lsmo)




# do a fit on the individual values accounting for random effects
sim.data.B$Study.AreaF <- factor(sim.data.B$Study.Area)
sim.data.B$TransectF   <- factor(interaction(sim.data.B$Study.Area,sim.data.B$Transect))
sim.data.B$YearF       <- factor(sim.data.B$Year)

re.fit <- lmerTest::lmer(Y~ Study.AreaF + Year + Year:Study.AreaF + 
                           (1|TransectF) + (1|YearF) + (1|YearF:Study.AreaF), data=sim.data.B)
anova(re.fit, dfm='kenward-roger')
summary(re.fit)
VarCorr(re.fit)

re.fit.lsmo <- lsmeans::lstrends(avg.fit, "Study.AreaF", var="Year")
lsmeans::cld(re.fit.lsmo)


#---------------------------------------
# Generate the prototype diagrams for ANCOVA

slopes.csv <- textConnection(
"Model, Study.Area, intercept, slope
Parallel, A, 30, -.5
Parallel, B, 20, -.5
Non-parallel, A, 30, 1
Non-parallel, B, 35, -1")

slopes <- read.csv(slopes.csv, header=TRUE, as.is=TRUE, strip.white=TRUE)
slopes

fake.data <- plyr::ddply(slopes, c("Study.Area","Model"), function(x){
   fake <- data.frame(Years=1:10)
   fake$Y <- x$intercept + x$slope*fake$Years + rnorm(10,sd=2)
   fake
})

plot.proto <- ggplot(data=fake.data, aes(x=Years, y=Y, color=Study.Area))+
   ggtitle("Prototype models for ANCOVA")+
   geom_point()+
   geom_smooth(method="lm", se=FALSE)+
   xlim(c(1,10))+
   #scale_x_continuous(breaks=1:10)+
   facet_wrap(~Model, ncol=1)
plot.proto

ggplot2::ggsave(plot=plot.proto, 
                file="trend-demo-proto-plot-multisite.png",
                h=4, w=6, units="in", dpi=300)


"
)
