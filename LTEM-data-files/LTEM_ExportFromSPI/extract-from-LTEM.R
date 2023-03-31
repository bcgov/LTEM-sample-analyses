# Testing various ways to extract data


# https://r-spatial.github.io/sf/articles/sf2.html
library(sf)
LTEM.data.path <- file.path("~",
                            "Dropbox","00-current-tasks",
                            "2023-03-LTEM","LTEM-sample-analyses",
                            "LTEM-data-files","LTEM_ExportFromSPI","LTEM_ExportFromSPI.gdb" ) 

# get the layers
sf::st_layers(LTEM.data.path)

# get a particular layer
temp <- sf::st_read(LTEM.data.path, layer="UserDefinedColumnAndCodeDefinitions")
temp

temp <- sf::st_read(LTEM.data.path, layer="SPI_SURVEYS_AnotherDisplayOfUserDefinedColumnNames")
temp[1:10,]
unique(temp$PROMPT)

temp <- sf::st_read(LTEM.data.path, layer="SPI_DESIGN_COMPONENTS_raw")
temp[1:10,]
 
temp <- sf::st_read(LTEM.data.path, layer="SPI_DESIGN_COMPONENT_VISITS_raw")
temp[1:10,]

# get the Project numbers etc
projects <- sf::st_read(LTEM.data.path, layer="SPI_SURVEYS_raw")
head(projects)
xtabs(~SPI_PROJECT_ID, data=projects, exclude=NULL, na.action=na.pass)


###-----------------------
alp.project <- '5524'
alp.project <- "Nadi"
alp.project <- '5570'
alp.site    <- 'Kiko'
alp.projects <- projects[ projects$SPI_PROJECT_ID == alp.project,]

alp.projects.Nadine <- alp.projects
alp.projects.Nadine <- alp.projects[ grepl(tolower(alp.site), 
                                               tolower(alp.projects$SURVEY_NAME), fixed=TRUE),]
alp.projects.Nadine

# Get the user defined fields
user.data <- sf::st_read(LTEM.data.path, layer="ObservationsWithUserDefinedDataAndDesignComponentVisitData")
user.data$Date <- lubridate::ymd(paste0(user.data$OBSERVATION_YEAR,  "-",
                                        user.data$OBSERVATION_MONTH, "-",
                                        user.data$OBSERVATION_DAY))
user.data$Year <- lubridate::year(user.data$Date)

alp.user.data <- user.data[ user.data$SPI_PROJECT_ID == alp.project,]
alp.user.data.Nadine <- user.data
alp.user.data.Nadine <- user.data[ user.data$SURVEY_ID %in% alp.projects.Nadine$SURVEY_ID,]

alp.user.data.Nadine[1:2,]
alp.user.data.Nadine[,c("Year","WLO_COUNT","A2","A26","SURVEY_OBSERVATION_NOTE")]
alp.user.data.Nadine$SURVEY_OBSERVATION_NOTE
alp.user.data.Nadine$SURVEY_OBSERVATION_ID

var.list <- c("SPI_PROJECT_ID","SURVEY_ID","DESIGN_COMPONENT_ID","Date", "UDC1","UDC2","UDC3","UDC4")
var.list %in% names(alp.user.data.Nadine)

alp.user.data.Nadine[1:10, var.list]

# add in the design component name
design.components <- sf::st_read(LTEM.data.path, layer="SPI_DESIGN_COMPONENTS_raw")

component.var.list <- c("DESIGN_COMPONENT_ID",
                        "DESIGN_COMPONENT_TYPE",
                        "DESIGN_COMPONENT_LABEL",
                        "PARENT_DESIGN_COMPONENT_ID")
component.var.list %in% names(design.components)

alp.user.data.Nadine <- merge(alp.user.data.Nadine,
                                design.components[,component.var.list], all.x=TRUE)
alp.user.data.Nadine[ alp.user.data.Nadine$Year==2015, c(var.list, component.var.list)]
alp.user.data.Nadine[alp.user.data.Nadine$Year==2014 & alp.user.data.Nadine$DESIGN_COMPONENT_LABEL=="T1P1",]

# get the parent labels
parent.design.components <- design.components[ design.components$DESIGN_COMPONENT_ID %in% 
                                                 alp.user.data.Nadine$PARENT_DESIGN_COMPONENT_ID,
                                               c("DESIGN_COMPONENT_ID",
                                                 "DESIGN_COMPONENT_TYPE",
                                                 "DESIGN_COMPONENT_LABEL",
                                                 "PARENT_DESIGN_COMPONENT_ID")]
names(parent.design.components) <- paste0("PARENT_", names(parent.design.components))
names(parent.design.components)
parent.design.components
names(alp.user.data.Nadine) %in% names(parent.design.components)


alp.user.data.Nadine <- merge(alp.user.data.Nadine,
                                parent.design.components, all=TRUE)

head(alp.user.data.Nadine)
head(parent.design.components)
alp.user.data.Nadine[ alp.user.data.Nadine$Year==2015, c(var.list, component.var.list, names(parent.design.components))]

xtabs(~PARENT_DESIGN_COMPONENT_LABEL+Year, data=alp.user.data.Nadine, exclude=NULL, na.action=na.pass)
xtabs(~DESIGN_COMPONENT_LABEL+Year, data=alp.user.data.Nadine, exclude=NULL, na.action=na.pass)

alp.user.data.Nadine[order(alp.user.data.Nadine$Year, alp.user.data.Nadine$DESIGN_COMPONENT_LABEL)
  , c("Year","DESIGN_COMPONENT_ID","DESIGN_COMPONENT_LABEL","WLO_COUNT","TAXONOMIC_UNIT_ID")]

alp.user.data.Nadine[alp.user.data.Nadine$Year==2014 & alp.user.data.Nadine$DESIGN_COMPONENT_LABEL=="T1P1",]
alp.user.data.Nadine[ alp.user.data.Nadine$DESIGN_COMPONENT_LABEL=="T1 P1",]
alp.user.data.Nadine[ alp.user.data.Nadine$DESIGN_COMPONENT_LABEL%in% c("T1 P1","TR1 P1"),]$A6

alp.user.data.Nadine$A6

###-----------------------
amp.project <- '5572'
amp.site    <- 'Alice'
amp.site    <- 'Big Bar'
# EG. 5572 = amphibian Counts
amp.projects <- projects[ projects$SPI_PROJECT_ID == amp.project,]

amp.projects.Alice <- amp.projects[ grepl(tolower(amp.site), 
                                               tolower(amp.projects$SURVEY_NAME), fixed=TRUE),]
amp.projects.Alice

# Get the user defined fields
user.data <- sf::st_read(LTEM.data.path, layer="ObservationsWithUserDefinedDataAndDesignComponentVisitData")
user.data$Date <- lubridate::ymd(paste0(user.data$OBSERVATION_YEAR,  "-",
                                        user.data$OBSERVATION_MONTH, "-",
                                        user.data$OBSERVATION_DAY))
user.data$Year <- lubridate::year(user.data$Date)

amp.user.data <- user.data[ user.data$SPI_PROJECT_ID == amp.project,]

# Egg mass field is A26 for Version 1.3 or A24 for Version 1.4 depending on version number - sigh
xtabs(~A24+VERSION_NO, data=amp.user.data, exclude=NULL, na.action=na.pass)
xtabs(~A26+VERSION_NO, data=amp.user.data, exclude=NULL, na.action=na.pass)
amp.user.data

amp.user.data.Alice <- user.data[ user.data$SURVEY_ID %in% amp.projects.Alice$SURVEY_ID,]



amp.user.data.Alice[1:2,]
amp.user.data.Alice[,c("Year","WLO_COUNT","A2","A26","SURVEY_OBSERVATION_NOTE")]
amp.user.data.Alice$SURVEY_OBSERVATION_NOTE
amp.user.data.Alice$SURVEY_OBSERVATION_ID

var.list <- c("SPI_PROJECT_ID","SURVEY_ID","DESIGN_COMPONENT_ID","Date", "UDC1","UDC2","UDC3","UDC4")
var.list %in% names(amp.user.data.Alice)

amp.user.data.Alice[1:10, var.list]

# add in the design component name
design.components <- sf::st_read(LTEM.data.path, layer="SPI_DESIGN_COMPONENTS_raw")

component.var.list <- c("DESIGN_COMPONENT_ID",
                        "DESIGN_COMPONENT_TYPE",
                        "DESIGN_COMPONENT_LABEL",
                        "PARENT_DESIGN_COMPONENT_ID")
component.var.list %in% names(design.components)

amp.user.data.Alice <- merge(amp.user.data.Alice,
                                design.components[,component.var.list], all.x=TRUE)
amp.user.data.Alice[ amp.user.data.Alice$Year==2015, c(var.list, component.var.list)]

# get the parent labels
parent.design.components <- design.components[ design.components$DESIGN_COMPONENT_ID %in% 
                                                 amp.user.data.Alice$PARENT_DESIGN_COMPONENT_ID,
                                               c("DESIGN_COMPONENT_ID",
                                                 "DESIGN_COMPONENT_TYPE",
                                                 "DESIGN_COMPONENT_LABEL",
                                                 "PARENT_DESIGN_COMPONENT_ID")]
names(parent.design.components) <- paste0("PARENT_", names(parent.design.components))
names(parent.design.components)
parent.design.components
names(amp.user.data.Alice) %in% names(parent.design.components)


amp.user.data.Alice <- merge(amp.user.data.Alice,
                                parent.design.components, all=TRUE)

head(amp.user.data.Alice)
head(parent.design.components)
amp.user.data.Alice[ amp.user.data.Alice$Year==2015, c(var.list, component.var.list, names(parent.design.components))]

xtabs(~PARENT_DESIGN_COMPONENT_LABEL+Year, data=amp.user.data.Alice, exclude=NULL, na.action=na.pass)
xtabs(~DESIGN_COMPONENT_LABEL+Year, data=amp.user.data.Alice, exclude=NULL, na.action=na.pass)

amp.user.data.Alice[order(amp.user.data.Alice$Year, amp.user.data.Alice$DESIGN_COMPONENT_LABEL)
  , c("Year","DESIGN_COMPONENT_ID","DESIGN_COMPONENT_LABEL","WLO_COUNT","TAXONOMIC_UNIT_ID")]


###-----------------------
wf.project <- '5516'
wf.site    <- 'Yellow'
# EG. 5516 = waterfowl Counts
wf.projects <- projects[ projects$SPI_PROJECT_ID == wf.project,]

wf.projects.Yellow <- wf.projects[ grepl(tolower(wf.site), 
                                               tolower(wf.projects$SURVEY_NAME), fixed=TRUE),]
wf.projects.Yellow

# Get the user defined fields
user.data <- sf::st_read(LTEM.data.path, layer="ObservationsWithUserDefinedDataAndDesignComponentVisitData")
user.data$Date <- lubridate::ymd(paste0(user.data$OBSERVATION_YEAR,  "-",
                                        user.data$OBSERVATION_MONTH, "-",
                                        user.data$OBSERVATION_DAY))
user.data$Year <- lubridate::year(user.data$Date)

wf.user.data <- user.data[ user.data$SPI_PROJECT_ID == wf.project,]
wf.user.data.Yellow <- user.data[ user.data$SURVEY_ID %in% wf.projects.Yellow$SURVEY_ID,]

var.list <- c("SPI_PROJECT_ID","SURVEY_ID","DESIGN_COMPONENT_ID","Date", "UDC1","UDC2","UDC3","UDC4")
var.list %in% names(wf.user.data.Yellow)

# add in the design component name
design.components <- sf::st_read(LTEM.data.path, layer="SPI_DESIGN_COMPONENTS_raw")

component.var.list <- c("DESIGN_COMPONENT_ID",
                        "DESIGN_COMPONENT_TYPE",
                        "DESIGN_COMPONENT_LABEL",
                        "PARENT_DESIGN_COMPONENT_ID")
component.var.list %in% names(design.components)

wf.user.data.Yellow <- merge(wf.user.data.Yellow,
                                design.components[,component.var.list], all.x=TRUE)
wf.user.data.Yellow[ wf.user.data.Yellow$Year==2017, c(var.list, component.var.list)]

# get the parent labels
parent.design.components <- design.components[ design.components$DESIGN_COMPONENT_ID %in% 
                                                 wf.user.data.Yellow$PARENT_DESIGN_COMPONENT_ID,
                                               c("DESIGN_COMPONENT_ID",
                                                 "DESIGN_COMPONENT_TYPE",
                                                 "DESIGN_COMPONENT_LABEL",
                                                 "PARENT_DESIGN_COMPONENT_ID")]
names(parent.design.components) <- paste0("PARENT_", names(parent.design.components))
names(parent.design.components)
parent.design.components
names(wf.user.data.Yellow) %in% names(parent.design.components)


wf.user.data.Yellow <- merge(wf.user.data.Yellow,
                                parent.design.components, all=TRUE)

head(wf.user.data.Yellow)
head(parent.design.components)
wf.user.data.Yellow[ wf.user.data.Yellow$Year==2015, c(var.list, component.var.list, names(parent.design.components))]

xtabs(~PARENT_DESIGN_COMPONENT_LABEL+Year, data=wf.user.data.Yellow, exclude=NULL, na.action=na.pass)
xtabs(~DESIGN_COMPONENT_LABEL+Year, data=wf.user.data.Yellow, exclude=NULL, na.action=na.pass)

wf.user.data.Yellow[order(wf.user.data.Yellow$Year, wf.user.data.Yellow$DESIGN_COMPONENT_LABEL)
  , c("Year","DESIGN_COMPONENT_ID","DESIGN_COMPONENT_LABEL","WLO_COUNT","TAXONOMIC_UNIT_ID")]




###-----------------------
sq.project <- '5569'
sq.site    <- 'Bowser'
# EG. 5568 = sq Counts
sq.projects <- projects[ projects$SPI_PROJECT_ID == sq.project,]

sq.projects.bowser <- sq.projects[ grepl(tolower(sq.site), 
                                               tolower(sq.projects$SURVEY_NAME), fixed=TRUE),]
sq.projects.bowser

# Get the user defined fields
user.data <- sf::st_read(LTEM.data.path, layer="ObservationsWithUserDefinedDataAndDesignComponentVisitData")
user.data$Date <- lubridate::ymd(paste0(user.data$OBSERVATION_YEAR,  "-",
                                        user.data$OBSERVATION_MONTH, "-",
                                        user.data$OBSERVATION_DAY))
user.data$Year <- lubridate::year(user.data$Date)

sq.user.data <- user.data[ user.data$SPI_PROJECT_ID == sq.project,]
sq.user.data.bowser <- user.data[ user.data$SURVEY_ID %in% sq.projects.bowser$SURVEY_ID,]

var.list <- c("SPI_PROJECT_ID","SURVEY_ID","DESIGN_COMPONENT_ID","Date", "UDC1","UDC2","UDC3","UDC4")
var.list %in% names(sq.user.data.bowser)

# add in the design component name
design.components <- sf::st_read(LTEM.data.path, layer="SPI_DESIGN_COMPONENTS_raw")

component.var.list <- c("DESIGN_COMPONENT_ID",
                        "DESIGN_COMPONENT_TYPE",
                        "DESIGN_COMPONENT_LABEL",
                        "PARENT_DESIGN_COMPONENT_ID")
component.var.list %in% names(design.components)

sq.user.data.bowser <- merge(sq.user.data.bowser,
                                design.components[,component.var.list], all.x=TRUE)
sq.user.data.bowser[ sq.user.data.bowser$Year==2017, c(var.list, component.var.list)]

# get the parent labels
parent.design.components <- design.components[ design.components$DESIGN_COMPONENT_ID %in% 
                                                 sq.user.data.bowser$PARENT_DESIGN_COMPONENT_ID,
                                               c("DESIGN_COMPONENT_ID",
                                                 "DESIGN_COMPONENT_TYPE",
                                                 "DESIGN_COMPONENT_LABEL",
                                                 "PARENT_DESIGN_COMPONENT_ID")]
names(parent.design.components) <- paste0("PARENT_", names(parent.design.components))
names(parent.design.components)
parent.design.components
names(sq.user.data.bowser) %in% names(parent.design.components)


sq.user.data.bowser <- merge(sq.user.data.bowser,
                                parent.design.components, all=TRUE)

head(sq.user.data.bowser)
head(parent.design.components)
sq.user.data.bowser[ sq.user.data.bowser$Year==2015, c(var.list, component.var.list, names(parent.design.components))]

xtabs(~PARENT_DESIGN_COMPONENT_LABEL+Year, data=sq.user.data.bowser, exclude=NULL, na.action=na.pass)
xtabs(~DESIGN_COMPONENT_LABEL+Year, data=sq.user.data.bowser, exclude=NULL, na.action=na.pass)

sq.user.data.bowser[order(sq.user.data.bowser$Year, sq.user.data.bowser$DESIGN_COMPONENT_LABEL)
  , c("Year","DESIGN_COMPONENT_ID","DESIGN_COMPONENT_LABEL","WLO_COUNT","TAXONOMIC_UNIT_ID")]



###-----------------------
berry.project <- '5568'
berry.site    <- 'Eskers'
# EG. 5568 = Berry Counts
berry.projects <- projects[ projects$SPI_PROJECT_ID == berry.project,]
berry.projects.eskers <- berry.projects[ grepl(tolower(berry.site), 
                                               tolower(berry.projects$SURVEY_NAME), fixed=TRUE),]
berry.projects.eskers

# Get the user defined fields
user.data <- sf::st_read(LTEM.data.path, layer="ObservationsWithUserDefinedDataAndDesignComponentVisitData")
user.data$Date <- lubridate::ymd(paste0(user.data$OBSERVATION_YEAR,  "-",
                                        user.data$OBSERVATION_MONTH, "-",
                                        user.data$OBSERVATION_DAY))
user.data$Year <- lubridate::year(user.data$Date)

berry.user.data <- user.data[ user.data$SPI_PROJECT_ID == berry.project,]
berry.user.data.eskers <- user.data[ user.data$SURVEY_ID %in% berry.projects.eskers$SURVEY_ID,]

var.list <- c("SPI_PROJECT_ID","SURVEY_ID","DESIGN_COMPONENT_ID","Date", "UDC1","UDC2","UDC3","UDC4")
var.list %in% names(berry.user.data.eskers)

# add in the design component name
design.components <- sf::st_read(LTEM.data.path, layer="SPI_DESIGN_COMPONENTS_raw")

component.var.list <- c("DESIGN_COMPONENT_ID",
                        "DESIGN_COMPONENT_TYPE",
                        "DESIGN_COMPONENT_LABEL",
                        "PARENT_DESIGN_COMPONENT_ID")
component.var.list %in% names(design.components)

berry.user.data.eskers <- merge(berry.user.data.eskers,
                                design.components[,component.var.list], all.x=TRUE)
berry.user.data.eskers[ berry.user.data.eskers$Year==2017, c(var.list, component.var.list)]

# get the parent labels
parent.design.components <- design.components[ design.components$DESIGN_COMPONENT_ID %in% 
                                                 berry.user.data.eskers$PARENT_DESIGN_COMPONENT_ID,
                                               c("DESIGN_COMPONENT_ID",
                                                 "DESIGN_COMPONENT_TYPE",
                                                 "DESIGN_COMPONENT_LABEL",
                                                 "PARENT_DESIGN_COMPONENT_ID")]
names(parent.design.components) <- paste0("PARENT_", names(parent.design.components))
names(parent.design.components)
parent.design.components
names(berry.user.data.eskers) %in% names(parent.design.components)


berry.user.data.eskers <- merge(berry.user.data.eskers,
                                parent.design.components, all=TRUE)

head(berry.user.data.eskers)
head(parent.design.components)
berry.user.data.eskers[ berry.user.data.eskers$Year==2015, c(var.list, component.var.list, names(parent.design.components))]

xtabs(~PARENT_DESIGN_COMPONENT_LABEL+Year, data=berry.user.data.eskers, exclude=NULL, na.action=na.pass)
xtabs(~DESIGN_COMPONENT_LABEL+Year, data=berry.user.data.eskers, exclude=NULL, na.action=na.pass)

