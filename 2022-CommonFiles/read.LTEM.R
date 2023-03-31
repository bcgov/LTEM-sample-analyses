# This file extracts the data from the LTEM
# We use separate (similar) functions for different species to allow for differential 

# Notice that for Amphibians (5572), a separate database was extracted.
# Once this code is taken over by the province, and the amphibian data is merged with the old
# data, this code will need to be modified.


PROJECT_CODE.Alpine    <- "5524"
PROJECT_CODE.Amphibians<- "5572"
PROJECT_CODE.Berries   <- "5568"
PROJECT_CODE.Grasslands<- "5570"
PROJECT_CODE.Squirrels <- "5569"
PROJECT_CODE.Waterfowl <- "5516"



####---------------------------------------------------------
# Get the species table

LTEM.species.path <- file.path("~",
                            "Dropbox","00-current-tasks",
                            "2023-03-LTEM","LTEM-sample-analyses",
                            "LTEM-data-files","LTEM_ExportFromSPI","LTEM_Species.xlsx") 

SPECIES.master <- readxl::read_excel(LTEM.species.path, sheet="Sheet1")

####---------------------------------------------------------
# Get the master list of study_area_id -> Study_area_name
LTEM.study_area.path <- file.path("~",
                            "Dropbox","00-current-tasks",
                            "2023-03-LTEM","LTEM-sample-analyses",
                            "LTEM-data-files","LTEM_ExportFromSPI","LTEM_StudyAreasAndSurveys.xlsx") 

temp <- readxl::read_excel(LTEM.study_area.path, sheet="tmpLTEM_StudyAreasAndSurveys")
STUDY_AREA.master <- unique(temp[,c("STUDY_AREA_ID","STUDY_AREA_NAME")])
SURVEY.master     <- unique(temp[,c("SURVEY_ID",    "STUDY_AREA_NAME")])
# 
# 37664 %in% SURVEY.master$SURVEY_ID
# 37667 %in% SURVEY.master$SURVEY_ID
####---------------------------------------------------------

LTEM.data.path <- file.path("~",
                            "Dropbox","00-current-tasks",
                            "2023-03-LTEM","LTEM-sample-analyses",
                            "LTEM-data-files","LTEM_ExportFromSPI","LTEM_ExportFromSPI.gdb" ) 
sf::st_layers(LTEM.data.path)

LTEM.5572.data.path <- file.path("~",
                            "Dropbox","00-current-tasks",
                            "2023-03-LTEM","LTEM-sample-analyses",
                            "LTEM-data-files","LTEM_ExportFromSPI","LTEM_5572_Amphib.gdb" ) 
sf::st_layers(LTEM.5572.data.path)



read.LTEM.data <- function(study.type,
                           site.names="**ALL**", error="STOP", .name_repair="FALSE"){
  # read the data from the LTEM surveys
  
  # we have downloaded the relevant files to a data directory that is in a *gdb database
  # this function could be modified to read directly from the web if needed
  #
  # Arguments
  #   study.type= type of study,e.g., "Berries","Waterfowl" etc.
  #     This should match the name of the data directory
  #   site.name - vector of sitenames to read.. If "ALL" then all workbooks will be read
   #   error     - what to do if an error is found?
  # Returns a list
  #      projects that match the site names and study type
  #      data frame with all sites/years stacked together
  require(sf)
  require(stringr)
  
  # do some data checking
  study.type.master <- c("Alpine","Amphibians","Berries","Grasslands","Squirrels","Waterfowl")
  
  if(length(study.type)>1)stop("Only one study type can be extracted at a time")
  if(!study.type %in% study.type.master){
     stop("Study type must be one of ", paste(study.type.master, collapse=", "), "\n")
  }
  
  # extract the project code
  project.code <- NULL
  if(study.type == "Alpine"    )project.code <- PROJECT_CODE.Alpine
  if(study.type == "Amphibians")project.code <- PROJECT_CODE.Amphibians
  if(study.type == "Berries"   )project.code <- PROJECT_CODE.Berries
  if(study.type == "Grasslands")project.code <- PROJECT_CODE.Grasslands
  if(study.type == "Squirrels" )project.code <- PROJECT_CODE.Squirrels
  if(study.type == "Waterfowl" )project.code <- PROJECT_CODE.Waterfowl
  
  LTEM.data.path <- file.path("~",
                            "Dropbox","00-current-tasks",
                            "2023-03-LTEM","LTEM-sample-analyses",
                            "LTEM-data-files","LTEM_ExportFromSPI","LTEM_ExportFromSPI.gdb" )
  LTEM.5572.data.path <- file.path("~",
                            "Dropbox","00-current-tasks",
                            "2023-03-LTEM","LTEM-sample-analyses",
                            "LTEM-data-files","LTEM_ExportFromSPI","LTEM_5572_Amphib.gdb" ) 
  
  # get the projects under the study type
  projects <- sf::st_read(LTEM.data.path, layer="SPI_SURVEYS_raw")
  projects <- projects[ projects$SPI_PROJECT_ID %in% project.code,]
  
  # attach the STUDY_AREA_NAME given the SURVEY_ID
  projects <- merge(projects, SURVEY.master, by="SURVEY_ID",all.x=TRUE)
  projects$STUDY_AREA_NAME <- ifelse(is.na(projects$STUDY_AREA_NAME), projects$SURVEY_NAME, projects$STUDY_AREA_NAME)
  projects$STUDY_AREA_NAME[ is.na(projects$STUDY_AREA_NAME)] <- "*M*I*S*S*I*N*G*"
  
  # get the projects that contain information for sites
  select <- TRUE
  if(site.names[1] != "**ALL**"){
     # only keep file names that match the patterns
     select1 <- outer(tolower(gsub(" ","",projects$SURVEY_NAME, fixed=TRUE)), 
                     stringr::fixed(tolower(gsub(" ","",site.names,fixed=TRUE))), stringr::str_detect )
     select2 <- outer(tolower(gsub(" ","",projects$STUDY_AREA_NAME, fixed=TRUE)), # see email from Province
                     stringr::fixed(tolower(gsub(" ","",site.names,fixed=TRUE))), stringr::str_detect )
     select <- apply(select1 | select2, 1, any, na.rm=TRUE)
  }
  #browser()
  projects <- projects[select,]
  #browser()

  # get the user loaded data
  if(!project.code %in% c(PROJECT_CODE.Amphibians)){
    user.data <- sf::st_read(LTEM.data.path, layer="ObservationsWithUserDefinedDataAndDesignComponentVisitData")
  }
  if( project.code %in% c(PROJECT_CODE.Amphibians)){
    user.data <- sf::st_read(LTEM.5572.data.path, layer="SurveyObs_NonSpatial")
  }
  user.data$Date <- lubridate::ymd(paste0(user.data$OBSERVATION_YEAR,  "-",
                                          user.data$OBSERVATION_MONTH, "-",
                                          user.data$OBSERVATION_DAY))
  user.data$Year <- lubridate::year(user.data$Date)

  user.data <- user.data[ user.data$SPI_PROJECT_ID %in%  project.code,] # select berries
  user.data <- user.data[ user.data$SURVEY_ID      %in%  projects$SURVEY_ID,] # select sites
  user.data <- merge(user.data, SURVEY.master, by="SURVEY_ID",all.x=TRUE) # add the study_area_name
  
  # get the species from the Taxonomic codes
  user.data <- merge(user.data, all.x=TRUE, by="TAXONOMIC_UNIT_ID",
                       SPECIES.master[, c("TAXONOMIC_UNIT_ID","CODE")])
  user.data$SPECIES_CODE<- user.data$CODE
  user.data$CODE <- NULL
  user.data$SPECIES_CODE[ is.na(user.data$SPECIES_CODE)] <- "MISSING"

  # add in the design component name
  design.components <- sf::st_read(LTEM.data.path, layer="SPI_DESIGN_COMPONENTS_raw")
    
  component.var.list <- c("DESIGN_COMPONENT_ID",
                          "DESIGN_COMPONENT_TYPE",
                          "DESIGN_COMPONENT_LABEL",
                          "PARENT_DESIGN_COMPONENT_ID")
  user.data <- merge(user.data, # add the lowest level design component
                       design.components[,component.var.list], all.x=TRUE)

  #browser()
  error.flag <- 0
  # special processing by species
  if(study.type %in% c("Amphibians")){
     # need special processing because data values are stored in different columns (A0, A1, etc) depending
     # on the version number of the general survey used.
     # For exampl, the number of eggmasses is A26 for Version 1.3 or A24 for Version 1.4
    
     # Create the fields of interest from the UDCs
     user.data[,"EggMasses"] <- NA
     user.data[,"Adults"   ] <- NA
     vn <- as.character(round(user.data$VERSION_NO,1))
     if(any(!vn %in% c("1.3", "1.4"))){
        cat("I don't know where the egg masses are stored in version numbers other than 1.3 and 1.4\n")
        print(xtabs(~vn))
        stop()
     }
     select <- vn %in% c("1.3")
     user.data$EggMasses[select] <- as.numeric(user.data$A26[select])
     #browser()
     user.data[, c("A16","A17","A18")] <- lapply(user.data[, c("A16","A17","A18")], as.numeric)
     user.data$Adults   [select] <- apply(user.data[select,c("A16","A17","A18")],1,sum, na.rm=TRUE) # add males, females, and unknown
     user.data$Adults   [select & apply(is.na(user.data[,c("A16","A17","A18")]),1,all)] <- NA # if no data for M, F, U then return NA rather than 0
       
     select <- vn %in% c("1.4")
     user.data$EggMasses[select] <- as.numeric(user.data$A24[select])
     user.data[, c("A14","A15","A16")] <- lapply(user.data[, c("A14","A15","A16")], as.numeric)
     user.data$Adults   [select] <- apply(user.data[select,c("A14","A15","A16")],1,sum, na.rm=TRUE) # add males, females, and unknown
     user.data$Adults   [select & apply(is.na(user.data[,c("A14","A15","A16")]),1,all)] <- NA # if no data for M, F, U then return NA rather than 0

     user.data$TRANSECT <- user.data$DESIGN_COMPONENT_LABEL
  }
  if(study.type %in% c("Berries")){
     # add in the parent (plant) label
     # get the relevant variables
     var.list <- c("SPI_PROJECT_ID","SURVEY_ID","DESIGN_COMPONENT_ID","Date", "UDC1","UDC2","UDC3","UDC4")

     # get the parent labels - e.g. plant id for berries
     parent.design.components <- design.components[ design.components$DESIGN_COMPONENT_ID %in% 
                                                    user.data$PARENT_DESIGN_COMPONENT_ID,
                                               c("DESIGN_COMPONENT_ID",
                                                 "DESIGN_COMPONENT_TYPE",
                                                 "DESIGN_COMPONENT_LABEL",
                                                 "PARENT_DESIGN_COMPONENT_ID")]
     names(parent.design.components) <- paste0("PARENT_", names(parent.design.components))
  
     user.data <- merge(user.data, parent.design.components, all=TRUE)
     
     # Create the fields of interest from the UDCs
     user.data[, projects$UDC1[1]] <- user.data$UDC1
     user.data[, projects$UDC2[1]] <- user.data$UDC2
     user.data[, projects$UDC3[1]] <- user.data$UDC3
     user.data[, projects$UDC4[1]] <- user.data$UDC4
     user.data[, "COMMENT"       ] <- user.data$SURVEY_OBSERVATION_NOTE
     user.data[, "STEM"]   <- user.data$DESIGN_COMPONENT_LABEL
     user.data[, "BUSH" ]  <- user.data$PARENT_DESIGN_COMPONENT_LABEL
  }
  
    if(study.type %in% c("Grasslands","Alpine")){
     # add in the parent (transect) label
     # get the relevant variables
     #var.list <- c("SPI_PROJECT_ID","SURVEY_ID","DESIGN_COMPONENT_ID","Date", "UDC1","UDC2","UDC3","UDC4")

     # get the parent labels - e.g. plant id for berries
     parent.design.components <- design.components[ design.components$DESIGN_COMPONENT_ID %in% 
                                                    user.data$PARENT_DESIGN_COMPONENT_ID,
                                               c("DESIGN_COMPONENT_ID",
                                                 "DESIGN_COMPONENT_TYPE",
                                                 "DESIGN_COMPONENT_LABEL",
                                                 "PARENT_DESIGN_COMPONENT_ID")]
     names(parent.design.components) <- paste0("PARENT_", names(parent.design.components))
  
     user.data <- merge(user.data, parent.design.components, all=TRUE)
     
     # Create the fields of interest from the UDCs
     user.data[, "PERCENT_COVER"]  <- as.numeric(user.data$A6)
     user.data[, "PLOT"]           <- user.data$DESIGN_COMPONENT_LABEL
     user.data[, "TRANSECT" ]      <- trimws(substr(user.data$PLOT, 1, -1+regexpr("P",user.data$PLOT, fixed=TRUE)))
  }
 
  # special processing for Squirrel counts
  if(study.type %in% c("Squirrels")){
    # extract the Squirrel call data
   
    # get the relevant variables
    var.list <- c("SPI_PROJECT_ID","SURVEY_ID","DESIGN_COMPONENT_ID","Date", "WLO_COUNT")
    
    # no parent label since transect is lowest level in the design
    # Create the fields of interest from the user xata
    user.data[, "COUNT"   ] <- user.data$WLO_COUNT
    user.data[, "COMMENT" ] <- user.data$SURVEY_OBSERVATION_NOTE
    user.data[, "TRANSECT"] <- user.data$DESIGN_COMPONENT_LABEL
  } 
  
    # special processing for Waterfowl counts
  if(study.type %in% c("Waterfowl")){
    # extract the Waterfowl Count
   
    # get the relevant variables
    var.list <- c("SPI_PROJECT_ID","SURVEY_ID","DESIGN_COMPONENT_ID","Date", "WLO_COUNT")
    
    # no parent label since transect is lowest level in the design
    # Create the fields of interest from the user xata
    user.data[, "COUNT"   ]     <- user.data$WLO_COUNT
    user.data[, "COMMENT" ]     <- user.data$SURVEY_OBSERVATION_NOTE
    user.data[, "SAMPLE_LABEL"] <- user.data$DESIGN_COMPONENT_LABEL
  } 
  
  if(.name_repair)names(user.data) <- make.names(names(user.data))
  #browser()
  list(study.type = study.type,
       site.names = site.names,
       error.flag = error.flag,
       projects   = projects,
       user.data  = user.data)
}

# testing Berries
#test<-         read.LTEM.data(study.type="Berries", site.names="Esk")
# test$user.data[order(test$user.data$Year, test$user.data$DESIGN_COMPONENT_LABEL)
#                     , c("Year","STUDY_AREA_NAME","DESIGN_COMPONENT_ID","DESIGN_COMPONENT_LABEL","Berry Count",
#                         "TAXONOMIC_UNIT_ID","SPECIES_CODE")]

# testing Alpine
# test<-         read.LTEM.data(study.type="Alpine", site.names="Nad")
# test$user.data[order(test$user.data$Year, test$user.data$DESIGN_COMPONENT_LABEL)
#                     , c("Year","STUDY_AREA_NAME","DESIGN_COMPONENT_ID","DESIGN_COMPONENT_LABEL",
#                         "TRANSECT","PLOT","PERCENT_COVER",
#                         "TAXONOMIC_UNIT_ID","SPECIES_CODE")]
# test$user.data$PERCENT_COVER
# 
# test<-         read.LTEM.data(study.type="Alpine")
# test$user.data[, c("Year","STUDY_AREA_NAME","PERCENT_COVER")]
# xtabs(~interaction(Year,STUDY_AREA_NAME, drop=TRUE)+is.na(PERCENT_COVER), data=test$user.data, exclude=NULL, na.action=na.pass)
# 
# xtabs(~interaction(Year,STUDY_AREA_NAME, drop=TRUE)+PLOT, data=test$user.data, exclude=NULL, na.action=na.pass)
# 
#

#testing Amphibians
# test<-         read.LTEM.data(study.type="Amphibians", site.names="Alice")
# xtabs(~Year+EggMasses, data=test$user.data, exclude=NULL, na.action=na.pass)
# xtabs(~SPECIES_CODE+EggMasses, data=test$user.data, exclude=NULL, na.action=na.pass)
# xtabs(EggMasses~SPECIES_CODE+Year, data=test$user.data)#, exclude=NULL, na.action=na.pass)
# 
# xtabs(~Year+Adults, data=test$user.data, exclude=NULL, na.action=na.pass)
# xtabs(Adults~SPECIES_CODE+Year, data=test$user.data)
# 
# test$user.data[,c("Year","STUDY_AREA_NAME","SPECIES_CODE","EggMasses")]
# 
# 
# test$user.data[ test$user.data$Year==2016,]
# 
# test<-         read.LTEM.data(study.type="Amphibians", site.names="Naz")
# xtabs(~Year+EggMasses, data=test$user.data, exclude=NULL, na.action=na.pass)
# xtabs(EggMasses~SPECIES_CODE+Year, data=test$user.data)
# 
# xtabs(~Year+Adults, data=test$user.data, exclude=NULL, na.action=na.pass)
# xtabs(Adults~SPECIES_CODE+Year, data=test$user.data, exclude=NULL, na.action=na.pass)
# 
# test$user.data[ test$user.data$Year==2015,]
# 
# 
# test<-         read.LTEM.data(study.type="Amphibians")
# xtabs(~Year+EggMasses, data=test$user.data, exclude=NULL, na.action=na.pass)
# xtabs(~SPECIES_CODE+EggMasses, data=test$user.data, exclude=NULL, na.action=na.pass)
# xtabs(EggMasses~SPECIES_CODE+Year, data=test$user.data)#, exclude=NULL, na.action=na.pass)
# 
# xtabs(~Year+Adults, data=test$user.data, exclude=NULL, na.action=na.pass)
# xtabs(Adults~SPECIES_CODE+Year, data=test$user.data)
# 
# test$user.data[,c("Year","STUDY_AREA_NAME","SPECIES_CODE","EggMasses")]
# 
# 
# test$user.data[order(test$user.data$Year, test$user.data$DESIGN_COMPONENT_LABEL)
#                     , c("Year","STUDY_AREA_NAME","DESIGN_COMPONENT_ID","DESIGN_COMPONENT_LABEL",
#                         "TRANSECT","PLOT","PERCENT_COVER",
#                         "TAXONOMIC_UNIT_ID","SPECIES_CODE")]





# # testing Grasslands
# test<-         read.LTEM.data(study.type="Grasslands", site.names="Kiko")
# test$user.data[order(test$user.data$Year, test$user.data$DESIGN_COMPONENT_LABEL)
#                     , c("Year","STUDY_AREA_NAME","DESIGN_COMPONENT_ID","DESIGN_COMPONENT_LABEL",
#                         "TRANSECT","PLOT","PERCENT_COVER",
#                         "TAXONOMIC_UNIT_ID","SPECIES_CODE")]
# test$user.data$PERCENT_COVER
# 
# test<-         read.LTEM.data(study.type="Grasslands")
# test$user.data[, c("Year","STUDY_AREA_NAME","PERCENT_COVER")]
# xtabs(~interaction(Year,STUDY_AREA_NAME, drop=TRUE)+is.na(PERCENT_COVER), data=test$user.data, exclude=NULL, na.action=na.pass)
# 
# xtabs(~interaction(Year,STUDY_AREA_NAME, drop=TRUE)+PLOT, data=test$user.data, exclude=NULL, na.action=na.pass)
# 
# 
# test$user.data[test$user.data$Year == 2015,]

# testing Squirrels
#test<-         read.LTEM.data(study.type="Squirrels", site.names=c("Bows","Pur"))
#
#test$user.data[order(test$user.data$Year, test$user.data$DESIGN_COMPONENT_LABEL)
#                    , c("Year","STUDY_AREA_NAME","DESIGN_COMPONENT_ID","DESIGN_COMPONENT_LABEL","WLO_COUNT",
#                        "TAXONOMIC_UNIT_ID","SPECIES_CODE")]

# testing Waterfows
# test<-         read.LTEM.data(study.type="Waterfowl", site.names=c("Yellow"))
# 
# test$user.data[order(test$user.data$Year, test$user.data$DESIGN_COMPONENT_LABEL)
#                     , c("Year","STUDY_AREA_NAME","DESIGN_COMPONENT_ID","DESIGN_COMPONENT_LABEL","WLO_COUNT",
#                         "TAXONOMIC_UNIT_ID","SPECIES_CODE")]
# 

#        read.LTEM.data(study.type="Waterfowl", site.names="Yellow Point", sheets='General Survey')
#test <- read.LTEM.data.old(                     sheets='General Survey')
#test <- read.LTEM.data.old(site.names="Yellow", sheets='xxx')
#test <- read.LTEM.data.old(site.names="Yellow", sheets='xxx', error="CONTINUE")





LTEM.data.path.old <- file.path("~/Dropbox/00-current-tasks/2023-03-LTEM/LTEM-sample-analyses/LTEM-data-files") 

read.LTEM.data.old <- function(study.type,
                           site.names="**ALL**", sheets=c("General Survey"), error="STOP"){
  # read the data from the LTEM surveys
  
  # we have downloaded the relevant files to a data directory
  # this function could be modified to read directly from the web if needed
  #
  # Arguments
  #   study.type= type of study,e.g., "Berries","Waterfowl" etc.
  #     This should match the name of the data directory
  #   site.name - vector of sitenames to read.. If "ALL" then all workbooks will be read
  #   sheets    - vector of sheetnames to read
  #   error     - what to do if an error is found?
  # Returns a list
  #      vector of files used to extract data
  #      data frames, one for each sheet read, with all years stacked together
  require(readxl)
  require(stringr)
  
  # do some data checking
  study.type.master <- list.files(LTEM.data.path.old)
  
  if(!study.type %in% study.type.master){
     stop("Study type must be one of ", paste(study.type.master, compress=", "), "\n")
  }
  
  # get the files under the study type
  data.dir <- file.path(LTEM.data.path.old, study.type)
  files <- list.files(data.dir)
  
  # remove any file names that start with a ~ (temporary files)
  select <- grepl("~",files, fixed=TRUE)
  files <- files[ !select]
  
  # get the file names that contain information for sites
  select <- TRUE
  if(site.names[1] != "**ALL**"){
     # only keep file names that match the patterns
     select <- outer(tolower(gsub(" ","",files, fixed=TRUE)), stringr::fixed(tolower(gsub(" ","",site.names,fixed=TRUE))), stringr::str_detect, )
  }
  files <- files[select]

  error.flag <- 0
  extract.sheets <- list()
  # extract the sheet names and flag any error
  plyr::l_ply(sheets, function (sheet){
    cat("\n\n**Extracting the following sheets ", sheet, "\n")
    all.years <- plyr::llply(files, function(file){
       cat("      From ", file, "\n")
       wb.sheets <- readxl::excel_sheets(file.path(data.dir, file))
       if(!any(tolower(wb.sheets) == tolower(sheet))){
          cat("........ERROR .... workbook does not contain sheet ", sheet, "\n")
          error.flag <- 1
          if(error=="STOP")stop()
          return(NULL)
       }
       wb.sheet <- readxl::read_excel(file.path(data.dir, file), sheet=sheet)
       # convert variable names to sentece case
       names(wb.sheet) <- stringr::str_to_sentence(names(wb.sheet))
       wb.sheet$workbook <- file
       wb.sheet
    })
    # combine all of the sheets
    all.years <- plyr::rbind.fill(all.years)
    extract.sheets[[sheet]] <<- all.years
    #browser()
  })
  #browser()
  list(study.type = study.type,
       site.names = site.names,
       sheets     = sheets,
       error.flag = error.flag,
       files = files,
       extracted.sheets=extract.sheets)
}

# testing
#        read.LTEM.data.old(study.type="Waterfowl", site.names="Yellow Point", sheets='General Survey')
#test <- read.LTEM.data.old(                     sheets='General Survey')
#test <- read.LTEM.data.old(site.names="Yellow", sheets='xxx')
#test <- read.LTEM.data.old(site.names="Yellow", sheets='xxx', error="CONTINUE")

  

