# This script will run ALL of the sites in the amphibian protocol


library(car)       # for testing for autocorrelation (2 libraries needed - see dwtest)
library(flextable) # for tables that look nice
library(ggfortify) # for residual and other diagnostic plot
library(ggplot2)   # for plotting
library(insight)   # for formatting p-values
library(lmtest)    # for testing for autocorrelation
library(lubridate) # date conversions
library(plyr)      # for group processing
library(readxl)    # for opening the Excel spreadsheets and reading off them
library(quarto)
library(lmerTest)  # for the linear mixed modelling
library(sf)        # for extracting the LTEM data
library(stringr)   # string handling (like case conversion)

# Load some common functions
source("../2022-CommonFiles/common.functions.R")
source("../2022-CommonFiles/read.LTEM.R")


# We need to get ALL of the raw data to identify which study areas have squirrel measurements
all.data <- read.LTEM.data(study.type="Amphibians")
xtabs(~SPECIES_CODE, data=all.data$user.data, exclude=NULL, na.action=na.pass)
all.data$user.data[ all.data$user.data$SPECIES_CODE=="NULL",]

xtabs(~STUDY_AREA_NAME, data=all.data$user.data, exclude=NULL, na.action=na.pass)

xtabs(~STUDY_AREA_NAME+STUDY_AREA_ID, data=all.data$user.data, exclude=NULL, na.action=na.pass)

# get the project records with the 
# get the projects under the study type
projects <- sf::st_read(LTEM.data.path, layer="SPI_SURVEYS_raw")
projects <- projects[ projects$SPI_PROJECT_ID %in% PROJECT_CODE.Amphibians,]
  
# attach the STUDY_AREA_NAME from the BCGW data
projects <- merge(projects, SURVEY.master, by="SURVEY_ID",all.x=TRUE)
projects$STUDY_AREA_NAME[ is.na(projects$STUDY_AREA_NAME)] <- "*M*I*S*S*I*N*G*"

projects <- projects[ projects$SURVEY_ID %in% all.data$user.data$SURVEY_ID,]

# create directory for the individual reports
if(!file.exists("IndivReports"))dir.create("IndivReports")

# Now to loop over the study areas
output_format="html"

projects <- projects[ projects$STUDY_AREA_NAME != "*M*I*S*S*I*N*G*",]

plyr::l_ply(sort(unique(projects$"STUDY_AREA_NAME"))[], function(x){
  cat("\n\n*******************************************************************************\n")
  cat("*******************************************************************************\n")
  cat("*******************************************************************************\n")
  cat("Starting rendering for ",x, "\n")
  #browser()
  unlink("Plots", recursive=TRUE)
  if(!file.exists("Plots"))       dir.create("Plots")
  
  report.dir <- file.path("IndivReports",fs::path_sanitize(x))
  if(!file.exists(report.dir))dir.create(report.dir)
     
  out_file <- file.path(#"IndivReports",
                         fs::path_sanitize(paste0("IndivReport-", x,".",output_format))
                       )

  params = list(STUDY_AREA_NAME=x)  
  # render the report. Must render it in the same directory as the *.qmd and then move
  # otherwise cannot create the self-contained report.
  quarto::quarto_render("amp-single-site.qmd",
                      output_file = out_file,
                      output_format=output_format,
                      execute_params = params,
                      )
  file.rename(out_file, # move to the directory
              file.path(report.dir, out_file))
  if(file.exists(file.path(report.dir,"IndivReport-Plots")))unlink(file.path(report.dir,"IndivReport-Plots"), recursive=TRUE)
  file.rename("Plots","IndivReport-Plots")
  file.copy("IndivReport-Plots",
              file.path(report.dir),overwrite=TRUE, recursive=TRUE)
  unlink("IndivReport-Plots", recursive=TRUE)
  })
  
  