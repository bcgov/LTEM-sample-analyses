# These file read the excel workbooks for the TLEM
# We use separate (similar) functions for different species to allow for differential 

LTEM.data.path <- file.path("~cschwarz/Dropbox/00-current-tasks/2023-03-LTEM/LTEM-sample-analyses/LTEM-data-files") 
  
read.LTEM.data <- function(study.type,
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
  study.type.master <- list.files(LTEM.data.path)
  
  if(!study.type %in% study.type.master){
     stop("Study type must be one of ", paste(study.type.master, compress=", "), "\n")
  }
  
  # get the files under the study type
  data.dir <- file.path(LTEM.data.path, study.type)
  files <- list.files(data.dir)
  
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
#read.LTEM.data(study.type="Waterfowl", site.names="Yellow Point", sheets='General Survey')
#test <- read.LTEM.data(                     sheets='General Survey')
#test <- read.LTEM.data(site.names="Yellow", sheets='xxx')
#test <- read.LTEM.data(site.names="Yellow", sheets='xxx', error="CONTINUE")

  

