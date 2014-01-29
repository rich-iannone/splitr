SplitR.get.exec <- function(){

  # Check for existence of Documents folder in user home folder
  if(length(grep("/Documents$",
                 list.dirs(path = "~", full.names = TRUE,
                           recursive = FALSE))) > 0) {
    documents_folder_path <- paste("~", "/Documents", sep = '')
  }
  
  # Check for existence of SplitR folder
  SplitR_dir_exists <- 
    ifelse(length(list.dirs(path = paste(documents_folder_path,
                                         "/SplitR", sep = ''),
                            full.names = TRUE, recursive = FALSE)) > 0, TRUE, FALSE)
  
  # If the SplitR folder exists, return its path
  if (SplitR_dir_exists == TRUE) {
    SplitR_path <- file.path(paste(documents_folder_path, "/SplitR", sep = ''))
  
    # Download the HYSPLIT Mac .dmg file, place it in the Exec folder
    download.file(url = "http://ready.arl.noaa.gov/data/web/models/hysplit4/applex/HYSPLIT_mac.dmg",
                  destfile = paste(documents_folder_path,
                                   "/SplitR/Exec",
                                   "/HYSPLIT_mac.dmg",
                                   sep = ''),
                  method = "auto",
                  mode = "w")
    system(paste("cd ", documents_folder_path, "/SplitR/Exec", " ; ",
                 "hdiutil attach HYSPLIT_mac.dmg", sep = ''))
    system(paste("cd '/Volumes/Disk Image/Hysplit4/exec/' ; cp * ",
                 paste(documents_folder_path, "/SplitR/Exec", sep = ''), sep = ''))
    
  }
  

                
                
}

