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
    
    # Attach the HYSPLIT .dmg file
    system(paste("cd ", documents_folder_path, "/SplitR/Exec", " ; ",
                 "hdiutil attach HYSPLIT_mac.dmg", sep = ''))
    
    # Get information on disks in order to get path for newly mounted disk image
    hdiutil_info <- system("hdiutil info", intern = TRUE)
    
    # Extract last line from output of hdiutil info stdout
    location <- hdiutil_info[length(hdiutil_info)]
    
    # Get device path for mounted .dmg file
    dev_path <- strsplit(location, split = "\\t", perl = TRUE)[[1]][1]
    
    # Get volume path for mounted .dmg file
    vol_path <- strsplit(location, split = "\\t", perl = TRUE)[[1]][3]
    
    # Copy executable binaries from disk image to 'SplitR/Exec' folder
    system(paste("cd '/Volumes/Disk Image/Hysplit4/exec/' ; cp * ",
                 paste(documents_folder_path, "/SplitR/Exec", sep = ''), sep = ''))
    
    # Get information on disks
    hdiutil_info <- system("hdiutil info", intern = TRUE)
    
    # Unmount and eject disk image
    system(paste("hdiutil detach /dev/disk2s9"))

  }
    
 # Close function     
}
