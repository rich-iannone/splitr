#' Get all of the necessary HYSPLIT binaries
#'
#' @export
splitr_setup <- function() {
  
  # Get the system OS
  system_os <- get_os()
  
  # Get the names of the files to be downloaded
  binary_files <- c("hyts_std", "hycs_std", "parhplot")
  
  # Slightly modify the file names for Windows
  if (system_os == "win") {
    binary_files <- paste0(binary_files, ".exe")
  }
  
  # Get the exact resource names according to the OS
  resource_names <- get_resource_names_by_system_os(system_os = system_os)
  
  # Ask if we should get the binary files
  should_get_bin_files <- 
    usethis::ui_yeah(
      paste0(
        "Let's set up `splitr` so that all the necessary model\n",
        "binaries are in an accessible location.\n\n",
        "Three binary files will be fetched and put into the\n",
        "current working directory.\n",
        "\n",
        "These files are: ",
        usethis::ui_path(binary_files[1]), ", ",
        usethis::ui_path(binary_files[2]), ", and ",
        usethis::ui_path(binary_files[3]), ".\n",
        "\n",
        "Put these binary files somewhere safe and accessible.\n",
        "We can specify a ", usethis::ui_field("binary_path"), " ",
        "that points to that\n",
        "directory when running models.\n",
        "\n",
        "Is it okay to get these files now?"
      )
    )
  
  # If we have the go ahead to fetch, do it; otherwise
  # state that nothing happened
  if (should_get_bin_files) {
    
    fetch_resources(
      resources = splitr_resources,
      names = resource_names
    )
    
    usethis::ui_done("The files were downloaded!")
    usethis::ui_todo("Move the files accordingly and record their location.")
    
  } else {
    
    usethis::ui_field("Okay, nothing was done.")
  }
}

