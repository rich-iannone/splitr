# Determine the operating system in use
get_os <- function() {
  if (.Platform$OS.type == "windows") { 
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac" 
  } else if (.Platform$OS.type == "unix") { 
    "unix"
  } else {
    stop("Unknown OS")
  }
}

# Determine whether a 64-bit architecture is present 
is_64bit_system <- function(){
  ifelse(.Machine$sizeof.pointer == 8, TRUE, FALSE)
}
