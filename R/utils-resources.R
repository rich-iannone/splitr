github_resources <- function(repo,
                             branch = "master",
                             ...) {
  
  names_paths <- list(...)
  
  names_paths <- 
    lapply(names_paths, function(x) {
      class(x) <- "github_resource"
      attr(x, "repo") <- repo
      attr(x, "branch") <- branch
      x
    })
  
  names_paths
}

web_resources <- function(...) {
  
  names_paths <- list(...)
  
  names_paths <- 
    lapply(names_paths, function(x) {
      class(x) <- "web_resource"
      x
    })
  
  names_paths
}

register_resources <- function(resource_list) {
  
  if (length(resource_list) == 0) {
    stop("The resource list must be populated with resources", call. = FALSE)
  }
  
  # Unlist by a single level if we have a list of lists where
  # the first accessible element is of a recognized class
  if (any(vapply(resource_list, is.list, FUN.VALUE = logical(1))) &&
      (inherits(resource_list[[1]][[1]], "github_resource") |
       inherits(resource_list[[1]][[1]], "web_resource"))) {
    
    resource_list <- unlist(resource_list, recursive = FALSE)
  }
  
  # Stop function if any classes are not recognized
  lapply(
    resource_list, function(x) {
      if (!(inherits(x, "github_resource") | inherits(x, "web_resource"))) {
        stop("There is an unrecognized resource present", call. = FALSE)
      }
    }
  )
  
  # TODO: Stop function if the list isn't completely named
  
  # TODO: Stop function if there are any duplicate names
  
  # Determine which elements are a `github_resource`
  github_resources <-
    vapply(
      resource_list, function(x) inherits(x, "github_resource"),
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE
    )
  
  # Determine which elements are a `web_resource`
  web_resources <-
    vapply(
      resource_list, function(x) inherits(x, "web_resource"),
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE
    )
  
  # Obtain URIs for a `github_resource`s
  resource_list[github_resources] <-
    lapply(
      resource_list[github_resources],
      function(x) {
        
        repo <- attr(x, "repo", exact = TRUE)
        branch <- attr(x, "branch", exact = TRUE)
        path <- as.character(x)
        
        x[1] <- gh_path_resolver(path = path, repo = repo, branch = branch)
        x
      }
    )
  
  resource_list
}

gh_path_resolver <- function(path, repo, branch) {
  file.path("https://raw.githubusercontent.com", repo, branch, path)
}

splitr_resources <-
  register_resources(
    resource_list = 
      github_resources(
        repo = "rich-iannone/splitr",
        branch = "master",
        osx_hyts_std = file.path("extras", "osx", "hyts_std"),
        osx_hycs_std = file.path("extras", "osx", "hycs_std"),
        osx_parhplot = file.path("extras", "osx", "parhplot"),
        win_hyts_std = file.path("extras", "win", "hyts_std.exe"),
        win_hycs_std = file.path("extras", "win", "hycs_std.exe"),
        win_parhplot = file.path("extras", "win", "parhplot.exe"),
        l32_hyts_std = file.path("extras", "linux-x86", "hyts_std"),
        l32_hycs_std = file.path("extras", "linux-x86", "hycs_std"),
        l32_parhplot = file.path("extras", "linux-x86", "parhplot"),
        l64_hyts_std = file.path("extras", "linux-amd64", "hyts_std"),
        l64_hycs_std = file.path("extras", "linux-amd64", "hycs_std"),
        l64_parhplot = file.path("extras", "linux-amd64", "parhplot")
      )
  )

fetch_resources <- function(resources,
                            names,
                            out_dir = NULL) {
  
  if (is.null(out_dir)) {
    out_dir <- getwd()
  } else {
    out_dir <- file.path(getwd(), out_dir)
  }
  
  resource_names <- names(resources)
  
  for (name in names) {
    
    if (name %in% resource_names) {
      
      filename <- basename(resources[[name]])
      
      downloader::download(
        url = resources[[name]],
        destfile = file.path(out_dir, filename)
      )
    }
  }
  
}

get_resource_names_by_system_os <- function(system_os) {
  
  if (system_os == "mac") {
    resource_names <- c("osx_hyts_std", "osx_hycs_std", "osx_parhplot")
  } else if (system_os == "win") {
    resource_names <- c("win_hyts_std", "win_hycs_std", "win_parhplot")
  } else {
    resource_names <- c("l64_hyts_std", "l64_hycs_std", "l64_parhplot")
  }
  
  resource_names
}
