demo.trajectory <- function(){
  
  # First, delete any previous instances of the 'Demo Project' that were created
  # when using this function previously
  
  project.archive("Demo Project")
  
  setwd("~/Documents/SplitR")
  
  step_1 <-
    readline(paste(cat("This demo will show you how a trajectory project is carried out", "\n",
                       "There are step-by-step prompts, showing which directories were made,", "\n",
                       "what functions were run, and where the output went.", "\n",
                       "It'll give you all you need to set up HYSPLIT trajectory models.", "\n",
                       "(N.B. increase the height of the console window as much you can.)", "\n",
                       "\n",
                       "Press <ENTER> to continue.", "\n",
                       sep = '')))
  
  if (step_1 == "") cat()
  
  paste(cat("---------------------------------------------------------------", "\n",
            "\n",
            "Let's have a look at the the folders made from using the", "\n",
            "'SplitR.init' function.", "\n",
            "Five folders were made: Exec, Met, Output, Projects, and Working.", "\n",
            sep = ''))
  
  dirs <- as.data.frame(list.dirs(path = "~/Documents/SplitR", recursive = FALSE))
  colnames(dirs) <- "Folders"
  dirs
  
  step_2 <-
    readline(paste(cat("---------------------------------------------------------------", "\n",
                       "The first step toward running models is to define a project.", "\n",
                       "Let's create a project and call it \"Demo Project\".", "\n",
                       "\n",
                       "Press <ENTER> to run the following:", "\n",
                       "\n",
                       "project.define(\"Demo Project\")",
                       sep = '')))
  
  if (step_2 == ""){
    project.define("Demo Project")
  }
  
  step_3 <-
    readline(paste(cat("---------------------------------------------------------------", "\n",
                       "You can see a list of all projects with 'project.list'", "\n",
                       "\n",
                       "Press <ENTER> to run the following:", "\n",
                       "\n",
                       "project.list()",
                       sep = '')))
  
  if (step_3 == ""){
    project.list()
  }
  
  step_4 <-
    readline(paste(cat("---------------------------------------------------------------", "\n",
                       "Once the project is defined, open that project with 'project.open'", "\n",
                       "\n",
                       "Press <ENTER> to run the following:", "\n",
                       "\n",
                       "project.open(project_name = \"Demo Project\")",
                       sep = '')))
  
  if (step_4 == ""){
    project.open(project_name = "Demo Project")
  }
  
  step_5 <-
    readline(paste(cat("---------------------------------------------------------------", "\n",
                       "You might notice that when using 'project.open', the working", "\n",
                       "directory moves to that of the project.", "\n",
                       "\n",
                       "Press <ENTER> to continue.", "\n",
                       sep = '')))
  
  if (step_5 == ""){
    print(getwd())
  }

  step_6 <-
    readline(paste(cat("---------------------------------------------------------------", "\n",
                       "Once a project is defined, we can define a series of trajectory", "\n",
                       "runs using the 'hysplit.trajectory' function. The necessary met", "\n",
                       "files will be downloaded automatically if they're not available.", "\n",
                       "\n",
                       "Press <ENTER> to run the following:", "\n",
                       "\n",
    "hysplit.trajectory(start_lat_deg = 42.83752, start_long_deg = -80.30364,
                   start_height_m_AGL = 5, simulation_duration_h = 24,
                   backtrajectory = FALSE, met_type = \"reanalysis\",
                   vertical_motion_option = 0,
                   top_of_model_domain_m = 20000,
                   run_type = \"day\", run_day = \"2012-03-12\",
                   daily_hours_to_start = c(\"00\", \"06\", \"12\", \"18\"))",
    sep = '')))
  
  if (step_6 == ""){
    hysplit.trajectory(start_lat_deg = 42.83752, start_long_deg = -80.30364,
                       start_height_m_AGL = 5, simulation_duration_h = 24,
                       backtrajectory = FALSE, met_type = "reanalysis",
                       vertical_motion_option = 0,
                       top_of_model_domain_m = 20000,
                       run_type = "day", run_day = "2012-03-12",
                       daily_hours_to_start = c("00", "06", "12", "18"))
  }
  
}