demo.trajectory <- function(){
  
  # First, delete any previous instances of the 'Demo Traj Project' that were created
  # when using this function previously
  
  setwd("~/Documents/SplitR")
  
  project.archive("Demo Traj Project")
  
  step_1 <-
    readline(paste(cat("This demo will show you how a trajectory project is carried out.", "\n",
                       "There are step-by-step prompts, showing which directories were made,", "\n",
                       "what functions were run, and where the output was placed. It'll give", "\n",
                       "you all that you need to set up some HYSPLIT trajectory models.", "\n",
                       "(N.B. increase the height of the console as much you can.)", "\n",
                       "\n",
                       "Press <ENTER> to continue.",
                       sep = '')))
  
  if (step_1 == "") cat()
  
  paste(cat("\n",
            "-------------------------------------------------------------------------------", "\n",
            "\n",
            "Let's have a look at the the folders made from using the 'SplitR.init'", "\n",
            "function. Five folders were made: Exec, Met, Output, Projects, and Working.", "\n",
            "(Archive might also be there; it's generated with use of 'project.archive'.)", "\n",
            "\n",
            sep = ''))
  
  dirs <- as.data.frame(list.dirs(path = "~/Documents/SplitR", recursive = FALSE))
  colnames(dirs) <- "Folders"
  print(dirs)
  
  step_2 <-
    readline(paste(cat("\n",
                       "--------------------------------------------------------------",
                       "-----------------", "\n",
                       "The first step toward running models is to define a project.", "\n",
                       "Let's create a project and call it \"Demo Traj Project\".", "\n",
                       "\n",
                       "Press <ENTER> to run the following:", "\n",
                       "\n",
                       "--> project.define(\"Demo Traj Project\")",
                       sep = '')))
  
  if (step_2 == ""){
    project.define("Demo Traj Project")
  }
  
  step_3 <-
    readline(paste(cat("\n",
                       "--------------------------------------------------------------",
                       "-----------------", "\n",
                       "You can see a list of all projects with 'project.list'", "\n",
                       "\n",
                       "Press <ENTER> to run the following:", "\n",
                       "\n",
                       "--> project.list()",
                       sep = '')))
  
  if (step_3 == ""){
    print(project.list())
  }
  
  step_4 <-
    readline(paste(cat("\n",
                       "--------------------------------------------------------------",
                       "-----------------", "\n",
                       "Once the project is defined, open that project with 'project.open'", "\n",
                       "\n",
                       "Press <ENTER> to run the following:", "\n",
                       "\n",
                       "--> project.open(project_name = \"Demo Traj Project\")",
                       sep = '')))
  
  if (step_4 == ""){
    project.open(project_name = "Demo Traj Project")
  }
  
  step_5 <-
    readline(paste(cat("\n",
                       "--------------------------------------------------------------",
                       "-----------------", "\n",
                       "You might notice that when using 'project.open', the working ",
                       "directory moves", "\n",
                       "to that of the project.", "\n",
                       "\n",
                       "Press <ENTER> to get the working directory", "\n",
                       sep = '')))
  
  if (step_5 == ""){
    print(getwd())
  }

  step_6 <-
    readline(paste(cat("\n",
                       "--------------------------------------------------------------",
                       "-----------------", "\n",
                       "Once a project is defined, we can define a series of trajectory",
                       " runs using the ", "\n",
                       "'hysplit.trajectory' function. The necessary met files will be ",
                       "downloaded ", "\n",
                       "automatically if they're not available.", "\n",
                       "\n",
                       "Press <ENTER> to run the following:", "\n",
                       "\n",
    "--> hysplit.trajectory(start_lat_deg = 42.83752, start_long_deg = -80.30364,
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
  
  step_7 <-
    readline(paste(cat("\n",
                       "--------------------------------------------------------------",
                       "-----------------", "\n",
                       "If you saw 'Complete Hysplit' (and you should be seeing it now), ",
                       "the trajectory", "\n",
                       "runs are now finished. The output files are in the project ",
                       "folder with the", "\n",
                       "naming ['Demo Traj Project' + a date string].", "\n",
                       "\n",
                       "Press <ENTER> to show the generated output", "\n",
                       sep = '')))
  
  if (step_7 == ""){                
    project_list <- project.list(display_paths = TRUE)
    files <-
      as.data.frame(list.files(path = project_list[project_list[,1] == "Demo Traj Project"][[3]],
                               recursive = FALSE))
    colnames(files) <- "Files"
    print(files)
  }
  
  step_8 <-
    readline(paste(cat("\n",
                       "--------------------------------------------------------------",
                       "-----------------", "\n",
                       "This concludes the demo of a set of HYSPLIT trajectory runs. ", "\n",
                       "Press <ENTER> to end the demo", "\n",
                       sep = '')))
  
  if (step_8 == ""){                
    cat()
  }
  
}