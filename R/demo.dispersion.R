demo.dispersion <- function(){
 
  # First, delete any previous instances of the 'Demo Disp Project' that were created
  # when using this function previously
  
  project.archive("Demo Disp Project")
  
  setwd("~/Documents/SplitR")
  
  step_1 <-
    readline(paste(cat("This demo will show you how a dispersion project is carried out.", "\n",
                       "There are step-by-step prompts, showing which directories were made,", "\n",
                       "what functions were run, and where the output was placed. It'll give", "\n",
                       "you all that you need to set up some HYSPLIT dispersion models.", "\n",
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
                       "Let's create a project and call it \"Demo Disp Project\".", "\n",
                       "\n",
                       "Press <ENTER> to run the following:", "\n",
                       "\n",
                       "--> project.define(\"Demo Disp Project\")",
                       sep = '')))
  
  if (step_2 == ""){
    project.define("Demo Disp Project")
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
                       "--> project.open(project_name = \"Demo Disp Project\")",
                       sep = '')))
  
  if (step_4 == ""){
    project.open(project_name = "Demo Disp Project")
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
                       "Let's set up some presets for the dispersion runs. The first ",
                       "is for the species.", "\n",
                       "\n",
                       "Press <ENTER> to set up a 'species' preset", "\n",
                       sep = '')))
  
  if (step_6 == ""){
    
  }
  
  
  
  
  step_10 <-
    readline(paste(cat("\n",
                       "--------------------------------------------------------------",
                       "-----------------", "\n",
                       "Once a project is defined, we can define a series of dispersion",
                       " runs using the ", "\n",
                       "'hysplit.dispersion' function. The necessary met files will be ",
                       "downloaded ", "\n",
                       "automatically if they're not available.", "\n",
                       "\n",
                       "Press <ENTER> to run the following:", "\n",
                       "\n",  
     "--> hysplit.dispersion(start_lat_deg = 37.5, start_long_deg = -100.0,
                       start_height_m_AGL = 5,
                       simulation_duration_h = 72, backward_running = FALSE,
                       met_type = \"reanalysis\",
                       vertical_motion_option = 0, top_of_model_domain_m = 20000, grids = c(1),
                       species = c(1), emissions = c(1),
                       run_type = \"range\", run_range = c(\"2010-05-01\", \"2010-05-01\"),
                       daily_hours_to_start = \"18\")",
                       sep = '')))
  
  if (step_10 == ""){
    hysplit.dispersion(start_lat_deg = 37.5, start_long_deg = -100.0,
                       start_height_m_AGL = 5,
                       simulation_duration_h = 72, backward_running = FALSE,
                       met_type = "reanalysis",
                       vertical_motion_option = 0, top_of_model_domain_m = 20000,
                       grids = c(1), species = c(1), emissions = c(1),
                       run_type = "range", run_range = c("2010-05-01", "2010-05-01"),
                       daily_hours_to_start = "18")
  }
  
}