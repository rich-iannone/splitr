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
                       "Let's set up some presets for the model runs. The first is",
                       " for the species.", "\n",
                       "\n",
                       "Press <ENTER> to set up a 'species' preset", "\n",
                       "\n",
     "--> dispersion.preset.add(\"species\", interactive = FALSE,
                          species_name = \"Demo\")",
                       sep = '')))
  
  if (step_6 == ""){
    dispersion.preset.add("species", interactive = FALSE,
                          species_name = "Demo")
  }
  
  step_7 <-
    readline(paste(cat("\n",
                       "--------------------------------------------------------------",
                       "-----------------", "\n",
                       "Another preset that you can add is for emissions. This sets up",
                       " the emission", "\n",
                       "rate, the timing of the emissions, and the name of the",
                       " emissions source.", "\n",
                       "\n",
                       "Press <ENTER> to set up an 'emissions' preset", "\n",
                       "\n",
     "--> dispersion.preset.add(\"emissions\", interactive = FALSE,
                          emissions_name = \"Demo\")",
                       sep = '')))
  
  if (step_7 == ""){
    dispersion.preset.add("emissions", interactive = FALSE,
                          emissions_name = "Demo")
  }
  
  step_8 <-
    readline(paste(cat("\n",
                       "--------------------------------------------------------------",
                       "-----------------", "\n",
                       "The last preset type is that for the sampling grids. This sets",
                       " up the grids", "\n",
                       "\n",
                       "Press <ENTER> to set up an 'grids' preset", "\n",
                       "\n",
     "--> dispersion.preset.add(\"grids\", interactive = FALSE,
                          grid_name = \"Demo\")",
                       sep = '')))
  
  if (step_8 == ""){
    dispersion.preset.add("grids", interactive = FALSE,
                          grid_name = "Demo")
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
                       start_height_m_AGL = 5, simulation_duration_h = 12,
                       backward_running = FALSE, met_type = \"reanalysis\",
                       vertical_motion_option = 0, top_of_model_domain_m = 20000,
                       grids = dispersion.preset.list(\"grids\", \"Demo\"),
                       species = dispersion.preset.list(\"species\", \"Demo\"),
                       emissions = dispersion.preset.list(\"emissions\", \"Demo\"),
                       run_type = \"range\", run_range = c(\"2010-05-01\", \"2010-05-01\"),
                       daily_hours_to_start = \"18\")",
                       sep = '')))
  
  if (step_10 == ""){
    hysplit.dispersion(start_lat_deg = 37.5, start_long_deg = -100.0,
                       start_height_m_AGL = 5,
                       simulation_duration_h = 12, backward_running = FALSE,
                       met_type = "reanalysis",
                       vertical_motion_option = 0, top_of_model_domain_m = 20000,
                       grids = dispersion.preset.list("grids", "Demo"),
                       species = dispersion.preset.list("species", "Demo"),
                       emissions = dispersion.preset.list("emissions", "Demo"),
                       run_type = "range", run_range = c("2010-05-01", "2010-05-01"),
                       daily_hours_to_start = "18")
  }
  
  step_11 <-
    readline(paste(cat("\n",
                       "--------------------------------------------------------------",
                       "-----------------", "\n",
                       "If you saw 'Complete Hysplit' (and you should be seeing it now), ",
                       "the dispersion", "\n",
                       "runs are now finished. The output files are in the project ",
                       "folder with the", "\n",
                       "naming ['Demo Disp Project' + a date string].", "\n",
                       "\n",
                       "Press <ENTER> to show the generated output", "\n",
                       sep = '')))
  
  if (step_11 == ""){                
    project_list <- project.list(display_paths = TRUE)
    files <-
      as.data.frame(list.files(path = project_list[project_list[,1] == "Demo Disp Project"][[3]],
                               recursive = FALSE))
    colnames(files) <- "Files"
    print(files)
  }
  
  
}