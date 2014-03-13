demo.dispersion <- function(){
 
  
  
  
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
  
}