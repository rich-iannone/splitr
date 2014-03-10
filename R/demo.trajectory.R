demo.trajectory <- function(){
  
  # This function is meant to show the user how a trajectory project is carried out
  # There are step-by-step prompts, showing the user which directories were made,
  # what functions were run, and where the output went
  
  # First, delete any previous instances of the 'Demo Project' that were created
  # when using this function previously
  
  
  # Welcome to the SplitR trajectory demo! The aim of this walkthrough
  # is give you all you need to run HYSPLIT trajectory runs.
  
  # Let's have a look around at the the folders that were generated from using the
  # 'SplitR.init' function. Five folders were made: Exec, Met, Output, Projects, and Working.
  
  setwd("~/Documents/SplitR")
  
  dirs <- as.data.frame(list.dirs(path = "~/Documents/SplitR", recursive = FALSE))
  colnames(dirs) <- "Folders"
  dirs
  
  # The first step toward running models is to define a project.
  # Let's create a project and call it "Demo Project".
    
  project.define("Demo Project")
  
  # You can see a list of all projects with 'project.list'
  
  project.list()
  
  # Once the project is defined, open that project with 'project.open'
  
  project.open(project_name = "Demo Project")
  
  # Notice that when using 'project.open', the working directory moves
  # to that of the project.
  
  getwd()
  
  
  
}