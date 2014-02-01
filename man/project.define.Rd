\name{Define a SplitR project}
\alias{project.define}
\title{
Define a name for a new SplitR project and create the project subfolder.
}
\description{
By defining a SplitR project, one can keep different models and analyses separated through project folders. If no folder structure is set up (i.e., first use of command), the project folder tree will be set up in the Documents folder.
}
\usage{
project.define(project_name)
}
\arguments{
  \item{project_name}{
a name that describes the project in sufficient detail.
}
}
\author{
Richard Iannone
}
\examples{
# Set up a new project entitled 'dispersion - rainforest case'
project.define('dispersion - rainforest case')

# Install the SplitR folders in '~/Documents' without defining
# a new project
project.define(install_folders = TRUE)

# Install the SplitR folders in an alternate location other than
# in '~/Documents' without defining a new project
project.define(install_folders = TRUE,
			   install_in_documents = FALSE,
			   other_install_path = '/Volumes/External')
}
