\name{Define a SplitR project}
\alias{project.define}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Define a name for a new SplitR project and create the project subfolder.
}
\description{
By defining a SplitR project, one can keep different models and analyses separated through project folders. If no folder structure is set up (i.e., first use of command), the project folder tree will be set up in the Documents folder. There is the option to manually specify a project folder location by supplying a path. This manual option is useful for having output written to external storage, for example.
}
\usage{
project.define(project_name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{project_name}{
a name that describes the project in sufficient detail.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
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
