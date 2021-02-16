FROM rocker/tidyverse:3.3.1
  
# Install splitr
RUN R -e "devtools::install_github('rich-iannone/splitr')"

#RUN install2.r --error --deps TRUE \
#here \ 
#&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds
#sudo apt-get install gcc-7 g++-7
#sudo apt-get install gfortran-7
