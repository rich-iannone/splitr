FROM rocker/rstudio:3.4.0
  
# Install
RUN install2.r --error --deps TRUE \
here \ 
&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN R -e "devtools::install_github('rich-iannone/splitr')"

#sudo apt-get install gcc-7 g++-7
#sudo apt-get install gfortran-7