FROM rocker/geospatial:3.6.2
  
# Install
RUN install2.r --error --deps TRUE \
here \ 
&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN R -e "devtools::install_github('rich-iannone/splitr')"

