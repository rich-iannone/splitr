FROM rocker/geospatial:3.6.2
  
# Install
RUN install2.r --error --deps TRUE \
splitr \
here \ 
&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds
