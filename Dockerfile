FROM rocker/tidyverse:3.5.0
  
# Install splitr
RUN R -e "devtools::install_github('rich-iannone/splitr')"

RUN install2.r --error --deps TRUE \
here \ 
&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds
