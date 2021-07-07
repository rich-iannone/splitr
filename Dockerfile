FROM rocker/tidyverse:3.5.0
  
# Install splitr
RUN R -e "devtools::install_github('rich-iannone/splitr')"

RUN chmod +x /usr/local/lib/R/site-library/splitr/linux-amd64/hyts_std
RUN chmod +x /usr/local/lib/R/site-library/splitr/linux-amd64/hycs_std
RUN chmod +x /usr/local/lib/R/site-library/splitr/linux-amd64/parhplot

RUN install2.r --error --deps TRUE \
here \ 
&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds
