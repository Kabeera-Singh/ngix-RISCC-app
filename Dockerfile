FROM rocker/shiny:latest

# Install system dependencies if needed
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libssl-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev

# Install R packages
RUN R -e "install.packages(c('shiny', 'tidyverse', 'shinyWidgets', 'DT', 'stringr', 'readxl', 'shinyjs', 'rrapply', 'data.table'), repos='https://cran.rstudio.com/', dependencies=TRUE)"

# Copy custom Shiny Server configuration
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Make sure app directories exist
RUN mkdir -p /srv/shiny-server/app1 /srv/shiny-server/app2

# Set proper permissions
RUN chown -R shiny:shiny /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]