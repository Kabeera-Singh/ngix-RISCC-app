FROM rocker/shiny:latest

# Install nginx and supervisor
RUN apt-get update && apt-get install -y \
    nginx \
    supervisor \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libssl-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'tidyverse', 'shinyWidgets', 'DT', 'stringr', 'readxl', 'shinyjs', 'rrapply', 'data.table','shinydashboard','bslib'), repos='https://cran.rstudio.com/', dependencies=TRUE)"

# Copy Shiny Server configuration
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Copy nginx configuration
COPY nginx/conf.d/default.conf /etc/nginx/nginx.conf

# Copy HTML files
COPY nginx/html/ /usr/share/nginx/html/

# Copy Shiny apps
COPY shiny-apps/app1/ /srv/shiny-server/app1/
COPY shiny-apps/app2/ /srv/shiny-server/app2/

# Create supervisor configuration
RUN mkdir -p /var/log/supervisor
COPY supervisor.conf /etc/supervisor/conf.d/supervisord.conf

# Set proper permissions
RUN chown -R shiny:shiny /srv/shiny-server/ && \
    chown -R www-data:www-data /usr/share/nginx/html && \
    mkdir -p /var/log/nginx && \
    touch /var/log/nginx/access.log /var/log/nginx/error.log && \
    chown www-data:www-data /var/log/nginx/*

# Expose port 80 (nginx will handle routing)
EXPOSE 80

# Use supervisor to run both nginx and shiny-server
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]