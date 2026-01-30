# Use the Rocker project’s Shiny base image
FROM rocker/shiny:4.5.1

# Install system dependencies
RUN apt-get update && apt-get install -y libcurl4-openssl-dev curl \
libssl-dev libxml2-dev libicu-dev libudunits2-dev zlib1g-dev libwebp-dev \
libpoppler-cpp-dev pkg-config libtesseract-dev libleptonica-dev \
libprotobuf-dev protobuf-compiler glpk-utils libglpk-dev libgdal-dev \
libfontconfig1-dev libcairo2-dev libxt-dev texlive-full libharfbuzz-dev \
libfribidi-dev libx11-dev libfontconfig1 fonts-liberation fpc gcc make

# Install Google Chrome from https://mirror.cs.uchicago.edu/google-chrome/pool/main/g/google-chrome-stable/
RUN wget https://mirror.cs.uchicago.edu/google-chrome/pool/main/g/google-chrome-stable/google-chrome-stable_140.0.7339.127-1_amd64.deb
RUN dpkg -i google-chrome-stable_140.0.7339.127-1_amd64.deb || apt-get install -fy
ENV CHROMOTE_CHROME=/usr/bin/google-chrome
RUN apt-get clean && rm *.deb && rm -rf /var/lib/apt/lists/*

# Copy Shiny app and custom config to the container
COPY ./ /srv/shiny-server/

# Move custom config into place
RUN mv /srv/shiny-server/shiny-server.conf /etc/shiny-server/shiny-server.conf

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Install R package dependencies
RUN R -e "install.packages(c('shiny', 'shinyBS', 'shinyWidgets', 'shinyjqui', \
'shinysky', 'readr', 'openxlsx', 'stringr', 'fs', 'plyr', 'ipa', 'seewave', \
'tuneR', 'proxy', 'dtw', 'udpipe', 'ggplot2', 'ggh4x', 'ggrepel', 'deldir', \
'ggdendro', 'dynamicTreeCut', 'dbscan', 'fpc', 'MASS', 'pcaPP', 'geodist', \
'colouR', 'crosstalk', 'htmltools', 'htmlwidgets', 'jquerylib', 'leaflet.providers', \
'magrittr', 'methods', 'png', 'raster', 'RColorBrewer', 'rlang', 'scales', \
'sf', 'viridisLite', 'xfun', 'leaflet', 'leaflet.extras', 'Rtsne', 'grid', 'dplyr', \
'svglite', 'Cairo', 'tikzDevice', 'htmlwidgets', 'webshot2', 'callr', 'remotes', \
'RJSONIO'))"

# Install naturalearch packages
RUN R -e 'install.packages(c("rnaturalearth", "rnaturalearthdata")); \
    options(timeout = 300); \
    if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes"); \
    tryCatch( \
        remotes::install_github("ropensci/rnaturalearthhires", quiet = TRUE, upgrade = "never", dependencies = TRUE), \
        error=function(e) message("Failed to install rnaturalearthhires from GitHub, continuing without it") \
    )'

# Install shinysky package
RUN R -e "install.packages('/srv/shiny-server/shinysky_0.1.3.tar.gz', repos=NULL, type='source')"

# Set working directory
WORKDIR /srv/shiny-server

# Compile programs
RUN fpc cron.pas
RUN fpc leven1.pas
RUN gcc -s -Wall -o leven2 leven2.c -lm
RUN fpc phon.pas
RUN gcc -s -Wall -o robust robust.c -lm

# Expose the Shiny port
EXPOSE 3838

# Clean up any leftover Chrome sessions before starting Shiny
CMD ["/bin/bash", "-c", "pkill chrome || true && pkill chromedriver || true && /usr/bin/shiny-server"]
