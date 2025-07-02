# Use the Rocker project’s Shiny base image
FROM rocker/shiny:4.5.1

# Install system dependencies
RUN apt-get update
RUN apt-get install -y libcurl4-openssl-dev
RUN apt-get install -y curl
RUN apt-get install -y libssl-dev
RUN apt-get install -y libxml2-dev
RUN apt-get install -y libicu-dev
RUN apt-get install -y libudunits2-dev
RUN apt-get install -y zlib1g-dev
RUN apt-get install -y libwebp-dev
RUN apt-get install -y libpoppler-cpp-dev
RUN apt-get install -y pkg-config
RUN apt-get install -y libtesseract-dev
RUN apt-get install -y libleptonica-dev
RUN apt-get install -y libprotobuf-dev
RUN apt-get install -y protobuf-compiler
RUN apt-get install -y glpk-utils
RUN apt-get install -y libglpk-dev
RUN apt-get install -y libgdal-dev
RUN apt-get install -y libfontconfig1-dev
RUN apt-get install -y libcairo2-dev
RUN apt-get install -y libxt-dev
RUN apt-get install -y texlive-full
RUN apt-get install -y libharfbuzz-dev
RUN apt-get install -y libfribidi-dev
RUN apt-get install -y libx11-dev
RUN apt-get install -y libfontconfig1
RUN apt-get install -y fpc
RUN apt-get install -y gcc
RUN apt-get install -y make

RUN apt-get install -y fonts-liberation
RUN wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
RUN dpkg -i google-chrome-stable_current_amd64.deb || apt-get install -fy
ENV CHROMOTE_CHROME=/usr/bin/google-chrome
RUN rm -rf /var/lib/apt/lists/*

# Copy your app to the container
COPY ./ /srv/shiny-server/

# Move custom config into place
RUN mv /srv/shiny-server/shiny-server.conf /etc/shiny-server/shiny-server.conf

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Install R package dependencies
RUN R -e "install.packages('shiny',             dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyBS',           dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyWidgets',      dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyjqui',         dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinysky',          dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('readr',             dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('openxlsx',          dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('stringr',           dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('fs',                dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('plyr',              dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ipa',               dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('tuneR',             dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('proxy',             dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('dtw',               dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('udpipe',            dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ggplot2',           dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ggh4x',             dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ggrepel',           dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('deldir',            dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ggdendro',          dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('dynamicTreeCut',    dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('apcluster',         dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('dbscan',            dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('fpc',               dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('MASS',              dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('pcaPP',             dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('geodist',           dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('colouR',            dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('leaflet',           dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('leaflet.extras',    dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('sf',                dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('Rtsne',             dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('grid',              dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('dplyr',             dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('svglite',           dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('Cairo',             dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('tikzDevice',        dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('htmlwidgets',       dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('webshot2',          dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('callr',             dependencies=TRUE, repos='https://cloud.r-project.org/')"

RUN R -e "install.packages('remotes',           dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "remotes::install_github('ropensci/rnaturalearth')"
RUN R -e "remotes::install_github('ropensci/rnaturalearthdata')"
RUN R -e "remotes::install_github('ropensci/rnaturalearthhires')"

RUN R -e "install.packages('RJSONIO',           dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('/srv/shiny-server/shinysky_0.1.3.tar.gz', repos=NULL, type='source')"

# Set working directory
WORKDIR /srv/shiny-server

# Compile programs
RUN fpc cron.pas
RUN fpc leven1.pas
RUN gcc -s -Wall -o leven2 leven2.c -lm
RUN fpc phon.pas
RUN fpc robust.pas

# Expose the Shiny port
EXPOSE 3838

# Run the Shiny app
CMD ["/usr/bin/shiny-server"]
