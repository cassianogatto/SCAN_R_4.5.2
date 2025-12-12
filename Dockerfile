# Base R Shiny image
FROM rocker/shiny:4.2.1

# 1. Install System Dependencies (keep this if you haven't already)
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# 2. Install R Packages
RUN install2.r --error \
    shiny \
    shinydashboard \
    dplyr \
    igraph \
    tidygraph \
    tidyr \
    ggraph \
    readr \
    sf \
    ggplot2 \
    leaflet \
    units \
    lwgeom \
    RColorBrewer \
    DT

# 3. Copy the app
COPY . /srv/shiny-server/

# --- THE FIX IS HERE ---
# Force Shiny Server to listen on port 7860 instead of default 3838
RUN sed -i 's/3838/7860/' /etc/shiny-server/shiny-server.conf
# -----------------------

# 4. Expose and Run
EXPOSE 7860
CMD ["/usr/bin/shiny-server"]
