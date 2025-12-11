# Base R Shiny image
FROM rocker/shiny:4.2.1

# Install R system dependencies (add any others you need here)
RUN install2.r --error \
    shiny \
    dplyr \
    ggplot2 \
    # Add other libraries your app uses here
    
# Copy the app to the image
COPY . /srv/shiny-server/

# Hugging Face runs on port 7860, so we need to set that
ENV SHINY_SERVER_PORT=7860
EXPOSE 7860

# Run the app
CMD ["/usr/bin/shiny-server"]
