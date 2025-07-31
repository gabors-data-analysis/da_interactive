FROM rocker/r-ubuntu:latest

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    r-cran-data.table \
    r-cran-checkmate \
    r-cran-tidyverse \
    r-cran-plotly \
    r-cran-cowplot \
    r-cran-skimr \
    r-cran-dt \
    r-cran-shiny \
    r-cran-shinythemes \
    r-cran-plyr \
    r-cran-writexl && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# not available as binary pkg
RUN install2.r --skipinstalled modelsummary shinyWidgets shinyBS

WORKDIR /app
COPY server.R ui.R theme_bg.R ./
COPY data/hotels_clean.rds ./data/

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
