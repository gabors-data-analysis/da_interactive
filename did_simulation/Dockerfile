FROM rocker/r-ubuntu:latest

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    r-cran-tidyverse \
    r-cran-plotly \
    r-cran-shiny \
    r-cran-knitr \
    r-cran-htmltools \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# not available as binary pkg
RUN install2.r --skipinstalled fixest did

WORKDIR /app
COPY server.r ui.r global.r ./

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
