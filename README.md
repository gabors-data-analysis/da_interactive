# Gabors Interactive Data Analysis
Interactive site for Data Analysis for Business, Economics, and Policy

## Shiny app
[Gabors Interactive Data Analysis](https://gabors-data-analysis.shinyapps.io/hotels-europe/)


## da_interactive
Current version: March 2021

Beta version with some minor bugs

## Docker Image

A Docker image for the hotels-europe Shiny application is automatically built and published to GitHub Container Registry using GitHub Actions when changes are made to the `hotels-europe/v0.2/` directory.

To pull the latest image:

```bash
docker pull ghcr.io/gabors-data-analysis/da_interactive/hotels-europe:latest
```

To run the container:

```bash
docker run -p 3838:3838 ghcr.io/gabors-data-analysis/da_interactive/hotels-europe:latest
```

Then access the Shiny application at http://localhost:3838

## Shinyproxy configuration

The public site run at https://dashboards.gabors-data-analysis.com is using the
configuration files stored in the `shinyproxy` directory. This includes:

- Dashboard listing page HTML structure and CSS styling
- List of applications and their configuration
