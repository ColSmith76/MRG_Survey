# Functions and code to build dashboard

# Renders the dashboard using Rmarkdown, then post-processes the HTML file to
# change display settings.

# read in helper functions and define any necessary objects

# Dashboard Name
dashboardName <- "MRG_Survey_Results.html"

# Support functions
source("db_build_graphics_functions.R")

# Plot theme
theme_db <- theme_bw() + theme(plot.margin = unit(c(10,10,20,10),"pt"))

# Packages
library(flexdashboard)
library(leaflet)
library(plotly)
library(scales)
library(pander)
library(geojsonlint)
library(stringr)

# Generate dashboard
rmarkdown::render("MRG_Survey_Results.Rmd",
                  output_file = dashboardName,
                  quiet = TRUE)

# The following lines change the rendering engine from SVG to Canvas, which
# speeds up map rendering considerably.
dashboardHTML <- readLines(dashboardName)
idx <- which(dashboardHTML == "window.FlexDashboardComponents = [];")[1]
dashboardHTML <- append(dashboardHTML, "L_PREFER_CANVAS = true;", after = idx)
writeLines(dashboardHTML, dashboardName)