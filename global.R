library(shinydashboard)
library(shinyWidgets)
library(RColorBrewer)
library(openlattice)
library(highcharter)
library(tidyverse)
library(lubridate)
library(shinyjs)
library(plotly)
library(shiny)
library(vroom)
# library(xts)

options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 0)))

ol_cols <- c("#ff3c5d",
             "#6124e2",
             "#ffe671",
             "#ff9a58",
             "#dd9e00",
             "#00be84")

source("helper_functions.R")
source("modules/authentication.R")
source("read_format_data.R")
source("modules/home.R")
source("modules/jail.R")
# source("modules/dictionaries.R")

required_role = "SU_dashboard"

pltl_margins <- list(
  l = 80,
  r = 50,
  b = 25,
  t = 50,
  pad = 4
)

color2 <- brewer.pal(5, "Purples")
customcolor2 <- color2[-1] #delete a color  - first one
customcolor2 <- rev(customcolor2)
customcolor2 <-
  append(customcolor2, "#708090", after = 0) #slategray1, add grey to represent NA values in the graph

