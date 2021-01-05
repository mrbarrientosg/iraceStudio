suppressMessages(library(iraceStudio))
suppressMessages(library(shiny))
suppressMessages(library(DT))
suppressMessages(library(bs4Dash))
suppressMessages(library(shinyjs))
suppressMessages(library(shinyalert))
suppressMessages(library(data.table))
suppressMessages(library(irace))
suppressMessages(library(promises))
suppressMessages(library(future))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shinyFiles))
suppressMessages(library(ipc))
suppressMessages(library(fs))
suppressMessages(library(logger))
suppressMessages(library(R6))
suppressMessages(library(dplyr))
suppressMessages(library(shinybusy))
suppressMessages(library(plotly))
suppressMessages(library(shinycssloaders))
suppressMessages(library(shinyhelper))

for (f in list.files("component", recursive = T, pattern = '.R', full.names = T)) {
  source(f)
}

for (f in list.files("modules", recursive = T, pattern = '.R', full.names = T)) {
  source(f)
}

app <- App$new()