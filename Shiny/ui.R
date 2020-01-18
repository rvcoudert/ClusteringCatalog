library(magrittr)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

#list of packages required
list.of.packages <- c(
  "magrittr",
  "shiny",
  "shinyWidgets",
  "shinydashboard",
  "ggplot2",
  "dplyr",
  "reshape2",
  "cluster")

#checking missing packages from list
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[
    ,"Package"])]

#install missing ones
if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)

# Changer le répertoire de travail sur celui du script en cours.
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Pour partager le projet aux autres utilisateurs de R :
# shiny::runGitHub(
#   repo = "ClusteringCatalog",
#   username = "rvcoudert",
#   subdir = "Shiny")



# ----- preprocess -----


# Build pages by calling another Rscript.
for (script in list.files("pages/")) {
  source(
    file = paste("pages/", script, sep = ""),
    local = TRUE,
    encoding = "UTF-8"
  )
}


# ----- dashboardPage -----


dashboardPagePlus(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "myStyle.css")
  ),
  skin = "red",
  # ----- dashboardHeader -----
  header = dashboardHeaderPlus(
    title = tagList(
      tag = span(class = "logo-lg", "ClusteringCatalog"),
      attr = icon("window-restore")
    ),
    fixed = TRUE,
    left_menu = tagList(
      dropdownButton(
        tooltip = tooltipOptions(title = "Data Parameters"),
        size = "sm",
        icon =  icon("sliders"),
        # icon =  tags$i(
        #   class = "fa fa-sliders",
        #   style = "color: rgb(0,166,90)"
        # ),
        # right = TRUE,
        radioButtons(
          inputId = "genData_method",
          label = "Data Generation",
          selected = "blobs",
          choiceNames = c("Blobs", "Moons", "Concentric Circles"),
          choiceValues = c("blobs", "moons", "circles")
        ),
        numericInput(
          inputId = "genData_nSamples",
          label = "Sample Size",
          min = 0,
          max = 10000,
          value = 500,
          step = 1
        ),
        numericInput(
          inputId = "genData_clusterSd",
          label = "Cluster Dispersion",
          min = 0,
          max = 10,
          value = 1,
          step = 0.1
        ),
        sliderTextInput(
          inputId = "seed",
          label = "Data Seed",
          choices = 1:20,
          selected = 1,
          grid = TRUE
        ),
        uiOutput(
          outputId = "blobsNbClusters"
        ),
        uiOutput(
          outputId = "blobsMinDist"
        ),
        uiOutput(
          outputId = "circlesScale"
        )
      )
    )
  ),
  # ----- dashboardSidebar -----
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebar_tab",
      menuItem(
        text = "Home",
        tabName = "home",
        icon = icon("home")
      ),
      menuItem(
        text = "Data Vizualisation",
        tabName = "genData",
        icon = icon("database"),
        selected = TRUE
      ),
      menuItem(
        text = "k-Means",
        tabName = "kMeans",
        icon = icon("spinner")
      ),
      menuItem(
        text = "DBSCAN",
        tabName = "DBSCAN",
        icon = icon("spinner")
      )
    )
  ),
  # ----- dashboardBody -----
  body = dashboardBody(
    tabItems(
      # ----- __home -----
      page_home(),
      # ----- __genData -----
      page_genData(),
      # ----- __kMeans -----
      page_kMeans(),
      # ----- __DBSCAN -----
      page_DBSCAN()
    )
  )
)
