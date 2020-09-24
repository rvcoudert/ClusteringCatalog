# List of required packages.
list.of.packages <- c(
  "shiny",
  "shinyWidgets",
  "shinydashboard",
  "shinydashboardPlus",
  "magrittr",
  "ggplot2",
  "dplyr",
  "magrittr",
  "plyr",
  "cluster",
  "reshape2",
  "NbClust",
  "mclust",
  "ggforce",
  "dbscan")

# Check missing packages from list.
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[
    ,"Package"])]

# Install missing ones.
if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)

library(magrittr)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
# Needed for model-based clustering.
library(mclust)

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


# Starter Data.
starter.n_samples <- 500
starter.centers <- 6
starter.cluster_sd <- 1
starter.cluster_sd_var <- 0.5
starter.cluster_min_dist <- 1
starter.seed <- 2


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
    # fixed = TRUE,
    left_menu = tagList(
      dropdownButton(
        tooltip = tooltipOptions(title = "Data Parameters"),
        size = "sm",
        icon =  icon("sliders"),
        h4("Data Generation"),
        fluidRow(
          column(
            width = 2,
            "Custom"
          ),
          column(
            width = 2,
            materialSwitch(inputId = "genData_preset")
          ),
          column(
            width = 2,
            "Preset"
          )
        ),
        uiOutput(
          outputId = "header_data"
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
        icon = icon("home"),
        selected = TRUE
      ),
      menuItem(
        text = "Data Vizualisation",
        tabName = "genData",
        icon = icon("database")
      ),
      menuItem(
        text = "k-Means",
        tabName = "kMeans",
        icon = icon("spinner")
      ),
      menuItem(
        text = "HDBSCAN",
        tabName = "HDBSCAN",
        icon = icon("spinner")
      ),
      menuItem(
        text = "Hierarchical",
        tabName = "hierarchical",
        icon = icon("spinner")
      ),
      menuItem(
        text = "Model-Based",
        tabName = "modelbased",
        icon = icon("spinner")
      )
    )
  ),
  # ----- dashboardBody -----
  body = dashboardBody(
    tabItems(
      page_home(),
      page_genData(),
      page_kMeans(),
      page_HDBSCAN(),
      page_hierarchical(),
      page_modelbased()
    )
  )
)
