page_DBSCAN <- function()
{
  tabItem(
    tabName = "DBSCAN",
    sidebarLayout(
      # ----- __sidebar -----
      sidebarPanel = sidebarPanel(
        titlePanel(
          h1("DBSCAN")
        ),
        tabsetPanel(
          id = "DBSCAN_panel1",
          # ----- ____Lean -----
          tabPanel(
            title = "Learn",
            p("Explanations about DBSCAN algorithm."),
            p("Display DBSCAN steps.")
          ),
          # ----- ____Auto Run -----
          tabPanel(
            title = "Auto Run",
            h4("Default parameters."),
            p("eps = 1"),
            p("minPts = 5"),
            p("How to define density ? How to define minPts ?")
          ),
          # ----- ____Manual Run -----
          tabPanel(
            title = "Manual Run",
            h4("Manual exploring"),
            sliderTextInput(
              inputId = "DBSCAN_eps",
              label = "Epsilon",
              choices = 1:50 / 10,
              selected = 1,
              grid = TRUE
            ),
            sliderTextInput(
              inputId = "DBSCAN_minPoints",
              label = "minPoints",
              choices = 1:20,
              selected = 5,
              grid = TRUE
            )
          )
        )
      ),
      # ----- __main -----
      mainPanel = mainPanel(
        fluidRow(
          id = "bodyTitle",
          column(
            width = 2,
            uiOutput(outputId = "DBSCAN_info",
                     inline = TRUE)
          ),
          column(
            width = 2,
            uiOutput(outputId = "DBSCAN_nbClusters")
          ),
          column(
            width = 2,
            uiOutput(outputId = "DBSCAN_silhouette")
          ),
          column(
            width = 2
          )
        ),
        fluidRow(
          column(
            width = 6,
            plotOutput(
              height = "500px",
              outputId = "DBSCAN_plot",
              brush = brushOpts(id = "DBSCAN_brush", resetOnNew = FALSE)
            )
          ),
          column(
            width = 6,
            plotOutput(
              height = "500px",
              outputId = "DBSCAN_plot_2",
              brush = brushOpts(id = "DBSCAN_brush_2")
            )
          )
        ),
        fluidRow(
          column(
            width = 2,
            class = "myCheckbox square",
            checkboxInput(
              inputId = "DBSCAN_borderPoints",
              label = "Border Points",
              value = FALSE
            )
          ),
          column(
            width = 4,
            class = "myCheckbox square",
            checkboxInput(
              inputId = "DBSCAN_density",
              label = "Density",
              value = FALSE
            )
          ),
          column(
            width = 4,
            class = "myCheckbox square",
            fluidRow(
              column(
                width = 2,
                p("Zoom")
              ),
              column(
                width = 2,
                materialSwitch(inputId = "DBSCAN_init", value = TRUE)
              ),
              column(
                width = 8,
                p("Initial Clusters")
              )
            )
          )
        )
      )
    )
  )
}
