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
            tabPanel(
              title = "Explore",
              h4("Manual exploring"),
              numericInput(
                inputId = "DBSCAN_eps",
                label = "Epsilon",
                min = 0,
                step = 0.1,
                max = 10,
                value =  1
              ),
              numericInput(
                inputId = "DBSCAN_minPoints",
                label = "minPoints",
                min = 2,
                step = 1,
                max = 20,
                value =  5
              )
            )
          )
        )
      ),
      # ----- __main -----
      mainPanel = mainPanel(
        fluidRow(
          column(
            width = 4,
            h3(textOutput(outputId = "DBSCAN_info"))
          ),
          column(
            width = 4,
            uiOutput(outputId = "DBSCAN_silhouette")
          ),
          column(
            width = 4,
            uiOutput(outputId = "DBSCAN_variation")
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
                materialSwitch(inputId = "DBSCAN_init")
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
