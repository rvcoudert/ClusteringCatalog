page_HDBSCAN <- function()
{
  tabItem(
    tabName = "HDBSCAN",
    sidebarLayout(
      # ----- __sidebar -----
      sidebarPanel = sidebarPanel(
        titlePanel(
          h1("HDBSCAN")
        ),
        tabsetPanel(
          id = "HDBSCAN_panel1",
          # ----- ____Lean -----
          tabPanel(
            title = "Learn",
            h4("Example of Density model"),
            p("Assume that the elements of each cluster are",
              "recognizable as density areas of the space.")
          ),
          # ----- ____Auto Run -----
          tabPanel(
            title = "Auto Run",
            h4("Default parameters."),
            p("minPts = 20")
          ),
          # ----- ____Manual Run -----
          tabPanel(
            title = "Manual Run",
            h4("Manual exploring"),
            numericInput(
              inputId = "HDBSCAN_minPts",
              label = "minPts",
              min = 5,
              max = 500,
              step = 5,
              value = 20
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
            uiOutput(outputId = "HDBSCAN_info",
                     inline = TRUE)
          ),
          column(
            width = 2,
            uiOutput(outputId = "HDBSCAN_nbClusters")
          ),
          column(
            width = 2,
            uiOutput(outputId = "HDBSCAN_silhouette")
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
              outputId = "HDBSCAN_plot",
              brush = brushOpts(id = "HDBSCAN_brush", resetOnNew = FALSE)
            )
          ),
          column(
            width = 6,
            plotOutput(
              height = "500px",
              outputId = "HDBSCAN_plot_2",
              brush = brushOpts(id = "HDBSCAN_brush_2")
            )
          )
        ),
        fluidRow(
          column(
            width = 2,
            class = "myCheckbox square",
            checkboxInput(
              inputId = "HDBSCAN_borderPoints",
              label = "Border Points",
              value = FALSE
            )
          ),
          column(
            width = 4,
            class = "myCheckbox square",
            checkboxInput(
              inputId = "HDBSCAN_density",
              label = "Density",
              value = FALSE
            )
          ),
          column(
            width = 6,
            radioGroupButtons(
              inputId = "HDBSCAN_plotChoice",
              label = "Plot Choice",
              choices = c(
                "Initial Clusters" = "init",
                "Zoom" = "zoom"
              ),
              selected = "init",
              checkIcon = list(
                yes = tags$i(class = "fa fa-check-square",
                             style = "color: #d73925"),
                no = tags$i(class = "fa fa-square",
                            style = "color: #d73925")
              )
            )
          )
        )
      )
    )
  )
}
