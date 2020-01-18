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
            h4("Selected parameters."),
            # fluidRow(
            #   uiOutput(
            #     outputId = "DBSCAN_optimalNbCenters"
            #   ),
            #   uiOutput(
            #     outputId = "DBSCAN_optimalSeed"
            #   )
            # ),
            h4("exp ? minPts ?"),
            p("How to define density ?")
          ),
          # ----- ____Manual Run -----
          tabPanel(
            title = "Manual Run",
            tabPanel(
              title = "Explore",
              h4("Manual exploring"),
              numericInput(
                inputId = "DBSCAN_eps",
                label = "Epsilon",,
                min = 0,
                step = 0.1,
                max = 1,
                value =  0.5
              ),
              numericInput(
                inputId = "DBSCAN_minPoints",
                label = "minPoints",
                min = 2,
                step = 1,
                max = 10,
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
        plotOutput(
          height = "600px",
          outputId = "DBSCAN_plot"
        ),
        fluidRow(
          column(
            width = 4,
            class = "myCheckbox square",
            checkboxInput(
              inputId = "DBSCAN_init",
              label = "Initial Data",
              value = FALSE
            )
          ),
          column(
            width = 4,
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
          )
        )
      )
    )
  )
}
