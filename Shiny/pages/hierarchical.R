page_hierarchical <- function()
{
  tabItem(
    tabName = "hierarchical",
    sidebarLayout(
      # ----- __sidebar -----
      sidebarPanel = sidebarPanel(
        titlePanel(
          h1("Hierarchical clustering")
        ),
        tabsetPanel(
          id = "hierarchical_panel1",
          # ----- ____Lean -----
          tabPanel(
            title = "Learn",
            h4("Example of Connectivity model"),
            p("Assume that the elements of each cluster are",
              "closer than from the elements of other clusters.")
          ),
          # ----- ____Auto Run -----
          tabPanel(
            title = "Auto Run",
            h4("Selected parameters.")
            # fluidRow(
            #   uiOutput(
            #     outputId = "kMeans_optimalNbCenters"
            #   ),
            #   uiOutput(
            #     outputId = "kMeans_optimalSeed"
            #   )
            # )
          ),
          # ----- ____Manual Run -----
          tabPanel(
            title = "Manual Run",
            h4("Manual exploring"),
            radioGroupButtons(
              inputId = "hierarchical_method",
              label = "Method",
              choices = list(
                Single = "single",
                Average = "average",
                Median = "median",
                Complete = "complete",
                Centroid = "centroid",
                Ward = "ward.D2"),
              direction = "vertical",
              justified = TRUE,
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle",
                             style = "color: #d73925"),
                no = tags$i(class = "fa fa-circle-o",
                            style = "color: white"))
            ),
            materialSwitch(
              inputId = "hierarchical_nbClustersAuto",
              label = "Auto find Expected clusters",
              value = TRUE
            ),
            uiOutput(outputId = "hierarchical_nbClustersAutoUI"),
            selected = TRUE
          )
        )
      ),
      # ----- __main -----
      mainPanel = mainPanel(
        fluidRow(
          id = "bodyTitle",
          column(
            width = 2,
            uiOutput(outputId = "hierarchical_info")
          ),
          column(
            width = 2,
            uiOutput(outputId = "hierarchical_nbClusters")
          ),
          column(
            width = 2,
            uiOutput(outputId = "hierarchical_silhouette")
          ),
          column(
            width = 2,
            uiOutput(outputId = "hierarchical_method")
          )
        ),
        fluidRow(
          column(
            width = 6,
            plotOutput(
              height = "500px",
              outputId = "hierarchical_plot",
              brush = brushOpts(id = "hierarchical_brush", resetOnNew = FALSE)
            )
          ),
          column(
            width = 6,
            plotOutput(
              height = "500px",
              outputId = "hierarchical_plot_2"
            )
          )
        ),
        fluidRow(
          column(
            width = 2,
            class = "myCheckbox square",
            checkboxInput(
              inputId = "hierarchical_1",
              label = "hierarchical_1",
              value = TRUE
            )
          ),
          column(
            width = 4,
            class = "myCheckbox square",
            checkboxInput(
              inputId = "hierarchical_2",
              label = "hierarchical_2",
              value = TRUE
            )
          ),
          column(
            width = 6,
            radioGroupButtons(
              inputId = "hierarchical_plotChoice",
              label = "Plot Choice",
              choices = c(
                "Initial Clusters" = "init",
                "Zoom" = "zoom",
                "Index Heatmap" = "heatmap"
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
