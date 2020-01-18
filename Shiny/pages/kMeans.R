page_kMeans <- function()
{
  tabItem(
    tabName = "kMeans",
    sidebarLayout(
      # ----- __sidebar -----
      sidebarPanel = sidebarPanel(
        titlePanel(
          h1("k-Means")
        ),
        tabsetPanel(
          id = "kMeans_panel1",
          # ----- ____Lean -----
          tabPanel(
            title = "Learn",
            p("Explanations about kMeans algorithm."),
            p("Explanations about variation handled and silhouette score"),
            p("Display kMeans steps.")
          ),
          # ----- ____Auto Run -----
          tabPanel(
            title = "Auto Run",
            h4("Selected parameters."),
            fluidRow(
              uiOutput(
                outputId = "kMeans_optimalNbCenters"
              ),
              uiOutput(
                outputId = "kMeans_optimalSeed"
              )
            ),
            h4("How many clusters ? Which seed ?"),
            p("In this kMeans version, the algorithm maximize",
              " the silhouette score by selecting the ideal configuration",
              " with the number of expected clusters between 1 and 8",
              " and the seed between 1 and 20."),
            h4("Silhouette score VS nbCenters / kMeans_seed"),
            plotOutput(
              outputId = "kMeans_heatMap_sil_A",
              height = "400px"
            )
          ),
          # ----- ____Manual Run -----
          tabPanel(
            title = "Manual Run",
            tabPanel(
              title = "Explore",
              h4("Manual exploring"),
              sliderTextInput(
                inputId = "kMeans_nbCenters",
                label = "Expected clusters",
                choices = 1:8,
                selected = 3,
                grid = TRUE
              ),
              sliderTextInput(
                inputId = "kMeans_seed",
                label = "kMeans seed",
                choices = 1:20,
                selected = 1,
                animate = TRUE,
                grid = TRUE
              ),
              sliderTextInput(
                inputId = "kMeans_myIterMax",
                label = "Maximum of iterations",
                choices = 1:20,
                selected = 10,
                grid = TRUE
              ),
              fluidPage(
                h4("Silhouette score VS nbCenters / kMeans_seed"),
                plotOutput(
                  outputId = "kMeans_heatMap_sil_M",
                  height = "400px"
                )
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
            h3(textOutput(outputId = "kMeans_info"))
          ),
          column(
            width = 4,
            uiOutput(outputId = "kMeans_silhouette")
          ),
          column(
            width = 4,
            uiOutput(outputId = "kMeans_variation")
          )
        ),
        fluidRow(
          column(
            width = 6,
            plotOutput(
              height = "500px",
              outputId = "kMeans_plot",
              brush = brushOpts(id = "kMeans_brush", resetOnNew = FALSE)
            )
          ),
          column(
            width = 6,
            plotOutput(
              height = "500px",
              outputId = "kMeans_zoom"
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            class = "myCheckbox square",
            checkboxInput(
              inputId = "kMeans_init",
              label = "Initial Data",
              value = FALSE
            )
          ),
          column(
            width = 4,
            class = "myCheckbox square",
            checkboxInput(
              inputId = "kMeans_initCenters",
              label = "Initial Centers",
              value = TRUE
            )
          ),
          column(
            width = 4,
            class = "myCheckbox square",
            checkboxInput(
              inputId = "kMeans_finalCenters",
              label = "Final Centers",
              value = TRUE
            )
          )
        )
      )
    )
  )
}
