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
            p("Display kMeans steps.")
          ),
          # ----- ____Auto Run -----
          tabPanel(
            title = "Auto Run",
            tabsetPanel(
              id = "kMeans_panel2",
              tabPanel(
                title = "Parameters",
                h4("Selected parameters."),
                fluidRow(
                  uiOutput(
                    outputId = "kMeans_optimalNbCenters"
                  ),
                  uiOutput(
                    outputId = "kMeans_optimalSeed"
                  )
                ),
                h4("How many clusters ?"),
                p("In this kMeans version, the algorithm select",
                  " the number of expected clusters",
                  " by maximizing the silhouette score."),
                p("For each NbCenters bewteen 1 end 8, two indicators",
                  " are computed then averaged on the seeds from 1 to 20."),
                plotOutput(
                  outputId = "kMeans_varHist_A",
                  height = "200px"
                ),
                h4("Which seed ?"),
                p("Once the NbCenters is fixed,",
                  " the algorithm select the seed from 1 to 20",
                  " that maximize the silhouette score.")
              ),
              tabPanel(
                title = "Results",
                h4("Indicators."),
                uiOutput(
                  outputId = "kMeans_silhouette"
                ),
                uiOutput(
                  outputId = "kMeans_variation"
                )
              )
            )
          ),
          # ----- ____Manual Run -----
          tabPanel(
            title = "Manual Run",
            tabPanel(
              title = "Explore",
              sliderTextInput(
                inputId = "kMeans_nbCenters",
                label = "Expected clusters",
                choices = 1:8,
                selected = 3,
                animate = TRUE,
                grid = TRUE
              ),
              sliderTextInput(
                inputId = "kMeans_seed",
                label = "kMeans seed",
                choices = 1:20,
                selected = 1,
                grid = TRUE
              ),
              uiOutput(outputId = "kMeans_silhouetteExplo"),
              uiOutput(outputId = "kMeans_variationExplo"),
              fluidPage(
                plotOutput(
                  outputId = "kMeans_varHist_M",
                  height = "200px"
                ),
                plotOutput(
                  outputId = "kMeans_seedHist_M",
                  height = "200px"
                )
              ),
              sliderTextInput(
                inputId = "kMeans_myIterMax",
                label = "Maximum of iterations",
                choices = 5:50,
                selected = 10,
                grid = TRUE
              )
            )
          )
        )
      ),
      # ----- __main -----
      mainPanel = mainPanel(
        h3(
          textOutput(
            outputId = "kMeans_info"
          )
        ),
        plotOutput(
          height = "600px",
          outputId = "kMeans_plot"
        ),
        fluidRow(
          div(
            style = "display: inline-block;",
            checkboxInput(
              inputId = "kMeans_init",
              label = "Initial clusters",
              value = FALSE
            )
          ),
          div(
            style = "display: inline-block;",
            checkboxInput(
              inputId = "kMeans_displayCenters",
              label = "Display Centers",
              value = TRUE
            )
          )
        )
      )
    )
  )
}
