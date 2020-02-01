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
            h4("Example of Centroid model"),
            p("Assume that the elements of each cluster are",
              "close from its centroid.")
          ),
          # ----- ____Auto Run -----
          tabPanel(
            title = "Auto Run",
            h4("How many clusters ? Which seed ?"),
            p("In this kMeans version, the algorithm maximize",
              " the silhouette score by selecting the ideal configuration",
              " with the number of expected clusters between 1 and 8",
              " and the seed between 1 and 20.")
          ),
          # ----- ____Manual Run -----
          tabPanel(
            title = "Manual Run",
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
            uiOutput(outputId = "kMeans_info",
                     inline = TRUE)
          ),
          column(
            width = 2,
            uiOutput(outputId = "kMeans_nbCenters")
          ),
          column(
            width = 2,
            uiOutput(outputId = "kMeans_silhouette")
          ),
          column(
            width = 2,
            uiOutput(outputId = "kMeans_seed")
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
              outputId = "kMeans_plot_2"
            )
          )
        ),
        fluidRow(
          column(
            width = 2,
            class = "myCheckbox square",
            checkboxInput(
              inputId = "kMeans_initCenters",
              label = "Initial Centers",
              value = TRUE
            )
          ),
          column(
            width = 4,
            class = "myCheckbox round",
            checkboxInput(
              inputId = "kMeans_finalCenters",
              label = "Final Centers",
              value = TRUE
            )
          ),
          column(
            width = 6,
            radioGroupButtons(
              inputId = "kMeans_plotChoice",
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
