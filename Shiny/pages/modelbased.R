page_modelbased <- function()
{
  tabItem(
    tabName = "modelbased",
    sidebarLayout(
      # ----- __sidebar -----
      sidebarPanel = sidebarPanel(
        titlePanel(
          h1("Model-Based clustering")
        ),
        tabsetPanel(
          id = "modelbased_panel1",
          # ----- ____Lean -----
          tabPanel(
            title = "Learn",
            h4("Example of Distribution model"),
            p("Assume that the elements of each cluster are",
              "distributed following some known distribution,",
              "most of the time gaussian.")
          ),
          # ----- ____Auto Run -----
          tabPanel(
            title = "Auto Run",
            h4("Selected parameters.")
          ),
          # ----- ____Manual Run -----
          tabPanel(
            title = "Manual Run",
            h4("Manual exploring")
          )
        )
      ),
      # ----- __main -----
      mainPanel = mainPanel(
        fluidRow(
          id = "bodyTitle",
          column(
            width = 2,
            uiOutput(outputId = "modelbased_info")
          ),
          column(
            width = 2,
            uiOutput(outputId = "modelbased_nbClusters")
          ),
          column(
            width = 2,
            uiOutput(outputId = "modelbased_silhouette")
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
              outputId = "modelbased_plot",
              brush = brushOpts(id = "modelbased_brush", resetOnNew = FALSE)
            )
          ),
          column(
            width = 6,
            plotOutput(
              height = "500px",
              outputId = "modelbased_plot_2"
            )
          )
        ),
        fluidRow(
          column(
            width = 2,
            class = "myCheckbox square",
            checkboxInput(
              inputId = "modelbased_centers",
              label = "Centers",
              value = TRUE)
          ),
          column(
            width = 4,
            class = "myCheckbox square",
            checkboxInput(
              inputId = "modelbased_ellipses",
              label = "Ellipses",
              value = TRUE)
          ),
          column(
            width = 6,
            radioGroupButtons(
              inputId = "modelbased_plotChoice",
              label = "Plot Choice",
              choices = c(
                "Initial Clusters" = "init",
                "Zoom" = "zoom",
                "Classification" = "classification",
                "Uncertainty" = "uncertainty",
                "Density" = "density"
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
