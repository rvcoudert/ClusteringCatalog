page_genData <- function()
{
  tabItem(
    tabName = "genData",
    sidebarLayout(
      # ----- __sidebar -----
      sidebarPanel = sidebarPanel(
        fluidPage(
          titlePanel(
            h1("Data Generation"),
          ),
          p("Points could be generated in several ways."),
          tags$ul(
            style = "font-style: italic;",
            tags$li("nSamples: Number of points in the Data."),
            tags$li("Cluster Dispersion: Dispersion of each cluster."),
            tags$li("Dispersion Variation:",
                    "Difference of variations between clusters."),
            tags$li("Data Seed: Seed to control randomness.")
          ),
          h4("Blobs"),
          p("Points are spread by blobs in the plan."),
          tags$ul(
            style = "font-style: italic;",
            tags$li("Blobs number: Number of blobs to get."),
            tags$li("Center Min Dist: Minimum distance between centers.")
          ),
          h4("Moons"),
          p("Points take shape into two criss-crossed moons."),
          h4("Concentric circles"),
          p("Point take shape into two circles nesting into each other."),
          tags$ul(
            style = "font-style: italic;",
            tags$li("ScaleFactor: Radius difference between the two circles.")
          ),
          p("Interlinking is interesting because it's a common",
            "difficulty encountered by seperation method.")
        )
        # fluidPage(
        #   style = "font-style: italic;",
        #   titlePanel(
        #     h1("Génération des données"),
        #   ),
        #   p("Les points peuvent être générés suivant plusieurs méthodes."),
        #   h4("Blobs"),
        #   p("Des paquets sont disséminés dans le plan."),
        #   h4("Moons"),
        #   p("Deux croissants de lunes s'entrecroisent."),
        #   h4("Concentric circles"),
        #   p("Deux cercles concentriques s'imbriquent l'un dans l'autre."),
        #   p("L'entrecroisement est intéressant car il s'agit d'une difficulté",
        #     "commune rencontrée par les méthodes de séparation.")
        # )
      ),
      # ----- __main -----
      mainPanel = mainPanel(
        fluidRow(
          id = "bodyTitle",
          column(
            width = 2,
            uiOutput(outputId = "genData_info")
          ),
          column(
            width = 2
          ),
          column(
            width = 2,
            uiOutput(outputId = "genData_silhouette")
          )
        ),
        fluidRow(
          column(
            width = 6,
            plotOutput(
              height = "500px",
              outputId = "genData_plot",
              brush = brushOpts(id = "genData_brush", resetOnNew = FALSE)
            )
          ),
          column(
            width = 6,
            plotOutput(
              height = "500px",
              outputId = "genData_plot_2"
            )
          )
        )
      )
    )
  )
}
