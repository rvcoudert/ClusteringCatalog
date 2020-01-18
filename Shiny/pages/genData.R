page_genData <- function()
{
  tabItem(
    tabName = "genData",
    sidebarLayout(
      # ----- __sidebar -----
      sidebarPanel = sidebarPanel(
        fluidPage(
          titlePanel(
            h1("Génération des données"),
          ),
          p("Points could be generated in several ways."),
          h4("Blobs"),
          p("Points are spread by blobs in the plan."),
          h4("Moons"),
          p("Points take shape into two criss-crossed moons."),
          h4("Concentric circles"),
          p("Point take shape into two circles nesting into each other."),
          p("Interlinking is interesting because it's a common",
            "difficulty encountered by seperation method.")
        ),
        fluidPage(
          style = "font-style: italic;",
          titlePanel(
            h1("Génération des données"),
          ),
          p("Les points peuvent être générés suivant plusieurs méthodes."),
          h4("Blobs"),
          p("Des paquets sont disséminés dans le plan."),
          h4("Moons"),
          p("Deux croissants de lunes s'entrecroisent."),
          h4("Concentric circles"),
          p("Deux cercles concentriques s'imbriquent l'un dans l'autre."),
          p("L'entrecroisement est intéressant car il s'agit d'une difficulté",
            "commune rencontrée par les méthodes de séparation.")
        )
      ),
      # ----- __main -----
      mainPanel = mainPanel(
        h3(
          textOutput(
            outputId = "genData_info"
          )
        ),
        fluidRow(
          column(
            width = 6,
            plotOutput(
              height = "600px",
              outputId = "genData_plot",
              brush = brushOpts(id = "genData_brush", resetOnNew = FALSE)
            )
          ),
          column(
            width = 6,
            plotOutput(
              height = "600px",
              outputId = "genData_plot_2"
            )
          )
        )
      )
    )
  )
}
