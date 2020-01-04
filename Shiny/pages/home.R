page_home <- function()
{
  tabItem(
    tabName = "home",
    fluidPage(
      titlePanel(
        h1("Welcome in the Clustering Catalog."),
      ),
      p("In Data Science, Clustering a data set means",
        "gathering similar data together."),

      p("In this Shiny app, the data set is simply some points in the plan",
        "generated in several ways. ",
        "Then different clustering methods are avaible to divide them."),
      h2("Instructions"),
      p("Select a clustering mode in the menu opposite.")
    ),
    fluidPage(
      style = "font-style: italic;",
      titlePanel(
        h1("Bienvenue dans le catalogue du clustering."),
      ),
      p("Le clustering consiste à regrouper les données similaires",
        " d'un ensemble de données."),
      p("Dans ce site web, vous pourrez générer des points dans le plan",
        "selon plusieurs méthodes et ensuite les regrouper via différentes",
        "techniques de clustering."),
      h2("Mode d'emploi."),
      p("Sélectionner un mode de clustering dans le menu ci-contre.")
    )
  )
}

