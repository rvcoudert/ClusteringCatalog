page_home <- function()
{
  tabItem(
    tabName = "home",
    fluidPage(
      titlePanel(
        h1("Welcome in the Clustering Catalog."),
      ),
      p("In Data Science, clustering a data set means",
        "gathering groups of similar data together."),

      p("In this Shiny app, the data set is simply some points in the plan",
        "generated in several ways. ",
        "Then different clustering methods are avaible to divide up them."),
      h2("Instructions"),
      p("Select a clustering mode in the menu opposite."),
      p("A lot of methods could be used. Eah of them are their pro and cons."),
      h2("Reflexion"),
      p("Clustering is unsupervised learning.",
        "Actually, when a Data Scientist analyse a new dataset",
        "the organisation of the information is unknown.",
        "The number of clusters, their positionning, their space,",
        "all of that remain undiscovered.",
        "Even the existence of any cluster is uncertain."),
      p("That's why clustering is rather delicate to process,",
        "as each unsupervised learning algorithm,",
        "because it's quite hard to evaluate",
        "the performance of the classification."),
      p("For example, the silhouette score currently used to optimize",
        "the kMeans algorithm works only if the clusters are convex.",
        "Even if it's effective with blobs,",
        "it remains inadequate with moons and circles.")
    )
    # fluidPage(
    #   style = "font-style: italic;",
    #   titlePanel(
    #     h1("Bienvenue dans le catalogue du clustering."),
    #   ),
    #   p("Le clustering consiste à regrouper les données similaires",
    #     " d'un ensemble de données."),
    #   p("Dans ce site web, vous pourrez générer des points dans le plan",
    #     "selon plusieurs méthodes et ensuite les regrouper via différentes",
    #     "techniques de clustering."),
    #   h2("Mode d'emploi."),
    #   p("Sélectionner un mode de clustering dans le menu ci-contre.")
    # )
  )
}

