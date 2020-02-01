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
      p("A lot of methods could be used. Each of them have their pro and cons."),
      h2("Reflexion"),
      p("Clustering is unsupervised learning.",
        "Actually, when a Data Scientist analyse a new dataset,",
        "the organisation of the information is unknown.",
        "The number of clusters, their positionning, their space,",
        "all of that remain undiscovered.",
        "Even the existence of any cluster is uncertain."),
      p("That's why clustering is rather delicate to process,",
        "as each unsupervised learning algorithm,",
        "because it's quite hard to evaluate",
        "the performance of the repartition"),
      p("For example, the silhouette score currently used to optimize",
        "the kMeans algorithm works only if the clusters are convex.",
        "Even if it's effective with blobs,",
        "it remains inadequate with moons and circles."),
      h2("Two types of results"),
      tags$ul(
        tags$li(strong("Hard Clustering : "),
                "Each data point belongs to an unique cluster."),
        tags$li(strong("Soft Clustering : "),
                "Each data point is associated with a probability",
                "or a likehood to be in each cluster.")
      ),
      h2("Several kind of algorithms"),
      tags$ul(
        tags$li(strong("Centroid models : "),
                tags$em("(k-Means) "),
                "Assume that the elements of each cluster are",
                "close from its centroid."),
        tags$li(strong("Connectivity models : "),
                tags$em("(HDBSCAN) "),
                "Assume that the elements of each cluster are",
                "closer than from the elements of other clusters."),
        tags$li(strong("Density Models : "),
                tags$em("(Hierarchical) "),
                "Assume that the elements of each cluster are",
                "recognizable as density areas of the space."),
        tags$li(strong("Distribution models : "),
                tags$em("(Model-Based) "),
                "Assume that the elements of each cluster are",
                "distributed following some known distribution,",
                "most of the time gaussian.")
      )
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

