# ----- preprocess -----


make_blobs <- function(
  n_samples = 100,
  centers = 3,
  cluster_sd = 1,
  center_limits = c(-20, 20),
  min_dist = 0,
  noise = FALSE,
  seed = 0
) {
  set.seed(seed)

  # Récupération et vérification des entrées.
  nb_centers <- centers

  # Générations des centres.
  i <- 0
  iteration_limit <- 500
  generated_min_dist <- 0
  repeat {
    i <- i + 1
    centers_points <- data.frame(
      x = runif(n = nb_centers, min = center_limits[[1]], max = center_limits[[2]]),
      y = runif(n = nb_centers, min = center_limits[[1]], max = center_limits[[2]])
    )

    if (nb_centers == 1) {
      break()
    }

    generated_min_dist <- dist(
      x = centers_points,
      method = "euclidean"
    ) %>% min()

    if (generated_min_dist >= min_dist | i >= iteration_limit) {
      break()
    }
  }

  # Effectifs des clusters.
  cluster_freq <- rep(n_samples %/% nb_centers, nb_centers)
  rest <- (n_samples %% nb_centers)
  if (rest > 0) {
    cluster_freq[1:rest] <-
      cluster_freq[1:rest] + 1
  }
  cluster_info <- data.frame(
    cluster = 1:length(cluster_freq),
    freq = cluster_freq
  ) %>% cbind(centers_points)

  # Génération des points.
  cluster_points <- cluster_info %>%
    dplyr::select(-freq) %>%
    dplyr::slice(rep(cluster_info$cluster, cluster_info$freq))
  set.seed(seed)
  cluster_points <- cluster_points %>%
    dplyr::mutate(x = x + rnorm(n = n_samples, mean = 0, sd = cluster_sd)) %>%
    dplyr::mutate(y = y + rnorm(n = n_samples, mean = 0, sd = cluster_sd))

  return(cluster_points)
}


make_moons <- function(
  n_samples = 200,
  cluster_sd = 1,
  noise = TRUE,
  seed = 0
) {
  # Récupération des entrées.
  nb_centers <- 2

  # Effectifs des lunes.
  cluster_freq <- rep(n_samples %/% nb_centers, nb_centers)
  rest <- (n_samples %% nb_centers)
  if (rest > 0) {
    cluster_freq[1:rest] <-
      cluster_freq[1:rest] + 1
  }

  # Génération des points.
  set.seed(seed)
  points_1 <- runif(
    n = cluster_freq[[1]],
    min = 0,
    max = 1
  )
  points_1 <- data.frame(
    cluster = 1,
    x = 10*cospi(points_1),
    y = 10*sinpi(points_1)
  )

  set.seed(seed)
  points_2 <- runif(
    n = cluster_freq[[1]],
    min = -1,
    max = 0
  )
  points_2 <- data.frame(
    cluster = 2,
    x = 10*cospi(points_2) + 10,
    y = 10*sinpi(points_2)
  )

  cluster_points <- rbind(
    points_1,
    points_2
  )
  set.seed(seed)
  cluster_points <- cluster_points %>%
    dplyr::mutate(x = x + rnorm(n = n_samples, mean = 0, sd = cluster_sd)) %>%
    dplyr::mutate(y = y + rnorm(n = n_samples, mean = 0, sd = cluster_sd))
}


make_circles <- function(
  n_samples = 200,
  cluster_sd = 1,
  scale = 0.6,
  noise = TRUE,
  seed = 0
) {
  # Récupération des entrées.
  nb_centers <- 2

  # Effectifs des cercles.
  cluster_freq <- rep(n_samples %/% nb_centers, nb_centers)
  rest <- (n_samples %% nb_centers)
  if (rest > 0) {
    cluster_freq[1:rest] <-
      cluster_freq[1:rest] + 1
  }

  # Génération des points.
  set.seed(seed)
  points_1 <- runif(
    n = cluster_freq[[1]],
    min = 0,
    max = 2
  )
  points_1 <- data.frame(
    cluster = 1,
    x = 20 * cospi(points_1),
    y = 20 * sinpi(points_1)
  )

  set.seed(seed)
  points_2 <- runif(
    n = cluster_freq[[1]],
    min = 0,
    max = 2
  )
  points_2 <- data.frame(
    cluster = 2,
    x = scale * 20 * cospi(points_2),
    y = scale * 20 * sinpi(points_2)
  )

  cluster_points <- rbind(
    points_1,
    points_2
  )
  set.seed(seed)
  cluster_points <- cluster_points %>%
    dplyr::mutate(x = x + rnorm(n = n_samples, mean = 0, sd = cluster_sd)) %>%
    dplyr::mutate(y = y + rnorm(n = n_samples, mean = 0, sd = cluster_sd))
}


plot_clusters <- function(clusters_points) {
  p <- ggplot2::ggplot(
    data = clusters_points
  ) + ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = x,
      y = y,
      colour = as.factor(cluster)
    ),
    size = 3
  ) + ggplot2::coord_fixed(
  ) + ggplot2::scale_color_brewer(
    palette = "Dark2"
  ) + ggplot2::theme_bw(
    base_size = 16
  ) + ggplot2::theme(
    legend.position = "none",
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )

  return(p)
}


function(input, output) {

  # ----- reac_expr -----

  # Ici, on centralise les appels à certains <input>
  # utilisés à plusieurs endroits.

  # ----- __seed -----

  get_seed <- reactive({
    seed <- input$seed
    if (!is.numeric(seed)) {
      seed <- 0
    }
    return(as.numeric(seed))
  })

  # ----- __clusters_data -----

  get_clusters_data <- reactive({
    nSamples <- input$genData_nSamples
    clusterSd <- input$genData_clusterSd

    if (is.null(nSamples) | is.null(clusterSd) |
        is.na(nSamples) | is.na(clusterSd)) {
      return(NULL)
    }

    if (input$genData_method == "blobs") {
      nb_clusters <- input$genData_blobsNbClusters
      if (is.null(nb_clusters)) {
        return(NULL)
      }
      if (input$genData_blobsMinDist == TRUE) {
        myMinDist <- clusterSd * 5
      } else {
        myMinDist <- 0
      }
      clusters_data <- make_blobs(
        n_samples = nSamples,
        centers = nb_clusters,
        cluster_sd = clusterSd,
        min_dist = myMinDist,
        seed = get_seed())
    }

    if (input$genData_method == "moons") {
      clusters_data <- make_moons(
        n_samples = nSamples,
        cluster_sd = clusterSd,
        seed = get_seed())
    }

    if (input$genData_method == "circles")
    {
      scale <- input$genData_circlesScale
      if (is.null(scale)) {
        return(NULL)
      }
      clusters_data <- make_circles(
        n_samples = nSamples,
        cluster_sd = clusterSd,
        scale = scale,
        seed = get_seed())
    }

    return(clusters_data)
  })

  # ----- genData -----

  # ----- __clusters_info -----

  get_clusters_info <- reactive({
    nbClusters <- input$genData_blobsNbClusters
    method <- input$genData_method

    if (is.null(nbClusters)) {
      return("")
    }

    if (nbClusters == 1) {
      cluster_string <- "1 cluster"
    } else {
      cluster_string <- paste(input$genData_blobsNbClusters, "clusters")
    }

    return(paste(
      cluster_string,
      " (",
      method,
      ")",
      sep = ""
    ))
  })

  # ----- __clusters_plot -----

  get_clusters_plot <- reactive({
    clusters_data <- get_clusters_data()
    if (is.null(clusters_data)) {
      return(plot.new())
    }

    p <- plot_clusters(
      clusters_points = clusters_data)

    return(p)
  })

  # ----- kMeans -----

  # ----- __indic -----

  get_kMeans_indic <- reactive({
    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) {
      return(NULL)
    }

    # Les indicateurs sont calculées
    #   pour le nombre de clusters attendus variant de 1 à 8
    #   et pour la graine variant de 1 à 20.
    indic_df <- plyr::ldply(1:8, function(nbCenters) {
      if (nbCenters == 1) {
        data.frame(
          nbCenters.col = 1,
          kMeans_seed.col = 0,
          silhouette_score.col = 0,
          handled_variation.col = 0
        )
      } else {
        # Pour les graines de 1 à 20,
        #    on calcule le score de silhouette et la variation considérée.
        indic_on_several_seeds <- plyr::ldply(1:20, function(kMeans_seed) {
          set.seed(kMeans_seed)
          kmeans_result <- clusters_data %>%
            dplyr::select(x, y) %>%
            kmeans(centers = nbCenters)

          silhouette_summary <- cluster::silhouette(
            kmeans_result$cluster,
            clusters_data %>%
              dplyr::select(x, y) %>%
              dist()
          ) %>% summary()
          silhouette_mean <- silhouette_summary$si.summary[["Mean"]]
          handled_variation <- kmeans_result$betweenss %>%
            magrittr::divide_by(kmeans_result$totss)
          data.frame(
            nbCenters.col = nbCenters,
            kMeans_seed.col = kMeans_seed,
            silhouette_score.col = silhouette_mean,
            handled_variation.col = handled_variation
          )
        })
      }
    })
  })

  # ----- __summary -----

  get_kMeans_summary <- reactive({
    indic_df <- get_kMeans_indic()

    if (is.null(indic_df)) {
      return(NULL)
    }
    indic_summary <- indic_df %>%
      dplyr::group_by(nbCenters.col) %>%
      dplyr::summarise(
        silhouette_score.col = mean(silhouette_score.col),
        handled_variation.col = mean(handled_variation.col))
  })

  # ----- __optimalNbCenters -----
  get_kMeans_optimalNbCenters <- reactive({
    indic_summary <- get_kMeans_summary()

    if (is.null(indic_summary)) {
      return(NULL)
    }

    # On choisit le nombre étudié de clusters attendus,
    #   puis sélectionne la meilleure graine suivant le score de silhouette.
    indic_summary %>%
      dplyr::filter(silhouette_score.col == max(silhouette_score.col)) %>%
      dplyr::select(nbCenters.col) %>%
      head(1) %>% unlist() %>% unname()
  })


  # ----- __optimalSeed -----
  get_kMeans_optimalSeed <- reactive({
    nbCenters <-  get_kMeans_optimalNbCenters()

    if (is.null(nbCenters)) {
      return(NULL)
    }

    indic_df <- get_kMeans_indic()

    # On choisit le nombre étudié de clusters attendus,
    #   puis on sélectionne la meilleure graine suivant le score de silhouette.
    indic_df %>%
      dplyr::filter(nbCenters.col == nbCenters) %>%
      dplyr::filter(silhouette_score.col == max(silhouette_score.col)) %>%
      dplyr::select(kMeans_seed.col) %>%
      head(1) %>% unlist() %>% unname()
  })


  # ----- __varHist_plot -----
  get_varHist_plot <- reactive({
    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) {
      return(plot.new())
    }

    indic_summary <- get_kMeans_summary()

    df_to_plot <- indic_summary %>%
      reshape2::melt(id.vars = "nbCenters.col")

    myMax <- df_to_plot %>%
      dplyr::filter(variable == "silhouette_score.col") %>%
      dplyr::filter(value == max(value)) %>%
      head(1)
    x_myMax <- myMax[["nbCenters.col"]]
    y_myMax <- myMax[["value"]]
    myMax_to_plot <- data.frame(
      x = c(x_myMax, 1),
      xend = c(x_myMax, x_myMax),
      y = c(0, y_myMax),
      yend = c(y_myMax, y_myMax)
    )

    p <- ggplot2::ggplot(
      data = df_to_plot
    ) + ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = nbCenters.col,
        y = value,
        color = variable
      ),
      size = 2
    ) + ggplot2::geom_segment(
      data = myMax_to_plot,
      mapping = ggplot2::aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend),
      linetype = "dashed",
      color = "orangered"
    ) + ggplot2::labs(
      x = "Expected clusters",
      y = ""
    ) + ggplot2::scale_x_continuous(
      breaks = 1:8,
      limits = c(1,8),
      expand = c(0,0)
    ) + ggplot2::scale_y_continuous(
      limits = c(0,1),
      expand = c(0,0)
    ) + ggplot2::scale_colour_manual(
      name = "Indicator",
      limits = c("silhouette_score.col", "handled_variation.col"),
      values = c("orangered", "darkgreen"),
      labels = c("Silhouette Score", "Handled Variation"),
      na.value = "gray75"
    ) + ggplot2::theme_bw(
    ) + ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      legend.position = "bottom"
    )

    return(p)
  })


  # ----- __seedHist_plot -----
  get_seedHist_plot <- reactive({
    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) {
      return(plot.new())
    }

    nbCenters <- input$kMeans_nbCenters

    if (is.null(nbCenters) | nbCenters == 1) {
      return(plot.new())
    } else {
      indic_df <- get_kMeans_indic()

      df_to_plot <- indic_df %>%
        dplyr::filter(nbCenters.col == nbCenters) %>%
        dplyr::select(-nbCenters.col) %>%
        reshape2::melt(id.vars = "kMeans_seed.col")

      myMax <- df_to_plot %>%
        dplyr::filter(variable == "silhouette_score.col") %>%
        dplyr::filter(value == max(value)) %>%
        head(1)
      x_myMax <- myMax[["kMeans_seed.col"]]
      y_myMax <- myMax[["value"]]
      if (x_myMax == 1) {
        myMax_to_plot <- data.frame(
          x = c(x_myMax, 20),
          xend = c(x_myMax, x_myMax),
          y = c(0, y_myMax),
          yend = c(y_myMax, y_myMax)
        )
      } else {
        myMax_to_plot <- data.frame(
          x = c(x_myMax, 1),
          xend = c(x_myMax, x_myMax),
          y = c(0, y_myMax),
          yend = c(y_myMax, y_myMax)
        )
      }

      p <- ggplot2::ggplot(
        data = df_to_plot
      ) + ggplot2::geom_line(
        mapping = ggplot2::aes(
          x = kMeans_seed.col,
          y = value,
          color = variable
        ),
        size = 2
      ) + ggplot2::geom_segment(
        data = myMax_to_plot,
        mapping = ggplot2::aes(
          x = x,
          y = y,
          xend = xend,
          yend = yend),
        linetype = "dashed",
        color = "orangered"
      ) + ggplot2::labs(
        x = "Seed",
        y = ""
      ) + ggplot2::scale_x_continuous(
        breaks = 1:20,
        limits = c(1,20),
        expand = c(0,0)
      ) + ggplot2::scale_y_continuous(
        limits = c(0,1),
        expand = c(0,0)
      ) + ggplot2::scale_colour_manual(
        name = "Indicator",
        limits = c("silhouette_score.col", "handled_variation.col"),
        values = c("orangered", "darkgreen"),
        labels = c("Silhouette Score", "Handled Variation"),
        na.value = "gray75"
      ) + ggplot2::theme_bw(
      ) + ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        legend.position = "bottom"
      )

      return(p)
    }
  })



  # ----- sideBar -----

  output$blobsNbClusters <- renderUI({
    UI <- NULL

    if (input$genData_method == "blobs") {
      UI <- sliderTextInput(
        inputId = "genData_blobsNbClusters",
        label = "Blobs number",
        choices = 1:8,
        selected = 3,
        animate = TRUE,
        grid = TRUE
      )
    }

    return(UI)
  })

  output$blobsMinDist <- renderUI({
    UI <- NULL

    if (input$genData_method == "blobs") {
      UI <- checkboxInput(
        inputId = "genData_blobsMinDist",
        label = "Blobs Min Dist"
      )
    }

    return(UI)
  })

  output$circlesScale <- renderUI({
    UI <- NULL

    if (input$genData_method == "circles") {
      UI <- sliderTextInput(
        inputId = "genData_circlesScale",
        label = "Scale Factor",
        choices = 0:10 / 10,
        selected = 0.6,
        animate = TRUE,
        grid = TRUE
      )
    }

    return(UI)
  })

  # ----- genData -----

  # ----- __info -----
  output$genData_info <- renderText({
    get_clusters_info()
  })

  # ----- __plot -----
  output$genData_plot <- renderPlot({
    get_clusters_plot()
  })


  # ----- kMeans -----

  # ----- __info -----
  output$kMeans_info <- renderText({
    get_clusters_info()
  })

  # ----- __optimalNbCenters -----
  output$kMeans_optimalNbCenters <- renderUI({
    optimalNbCenters <- get_kMeans_optimalNbCenters()
    myTitle <- "Optimal NbCenters"

    if (is.null(optimalNbCenters)) {
      # return("")
      return(infoBox(title = myTitle, value = NULL))
    } else {
      # return(optimalNbCenters)
      valueBox(
        subtitle = myTitle,
        value = optimalNbCenters,
        # icon = icon("sort-numeric-up"),
        color = "black",
        width = 6
      )
    }
  })

  # ----- __optimalSeed -----
  output$kMeans_optimalSeed <- renderUI({
    optimalSeed <- get_kMeans_optimalSeed()
    myTitle <- "Optimal Seed"

    if (is.null(optimalSeed)) {
      # return("")
      return(infoBox(title = myTitle, value = NULL))
    } else {
      # return(optimalSeed)
      valueBox(
        subtitle = myTitle,
        value = optimalSeed,
        # icon = icon("sort-numeric-up"),
        color = "black",
        width = 6
      )
    }
  })


  # ----- __silhouette -----
  output$kMeans_silhouette <- renderUI({
    myTitle <- "Silhouette Score"
    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) {
      return(infoBox(title = myTitle, value = NULL))
    }

    nbCenters <- get_kMeans_optimalNbCenters()
    kMeans_seed <- get_kMeans_optimalSeed()

    if (is.null(nbCenters) | nbCenters == 1 | is.null(kMeans_seed)) {
      silhouette_score <- 0 %>% paste("%")
    } else {
      silhouette_score <- get_kMeans_indic() %>%
        dplyr::filter(nbCenters.col == nbCenters) %>%
        dplyr::filter(kMeans_seed.col == kMeans_seed) %>%
        dplyr::select(silhouette_score.col) %>%
        head(1) %>% unlist() %>% unname() %>%
        magrittr::multiply_by(100) %>% round(1) %>% paste("%")
    }

    myInfoBox <- infoBox(
      title = myTitle,
      value = silhouette_score,
      icon = icon("chart-bar"),
      color = "orange",
      width = 12
    )

    return(myInfoBox)
  })
  # ----- __variation -----
  output$kMeans_variation <- renderUI({
    myTitle <- "Variation Handled"
    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) {
      return(infoBox(title = myTitle, value = NULL))
    }

    nbCenters <- get_kMeans_optimalNbCenters()
    kMeans_seed <- get_kMeans_optimalSeed()

    if (is.null(nbCenters) | nbCenters == 1 | is.null(kMeans_seed)) {
      handled_variation <- 0 %>% paste("%")
    } else {
      handled_variation <- get_kMeans_indic() %>%
        dplyr::filter(nbCenters.col == nbCenters) %>%
        dplyr::filter(kMeans_seed.col == kMeans_seed) %>%
        dplyr::select(handled_variation.col) %>%
        head(1) %>% unlist() %>% unname() %>%
        magrittr::multiply_by(100) %>% round(1) %>% paste("%")
    }

    myInfoBox <- infoBox(
      title = myTitle,
      value = handled_variation,
      icon = icon("chart-bar"),
      color = "green",
      width = 12
    )

    return(myInfoBox)
  })

  # ----- __varHist_A -----
  output$kMeans_varHist_A <- renderPlot({
    get_varHist_plot()
  })

  # ----- __varHist_M -----
  output$kMeans_varHist_M <- renderPlot({
    get_varHist_plot()
  })

  # ----- __seedHist_M -----
  output$kMeans_seedHist_M <- renderPlot({
    get_seedHist_plot()
  })

  # ----- __silhouetteExplo -----
  output$kMeans_silhouetteExplo <- renderUI({
    myTitle <- "Silhouette Score"

    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) {
      return(infoBox(title = myTitle, value = NULL))
    }

    nbCenters <- input$kMeans_nbCenters
    kMeans_seed <- input$kMeans_seed
    myIterMax <- input$kMeans_myIterMax

    if (is.null(nbCenters) | nbCenters == 1 | is.null(kMeans_seed)) {
      silhouette_score <- 0 %>% paste("%")
    } else {
      set.seed(kMeans_seed)
      kmeans_result <- clusters_data %>%
        dplyr::select(x, y) %>%
        kmeans(centers = nbCenters, iter.max = myIterMax)

      silhouette_summary <- cluster::silhouette(
        kmeans_result$cluster,
        clusters_data %>%
          dplyr::select(x, y) %>%
          dist()
      ) %>% summary()
      silhouette_score <- silhouette_summary$si.summary[["Mean"]] %>%
        magrittr::multiply_by(100) %>% round(1) %>% paste("%")}

    myInfoBox <- infoBox(
      title = myTitle,
      value = silhouette_score,
      icon = icon("chart-bar"),
      color = "orange",
      width = 12
    )

    return(myInfoBox)
  })

  # ----- __variationExplo -----
  output$kMeans_variationExplo <- renderUI({
    myTitle <- "Variation Handled"

    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) {
      return(infoBox(title = myTitle, value = NULL))
    }

    nbCenters <- input$kMeans_nbCenters
    kMeans_seed <- input$kMeans_seed
    myIterMax <- input$kMeans_myIterMax

    if (is.null(nbCenters) | nbCenters == 1 | is.null(kMeans_seed)) {
      handled_variation <- 0 %>% paste("%")
    } else {
      set.seed(kMeans_seed)
      kmeans_result <- clusters_data %>%
        dplyr::select(x, y) %>%
        kmeans(centers = nbCenters, iter.max = myIterMax)

      silhouette_summary <- cluster::silhouette(
        kmeans_result$cluster,
        clusters_data %>%
          dplyr::select(x, y) %>%
          dist()
      ) %>% summary()
      handled_variation <- kmeans_result$betweenss %>%
        magrittr::divide_by(kmeans_result$totss) %>%
        magrittr::multiply_by(100) %>% round(1) %>% paste("%")
    }

    myInfoBox <- infoBox(
      title = myTitle,
      value = handled_variation,
      icon = icon("chart-bar"),
      color = "green",
      width = 12
    )

    return(myInfoBox)
  })

  # ----- __plot -----
  output$kMeans_plot <- renderPlot({
    if (input$kMeans_init == TRUE) {
      return(get_clusters_plot())
    }

    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) {
      return(plot.new())
    }

    if (input$kMeans_panel1 == "Manual Run") {
      nbCenters <- input$kMeans_nbCenters
      kMeans_seed <- input$kMeans_seed
      myIterMax <- input$kMeans_myIterMax
    } else {
      nbCenters <- get_kMeans_optimalNbCenters()
      kMeans_seed <- get_kMeans_optimalSeed()
      myIterMax <- 10
      if (is.null(nbCenters) | is.null(kMeans_seed)) {
        return(plot.new())
      }
    }

    set.seed(kMeans_seed)
    kmeans_result <- clusters_data %>%
      dplyr::select(x, y) %>%
      kmeans(centers = nbCenters, iter.max = myIterMax)

    p <- data.frame(
      cluster = kmeans_result$cluster,
      x = clusters_data$x,
      y = clusters_data$y
    ) %>% plot_clusters()

    if (input$kMeans_displayCenters) {
      p <- p + ggplot2::geom_point(
        data = as.data.frame(kmeans_result$center),
        mapping = ggplot2::aes(x = x, y = y),
        size = 3
      )
    }

    return(p)
  })
}
