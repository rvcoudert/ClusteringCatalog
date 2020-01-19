# ----- preprocess -----


make_blobs <- function(
  n_samples = 100,
  centers = 3,
  cluster_sd = 1,
  cluster_sd_var = 0,
  min_dist = 5,
  center_limits = c(-20, 20),
  noise = FALSE,
  seed = 0
) {
  set.seed(seed)

  ## Récupération des entrées.
  nb_centers <- centers

  ## Générations des centres.
  # Initialisation du premier centre.
  keeperX <- c(0)
  keeperY <- c(0)
  if (centers > 1) {
    # Ajout du prochain centre en essayant de le placer au hasard
    #   jusqu'à ce qu'il ne soit pas trop proche d'un autre.
    for (counter in 2:centers) {
      try <- 1
      repeat {
        # On prend un point aléatoire.
        thisX <- runif(
          n = 1,
          min = center_limits[[1]],
          max = center_limits[[2]])
        thisY <- runif(
          n = 1,
          min = center_limits[[1]],
          max = center_limits[[2]])
        # On regarde s'il est assez loin des centres déjà existants.
        distances = sqrt((thisX - keeperX)**2 + (thisY - keeperY)**2)
        myMinDist = min(distances)
        # Si c'est bon, alors on l'ajoute. Sinon on recommence.
        if (myMinDist >= min_dist | try > 500) {
          keeperX[[counter]] <-  thisX
          keeperY[[counter]] <-  thisY
          break()
        } else {
          try <- try + 1
        }
      }
    }
  }
  centers_points <- data.frame(x = keeperX, y = keeperY)

  ## Effectifs des clusters.
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

  ## Calcul des points.
  # Initialisation des points sur les centres des clusters.
  cluster_points <- cluster_info %>%
    dplyr::select(-freq) %>%
    dplyr::slice(rep(cluster_info$cluster, cluster_info$freq))
  # Calcul des perturbations.
  set.seed(seed)
  deviations_x <- apply(cluster_info, 1, function(cluster) {
    list(rnorm(
      n = cluster[["freq"]],
      mean = 0,
      sd = cluster_sd * (1 + (cluster[["cluster"]] - 1) * cluster_sd_var)
    ))
  }) %>% unlist()
  set.seed(seed + 1)
  deviations_y <- apply(cluster_info, 1, function(cluster) {
    list(rnorm(
      n = cluster[["freq"]],
      mean = 0,
      sd = cluster_sd * (1 + (cluster[["cluster"]] - 1) * cluster_sd_var)
    ))
  }) %>% unlist()
  # Ajout des perturbations aux points sur les centres.
  cluster_points <- cluster_points %>%
    dplyr::mutate(x = x + deviations_x) %>%
    dplyr::mutate(y = y + deviations_y)

  return(cluster_points)
}


make_moons <- function(
  n_samples = 200,
  cluster_sd = 1,
  cluster_sd_var = 0,
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
  deviations_x <- c(
    rnorm(
      n = cluster_freq[[1]],
      mean = 0,
      sd = cluster_sd
    ),
    rnorm(
      n = cluster_freq[[2]],
      mean = 0,
      sd = cluster_sd * (1 + cluster_sd_var)
    )
  )
  deviations_y <- c(
    rnorm(
      n = cluster_freq[[1]],
      mean = 0,
      sd = cluster_sd
    ),
    rnorm(
      n = cluster_freq[[2]],
      mean = 0,
      sd = cluster_sd * (1 + cluster_sd_var)
    )
  )
  # Ajout des perturbations depuis les centres.
  cluster_points <- cluster_points %>%
    dplyr::mutate(x = x + deviations_x) %>%
    dplyr::mutate(y = y + deviations_y)
}


make_circles <- function(
  n_samples = 200,
  cluster_sd = 1,
  cluster_sd_var = 0,
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
  deviations_x <- c(
    rnorm(
      n = cluster_freq[[1]],
      mean = 0,
      sd = cluster_sd
    ),
    rnorm(
      n = cluster_freq[[2]],
      mean = 0,
      sd = cluster_sd * (1 + cluster_sd_var)
    )
  )
  deviations_y <- c(
    rnorm(
      n = cluster_freq[[1]],
      mean = 0,
      sd = cluster_sd
    ),
    rnorm(
      n = cluster_freq[[2]],
      mean = 0,
      sd = cluster_sd * (1 + cluster_sd_var)
    )
  )
  # Ajout des perturbations depuis les centres.
  cluster_points <- cluster_points %>%
    dplyr::mutate(x = x + deviations_x) %>%
    dplyr::mutate(y = y + deviations_y)
}


plot_clusters <- function(clusters_points, xlim = NULL, ylim = NULL) {
  p <- ggplot2::ggplot(
    data = clusters_points
  ) + ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = x,
      y = y,
      colour = as.factor(cluster)
    ),
    size = 3
  ) + ggplot2::scale_color_brewer(
    palette = "Dark2",
    guide = FALSE,
    na.value = "black"
  ) + ggplot2::theme_bw(
    base_size = 14
  ) + ggplot2::theme(
    legend.position = "none",
    plot.title = ggplot2::element_text(hjust = 0.5),
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )

  # if (is.null(xlim) | is.null(ylim)) {
  #   p <- p + ggplot2::coord_fixed(
  #   ) + ggplot2::labs(
  #     title = "General View"
  #   )
  # } else {
  #   p <- p + ggplot2::coord_cartesian(
  #     xlim = xlim,
  #     ylim = ylim,
  #     expand = FALSE
  #   ) + ggplot2::labs(
  #     title = "Zoom"
  #   )
  # }

  return(p)
}


circleFun <- function(center = c(0,0), r = 1, npoints = 20) {
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}


function(input, output) {

  # ----- genData_reac -----

  # ----- __clusters_data -----

  get_clusters_data <- reactive({
    if (
      is.null(input$genData_method) |
      is.null(input$genData_nSamples) |
      is.null(input$genData_clusterSd) |
      is.null(input$genData_clusterSdVar) |
      is.null(input$genData_blobsNbClusters) |
      is.null(input$genData_blobsMinDist) |
      is.null(input$seed)
    ) {
      clusters_data <- make_blobs(
        n_samples = 500,
        centers = 3,
        cluster_sd = 1,
        cluster_sd_var = 0,
        min_dist = 2,
        seed = 1)
    } else {
      nSamples <- input$genData_nSamples
      clusterSd <- input$genData_clusterSd
      clusterSdVar <- input$genData_clusterSdVar
      # On force les entrées si inadaptées.
      if (!is.numeric(nSamples)) {
        nSamples <- 0
      } else if (nSamples < 8) {
        nSamples <- 8
      }
      if (!is.numeric(clusterSd)) {
        clusterSd <- 1
      }
      if (input$genData_method == "blobs") {
        nb_clusters <- input$genData_blobsNbClusters
        myMinDist <- input$genData_blobsMinDist
        clusters_data <- make_blobs(
          n_samples = nSamples,
          centers = nb_clusters,
          cluster_sd = clusterSd,
          cluster_sd_var = clusterSdVar,
          min_dist = myMinDist,
          seed = input$seed)
      }

      if (input$genData_method == "moons") {
        clusters_data <- make_moons(
          n_samples = nSamples,
          cluster_sd = clusterSd,
          cluster_sd_var = clusterSdVar,
          seed = input$seed)
      }

      if (input$genData_method == "circles")
      {
        if (is.null(input$genData_circlesScale)) return(NULL)
        scale <- input$genData_circlesScale
        clusters_data <- make_circles(
          n_samples = nSamples,
          cluster_sd = clusterSd,
          cluster_sd_var = clusterSdVar,
          scale = scale,
          seed = input$seed)
      }
    }

    return(clusters_data)
  })
  # ----- sideBar_output -----

  output$blobsNbClusters <- renderUI({
    UI <- NULL

    if (input$genData_method == "blobs") {
      UI <- sliderTextInput(
        inputId = "genData_blobsNbClusters",
        label = "Blobs number",
        choices = 1:8,
        selected = 3,
        grid = TRUE
      )
    }

    return(UI)
  })

  output$blobsMinDist <- renderUI({
    UI <- NULL

    if (input$genData_method == "blobs") {
      UI <- sliderTextInput(
        inputId = "genData_blobsMinDist",
        label = "Centers Min Dist",
        choices = 1:20,
        selected = 5,
        grid = TRUE
      )
      # UI <- sliderTextInput(
      #   inputId = "genData_distLim",
      #   label = "Centers Dist Limits",
      #   choices = 1:20,
      #   selected = c(5, 8)
      # )
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


  # ----- genData_reac -----

  # ----- __clusters_info -----

  get_clusters_info <- reactive({
    method <- input$genData_method

    if (input$genData_method %in% c("moons", "circles")) {
      nbClusters <- 2
    }

    if (input$genData_method == "blobs") {
      nbClusters <- input$genData_blobsNbClusters
    }

    if (is.null(nbClusters)) {
      return("3 clusters (blobs)")
    }

    if (nbClusters == 1) {
      cluster_string <- "1 cluster"
    } else {
      cluster_string <- paste(nbClusters, "clusters")
    }

    return(paste(
      cluster_string,
      " (",
      method,
      ")",
      sep = ""
    ))
  })

  0# ----- __clusters_plot -----

  get_clusters_plot <- reactive({
    clusters_data <- get_clusters_data()
    if (is.null(clusters_data)) {
      return(plot.new())
    }

    p <- plot_clusters(clusters_points = clusters_data)

    return(p)
  })
  # ----- genData_output -----

  # ----- __info -----
  output$genData_info <- renderText({
    get_clusters_info()
  })

  # ----- __plot -----
  output$genData_plot <- renderPlot({
    get_clusters_plot() +
      ggplot2::coord_fixed() +
      ggplot2::labs(title = "General View")
  })

  ranges_genData <- reactiveValues(x = NULL, y = NULL)

  observe({
    brush <- input$genData_brush
    if (!is.null(brush)) {
      ranges_genData$x <- c(brush$xmin, brush$xmax)
      ranges_genData$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges_genData$x <- NULL
      ranges_genData$y <- NULL
    }
  })

  # ----- __plot_2 -----
  output$genData_plot_2 <- renderPlot({
    if (is.null(ranges_genData$x) | is.null(ranges_genData$y)) {
      message <- "Waiting for zoom."
      p <- ggplot2::ggplot(
      ) + ggplot2::annotate(
        "text", x = 0, y = 0, size = 8, label = message
      ) + ggplot2::theme_void()
      return(p)
    } else {
      p <- get_clusters_plot(
      ) + ggplot2::coord_cartesian(
        xlim = ranges_genData$x,
        ylim = ranges_genData$y,
        expand = FALSE
      ) + ggplot2::labs(
        title = "Zoom"
      )
    }

    return(p)
  })


  # ----- kMeans_reac -----

  # ----- __run_KMeans -----

  run_kMeans <- reactive({
    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) {
      return(NULL)
    }

    if (input$kMeans_panel1 == "Manual Run") {
      nbCenters <- input$kMeans_nbCenters
      kMeans_seed <- input$kMeans_seed
      myIterMax <- input$kMeans_myIterMax
    } else {
      nbCenters <- get_kMeans_optimalNbCenters()
      kMeans_seed <- get_kMeans_optimalSeed()
      myIterMax <- 10
    }

    if (is.null(nbCenters) | is.null(kMeans_seed)) {
      return(plot.new())
    }

    clusters_space <- clusters_data %>%
      dplyr::select(x, y)
    set.seed(kMeans_seed)
    init_centers <- clusters_space %>%
      dplyr::slice(sample(nrow(.), size = nbCenters))
    init_centers_space <- data.frame(
      init_centers,
      cluster = 1:nbCenters
    )
    kmeans_result <- kmeans(
      x = clusters_space,
      centers = init_centers,
      iter.max = myIterMax
    )
    final_centers_space <- data.frame(
      kmeans_result$centers,
      cluster = 1:nbCenters
    )

    kmeans_space <- data.frame(
      cluster = kmeans_result$cluster,
      x = clusters_data$x,
      y = clusters_data$y
    ) %>% dplyr::arrange(cluster)

    attr(kmeans_space, "init_centers_space") <- init_centers_space
    attr(kmeans_space, "final_centers_space") <- final_centers_space

    return(kmeans_space)
  })

  # ----- __plot_kMeans -----

  plot_kMeans <- reactive({
    kmeans_space <- run_kMeans()

    if (is.null(kmeans_space)) return(plot.new())

    init_centers_space <- attr(kmeans_space, "init_centers_space")
    final_centers_space <- attr(kmeans_space, "final_centers_space")

    p <- kmeans_space %>% plot_clusters()

    if (input$kMeans_initCenters) {
      p <- p + ggplot2::geom_point(
        data = init_centers_space,
        mapping = ggplot2::aes(x = x, y = y, fill = as.factor(cluster)),
        size = 6,
        shape = 22
      )
    }

    if (input$kMeans_finalCenters) {
      p <- p + ggplot2::geom_point(
        data = final_centers_space,
        mapping = ggplot2::aes(x = x, y = y, fill = as.factor(cluster)),
        size = 6,
        shape = 21
      )
    }

    if (input$kMeans_finalCenters | input$kMeans_initCenters) {
      p <- p + ggplot2::scale_fill_brewer(
        palette = "Dark2"
      )
    }

    if (input$kMeans_finalCenters & input$kMeans_initCenters) {
      p <- p + ggplot2::geom_segment(
        data = data.frame(
          init_centers_space,
          final_centers_space %>%
            dplyr::mutate(xend = x, yend = y) %>%
            dplyr::select(-cluster)
        ),
        mapping = ggplot2::aes(
          x = x, y = y,
          xend = xend, yend = yend
        ),
        arrow = ggplot2::arrow(length = ggplot2::unit(0.02, "npc"))
      )
    }

    return(p)
  })

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

  # ----- __optimalNbCenters -----
  get_kMeans_optimalNbCenters <- reactive({
    indic_df <- get_kMeans_indic()

    if (is.null(indic_df)) {
      return(NULL)
    }

    # On sélectionne simplement le nombre de clusters issu
    #   la configuration avec le meilleur score de silhouette.
    indic_df %>%
      dplyr::filter(silhouette_score.col == max(silhouette_score.col)) %>%
      dplyr::select(nbCenters.col) %>%
      head(1) %>% unlist() %>% unname()
  })


  # ----- __optimalSeed -----
  get_kMeans_optimalSeed <- reactive({
    indic_df <- get_kMeans_indic()

    if (is.null(indic_df)) {
      return(NULL)
    }

    # On sélectionne simplement la graine issue
    #   la configuration avec le meilleur score de silhouette.
    indic_df %>%
      dplyr::filter(silhouette_score.col == max(silhouette_score.col)) %>%
      dplyr::select(kMeans_seed.col) %>%
      head(1) %>% unlist() %>% unname()
  })


  circleFun <- function(center = c(0,0), r = 1, npoints = 20) {
    tt <- seq(0,2 * pi, length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }


  # ----- __heatMap_sil -----
  get_kMeans_heatMap_sil <- reactive({
    indic_df <- get_kMeans_indic()

    if (is.null(indic_df)) {
      return(plot.new())
    }

    # On supprime les lignes où il n'y a qu'un centre. (Inintéressant.)
    indic_df <- indic_df %>% dplyr::filter(nbCenters.col != 1)

    myColours <- c(
      "darkred", "red3", "red",
      "orangered", "gold2",
      "forestgreen", "darkgreen")
    names(myColours) <-
      c(0, 0.1, 0.2,
        0.4, 0.5,
        0.8, 1)

    # myColours <- c("red3", "gold2", "darkgreen")
    # names(myColours) <- c(0, 0.5, 1)

    p <- ggplot2::ggplot(
      data = indic_df,
      mapping = ggplot2::aes(
        x = nbCenters.col,
        y = kMeans_seed.col,
        fill = silhouette_score.col
      )
    ) + ggplot2::geom_tile(
    ) + ggplot2::scale_fill_gradientn(
      colours = myColours,
      values = names(myColours)
      # limits = c(0,1),
      # breaks = c(0, 0.5, 0.8, 0.9, 1)
    ) + ggplot2::scale_x_discrete(
      breaks = 2:8,
      limits = 2:8,
      expand = c(0,0)
    ) + ggplot2::scale_y_discrete(
      breaks = 1:20,
      limits = 1:20,
      expand = c(0,0)
    ) + ggplot2::labs(
      x = "nbCenters",
      y = "kMeans_seed",
      fill = "silhouette_score"
    ) + ggplot2::theme(
      legend.position = "bottom",
      legend.key.size = ggplot2::unit(1, "cm"),
      legend.title = ggplot2::element_text(vjust = 0.9),
      legend.text = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
    )

    return(p)
  })


  # ----- kMeans_output -----

  # ----- __info -----
  output$kMeans_info <- renderText({
    get_clusters_info()
  })

  # ----- __optimalNbCenters -----
  output$kMeans_optimalNbCenters <- renderUI({
    optimalNbCenters <- get_kMeans_optimalNbCenters()
    myTitle <- "Optimal NbCenters"

    valueBox(
      subtitle = myTitle,
      value = optimalNbCenters,
      color = "black",
      width = 6
    )
  })

  # ----- __optimalSeed -----
  output$kMeans_optimalSeed <- renderUI({
    optimalSeed <- get_kMeans_optimalSeed()
    myTitle <- "Optimal Seed"

    valueBox(
      subtitle = myTitle,
      value = optimalSeed,
      color = "black",
      width = 6
    )
  })


  # ----- __silhouette -----
  output$kMeans_silhouette <- renderUI({
    myTitle <- HTML(paste("Silhouette", br(), "Score", sep = ""))

    # On récupère les valeurs en fonction du mode utilisé.
    if (input$kMeans_panel1 == "Auto Run")
    {
      ## Auto Run.
      nbCenters <- get_kMeans_optimalNbCenters()
      kMeans_seed <- get_kMeans_optimalSeed()
      myIterMax <- 10
    } else {
      ## Manual Run.
      nbCenters <- input$kMeans_nbCenters
      kMeans_seed <- input$kMeans_seed
      myIterMax <- input$kMeans_myIterMax
    }
    indic_df <- get_kMeans_indic()

    # On gère les cas où les valeurs ne sont pas encore prêtes.
    if (is.null(nbCenters) | is.null(kMeans_seed) |
        is.null(myIterMax) | is.null(indic_df)) {
      silhouette_score <- NULL
    } else if (nbCenters == 1) {
      silhouette_score <- 0 %>% paste("%")
    } else {
      silhouette_score <- indic_df %>%
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
      color = "navy",
      width = 12
    )
    return(myInfoBox)
  })

  # ----- __variation -----
  output$kMeans_variation <- renderUI({
    myTitle <- HTML(paste("Variation", br(), "Handled", sep = ""))

    # On récupère les valeurs en fonction du mode utilisé.
    if (input$kMeans_panel1 == "Auto Run")
    {
      ## Auto Run.
      nbCenters <- get_kMeans_optimalNbCenters()
      kMeans_seed <- get_kMeans_optimalSeed()
      myIterMax <- 10
    } else {
      ## Manual Run.
      nbCenters <- input$kMeans_nbCenters
      kMeans_seed <- input$kMeans_seed
      myIterMax <- input$kMeans_myIterMax
    }
    indic_df <- get_kMeans_indic()

    # On gère les cas où les valeurs ne sont pas encore prêtes.
    if (is.null(nbCenters) | is.null(kMeans_seed) |
        is.null(myIterMax) | is.null(indic_df)) {
      handled_variation <- NULL
    } else if (nbCenters == 1) {
      handled_variation <- 0 %>% paste("%")
    } else {
      handled_variation <- indic_df %>%
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
      color = "navy",
      width = 12
    )

    return(myInfoBox)
  })

  # ----- __heatMap_sil_A -----
  output$kMeans_heatMap_sil_A <- renderPlot({
    get_kMeans_heatMap_sil()
  })

  # ----- __heatMap_sil_M -----
  output$kMeans_heatMap_sil_M <- renderPlot({
    get_kMeans_heatMap_sil()
  })

  # ----- __plot -----
  output$kMeans_plot <- renderPlot({
    plot_kMeans() +
      ggplot2::coord_fixed() +
      ggplot2::labs(title = "General View")
  })

  ranges_kMeans <- reactiveValues(x = NULL, y = NULL)

  observe({
    brush <- input$kMeans_brush
    if (!is.null(brush)) {
      ranges_kMeans$x <- c(brush$xmin, brush$xmax)
      ranges_kMeans$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges_kMeans$x <- NULL
      ranges_kMeans$y <- NULL
    }
  })

  # ----- __plot_2 -----
  output$kMeans_plot_2 <- renderPlot({
    if (input$kMeans_init == TRUE) {
      p <- get_clusters_plot() +
        ggplot2::labs(title = "Initial clusters")
      return(p)
    }

    if (is.null(ranges_kMeans$x) | is.null(ranges_kMeans$y)) {
      message <- "Waiting for zoom."
      p <- ggplot2::ggplot(
      ) + ggplot2::annotate(
        "text", x = 0, y = 0, size = 8, label = message
      ) + ggplot2::theme_void()
      return(p)
    } else {
      p <- plot_kMeans(
      ) + ggplot2::coord_cartesian(
        xlim = ranges_kMeans$x,
        ylim = ranges_kMeans$y,
        expand = FALSE
      ) + ggplot2::labs(
        title = "Zoom"
      )
    }

    return(p)
  })


  # ----- DBSCAN_reac -----


  # ----- __run_DBSCAN -----
  run_DBSCAN <- reactive({
    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) return(NULL)

    if (input$DBSCAN_panel1 == "Manual Run") {
      myEps <- input$DBSCAN_eps
      myMinPoints <- input$DBSCAN_minPoints
    } else {
      myEps <- 1
      myMinPoints <- 5
    }

    if (is.null(myEps) | is.null(myMinPoints)) {
      return(NULL)
    }

    clusters_space <- clusters_data %>%
      dplyr::select(x, y)

    DBSCAN_result <- dbscan::dbscan(
      x = clusters_space,
      eps = myEps,
      minPts = myMinPoints,
      borderPoints = input$DBSCAN_borderPoints
    )

    return(DBSCAN_result)
  })


  # ----- __plot_DBSCAN -----
  plot_DBSCAN <- reactive({
    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) return(plot.new())

    DBSCAN_result <- run_DBSCAN()

    if (is.null(DBSCAN_result)) return(plot.new())

    DBSCAN_space <- data.frame(
      cluster = DBSCAN_result$cluster,
      x = clusters_data$x,
      y = clusters_data$y
    ) %>%
      dplyr::arrange(cluster) %>%
      dplyr::mutate(cluster = ifelse(cluster == 0, NA, cluster))

    p <- DBSCAN_space %>% plot_clusters()

    return(p)
  })


  # ----- DBSCAN_output -----


  # ----- __info -----
  output$DBSCAN_info <- renderText({
    get_clusters_info()
  })

  # ----- __plot -----
  output$DBSCAN_plot <- renderPlot({
    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) return(plot.new())

    DBSCAN_result <- run_DBSCAN()

    if (is.null(DBSCAN_result)) return(plot.new())

    DBSCAN_space <- data.frame(
      cluster = DBSCAN_result$cluster,
      x = clusters_data$x,
      y = clusters_data$y
    ) %>%
      dplyr::arrange(cluster) %>%
      dplyr::mutate(cluster = ifelse(cluster == 0, NA, cluster))

    p <- DBSCAN_space %>% plot_clusters() +
      ggplot2::coord_fixed(
        xlim = c(min(DBSCAN_space$x), max(DBSCAN_space$x)),
        ylim = c(min(DBSCAN_space$y), max(DBSCAN_space$y))
      ) +
      ggplot2::labs(title = "General View")

    if (input$DBSCAN_density == TRUE) {

      DBSCAN_selected <- DBSCAN_space

      n_captured_points <- nrow(DBSCAN_selected)

      if (n_captured_points > 0) {
        circle_points <- (2000 / n_captured_points) %>% round() %>%
          max(10) %>% min(100)
        circle_base <- circleFun(
          center = c(0,0),
          r = DBSCAN_result$eps,
          npoints = circle_points
        ) %>% dplyr::slice(rep(1:circle_points, n_captured_points))
        circle_points <- DBSCAN_selected %>%
          dplyr::slice(
            plyr::llply(1:n_captured_points, rep, circle_points) %>% unlist()
          )
        circle_points$x <- circle_points$x + circle_base$x
        circle_points$y <- circle_points$y + circle_base$y

        p <- p + ggplot2::geom_point(
          mapping = ggplot2::aes(x = x, y = y, color = as.factor(cluster)),
          data = circle_points,
          size = 0.1,
          alpha = 0.5
        )
      }
    }

    return(p)
  })

  ranges_DBSCAN <- reactiveValues(x = NULL, y = NULL)

  observe({
    brush <- input$DBSCAN_brush
    if (!is.null(brush)) {
      ranges_DBSCAN$x <- c(brush$xmin, brush$xmax)
      ranges_DBSCAN$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges_DBSCAN$x <- NULL
      ranges_DBSCAN$y <- NULL
    }
  })

  # ----- __plot_2 -----
  output$DBSCAN_plot_2 <- renderPlot({
    if (input$DBSCAN_init == TRUE) {
      p <- get_clusters_plot() +
        ggplot2::labs(title = "Initial clusters")
      return(p)
    }

    if (is.null(ranges_DBSCAN$x) | is.null(ranges_DBSCAN$y)) {
      message <- "Waiting for zoom."
      p <- ggplot2::ggplot(
      ) + ggplot2::annotate(
        "text", x = 0, y = 0, size = 8, label = message
      ) + ggplot2::theme_void()
      return(p)
    } else {
      p <- plot_DBSCAN(
      ) + ggplot2::coord_cartesian(
        xlim = ranges_DBSCAN$x,
        ylim = ranges_DBSCAN$y,
        expand = FALSE
      ) + ggplot2::labs(
        title = "Zoom"
      )
    }

    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) return(plot.new())

    DBSCAN_result <- run_DBSCAN()

    if (is.null(DBSCAN_result)) return(plot.new())

    DBSCAN_space <- data.frame(
      cluster = DBSCAN_result$cluster,
      x = clusters_data$x,
      y = clusters_data$y
    ) %>%
      dplyr::arrange(cluster) %>%
      dplyr::mutate(cluster = ifelse(cluster == 0, NA, cluster))

    if (!is.null(ranges_2_DBSCAN$x) & !is.null(ranges_2_DBSCAN$y)) {

      DBSCAN_selected <- DBSCAN_space %>%
        dplyr::filter(
          x > ranges_2_DBSCAN$x[[1]] &
            x < ranges_2_DBSCAN$x[[2]] &
            y > ranges_2_DBSCAN$y[[1]] &
            y < ranges_2_DBSCAN$y[[2]]
        )

      n_captured_points <- nrow(DBSCAN_selected)

      if (n_captured_points > 0) {
        circle_points <- (3000 / n_captured_points) %>% round() %>%
          max(10) %>% min(200)
        circle_base <- circleFun(
          center = c(0,0),
          r = DBSCAN_result$eps,
          npoints = circle_points
        ) %>% dplyr::slice(rep(1:circle_points, n_captured_points))
        circle_points <- DBSCAN_selected %>%
          dplyr::slice(
            plyr::llply(1:n_captured_points, rep, circle_points) %>% unlist()
          )
        circle_points$x <- circle_points$x + circle_base$x
        circle_points$y <- circle_points$y + circle_base$y

        p <- p + ggplot2::geom_point(
          mapping = ggplot2::aes(x = x, y = y, color = as.factor(cluster)),
          data = circle_points,
          size = 0.1,
          alpha = 0.5
        )
      }
    }

    return(p)
  })

  ranges_2_DBSCAN <- reactiveValues(x = NULL, y = NULL)

  observe({
    brush <- input$DBSCAN_brush_2
    if (!is.null(brush)) {
      ranges_2_DBSCAN$x <- c(brush$xmin, brush$xmax)
      ranges_2_DBSCAN$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges_2_DBSCAN$x <- NULL
      ranges_2_DBSCAN$y <- NULL
    }
  })

  # ----- __silhouette -----
  output$DBSCAN_silhouette <- renderUI({
    myTitle <- HTML(paste("Silhouette", br(), "Score", sep = ""))

    clusters_data <- get_clusters_data()
    DBSCAN_result <- run_DBSCAN()

    if (is.null(DBSCAN_result) | is.null(clusters_data)) {
      silhouette_score <- NULL
    } else {
      if ((DBSCAN_result$cluster %>% unique() %>% length()) == 1) {
        silhouette_score <- NULL
      } else {
      silhouette_summary <- cluster::silhouette(
        DBSCAN_result$cluster,
        clusters_data %>%
          dplyr::select(x, y) %>%
          dist()
      ) %>% summary()
      silhouette_score <- silhouette_summary$si.summary[["Mean"]] %>%
        magrittr::multiply_by(100) %>% round(1) %>% paste("%")
      }
    }

    myInfoBox <- infoBox(
      title = myTitle,
      value = silhouette_score,
      icon = icon("chart-bar"),
      color = "navy",
      width = 12
    )
    return(myInfoBox)
  })

  # ----- __variation -----
  output$DBSCAN_variation <- renderUI({
    myTitle <- HTML(paste("Variation", br(), "Handled", sep = ""))

    clusters_data <- get_clusters_data()
    DBSCAN_result <- run_DBSCAN()

    if (is.null(DBSCAN_result) | is.null(clusters_data)) {
      handled_variation <- NULL
    } else {
      handled_variation <- NULL
    }

    myInfoBox <- infoBox(
      title = myTitle,
      value = handled_variation,
      icon = icon("chart-bar"),
      color = "navy",
      width = 12
    )

    return(myInfoBox)
  })
}
