# ----- preprocess -----


# Starter Data.
starter.n_samples <- 500
starter.centers <- 6
starter.cluster_sd <- 1
starter.cluster_sd_var <- 0.5
starter.cluster_min_dist <- 1
starter.seed <- 2


make_blobs <- function(
  n_samples = starter.n_samples,
  centers = starter.centers,
  cluster_sd = starter.cluster_sd,
  cluster_sd_var = starter.cluster_sd_var,
  min_dist = starter.cluster_min_dist,
  center_limits = c(-20, 20),
  noise = FALSE,
  seed = starter.seed
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


plot_clusters <- function(
  clusters_points,
  xlim = NULL,
  ylim = NULL) {
  p <- ggplot2::ggplot(
    data = clusters_points
  ) + ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = x,
      y = y,
      colour = as.factor(cluster)
    ),
    size = 2
  ) + ggplot2::scale_color_brewer(
    palette = "Dark2",
    na.value = "black",
    name = "Cluster"
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
    preset <- input$genData_preset
    if (is.null(preset)) preset <- FALSE

    if (preset) {
      presetChoice <- input$genData_presetChoice
      if (is.null(presetChoice)) presetChoice <- "irisPetal"

      if (presetChoice == "irisSepal") {
        clusters_data <- iris %>%
          dplyr::select(1, 2, 5) %>%
          magrittr::set_colnames(c("x", "y", "cluster")) %>%
          dplyr::mutate(cluster = as.integer(as.factor(cluster)))
      } else if (presetChoice == "irisPetal") {
        clusters_data <- iris %>%
          dplyr::select(3, 4, 5) %>%
          magrittr::set_colnames(c("x", "y", "cluster")) %>%
          dplyr::mutate(cluster = as.integer(as.factor(cluster)))
      } else if (presetChoice == "DS3") {
        data(DS3, package = "dbscan")
        set.seed(0)
        clusters_data <- DS3 %>%
          magrittr::set_colnames(c("x", "y")) %>%
          dplyr::mutate(cluster = 0) %>%
          dplyr::sample_n(2000)
      } else if (presetChoice == "faithful") {
        set.seed(0)
        clusters_data <- faithful %>%
          scale() %>%
          as.data.frame() %>%
          magrittr::set_colnames(c("x", "y")) %>%
          dplyr::mutate(cluster = 0)
      } else if (presetChoice == "rock") {
        set.seed(0)
        clusters_data <- rock %>%
          dplyr::select(1, 2) %>%
          scale() %>%
          as.data.frame() %>%
          magrittr::set_colnames(c("x", "y")) %>%
          dplyr::mutate(cluster = 0)
      } else if (presetChoice == "rock2") {
        set.seed(0)
        clusters_data <- rock %>%
          dplyr::select(2, 3) %>%
          scale() %>%
          as.data.frame() %>%
          magrittr::set_colnames(c("x", "y")) %>%
          dplyr::mutate(cluster = 0)
      }
    } else
    {
      if (
        is.null(input$genData_method) |
        is.null(input$genData_nSamples) |
        is.null(input$genData_clusterSd) |
        is.null(input$genData_clusterSdVar) |
        is.null(input$genData_blobsNbClusters) |
        is.null(input$genData_blobsMinDist) |
        is.null(input$seed)
      ) {
        # Starter Data.
        clusters_data <- make_blobs(
          n_samples = starter.n_samples,
          centers = starter.centers,
          cluster_sd = starter.cluster_sd,
          cluster_sd_var = starter.cluster_sd_var,
          min_dist = starter.cluster_min_dist,
          seed = starter.seed)
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
    }
    return(clusters_data)
  })


  # ----- header_reac -----

  header_blobsNbClusters <- reactive({
    UI <- NULL

    if (input$genData_method == "blobs") {
      UI <- sliderTextInput(
        inputId = "genData_blobsNbClusters",
        label = "Blobs number",
        choices = 1:8,
        selected = starter.centers,
        grid = TRUE
      )
    }

    return(UI)
  })

  header_blobsMinDist <- reactive({
    UI <- NULL

    if (input$genData_method == "blobs") {
      UI <- sliderTextInput(
        inputId = "genData_blobsMinDist",
        label = "Centers Min Dist",
        choices = 1:20,
        selected = starter.cluster_min_dist,
        grid = TRUE
      )
    }

    return(UI)
  })

  header_circlesScale <- reactive({
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

  header_presetInfo <- reactive({
    UI <- NULL

    preset <- input$genData_preset
    if (is.null(preset)) preset <- FALSE

    if (preset) {
      clusters_data <- get_clusters_data()
      UI <- fluidPage(
        p(strong("n_samples :"), nrow(clusters_data)),
        p(strong("n_clusters :"), length(unique(clusters_data$cluster)))
      )
    }

    return(UI)
  })



  # ----- header_output -----
  output$blobsNbClusters <- renderUI({
    header_blobsNbClusters()
  })

  output$blobsMinDist <- renderUI({
    header_blobsMinDist()
  })

  output$circlesScale <- renderUI({
    header_circlesScale()
  })

  output$header_presetInfo <- renderUI({
    header_presetInfo()
  })

  output$header_data <- renderUI({
    preset <- input$genData_preset
    if (is.null(preset)) preset <- FALSE

    if (preset) {
      fluidPage(
        radioGroupButtons(
          inputId = "genData_presetChoice",
          label = "Preset Data",
          direction = "vertical",
          justified = TRUE,
          choices = c(
            "Iris Sepal" = "irisSepal",
            "Iris Petal" = "irisPetal",
            "faithful" = "faithful",
            "rock (area VS peri)" = "rock",
            "rock (peri VS shape)" = "rock2",
            "DS3" = "DS3"
          ),
          selected = "irisSepal",
          checkIcon = list(
            yes = tags$i(class = "fa fa-check-square",
                         style = "color: #d73925"),
            no = tags$i(class = "fa fa-square",
                        style = "color: #d73925")
          )
        ),
        uiOutput(outputId = "header_presetInfo")
      ) %>% return()
    } else
    {
      fluidPage(
        radioButtons(
          inputId = "genData_method",
          label = "Method",
          selected = "blobs",
          choiceNames = c("Blobs", "Moons", "Concentric Circles"),
          choiceValues = c("blobs", "moons", "circles")
        ),
        # radioGroupButtons(
        #   inputId = "genData_method",
        #   label = "Method",
        #   direction = "vertical",
        #   justified = TRUE,
        #   choices = c(
        #     "Blobs" = "blobs",
        #     "Moons" = "moons",
        #     "Concentric Circles" = "circles"
        #   ),
        #   checkIcon = list(
        #     yes = tags$i(class = "fa fa-check-square",
        #                  style = "color: #d73925"),
        #     no = tags$i(class = "fa fa-square",
        #                 style = "color: #d73925")
        #   )
        # ),
        numericInput(
          inputId = "genData_nSamples",
          label = "Sample Size",
          min = 0,
          max = 10000,
          step = 1,
          value = starter.n_samples
        ),
        numericInput(
          inputId = "genData_clusterSd",
          label = "Cluster Dispersion",
          min = 0,
          max = 10,
          step = 0.1,
          value = starter.cluster_sd
        ),
        sliderTextInput(
          inputId = "genData_clusterSdVar",
          label = "Dispersion Variation",
          choices = 0:20 / 10,
          selected = starter.cluster_sd_var,
          grid = TRUE
        ),
        sliderTextInput(
          inputId = "seed",
          label = "Data Seed",
          choices = 1:20,
          selected = starter.seed,
          grid = TRUE
        ),
        uiOutput(
          outputId = "blobsNbClusters"
        ),
        uiOutput(
          outputId = "blobsMinDist"
        ),
        uiOutput(
          outputId = "circlesScale"
        )
      ) %>% return()
    }
  })


  # ----- genData_reac -----


  # ----- __clusters_info -----

  get_clusters_info <- reactive({
    nullBox <- valueBox(
      subtitle = NULL,
      value = NULL,
      icon = icon("chart-bar"),
      color = "red",
      width = 12
    )
    preset <- input$genData_preset
    if (is.null(preset)) preset <- FALSE

    if (preset) {
      myValue <- input$genData_presetChoice
      mySubtitle <- "Preset Data"
    } else
    {
      method <- input$genData_method

      if (is.null(input$genData_method)) method <- "blobs"

      if (method %in% c("moons", "circles")) {
        nc <- 2
      } else if (method == "blobs") {
        nc <- input$genData_blobsNbClusters
        if (is.null(nc)) {
          nc <- starter.centers
        }
      }

      if (nc == 1) {
        myValue <- "1 cluster"
      } else {
        myValue <- paste(nc, "clusters")
      }

      mySubtitle <- paste("Generated (", method, ")", sep = "")
    }

    myValueBox <- valueBox(
      subtitle = mySubtitle,
      value = myValue,
      icon = icon("chart-bar"),
      color = "red",
      width = 12
    )

    return(myValueBox)
  })

  # ----- __silhouette -----

  get_clusters_silhouette <- reactive({
    clusters_data <- get_clusters_data()
    if (is.null(clusters_data)) {
      return(NULL)
    }

    if ((clusters_data$cluster %>% unique %>% length) == 1) return(0)

    silhouette_summary <- cluster::silhouette(
      x = clusters_data$cluster,
      dist = dist(clusters_data %>% dplyr::select(x, y))
    ) %>% summary()
    silhouette_mean <- silhouette_summary$si.summary[["Mean"]]
    return(silhouette_mean)
  })



  # ----- __clusters_plot -----

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
  output$genData_info <- renderUI({
    get_clusters_info()
  })


  # ----- __silhouette -----
  output$genData_silhouette <- renderUI({
    mySubTitle <- HTML(paste("Silhouette Score", sep = ""))

    silhouette_score <- get_clusters_silhouette()

    if (!is.null(silhouette_score)) silhouette_score <- silhouette_score %>%
      magrittr::multiply_by(100) %>% round(1) %>% paste("%")

    myValueBox <- valueBox(
      value = silhouette_score,
      subtitle = mySubTitle,
      icon = icon("chart-bar"),
      color = "red",
      width = 12
    )
    return(myValueBox)
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
      nbCenters <- get_kMeans_nbCenters()
      kMeans_seed <- get_kMeans_seed()
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

    if (is.null(input$kMeans_finalCenters) |
        is.null(input$kMeans_initCenters)) return(p)

    if (input$kMeans_initCenters) {
      p <- p + ggplot2::geom_point(
        data = init_centers_space,
        mapping = ggplot2::aes(x = x, y = y, fill = as.factor(cluster)),
        size = 5,
        shape = 22
      )
    }

    if (input$kMeans_finalCenters) {
      p <- p + ggplot2::geom_point(
        data = final_centers_space,
        mapping = ggplot2::aes(x = x, y = y, fill = as.factor(cluster)),
        size = 5,
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


    nbSteps <- 8 * 20

    withProgress(
      message = 'Optimize kMeans.',
      value = 0,
      min = 0,
      max = 1,
      {
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
              # Increment the progress bar, and update the detail text.
              incProgress(
                amount = 1 / nbSteps,
                detail = paste("Trying", nbCenters, "centers"))

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
  })

  # ----- __nbCenters -----
  get_kMeans_nbCenters <- reactive({
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


  # ----- __seed -----
  get_kMeans_seed <- reactive({
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
      legend.text = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
    )

    return(p)
  })


  # ----- kMeans_output -----

  # ----- __info -----
  output$kMeans_info <- renderUI({
    get_clusters_info()
  })

  # ----- __nbCenters -----
  output$kMeans_nbCenters <- renderUI({
    nbCenters <- get_kMeans_nbCenters()
    mySubTitle <- "Expected"

    if (nbCenters == 1) myValue <- "1 cluster"
    else myValue <- paste(nbCenters, "clusters")

    valueBox(
      value = myValue,
      subtitle = mySubTitle,
      icon = icon("chart-bar"),
      color = "red",
      width = 12
    )
  })

  # ----- __seed -----
  output$kMeans_seed <- renderUI({
    seed <- get_kMeans_seed()
    mySubTitle <- "Seed"

    valueBox(
      value = seed,
      subtitle = mySubTitle,
      icon = icon("chart-bar"),
      color = "red",
      width = 12
    )
  })


  # ----- __silhouette -----
  output$kMeans_silhouette <- renderUI({
    mySubTitle <- HTML(paste("Silhouette Score", sep = ""))

    # On récupère les valeurs en fonction du mode utilisé.
    if (input$kMeans_panel1 == "Auto Run")
    {
      ## Auto Run.
      nbCenters <- get_kMeans_nbCenters()
      kMeans_seed <- get_kMeans_seed()
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

    myValueBox <- valueBox(
      value = silhouette_score,
      subtitle = mySubTitle,
      icon = icon("chart-bar"),
      color = "red",
      width = 12
    )
    return(myValueBox)
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

    p <- plot_kMeans()

    if (is.null(p)) return(plot.new())

    p +
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
    if (input$kMeans_plotChoice == "init") {
      p <- get_clusters_plot() +
        ggplot2::coord_fixed() +
        ggplot2::labs(title = "Initial clusters")
      return(p)
    } else if (input$kMeans_plotChoice == "zoom") {
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
        return(p)
      }
    } else if (input$kMeans_plotChoice == "heatmap") {
      p <- get_kMeans_heatMap_sil()
      return(p)
    }

    return(plot.new())
  })


  # ----- HDBSCAN_reac -----


  # ----- __run_HDBSCAN -----
  run_HDBSCAN <- reactive({
    clusters_data <- get_clusters_data()

    clusters_space <- clusters_data %>%
      dplyr::select(x, y)

    if (is.null(clusters_data)) return(NULL)

    if (input$HDBSCAN_panel1 == "Manual Run" & !is.null(input$HDBSCAN_minPts)) {
      myMinPts <- input$HDBSCAN_minPts
    } else {
      myMinPts <- 20
    }
      HDBSCAN_result <- dbscan::hdbscan(
        x = clusters_space,
        minPts = myMinPts)

      # On représente les outliers par des <NA> et non des zéros.
      HDBSCAN_result$cluster[HDBSCAN_result$cluster == 0] <- NA

      # On réassigne le partitionnement dans l'ordre des effectifs.
      partition <- HDBSCAN_result$cluster
      new_order <- partition %>% table %>% sort(decreasing = TRUE) %>%
        names %>% as.character
      new_order <- 1:length(new_order) %>% set_names(new_order)
      # Les noms de <new_order> représentent les anciens clusters
      #   et la liste de <new_order> représente les nouveaux clusters,
      #   donc on envoie l'ancienne partition dans les noms de <new_order>
      #   pour récupérer le nouvel ordre dans la liste.
      HDBSCAN_result$cluster <- new_order[partition %>% as.character]

      names(HDBSCAN_result$cluster_scores) <- new_order[
        names(HDBSCAN_result$cluster_scores) %>% as.character]

      return(HDBSCAN_result)
  })


  # ----- __nbClusters -----
  get_HDBSCANnbClusters <- reactive({
    HDBSCAN_result <- run_HDBSCAN()

    if (is.null(HDBSCAN_result)) return(NULL)

    nc <- HDBSCAN_result$cluster %>% unique %>% length %>% magrittr::subtract(1)
    return(nc)
  })


  # ----- __plot_HDBSCAN -----
  plot_HDBSCAN <- reactive({
    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) return(plot.new())

    HDBSCAN_result <- run_HDBSCAN()

    if (is.null(HDBSCAN_result)) return(plot.new())

    # On ne mélange que 8 couleurs.
    partition <- HDBSCAN_result$cluster %% 8

    HDBSCAN_space <- data.frame(
      cluster = partition,
      x = clusters_data$x,
      y = clusters_data$y
    ) %>%
      dplyr::arrange(cluster) %>%
      dplyr::mutate(cluster = ifelse(cluster == 0, NA, cluster))

    p <- HDBSCAN_space %>% plot_clusters()

    return(p)
  })


  # ----- HDBSCAN_output -----


  # ----- __info -----
  output$HDBSCAN_info <- renderUI({
    get_clusters_info()
  })

  # ----- __nbClusters -----
  output$HDBSCAN_nbClusters <- renderUI({
    mySubTitle <- "Found"

    nc <- get_HDBSCANnbClusters()

    if (!is.null(nc)) {
      if (nc == 1) nc <- "1 cluster"
      else nc <- paste(nc, "clusters")
    }


    myValueBox <- valueBox(
      value = nc,
      subtitle = mySubTitle,
      icon = icon("chart-bar"),
      color = "red",
      width = 12
    )
    return(myValueBox)
  })

  # ----- __silhouette -----
  output$HDBSCAN_silhouette <- renderUI({
    mySubTitle <- HTML(paste("Silhouette Score", sep = ""))

    clusters_data <- get_clusters_data()
    HDBSCAN_result <- run_HDBSCAN()

    if (is.null(HDBSCAN_result) | is.null(clusters_data)) {
      silhouette_score <- NULL
    } else {
      if ((HDBSCAN_result$cluster %>% unique() %>% length()) == 1) {
        silhouette_score <- NULL
      } else {
        silhouette_summary <- cluster::silhouette(
          HDBSCAN_result$cluster[!is.na(HDBSCAN_result$cluster)],
          clusters_data %>%
            dplyr::select(x, y) %>%
            dplyr::filter(!is.na(HDBSCAN_result$cluster)) %>%
            dist()
        ) %>% summary()
        silhouette_score <- silhouette_summary$si.summary[["Mean"]] %>%
          magrittr::multiply_by(100) %>% round(1) %>% paste("%")
      }
    }

    myValueBox <- valueBox(
      value = silhouette_score,
      subtitle = mySubTitle,
      icon = icon("chart-bar"),
      color = "red",
      width = 12
    )
    return(myValueBox)
  })

  # ----- __plot -----
  output$HDBSCAN_plot <- renderPlot({
    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) return(plot.new())

    HDBSCAN_result <- run_HDBSCAN()

    if (is.null(HDBSCAN_result)) return(plot.new())

    # On ne mélange que 8 couleurs.
    partition <- HDBSCAN_result$cluster %% 8

    HDBSCAN_space <- data.frame(
      cluster = partition,
      x = clusters_data$x,
      y = clusters_data$y
    ) %>%
      dplyr::arrange(cluster) %>%
      dplyr::mutate(cluster = ifelse(cluster == 0, NA, cluster))

    p <- HDBSCAN_space %>% plot_clusters() +
      ggplot2::coord_fixed(
        xlim = c(min(HDBSCAN_space$x), max(HDBSCAN_space$x)),
        ylim = c(min(HDBSCAN_space$y), max(HDBSCAN_space$y))
      ) +
      ggplot2::labs(title = "General View")

    # if (input$HDBSCAN_density == TRUE) {
    #   HDBSCAN_selected <- HDBSCAN_space
    #
    #   n_captured_points <- nrow(HDBSCAN_selected)
    #
    #   if (n_captured_points > 0) {
    #     circle_points <- (2000 / n_captured_points) %>% round() %>%
    #       max(10) %>% min(100)
    #     circle_base <- circleFun(
    #       center = c(0,0),
    #       r = HDBSCAN_result$eps,
    #       npoints = circle_points
    #     ) %>% dplyr::slice(rep(1:circle_points, n_captured_points))
    #     circle_points <- HDBSCAN_selected %>%
    #       dplyr::slice(
    #         plyr::llply(1:n_captured_points, rep, circle_points) %>% unlist()
    #       )
    #     circle_points$x <- circle_points$x + circle_base$x
    #     circle_points$y <- circle_points$y + circle_base$y
    #
    #     p <- p + ggplot2::geom_point(
    #       mapping = ggplot2::aes(x = x, y = y, color = as.factor(cluster)),
    #       data = circle_points,
    #       size = 0.1,
    #       alpha = 0.5
    #     )
    #   }
    # }

    return(p)
  })

  ranges_HDBSCAN <- reactiveValues(x = NULL, y = NULL)

  observe({
    brush <- input$HDBSCAN_brush
    if (!is.null(brush)) {
      ranges_HDBSCAN$x <- c(brush$xmin, brush$xmax)
      ranges_HDBSCAN$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges_HDBSCAN$x <- NULL
      ranges_HDBSCAN$y <- NULL
    }
  })

  ranges_2_HDBSCAN <- reactiveValues(x = NULL, y = NULL)

  observe({
    brush <- input$HDBSCAN_brush_2
    if (!is.null(brush)) {
      ranges_2_HDBSCAN$x <- c(brush$xmin, brush$xmax)
      ranges_2_HDBSCAN$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges_2_HDBSCAN$x <- NULL
      ranges_2_HDBSCAN$y <- NULL
    }
  })

  # ----- __plot_2 -----
  output$HDBSCAN_plot_2 <- renderPlot({
    if (input$HDBSCAN_plotChoice == "init") {
      p <- get_clusters_plot() +
        ggplot2::coord_fixed() +
        ggplot2::labs(title = "Initial clusters")
      return(p)
    }

    if (input$HDBSCAN_plotChoice == "zoom") {
      if (is.null(ranges_HDBSCAN$x) | is.null(ranges_HDBSCAN$y)) {
        message <- "Waiting for zoom."
        p <- ggplot2::ggplot(
        ) + ggplot2::annotate(
          "text", x = 0, y = 0, size = 8, label = message
        ) + ggplot2::theme_void()
        return(p)
      } else {
        p <- plot_HDBSCAN(
        ) + ggplot2::coord_cartesian(
          xlim = ranges_HDBSCAN$x,
          ylim = ranges_HDBSCAN$y,
          expand = FALSE
        ) + ggplot2::labs(
          title = "Zoom"
        )
      }

      clusters_data <- get_clusters_data()

      if (is.null(clusters_data)) return(plot.new())

      HDBSCAN_result <- run_HDBSCAN()

      if (is.null(HDBSCAN_result)) return(plot.new())

      # On ne mélange que 8 couleurs.
      partition <- HDBSCAN_result$cluster %% 8

      HDBSCAN_space <- data.frame(
        cluster = partition,
        x = clusters_data$x,
        y = clusters_data$y
      ) %>%
        dplyr::arrange(cluster) %>%
        dplyr::mutate(cluster = ifelse(cluster == 0, NA, cluster))

      if (!is.null(ranges_2_HDBSCAN$x) & !is.null(ranges_2_HDBSCAN$y)) {

        HDBSCAN_selected <- HDBSCAN_space %>%
          dplyr::filter(
            x > ranges_2_HDBSCAN$x[[1]] &
              x < ranges_2_HDBSCAN$x[[2]] &
              y > ranges_2_HDBSCAN$y[[1]] &
              y < ranges_2_HDBSCAN$y[[2]]
          )

        n_captured_points <- nrow(HDBSCAN_selected)

        if (n_captured_points > 0) {
          circle_points <- (3000 / n_captured_points) %>% round() %>%
            max(10) %>% min(200)
          circle_base <- circleFun(
            center = c(0,0),
            r = HDBSCAN_result$eps,
            npoints = circle_points
          ) %>% dplyr::slice(rep(1:circle_points, n_captured_points))
          circle_points <- HDBSCAN_selected %>%
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
    }

    return(plot.new())
  })



  # ----- hierarchical_reac -----

  # ----- __results -----

  get_hierarchical_results <- reactive({
    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) {
      return(NULL)
    }

    clusters_space <- clusters_data %>%
      dplyr::select(x, y)


    nbSteps <- 7

    withProgress(
      message = 'Optimize kMeans.',
      value = 0,
      min = 0,
      max = 1,
      {


        # Les scores sont calculés pour chaque méthode.
        all_results <- list(
          "single",
          "average",
          "median",
          "complete",
          "centroid",
          "ward.D2"
        ) %>%
          magrittr::set_names(x = ., value = .) %>%
          plyr::llply(function(myMethod) {
            incProgress(
              amount = 1 / nbSteps,
              detail = paste("Trying", myMethod))
            results <- NbClust::NbClust(

              data = clusters_space,
              min.nc = 2,
              max.nc = 8,
              method = myMethod,
              index = "silhouette"
            )
            append(results, c(method = myMethod))
          })

        incProgress(
          amount = 1 / nbSteps,
          detail = "Merging results")

        All.index <- plyr::ldply(all_results, function(results) {
          data.frame(
            nc = names(results$All.index),
            index = results$All.index
          )
        }, .id = "method") %>%
          dplyr::mutate(nc = nc %>% as.character() %>% as.integer())

        Best.nc <- plyr::ldply(all_results, function(results) {
          data.frame(
            nc = names(results$Best.nc),
            index = results$Best.nc
          )
        }, .id = "method")


        Best.partition <- plyr::llply(all_results, function(results) {
          results$Best.partition
        })

        all_results <- list(
          All.index = All.index,
          Best.nc = Best.nc,
          Best.partition = Best.partition
        )
      })

    return(all_results)
  })


  # ----- __run_hierarchical -----
  run_hierarchical <- reactive({

    if (input$hierarchical_panel1 == "Manual Run") {
      myMethod <- input$hierarchical_method

      if (is.null(myMethod)) {
        return(NULL)
      }

      if (input$hierarchical_nbClustersAuto == TRUE)
      {
        all_results <- get_hierarchical_results()

        if (is.null(all_results)) return(NULL)

        best_choice <- all_results$All.index %>%
          dplyr::filter(method == myMethod) %>%
          dplyr::filter(index == max(index))

        myNc <- best_choice$nc[[1]]
        myIndex <- best_choice$index[[1]]

        myPartition <- all_results$Best.partition[[myMethod]]

        hierarchical_results <- list(
          nc = myNc,
          index = myIndex,
          method = myMethod,
          partition = myPartition
        )
      } else {
        myMinNc <- input$hierarchical_nbClusters
        myMaxNc <- input$hierarchical_nbClusters

        if (is.null(myMinNc) | is.null(myMaxNc)) {
          return(NULL)
        }

        clusters_data <- get_clusters_data()

        if (is.null(clusters_data)) {
          return(NULL)
        }

        clusters_space <- clusters_data %>%
          dplyr::select(x, y)

        temp_results <- NbClust::NbClust(
          data = clusters_space,
          min.nc = myMinNc,
          max.nc = myMaxNc,
          method = myMethod,
          index = "silhouette"
        )

        hierarchical_results <- list(
          nc = temp_results$Best.nc[["Number_clusters"]],
          index = temp_results$Best.nc[["Value_Index"]],
          method = myMethod,
          partition = temp_results$Best.partition
        )
      }
    } else {
      all_results <- get_hierarchical_results()

      if (is.null(all_results)) return(NULL)

      best_choice <- all_results$All.index %>%
        dplyr::filter(index == max(index))
      myNc <- best_choice$nc[[1]]
      myIndex <- best_choice$index[[1]]
      myMethod <- best_choice$method[[1]]
      myPartition <- all_results$Best.partition[[myMethod]]

      hierarchical_results <- list(
        nc = myNc %>% as.integer(),
        index = myIndex,
        method = myMethod %>%
          as.character(),
        partition = myPartition
      )
    }

    return(hierarchical_results)
  })


  # ----- __plot_heatmap -----
  plot_hierarchical_heatmap <- reactive({
    all_results <- get_hierarchical_results()

    if (is.null(all_results)) return(plot.new())

    myColours <- c(
      "darkred", "red3", "red",
      "orangered", "gold2",
      "forestgreen", "darkgreen")
    names(myColours) <-
      c(0, 0.1, 0.2,
        0.4, 0.5,
        0.8, 1)
    xBreaks <- c(
      Single = "single",
      Average = "average",
      Median = "median",
      Complete = "complete",
      Centroid = "centroid",
      Ward = "ward.D2")

    p <- ggplot2::ggplot(
      data = all_results$All.index,
      mapping = ggplot2::aes(
        x = method,
        y = nc,
        fill = index
      )
    ) + ggplot2::geom_tile(
    ) + ggplot2::scale_fill_gradientn(
      colours = myColours,
      values = names(myColours)
    ) + ggplot2::scale_x_discrete(
      breaks = xBreaks %>% unname(),
      limits = xBreaks %>% unname(),
      labels = xBreaks %>% names(),
      expand = c(0,0)
    ) + ggplot2::scale_y_discrete(
      breaks = 2:8,
      limits = 2:8,
      expand = c(0,0)
    ) + ggplot2::labs(
      x = "Hierarchical Method",
      y = "Number of Expected Clusters",
      fill = "Silhouette Score"
    ) + ggplot2::theme(
      legend.position = "bottom",
      legend.key.size = ggplot2::unit(1, "cm"),
      legend.title = ggplot2::element_text(vjust = 0.9),
      legend.text = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
    )

    return(p)
  })



  # ----- __plot_hierarchical -----
  plot_hierarchical <- reactive({
    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) return(plot.new())

    hierarchical_results <- run_hierarchical()

    if (is.null(hierarchical_results)) return(plot.new())

    hierarchical_space <- data.frame(
      cluster = hierarchical_results$partition,
      x = clusters_data$x,
      y = clusters_data$y
    ) %>%
      dplyr::arrange(cluster)

    p <- hierarchical_space %>% plot_clusters()

    return(p)
  })


  # ----- hierarchical_output -----


  # ----- __nbClustersAutoUI -----
  output$hierarchical_nbClustersAutoUI <- renderUI({
    if (!is.null(input$hierarchical_nbClustersAuto)) {
      if (!input$hierarchical_nbClustersAuto == TRUE) {
        return(
          sliderTextInput(
            inputId = "hierarchical_nbClusters",
            label = "Manual Expected clusters",
            choices = 2:8,
            selected = 3,
            grid = TRUE
          )
        )
      }
    }

    return(NULL)
  })


  # ----- __info -----
  output$hierarchical_info <- renderUI({
    get_clusters_info()
  })

  # ----- __nbClusters -----
  output$hierarchical_nbClusters <- renderUI({
    mySubTitle <- "Found"

    hierarchical_results <- run_hierarchical()

    if (is.null(hierarchical_results)) {
      nc <- NULL
    } else {
      nc <- hierarchical_results$nc[[1]] %>%
        paste("clusters")

    }

    myValueBox <- valueBox(
      subtitle = mySubTitle,
      value = nc,
      icon = icon("chart-bar"),
      color = "red",
      width = 16
    )
    return(myValueBox)
  })

  # ----- __method -----
  output$hierarchical_method <- renderUI({
    mySubTitle <- HTML(paste("Hierarchical Method", sep = ""))

    hierarchical_results <- run_hierarchical()

    if (is.null(hierarchical_results)) {
      method <- NULL
    } else {
      method <- hierarchical_results$method[[1]]

    }

    myValueBox <- valueBox(
      subtitle = mySubTitle,
      value = method,
      icon = icon("chart-bar"),
      color = "red",
      width = 12
    )
    return(myValueBox)
  })

  # ----- __silhouette -----
  output$hierarchical_silhouette <- renderUI({
    mySubTitle <- HTML(paste("Silhouette Score", sep = ""))

    hierarchical_results <- run_hierarchical()

    if (is.null(hierarchical_results)) {
      silhouette_score <- NULL
    } else {
      silhouette_score <- hierarchical_results$index[[1]] %>%
        magrittr::multiply_by(100) %>% round(1) %>% paste("%")
    }

    myValueBox <- valueBox(
      subtitle = mySubTitle,
      value = silhouette_score,
      icon = icon("chart-bar"),
      color = "red",
      width = 12
    )
    return(myValueBox)
  })

  # ----- __plot -----
  output$hierarchical_plot <- renderPlot({
    p <- plot_hierarchical()

    if (is.null(p)) return(plot.new())

    p <- p +
      ggplot2::coord_fixed() +
      ggplot2::labs(title = "General View")

    return(p)
  })

  ranges_hierarchical <- reactiveValues(x = NULL, y = NULL)

  observe({
    brush <- input$hierarchical_brush
    if (!is.null(brush)) {
      ranges_hierarchical$x <- c(brush$xmin, brush$xmax)
      ranges_hierarchical$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges_hierarchical$x <- NULL
      ranges_hierarchical$y <- NULL
    }
  })

  # ----- __plot_2 -----
  output$hierarchical_plot_2 <- renderPlot({
    if (input$hierarchical_plotChoice == "init") {
      p <- get_clusters_plot() +
        ggplot2::coord_fixed() +
        ggplot2::labs(title = "Initial clusters")
      return(p)
    } else if (input$hierarchical_plotChoice == "zoom") {
      if (is.null(ranges_hierarchical$x) | is.null(ranges_hierarchical$y)) {
        message <- "Waiting for zoom."
        p <- ggplot2::ggplot(
        ) + ggplot2::annotate(
          "text", x = 0, y = 0, size = 8, label = message
        ) + ggplot2::theme_void()
        return(p)
      } else {
        p <- plot_hierarchical(
        ) + ggplot2::coord_cartesian(
          xlim = ranges_hierarchical$x,
          ylim = ranges_hierarchical$y,
          expand = FALSE
        ) + ggplot2::labs(
          title = "Zoom"
        )
        return(p)
      }
    }

    if (input$hierarchical_plotChoice == "heatmap") {
      p <- plot_hierarchical_heatmap()
      return(p)
    }

    return(plot.new())
  })




  # ----- modelbased_reac -----

  # ----- __run_modelbased -----
  run_modelbased <- reactive({
    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) {
      return(NULL)
    }

    clusters_space <- clusters_data %>%
      dplyr::select(x, y)

    modelbased_results <- mclust::Mclust(data = clusters_space, verbose = FALSE)

    return(modelbased_results)
  })



  # ----- __plot_modelbased -----
  plot_modelbased <- reactive({
    clusters_data <- get_clusters_data()

    if (is.null(clusters_data)) return(plot.new())

    modelbased_results <- run_modelbased()

    if (is.null(modelbased_results)) return(plot.new())

    modelbased_space <- data.frame(
      cluster = modelbased_results$classification,
      x = clusters_data$x,
      y = clusters_data$y
    ) %>%
      dplyr::arrange(cluster)

    p <- modelbased_space %>% plot_clusters()

    if (!is.null(input$modelbased_centers)) {
      if (input$modelbased_centers) {
        centers_space <- modelbased_results$parameters$mean %>%
          t() %>% as.data.frame() %>% cbind(cluster = 1:nrow(.))
        p <- p + ggplot2::geom_point(
          mapping = ggplot2::aes(x = x, y = y, fill = as.factor(cluster)),
          data = centers_space,
          size = 5,
          shape = 22
        )
      }
    }

    if (!is.null(input$modelbased_ellipses)) {
      if (input$modelbased_ellipses) {
        centers_space <- modelbased_results$parameters$mean %>%
          t() %>% as.data.frame() %>% cbind(cluster = 1:nrow(.))
        ellipse_df <- modelbased_results$parameters$variance$sigma %>%
          reshape2::melt(
            varnames = c("Coord", "Coord2", "cluster")) %>%
          dplyr::select(-Coord2) %>%
          dplyr::filter(value != 0) %>%
          reshape2::dcast(cluster ~ Coord) %>%
          dplyr::mutate(x = sqrt(x), y = sqrt(y)) %>%
          dplyr::rename(a = x, b = y) %>%
          dplyr::left_join(
            centers_space %>%
              dplyr::rename(x0 = x, y0 = y),
            by = "cluster")

        p <- p + ggforce::geom_ellipse(
          mapping = ggplot2::aes(x0 = x0, y0 = y0, a = a, b = b,
                                 angle = 0,
                                 fill = as.factor(cluster)),
          data = ellipse_df,
          size = 1,
          alpha = 0.2)
      }
    }


    if (!is.null(input$modelbased_centers) |
        !is.null(input$modelbased_ellipses)) {
      if (input$modelbased_centers |
          input$modelbased_ellipses) {
        p <- p + ggplot2::scale_fill_brewer(
          palette = "Dark2"
        )
      }
    }

    return(p)
  })


  # ----- modelbased_output -----


  # ----- __info -----
  output$modelbased_info <- renderUI({
    get_clusters_info()
  })

  # ----- __nbClusters -----
  output$modelbased_nbClusters <- renderUI({
    mySubTitle <- "Found"

    modelbased_results <- run_modelbased()

    if (is.null(modelbased_results)) {
      nc <- NULL
    } else {
      nc <- modelbased_results$G %>%
        paste("clusters")
    }

    myValueBox <- valueBox(
      subtitle = mySubTitle,
      value = nc,
      icon = icon("chart-bar"),
      color = "red",
      width = 16
    )
    return(myValueBox)
  })

  # ----- __silhouette -----
  output$modelbased_silhouette <- renderUI({
    mySubTitle <- "Silhouette Score"

    clusters_data <- get_clusters_data()
    modelbased_results <- run_modelbased()

    if (is.null(clusters_data) | is.null(modelbased_results)) {
      silhouette_score <- NULL
    } else if (length(unique(modelbased_results$classification)) <= 1) {
      silhouette_score <- 0 %>% paste("%")
    } else {
      silhouette_summary <- cluster::silhouette(
        modelbased_results$classification,
        clusters_data %>%
          dplyr::select(x, y) %>%
          dist()
      ) %>% summary()
      silhouette_score <- silhouette_summary$si.summary[["Mean"]] %>%
        magrittr::multiply_by(100) %>% round(1) %>% paste("%")
    }

    myValueBox <- valueBox(
      value = silhouette_score,
      subtitle = mySubTitle,
      icon = icon("chart-bar"),
      color = "red",
      width = 12
    )
    return(myValueBox)
  })

  # ----- __plot -----
  output$modelbased_plot <- renderPlot({
    p <- plot_modelbased()

    if (is.null(p)) return(plot.new())

    p <- p +
      ggplot2::coord_fixed() +
      ggplot2::labs(title = "General View")

    return(p)
  })

  ranges_modelbased <- reactiveValues(x = NULL, y = NULL)

  observe({
    brush <- input$modelbased_brush
    if (!is.null(brush)) {
      ranges_modelbased$x <- c(brush$xmin, brush$xmax)
      ranges_modelbased$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges_modelbased$x <- NULL
      ranges_modelbased$y <- NULL
    }
  })

  # ----- __plot_2 -----
  output$modelbased_plot_2 <- renderPlot({
    if (input$modelbased_plotChoice == "init") {
      p <- get_clusters_plot() +
        ggplot2::coord_fixed() +
        ggplot2::labs(title = "Initial clusters")
      return(p)
    } else if (input$modelbased_plotChoice == "zoom") {
      if (is.null(ranges_modelbased$x) | is.null(ranges_modelbased$y)) {
        message <- "Waiting for zoom."
        p <- ggplot2::ggplot(
        ) + ggplot2::annotate(
          "text", x = 0, y = 0, size = 8, label = message
        ) + ggplot2::theme_void()
        return(p)
      } else {
        p <- plot_modelbased(
        ) + ggplot2::coord_cartesian(
          xlim = ranges_modelbased$x,
          ylim = ranges_modelbased$y,
          expand = FALSE
        ) + ggplot2::labs(
          title = "Zoom"
        )
        return(p)
      }
    }

    if (input$modelbased_plotChoice %in% c("classification",
                                           "uncertainty",
                                           "density")) {

      modelbased_results <- run_modelbased()

      if (is.null(modelbased_results)) {
        return(plot.new())
      } else {
        p <- modelbased_results %>%
          mclust::plot.Mclust(what = input$modelbased_plotChoice)

        return(p)
      }
    }
    return(plot.new())
  })


}
