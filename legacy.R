# ----- kMeans -----
# ----- __summary -----

# get_kMeans_summary <- reactive({
#   indic_df <- get_kMeans_indic()
#
#   if (is.null(indic_df)) {
#     return(NULL)
#   }
#
#   indic_summary <- indic_df %>%
#     dplyr::group_by(nbCenters.col) %>%
#     dplyr::summarise(
#       silhouette_score.col = mean(silhouette_score.col),
#       handled_variation.col = mean(handled_variation.col))
# })


# ----- __varHist_plot -----
# get_varHist_plot <- reactive({
#   indic_summary <- get_kMeans_summary()
#
#   df_to_plot <- indic_summary %>%
#     reshape2::melt(id.vars = "nbCenters.col")
#
#   myMax <- df_to_plot %>%
#     dplyr::filter(variable == "silhouette_score.col") %>%
#     dplyr::filter(value == max(value)) %>%
#     head(1)
#   x_myMax <- myMax[["nbCenters.col"]]
#   y_myMax <- myMax[["value"]]
#   myMax_to_plot <- data.frame(
#     x = c(x_myMax, 1),
#     xend = c(x_myMax, x_myMax),
#     y = c(0, y_myMax),
#     yend = c(y_myMax, y_myMax)
#   )
#
#   p <- ggplot2::ggplot(
#     data = df_to_plot
#   ) + ggplot2::geom_line(
#     mapping = ggplot2::aes(
#       x = nbCenters.col,
#       y = value,
#       color = variable
#     ),
#     size = 2
#   ) + ggplot2::geom_segment(
#     data = myMax_to_plot,
#     mapping = ggplot2::aes(
#       x = x,
#       y = y,
#       xend = xend,
#       yend = yend),
#     linetype = "dashed",
#     color = "orangered"
#   ) + ggplot2::labs(
#     x = "Expected clusters",
#     y = ""
#   ) + ggplot2::scale_x_continuous(
#     breaks = 1:8,
#     limits = c(1,8),
#     expand = c(0,0)
#   ) + ggplot2::scale_y_continuous(
#     limits = c(0,1),
#     expand = c(0,0)
#   ) + ggplot2::scale_colour_manual(
#     name = "Indicator",
#     limits = c("silhouette_score.col", "handled_variation.col"),
#     values = c("orangered", "darkgreen"),
#     labels = c("Silhouette Score", "Handled Variation"),
#     na.value = "gray75"
#   ) + ggplot2::theme_bw(
#   ) + ggplot2::theme(
#     axis.title.y = ggplot2::element_blank(),
#     legend.position = "bottom"
#   )
#
#   return(p)
# })


# ----- __seedHist_plot -----
# get_seedHist_plot <- reactive({
#   nbCenters <- input$kMeans_nbCenters
#
#   if (is.null(nbCenters) | nbCenters == 1) {
#     return(plot.new())
#   } else {
#     indic_df <- get_kMeans_indic()
#
#     df_to_plot <- indic_df %>%
#       dplyr::filter(nbCenters.col == nbCenters) %>%
#       dplyr::select(-nbCenters.col) %>%
#       reshape2::melt(id.vars = "kMeans_seed.col")
#
#     myMax <- df_to_plot %>%
#       dplyr::filter(variable == "silhouette_score.col") %>%
#       dplyr::filter(value == max(value)) %>%
#       head(1)
#     x_myMax <- myMax[["kMeans_seed.col"]]
#     y_myMax <- myMax[["value"]]
#     if (x_myMax == 1) {
#       myMax_to_plot <- data.frame(
#         x = c(x_myMax, 20),
#         xend = c(x_myMax, x_myMax),
#         y = c(0, y_myMax),
#         yend = c(y_myMax, y_myMax)
#       )
#     } else {
#       myMax_to_plot <- data.frame(
#         x = c(x_myMax, 1),
#         xend = c(x_myMax, x_myMax),
#         y = c(0, y_myMax),
#         yend = c(y_myMax, y_myMax)
#       )
#     }
#
#     p <- ggplot2::ggplot(
#       data = df_to_plot
#     ) + ggplot2::geom_line(
#       mapping = ggplot2::aes(
#         x = kMeans_seed.col,
#         y = value,
#         color = variable
#       ),
#       size = 2
#     ) + ggplot2::geom_segment(
#       data = myMax_to_plot,
#       mapping = ggplot2::aes(
#         x = x,
#         y = y,
#         xend = xend,
#         yend = yend),
#       linetype = "dashed",
#       color = "orangered"
#     ) + ggplot2::labs(
#       x = "Seed",
#       y = ""
#     ) + ggplot2::scale_x_continuous(
#       breaks = 1:20,
#       limits = c(1,20),
#       expand = c(0,0)
#     ) + ggplot2::scale_y_continuous(
#       limits = c(0,1),
#       expand = c(0,0)
#     ) + ggplot2::scale_colour_manual(
#       name = "Indicator",
#       limits = c("silhouette_score.col", "handled_variation.col"),
#       values = c("orangered", "darkgreen"),
#       labels = c("Silhouette Score", "Handled Variation"),
#       na.value = "gray75"
#     ) + ggplot2::theme_bw(
#     ) + ggplot2::theme(
#       axis.title.y = ggplot2::element_blank(),
#       legend.position = "bottom"
#     )
#
#     return(p)
#   }
# })
