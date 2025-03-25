determineMaxLLBottleneck <- function(mutation_matrix,
                                     type = "overall",
                                     createPlot = FALSE) {
  sample <- mutation_matrix %>%
    dplyr::rename(ClonalMu = clonalMu)
  
  df_meanNbLL <- LL_meanNb.df(
    df = sample,
    listClonal = listClonal,
    lambda_values = seq(0.01, 4.00, by = 0.1),
    R0 = 11.1,
    mu_values = seq(0.02, 5, by = 0.01),
    maxMuGen = 50,
    maxFS = 50,
    maxIni = 8
  )
  
  maxLL_bottleneck <- df_meanNbLL[which(df_meanNbLL$prob == max(df_meanNbLL$prob, na.rm = TRUE)), ]$meanNb
  maxLL_mu <- df_meanNbLL[which(df_meanNbLL$prob == max(df_meanNbLL$prob, na.rm = TRUE)), ]$mu
  maxLL_lambda <- df_meanNbLL[which(df_meanNbLL$prob == max(df_meanNbLL$prob, na.rm = TRUE)), ]$lambda
  maxLL_meanN <- df_meanNbLL[which(df_meanNbLL$prob == max(df_meanNbLL$prob, na.rm = TRUE)), ]$meanN
  
  
  max <- tibble(
    meanNb = maxLL_bottleneck,
    mu = maxLL_mu,
    lambda = maxLL_lambda,
    meanN = maxLL_meanN,
    id = "max"
  )
  
  out <- full_join(max, df_meanNbLL) %>%
    mutate(type = type)
  
  if (createPlot == TRUE) {
    a <- df_meanNbLL %>%
      ggplot() +
      metR::geom_contour_fill(aes(x = lambda, y = mu, z = prob),
                              na.fill = TRUE,
                              bins = 400) +
      theme_bw(base_size = 24) +
      scale_fill_viridis_c(
        na.value = "transparent",
        limits = c(-142.8, -130.4),
        breaks = seq(-142.8, -130.4, 6)
      ) +
      geom_hline(
        yintercept = maxLL_mu,
        lty = 2,
        col = "red",
        lwd = 1.5
      ) +
      geom_vline(
        xintercept = maxLL_lambda,
        lty = 2,
        col = "red",
        lwd = 1.5
      ) +
      annotate(
        geom = "text",
        y = 0.5,
        x = maxLL_lambda + 0.8,
        label = expression(lambda ~ "\u2192" ~ 0),
        size = 10,
        col = "red"
      ) +
      annotate(
        geom = "text",
        x = 3,
        y = maxLL_mu + 0.2,
        label = bquote(mu == .(maxLL_mu)),
        size = 10,
        col = "red"
      ) +
      labs(x = bquote(lambda),
           y = bquote(mu),
           fill = element_blank()) +
      coord_fixed() +
      scale_y_continuous(expand = c(0, 0)) +
      guides(fill = guide_colorbar(barheight = 20))
    
    b <- df_meanNbLL %>%
      ggplot(aes(x = meanN, y = mu, z = prob)) +
      geom_contour_fill(na.fill = TRUE, bins = 400) +
      geom_vline(
        xintercept = maxLL_meanN,
        lty = 2,
        col = "red",
        lwd = 1.5
      ) +
      annotate(
        geom = "text",
        y = 0.5,
        x = maxLL_meanN + 1.5,
        label = bquote(italic(bar(N)) == .(round(
          maxLL_meanN, digits = 2
        ))),
        size = 10,
        col = "red"
      ) +
      theme_bw(base_size = 24) +
      scale_fill_viridis_c(
        na.value = "transparent",
        limits = c(-142.8, -130.4),
        breaks = seq(-142.8, -130.4, 6)
      ) +
      scale_x_continuous(limits = c(0, 4)) +
      labs(x = bquote(italic(bar(N))),
           y = expression(mu),
           fill = element_blank()) +
      coord_fixed() +
      scale_y_continuous(expand = c(0, 0)) +
      guides(fill = guide_colorbar(barheight = 20))
    
    c <- df_meanNbLL %>%
      ggplot(aes(x = meanNb, y = mu, z = prob)) +
      geom_contour_fill(na.fill = TRUE, bins = 400) +
      geom_vline(
        xintercept = maxLL_bottleneck,
        lty = 2,
        col = "red",
        lwd = 1.5
      ) +
      annotate(
        geom = "text",
        y = 0.5,
        x = maxLL_lambda + 1.9,
        label = bquote(italic(bar(N))[b] == .(round(
          maxLL_bottleneck, digits = 2
        ))),
        size = 10,
        col = "red"
      ) +
      theme_bw(base_size = 24) +
      scale_fill_viridis_c(
        na.value = "transparent",
        limits = c(-142.8, -130.4),
        breaks = seq(-142.8, -130.4, 6)
      ) +
      scale_x_continuous(limits = c(0, 4)) +
      labs(x = expression(italic(bar(N))[b]),
           y = expression(mu),
           fill = element_blank()) +
      coord_fixed() +
      scale_y_continuous(expand = c(0, 0)) +
      guides(fill = guide_colorbar(barheight = 20))
    
    overall_plot <- a + b + c + plot_layout(guides = "collect")
    
    ggsave(
      filename = "/Users/katykrupinsky/git/FluTES_bottleneck/05 - Figures and Tables/clonal/overall_plot.png",
      plot = overall_plot,
      width = 16.9,
      height = 5.65
    )
  }
  return(out)
}
