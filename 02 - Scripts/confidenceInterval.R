library(tidyverse)
library(plotly)

raw <- read_csv(
  "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/Clonal_data/R0_sensitivity/bottlenecks_11.1.csv"
)

factors <- unique(raw$type)

make3dCiPlot <- function(raw = raw,
                         factor,
                         confidence = TRUE) {
  df <- raw %>%
    dplyr::filter(type == factor) %>%
    dplyr::select(meanNb, lambda, mu, prob, id) %>%
    dplyr::rename(theta = meanNb, ll = prob)
  
  maxLL <- max(df$ll)
  maxMuLL <- as_vector(df[df$ll == maxLL, "mu"])
  
  LL_tibble <- df %>%
    filter(mu == maxMuLL) %>%
    arrange(mu)
  
  Max_LL <- max(LL_tibble$ll)
  Max_LL_bottleneck_index <- which(LL_tibble$ll == max(LL_tibble$ll))
  Max_LL_bottleneck <- LL_tibble$theta[Max_LL_bottleneck_index]
  likelihood_ratio <- qchisq(0.95, df = 1)
  
  ci_tibble <- LL_tibble %>%
    filter(2 * (Max_LL - ll) < likelihood_ratio)
  lower_CI_bottleneck <- min(ci_tibble$theta)
  upper_CI_bottleneck <- max(ci_tibble$theta)
  
  plane <- expand.grid(
    lambda = seq(min(LL_tibble$lambda), max(LL_tibble$lambda), length.out = 20),
    theta = seq(min(LL_tibble$theta), max(LL_tibble$theta), length.out = 20)
  ) %>%
    mutate(ll = Max_LL - 0.5 * likelihood_ratio)
  
  plane_matrix <- list(
    x = matrix(plane$theta, nrow = 20),
    y = matrix(plane$lambda, nrow = 20),
    z = matrix(plane$ll, nrow = 20)
  )
  
  uci_plane <- expand.grid(
    lambda = seq(min(LL_tibble$lambda), max(LL_tibble$lambda), length.out = 20),
    ll = seq(min(LL_tibble$ll), max(LL_tibble$ll), length.out = 20)
  ) %>%
    mutate(theta = upper_CI_bottleneck)
  uci_plane_matrix <- list(
    x = matrix(uci_plane$theta, nrow = 20),
    y = matrix(uci_plane$lambda, nrow = 20),
    z = matrix(uci_plane$ll, nrow = 20)
  )
  
  lci_plane <- expand.grid(
    lambda = seq(min(LL_tibble$lambda), max(LL_tibble$lambda), length.out = 20),
    ll = seq(min(LL_tibble$ll), max(LL_tibble$ll), length.out = 20)
  ) %>%
    mutate(theta = lower_CI_bottleneck)
  lci_plane_matrix <- list(
    x = matrix(lci_plane$theta, nrow = 20),
    y = matrix(lci_plane$lambda, nrow = 20),
    z = matrix(lci_plane$ll, nrow = 20)
  )
  
  max_plane <- expand.grid(
    lambda = seq(min(LL_tibble$lambda), max(LL_tibble$lambda), length.out = 20),
    ll = seq(min(LL_tibble$ll), max(LL_tibble$ll), length.out = 20)
  ) %>%
    mutate(theta = Max_LL_bottleneck)
  max_plane_matrix <- list(
    x = matrix(max_plane$theta, nrow = 20),
    y = matrix(max_plane$lambda, nrow = 20),
    z = matrix(max_plane$ll, nrow = 20)
  )
  if (confidence == TRUE) {
    plot_ly(LL_tibble) %>%
      add_trace(
        x = ~ theta,
        y = ~ lambda,
        z = ~ ll,
        type = "scatter3d",
        mode = "lines+markers",
        line = list(width = 5, color = "black"),
        marker = list(size = 4, color = "black")
      ) %>%
      add_surface(
        x = plane_matrix$x,
        y = plane_matrix$y,
        z = plane_matrix$z,
        surfacecolor = matrix(rep(1, 400), nrow = 20),
        colorscale = list(c(0, 1), c("pink3", "pink3")),
        showscale = FALSE,
        opacity = 0.3
      ) %>%
      add_surface(
        x = uci_plane_matrix$x,
        y = uci_plane_matrix$y,
        z = uci_plane_matrix$z,
        surfacecolor = matrix(rep(1, 400), nrow = 20),
        colorscale = list(c(0, 1), c("lightgreen", "lightgreen")),
        showscale = FALSE,
        opacity = 0.3
      ) %>%
      add_surface(
        x = lci_plane_matrix$x,
        y = lci_plane_matrix$y,
        z = lci_plane_matrix$z,
        surfacecolor = matrix(rep(1, 400), nrow = 20),
        colorscale = list(c(0, 1), c("lightgreen", "lightgreen")),
        showscale = FALSE,
        opacity = 0.3
      ) %>%
      add_surface(
        x = max_plane_matrix$x,
        y = max_plane_matrix$y,
        z = max_plane_matrix$z,
        surfacecolor = matrix(rep(1, 400), nrow = 20),
        colorscale = list(c(0, 1), c("darkgreen", "darkgreen")),
        showscale = FALSE,
        opacity = 0.3
      ) %>%
      layout(scene = list(
        zaxis = list(title = "log likelihood"),
        yaxis = list(title = "lambda"),
        xaxis = list(title = "meanNb")
      ))
  } else {
    plot_ly(LL_tibble) %>%
      add_trace(
        x = ~ theta,
        y = ~ lambda,
        z = ~ ll,
        type = "scatter3d",
        mode = "lines+markers",
        line = list(width = 5, color = "black"),
        marker = list(size = 4, color = "black")
      ) %>%
      add_surface(
        x = plane_matrix$x,
        y = plane_matrix$y,
        z = plane_matrix$z,
        surfacecolor = matrix(rep(1, 400), nrow = 20),
        colorscale = list(c(0, 1), c("pink3", "pink3")),
        showscale = FALSE,
        opacity = 0.3
      ) %>%
      layout(scene = list(
        zaxis = list(title = "log likelihood"),
        yaxis = list(title = "lambda"),
        xaxis = list(title = "meanNb")
      ))
  }
}


make3dCiPlot(raw, factor = "Overall", confidence = TRUE)
make3dCiPlot(raw, factor = "17/18")
make3dCiPlot(raw, factor = "18/19")
make3dCiPlot(raw, factor = "21/22")
make3dCiPlot(raw, factor = "H1N1")
make3dCiPlot(raw, factor = "H3N2")

list <- list()
for (i in 1:length(factors)) {
  f <- factors[i]
  list[[i]] <- findCI(raw, factor = f)
}

ci <- bind_rows(list)

ci2 <- ci %>%
  dplyr::rename(metric = factor) %>%
  mutate(
    metric = case_when(
      metric == "Adult-to-adult" ~ "Adult / Adult",
      metric == "Adult-to-child" ~ "Adult / Child",
      metric == "Child-to-adult" ~ "Child / Adult",
      metric == "Child-to-child" ~ "Child / Child",
      metric == "Both Female" ~ "Female / Female",
      metric == "Both male" ~ "Male / Male",
      metric == "Female-to-male" ~ "Female / Male",
      metric == "Male-to-female" ~ "Male / Female",
      metric == "Both" ~ "Vaccinated / Vaccinated",
      metric == "Neither" ~ "Unknown or unvaccinated / Unknown or unvaccinated",
      metric == "Donor only" ~ "Vaccinated / Unknown or unvaccinated",
      metric == "Recipient only" ~ "Unknown or unvaccinated / Vaccinated",
      metric == "overall" ~ "Overall",
      .default = metric
    )
  ) %>%
  dplyr::mutate(metric = factor(
    metric,
    levels = c(
      "Overall",
      "17/18",
      "18/19",
      "19/20",
      "21/22",
      "H1N1",
      "H3N2",
      "Adult / Adult",
      "Adult / Child",
      "Child / Adult",
      "Child / Child",
      "Male / Male",
      "Male / Female",
      "Female / Male",
      "Female / Female",
      "Vaccinated / Unknown or unvaccinated",
      "Vaccinated / Vaccinated",
      "Unknown or unvaccinated / Unknown or unvaccinated",
      "Unknown or unvaccinated / Vaccinated"
    )
  ))


levels2 <- c(
  "Overall",
  "17/18",
  "18/19",
  "19/20",
  "21/22",
  "H1N1",
  "H3N2",
  "A/A",
  "A/C",
  "C/A",
  "C/C",
  "M/M",
  "M/F",
  "F/M",
  "F/F",
  "V/U",
  "V/V",
  "U/U",
  "V/U"
)


ci2 %>%
  ggplot() +
  geom_col(aes(y = maxNb, x = metric),
           fill = NA,
           color = "black") +
  geom_point(aes(x = metric, y = lower), shape = 12, size = 4.5) +
  geom_point(aes(x = metric, y = upper), shape = 15, size = 4.5) +
  geom_segment(aes(y = lower, yend = upper, x = metric), lwd = 1) +
  scale_y_continuous(limits = c(0, 2), breaks = c(0, 1, 2)) +
  scale_x_discrete(labels = levels2) +
  theme_minimal(base_size = 18) +
  labs(x = "", y = "Bottleneck size")
