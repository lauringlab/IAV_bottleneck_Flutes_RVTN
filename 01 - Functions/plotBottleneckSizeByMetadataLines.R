plotBottleneckSizeByMetadataLines <- function(bottleneck_by_metadata, opt = "all") {
  if (opt == "all") {
    
  } else if (opt == "season") {
    temp <- bottleneck_by_metadata %>%
      filter(meta_factor == "season")
    
    temp %>%
      ggplot() +
      # geom_hline(yintercept = 2, col = "red", lty = 1, lwd = 1, alpha = 0.7) +
      geom_point(aes(x = x, y = max_LL), size = 8) +
      geom_segment(aes(
        x = x - 0.25,
        xend = x + 0.25,
        y = lower_CI - 0.01
      ), lwd = 1) +
      geom_segment(aes(
        x = x - 0.25,
        xend = x + 0.25,
        y = upper_CI + 0.01
      ), lwd = 1) +
      geom_segment(aes(x = x, y = lower_CI, yend = upper_CI), lwd = 1) +
      labs(x = "Season", y = "Bottleneck Size") +
      theme_bottleneck() +
      scale_y_continuous(breaks = seq(1, 10, 1)) +
      scale_x_continuous(
        breaks = c(1, 2, 3, 4),
        labels = c("17/18", "18/19", "19/20", "21/22")
      )
    
  } else if (opt == "subtype") {
    temp <- bottleneck_by_metadata %>%
      filter(meta_factor == "subtype")
    
    temp %>%
      ggplot() +
      geom_point(aes(x = x, y = max_LL), size = 8) +
      geom_segment(aes(
        x = x - 0.2,
        xend = x + 0.2,
        y = lower_CI
      ), lwd = 1) +
      geom_segment(aes(
        x = x - 0.2,
        xend = x + 0.2,
        y = upper_CI
      ), lwd = 1) +
      geom_segment(aes(x = x, y = lower_CI, yend = upper_CI), lwd = 1) +
      labs(x = "Subtype", y = "Bottleneck Size") +
      theme_bottleneck() +
      scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
      scale_x_continuous(
        breaks = c(5, 6),
        labels = c("H1N1", "H3N2"),
        limits = c(4.5, 6.5)
      )
  } else if (opt == "age") {
    temp <- bottleneck_by_metadata %>%
      filter(meta_factor == "age_cat")
    
    temp %>%
      ggplot() +
      geom_point(aes(x = x, y = max_LL), size = 8) +
      geom_segment(aes(
        x = x - 0.2,
        xend = x + 0.2,
        y = lower_CI
      ), lwd = 1) +
      geom_segment(aes(
        x = x - 0.2,
        xend = x + 0.2,
        y = upper_CI
      ), lwd = 1) +
      geom_segment(aes(x = x, y = lower_CI, yend = upper_CI), lwd = 1) +
      labs(x = "Age", y = "Bottleneck Size") +
      theme_bottleneck() +
      scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
      scale_x_continuous(
        breaks = c(7, 8, 9, 10),
        labels = c(
          "Adult-to-adult",
          "Adult-to-child",
          "Child-to-adult",
          "Child-to-child"
        )
      )
  } else if (opt == "sex") {
    temp <- bottleneck_by_metadata %>%
      filter(meta_factor == "sex_cat")
    
    temp %>%
      ggplot() +
      geom_point(aes(x = x, y = max_LL), size = 8) +
      geom_segment(aes(
        x = x - 0.2,
        xend = x + 0.2,
        y = lower_CI
      ), lwd = 1) +
      geom_segment(aes(
        x = x - 0.2,
        xend = x + 0.2,
        y = upper_CI
      ), lwd = 1) +
      geom_segment(aes(x = x, y = lower_CI, yend = upper_CI), lwd = 1) +
      labs(x = "Sex-at-birth", y = "Bottleneck Size") +
      theme_bottleneck() +
      scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
      scale_x_continuous(
        breaks = c(11, 12, 13, 14),
        labels = c(
          "Both Female",
          "Both Male",
          "Female-to-male",
          "Male-to-female"
        )
      )
  } else if (opt == "vax") {
    temp <- bottleneck_by_metadata %>%
      filter(meta_factor == "vax_cat")
    
    temp %>%
      ggplot() +
      geom_point(aes(x = x, y = max_LL), size = 8) +
      geom_segment(aes(
        x = x - 0.2,
        xend = x + 0.2,
        y = lower_CI
      ), lwd = 1) +
      geom_segment(aes(
        x = x - 0.2,
        xend = x + 0.2,
        y = upper_CI
      ), lwd = 1) +
      geom_segment(aes(x = x, y = lower_CI, yend = upper_CI), lwd = 1) +
      labs(x = "Vaccination", y = "Bottleneck Size") +
      theme_bottleneck() +
      scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
      scale_x_continuous(
        breaks = c(15, 16, 17, 18),
        labels = c("Both", "Donor only", "Neither", "Recipient only")
      )
  }
}