plotBottleneckSizeByMetadataBars <- function(bottleneck_by_metadata, opt = "all") {
  if (opt == "all") {
    levels <- c("17/18", "18/19", "19/20", "21/22",
                "H1N1", "H3N2",
                "Adult-to-adult", "Adult-to-child", "Child-to-adult", "Child-to-child",
                "Both male", "Male-to-female", "Female-to-male", "Both Female",
                "Donor only", "Both", "Neither", "Recipient only")
    
    bottleneck_by_metadata %>%
      ggplot() +
      geom_col(aes(x = x, y = 10 * max_LL, fill = meta_factor),
               col = "black") +
      geom_col(aes(x = x, y = -sample_size),
               size = 2,
               fill = "grey") +
      geom_segment(
        aes(
          x = x,
          xend = x,
          y = 10 * lower_CI,
          yend = 10 * upper_CI
        ),
        lwd = 1,
        col = "grey4"
      ) +
      # geom_point(aes(
      #   x = x,
      #   y = (10 * max_LL) + 5,
      #   shape = as.factor(no_conf)
      # ), size = 4) +
      geom_hline(
        yintercept = 20,
        col = "red",
        lwd = 2
      ) +
      scale_shape_manual(
        values = c(NA, 13),
        labels = c("", "point estimate =\nconfidence interval")
      ) +
      scale_fill_manual(values = c("#691883",
                                   "#ff7e26",
                                   "#b148d2",
                                   "#ffc99d",
                                   "#f3ccff")) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(
        expand = c(0, 0),
        breaks = c(-40, -30, -20, -10, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
        labels = c(40, 30, 20, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        limits = c(-41, 105)
      ) +
      scale_x_continuous(
        breaks = seq(1, 18, 1),
        labels = levels,
        expand = c(0, 0),
        limits = c(0.5, 18.5)
      ) +
      guides(shape = guide_legend(position = "top")) +
      geom_vline(xintercept = c(4.5, 6.5, 10.5, 14.5)) +
      theme_bottleneck() +
      theme(
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        ),
        axis.title = element_blank(),
      )
  } else if (opt == "season") {
    temp <- bottleneck_by_metadata %>%
      filter(meta_factor == "season")
    
    temp %>%
      ggplot() +
      geom_col(aes(x = x, y = 10 * max_LL),
               col = "black",
               fill = "#ff7e26") +
      geom_col(aes(x = x, y = -sample_size),
               size = 2,
               fill = "grey") +
      geom_segment(
        aes(
          x = x,
          xend = x,
          y = 10 * lower_CI,
          yend = 10 * upper_CI
        ),
        lwd = 1,
        col = "grey4"
      ) +
      geom_point(aes(
        x = x,
        y = (10 * max_LL) + 5,
        shape = as.factor(no_conf)
      ), size = 4) +
      scale_shape_manual(
        values = c(NA, 13),
        labels = c("", "point estimate =\nconfidence interval")
      ) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(
        expand = c(0, 0),
        breaks = c(-40, -30, -20, -10, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
        labels = c(40, 30, 20, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        limits = c(-41, 105)
      ) +
      theme_bottleneck() +
      theme(legend.position = "none",
            axis.title = element_blank()) +
      scale_x_continuous(
        breaks = c(1, 2, 3, 4),
        labels = c("17/18", "18/19", "19/20", "21/22")
      ) 
    
  } else if (opt == "subtype") {
    temp <- bottleneck_by_metadata %>%
      filter(meta_factor == "subtype")
    
    temp %>%
      ggplot() +
      geom_col(aes(x = x, y = 10 * max_LL),
               col = "black",
               fill = "#ffc99d") +
      geom_col(aes(x = x, y = -sample_size),
               size = 2,
               fill = "grey") +
      geom_segment(
        aes(
          x = x,
          xend = x,
          y = 10 * lower_CI,
          yend = 10 * upper_CI
        ),
        lwd = 1,
        col = "grey4"
      ) +
      geom_point(aes(
        x = x,
        y = (10 * max_LL) + 5,
        shape = as.factor(no_conf)
      ), size = 4) +
      scale_shape_manual(
        values = c(NA, 13),
        labels = c("", "point estimate =\nconfidence interval")
      ) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(
        expand = c(0, 0),
        breaks = c(-40, -30, -20, -10, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
        labels = c(40, 30, 20, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        limits = c(-41, 105)
      ) +
      theme_bottleneck() +
      theme(legend.position = "none",
            axis.title = element_blank()) +
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
      geom_col(aes(x = x, y = 10 * max_LL),
               col = "black",
               fill = "#691883") +
      geom_col(aes(x = x, y = -sample_size),
               size = 2,
               fill = "grey") +
      geom_segment(
        aes(
          x = x,
          xend = x,
          y = 10 * lower_CI,
          yend = 10 * upper_CI
        ),
        lwd = 1,
        col = "grey4"
      ) +
      geom_point(aes(
        x = x,
        y = (10 * max_LL) + 5,
        shape = as.factor(no_conf)
      ), size = 4) +
      scale_shape_manual(
        values = c(NA, 13),
        labels = c("", "point estimate =\nconfidence interval")
      ) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(
        expand = c(0, 0),
        breaks = c(-40, -30, -20, -10, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
        labels = c(40, 30, 20, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        limits = c(-41, 105)
      ) +
      theme_bottleneck() +
      theme(legend.position = "none",
            axis.title = element_blank()) +
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
      geom_col(aes(x = x, y = 10 * max_LL),
               col = "black",
               fill = "#b148d2") +
      geom_col(aes(x = x, y = -sample_size),
               size = 2,
               fill = "grey") +
      geom_segment(
        aes(
          x = x,
          xend = x,
          y = 10 * lower_CI,
          yend = 10 * upper_CI
        ),
        lwd = 1,
        col = "grey4"
      ) +
      geom_point(aes(
        x = x,
        y = (10 * max_LL) + 5,
        shape = as.factor(no_conf)
      ), size = 4) +
      scale_shape_manual(
        values = c(13, NA),
        labels = c("", "point estimate =\nconfidence interval")
      ) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(
        expand = c(0, 0),
        breaks = c(-40, -30, -20, -10, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
        labels = c(40, 30, 20, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        limits = c(-41, 105)
      ) +
      theme_bottleneck() +
      theme(legend.position = "none",
            axis.title = element_blank()) +
      scale_x_continuous(
        breaks = c(11, 12, 13, 14),
        labels = c(
          "Both Female",
          "Both Male",
          "Female-to-\nmale",
          "Male-to-\nfemale"
        )
      )
  } else if (opt == "vax") {
    temp <- bottleneck_by_metadata %>%
      filter(meta_factor == "vax_cat")
    
    temp %>%
      ggplot() +
      geom_col(aes(x = x, y = 10 * max_LL),
               col = "black",
               fill = "#f3ccff") +
      geom_col(aes(x = x, y = -sample_size),
               size = 2,
               fill = "grey") +
      geom_segment(
        aes(
          x = x,
          xend = x,
          y = 10 * lower_CI,
          yend = 10 * upper_CI
        ),
        lwd = 1,
        col = "grey4"
      ) +
      geom_point(aes(
        x = x,
        y = (10 * max_LL) + 5,
        shape = as.factor(no_conf)
      ), size = 4) +
      scale_shape_manual(
        values = c(NA, 13),
        labels = c("", "point estimate =\nconfidence interval")
      ) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(
        expand = c(0, 0),
        breaks = c(-40, -30, -20, -10, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
        labels = c(40, 30, 20, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        limits = c(-41, 105)
      ) +
      theme_bottleneck() +
      theme(legend.position = "none",
            axis.title = element_blank()) +
      scale_x_continuous(
        breaks = c(15, 16, 17, 18),
        labels = c("Both", "Donor only", "Neither", "Recipient only")
      )
  }
}