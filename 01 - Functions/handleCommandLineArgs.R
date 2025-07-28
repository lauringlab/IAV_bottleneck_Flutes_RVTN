handleCommandLineArgs <- function(filename = filename,
                                  plot_bool = FALSE,
                                  var_thresh = var_thresh,
                                  nb_min = 1,
                                  nb_max = 200,
                                  nb_increment = 1,
                                  conf_level = 0.95) {
    parser <- ArgumentParser()
    
    parser$add_argument("--file",
                        type = "character",
                        default = filename ,
                        help = "file containing variant frequencies")
    parser$add_argument("--plot_bool",
                        type = "logical",
                        default = plot_bool,
                        help = "determines whether pdf plot approx_plot.pdf is produced or not")
    parser$add_argument(
        "--var_calling_threshold",
        type = "double",
        default = var_thresh,
        help = "variant calling threshold"
    )
    parser$add_argument("--Nb_min",
                        type = "integer",
                        default = nb_min,
                        help = "Minimum bottleneck value considered")
    parser$add_argument("--Nb_max",
                        type = "integer",
                        default = nb_max,
                        help = "Maximum bottleneck value considered")
    parser$add_argument(
        "--Nb_increment",
        type = "integer",
        default = nb_increment,
        help = "increment between Nb values considered, i.e., all values considered will be multiples of Nb_increment that fall between Nb_min and Nb_max"
    )
    parser$add_argument(
        "--confidence_level",
        type = "double",
        default = conf_level,
        help = "Confidence level (determines bounds of confidence interval)"
    )
    args <- parser$parse_args()
    
    # return(args)
}