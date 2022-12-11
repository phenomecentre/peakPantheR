#' @title Plot variable histogram and density
#'
#' @description Plot the histogram and density of the variable
#'
#' @param var (float) vector of values to plot
#' @param varName (str) Name of the variable to plot
#' @param density (bool) If TRUE plot overlay the density on the variable
#' @param ... Passes arguments to ggplot2::geom_histogram, e.g. \code{bins=20},
#' \code{binwidth=1}
#' 
#' @return Grob (ggplot object)
plotHistogram <- function(var, varName = "Variable", density = TRUE, ...) {
    
    input_var <- var[!is.na(var)]
    
    ## Plot histogram
    p_hist <- ggplot2::ggplot(data.frame(x = input_var), ggplot2::aes(x = x)) +
                ggplot2::xlab(varName) + ggplot2::theme_bw()
    # with density
    if (density) {
        p_hist <- p_hist +
                    ggplot2::geom_histogram(ggplot2::aes(y=ggplot2::after_stat(density)),
                        colour = "black", fill = "white", ...) +
                    ggplot2::geom_density(alpha = 0.1, fill = "blue")
        # without density
    } else {
        p_hist <- p_hist + ggplot2::geom_histogram(colour = "black",
                                                    fill = "white", ...)
    }
    
    return(p_hist)
}
