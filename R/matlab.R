
#' @export
theme_matlab <- function(base_size = 12, base_family = '', aspect.ratio = 1){

  out <- theme_base(base_size, base_family) +
    theme(
      axis.ticks.length = unit( -base_size * 0.25, "points"),
      axis.text.x = element_text(margin = unit(rep(0.025, 4) * base_size, "cm")),
      axis.text.y = element_text(margin = unit(rep(0.025, 4) * base_size, "cm")),
      panel.border = element_rect(fill=NA, colour = "black", size= 0.01 * base_size),
      plot.margin = unit(rep(1/16, 4) * base_size, "cm")
    )

  if(!is.na(aspect.ratio)){
    out + theme(aspect.ratio = 1)
  }
}

#' Make plot look like a Matlab plot
#' @param obj ggplot object
#' @param xlim NULL or numeric vector of length 2 provides c(from, to) for x-axis
#' @param ylim NULL or numeric vector of length 2 provides c(from, to) for y-axis
#' @param base_size base font size
#' @param base_family base font family
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100)
#'
#' obj <- data.frame(x = x, y = y)
#'
#' post_matlab(
#'   ggplot(obj) +
#'     aes(x = x, y = y) +
#'     geom_point(),
#'   c(-3, 3), c(-3, 3), 12
#' ) + stat_smooth(se = FALSE)
#'
#' @export
post_matlab <- function(obj, xlim = c(NA, NA), ylim = c(NA, NA), base_size = 12, base_family = '', aspect.ratio = 1){

  if(!is.na(ylim[2])){
    y_mult_max = 0
  } else {
    y_mult_max = 0.05
  }

  output <- obj +
    scale_x_continuous(
      expand = expand_scale(add = c(0, 0), mult = c(0, 0)),
      limits = xlim
    ) +
    scale_y_continuous(
      expand = expand_scale(add = c(0, 0), mult = c(0, y_mult_max)),
      limits = ylim
    ) +
    theme_matlab(base_size, base_family, aspect.ratio = aspect.ratio)

  return(output)

}
