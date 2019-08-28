# utilities to make working with ggplot a little easier

#' My preferred ggplot theme
#'
#' @param title.size font size for title
#' @param axis.title.size font size for axis title
#' @param axis.text.size font size for axis text
#' @param legend.text.size font size for legend
#' @param strip.text.size font size for grid panels
#'
#' @return does not return anything
#' @importFrom ggplot2 theme theme_set theme_bw element_text element_blank
#' @export
set_ggplot <- function(title.size = 16,
                       axis.title.size = 15,
                       axis.text.size = 14,
                       legend.text.size = 14,
                       strip.text.size = 14){
  theme_set(theme_bw() +
                      theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    title = element_text(size = title.size),
                    axis.title = element_text(size = axis.title.size),
                    axis.text = element_text(size = axis.text.size),
                    legend.text = element_text(size = legend.text.size),
                    strip.text = element_text(size = strip.text.size)))}

#' Generate a color scheme similar to ggplot's default, given either the number of categories
#' or the categories themselves
#'
#' @param values separate categories, or
#' @param n the number of categories
#'
#' @return length-n color scheme
#' @export
gg_color_hue <- function(values = NULL, n = NULL) {
  if(!is.null(values)) {
    if(anyDuplicated(values)) stop("Values should be unique if provided!")
    if(!is.character(values)) stop("Values should be of character class!")
    n <- length(values)
    hues <- seq(15, 375, length = n + 1)
    colors <- hcl(h = hues, l = 65, c = 100)[1:n]
    names(colors) <- values
    return(colors)
  }
  hues <- seq(15, 375, length = n + 1)
  colors <- hcl(h = hues, l = 65, c = 100)[1:n]
}


#' Rotate x axis of a ggplot
#'
#' @param angle angle to rotate
#' @param hjust hjust
#' @param vjust vjust
#'
#' @return plot with x axis rotated as specified
#' @export
#' @importFrom ggplot2 theme element_text
rotate_xaxis <- function(angle, hjust = 1, vjust = 1) {
  theme(axis.text.x = element_text(angle = angle, hjust = hjust, vjust = vjust))
}

#' Add title to a cowplot object
#'
#' @param p the cowplot object
#' @param title title
#' @param rel_heights relative heights between the title and the cowplot object
#'
#' @return the plot with title added in
#' @export
cowplot_title <- function(p, title, rel_heights = c(0.1, 1)) {
  title <- cowplot::ggdraw() +
    cowplot::draw_label(title, fontface = "bold")
  cowplot::plot_grid(title, p, ncol =1 , rel_heights = rel_heights)
}
