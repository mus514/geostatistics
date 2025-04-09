#' ggplot2 theme for habitat
#'
#' Loads a set of themes for ggplot2
#'
#' @name theme_habitat
#'
#' @param base_size base font size, given in pts.
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#'
#' @return Themes for publication
#'
#' @description
#' Set of predefined themes for plotting
#'
#' @details
#' \describe{
#'
#' \item{`theme_habitat()`}{
#' A clean theme without the fuss}
#'
#' }
#'
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Default theme
#' ggplot(iris) +
#'   geom_point(mapping = aes(x = Sepal.Length,
#'                            y = Sepal.Width,
#'                            color = Species))
#' # With theme_habitat presets
#' ggplot(iris) +
#'   geom_point(mapping = aes(x = Sepal.Length,
#'                            y = Sepal.Width,
#'                            color = Species)) +
#'   theme_habitat()
#' }
#' @export

requireNamespace("ggplot2", quietly = TRUE)

theme_habitat <- function(base_size = 12,
                      base_family = "",
                      base_line_size = base_size / 22,
                      base_rect_size = base_size / 22) {
  # Based on theme_classic modifying some parts
  ggplot2::theme_classic(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    ggplot2::theme(
      # Plot text
      plot.title = ggplot2::element_text(size = base_size+2),
      plot.subtitle = ggplot2::element_text(colour = "black"),
      plot.caption = ggplot2::element_text(colour = "black"),
      # Panels
      panel.border     = ggplot2::element_blank(),
      # panel.border     = ggplot2::element_rect(colour = "black", fill = NA, linetype = "dotted"),
      panel.grid.major = ggplot2::element_line(colour = "gray99"),
      panel.grid.minor = ggplot2::element_line(colour = "gray99"),
      panel.background = ggplot2::element_blank(),

      # Strips
      strip.background = ggplot2::element_rect(fill = NA,
                                               color = "black"),
      strip.text = ggplot2::element_text(face = "bold",
                                         margin = ggplot2::margin(t = 4.4,4.4,4.4,4.4,
                                                                  unit = "points")),
      # Axis information
      axis.title = ggplot2::element_text(size = base_size),
      axis.text.x = ggplot2::element_text(size = base_size, colour = "black"),
      axis.text.y = ggplot2::element_text(size = base_size, colour = "black"),
      # show axes
      # axis.line = ggplot2::element_line(linetype = "solid"),
      axis.ticks = ggplot2::element_line(colour = "black"),

      # Legend
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 14),
      legend.key = ggplot2::element_rect(fill = NA),
      legend.background = ggplot2::element_blank(),

      complete = TRUE
    ) # End
}
