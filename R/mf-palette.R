mf_colors <- c(
#  "Monkey Madness" = "#62584C",
  "Chocolate Truffle" = "#705645",
  "Tanglewood" = "#C9BAA5",
  "Moth Gray" = "#D1CABA",
  "Bone White" = "#E4D9C5",
 # "Pony Tail" = "#D3BC9C",
  "Covered Wagon" = "#CCA884",
#  "Dusty Train" = "#C9BAA5",
  "Lion's Main" = "#E2B272",
  "Grapevine" = "#5E5640",
  "Mountain Forest" = "#4D663E",
  "Lava Gray" = "#60676F",
  "Blue Lava" = "#334F67",
  "Cherokee Red" = "#814E4A",
  "Red Gumball" = "#AD3A3F"
)

#' mf color palette
#'
#' Creates a palette function that accepts an integer argument (the number of
#' levels in the scale) and returns a vector of corresponding hex values.
#'
#' mf_pal is a utility function for scale_color_
#'
#' @param fill Use the fill palette.
#' @family colour mf
#' @export
#' @example inst/examples/ex-mf_pal.R
mf_pal <- function(reverse = FALSE, ...) {
  pal <- mf_colors
  f <- function(n) {
    if (reverse) pal <- rev(pal)
    colorRampPalette(pal, ...)(n)
  }
  f
}

#' mf color scales
#'
#' Color scales using the colors in the mf palette.
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @inheritParams mf_pal
#' @family colour mf
#' @rdname scale_mf
#' @seealso \code{\link{theme_mf}()} for examples.
#' @export
scale_colour_mf <- function(discrete = TRUE, reverse = FALSE, ...) {
  pal <- mf_pal(reverse = reverse)

  #' Scale color using AgC color palette.
  #' @param palette: main, greens or greys
  #' @param discrete: T or F
  #' @param reverse: reverse the direction of the color scheme

  if (discrete) {
    discrete_scale("colour", "mf", palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
#  discrete_scale("colour", "mf", mf_pal(), ...)
}

#' @rdname scale_mf
#' @export
scale_color_mf <- scale_colour_mf

#' @rdname scale_mf
#' @export
scale_fill_mf <- function(...) {
  discrete_scale("fill", "mf", mf_pal(), ...)
}

#' ggplot color theme based on MF's preferences
#'
#' A theme in the style preferences of \emph{MF}.
#'
#' \code{theme_mf} implements the standard bluish-gray
#' background theme in the print \emph{The Economist} and
#' \href{http://economist.com}{economist.com}.
#'
#' \code{theme_economist_white} implements a variant with a while
#' panel and light gray (or white) background often used by \emph{The Economist}
#' blog \href{http://www.economist.com/blogs/graphicdetail}{Graphic Detail}.
#'
#' Use \code{\link{scale_color_mf}()} with this theme.
#' The x axis should be displayed on the right hand side.
#'
#' \emph{The Economist} uses "ITC Officina Sans" as its font for graphs. If
#' you have access to this font, you can use it with the
#' \pkg{extrafont} package. "Verdana" is a good substitute.
#'
#' @inheritParams ggplot2::theme_mimimal
#' @param horizontal \code{logical} Horizontal axis lines?
#' @param dkpanel \code{logical} Darker background for panel region?
#' @param gray_bg \code{logical} If \code{TRUE}, use gray background, else
#'    use white
#' background.
#'
#' @return An object of class \code{\link[ggplot2]{theme}()}.
#'
#' @export
#' @family themes economist
#'
#' @references
#' \itemize{
#' \item \href{http://economist.com}{The Economist}
#' \item \href{http://spiekermann.com/en/itc-officina-display/}{Spiekerblog, "ITC Officina Display", January 1, 2007.}
#' \item \url{http://www.economist.com/help/about-us}
#' }
#'
#' @example inst/examples/ex-theme_mf.R
theme_mf <- function(base_size = 10, base_family = "sans",
                            horizontal = TRUE, dkpanel = FALSE) {
  bgcolors <- c("#d5e4eb", "#c3d6df", "#ed111a", "#ebebeb", "#c9c9c9")
  names(bgcolors) <- c("blue-gray", "dark blue-gray", "red", "light gray", "dark gray")
  my_colors <- mf_colors
  ## From measurements
  ## Ticks = 1 / 32 in, with margin about 1.5 / 32
  ## Title = 3 / 32 in (6 pt)
  ## Legend Labels = 2.5 / 32 in (5pt)
  ## Axis Labels = 2
  ## Axis Titles and other text ~ 2
  ## Margins: Top / Bottom = 6 / 32, sides = 5 / 32
  ret <-
    theme_foundation(base_size = base_size, base_family = base_family) +
    theme(line = element_line(colour = "black"),
          rect = element_rect(fill = bgcolors["ebg"], colour = NA,
                              linetype = 1),
          text = element_text(colour = "black"),
          ## Axis
          axis.line = element_line(size = rel(0.8)),
          axis.line.y = element_blank(),
          axis.text = element_text(size = rel(1)),
          axis.text.x = element_text(vjust = 0,
                                     margin = margin(t = base_size,
                                                     unit = "pt")),
          axis.text.y = element_text(hjust = 0,
                                     margin = margin(r = base_size,
                                                     unit = "pt")),
          ## I cannot figure out how to get ggplot to do 2 levels of ticks
          ## axis.ticks.margin = unit(3 / 72, "in"),
          axis.ticks = element_line(),
          axis.ticks.y = element_blank(),
          axis.title = element_text(size = rel(1)),
          axis.title.x = element_text(),
          axis.title.y = element_text(angle = 90),
          # axis.ticks.length = unit( -1/32, "in"),
          axis.ticks.length = unit( -base_size * 0.5, "points"),
          legend.background = element_rect(linetype = 0),
          legend.spacing = unit(base_size * 1.5, "points"),
          legend.key = element_rect(linetype = 0),
          legend.key.size = unit(1.2, "lines"),
          legend.key.height = NULL,
          legend.key.width = NULL,
          legend.text = element_text(size = rel(1.25)),
          legend.text.align = NULL,
          legend.title = element_text(size = rel(1),  hjust = 0),
          legend.title.align = NULL,
          legend.position = "top",
          legend.direction = NULL,
          legend.justification = "center",
          ## legend.box = element_rect(fill = palette_mf['bgdk'],
          ## colour=NA, linetype=0),
          ## Economist only uses vertical lines
          panel.background = element_rect(linetype = 0),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = "white", size = rel(1.75)),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(0.25, "lines"),
          strip.background = element_rect(fill = bgcolors["ebg"],
                                          colour = NA, linetype = 0),
          strip.text = element_text(size = rel(1.25)),
          strip.text.x = element_text(),
          strip.text.y = element_text(angle = -90),
          plot.background = element_rect(fill = mf_colors["Cherokee Red"], #bgcolors["blue-gray"]
                                         colour = NA),
          plot.title = element_text(size = rel(1.5),
                                    hjust = 0, face = "bold"),
          plot.margin = unit(c(6, 5, 6, 5) * 2, "points"),
          complete = TRUE)
  if (horizontal) {
    ret <- ret + theme(panel.grid.major.x = element_blank())
  } else {
    ret <- ret + theme(panel.grid.major.y = element_blank())
  }
  if (dkpanel == TRUE) {
    ret <- ret + theme(panel.background =
                         element_rect(fill =
                                        unname(bgcolors["dark blue-gray"])),
                       strip.background =
                         element_rect(fill =
                                        unname(bgcolors["dark blue-gray"])))
  }
  ret
}

#' @rdname theme_mf
#' @export
theme_mf_white <- function(base_size = 11, base_family = "sans",
                                  gray_bg = TRUE, horizontal = TRUE) {
  if (gray_bg) {
    bgcolor <- "#ebebeb"  # light gray
  } else {
    bgcolor <- "white"
  }
  theme_mf(base_family = base_family,
                  base_size = base_size,
                  horizontal = horizontal) +
    theme(rect = element_rect(fill = bgcolor),
          plot.background = element_rect(fill = bgcolor),
          panel.background = element_rect(fill = "white"),
          panel.grid.major =
            element_line(colour = "#c9c9c9"),  # dark gray
          strip.background = element_rect(fill = "white"))
}



check_pal_n <- function(n, max_n) {
  if (n > max_n) {
    warning("This palette can handle a maximum of ", max_n, " values. ",
            "You supplied ", n, ".")
  } else if (n < 0) {
    stop("`n` must be a non-negative integer.")
  }
}
