#' These function loads the DDSB Color palettes which includes:
#'  
#'     DDSB_cols(): 
#'        turquoise, orange, yellow, burgundy, blue, grey, green
#'
#'     DDSB_pal():
#'          main  = blue, green, yellow
#'          cool  = blue, green
#'          hot   = yellow, orange, burgundy),
#'          mixed = blue, green, yellow, orange, burgundy),
#'          grey  = light grey, dark grey)
#'          
#'    DDSB_scale_color()
#' 
#'    DDSB_scale_fill() 
#' 
#' #Use https://coolors.co/ to get color tints

DDSB_colors <- c(
  `turquoise`  = "#00B1cf", 
  `orange`     = "#f7941e", 
  `yellow`     = "#ffdd00",
  `burgundy`   = "#971a31", 
  `blue`       = "#0056a2",
  `grey`       = "#636466",
  `green`      = "#2faa44"
)

DDSB_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (DDSB_colors)
  
  DDSB_colors[cols]
}


DDSB_palettes <- list(
  `main`  = DDSB_cols("blue", "green", "yellow"),
  `cool`  = DDSB_cols("blue", "green"),
  `hot`   = DDSB_cols("yellow", "orange", "burgundy"),
  `mixed` = DDSB_cols("blue", "green", "yellow", "orange", "burgundy"),
  `grey`  = DDSB_cols("light grey", "dark grey")
)

#' Return function to interpolate a drsimonj color palette
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
DDSB_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- DDSB_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


#' Color scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
DDSB_scale_color <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- DDSB_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("DDSB_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
DDSB_scale_fill <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- DDSB_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("DDSB_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

