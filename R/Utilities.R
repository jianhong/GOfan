#' @noRd
#' @importFrom igraph V degree
get_root <- function(g) {
    stopifnot(is(g, 'igraph'))
    roots <- V(g)[degree(g, mode = "in") == 0]
    return(roots)
}

is_GO_IDs <- function(GO_IDs){
    stopifnot(is.character(GO_IDs))
    stopifnot(
        "All GO_IDs must be the format like GO:0099536"=
            all(grepl('^GO:\\d+$', GO_IDs)))
}

jaccard <- function(d1, d2) {
    inter <- length(intersect(d1, d2))
    union  <- length(unique(c(d1, d2)))
    if (union == 0) return(0)
    return(inter / union)
}

rescale <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    # linearly map x from [from[1], from[2]] to [to[1], to[2]]
    (x - from[1]) / (from[2] - from[1]) * (to[2] - to[1]) + to[1]
}

#' @importFrom grDevices col2rgb rgb
color2gray <- function(col, rate) {
    # Convert color to RGB (0–1)
    rgb <- grDevices::col2rgb(col) / 255

    # Compute luminance (Illuminant D65)
    lum <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
    lum <- rescale(lum, to=c(rate, 1))

    # Convert back to hex grayscale
    gray_hex <- grDevices::rgb(lum, lum, lum)

    # Return named vector
    names(gray_hex) <- col
    gray_hex
}
