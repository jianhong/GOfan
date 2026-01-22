#' @noRd
#' @importFrom igraph V degree
get_root <- function(g) {
    stopifnot(is(g, "igraph"))
    roots <- V(g)[degree(g, mode = "in") == 0]
    return(roots)
}

is_GO_IDs <- function(GO_IDs) {
    stopifnot(is.character(GO_IDs))
    stopifnot(
        "All GO_IDs must be the format like GO:0099536" =
            all(grepl("^GO:\\d+$", GO_IDs))
    )
}

jaccard <- function(d1, d2) {
    inter <- length(intersect(d1, d2))
    union <- length(unique(c(d1, d2)))
    if (union == 0) {
        return(0)
    }
    return(inter / union)
}

rescale <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    # Input validation
    if (missing(x)) stop("'x' is required")

    if (length(x) == 0) {
        return(numeric(0))
    }

    if (!is.numeric(x)) stop("'x' must be numeric")
    if (!is.numeric(from) || length(from) != 2) {
        stop("'from' must be a numeric vector of length 2")
    }
    if (!is.numeric(to) || length(to) != 2) {
        stop("'to' must be a numeric vector of length 2")
    }

    if (any(is.na(from))) stop("'from' range cannot contain NA")
    if (any(is.na(to))) stop("'to' range cannot contain NA")
    if (any(is.infinite(from))) stop("'from' range cannot contain Inf")
    if (any(is.infinite(to))) stop("'to' range cannot contain Inf")

    # Check for zero-width input range
    if (from[1] == from[2]) {
        warning("Input range has zero width, returning midpoint of output range")
        return(rep(mean(to), length(x)))
    }

    # y = ((x - from[1]) / (from[2] - from[1])) * (to[2] - to[1]) + to[1]
    from_min <- from[1]
    from_max <- from[2]
    to_min <- to[1]
    to_max <- to[2]

    # Normalize to [0, 1]
    normalized <- (x - from_min) / (from_max - from_min)

    # Scale to target range
    mapped <- normalized * (to_max - to_min) + to_min

    return(mapped)
}

#' @importFrom grDevices col2rgb rgb
color2gray <- function(col, rate) {
    # Convert color to RGB (0–1)
    rgb <- grDevices::col2rgb(col) / 255

    # Compute luminance (Illuminant D65)
    lum <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
    lum <- rescale(lum, to = c(rate, 1))

    # Convert back to hex grayscale
    gray_hex <- grDevices::rgb(lum, lum, lum)

    # Return named vector
    names(gray_hex) <- col
    gray_hex
}

parse_ratio <- function(x) {
    stopifnot(grepl("/", x))
    vapply(x, function(.ele) eval(parse(text = .ele)), numeric(1L))
}
