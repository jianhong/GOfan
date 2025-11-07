#' Creates sunburst diagram using ggplot2.
#'
#' Creates sunburst diagram using ggplot2.
#'
#' @importFrom ggplot2 ggplot aes geom_rect theme_minimal theme element_blank geom_text .data xlab ylab coord_radial labs
#' @export
#' @param plotdata A data.frame.
#' @param fontsize Default fontsize.
#' @param rotate90 Rotate the labels 90 degree or not. Default NULL will try to
#' auto rotate the labels according the space.
#' @param maxCharacters Maximal number of characters for labels
#' @param legendTitle The title of the legend.
#' @param start,end Offset of starting or ending point from 12 o'clock in radians.
#' see \link[ggplot2]{coord_radial}.
#' @param clip Should drawing be clipped to the extent of the plot panel?
#' Default "on" means yes.
#' @param expand If TRUE, adds a small expansion factor the the
#'   limits to prevent overlap between data and axes.
#'  If FALSE, the default, limits are taken directly from the scale.
#' @param ... Other parameters (except theta) passed to \link[ggplot2]{coord_radial}.
#' @return A \code{\link[ggplot2]{ggplot}} object
#' @examples
#' plotdata <- data.frame(
#'     id=c('GO:0023052', 'GO:0007267', 'GO:0099536', 'GO:0099537', 'GO:0098916'),
#'     x=0.5,
#'     y=seq.int(5),
#'     xmin=0,
#'     ymin=c(0.5, 1.5, 2.5, 3.5, 4.5),
#'     xmax=1,
#'     ymax=c(1.5, 2.5, 3.5, 4.5, 5.5),
#'     fill=seq(1, 5),
#'     label=c('signaling', 'cell-cell signaling', 'synaptic signaling',
#'         'trans-synaptic signaling', 'anterograde trans-synaptic signaling')
#' )
#' ggSunburst(plotdata, end=pi/2)
#'
ggSunburst <- function(plotdata, fontsize=1, rotate90=NULL,
                       maxCharacters=30,
                       legendTitle='color',
                       start = 0, end = NULL,
                       clip = "off", expand = FALSE, ...){
    # Calculate the angular position in polar coordinates (midpoint angle)
    TWO_PI <- 2*pi
    if(is.null(end)){
        end <- start + TWO_PI
    }
    if(end<start){
        stop('end must greater than start.')
    }
    if(end-start>TWO_PI){
        stop('There are overlaps of the plot. ',
             'Please set the difference of start and end within two PI.')
    }
    ## current, can only handle default expansion(mult=0.05, add=0)
    stopifnot(is.logical(expand))

    compute_angle <- function(p, start=0, end=2*pi, expand=FALSE){
        expansion <- 0.05
        if(expand){
            p <- p * (1-2*expansion) + expansion
        }
        p <- p *(end - start)/TWO_PI + start/TWO_PI
        # p is percentage value [0, 1]
        ifelse(p < 0.5,
               (180 - (p/0.5) * 180) - 90,
               90 - (p-0.5)/0.5 * 180)
    }
    if(maxCharacters<4) {
        stop("maxCharacters should not be smaller than 4")
    }
    ## safe substring
    plotdata$label[nchar(plotdata$label)>maxCharacters] <-
        gsub(paste0('^(.{', maxCharacters - 3, '}.*?)\\s+.*$'), '\\1...',
             plotdata$label[nchar(plotdata$label)>maxCharacters])

    plotdata$polar_angle <- compute_angle(plotdata$x /max(plotdata$xmax),
                                          start=start, end=end,
                                          expand=expand)
    if(!is.null(rotate90)){
        if(length(rotate90)!=nrow(plotdata)){
            if(length(rotate90)==1){
                plotdata$rotate90 <- rotate90
            }else{
                stop('the length of rotate90 is not equal to the number of data')
            }
        }else{
            plotdata$rotate90 <- rotate90
        }
    }else{
        plotdata$rotate90 <- NA
    }
    if(length(plotdata$sub_rect)!=nrow(plotdata)){
        plotdata$sub_rect <- 1
    }

    if(is.numeric(fontsize)){
        fontsize <- fontsize * sqrt((end-start)/TWO_PI)
        g <- ggplot(plotdata, aes(x=.data$x, y=.data$y,
                                  xmin=.data$xmin, ymin=.data$ymin,
                                  xmax=.data$xmax, ymax=.data$ymax,
                                  fill=.data$fill,
                                  label=.data$label,
                                  angle = ifelse(.data$polar_angle > 90 &
                                                     .data$polar_angle < 270,
                                                 .data$polar_angle + 180,
                                                 .data$polar_angle),
                                  rotate90=.data$rotate90,
                                  sub_rect=.data$sub_rect)) +
            geom_sunburst(size=fontsize)
    }else{
        if(is.character(fontsize)){
            if(fontsize %in% colnames(plotdata)){
                plotdata[is.na(plotdata[, fontsize]), fontsize] <-
                    min(plotdata[, fontsize], na.rm = TRUE)
                g <- ggplot(plotdata,
                            aes(x=.data$x, y=.data$y,
                                xmin=.data$xmin, ymin=.data$ymin,
                                xmax=.data$xmax, ymax=.data$ymax,
                                fill=.data$fill,
                                label=.data$label,
                                angle = ifelse(.data$polar_angle > 90 &
                                                   .data$polar_angle < 270,
                                               .data$polar_angle + 180,
                                               .data$polar_angle),
                                rotate90=.data$rotate90,
                                sub_rect=.data$sub_rect)) +
                    geom_sunburst(aes(size=.data[[fontsize]]))
            }else{
                stop('fontsize is not a column in plotdata.')
            }
        }else{
            stop('Can not handel fontsize.')
        }
    }
    g + coord_radial(theta='x', start = start, end=end,
                     clip=clip, expand = expand, ...) +
        xlab('') + ylab('') + labs(fill=legendTitle[1]) +
        theme_minimal()  +
        theme(
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid = element_blank())
}

#' Geom for sunburst
#' @description
#' Mainly copied from geom-rect. The improvement part is how to fit the text
#' into the rectangle.
#' @noRd
#' @importFrom vctrs vec_interleave
#' @importFrom ggplot2 ggproto from_theme aes draw_key_polygon Geom make_constructor fill_alpha layer GeomPolygon gg_par
#' @importFrom scales col_mix alpha
#' @importFrom grid grobName textGrob gList rectGrob
GeomSunburst <- ggproto(
    "GeomSunburst", Geom,
    default_aes = aes(
        label = NA,
        family = from_theme(family),
        size = from_theme(fontsize),
        fontcolour = 'black', # font color
        colour = from_theme(colour %||% NA), # rect border color
        fill = from_theme(fill %||% col_mix(ink, paper, 0.35)), # rect fill color
        linewidth = from_theme(borderwidth),
        linetype = from_theme(bordertype),
        alpha = 1,
        angle = 0, # will calculate automatically unless user defined
        hjust = 0.5,
        vjust = 0.5,
        fontface = 1,
        lineheight = 1.1,
        rotate90 = NA,
        sub_rect = 1
    ),
    required_aes = c("x|width|xmin|xmax", "y|height|ymin|ymax"),
    setup_data = function(self, data, params){
        # handles for text label
        names(data) <- rename_aes_fontcolour(names(data))
        lab <- data$label

        if(all(c('x', 'y', 'xmin', 'ymin', 'xmax', 'ymax') %in% names(data))){
            return(data)
        }
        # Fill in missing aesthetics from parameters
        required <- strsplit(self$required_aes, "|", fixed = TRUE)
        missing  <- setdiff(unlist(required), names(data))
        default <- params[intersect(missing, names(params))]
        data[names(default)] <- default

        if (is.null(data$xmin) || is.null(data$xmax)) {
            x <- resolve_rect(
                data[["xmin"]], data[["xmax"]],
                data[["x"]], data[["width"]],
                fun = snake_class(self), type = "x"
            )
            i <- lengths(x) > 1
            data[c("xmin", "xmax")[i]] <- x[i]
        }
        if (is.null(data$ymin) || is.null(data$ymax)) {
            y <- resolve_rect(
                data[["ymin"]], data[["ymax"]],
                data[["y"]], data[["height"]],
                fun = snake_class(self), type = "y"
            )
            i <- lengths(y) > 1
            data[c("ymin", "ymax")[i]] <- y[i]
        }
        # check sub_rect
        ## it should be a number no greater than 1 and no less than 0
        if(any(data$sub_rect>1 | data$sub_rect<0)){
            data$sub_rect <- rescale(data$sub_rect)
        }
        data$sub_rect[is.na(data$sub_rect)] <- 0
        data
    },
    draw_panel = function(self, data, panel_params, coord,
                          lineend = "butt", linejoin = "mitre",
                          na.rm = FALSE, parse = FALSE,
                          size.unit = "mm",
                          check_overlap = FALSE) {
        data <- fix_linewidth(data, snake_class(self))

        lab <- data$label
        if (parse) {
            lab <- parse_safe(as.character(lab))
        }
        lab <- validate_labels(lab)
        size.unit <- resolve_text_unit(size.unit)
        data <- fix_fontsize(data, lab, size.unit)

        if (!coord$is_linear()) {
            ## polar coord
            aesthetics <- setdiff(
                names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
            )
            index <- rep(seq_len(nrow(data)), each = 4)

            new <- data[index, aesthetics, drop = FALSE]
            new$x <- vctrs::vec_interleave(data$xmin, data$xmax,
                                           data$xmax, data$xmin)
            new$y <- vctrs::vec_interleave(data$ymax, data$ymax,
                                           data$ymin, data$ymin)
            new$group <- index
            new_top_layer <- new
            new$alpha <- new$alpha * 0.5 ## background
            new$fill <- color2gray(new$fill, 0.5) ## make it more bright light

            grob_rect <- GeomPolygon$draw_panel(
                new, panel_params, coord,
                lineend = lineend, linejoin = linejoin
            )
            grob_rect$name <- grobName(grob_rect, 'geom_rect')

            ## second layer of rect
            ## map to the alpha,
            ## full Y is 100%
            fixY <- function(y, prop){
                floor(y) + (y-floor(y))*prop
            }
            new_top_layer$y <- vctrs::vec_interleave(
                fixY(data$ymax, data$sub_rect),
                fixY(data$ymax, data$sub_rect),
                data$ymin, data$ymin)
            new_top_layer$colour <- rep(NA, nrow(new_top_layer))
            grob_rect_top_layer <- GeomPolygon$draw_panel(
                new_top_layer, panel_params, coord,
                lineend = lineend, linejoin = linejoin
            )
            grob_rect_top_layer$name <- grobName(grob_rect, 'geom_top_rect')

            ## adjust for polar
            fontsize_factors <- 2*data$y/(max(data$ymax)-min(data$ymin))
            data <- coord$transform(data, panel_params)

            grob_text <- textGrob(
                data$wrapped_text,
                data$x, data$y, default.units = "native",
                hjust = data$hjust, vjust = data$vjust,
                rot = ifelse(data$rotate90,
                             ifelse(## not validated, need more examples
                                 data$angle %% 360<90,
                                 data$angle-90,data$angle+90),
                             data$angle),
                gp = gg_par(
                    col = alpha(data$fontcolour, data$alpha),
                    fontsize = data$size * fontsize_factors,
                    fontfamily = data$family,
                    fontface = data$fontface,
                    lineheight = data$lineheight
                ),
                check.overlap = check_overlap
            )

            gList(grob_rect, grob_rect_top_layer, grob_text)
        } else {
            ## not polar coord

            data$vjust <- compute_just(data$vjust, data$y, data$x, data$angle)
            data$hjust <- compute_just(data$hjust, data$x, data$y, data$angle)

            grob_rect <- rectGrob(
                data$xmin, data$ymax,
                width = data$xmax - data$xmin,
                height = data$ymax - data$ymin,
                default.units = "native",
                just = c("left", "top"),
                gp = gg_par(
                    col = data$colour,
                    fill = fill_alpha(data$fill, data$alpha),
                    lwd = data$linewidth,
                    lty = data$linetype,
                    linejoin = linejoin,
                    lineend = lineend
                )
            )

            grob_rect$name <- grobName(grob_rect, 'geom_rect')

            data <- coord$transform(data, panel_params)
            grob_text <- textGrob(
                data$wrapped_text,
                data$x, data$y, default.units = "native",
                hjust = data$hjust, vjust = data$vjust,
                rot = ifelse(data$rotate90, 0, 90),
                gp = gg_par(
                    col = alpha(data$fontcolour, data$alpha),
                    fontsize = data$size * size.unit,
                    fontfamily = data$family,
                    fontface = data$fontface,
                    lineheight = data$lineheight
                ),
                check.overlap = check_overlap
            )

            gList(grob_rect, grob_text)
        }
    },

    draw_key = draw_key_polygon,

    rename_size = TRUE)

resolve_rect <- function(min = NULL, max = NULL, center = NULL, length = NULL,
                         fun, type) {
    absent <- c(is.null(min), is.null(max), is.null(center), is.null(length))
    if (sum(absent) > 2) {
        missing <- switch(
            type,
            x = "xmin, xmax, x, width",
            y = "ymin, ymax, y, height"
        )
        stop('geom_sunburst requires two of the following aesthetics: \\',
             missing)
    }

    if (absent[1] && absent[2]) {
        min <- center - 0.5 * length
        max <- center + 0.5 * length
        return(list(min = min, max = max))
    }
    if (absent[1]) {
        if (is.null(center)) {
            min <- max - length
        } else {
            min <- max - 2 * (max - center)
        }
    }
    if (absent[2]) {
        if (is.null(center)) {
            max <- min + length
        } else {
            max <- min + 2 * (center - min)
        }
    }
    list(min = min, max = max)
}

rename_aes_fontcolour <- function(x){
    # Convert alternate names to canonical form
    nN <- 'fontcolour'
    if(c('fontcolor' %in% x)){
        x[x %in% 'fontcolor'] <- nN
        return(x)
    }
    if(c('font_color' %in% x)){
        x[x %in% 'fontcolor'] <- nN
        return(x)
    }
    if(c('font.colour' %in% x)){
        x[x %in% 'fontcolor'] <- nN
        return(x)
    }
    return(x)
}

fix_linewidth <- function (data, name) {
    if (is.null(data$linewidth) && !is.null(data$size)) {
        data$linewidth <- data$size
    }
    data
}

#' @importFrom vctrs obj_is_list
#' @importFrom rlang inject
validate_labels <- function (labels) {
    if (!vctrs::obj_is_list(labels)) {
        return(labels)
    }
    labels[lengths(labels) == 0L] <- ""
    if (any(vapply(labels, is.language, logical(1)))) {
        inject(expression(!!!labels))
    }
    else {
        unlist(labels)
    }
}

#' @importFrom ggplot2 .pt
#' @importFrom rlang arg_match0
resolve_text_unit <- function (unit) {
    unit <- arg_match0(unit, c("mm", "pt", "cm", "in", "pc"))
    switch(unit, mm = .pt, cm = .pt * 10, `in` = 72.27, pc = 12,
           1)
}

just_dir <- function (x, tol = 0.001) {
    out <- rep(2L, length(x))
    out[x < 0.5 - tol] <- 1L
    out[x > 0.5 + tol] <- 3L
    out
}

compute_just <- function (just, a = 0.5, b = a, angle = 0) {
    if (!is.character(just)) {
        return(just)
    }
    if (any(grepl("outward|inward", just))) {
        angle <- angle%%360
        angle <- ifelse(angle > 180, angle - 360, angle)
        angle <- ifelse(angle < -180, angle + 360, angle)
        rotated_forward <- grepl("outward|inward", just) & (angle >
                                                                45 & angle < 135)
        rotated_backwards <- grepl("outward|inward", just) &
            (angle < -45 & angle > -135)
        ab <- ifelse(rotated_forward | rotated_backwards, b,
                     a)
        just_swap <- rotated_backwards | abs(angle) > 135
        inward <- (just == "inward" & !just_swap | just == "outward" &
                       just_swap)
        just[inward] <- c("left", "middle", "right")[just_dir(ab[inward])]
        outward <- (just == "outward" & !just_swap) | (just ==
                                                           "inward" & just_swap)
        just[outward] <- c("right", "middle", "left")[just_dir(ab[outward])]
    }
    unname(c(left = 0, center = 0.5, right = 1, bottom = 0, middle = 0.5,
             top = 1)[just])
}

#' @importFrom graphics strwidth strheight
#' @importFrom grid convertWidth convertHeight unit
# Function to split text into lines and calculate dimensions
get_text_dimensions <- function(text, cex = 1, sep = "\n", lineheight=1.2) {
    lines <- strsplit(text, sep, fixed = TRUE)[[1]]
    widths <- strwidth(lines, units = "inches", cex = cex)
    heights <- strheight(lines, units = "inches", cex = cex)

    list(
        width = max(widths),  # widest line
        height = sum(heights) * lineheight, # total height
        n_lines = length(lines)
    )
}

# Function to try different line break configurations
find_optimal_fontsize <- function(label,
                                  rect_width, rect_height,
                                  lineheight = 1.2,
                                  min_size = 0.1) {
    # Split label into words
    words <- strsplit(label, "\\s+")[[1]]
    n_words <- length(words)

    if (n_words == 0) return(list(text = "", size = min_size))
    if (n_words == 1) {
        # Single word - no wrapping possible
        w <- strwidth(label, units = "inches", cex = 1)
        h <- strheight(label, units = "inches", cex = 1)
        size <- min(rect_width / w, rect_height / h)
        return(list(text = label, size = max(size, min_size)))
    }

    best_size <- min_size
    best_text <- label

    # Try different numbers of lines (1 to n_words)
    for (n_lines in seq.int(min(n_words, 3))) {  # limit to 4 lines max

        # Try to distribute words evenly across lines
        words_per_line <- ceiling(n_words / n_lines)

        # Create line breaks
        lines <- character(0)
        for (i in seq(1, n_words, by = words_per_line)) {
            end_idx <- min(i + words_per_line - 1, n_words)
            lines <- c(lines, paste(words[seq(i, end_idx)], collapse = " "))
        }

        # Create wrapped text
        wrapped_text <- paste(lines, collapse = "\n")

        # Calculate dimensions at size = 1
        dims <- get_text_dimensions(wrapped_text, cex = 1,
                                    lineheight=lineheight)
        # Calculate maximum size that fits
        size_width <- rect_width / dims$width
        size_height <- rect_height / dims$height
        size <- min(size_width, size_height)

        # Keep track of best configuration
        if (size > best_size) {
            best_size <- size
            best_text <- wrapped_text
        }
    }

    # Also try greedy wrapping (fill each line as much as possible)
    current_line <- character(0)
    lines <- character(0)

    for (word in words) {
        test_line <- paste(c(current_line, word), collapse = " ")
        test_width <- strwidth(test_line, units = "inches", cex = 1)

        # Estimate if this would fit (rough check)
        if (length(current_line) > 0 &&
            test_width > rect_width * 1.5) {
            # Start new line
            lines <- c(lines, paste(current_line, collapse = " "))
            current_line <- word
        } else {
            current_line <- c(current_line, word)
        }
    }
    if (length(current_line) > 0) {
        lines <- c(lines, paste(current_line, collapse = " "))
    }

    wrapped_text <- paste(lines, collapse = "\n")
    dims <- get_text_dimensions(wrapped_text, cex = 1, lineheight=lineheight)
    size <- min(rect_width / dims$width,
                rect_height / dims$height)

    if (size > best_size) {
        best_size <- size
        best_text <- wrapped_text
    }

    return(list(text = best_text, size = max(best_size, min_size)))
}

fix_fontsize <- function(data, labels, size.unit,
                         lineheight=1.2,
                         margin_factor = 1, min_size = 0.1){
    # Calculate available space in rectangles (in inches)
    rect_widths <- grid::convertWidth(unit((data$xmax - data$xmin) *
                                               margin_factor/max(data$xmax),
                                           'npc'), unitTo = "inches")
    rect_heights <- grid::convertHeight(unit((data$ymax - data$ymin) *
                                                 margin_factor/max(data$ymax),
                                             'npc'), unitTo = "inches")
    w <- as.numeric(rect_heights) ## switch, because of the coordinates switched
    h <- as.numeric(rect_widths)
    results <- mapply(
        find_optimal_fontsize,
        label = labels,
        rect_width = ifelse(w>h, w, h),
        rect_height = ifelse(w>h, h, w),
        lineheight = data$lineheight,
        min_size = min_size,
        SIMPLIFY = FALSE
    )

    size <- vapply(results, function(x) as.numeric(x$size), numeric(1L))
    data$wrapped_text <- vapply(results, function(x) x$text, character(1L))
    if(length(data$rotate90)!=nrow(data) || any(is.na(data$rotate90))){
        data$rotate90 <- w<h
    }
    if(all(data$size==data$size[1])){
        data$size <- data$size * size  * size.unit
    }
    return(data)
}

#' Sunburst plot
#'
#' create a sunburst geom.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. If specified and
#'   `inherit.aes = TRUE` (the default), it is combined with the default mapping
#'   at the top level of the plot. You must supply `mapping` if there is no plot
#'   mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to [ggplot()].
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    [fortify()] for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame`, and
#'    will be used as the layer data. A `function` can be created
#'    from a `formula` (e.g. `~ head(.x, 10)`).
#' @param stat The statistical transformation to use on the data for this layer.
#'   When using a `geom_*()` function to construct a layer, the `stat`
#'   argument can be used to override the default coupling between geoms and
#'   stats. The `stat` argument accepts the following:
#'   * A `Stat` ggproto subclass, for example `StatCount`.
#'   * A string naming the stat. To give the stat as a string, strip the
#'     function name of the `stat_` prefix. For example, to use `stat_count()`,
#'     give the stat as `"count"`.
#'   * For more information and other ways to specify the stat, see the
#'     [layer stat][layer_stats] documentation.
#' @param position A position adjustment to use on the data for this layer. This
#'   can be used in various ways, including to prevent overplotting and
#'   improving the display. The `position` argument accepts the following:
#'   * The result of calling a position function, such as `position_jitter()`.
#'     This method allows for passing extra arguments to the position.
#'   * A string naming the position adjustment. To give the position as a
#'     string, strip the function name of the `position_` prefix. For example,
#'     to use `position_jitter()`, give the position as `"jitter"`.
#'   * For more information and other ways to specify the position, see the
#'     [layer position][layer_positions] documentation.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display. To include legend keys for all levels, even
#'   when no data exists, use `TRUE`.  If `NA`, all levels are shown in legend,
#'   but unobserved levels are omitted.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. [annotation_borders()].
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to [layer()]'s `params` argument. These
#'   arguments broadly fall into one of 4 categories below. Notably, further
#'   arguments to the `position` argument, or aesthetics that are required
#'   can *not* be passed through `...`. Unknown arguments that are not part
#'   of the 4 categories below are ignored.
#'   * Static aesthetics that are not mapped to a scale, but are at a fixed
#'     value and apply to the layer as a whole. For example, `colour = "red"`
#'     or `linewidth = 3`. The geom's documentation has an **Aesthetics**
#'     section that lists the available options. The 'required' aesthetics
#'     cannot be passed on to the `params`. Please note that while passing
#'     unmapped aesthetics as vectors is technically possible, the order and
#'     required length is not guaranteed to be parallel to the input data.
#'   * When constructing a layer using
#'     a `stat_*()` function, the `...` argument can be used to pass on
#'     parameters to the `geom` part of the layer. An example of this is
#'     `stat_density(geom = "area", outline.type = "both")`. The geom's
#'     documentation lists which parameters it can accept.
#'   * Inversely, when constructing a layer using a
#'     `geom_*()` function, the `...` argument can be used to pass on parameters
#'     to the `stat` part of the layer. An example of this is
#'     `geom_area(stat = "density", adjust = 0.5)`. The stat's documentation
#'     lists which parameters it can accept.
#'   * The `key_glyph` argument of [`layer()`] may also be passed on through
#'     `...`. This can be one of the functions described as
#'     [key glyphs][draw_key], to change the display of the layer in the legend.
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @param parse If `TRUE`, the labels will be parsed into expressions and
#'   displayed as described in `?plotmath`.
#' @param check_overlap If `TRUE`, text that overlaps previous text in the
#'   same layer will not be plotted. `check_overlap` happens at draw time and in
#'   the order of the data. Therefore data should be arranged by the label
#'   column before calling `geom_text()`. Note that this argument is not
#'   supported by `geom_label()`.
#' @param size.unit How the `size` aesthetic is interpreted: as millimetres
#'   (`"mm"`, default), points (`"pt"`), centimetres (`"cm"`), inches (`"in"`),
#'   or picas (`"pc"`).

#' @export
#'
#' @details
#' Please note that the `width` and `height` aesthetics are not true position
#' aesthetics and therefore are not subject to scale transformation. It is
#' only after transformation that these aesthetics are applied.
#'
#' @section Aesthetics:
#' `geom_sunburst` understands the following aesthetics:
#' \itemize{
#'   \item `x` define the locations of x.
#'   \item `y` define the locations of y.
#'   \item `xmin` define the bottom of rectangle
#'   \item `ymin` define the left of rectangle
#'   \item `xmax` define the top of rectangle
#'   \item `ymax` define the right of rectangle
#'   \item `width` define the width of rectangle
#'   \item `height` define the height of rectangle
#'   \item `colour` rectangle border color
#'   \item `fill` rectangle fill color
#'   \item `alpha` rectangle fill alpha and font alpha
#'   \item `linewidth` line width for rectangle
#'   \item `linetype` line type for rectangle
#'   \item `label` label text
#'   \item `angle` label angle
#'   \item `family` label family
#'   \item `size` label default size
#'   \item `fontcolour` font color
#'   \item `fontface` font face
#'   \item `lineheight` font line height
#'   \item `hjust` horizontal just for label
#'   \item `vjust` vertical just for label
#'   \item `rotate90` Rotate the labels 90 degree or not. Default NULL will try
#'   to auto rotate the labels according the space.
#'   \item `sub_rect` A proportional sub-rectangle representing a share of the
#'   whole.
#' }
#'
#' @examples
#' plotdata <- data.frame(
#'     id=c('GO:0023052', 'GO:0007267', 'GO:0099536', 'GO:0099537', 'GO:0098916'),
#'     x=0.5,
#'     y=seq.int(5),
#'     xmin=0,
#'     ymin=c(0.5, 1.5, 2.5, 3.5, 4.5),
#'     xmax=1,
#'     ymax=c(1.5, 2.5, 3.5, 4.5, 5.5),
#'     fill=seq(1, 5),
#'     label=c('signaling', 'cell-cell signaling', 'synaptic signaling',
#'         'trans-synaptic signaling', 'anterograde trans-synaptic signaling')
#' )
#' library(ggplot2)
#' ggplot(plotdata, aes(x=x, y=y, xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
#'       fill=fill, label=label)) + geom_sunburst(size=0.5, angle=-90) +
#'       coord_polar()
geom_sunburst <- make_constructor(GeomSunburst)
