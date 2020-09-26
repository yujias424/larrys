#‘ Merge two graphs together with sharing legend
#' 
#' This function is used to merge multiple ggplot2 object together and share same legend.
#' 
#' @... ggplot2 objects
#' @ncol number of columns
#' @nrow number of rows
#' @position "bottom" or "right", location to place the legend
#' @title title of the graph
#' 
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right"), title = NULL) {
  
    plots <- list(...)
    position <- match.arg(position)
    g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x) x + theme(legend.position="none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)

    # Combine ggplot objects together, add shared legend and title.
    combined <- switch(position,
                        "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight),
                                            top=textGrob(title, vjust = 1.5)),
                        "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 2,
                                            widths = unit.c(unit(1, "npc") - lwidth, lwidth),
                                            top=title))

    grid.newpage()
    grid.draw(combined)

    # return gtable invisibly
    invisible(combined)

}