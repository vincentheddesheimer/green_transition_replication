theme_custom <- function(fontsize = 15, facet_alt = F) {
    th <- ggplot2::theme_bw() + ggplot2::theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), text = element_text(size = fontsize)
    )
    if (facet_alt) {
        th <- th + theme(strip.background = element_blank()) +
            theme(strip. = element_text(colour = "black"))
    }
    th
}
