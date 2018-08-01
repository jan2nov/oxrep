gg_timeline_plot <- function(start, end,
                             minyear,
                             maxyear,
                             breaks) {
  timeline_data <- tibble(start = start,
                          end = end,
                          minyear,
                          maxyear)
  
  timeline_data %>%
    ggplot(aes(
      y = 0,
      yend = 0,
      x = start,
      xend = end
    )) + geom_segment(size = 5, linetype = 1) +
    scale_x_continuous(limits = c(minyear,
                                  maxyear),
                       breaks = seq(minyear, maxyear, breaks)) + ylab("") + xlab("") +
    scale_y_continuous(limits = c(0, 0.1)) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major =   element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    theme(aspect.ratio = .01)
}
