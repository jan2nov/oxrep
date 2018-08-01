my_fitBounds <- function(map, bbox) {
  fitBounds(map, bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
}

map_point_labeller <-
  function(site = NA,
           province = NA,
           country = NA) {
    paste0(
      # "<p>", Name, "</p>",
      "<p><b>Site:</b> ",
      site,
      "</p>",
      "<p><b>Province:</b> ",
      province,
      "</p>",
      "<p><b>Country:</b> ",
      country,
      "</p>"
    )
  }
