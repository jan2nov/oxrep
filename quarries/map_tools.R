my_fitBounds <- function(map, bbox) {
  fitBounds(map, bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
}

map_point_labeller <-
  function(quarrySite = NA,
           province = NA,
           country = NA) {
    paste0(
      # "<p>", Name, "</p>",
      "<p><b>Quarry Site:</b> ",
      quarrySite,
      "</p>",
      "<p><b>Province:</b> ",
      province,
      "</p>",
      "<p><b>Country:</b> ",
      country,
      "</p>"
    )
  }
