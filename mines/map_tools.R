my_fitBounds <- function(map, bbox) {
  fitBounds(map, bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
}

map_point_labeller <-
  function(site = NA,
           Province = NA,
           country = NA,
           region = NA,
           notBeforeOpeningDate = NA,
           notAfterClosingDate = NA) {
    paste0(
      # "<p>", Name, "</p>",
      "<p><b>Mine site:</b> ",
      site,
      "</p>",
      "<p><b>Province:</b> ",
      Province,
      "</p>",
      "<p><b>Country:</b> ",
      country,
      "</p>",
      "<p><b>Region:</b> ",
      Province,
      "</p>",
      "<p><b>Date Range:</b> ",
      "from ",
      notBeforeOpeningDate,
      " to ",
      notAfterClosingDate,
      "</p>"
    )
  }