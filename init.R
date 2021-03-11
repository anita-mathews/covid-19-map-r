my_packages = c("leaflet", "rgeos", "dplyr", "viridis",
                "RColorBrewer", "httr", "geojsonio",
                "geojson", "raster", "ggplot2", "ggrepel",
                "scales", "ggeasy")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))