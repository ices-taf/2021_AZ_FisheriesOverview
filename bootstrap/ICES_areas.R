library(icesTAF)
taf.library(icesFO)

areas <- icesFO::load_areas("Azores")

sf::st_write(areas, "areas.csv", layer_options = "GEOMETRY=AS_WKT")
