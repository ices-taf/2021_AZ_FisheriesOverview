

library(icesTAF)
library(dplyr)
library(ggplot2)
library(sf)

mkdir("report")

# file name utilitiy
file_name <- function(name, ext = "") {
  name <- gsub(" ", "_", name)
  if (nzchar(ext)) ext <- paste0(".", ext)
  paste0("2020_AZ_FO_", name, ext)
}


ecoregion <-
  st_read(taf.data.path("ICES_ecoregions", "ecoregion.csv"),
    options = "GEOM_POSSIBLE_NAMES=WKT", crs = 4326
  )
ecoregion <- dplyr::select(ecoregion, -WKT)

# read vms fishing effort
effort <-
  st_read(taf.data.path("ICES_vms_effort_map", "vms_effort.csv"),
    options = "GEOM_POSSIBLE_NAMES=wkt", crs = 4326
  )
effort <- dplyr::select(effort, -WKT)

if (FALSE) {
  # read vms swept area ratio
  sar <-
    st_read(taf.data.path("ICES_vms_sar_map", "vms_sar.csv"),
      options = "GEOM_POSSIBLE_NAMES=wkt", crs = 4326
    )
  sar <- dplyr::select(sar, -WKT)
}

# ~~~~~~~~~~~~~~~#
# A. Effort map
# ~~~~~~~~~~~~~~~#

gears <- c("Static", "Midwater", "Otter", "Demersal seine")

effort <-
  effort %>%
  dplyr::filter(fishing_category_FO %in% gears) %>%
  dplyr::mutate(
    fishing_category_FO =
      dplyr::recode(fishing_category_FO,
        Static = "Static gears",
        Midwater = "Pelagic trawls and seines",
        Otter = "Bottom otter trawls",
        `Demersal seine` = "Bottom seines"
      ),
    mw_fishinghours = as.numeric(mw_fishinghours)
  ) %>%
  filter(!is.na(mw_fishinghours))

# write layer
write_layer <- function(dat, fname) {
  write_sf(dat, paste0("report/", fname, ".shp"))
  files <- dir("report", pattern = fname, full = TRUE)
  files <- files[tools::file_ext(files) != "png"]
  zip(paste0("report/", fname, ".zip"), files, extras = "-j")
  file.remove(files)
}
write_layer(effort, file_name("effort_map"))

# save plot
plot_effort_map(effort, ecoregion) +
  ggtitle("Average MW Fishing hours 2015-2018")

ggsave(file_name("effort_map", ext = "png"), path = "report", width = 170, height = 200, units = "mm", dpi = 300)

# ~~~~~~~~~~~~~~~#
# A. Swept area map
# ~~~~~~~~~~~~~~~#

if (FALSE) {
  # write layer
  write_layer(sar, file_name("sar_map"))

  plot_sar_map(sar, ecoregion, what = "surface") +
    ggtitle("Average surface swept area ratio 2015-2018")

  ggsave(file_name("sar_map_surface", "png"), path = "report", width = 170, height = 200, units = "mm", dpi = 300)

  plot_sar_map(sar, ecoregion, what = "subsurface") +
    ggtitle("Average subsurface swept area ratio 2015-2018")

  ggsave(file_name("sar_map_subsurface", "png"), path = "report", width = 170, height = 200, units = "mm", dpi = 300)
}
