# Plot a map with maptiles to avoid ggmap incompatibility with ggplot2::geom_sf
# https://github.com/dkahle/ggmap/issues/160

bbox <- sf::st_as_sfc("POLYGON((-76.751013 39.405805, -76.446142 39.405805, -76.446142 39.174102, -76.751013 39.174102, -76.751013 39.405805))", crs = 4326)

tmp_map <- maptiles::get_tiles(bbox |>
                                 sf::st_transform(3857), 
                               provider = "CartoDB.Positron",
                               zoom = 12,
                               crop = TRUE)

ctny <- tigris::counties(state = "MD",
                         cb = TRUE) |> 
  sf::st_transform(4326)

library(magrittr)
ctny %<>%
  rmapshaper::ms_lines() |> 
  sf::st_crop(bbox)

ggplot2::ggplot() +
  tidyterra::geom_spatraster_rgb(data = tmp_map,
                                 maxcell = 5e9) + # max n pixels for raster; if raster has more than that tidyterra downsamples it. Try with lower numbers, it gets blurry.
  ggplot2::geom_sf(data = ctny,
                   fill = NA,
                   colour = "black",
                   linewidth = 0.5,
                   linetype = 2) +
  ggplot2::coord_sf(crs = 5070)
