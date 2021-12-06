library(tidyverse)
library(osmdata)
library(sf)
library(sp)
library(RColorBrewer)
library(ggsn)

#'*Map Making Extravaganza!*'#

#'*Wiesbaden, Germany*'#

bbx_w <- getbb("Wiesbaden, DE")

min_lon <- 8.172850; max_lon <- 8.363990
min_lat <- 50.008220; max_lat <- 50.107170
# # bbx_w <- rbind(x=c(min_lon,max_lon),y=c(min_lat,max_lat))
# 
# colnames(bbx_w) <- c("min","max")

#'* WATER '*#

waterways_w <- bbx_w %>%
  opq() %>%
  add_osm_feature(
    key = "natural", 
    value = "water"
  ) %>% 
  osmdata_sf() %$% 
  osm_multipolygons %>% 
  select(geometry)

# ggplot(waterways_w) +
#   geom_sf(fill = "blue", color = NA)

#'* THEATER AND KURHAUS '*# /CASINO

theater_w <- bbx_w %>%
  opq() %>%
  add_osm_feature(
    key = "amenity", 
    value = c("theatre")
  ) %>% 
  osmdata_sf() %$% 
  osm_multipolygons %>% 
  select(geometry)

kurhaus_w <- bbx_w %>%
  opq() %>%
  add_osm_feature(
    key = "tourism",
    value = "attraction"
  ) %>%
  osmdata_sf() %$%
  osm_polygons %>%
  filter(name == "Kurhaus Wiesbaden") %>%
  select(geometry)

# ggplot(courthouse_w) +
#   geom_sf(fill = "blue", color = NA)

#'* MAJOR ROADS '*#

highways_w <- bbx_w %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value=c("motorway", "trunk",
                          "primary","secondary", 
                          "tertiary","motorway_link",
                          "trunk_link","primary_link",
                          "secondary_link",
                          "tertiary_link")) %>%
  osmdata_sf()

# ggplot() +
#   geom_sf(data = highways_w$osm_lines,
#           aes(color = highway),
#           size = .4,
#           alpha = .65) +
#   theme_void()

#'* RAILWAYS '*#

railways_w <- bbx_w %>%
  opq() %>%
  add_osm_feature(key = "railway", 
                  value=c("rail", "light_rail",
                          "subway","funicular")) %>%
  osmdata_sf()

# ggplot() +
#   geom_sf(data = railways_w$osm_lines,
#           aes(color = "red"),
#           size = .4,
#           alpha = .65) +
#   theme_void()

#'* MINOR ROADS '*#

streets_w <- bbx_w %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service","unclassified",
                            "pedestrian", "footway",
                            "track","path")) %>%
  osmdata_sf()

# ggplot() +
#   geom_sf(data = streets_w$osm_lines,
#           aes(color=highway),
#           size = .4,
#           alpha = .65)+
#   theme_void()

#'* RUNWAY + TAXIWAYS + APRONS'*#

runways_w <- bbx_w %>%
  opq()%>%
  add_osm_feature(key = "aeroway", 
                  value = c("runway")) %>%
  osmdata_sf()

taxiways_w <- bbx_w %>%
  opq()%>%
  add_osm_feature(key = "aeroway", 
                  value = c("taxiway")) %>%
  osmdata_sf()

aprons_w <- bbx_w %>%
  opq() %>%
  add_osm_feature(
    key = "aeroway", 
    value = c("apron")
  ) %>% 
  osmdata_sf() %$% 
  osm_polygons %>% 
  select(geometry)

# ggplot() +
#   geom_sf(data = runways_w$osm_lines,
#           col = color_roads,
#           size = 6,
#           alpha = .65) +
#   geom_sf(data = taxiways_w$osm_lines,
#           col = color_roads,
#           size = .2,
#           alpha = .65)






col_streets <- "#D4AC0D"
col_highways <- "#B7950B"
col_airport <- "#9A7D0A"
col_railways <- "#9A7D0A"
col_waterways <- "#34495E"
col_attractions <- "#6C3483"
col_background <- "#E5E7E9"

alpha_level <- 1

map_w <- ggplot() +
  geom_sf(data = streets_w$osm_lines,
          col = col_streets,
          size = .4,
          alpha = alpha_level) +
  geom_sf(data = highways_w$osm_lines,
          col = col_highways,
          size = .6,
          alpha = alpha_level)+
  geom_sf(data = runways_w$osm_lines,
          col = col_airport,
          size = 2,
          alpha = alpha_level) +
  geom_sf(data = taxiways_w$osm_lines,
          col = col_airport,
          size = .2,
          alpha = alpha_level) +
  geom_sf(data = aprons_w,
          fill = col_airport, 
          color = NA,
          alpha = alpha_level) +
  geom_sf(data = waterways_w,
          fill = col_waterways, 
          color = NA,
          alpha = alpha_level) +
  geom_sf(data = railways_w$osm_lines,
          color = col_railways,
          size = .2,
          alpha = alpha_level) +
  geom_sf(data = theater_w,
          fill = col_attractions, 
          alpha = alpha_level,
          color = NA) +
  geom_sf(data = kurhaus_w,
          fill = col_attractions, 
          alpha = alpha_level,
          color = NA) +
  coord_sf(xlim = c(min_lon,max_lon),
           ylim = c(min_lat,max_lat),
           expand = FALSE) +
  theme(legend.position = F) + theme_void() +
  theme(panel.background=
          element_rect(fill = col_background))

ggsave(map_w,
       filename = "wiesbaden_map.png",
       scale = 1,
       width = 18,
       height = 18,
       units = "in",
       dpi = 1000)



#'*CLEVELAND*'#


#'*Map Making Extravaganza!*'#

#'*Wiesbaden, Germany*'#

bbx_c <- getbb("Cleveland, OH")

min_lon_c <- -81.756302; max_lon_c <- -81.614056
min_lat_c <- 41.452099; max_lat_C <- 41.540301
# # bbx_c <- rbind(x=c(min_lon,max_lon),y=c(min_lat,max_lat))
# 
# colnames(bbx_c) <- c("min","max")

#'* WATER '*#

waterways_c <- bbx_c %>%
  opq() %>%
  add_osm_feature(
    key = "natural", 
    value = "water"
  ) %>% 
  osmdata_sf() %$% 
  osm_multipolygons %>% 
  select(geometry)

# ggplot(waterways_c) +
#   geom_sf(fill = "blue", color = NA)

#'* THEATER AND KURHAUS '*# /CASINO

# theater_c <- bbx_c %>%
#   opq() %>%
#   add_osm_feature(
#     key = "amenity", 
#     value = c("theatre")
#   ) %>% 
#   osmdata_sf() %$% 
#   osm_multipolygons %>% 
#   select(geometry)

kurhaus_c <- bbx_c %>%
  opq() %>%
  add_osm_feature(
    key = "historic",
    value = "building"
  ) %>%
  osmdata_sf() %$%
  osm_polygons %>%
  filter(name == "Cleveland Arcade") %>%
  select(geometry)

# ggplot(courthouse_c) +
#   geom_sf(fill = "blue", color = NA)

#'* MAJOR ROADS '*#

highways_c <- bbx_c %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value=c("motorway", "trunk",
                          "primary","secondary", 
                          "tertiary","motorway_link",
                          "trunk_link","primary_link",
                          "secondary_link",
                          "tertiary_link")) %>%
  osmdata_sf()

# ggplot() +
#   geom_sf(data = highways_c$osm_lines,
#           aes(color = highway),
#           size = .4,
#           alpha = .65) +
#   theme_void()

#'* RAILWAYS '*#

railways_c <- bbx_c %>%
  opq() %>%
  add_osm_feature(key = "railway", 
                  value=c("rail", "light_rail",
                          "subway","funicular")) %>%
  osmdata_sf()

# ggplot() +
#   geom_sf(data = railways_c$osm_lines,
#           aes(color = "red"),
#           size = .4,
#           alpha = .65) +
#   theme_void()

#'* MINOR ROADS '*#

streets_c <- bbx_c %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service","unclassified",
                            "pedestrian", "footway",
                            "track","path")) %>%
  osmdata_sf()

# ggplot() +
#   geom_sf(data = streets_c$osm_lines,
#           aes(color=highway),
#           size = .4,
#           alpha = .65)+
#   theme_void()

#'* RUNWAY + TAXIWAYS + APRONS'*#

runways_c <- bbx_c %>%
  opq()%>%
  add_osm_feature(key = "aeroway", 
                  value = c("runway")) %>%
  osmdata_sf()

taxiways_c <- bbx_c %>%
  opq()%>%
  add_osm_feature(key = "aeroway", 
                  value = c("taxiway")) %>%
  osmdata_sf()

aprons_c <- bbx_c %>%
  opq() %>%
  add_osm_feature(
    key = "aeroway", 
    value = c("apron")
  ) %>% 
  osmdata_sf() %$% 
  osm_polygons %>% 
  select(geometry)

# ggplot() +
#   geom_sf(data = runways_c$osm_lines,
#           col = color_roads,
#           size = 6,
#           alpha = .65) +
#   geom_sf(data = taxiways_c$osm_lines,
#           col = color_roads,
#           size = .2,
#           alpha = .65)






col_streets <- "#839192"
col_highways <- "#717D7E"
col_airport <- "#717D7E"
col_railways <- "#5F6A6A"
col_waterways <- "#34495E"
col_attractions <- "#6C3483"
col_background <- "#F4D03F"

alpha_level <- 1

map_c <- ggplot() +
  geom_sf(data = streets_c$osm_lines,
          col = col_streets,
          size = .4,
          alpha = alpha_level) +
  geom_sf(data = highways_c$osm_lines,
          col = col_highways,
          size = .6,
          alpha = alpha_level)+
  geom_sf(data = runways_c$osm_lines,
          col = col_airport,
          size = 2,
          alpha = alpha_level) +
  geom_sf(data = taxiways_c$osm_lines,
          col = col_airport,
          size = .2,
          alpha = alpha_level) +
  geom_sf(data = aprons_c,
          fill = col_airport, 
          color = NA,
          alpha = alpha_level) +
  geom_sf(data = waterways_c,
          fill = col_waterways, 
          color = NA,
          alpha = alpha_level) +
  geom_sf(data = railways_c$osm_lines,
          color = col_railways,
          size = .2,
          alpha = alpha_level) +
  geom_sf(data = kurhaus_c,
          fill = col_attractions, 
          alpha = alpha_level,
          color = NA) +
  coord_sf(xlim = c(min_lon_c,max_lon_c),
           ylim = c(min_lat_c,max_lat_C),
           expand = FALSE) +
  theme(legend.position = F) + theme_void() +
  theme(panel.background=
          element_rect(fill = col_background))
map_c

ggsave(map_c,
       filename = "cleveland_map.png",
       scale = 1,
       width = 18,
       height = 18,
       units = "in",
       dpi = 1000)



#'*Wiesbaden, Germany*'#

bbx_w <- getbb("Monterey, CA")

min_lon <- 8.172850; max_lon <- 8.363990
min_lat <- 50.008220; max_lat <- 50.107170
# # bbx_w <- rbind(x=c(min_lon,max_lon),y=c(min_lat,max_lat))
# 
# colnames(bbx_w) <- c("min","max")

#'* WATER '*#

waterways_w <- bbx_w %>%
  opq() %>%
  add_osm_feature(
    key = "natural", 
    value = c("water", "bay", "coastline")
  ) %>% 
  osmdata_sf() %$% 
  osm_multipolygons %>% 
  select(geometry)

# ggplot(waterways_w) +
#   geom_sf(fill = "blue", color = NA)

#'* THEATER AND KURHAUS '*# /CASINO

theater_w <- bbx_w %>%
  opq() %>%
  add_osm_feature(
    key = "amenity", 
    value = c("theatre")
  ) %>% 
  osmdata_sf() %$% 
  osm_multipolygons %>% 
  select(geometry)

kurhaus_w <- bbx_w %>%
  opq() %>%
  add_osm_feature(
    key = "tourism",
    value = "attraction"
  ) %>%
  osmdata_sf() %$%
  osm_polygons %>%
  filter(name == "Kurhaus Wiesbaden") %>%
  select(geometry)

# ggplot(courthouse_w) +
#   geom_sf(fill = "blue", color = NA)

#'* MAJOR ROADS '*#

highways_w <- bbx_w %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value=c("motorway", "trunk",
                          "primary","secondary", 
                          "tertiary","motorway_link",
                          "trunk_link","primary_link",
                          "secondary_link",
                          "tertiary_link")) %>%
  osmdata_sf()

# ggplot() +
#   geom_sf(data = highways_w$osm_lines,
#           aes(color = highway),
#           size = .4,
#           alpha = .65) +
#   theme_void()

#'* RAILWAYS '*#

railways_w <- bbx_w %>%
  opq() %>%
  add_osm_feature(key = "railway", 
                  value=c("rail", "light_rail",
                          "subway","funicular")) %>%
  osmdata_sf()

# ggplot() +
#   geom_sf(data = railways_w$osm_lines,
#           aes(color = "red"),
#           size = .4,
#           alpha = .65) +
#   theme_void()

#'* MINOR ROADS '*#

streets_w <- bbx_w %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service","unclassified",
                            "pedestrian", "footway",
                            "track","path")) %>%
  osmdata_sf()

# ggplot() +
#   geom_sf(data = streets_w$osm_lines,
#           aes(color=highway),
#           size = .4,
#           alpha = .65)+
#   theme_void()

#'* RUNWAY + TAXIWAYS + APRONS'*#

runways_w <- bbx_w %>%
  opq()%>%
  add_osm_feature(key = "aeroway", 
                  value = c("runway")) %>%
  osmdata_sf()

taxiways_w <- bbx_w %>%
  opq()%>%
  add_osm_feature(key = "aeroway", 
                  value = c("taxiway")) %>%
  osmdata_sf()

aprons_w <- bbx_w %>%
  opq() %>%
  add_osm_feature(
    key = "aeroway", 
    value = c("apron")
  ) %>% 
  osmdata_sf() %$% 
  osm_polygons %>% 
  select(geometry)

# ggplot() +
#   geom_sf(data = runways_w$osm_lines,
#           col = color_roads,
#           size = 6,
#           alpha = .65) +
#   geom_sf(data = taxiways_w$osm_lines,
#           col = color_roads,
#           size = .2,
#           alpha = .65)






col_streets <- "#D4AC0D"
col_highways <- "#B7950B"
col_airport <- "#9A7D0A"
col_railways <- "#9A7D0A"
col_waterways <- "#34495E"
col_attractions <- "#6C3483"
col_background <- "#E5E7E9"

alpha_level <- 1

map_w <- ggplot() +
  geom_sf(data = streets_w$osm_lines,
          col = col_streets,
          size = .4,
          alpha = alpha_level) +
  geom_sf(data = highways_w$osm_lines,
          col = col_highways,
          size = .6,
          alpha = alpha_level)+
  geom_sf(data = runways_w$osm_lines,
          col = col_airport,
          size = 2,
          alpha = alpha_level) +
  geom_sf(data = taxiways_w$osm_lines,
          col = col_airport,
          size = .2,
          alpha = alpha_level) +
  geom_sf(data = aprons_w,
          fill = col_airport, 
          color = NA,
          alpha = alpha_level) +
  geom_sf(data = waterways_w,
          fill = col_waterways, 
          color = NA,
          alpha = alpha_level) +
  geom_sf(data = railways_w$osm_lines,
          color = col_railways,
          size = .2,
          alpha = alpha_level) +
  # geom_sf(data = theater_w,
  #         fill = col_attractions, 
  #         alpha = alpha_level,
  #         color = NA) +
  # geom_sf(data = kurhaus_w,
  #         fill = col_attractions, 
  #         alpha = alpha_level,
  #         color = NA) +
  # coord_sf(xlim = c(min_lon,max_lon),
  #          ylim = c(min_lat,max_lat),
  #          expand = FALSE) +
  theme(legend.position = F) + theme_void() +
  theme(panel.background=
          element_rect(fill = col_background))
map_w

ggsave(map_w,
       filename = "wiesbaden_map.png",
       scale = 1,
       width = 18,
       height = 18,
       units = "in",
       dpi = 1000)
