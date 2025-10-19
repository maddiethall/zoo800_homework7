##############################
## HOMEWORK 7: Maddie Thall ##
##############################
## I did this individually because I was out of town
## I do not have my spatial data, so I pulled from an open dataset
## citation: Agger C, Griffin O, Nuttall M, O'Kelly H (2022). 
## Long-term monitoring of primate, bird, and ungulate populations 
## 2010-2022 for protected area management, Keo Seima Wildlife Sanctuary, Cambodia. 
## Version 1.8. WCS Cambodia. Sampling event dataset https://doi.org/10.15468/37thhj 

library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

#file is tab-separated, not comma-separated, needed to use read_tsv
occurance_data = read_tsv("occurance_data.csv")

#filtering relevant columns, primates only
primate_data <- occurance_data %>%
  filter(order == "Primates") %>%     
  select(
    species,                           
    genus,                             
    family,                            
    decimalLatitude,                   
    decimalLongitude                 
  ) %>%
  filter(!is.na(decimalLatitude), !is.na(decimalLongitude))

#boundaries for the sanvtuary and cambodia
keo_seima = st_read("keo_seima_boundary.shp")
cambodia = ne_countries(scale = "medium", country = "Cambodia", returnclass = "sf")

#making everything in the same coordinate system
primate_sf = st_as_sf(
  primate_data,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326)
st_crs(keo_seima) = 32648
keo_seima = st_transform(keo_seima, 4326)

#mapping
ggplot() +
  geom_sf(data = cambodia, fill = "darkseagreen2", color = "gray60", linewidth = 0.4) +
  geom_sf(data = keo_seima, fill = "honeydew2", color = "darkgreen", linewidth = 0.6) +
  geom_point(
    data = primate_data,
    aes(color = species, x = decimalLongitude, y = decimalLatitude),
    size = 2, alpha = 0.4,
    position = position_jitter(width = 0.022, height = 0.022)
  ) +
  annotate(
    "text",
    x = 107.17, y = 12.44,               
    label = "Keo Seima\nWildlife Sanctuary",
    size = 3, fontface = "italic",
    color = "darkgreen", hjust = 0
  ) +
  coord_sf(
    xlim = c(106.4, 107.4),
    ylim = c(11.9, 12.8),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_blank()
  )
