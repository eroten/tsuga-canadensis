library(sf)
library(tigris)
library(dplyr)
library(ggplot2)
library(rinat)

# Spatial setup

# fetch MN counties
mn_counties <- tigris::counties(state = "MN")

# DNR data
# https://www.dnr.state.mn.us/rsg/profile.html?action=elementDetail&selectedElement=PGPIN06010#:~:text=Tsuga%20canadensis%20was%20listed%20as,elevated%20to%20endangered%20in%202013.
dnr_tsuga <- mn_counties %>%
  mutate(present = ifelse(NAME %in% c(
    "St. Louis",
    "Itasca",
    "Carlton",
    "Pine",
    "Kanabec",
    "Mille Lacs",
    "Aitkin"
  ), "Recorded", "Absent") %>%
    factor(levels = c(
      "Recorded",
      "Absent"
    )))



# iNaturalist
# https://www.inaturalist.org/observations?place_id=38&subview=map&taxon_id=48734
inat_tsuga <- rinat::get_inat_obs(
  taxon_id = 48734,
  place_id = 38,
  geo = TRUE
) %>%
  sf::st_as_sf(coords = c("longitude", "latitude")) %>%
  sf::st_set_crs(4326)

research_inat_tsuga <- inat_tsuga %>%
  filter(quality_grade == "research")



ggplot() +
  geom_sf(
    data = dnr_tsuga,
    aes(fill = present)
  ) +
  scale_fill_manual(values = c("#aacc55", "gray95")) +
  geom_sf(
    data = research_inat_tsuga,
    aes(color = "iNaturalist"),
    alpha = 0.7
  ) +
  scale_color_manual(values = c("black")) +
  theme_void() +
  labs(
    fill = "Minnesota DNR",
    color = "",
    title = "Tsuga canadensis observations",
    caption = paste0(
      Sys.Date()
    )
  )

ggsave("fig/map-dnr_inat_obs.png",
       device = "png"
)
