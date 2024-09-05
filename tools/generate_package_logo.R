library(mapSpain)
library(hexSticker)
library(spanishoddata)
library(flowmapper)
library(tidyverse)
library(sf)

od <- spod_get("od", zones = "distr", dates = "2022-04-06")
districts <- spod_get_zones("distr", ver = 2)

spain_for_vis <- esp_get_ccaa()
spain_for_join <- esp_get_ccaa(moveCAN = FALSE)

flows_by_district <- od |>
  group_by(id_origin, id_destination) |> 
  summarise(n_trips = sum(n_trips, na.rm = TRUE), .groups = "drop") |> 
  collect() |> 
  arrange(desc(id_origin), id_destination, n_trips)


district_centroids <- districts |>
  st_centroid() |> 
  st_transform(crs = st_crs(spain_for_join))

ca_distr <- district_centroids |>
  st_join(spain_for_join) |> 
  st_drop_geometry() |>
  filter(!is.na(ccaa.shortname.en)) |> 
  select(id, ca_name = ccaa.shortname.en)

flows_by_ca <- flows_by_district |>
  left_join(ca_distr |>
    rename(id_orig = ca_name),
      by = c("id_origin" = "id")
    ) |> 
  left_join(ca_distr |>
    rename(id_dest = ca_name),
      by = c("id_destination" = "id")
    ) |> 
  group_by(id_orig, id_dest) |>
  summarise(n_trips = sum(n_trips, na.rm = TRUE),
    .groups = "drop") |> 
  rename(o = id_orig, d = id_dest, value = n_trips)

spain_for_vis_coords <- spain_for_vis |>
  st_centroid() |>
  st_coordinates() |>
  as.data.frame() |>
  mutate(name = spain_for_vis$ccaa.shortname.en) |>
  rename(x = X, y = Y)

# create base ggplot with boundaries removing any extra elements
base_plot <- ggplot() +
  geom_sf(data = spain_for_vis, fill=NA, col = "grey30", linewidth = 0.05)+
  theme_classic(base_size = 20) +
  labs(title = "",
    subtitle = "", fill = "", caption = "") +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

# flows_by_ca_twoway_coords |> arrange(desc(flow_ab))
# add the flows
flows_plot <- base_plot|>
  add_flowmap(
    od = flows_by_ca,
    nodes = spain_for_vis_coords,
    node_radius_factor = 1,
    edge_width_factor = 1,
    arrow_point_angle = 35,
    node_buffer_factor = 1.5,
    outline_col = "grey80",
    k_node = 10 # play around with this parameter to aggregate nodes and flows
  )

# customise colours and remove legend, as we need a clean image for the logo
flows_plot <- flows_plot +
  guides(fill="none") +
  scale_fill_gradient(low="#FABB29", high = "#AB061F")


# flows_plot

sticker(flows_plot,

  # package name
  package= "spanishoddata", 
  p_size=4, p_y = 1.6,
  p_color = "gray25", p_family="Roboto",

  # ggplot image size and position
  s_x=1.02, s_y=1.19, s_width=2.6, s_height=2.72,

  # white hex
  h_fill="#ffffff", h_color="grey", h_size=1.3,

  # url
  url = "github.com/rOpenSpain/spanishoddata",
  u_color= "gray25",
  u_family = "Roboto",
  u_size = 1.2,

  # save output name and resolution
  filename="./man/figures/logo.png", dpi=300 #
)
