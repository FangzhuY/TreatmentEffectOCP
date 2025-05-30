rm(list=ls())

library(ggplot2)
library(sf)
library(viridis)
library(dplyr)

# Construct a data frame with province and estimated treatment effect
province_effects <- data.frame(
  province = c("Beijing","Tianjin","Hebei","Shanxi","Liaoning","Jilin","Heilongjiang",
               "Shanghai","Jiangsu","Zhejiang","Anhui","Fujian","Jiangxi",
               "Shandong","Henan","Hubei","Hunan","Guangdong","Guangxi","Chongqing",
               "Sichuan","Yunnan","Shaanxi","Gansu"),
  effect =   c(-0.18,-0.17,-0.20,-0.07,-0.09,-0.26,-0.26,-0.35,-0.43,-0.14,-0.12,
               -0.18,0.02,-0.27,-0.15,-0.53,-0.42,-0.07, 0.02, -0.43, -0.10,
               -0.02, -0.07, -0.06)
)

# Load China provinces shape file
china_map <- st_read("Raw_data/gadm41_CHN_1.shp")

# Join the treatment effect data to map data
china_map <- china_map %>%
  left_join(province_effects, by = c("NAME_1" = "province"))

# --- Plotting ---

map_plot <- ggplot(china_map) +
  geom_sf(aes(fill = effect), color = "white") +
  scale_fill_gradientn(
    name = "Treatment Effect",
    colours = rev(c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15", "#f0f0f0")),  # reversed
    values = scales::rescale(c(-0.6, -0.4, -0.25, -0.1, -0.01, 0)),  # nonlinear emphasis
    limits = c(-0.6, 0),
    na.value = "grey90"
  ) +
  theme_minimal() +
  # labs(title = "Provincial Variations in Treatment Effect") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

ggsave("Output/treatment_map.pdf", plot = map_plot, width = 10, height = 6)


# EOF #