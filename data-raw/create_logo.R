library(tidyverse)
library(data.table)
library(hexSticker)
library(mELO)


rm(list=ls())
gc()

# Fit a model
rpsfw_ELO <- ELO(
    rpsfw_df,
    K_factor = 16,
    p1_advantage = 0,
    save_history = TRUE
)

# Generate some data to plot
rating_plot_df <- rpsfw_ELO$history %>%
    as.data.table() %>%
    mutate(time = as.numeric(V2)) %>%
    filter(V3 == "rating") %>%
    arrange(V1, V3, time)

rating_plot <- rating_plot_df
    ggplot(aes(x=time,y=value,colour=V1)) +
    geom_line(size = 0.5) +
    scale_colour_viridis_d(
        option="D",
        guide = "none",
        begin = 0, end = 0.75
    ) +
    #scale_colour_brewer(palette = "Spectral", guide = "none") +
    #scale_colour_brewer(palette = "Set1", guide = "none") +
    theme_void() +
    theme_transparent()


sticker(
    rating_plot,
    package = "mELO",
    p_size = 20,
    p_y = 1.5,
    p_color = "#422574",
    h_fill = "#F2EDFA",
    h_color = "#422574",
    s_x = 0.95,
    s_y = 0.8,
    s_width = 1.3,
    s_height = 1,
    #spotlight = TRUE,
    #p_family = "Century Gothic",
    filename = "data_raw/mELO_hex.png"
)


usethis::use_logo(
    "data_raw/mELO_hex.png",
    geometry = "240x278",
    retina = TRUE
)

