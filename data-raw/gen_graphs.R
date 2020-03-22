# Can we make pretty graphs in R?


install.packages("ggdag")


dag  <- dagify(rock + scissors)

ggdag(dag) +
    theme_dag_blank()



install.packages(c("tidygraph", "ggraph"))


library(tidyverse)
library(tidygraph)
library(ggraph)

rps <- as_tbl_graph(
    data.frame(
        from = c("rock", "paper", "scissors"),
        to = c("scissors", "rock", "paper" ),
        weight = 1
    )
)

rps_plot <- rps %>%
    ggraph(
        layout = 'auto',
        weights = weight
    ) +
    geom_edge_link(
        arrow = grid::arrow(
            type = "closed",
            length = unit(0.05, "npc")
        ),
        edge_width = 2
    ) +
    geom_node_point(
        size = 30,
        colour = "black"
    ) +
    geom_node_text(
        aes(label = name),
        size = 5,
        colour = "white"
    )

rps_plot



library(network)
library(sna)
library(GGally)


non_transitive_data


rps <- non_transitive_data %>%
    filter(outcome == 1) %>%
    select(player_1, player_2, outcome) %>%
    distinct() %>%
    spread(player_2, outcome,  fill = 0) %>%
    select(-player_1) %>%
    as.matrix()
rownames(rps) <- colnames(rps)


rps <- network(
    rps,
    directed = TRUE
)

ggnet2(
    rps,
    node.size = 30,
    node.color = "black",
    edge.size = 2,
    edge.color = "darkgrey",
    label = TRUE,
    label.color = "white",
    arrow.size = 12,
    arrow.gap = 0.05
)


library(ggnetwork)

rps %>%
    ggnetwork(
        arrow.gap = 0.03
    ) %>%
    ggplot(
        aes(x = x, y = y, xend = xend, yend = yend)

    ) +
    geom_edges(
        color = "grey50",
        size = 2,
        arrow = arrow(length = unit(20, "pt"), type = "closed")
    ) +
    geom_nodes() +
    geom_nodelabel_repel(
        aes(label = vertex.names),
        fontface = "bold",
        size = 10
    ) +
    theme_blank()



