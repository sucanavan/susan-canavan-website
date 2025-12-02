library(tidyverse)
library(visNetwork)
library(htmlwidgets)

# Economist colors
economist_red <- "#E3120B"
economist_dark <- "#1A1A1A"
los_angeles_85 <- "#E1DFD0"
los_angeles_90 <- "#EBE9E0"
paris_85 <- "#D0E1E1"

# Load publications
pubs <- read.csv("publications.csv")

# Create co-author pairs, filtering out "..." and empty strings
edges <- pubs %>%
  separate_rows(Authors, sep = ",\\s*") %>%
  mutate(Authors = str_trim(Authors)) %>%
  filter(Authors != "...", Authors != "", !is.na(Authors)) %>%
  group_by(Title) %>%
  filter(n_distinct(Authors) > 1) %>%
  group_modify(~ {
    auths <- unique(.x$Authors)
    if (length(auths) < 2) return(tibble(from = character(), to = character()))
    pairs <- combn(auths, 2)
    tibble(from = pairs[1, ], to = pairs[2, ])
  }) %>%
  ungroup()

weighted_edges <- edges %>%
  count(from, to, name = "weight")

author_pubs <- pubs %>%
  separate_rows(Authors, sep = ",\\s*") %>%
  mutate(Authors = str_trim(Authors)) %>%
  filter(Authors != "...", Authors != "", !is.na(Authors)) %>%
  distinct(Title, Authors) %>%
  count(Authors, name = "n_pubs") %>%
  rename(id = Authors)

# Create nodes - no borders, large text
nodes <- author_pubs %>%
  mutate(
    label = id,
    value = n_pubs,
    title = paste0("<b style='font-family: Arial; color: #1A1A1A;'>", id, "</b><br>", 
                   "<span style='font-family: Arial; color: #595959;'>", n_pubs, " publications</span>"),
    color.background = case_when(
      id == "S Canavan" ~ economist_red,
      n_pubs >= 5 ~ economist_dark,
      n_pubs >= 3 ~ paris_85,
      TRUE ~ los_angeles_85
    ),
    color.border = case_when(
      id == "S Canavan" ~ economist_red,
      n_pubs >= 5 ~ economist_dark,
      n_pubs >= 3 ~ paris_85,
      TRUE ~ los_angeles_85
    ),
    borderWidth = 0,
    font.size = case_when(
      id == "S Canavan" ~ 40,
      n_pubs >= 5 ~ 32,
      n_pubs >= 3 ~ 26,
      TRUE ~ 22
    ),
    font.color = economist_dark
  )

# Create edges
edges_vis <- weighted_edges %>%
  mutate(
    width = weight * 1.5,
    color = los_angeles_90,
    title = paste0("<span style='font-family: Arial;'>", weight, " co-authored papers</span>")
  )

# Create network - spread out layout
network <- visNetwork(nodes, edges_vis, width = "100%", height = "800px") %>%
  visNodes(
    shape = "dot",
    scaling = list(min = 15, max = 50),
    font = list(
      face = "Arial",
      vadjust = -20,
      strokeWidth = 3,
      strokeColor = "white"
    )
  ) %>%
  visEdges(
    smooth = list(enabled = TRUE, type = "continuous"),
    color = list(opacity = 0.4)
  ) %>%
  visPhysics(
    solver = "forceAtlas2Based",
    forceAtlas2Based = list(
      gravitationalConstant = -300,
      centralGravity = 0.003,
      springLength = 300,
      springConstant = 0.01,
      damping = 0.4
    ),
    stabilization = list(iterations = 500)
  ) %>%
  visInteraction(
    hover = TRUE,
    tooltipDelay = 100,
    hideEdgesOnDrag = TRUE,
    navigationButtons = FALSE,
    zoomView = TRUE
  ) %>%
  visOptions(
    highlightNearest = list(
      enabled = TRUE,
      degree = 1,
      hover = TRUE,
      labelOnly = FALSE
    )
  ) %>%
  visLayout(randomSeed = 42)

# Save the widget
saveWidget(network, "images/coauthorship_network.html", selfcontained = TRUE)