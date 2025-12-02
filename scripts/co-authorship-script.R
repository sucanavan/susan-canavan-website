library(tidyverse)
library(visNetwork)
library(htmlwidgets)

# Economist colors
economist_red <- "#E3120B"
economist_dark <- "#1A1A1A"
economist_grey <- "#595959"
economist_light <- "#D9D9D9"

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

# Create nodes with Economist styling
nodes <- author_pubs %>%
  mutate(
    label = id,
    value = n_pubs,
    title = paste0("<b style='font-family: Arial; color: #1A1A1A;'>", id, "</b><br>", 
                   "<span style='font-family: Arial; color: #595959;'>", n_pubs, " publications</span>"),
    color.background = case_when(
      id == "S Canavan" ~ economist_red,
      n_pubs >= 5 ~ economist_dark,
      n_pubs >= 3 ~ economist_grey,
      TRUE ~ "#B3B3B3"
    ),
    color.border = case_when(
      id == "S Canavan" ~ "#B30000",
      TRUE ~ economist_dark
    ),
    borderWidth = case_when(
      id == "S Canavan" ~ 3,
      TRUE ~ 1
    ),
    font.size = case_when(
      id == "S Canavan" ~ 22,
      n_pubs >= 5 ~ 16,
      n_pubs >= 3 ~ 14,
      TRUE ~ 12
    ),
    font.color = economist_dark,
    font.face = "bold"
  )

# Create edges
edges_vis <- weighted_edges %>%
  mutate(
    width = weight * 1.5,
    color = economist_light,
    title = paste0("<span style='font-family: Arial;'>", weight, " co-authored papers</span>")
  )

# Create network
network <- visNetwork(nodes, edges_vis, width = "100%", height = "650px") %>%
  visNodes(
    shape = "dot",
    scaling = list(min = 10, max = 40),
    font = list(
      face = "Arial",
      vadjust = -12,
      strokeWidth = 3,
      strokeColor = "white"
    ),
    shadow = list(enabled = TRUE, size = 5, x = 2, y = 2)
  ) %>%
  visEdges(
    smooth = list(enabled = TRUE, type = "continuous"),
    color = list(opacity = 0.4)
  ) %>%
  visPhysics(
    solver = "forceAtlas2Based",
    forceAtlas2Based = list(
      gravitationalConstant = -80,
      centralGravity = 0.015,
      springLength = 120,
      springConstant = 0.05,
      damping = 0.4
    ),
    stabilization = list(iterations = 300)
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