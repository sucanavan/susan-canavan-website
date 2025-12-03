library(tidyverse)
library(httr)
library(jsonlite)
library(igraph)
library(ggraph)
library(scales)

# Economist colors
economist_dark <- "#1A1A1A"
economist_grey <- "#595959"

# Country color palette - grouped by region
# Africa - warm reds/oranges
# Europe - blues, greens, teals
# Americas - purples
# Asia - yellows/golds

country_colors <- c(
  "ZA" = "#E3120B",
  "MA" = "#F97A1F",
  "GB" = "#006BA2",
  "DE" = "#3D89C3",
  "FR" = "#7BBFFC",
  "CZ" = "#3EBCD2",
  "CH" = "#00786B",
  "IE" = "#379A8B",
  "IT" = "#4DAD9E",
  "ES" = "#00588D",
  "PT" = "#5DA4DF",
  "US" = "#9A607F",
  "CN" = "#EBB434"
)

country_names <- c(
  "ZA" = "South Africa",
  "MA" = "Morocco",
  "GB" = "UK",
  "DE" = "Germany",
  "FR" = "France",
  "CZ" = "Czech Republic",
  "CH" = "Switzerland",
  "IE" = "Ireland",
  "IT" = "Italy",
  "ES" = "Spain",
  "PT" = "Portugal",
  "US" = "USA",
  "CN" = "China"
)

# -----------------------------------------------
# 1. Fetch publications from OpenAlex using ORCID
# -----------------------------------------------

orcid <- "0000-0002-7972-7928"
base_url <- "https://api.openalex.org/works"

all_works <- list()
page <- 1
per_page <- 100

repeat {
  message(paste("Fetching page", page))
  
  response <- GET(
    base_url,
    query = list(
      filter = paste0("author.orcid:", orcid),
      per_page = per_page,
      page = page
    ),
    add_headers(`User-Agent` = "mailto:sucanavan@gmail.com")
  )
  
  data <- content(response, as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = FALSE)
  
  if (length(data$results) == 0) break
  
  all_works <- c(all_works, list(data$results))
  
  if (nrow(data$results) < per_page) break
  page <- page + 1
}

works <- bind_rows(all_works)
message(paste("Total works fetched:", nrow(works)))

# -----------------------------------------------
# 2. Extract author names from each publication
# -----------------------------------------------

extract_authors <- function(authorships_df) {
  if (is.null(authorships_df) || nrow(authorships_df) == 0) {
    return(NA_character_)
  }
  
  authors <- authorships_df$author$display_name
  authors <- authors[!is.na(authors)]
  
  if (length(authors) == 0) return(NA_character_)
  
  paste(authors, collapse = ", ")
}

extract_author_countries <- function(authorships_df) {
  if (is.null(authorships_df) || nrow(authorships_df) == 0) {
    return(tibble(author = character(), country = character()))
  }
  
  authors <- authorships_df$author$display_name
  
  countries <- sapply(authorships_df$countries, function(c) {
    if (is.null(c) || length(c) == 0) return(NA_character_)
    return(c[1])
  })
  
  tibble(author = authors, country = countries) %>%
    filter(!is.na(author))
}

# Extract authors for each work, filter out datasets and peer-reviews
pubs <- tibble(
  Title = works$display_name,
  Year = works$publication_year,
  Type = works$type,
  Journal = works$primary_location$source$display_name,
  authorships = works$authorships
) %>%
  filter(!Type %in% c("dataset", "peer-review")) %>%
  rowwise() %>%
  mutate(Authors = extract_authors(authorships)) %>%
  ungroup() %>%
  select(Title, Year, Type, Journal, Authors, authorships) %>%
  filter(!is.na(Authors))

message(paste("Publications with authors:", nrow(pubs)))

# -----------------------------------------------
# 3. Build author-country lookup
# -----------------------------------------------

author_country_list <- pubs %>%
  rowwise() %>%
  mutate(author_countries = list(extract_author_countries(authorships))) %>%
  ungroup() %>%
  select(author_countries) %>%
  unnest(author_countries) %>%
  filter(!is.na(country))

author_country_lookup <- author_country_list %>%
  count(author, country) %>%
  group_by(author) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(author, country)

# -----------------------------------------------
# 4. Create co-author pairs
# -----------------------------------------------

edges <- pubs %>%
  select(-authorships) %>%
  separate_rows(Authors, sep = ",\\s*") %>%
  mutate(Authors = str_trim(Authors)) %>%
  filter(Authors != "", !is.na(Authors)) %>%
  group_by(Title) %>%
  filter(n_distinct(Authors) > 1) %>%
  group_modify(~ {
    auths <- unique(.x$Authors)
    if (length(auths) < 2) return(tibble(from = character(), to = character()))
    pairs <- combn(auths, 2)
    tibble(from = pairs[1, ], to = pairs[2, ])
  }) %>%
  ungroup()

# -----------------------------------------------
# 5. Add weights
# -----------------------------------------------

weighted_edges <- edges %>%
  count(from, to, name = "weight")

# -----------------------------------------------
# 6. Count unique papers per author
# -----------------------------------------------

author_pubs <- pubs %>%
  select(-authorships) %>%
  separate_rows(Authors, sep = ",\\s*") %>%
  mutate(Authors = str_trim(Authors)) %>%
  filter(Authors != "", !is.na(Authors)) %>%
  distinct(Title, Authors) %>%
  count(Authors, name = "n_pubs") %>%
  rename(author = Authors) %>%
  left_join(author_country_lookup, by = "author") %>%
  mutate(
    country = ifelse(is.na(country), "Other", country),
    country_display = ifelse(country %in% names(country_colors), country, "Other")
  )

message(paste("Total unique authors:", nrow(author_pubs)))

# -----------------------------------------------
# 7. Create igraph object
# -----------------------------------------------

g <- graph_from_data_frame(weighted_edges, directed = FALSE, vertices = author_pubs)

V(g)$country <- author_pubs$country_display[match(V(g)$name, author_pubs$author)]


# -----------------------------------------------
# 8. Plot with Economist styling and legend
# -----------------------------------------------

plot_colors <- c(country_colors, "Other" = "#D9D9D9")

legend_countries <- names(country_colors)
legend_labels <- unname(country_names[legend_countries])

# Add text size based on publications
V(g)$text_size <- case_when(
  V(g)$n_pubs >= 15 ~ 22,
  V(g)$n_pubs >= 10 ~ 18,
  V(g)$n_pubs >= 5 ~ 10,
  V(g)$n_pubs >= 3 ~ 6,
  V(g)$n_pubs >= 2 ~ 1,
  TRUE ~ 1
)

# Create layout and compress outliers
set.seed(42)
layout <- create_layout(g, layout = "kk")

# Scale coordinates to compress spread - pull outliers toward center
center_x <- median(layout$x)
center_y <- median(layout$y)

layout$x <- center_x + (layout$x - center_x) * 0.7
layout$y <- center_y + (layout$y - center_y) * 0.7

p <- ggraph(layout) +
  geom_edge_link(aes(width = weight), alpha = 0.1, color = "#B3B3B3", show.legend = TRUE) +
  geom_node_point(aes(size = n_pubs, fill = country), shape = 21, color = "white", stroke = 0.5, show.legend = TRUE) +
  scale_fill_manual(
    name = "Country",
    values = plot_colors,
    breaks = legend_countries,
    labels = legend_labels
  ) +
  geom_node_text(
    aes(label = name, size = text_size),
    repel = TRUE,
    family = "sans",
    color = economist_dark,
    segment.color = "#B3B3B3",
    segment.alpha = 0.2,
    max.overlaps = 50,
    show.legend = FALSE
  ) +
  scale_size_continuous(name = "Publications", range = c(2, 20), guide = "legend") +
  scale_edge_width(name = "Co-authored Papers", range = c(0.3, 5), guide = "legend") +
  labs(title = NULL) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 36, family = "sans", color = economist_dark, face = "bold"),
    legend.text = element_text(size = 32, family = "sans", color = economist_grey),
    legend.key.size = unit(1.5, "cm"),
    legend.margin = margin(t = 30),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  guides(
    fill = guide_legend(nrow = 2, override.aes = list(size = 12)),
    size = guide_legend(nrow = 1),
    edge_width = guide_legend(nrow = 1)
  )
# -----------------------------------------------
# 9. Save the plot
# -----------------------------------------------

ggsave("images/coauthorship_network.png", plot = p, width = 18, height = 14, dpi = 200, bg = "white")

message("Plot saved to images/coauthorship_network.png")

# -----------------------------------------------
# 10. Print country summary
# -----------------------------------------------

message("\nAuthors by country:")
author_pubs %>%
  count(country, sort = TRUE) %>%
  print(n = 30)

