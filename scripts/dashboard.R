library(tidyverse)
library(httr)
library(jsonlite)
library(plotly)
library(htmlwidgets)

# Economist colors
economist_red <- "#E3120B"
economist_dark <- "#1A1A1A"
economist_grey <- "#595959"

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
# 2. Extract data
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

# Build publications dataframe
pubs <- tibble(
  Title = works$display_name,
  Year = works$publication_year,
  Type = works$type,
  Journal = works$primary_location$source$display_name,
  OpenAccess = works$open_access$is_oa,
  authorships = works$authorships
) %>%
  filter(!Type %in% c("dataset", "peer-review")) %>%
  rowwise() %>%
  mutate(Authors = extract_authors(authorships)) %>%
  ungroup() %>%
  filter(!is.na(Authors))

message(paste("Publications:", nrow(pubs)))

# Get author countries
author_country_list <- pubs %>%
  rowwise() %>%
  mutate(author_countries = list(extract_author_countries(authorships))) %>%
  ungroup() %>%
  select(author_countries) %>%
  unnest(author_countries) %>%
  filter(!is.na(country))

# -----------------------------------------------
# 3. Calculate summary statistics
# -----------------------------------------------

# Total publications
total_pubs <- nrow(pubs)

# Publications by type
pubs_by_type <- pubs %>%
  count(Type, sort = TRUE)

# Count articles
n_articles <- pubs_by_type %>% filter(Type == "article") %>% pull(n)
if(length(n_articles) == 0) n_articles <- 0

# Count book chapters
n_chapters <- pubs_by_type %>% filter(Type == "book-chapter") %>% pull(n)
if(length(n_chapters) == 0) n_chapters <- 0

# Total unique collaborators (excluding self)
total_collaborators <- author_country_list %>%
  filter(author != "Susan Canavan") %>%
  distinct(author) %>%
  nrow()

# Number of countries
total_countries <- author_country_list %>%
  filter(!is.na(country)) %>%
  distinct(country) %>%
  nrow()

# Years active
first_year <- min(pubs$Year, na.rm = TRUE)
last_year <- max(pubs$Year, na.rm = TRUE)
years_active <- paste0(first_year, "–", last_year)

# Open access percentage
oa_pct <- round(sum(pubs$OpenAccess == TRUE, na.rm = TRUE) / nrow(pubs) * 100)

# Create data directory if needed
if (!dir.exists("data")) dir.create("data")

# Save stats to a file for the dashboard
stats <- list(
  total_pubs = total_pubs,
  n_articles = n_articles,
  n_chapters = n_chapters,
  total_collaborators = total_collaborators,
  total_countries = total_countries,
  years_active = years_active,
  oa_pct = oa_pct,
  first_year = first_year,
  last_year = last_year
)

saveRDS(stats, "data/dashboard_stats.rds")
message("Saved: dashboard_stats.rds")

# -----------------------------------------------
# PLOT 1: Publications over time
# -----------------------------------------------

pubs_by_year <- pubs %>%
  count(Year) %>%
  filter(!is.na(Year), Year >= 2010)

p1 <- plot_ly(pubs_by_year, x = ~Year, y = ~n, type = "bar",
              marker = list(color = economist_red),
              text = ~n, textposition = "outside", textfont = list(size = 10),
              hovertemplate = "<b>%{x}</b><br>%{y} publications<extra></extra>") %>%
  layout(
    xaxis = list(title = "", tickmode = "linear", dtick = 2, tickfont = list(size = 10)),
    yaxis = list(title = "", tickfont = list(size = 10)),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(l = 30, r = 10, t = 10, b = 30)
  )

saveWidget(p1, "images/plot_publications_year.html", selfcontained = TRUE)
message("Saved: plot_publications_year.html")

# -----------------------------------------------
# PLOT 2: Top co-authors
# -----------------------------------------------

top_coauthors <- author_country_list %>%
  filter(author != "Susan Canavan") %>%
  count(author, name = "papers") %>%
  arrange(desc(papers)) %>%
  head(12) %>%
  arrange(papers)

p2 <- plot_ly(top_coauthors, x = ~papers, y = ~reorder(author, papers), type = "bar",
              orientation = "h",
              marker = list(color = "#006BA2"),
              text = ~papers, textposition = "outside", textfont = list(size = 10),
              hovertemplate = "<b>%{y}</b><br>%{x} papers<extra></extra>") %>%
  layout(
    xaxis = list(title = "", tickfont = list(size = 10)),
    yaxis = list(title = "", tickfont = list(size = 10)),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(l = 120, r = 30, t = 10, b = 30)
  )

saveWidget(p2, "images/plot_coauthors.html", selfcontained = TRUE)
message("Saved: plot_coauthors.html")

# -----------------------------------------------
# PLOT 3: Top journals
# -----------------------------------------------

top_journals <- pubs %>%
  filter(!is.na(Journal)) %>%
  count(Journal, name = "papers") %>%
  arrange(desc(papers)) %>%
  head(8) %>%
  arrange(papers) %>%
  mutate(Journal = str_trunc(Journal, 30))

p3 <- plot_ly(top_journals, x = ~papers, y = ~reorder(Journal, papers), type = "bar",
              orientation = "h",
              marker = list(color = "#3EBCD2"),
              text = ~papers, textposition = "outside", textfont = list(size = 10),
              hovertemplate = "<b>%{y}</b><br>%{x} papers<extra></extra>") %>%
  layout(
    xaxis = list(title = "", tickfont = list(size = 10)),
    yaxis = list(title = "", tickfont = list(size = 9)),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(l = 150, r = 30, t = 10, b = 30)
  )

saveWidget(p3, "images/plot_journals.html", selfcontained = TRUE)
message("Saved: plot_journals.html")

# -----------------------------------------------
# PLOT 4: Open Access status
# -----------------------------------------------

oa_status <- pubs %>%
  mutate(OA = ifelse(OpenAccess == TRUE, "Open Access", "Closed")) %>%
  count(OA) %>%
  mutate(pct = round(n / sum(n) * 100))

p4 <- plot_ly(oa_status, labels = ~OA, values = ~n, type = "pie",
              marker = list(colors = c("#B3B3B3", "#379A8B")),
              textinfo = "label+percent",
              textfont = list(size = 11, color = "white"),
              hovertemplate = "<b>%{label}</b><br>%{value} papers (%{percent})<extra></extra>") %>%
  layout(
    showlegend = FALSE,
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(l = 10, r = 10, t = 10, b = 10)
  )

saveWidget(p4, "images/plot_open_access.html", selfcontained = TRUE)
message("Saved: plot_open_access.html")

# -----------------------------------------------
# PLOT 5: World map
# -----------------------------------------------

country_counts <- author_country_list %>%
  distinct(author, country) %>%
  count(country, name = "collaborators")

country_info <- tibble(
  country = c("ZA", "US", "GB", "DE", "CZ", "IT", "CH", "FR", "ES", "PT", "CN", "IE", "MA",
              "AR", "CA", "CR", "DK", "AU", "FI", "AT", "BE", "BR", "CL", "IL", "NZ",
              "EC", "HU", "RO", "SI", "PL", "SE", "NO", "MX", "IN", "JP", "KR", "SG"),
  iso3 = c("ZAF", "USA", "GBR", "DEU", "CZE", "ITA", "CHE", "FRA", "ESP", "PRT", "CHN", "IRL", "MAR",
           "ARG", "CAN", "CRI", "DNK", "AUS", "FIN", "AUT", "BEL", "BRA", "CHL", "ISR", "NZL",
           "ECU", "HUN", "ROU", "SVN", "POL", "SWE", "NOR", "MEX", "IND", "JPN", "KOR", "SGP"),
  country_name = c("South Africa", "United States", "United Kingdom", "Germany", "Czech Republic",
                   "Italy", "Switzerland", "France", "Spain", "Portugal", "China", "Ireland", "Morocco",
                   "Argentina", "Canada", "Costa Rica", "Denmark", "Australia", "Finland", "Austria",
                   "Belgium", "Brazil", "Chile", "Israel", "New Zealand", "Ecuador", "Hungary", 
                   "Romania", "Slovenia", "Poland", "Sweden", "Norway", "Mexico", "India", "Japan", 
                   "South Korea", "Singapore")
)

country_counts <- country_counts %>%
  left_join(country_info, by = "country") %>%
  filter(!is.na(iso3))

p5 <- plot_ly(country_counts, type = "choropleth",
              locations = ~iso3,
              locationmode = "ISO-3",
              z = ~collaborators,
              text = ~paste(country_name, "<br>", collaborators, "collaborators"),
              colorscale = list(c(0, "#F5F4EF"), c(1, economist_red)),
              hovertemplate = "%{text}<extra></extra>",
              showscale = FALSE) %>%
  layout(
    geo = list(
      showframe = FALSE,
      showcoastlines = TRUE,
      coastlinecolor = "#D9D9D9",
      countrycolor = "#D9D9D9",
      showland = TRUE,
      landcolor = "#F5F4EF",
      projection = list(type = "natural earth"),
      bgcolor = "white"
    ),
    paper_bgcolor = "white",
    margin = list(l = 0, r = 0, t = 0, b = 0)
  )

saveWidget(p5, "images/plot_world_map.html", selfcontained = TRUE)
message("Saved: plot_world_map.html")

message("\nAll dashboard plots saved!")