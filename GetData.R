library(tidyverse)
library(httr)
library(xml2)
library(XML)

# Create list of school names
big10 <- c(
  "University of California, Los Angeles",
  "University of Illinois at Urbana-Champaign",
  "Indiana University Bloomington",
  "Indiana University School of Medicine",
  "Indiana University School of Public Health",
  "University of Iowa",
  "University of Maryland, College Park",
  "University of Maryland School of Medicine",
  "University of Michigan",
  "Michigan State University",
  "University of Minnesota",
  "University of Nebraska-Lincoln",
  "University of Nebraska Medical Center",
  "Northwestern University",
  "The Ohio State University",
  "University of Oregon",
  "Pennsylvania State University",
  "Rutgers",
  "University of Southern California",
  "University of Washington",
  "University of Wisconsin-Madison",
  "Purdue University"
)

# Condensed list of school names
big10short <- c(
  "University of California, Los Angeles",
  "University of Illinois at Urbana-Champaign",
  "Indiana University",
  "University of Iowa",
  "University of Maryland",
  "University of Michigan",
  "Michigan State University",
  "University of Minnesota",
  "University of Nebraska",
  "Northwestern University",
  "The Ohio State University",
  "University of Oregon",
  "Pennsylvania State University",
  "Rutgers University",
  "University of Southern California",
  "University of Washington",
  "University of Wisconsin-Madison",
  "Purdue University"
)
saveRDS(big10short, "big10short.rds")

# Create empty data frame
summary_df <- data.frame(
  pmid = c(),
  title = c(),
  affiliations = c(),
  keywords = c()
)
for (i in big10) {
  ## Get pubmed IDs for institution (up to 10000 most recent in 2024)
  raw_data <- GET("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
    query = list(
      "db" = "pubmed",
      "tool" = "R",
      "email" = "seth.borrowman@northwestern.edu",
      "retmax" = 10000,
      "version" = "2.0",
      "term" = sprintf(
        '(%s[Affiliation]) AND (("2024/01/01"[Date - Publication] : "3000"[Date - Publication]))',
        i
      ),
      "sort" = "date"
    )
  )

  # Format IDs as list for querying
  id_list <- (rawToChar(raw_data$content)) %>%
    xml() %>%
    read_xml() %>%
    xml_find_all(., ".//Id") %>%
    xml_text() %>%
    unlist()

  # Split in batches of 50
  min <- 1L
  top <- 50L
  max <- length(id_list)

  print(i)
  # Query summary in batches
  while (min < max) {
    print(paste(min, "/", max))
    summary <- RETRY("GET",
      url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
      query = list(
        "db" = "pubmed",
        "version" = "2.0",
        "id" = list(id_list[min:top])
      )
    )
    # Add to data frame
    articles <- (rawToChar(summary$content)) %>%
      xml() %>%
      read_xml() %>%
      xml_find_all(., ".//PubmedArticle")

    # Gonna be slow :( Hard to work around tho
    for (article in articles) {
      # Get PMID
      pmid <- xml_text(xml_find_first(article, ".//PMID"))
      # Get affiliations, list with ; separating
      affiliations <- article %>%
        xml_find_all(".//Affiliation") %>%
        xml_text() %>%
        unlist() %>%
        paste0(collapse = ";")
      # Get article title
      title <- xml_text(xml_find_first(article, ".//ArticleTitle"))
      # Get keywords and separate with ;
      keywords <- article %>%
        xml_find_all(".//Keyword") %>%
        xml_text() %>%
        unlist() %>%
        str_to_title() %>%
        paste0(collapse = ";")
      temp_df <- data.frame(
        pmid = pmid,
        title = title,
        affiliations = affiliations,
        keywords = keywords
      )
      # Append
      summary_df <- rbind(summary_df, temp_df)
    }

    # Iteration
    min <- min + 50
    top <- ifelse(top + 50 < max, top + 50, max)
  }
  # Only keep unique PMIDs
  summary_df <- summary_df %>%
    distinct(pmid, .keep_all = TRUE)
}
saveRDS(summary_df, "summary_df.rds")

## Create nodes and edges for network
nodes <- data.frame(
  label = c(),
  value = c()
)
edges <- data.frame(
  from = c(),
  to = c(),
  width = c()
)
# Count total pubs for nodes
for (i in seq_along(big10)) {
  counter <- summary_df %>%
    filter(grepl(big10[i], affiliations))
  nodes[i, 1] <- big10[i]
  nodes[i, 2] <- nrow(counter)
}
colnames(nodes) <- c("label", "value")
nodes <- nodes %>%
  mutate(label = case_when(
    grepl("Indiana", label) ~ "Indiana University",
    grepl("Maryland", label) ~ "University of Maryland",
    grepl("Nebraska", label) ~ "University of Nebraska",
    grepl("Rutgers", label) ~ "Rutgers University",
    .default = label
  )) %>%
  group_by(label) %>%
  summarise(across(value, sum), .groups = "keep") %>%
  mutate(label = factor(label, levels = big10short)) %>%
  mutate(id = label)

saveRDS(nodes, "nodes.rds")

# Count collaborations for edges
for (i in seq_along(1:(length(big10) - 1))) {
  for (j in (i + 1):length(big10)) {
    if (grepl("Indiana", big10[i]) && grepl("Indiana", big10[j])) {
      next()
    } else if (grepl("Maryland", big10[i]) && grepl("Maryland", big10[j])) {
      next()
    } else if (grepl("Nebraska", big10[i]) && grepl("Nebraska", big10[j])) {
      next()
    } else {
      counter <- summary_df %>%
        filter(grepl(big10[i], affiliations) & grepl(big10[j], affiliations))
      edges <- rbind(edges, c(big10[i], big10[j], nrow(counter)))
    }
  }
}
colnames(edges) <- c("from", "to", "width")
edges$width <- as.numeric(edges$width)
edges$from <- factor(edges$from, levels = big10)
edges$to <- factor(edges$to, levels = big10)

# Combine those that had multiple 'schools'
edges <- edges %>%
  mutate(to = case_when(
    grepl("Indiana", to) ~ "Indiana University",
    grepl("Maryland", to) ~ "University of Maryland",
    grepl("Nebraska", to) ~ "University of Nebraska",
    grepl("Rutgers", to) ~ "Rutgers University",
    .default = to
  )) %>%
  mutate(from = case_when(
    grepl("Indiana", from) ~ "Indiana University",
    grepl("Maryland", from) ~ "University of Maryland",
    grepl("Nebraska", from) ~ "University of Nebraska",
    grepl("Rutgers", from) ~ "Rutgers University",
    .default = from
  )) %>%
  group_by(to, from) %>%
  summarise(across(width, sum), .groups = "keep") %>%
  mutate(to = factor(to,
    levels = big10short
  )) %>%
  mutate(from = factor(from,
    levels = big10short
  )) %>%
  droplevels.data.frame()

edges <- edges %>%
  mutate(text = paste0(
    "Institution 1: ", to, "\n",
    "Institution 2: ", from, "\n",
    "Collaborative Papers: ", width
  ))
saveRDS(edges, "edges.rds")
