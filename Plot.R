library(tidyverse)
library(plotly)
library(htmlwidgets)
library(visNetwork)
library(geomnet)
library(igraph)
library(wordcloud2)
library(pandoc)

summary_df <- readRDS("summary_df.rds")
big10short <- readRDS("big10short.rds")
nodes <- readRDS("nodes.rds")
edges <- readRDS("edges.rds")

## Create heatmap of collaboration
edge_heatmap <- edges %>%
  ggplot(aes(x = from, y = to, fill = width, text = text)) +
  geom_tile(color = "black", lwd = 5) +
  scale_fill_distiller(palette = "RdYlBu", name = "Collaborative Papers") +
  xlab("Institution 2") +
  ylab("Institution 1") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90)
  )
ggplotly(edge_heatmap, tooltip = "text")
saveWidget(ggplotly(edge_heatmap, tooltip = "text"),
  file = here::here("widgets", "ggplotlyHeatmap.html"),
  selfcontained = TRUE
)

# Use colors of universities
colors <- data.frame(
  id = big10short,
  color = c(
    "#2774AE",
    "#FF5F05",
    "#990000",
    "#FFCD00",
    "#E21833",
    "#FFCB05",
    "#18453B",
    "#7A0019",
    "#D00D2C",
    "#4E2A84",
    "#CE0F3D",
    "#007030",
    "#001E44",
    "#CC0033",
    "#990000",
    "#4B2E83",
    "#C5050C",
    "#CFB991"
  )
)
# Duplicate edges to have highlights to/from each school
edges_new <- edges %>%
  ungroup() %>%
  mutate(newfrom = to) %>%
  mutate(newto = from) %>%
  select(-from) %>%
  select(-to) %>%
  rename(to = newto) %>%
  rename(from = newfrom)
edges1 <- rbind(edges, edges_new)

## Create network graph
network_graph <- visNetwork(
  nodes %>%
    left_join(., colors, by = "id") %>%
    mutate(title = paste0(id, "\nPublications: ", value)) %>%
    mutate(font.size = 20) %>%
    ungroup() %>%
    mutate(label = ""),
  edges1 %>%
    mutate(title = paste0(from, ", ", to, " Shared Publications: ", width)) %>%
    mutate(width = width * 0.01),
  width = "100%",
  height = "600px"
) %>%
  visNodes(label = NULL) %>%
  visIgraphLayout(
    layout = "layout_in_circle",
    physics = FALSE,
    smooth = FALSE
  ) %>%
  visOptions(
    highlightNearest = list(
      enabled = TRUE,
      hover = TRUE,
      hideColor = "rgba(0, 0, 0, 0.2)",
      degree = 0,
      algorithm = "all"
    ),
    nodesIdSelection = T,
    collapse = list(enabled = TRUE),
    autoResize = TRUE
  )
network_graph
visSave(network_graph,
  file = here::here("widgets", "network.html"),
  selfcontained = TRUE
)

## Get most common keywords
words <- summary_df %>%
  select(keywords) %>%
  filter(keywords != "") %>%
  separate_longer_delim(cols = keywords, delim = ";") %>%
  group_by(keywords) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  mutate(keywords = case_when(
    keywords == "Covid-19" ~ "COVID-19",
    keywords == "Hiv" ~ "HIV",
    keywords == "Sars-Cov-2" ~ "SARS-CoV-2",
    keywords == "Mri" ~ "MRI",
    keywords == "Fmri" ~ "fMRI",
    keywords == "Chatgpt" ~ "ChatGPT",
    keywords == "Gwas" ~ "GWAS",
    keywords == "Auc" ~ "AUC",
    keywords == "Bcr" ~ "BCR",
    keywords == "Copd" ~ "COPD",
    keywords == "Hbcd" ~ "HBCD",
    keywords == "Rna-Seq" ~ "RNA-Seq",
    keywords == "Bmi" ~ "BMI",
    keywords == "Ptsd" ~ "PTSD",
    keywords == "Ai" ~ "AI",
    keywords == "Hpv" ~ "HPV",
    keywords == "Eeg" ~ "EEG",
    keywords == "Mtor" ~ "mTOR",
    keywords == "Crispr" ~ "CRISPR",
    keywords == "Rct" ~ "RCT",
    keywords == "Hba1c" ~ "HbA1c",
    keywords == "Lvad" ~ "LVAD",
    keywords == "Gpt" ~ "GPT",
    keywords == "Tavr" ~ "TAVR",
    .default = keywords
  ))

# Make wordcloud
wordcloud <- wordcloud2(
  words,
  color = sample(colors$color, nrow(words), replace = TRUE)
)
saveWidget(wordcloud,
  here::here("widgets", "wordcloud.html"),
  selfcontained = TRUE
)
shaped_wordcloud <- letterCloud(
  words,
  color = sample(colors$color, nrow(words), replace = TRUE),
  word = "B1G"
)
saveWidget(shaped_wordcloud,
  here::here("widgets", "shaped_wordcloud.html"),
  selfcontained = TRUE
)

## Make shared pub keyword cloud
shared_words <- summary_df %>%
  select(affiliations, keywords) %>%
  filter(keywords != "")
shared <- sapply(big10short, grepl, x = shared_words$affiliations) %>%
  as.data.frame()
shared$multiple <- rowSums(shared) > 2
shared_words$multiple <- shared$multiple
shared_words <- shared_words %>%
  select(keywords, multiple) %>%
  filter(multiple == TRUE) %>%
  select(keywords) %>%
  separate_longer_delim(cols = keywords, delim = ";") %>%
  group_by(keywords) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  mutate(keywords = case_when(
    keywords == "Covid-19" ~ "COVID-19",
    keywords == "Hiv" ~ "HIV",
    keywords == "Sars-Cov-2" ~ "SARS-CoV-2",
    keywords == "Mri" ~ "MRI",
    keywords == "Fmri" ~ "fMRI",
    keywords == "Chatgpt" ~ "ChatGPT",
    keywords == "Gwas" ~ "GWAS",
    keywords == "Auc" ~ "AUC",
    keywords == "Bcr" ~ "BCR",
    keywords == "Copd" ~ "COPD",
    keywords == "Hbcd" ~ "HBCD",
    keywords == "Rna-Seq" ~ "RNA-Seq",
    keywords == "Bmi" ~ "BMI",
    keywords == "Ptsd" ~ "PTSD",
    keywords == "Ai" ~ "AI",
    keywords == "Hpv" ~ "HPV",
    keywords == "Eeg" ~ "EEG",
    keywords == "Mtor" ~ "mTOR",
    keywords == "Crispr" ~ "CRISPR",
    keywords == "Rct" ~ "RCT",
    keywords == "Hba1c" ~ "HbA1c",
    keywords == "Lvad" ~ "LVAD",
    keywords == "Gpt" ~ "GPT",
    keywords == "Tavr" ~ "TAVR",
    .default = keywords
  ))

shared_wordcloud <- wordcloud2(
  shared_words,
  color = sample(colors$color, nrow(words), replace = TRUE),
)
saveWidget(shared_wordcloud,
  here::here("widgets", "shared_wordcloud.html"),
  selfcontained = TRUE
)

# Compare what words are more/less common in shared/indiv pubs
compare_words <- merge(
  words,
  shared_words %>% rename(shared_freq = freq),
  all.x = TRUE
) %>%
  mutate(ratio = shared_freq / freq)

shared_cluster <- summary_df %>%
  select(keywords) %>%
  filter(keywords != "") %>%
  cbind(shared %>% select(!multiple)) %>%
  separate_longer_delim(cols = keywords, delim = ";") %>%
  filter(keywords %in% compare_words$keywords) %>%
  group_by(keywords) %>%
  mutate(keywords = case_when(
    keywords == "Covid-19" ~ "COVID-19",
    keywords == "Hiv" ~ "HIV",
    keywords == "Sars-Cov-2" ~ "SARS-CoV-2",
    keywords == "Mri" ~ "MRI",
    keywords == "Fmri" ~ "fMRI",
    keywords == "Chatgpt" ~ "ChatGPT",
    keywords == "Gwas" ~ "GWAS",
    keywords == "Auc" ~ "AUC",
    keywords == "Bcr" ~ "BCR",
    keywords == "Copd" ~ "COPD",
    keywords == "Hbcd" ~ "HBCD",
    keywords == "Rna-Seq" ~ "RNA-Seq",
    keywords == "Bmi" ~ "BMI",
    keywords == "Ptsd" ~ "PTSD",
    keywords == "Ai" ~ "AI",
    keywords == "Hpv" ~ "HPV",
    keywords == "Eeg" ~ "EEG",
    keywords == "Mtor" ~ "mTOR",
    keywords == "Crispr" ~ "CRISPR",
    keywords == "Rct" ~ "RCT",
    keywords == "Hba1c" ~ "HbA1c",
    keywords == "Lvad" ~ "LVAD",
    keywords == "Gpt" ~ "GPT",
    keywords == "Tavr" ~ "TAVR",
    .default = keywords
  ))

cluster <- kmeans(shared_cluster %>% select(!keywords), 20)

compare_words %>%
  separate_longer_delim(cols = keywords, delim = ";") %>%
  cbind(cluster$cluster) %>%
  na.omit() %>%
  ggplot(aes(x = freq, y = shared_freq, fill = cluster)) +
  geom_point()
