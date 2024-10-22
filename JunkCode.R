library(googlesheets4)
library(tidyverse)
library(janitor)

k <- read_sheet("https://docs.google.com/spreadsheets/d/1jPKow4_vQ8CMUBfIYf_SyXNC-6hJNsg1AP0zcS-pOiY/edit?gid=386609990#gid=386609990", sheet = "PlatformCodes")

k_plat <- k %>% 
  select(Timestamp, Code) %>% 
  filter(!is.na(Code)) %>%  # Remove rows where 'Code' is NA
  separate_rows(Code, sep = ", ") %>% 
  mutate(Code = case_when(
    Code == "NoOpinion" ~ "No opinion on platform",
    Code == "NotFam" ~ "Not familiar with options",
    Code == "WillGo" ~ "Will go where group goes",
    Code == "SelfHost" ~ "Any platform that is self hosted", 
    Code == "DontLike" ~ "Dislike options", 
    TRUE ~ Code
  )) %>% 
  arrange(Code) %>% 
  count(Code, sort = TRUE) %>% 
  arrange(desc(n))

ggplot(k_plat, aes(x = fct_reorder(Code, n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +  # Set the fill color to blue
  geom_text(aes(label = n), 
            hjust = -0.2,  # Adjust horizontal position of labels
            size = 3) +     # Set the text size
  coord_flip() +
  labs(
    title = "Platform Preferences Counts",
    x = "Platform",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)  # Adjust margins: top, right, bottom, left
  )

