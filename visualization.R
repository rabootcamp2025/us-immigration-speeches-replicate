library(quanteda)
library(readtext)
library(spacyr)
library(quanteda.corpora)
library(tidyverse)
library(rlist)
library(jsonlite)
library(purrr)
library(ggplot2)

df_imm_counts <- fromJSON("D:/git-project/data/intermediate/imm_country_total_counts.json")

df_imm_counts <- arrange(df_imm_counts, counts)

df_imm_counts <- df_imm_counts |> 
  mutate(country = rownames(df_imm_counts))

df_imm_counts$country <- str_to_title(df_imm_counts$country)

p1 <- df_imm_counts |>
  ggplot(aes(x = counts, y = reorder(country, counts))) +
  geom_col() + 
  theme_bw() +
  labs(x = "Number of Mentions", y = "", title = "Mentions of Immigration in the U.S. Congress (1880 - 2020)")
p1

ggsave("D:/git-project/us-immigration-speeches-replicate/plot1.png", plot = p1)

