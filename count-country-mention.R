
# Preparation --------------------------------------------
install.packages("quanteda")
install.packages("readtext")
install.packages("spacyr")
devtools::install_github("quanteda/quanteda.corpora")
devtools::install_github("kbenoit/quanteda.dictionaries")
install.packages("tidyverse")
install.packages("rlist")


library(quanteda)
library(readtext)
library(spacyr)
library(quanteda.corpora)
library(quanteda.dictionaries)
library(tidyverse)
library(rlist)
library(jsonlite)
library(purrr)


# Load data ---------------------------------------------------------------
filepath <- "D:/git-project/data/raw/data/speeches/Congress/imm_segments_with_tone_and_metadata.jsonlist"
read_jsonlist <- function(filepath) {
  # 一行ずつ読み込み、各行をJSONとしてパースしてリストに格納
  lines <- readLines(filepath, warn = FALSE)
  parsed <- lapply(lines, fromJSON)
  return(parsed)
}

list_speech <- read_jsonlist(filepath)

# Create data frame -------------------------------------------------------
df_speech <- tibble(list_speech)
df_speech <- bind_rows(list_speech)
head(df_speech)

countries_low <- map(countries, str_to_lower)

df_speech_low <- df_speech %>% 
  mutate(text_low = str_to_lower(text))


# prepare dataframe ---------------------------------------------------
country_list <- str_to_lower(names(countries_low))
congress_range <- min(df_speech_low$congress) : max(df_speech_low$congress)

df_count_by_country <- matrix(0,
                              nrow = length(congress_range),
                              ncol = length(country_list),
                              dimnames = list(congress_range, country_list)) %>%
  as.data.frame()


# count countries words ---------------------------------------------------
for (i in 1:nrow(df_speech_low)) {
  word_set <- unique(
    str_split(df_speech_low$text_low[i], pattern = " ")
  )
  for (j in 1:length(country_list)) {
    pattern_str <- paste(countries_low[[1]], collapse = "|")
    hits <- str_detect(word_set, pattern = pattern_str)
    if (any(hits)) {
      df_count_by_country[df_speech_low$congress[i] , j] + 1
    }
  }
}
