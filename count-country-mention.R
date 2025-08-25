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


# load countries dict -----------------------------------------------------
# step1 : 国コードの読み込み
countries <- list(
  Ireland = c("Ireland"),
  Germany = c("Germany"),
  Mexico = c("Mexico"),
  Italy = c("Italy"),
  England = c("England"),
  Canada = c("Canada"),
  Russia = c("Russia", "USSR"),
  Poland = c("Poland"),
  China = c("China"),
  India = c("India"),
  Sweden = c("Sweden"),
  Austria = c("Austria"),
  Philippines = c("Philippines", "Philippine"),
  Cuba = c("Cuba"),
  Hungary = c("Hungary"),
  Norway = c("Norway"),
  Czechoslovakia = c("Czechoslovakia", "Czech", "Slovakia", "Slovak"),
  Vietnam = c("Vietnam"),
  Scotland = c("Scotland"),
  `El Salvador` = c("El Salvador"),
  Korea = c("Korea"),
  France = c("France"),
  `Dominican Republic` = c("Dominican"),
  Guatemala = c("Guatemala"),
  Greece = c("Greece"),
  Colombia = c("Colombia"),
  Jamaica = c("Jamaica"),
  Yugoslavia = c("Yugoslavia", "Serbia", "Croatia", "Macedonia", "Bosnia", "Herzegovina", "Montenegro"),
  Honduras = c("Honduras"),
  Japan = c("Japan"),
  Haiti = c("Haiti"),
  Portugal = c("Portugal"),
  Denmark = c("Denmark"),
  Lithuania = c("Lithuania"),
  Switzerland = c("Switzerland"),
  Wales = c("Wales"),
  Taiwan = c("Taiwan"),
  Netherlands = c("Netherlands", "Holland"),
  Brazil = c("Brazil"),
  Finland = c("Finland"),
  Iran = c("Iran"),
  Ecuador = c("Ecuador"),
  Venezuela = c("Venezuela"),
  Romania = c("Romania", "Rumania", "Roumania"),
  Peru = c("Peru")
)


# step 1 ---------------------------------------------------------------
mention_dict <- quanteda::dictionary(countries)



# step 2 ------------------------------------------------------------------

filepath <- "D:/git-project/data/raw/data/speeches/Congress/imm_segments_with_tone_and_metadata.jsonlist"

read_jsonlist_stream <- function(filepath) {
  con <- file(filepath, "r")  # ファイルを読み込みモードで開く
  on.exit(close(con))        # 関数終了時にファイルを閉じる
  
  # stream_inはデフォルトでJSONL形式（1行に1つのJSONオブジェクト）を読み込む
  data <- stream_in(con, verbose = FALSE)
  
  return(data)
}
list_speech <- read_jsonlist_stream(filepath)
df_speech <- tibble(list_speech)
df_speech <- bind_rows(list_speech)

df_speech_low <- df_speech |>
  mutate(text_low = str_to_lower(text))

# IDの付与
df_speech_low <- df_speech_low |>
  mutate(doc_id = 1:nrow(df_speech_low))

# コーパス化
speech_corpus <- quanteda::corpus(
  df_speech_low,
  docid_field = "doc_id",
  text_field = "text_low"
)


# step3 -------------------------------------------------------------------
mentions_dfm <- quanteda::tokens(speech_corpus) |>
  quanteda::tokens_lookup(mention_dict) |>
  quanteda::dfm()

# str(mentions_dfm)
# as.data.frame(mentions_dfm) エラー確認

# step 4 ------------------------------------------------------------------
imm_country_total_counts <- 
  data.frame(quanteda::colSums(mentions_dfm)) |>
  rename(counts = quanteda..colSums.mentions_dfm.)
  
write_json(imm_country_total_counts,
           path = "D:/git-project/data/intermediate/imm_country_total_counts.json")







# Old coding --------------------------------------------------------------

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