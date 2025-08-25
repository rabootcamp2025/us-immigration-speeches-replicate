library(quanteda)
library(readtext)
library(spacyr)
library(quanteda.corpora)
library(tidyverse)
library(rlist)
library(jsonlite)
library(purrr)
library(ggplot2)

indir <- "D:/git-project/hein_bound_imm"
outdir <- "D:/git-project/data/intermediate/hein_output"


# descr_file <- readtext(paste0(indir, "/descr*.txt"))
# 
# descr_split <- descr_file |>
#   mutate(text_split = str_split(text, "\\|")) |>
#   unnest_wider(text_split, names_sep = "_")

# ファイル一覧の取得
descr_file_paths <- list.files(indir, pattern = "^descr.*\\.txt$", full.names = TRUE)

# 全ファイルの読み込み
descr_split <- map_dfr(descr_file_paths, function(file) {
  lines <- readLines(file)  # 各行をベクトルとして読み込む
  
  tibble(text = lines) |>
    mutate(file = basename(file)) |>
    separate(text, into = as.character(c(1:15)), sep = "\\|")
})







# ファイル一覧の取得
file_paths <- list.files(indir, pattern = "^speeches.*\\.txt$", full.names = TRUE)

# 全ファイルの読み込み
speeches_split <- map_dfr(file_paths, function(file) {
  lines <- readLines(file)  # 各行をベクトルとして読み込む
  
  tibble(text = lines) |>
    mutate(file = basename(file)) |>
    separate(text, into = c("id", "text"), sep = "\\|")
})



# speeches_file <- readtext(paste0(indir, "/speeches*.txt"))
# 
# speeches_split <- speeches_file |>
#   mutate(text_split = str_split(text, "^\\d+\\|")) |>
#   unnest_wider(text_split, names_sep = "_") 
#   
# speeches_split <- speeches_file |>
#   mutate(
#     id = str_extract(text, "^\\d+"),                      # 行頭の数字（ID）を抽出
#     speech_text = str_remove(text, "^\\d+\\|")            # IDとパイプ記号を取り除いて発言だけにする
#   )

speech_dates <- descr_split |>
  select(
    id = "1", 
    date = "3")
  
df_speech <- 
  left_join(speeches_split, speech_dates, by = "id")

df_speech <- df_speech |>
  filter(date != "18940614") 

speech_corpus <- quanteda::corpus(
  df_speech,
  docid_field = "id",
  text_field = "text"
)

sents <- tokens(speech_corpus, what = "sentence")
tokens <- tokens(sents, what = "word")

sents_characters <- lapply(sents, as.character)
tokens_characters <- lapply(tokens, as.character)


outlines <- list(
  id = df_speech$id,
  sents = sents_characters,
  tokens = tokens_characters
)

write_json(outlines, path =  paste0(outdir, "/speeches.jsonlist"))

# speech dates作成 -------------------------------------------------------
# tail(speeches_split)
# ncol(speeches_split)
# colnames(speeches_split)
# tail(df_speech)
