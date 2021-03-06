library(tidyverse)
library(pdftools)
library(shiny)

# Download report from https://www.documentcloud.org/documents/5955210-Redacted-Mueller-Report.html
mueller_report_txt <- pdf_text("Redacted-Mueller-Report.pdf")

write_rds(mueller_report_txt, path = "mueller_report/data.rds")

mueller_report <- tibble(
  page = 1:length(mueller_report_txt),
  text = mueller_report_txt
) %>% 
  separate_rows(text, sep = "\n") %>% 
  group_by(page) %>% 
  mutate(line = row_number()) %>% 
  ungroup() %>% 
  select(page, line, text)

# Fix Corney -> Comey
mueller_report <-
  mueller_report %>% 
  mutate(
    text = str_replace(text, "([Cc])orney", "\\1omey")
  )

write_csv(mueller_report, "mueller_report.csv")

mueller_report <- read_csv("mueller_report.csv")

create_wordcloud(mueller_report_txt, num_words = 100, background = "white")
