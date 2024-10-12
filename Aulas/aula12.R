library(rvest)
library(dplyr)
library(tidytext)
library(tidyr) # organizar um conjunto
library(ggplot2)
library(stopwords)
library(rvest) # raspar dados
library(stringr)

library(janeaustenr) # todos livros da jane austen

url <- "https://www.bbc.com/portuguese/articles/c3dv8yy3d8jo"


html <- read_html(url)
html    

noticia <- html |>
  html_elements("p.bbc-hhl7in") |>
  html_text2()

noticia

noticia <- paste(noticia, collapse = " ")

noticia

artigos <- data.frame(noticia)

artigos |>
  unnest_tokens(output = words, input = noticia) |>
  count(words, sort = TRUE) |>
  top_n(10) # pega as 10 palavras mais frequentes

stopwords_br <- data.frame(word = stopwords("pt"))

frequentes <- artigos |>
  unnest_tokens(output = word, input = noticia) |>
  anti_join(stopwords_br) |>
  count(word, sort = TRUE) |>
  top_n(10) |>
  mutate(word = reorder(word, n))

ggplot(frequentes, aes(x = n, y = word)) +
  geom_col(fill = "orangered4") +
  theme_minimal()

#_______________________________________________________________________________

livro <- prideprejudice # todo orgulho e preconceito

conjunto2 <- data.frame(livro)

stopword_en <- data.frame(word = stopwords("en"))

conjunto2 |>
  unnest_tokens(output = word, input = livro) |>
  anti_join(stopword_en) |>
  count(word, sort = TRUE) |>
  top_n(10) |>
  mutate(word = reorder(word, n))

ggplot(frequentes, aes(x = n, y = word)) +
  geom_col(fill = "lightblue") +
  theme_minimal()

sentimentos <- get_sentiments("bing")
capitulos <- str_detect(conjunto2$livro, "^Chapter \\d+")
capitulos <- cumsum(capitulos)

conjunto2 |>
  mutate(capitulo = capitulos) |>
  unnest_tokens(word, livro) |>
  inner_join(sentimentos) |>
  count(capitulo, sentiment) |>
  spread(sentiment, n, fill = 0) |>
  mutate(total = positive - negative) |>
  ggplot(aes(x = capitulo, y = total, fill = total > 0)) +
  geom_col() +
  theme_minimal()
