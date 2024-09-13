library(rvest) # biblioteca utilizada para fazer a raspagem
library(dplyr) # bibliotea para manipular os dados
library(ggplot2)
library(stringr)


url <- "https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_alfabetiza%C3%A7%C3%A3o"

html <- read_html(url)
html


tabelas <- html |>
  html_elements("table") |>
  html_table()



alfabetizacao <- tabelas[[3]]
tabelas[[3]]

View(tabelas[[3]])

alfabetizacao <- alfabetizacao[,c(2, 3)]
names(alfabetizacao) <- c("estado", "taxa")
names(alfabetizacao)


library(stringr)
parte1 <- str_replace_all(string = alfabetizacao$taxa, pattern = ",", replacement = ".")
parte2 <- str_replace_all(string = parte1, pattern = "%", replacement = "")

parte_final <- as.numeric(parte2)
parte_final <- parte_final / 100

alfabetizacao$taxa <- parte_final

View(alfabetizacao)


install.packages("geobr")
library(geobr)


minas <- read_state(code_state = "MG")
library(ggplot2)

ggplot(data = minas)+
  geom_sf(fill = "darkblue")+
  theme_void()


municipios_mg <- read_municipality(code_muni = "MG")

ggplot(data = municipios_mg)+
  geom_sf()+
  theme_void()


estados <- read_state()

estados$name_state
order(estados$name_state)
estados[2, ]
estados <- estados[order(estados$name_state),]
alfabetizacao <- alfabetizacao[order(alfabetizacao$estado),]

# cria na tabela estado uma coluna taxa colocando os valoress de alfabetização
estados$taxa <- alfabetizacao$taxa

ggplot(data = estados, aes(fill = taxa)) +
  geom_sf() +
  scale_fill_gradient(high = "#331F4F", low = "#6A2470")




url <- "https://www.timeout.com/film/best-horror-films"
html <- read_html(url)

nomes <- html |> 
  html_elements("h3._h3_cuogz_1") |>
  html_text2()

posicao <- str_extract_all(string = nomes, pattern = "^\\d+") # o ^ pega o inicio da frase o \\d é para pegar os digitos e o + é para pegar 1 ou mais vezes
posicao <- unlist(posicao)
posicao <- as.numeric(posicao)

anos <- str_extract_all(string = nomes, pattern = "\\(\\d+\\)$") # $ pega o fim da frase
anos <- unlist(str_extract_all(anos, "\\d+")) 
anos <- as.numeric(anos)

titulos <- str_remove_all(string = nomes, "^\\d+\\.\\s+") # o \\s significa espaço
titulos <- str_remove_all(string = titulos, "\\s+\\(\\d+\\)$")

filmes <- data.frame(posicao, anos, titulos)

write.csv(filmes, file = "files_horror.csv", row.names = FALSE) # cria um arquivo csv


url2 <- "https://www.bosshunting.com.au/entertainment/movies/best-movies-imdb/"
html2 <- read_html(url2)

dados <- html2 |>
  html_elements("ol.wp-block-list>li") |>
  html_text2()


anos2 <- str_extract_all(dados, "\\(\\d+\\)")
anos2 <- unlist(str_extract_all(anos2, "\\d+")) 
anos2 <- as.numeric(anos2)

diretores <- str_extract_all(dados, "dir\\.")




