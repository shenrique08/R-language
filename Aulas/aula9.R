library(rvest) # biblioteca utilizada para fazer a raspagem
library(dplyr) # bibliotea para manipular os dados
library(ggplot2)


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







