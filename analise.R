################################################################################################
#
# ANÁLISE DE DADOS
# Por: RICARDO REIS
#
# CASE - NETFLIX
#
#
################################################################################################


# Carrega Pacotes ---------------------------------------------------------

library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)

# Limpando o console.
cat("\014") 
# Limpando o Global Environment.
rm(list = ls())


# Leitura de Dados --------------------------------------------------------

historico <- read.csv("~/R-Projetos/Netflix/NetflixViewingHistory.csv", encoding="UTF-8")
glimpse(historico)
historico$Date <- dmy(historico$Date)

head(historico)
summary(historico)


# Tidying  ----------------------------------------------------------------

# Separando titulo em titulo, temporada e episodio
historico_serie <- historico %>%
  separate(col = Title, into = c("titulo", "temporada", "titulo_episodio"), sep = ': ')

# Filtrando o que for serie
historico_serie <- subset(historico_serie, !is.na(historico_serie$temporada))
historico_serie <- subset(historico_serie, !is.na(historico_serie$titulo_episodio))


# Análise -----------------------------------------------------------------

# Séries maratonadas
maratona <- historico_serie %>%
  count(titulo, Date) %>%
  subset(n >= 6) %>%
  group_by(titulo) %>% 
  summarise(n = sum(n)) %>%
  arrange(desc(n))

maratona %>% 
  top_n(10) %>%
  ggplot(aes(x = reorder(titulo, n), y = n)) +
  geom_col(fill = "#0097d6") +
  coord_flip() +
  ggtitle("Top 10 de Séries Maratonadas no Netflix", "Séries Com 6 ou Mais Episódios Vistos Por Dia") +
  labs(x = "Séries", y = "Episódios") +
  theme_minimal()

# Episódios por dia
episodios_dia <- historico_serie %>%
  count(Date) %>%
  arrange(desc(n))

episodios_dia %>%
  ggplot(aes(x = Date, y = n)) +
  geom_col(color = c("#0097d6")) +
  theme_minimal() +
  ggtitle("Episódios Por Dia", "Histórico de 2017 à 2020") +
  labs(x = "Data", y = "Episódios") 
