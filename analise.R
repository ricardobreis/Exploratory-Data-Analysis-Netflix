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
library(plotly)
library(wordcloud2)
library(forcats)

# Limpando o console.
cat("\014") 
# Limpando o Global Environment.
rm(list = ls())


# Leitura de Dados --------------------------------------------------------

historico <- read.csv("~/R-Projetos/Netflix/NetflixViewingHistory.csv", encoding="UTF-8")
glimpse(historico)
head(historico)
summary(historico)


# Tidying  ----------------------------------------------------------------

historico$Date <- dmy(historico$Date)

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
  labs(
    title = "Top 10 de Séries Maratonadas no Netflix",
    subtitle = "Séries Com 6 ou Mais Episódios Vistos Por Dia",
    x = "Séries",
    y = "Episódios"
  )

# Episódios por dia
episodios_dia <- historico_serie %>%
  count(Date) %>%
  arrange(desc(n))

ggplot(episodios_dia, aes(x = Date, y = n)) +
  geom_col(color = c("#0097d6")) +
  labs(
    title = "Episódios Por Dia",
    subtitle = "Histórico de 2017 à 2020",
    x = "Data",
    y = "Episódios"
  )

# Heatmap do calendário
episodios_dia <- episodios_dia %>% arrange(Date)
episodios_dia$dia_semana_numero <- wday(episodios_dia$Date)
episodios_dia$dia_semana_nome <- weekdays(episodios_dia$Date, abbreviate = T)
episodios_dia$mes_nome <- months(episodios_dia$Date, abbreviate = T)
episodios_dia$dia_semana_nome <- factor(episodios_dia$dia_semana_nome, levels = rev(c("dom", "seg","ter","qua","qui","sex","sáb")), labels = rev(c("Dom","Seg","Ter","Qua","Qui","Sex","Sáb")))
episodios_dia$mes_nome <- factor(month(episodios_dia$Date),levels = as.character(1:12), labels = c("Janeiro","Fevereiro","Março","Abril","Maio","Junho","Julio","Agosto","Setembro","Outubro","Novembro","Dezembro"))
episodios_dia$ano_mes <- factor(as.yearmon(episodios_dia$Date)) 
episodios_dia$semana <- as.numeric(format(episodios_dia$Date,"%W"))
episodios_dia$semana_mes <- ceiling(day(episodios_dia$Date) / 7)

ggplot(episodios_dia, aes(semana_mes, dia_semana_nome, fill = episodios_dia$n)) + 
  geom_tile(colour = "white") + 
  facet_grid(year(episodios_dia$Date) ~ mes_nome) + 
  scale_fill_gradient(low = "#FFD000", high = "#FF1919") + 
  labs(
    title = "Episódios por Dia", 
    subtitle = "Heatmap",
    x = "Semana",
    y = "Dia",
    fill = "Nº de Episódios"
  )

# Frequência por dia da semana
episodio_dia_semana <- episodios_dia %>%
  count(dia_semana_nome)

ggplot(episodio_dia_semana, aes(dia_semana_nome, n)) +
  geom_col(fill = "#0097d6") +
  coord_polar() +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 16, face = "bold")) +
  labs(
    title = "Frequência por Dia da Semana"
  )


# Frequência por mês
episodios_mes <- episodios_dia %>%
  count(mes_nome)


ggplot(episodios_mes, aes(mes_nome, n)) +
  geom_col(fill = "#0097d6") +
  coord_polar()  +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  labs(
    title = "Frequência por Mês"
  )

# Frequência por mês e ano
episodios_mes_ano <- episodios_dia %>%
  count(ano_mes)

ggplot(episodios_mes_ano, aes(ano_mes, n)) +
  geom_col(fill = "#0097d6") +
  coord_polar()  +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  labs(
    title = "Frequência por Mês e Ano"
  )

# Worldcloud de séries
contagem_nuvem <- historico_serie %>%
  count(titulo) %>%
  arrange(desc(n)) %>%
  top_n(50, n)

wordcloud2(
  data = contagem_nuvem, 
  size = 0.7, 
  shape = 'pentagon'
)

