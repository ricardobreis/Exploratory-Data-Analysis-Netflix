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
  ggtitle("Episódios por Dia", "Heatmap") +
  labs(x = "Semana", y = "Dia") +
  labs(fill = "Nº de Episódios")

# Frequência por dia da semana

episodio_dia_semana <- episodios_dia %>%
  count(dia_semana_nome)

ggplot(episodio_dia_semana, aes(dia_semana_nome, n)) +
  geom_col(fill = "#5b59d6") +
  coord_polar() +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Frequência por Dia da Semana")

