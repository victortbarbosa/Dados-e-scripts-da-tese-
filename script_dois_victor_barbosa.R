install.packages("ggplot2")
install.packages("dplyr")
install.packages("readxl")
install.packages("tidyr")
install.packages("readxl", type = "binary")


library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)


dados <- read_excel("C:/Users/vi0_1/Documents/Tese/parte quali/matriz_bolha.xlsx")

View(dados)

dados_longos <- dados %>%
  pivot_longer(
    cols = -Categoria, # Todas as colunas, exceto "Categoria"
    names_to = "Ano",
    values_to = "Frequencia"
  )

View(dados_longos)


# heat map 

ggplot(dados_longos, aes(x = Ano, y = Categoria, fill = Frequencia)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "black") +
  labs(
    title = "",
    x = NULL,
    y = NULL,
    fill = "Frequência"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )



# soma por ano 

library(dplyr)
library(ggplot2)

# Soma das frequências por ano
dados_resumo <- dados_longos %>%
  group_by(Ano) %>%
  summarise(Soma_Frequencia = sum(Frequencia, na.rm = TRUE))

dados_resumo$Ano <- as.numeric(as.character(dados_resumo$Ano))

# Gráfico de linha
ggplot(dados_resumo, aes(x = Ano, y = Soma_Frequencia)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "red", size = 2) +
  labs(title = "Evolução da Soma das Frequências por Ano",
       x = "Ano",
       y = "Soma das Frequências") +
  theme_minimal()

# soma por ano 

ggplot(dados_resumo, aes(x = as.numeric(Ano), y = Soma_Frequencia)) +
  geom_line(color = "gray", linewidth = 1.2) +
  geom_point(color = "black", size = 2) +
  labs(title = "",
       x = "ano",
       y = "soma") +
  scale_x_continuous(breaks = sort(unique(dados_resumo$Ano))) +
  theme_minimal()


# Criar os dados
pib_china <- data.frame(
  Ano = c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021),
  PIB = c(1.21, 1.66, 2.75, 5.10, 8.53, 11.06, 13.89, 17.82)
)

# Gerar o gráfico
ggplot(pib_china, aes(x = Ano, y = PIB)) +
  geom_line(color = "gray", lwd = 1.2) +   # Linha azul
  geom_point(color = "black", size = 2.5) +  # Pontos vermelhos
  labs(
    title = "",
    x = "ano",
    y = "PIB (trilhões de dólares)"
  ) +
  scale_x_continuous(breaks = pib_china$Ano) +  # Garante que todos os anos apareçam no eixo X
  theme_minimal()



##########################################################################
##########################################################################
##########################################################################

dados_rs <- read_excel("C:/Users/vi0_1/Documents/Tese/Revisão sistemática (china-africa)/quadro.xlsx")

View(dados_rs)

library(forcats)
install.packages('ggalluvial')
library(ggalluvial)


### barra para cada área temática, dividida pelas abordagens metodológicas.

ggplot(
  dados_rs %>%
    count(`Área temática`, `Abordagem metodológica`) %>%
    group_by(`Área temática`) %>%
    mutate(total = sum(n)) %>%
    ungroup() %>%
    mutate(`Área temática` = fct_rev(fct_reorder(`Área temática`, total))),
  aes(
    y = `Área temática`,
    x = n,
    fill = `Abordagem metodológica`
  )
) +
  geom_col(width = 0.75) +
  scale_x_continuous(
    breaks = scales::breaks_width(1),
    expand = c(0, 0)
  ) +
  labs(
    x = "número de obras",
    y = NULL,
    fill = "Abordagem metodológica"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )


## linha do tempo

ggplot(
  dados_rs %>%
    arrange(Ano) %>%
    group_by(Ano) %>%
    mutate(nivel = row_number()) %>%
    ungroup(),
  aes(x = Ano)
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.8,
    colour = "grey40"
  ) +
  geom_segment(
    aes(
      xend = Ano,
      y = 0,
      yend = nivel * 0.35,
      colour = `Área temática`
    ),
    linewidth = 0.5,
    alpha = 0.8
  ) +
  geom_point(
    aes(
      y = nivel * 0.35,
      colour = `Área temática`
    ),
    size = 3.8
  ) +
  scale_colour_brewer(
    palette = "Dark2",
    name = "Área temática"
  ) +
  scale_x_continuous(
    breaks = seq(min(dados_rs$Ano), max(dados_rs$Ano), by = 2),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    x = "Ano de publicação",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  )


## sankey

ggplot(
  dados_rs %>%
    mutate(
      `Área curta` = case_when(
        `Área temática` == "Antecedentes históricos e sua trajetória de desenvolvimento" ~ "Antecedentes\nhistóricos",
        `Área temática` == "Caracterização da ajuda externa" ~ "Caracterização",
        `Área temática` == "Relação entre ajuda externa e recursos energético" ~ "Ajuda e\nrecursos\nenergéticos",
        `Área temática` == "Transferência de conhecimento e tecnologia" ~ "Know-how",
        `Área temática` == "Determinantes da alocação" ~ "Determinantes",
        `Área temática` == "Comparativo com outros doadores" ~ "Comparativo",
        `Área temática` == "Impacto da ajuda chinesa" ~ "Impacto",
        TRUE ~ `Área temática`
      )
    ) %>%
    count(
      `Área curta`,
      `Área temática`,
      `Abordagem metodológica`,
      `Fonte de dados principal sobre a China`
    ),
  aes(
    axis1 = `Área curta`,
    axis2 = `Abordagem metodológica`,
    axis3 = `Fonte de dados principal sobre a China`,
    y = n
  )
) +
  geom_alluvium(
    aes(fill = `Área temática`),
    alpha = 0.8,
    width = 0.2
  ) +
  geom_stratum(
    width = 0.2,
    fill = "grey95",
    colour = "grey40"
  ) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 3
  ) +
  scale_x_discrete(
    limits = c("Área temática", "Metodologia", "Fonte de dados"),
    expand = c(0.08, 0.08)
  ) +
  scale_fill_brewer(
    palette = "Set1",
    name = "Área temática"
  ) +
  labs(
    x = NULL,
    y = "Número de estudos"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  )



