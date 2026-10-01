
# autor: Victor Tavares Barbosa

# diretório ####################################################################

setwd('C:/Users/vi0_1/Documents/Tese/Capítulo 5/quanti')
getwd()

# pacotes utilizados ###########################################################

library(dplyr)
library(readxl)
library(ggplot2)
library(flextable)
library(ggstatsplot)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(readr)
library(tidyr)
library(fixest)
library(lmtest)
library(sandwich)
library(tseries)
library(plm)
options(scipen = 999) 

# carregando a base de dados de ajuda externa chinesa ##########################

raw_data <- read_excel('../quanti/AidDatasGlobalChineseDevelopmentFinanceDataset_v3.0.xlsx',
                    5)

# separando as variáveis de interesse ##########################################

# salvando uma base de dados apenas com as variáveis de interesse, mas com dados
# ##############de todos os países##############################################

data_global <- raw_data %>%
  select('AidData Record ID', Status, Recipient, 'Recipient ISO-3', 
         'Recipient Region', 'Commitment Year', Intent, 'Flow Type Simplified',
         'Flow Class', 'Sector Name', Infrastructure, COVID,
         'Funding Agencies', 'Funding Agencies Type', 'Direct Receiving Agencies',
         'Direct Receiving Agencies Type', 'Original Currency', 
         'Amount (Constant USD 2021)')


# selecionando apenas os países africanos ######################################

data_africa <- data_global %>%
  filter(`Recipient Region` == "Africa") %>%
  filter(Recipient != "Africa, regional") %>%
  rename(
    id = `AidData Record ID`,
    status = Status,
    recipient = Recipient,
    recipient_iso_3 = `Recipient ISO-3`,
    region = `Recipient Region`,
    year = `Commitment Year`,
    intent = Intent,
    flow_type = `Flow Type Simplified`,
    flow_class = `Flow Class`,
    sector = `Sector Name`,
    infrastructure = Infrastructure,
    covid = COVID,
    funding_agency = `Funding Agencies`,
    funding_agency_type = `Funding Agencies Type`,
    direct_receiving_agency = `Direct Receiving Agencies`,
    direct_receiving_agency_type = `Direct Receiving Agencies Type`,
    original_currency = `Original Currency`,
    amount = `Amount (Constant USD 2021)`
  ) %>%
  filter(status %in% c("Completion", "Implementation"))



write.csv2(data_africa, file = "data_africa.csv")


################### análise descritiva dos dados ###############################

# soma entre 2000 e 2021 #######################################################

format(sum(data_africa$amount, na.rm = TRUE), 
       scientific = FALSE, big.mark = ".", decimal.mark = ",")



# maior e menor valor ##########################################################

print(format(range(data_africa$amount, na.rm = TRUE), 
             scientific = FALSE, big.mark = ".", decimal.mark = ","))


# agrupando por ano ############################################################

data_year <- data_africa %>%
  group_by(year) %>%
  summarise(total = sum(amount, na.rm = TRUE))

# gráfico montante por ano #####################################################

grafico_year <- ggplot(data_year, aes(x = as.factor(year), y = total)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-9, suffix = "B")) +  
  scale_x_discrete(labels = data_year$year) +
  labs(x = "ano", y = "ajuda externa (bilhões)")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(grafico_year)

View(data_year)

# total por ano 

data_africa %>%
  group_by(year) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    total = scales::comma(total, scale = 1e-9, suffix = "B")
  ) %>%
  arrange(desc(year)) %>%
  select(year, total) %>%
  print(n = Inf)

# total por ano e país (há um filtro para ver o ano específico)


data_africa %>%
  filter(year == 2015) %>%
  group_by(recipient) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>%
  mutate(total_fmt = label_number(scale_cut = cut_short_scale())(total)) %>%
  print(n = Inf)

# agrupando por país ###########################################################

data_country <- data_africa %>%
  group_by(recipient) %>%
  summarise(total = sum(amount, na.rm = TRUE))


# país que mais e país que menos recebeu #######################################
highest <- data_country %>%
  slice_max(order_by = total)

# qual país menos recebeu
lowest <- data_country %>%
  slice_min(order_by = total)

# Exibir os resultados
print(highest)
print(lowest)

# média e mediana 

print(format(summary(data_country$total, na.rm = TRUE),
             scientific = FALSE,
             big.mark = ".",
             decimal.mark = ","))

View(data_country)


# gráfico valor total por país #################################################


grafico_country <- ggplot(data_country, aes(x = reorder(recipient, -total), 
                                       y = total)) +
  geom_col(fill = "black") +
  labs(x = "país",
       y = "ajuda externa (bilhões)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-9, suffix = "B"))

print(grafico_country)


## apenas o resultado 

data_country %>%
  group_by(recipient) %>%
  summarise(total = sum(total, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  mutate(total_formatado = scales::comma(total / 1e9, suffix = "B")) %>%
  print(n = Inf)


## valor total por país específico


data_africa %>%
  filter(recipient == "Ethiopia") %>%
  group_by(year) %>%
  summarise(amount_total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = amount_total)) +
  geom_col() +
  geom_text(
    aes(label = scales::comma(amount_total / 1e9, accuracy = 0.1, suffix = "B")),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "",
    x = "ano",
    y = "total (bilhões)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-9, suffix = "B"))


# gráfico de linhas por país ###################################################


data_africa %>%
  group_by(recipient, year) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = total, group = recipient, color = recipient)) +
  geom_line(size = 0.8, alpha = 0.7) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_y_continuous(labels = comma_format(scale = 1e-9, suffix = "B")) +
  scale_x_continuous(breaks = unique(data_africa$year)) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7)
  ) +
  labs(
    x = "ano",
    y = "ajuda externa (bilhões)"
  )


##### quantidade de recipientes por ano 

data_africa %>%
  group_by(year) %>%
  summarise(n_recipients = n_distinct(recipient)) %>%
  print(n = Inf)

### mapa de calor (país + ano)


data_africa %>%
  filter(year >= 2000, year <= 2021) %>%
  group_by(year, recipient) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(recipient = reorder(recipient, total, sum)) %>%
  ggplot(aes(x = factor(year), y = recipient, fill = total)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c(
      "white",
      "#deebf7",
      "#9ecae1",
      "#4292c6",
      "#08519c"
    ),
    trans = "sqrt",
    labels = comma_format(scale = 1e-9, suffix = "B")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
    axis.text.y = element_text(size = 6)
  ) +
  labs(
    title = "",
    x = "ano",
    y = "beneficiário",
    fill = "ajuda externa (bilhões)"
  )

# Gráfico de distribuição por no ano específico ##########################

data_africa %>%
  filter(year == 2017) %>%
  group_by(recipient) %>%
  summarise(total_amount = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = reorder(recipient, -total_amount), y = total_amount)) +
  geom_col(fill = "#2C77B8", width = 0.7) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-9, suffix = "B")) +
  labs(
    x = "país",
    y = "ajuda externa (bilhões)",
    title = ""
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

View(data_africa)

# apenas o resultado 

data_africa %>%
  filter(year == 2017) %>%
  group_by(recipient) %>%
  summarise(total_amount = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(percentual_num = total_amount/sum(total_amount)*100) %>%
  arrange(desc(percentual_num)) %>%
  mutate(total_amount = scales::comma(total_amount, scale = 1e-9, suffix = "B"),
         percentual = paste0(round(percentual_num, 2), "%")) %>%
  select(recipient, total_amount, percentual) %>%
  print(n = Inf)

View(data_africa)

# mapa ########################################################################


africa_sf <- ne_countries(continent = "Africa", returnclass = "sf")

map_data <- africa_sf %>%
  left_join(data_country, by = c("name" = "recipient"))

map_data <- st_make_valid(map_data)

pts <- st_point_on_surface(map_data)
coords <- st_coordinates(pts)
map_data <- map_data %>%
  mutate(lon = coords[,1], lat = coords[,2])

# gráfico 
ne_countries(continent = "Africa", returnclass = "sf") %>%
  st_make_valid() %>%  
  left_join(
    data_africa %>%
      group_by(recipient_iso_3) %>%
      summarise(total = sum(amount, na.rm = TRUE), .groups = "drop"),
    by = c("iso_a3" = "recipient_iso_3")
  ) %>%
  mutate(
    pt  = st_point_on_surface(geometry),
    lon = st_coordinates(pt)[,1],
    lat = st_coordinates(pt)[,2]
  ) %>%
  ggplot() +
  geom_sf(fill = "gray95", color = "gray80") +
  geom_point(
    aes(x = lon, y = lat, size = total),
    shape = 21, fill = "steelblue", color = "white", alpha = 0.7,
    na.rm = TRUE
  ) +
  scale_size_continuous(
    range  = c(2, 20),
    labels = label_number(scale = 1e-9, suffix = "B")
  ) +
  labs(
    size = "Ajuda externa (bilhões)",
    x    = NULL, 
    y    = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid   = element_blank(),
    axis.text    = element_blank(),
    axis.ticks   = element_blank()
  )



#categorias de montante para cores discretas (ex: quartis)

ne_countries(continent = "Africa", returnclass = "sf") %>%
  st_make_valid() %>%
  left_join(
    data_africa %>%
      group_by(recipient_iso_3) %>%
      summarise(total = sum(amount, na.rm = TRUE), .groups = "drop"),
    by = c("iso_a3" = "recipient_iso_3")
  ) %>%
  mutate(
    pt  = st_point_on_surface(geometry),
    lon = st_coordinates(pt)[,1],
    lat = st_coordinates(pt)[,2]
  ) %>%
  filter(!is.na(total)) %>%
  mutate(
    category = cut(
      total,
      breaks = quantile(total, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
      labels = c("Baixo", "Médio", "Alto", "Muito Alto"),
      include.lowest = TRUE
    )
  ) %>%
  ggplot() +
  geom_sf(fill = "gray95", color = "gray80") +
  geom_point(
    aes(x = lon, y = lat, size = total, fill = category),
    shape = 21, color = "white", alpha = 0.8, show.legend = TRUE
  ) +
  scale_fill_brewer(palette = "Set1", name = "Categoria") +
  scale_size_continuous(
    range  = c(2, 20),
    labels = label_number(scale = 1e-9, suffix = "B")
  ) +
  guides(
    size = "none",
    fill = guide_legend(override.aes = list(size = 6))
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid   = element_blank(),
    axis.text    = element_blank(),
    axis.ticks   = element_blank(),
    legend.position = "right",
    legend.title    = element_text(face = "bold"),
    legend.text     = element_text(size = 8),
    legend.key      = element_rect(fill = NA),
    legend.key.size = unit(1, "lines")
  )


View(data_africa)

# porcentagem do top 5 #########################################################

format(sum(data_country$total, na.rm = TRUE), 
       scientific = FALSE, big.mark = ".", decimal.mark = ",")


format(
  sum(data_country$total[data_country$recipient == "Sudan"], na.rm = TRUE),
  scientific = FALSE,
  big.mark = ".",
  decimal.mark = ","
)



# grafico distribuição top e bottom 5 ##########################################

paises_top <- c("Angola", "Guinea", "Egypt", "Ethiopia", "Sudan")

data_paises_top <- data_africa %>%
  filter(recipient %in% paises_top) %>%
  group_by(year, recipient) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop")


grafico_paises_top <- ggplot(data_paises_top, aes(x = year, y = total, color = recipient)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "ano",
    y = "ajuda externa (bilhões)",
    color = "país"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(data_africa$year)) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-9, suffix = "B")) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )



print(grafico_paises_top)

# bottom 5

paises_bottom <- c("Libya", "Somalia", "Sao Tome and Principe", "Seychelles", "Gambia")

data_paises_bottom <- data_africa %>%
  filter(recipient %in% paises_bottom) %>%
  group_by(year, recipient) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop")



grafico_paises_bottom <- ggplot(data_paises_bottom, aes(x = year, y = total, color = recipient)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "ano",
    y = "ajuda externa (milhões)",
    color = "país"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(data_africa$year)) +
  scale_y_continuous(
    labels = scales::comma_format(scale = 1e-6, suffix = "M")
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

print(grafico_paises_bottom)

# agrupando por setor ##########################################################


data_sector <- data_africa %>%
  group_by(sector) %>%
  summarise(total = sum(amount, na.rm = TRUE))


grafico_setor <- ggplot(data_sector, aes(x = reorder(sector, total), y = total)) +
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
  geom_text(aes(label = scales::label_number(scale = 1e-9, accuracy = 0.01, suffix = "B")(total)),
            hjust = -0.1, size = 3) +
  labs(x = "setor",
       y = "total (em bilhões)",
       title = "") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = "B"),
                     expand = expansion(mult = c(0, 0.1))) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 12, face = "bold")
  )


print(grafico_setor)

#####  setor por flow class ##################################################

# recorte temporal 2000-2021

ggplot(
  data_africa %>%
    filter(flow_class == "ODA-like") %>%
    group_by(sector) %>%
    summarise(total = sum(amount, na.rm = TRUE)),
  aes(x = reorder(sector, total), y = total)
) +
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
  geom_text(
    aes(label = scales::label_number(scale = 1e-9, accuracy = 0.01, suffix = "B")(total)),
    hjust = -0.1, size = 3
  ) +
  labs(
    x = "setor",
    y = "total (em bilhões)",
    title = ""
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-9, suffix = "B"),
    expand = expansion(mult = c(0, 0.1))
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 12, face = "bold")
  )

# recorte temporal 2014-2018

ggplot(
  data_africa %>%
    filter(flow_class == "ODA-like", dplyr::between(year, 2014, 2018)) %>%
    group_by(sector) %>%
    summarise(total = sum(amount, na.rm = TRUE)),
  aes(x = reorder(sector, total), y = total)
) +
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
  geom_text(
    aes(label = scales::label_number(scale = 1e-9, accuracy = 0.01, suffix = "B")(total)),
    hjust = -0.1, size = 3
  ) +
  labs(
    x = "setor",
    y = "total (em bilhões)",
    title = ""
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-9, suffix = "B"),
    expand = expansion(mult = c(0, 0.1))
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 12, face = "bold")
  )



####### distribuição dos setores mas sem dados de angola ####################################################


data_africa %>%
  filter(tolower(recipient) != "angola") %>%
  group_by(sector) %>%
  summarise(total = sum(amount, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(sector, total), y = total)) +
  geom_col(width = 0.5, fill = "steelblue") +
  geom_text(aes(label = scales::label_number(scale = 1e-9, accuracy = 0.01, suffix = "B")(total)),
            hjust = -0.1, size = 3) +
  labs(x = "setor",
       y = "total (em bilhões)",
       title = "") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = "B"),
                     expand = expansion(mult = c(0, 0.1))) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 12, face = "bold")
  )



###### comparação de porcentagem de invertimento nos principais setores por ano #

setores_selecionados <- c("ENERGY", "TRANSPORT AND STORAGE", "OTHER MULTISECTOR",
                          "INDUSTRY, MINING, CONSTRUCTION", "COMMUNICATIONS")

dados_top_setores <- data_africa %>%
  mutate(sector = ifelse(sector %in% setores_selecionados, as.character(sector), 
                         "OTHER")) %>%
  group_by(year, sector) %>%
  summarise(total = sum(amount, na.rm = TRUE))


grafico_porcentagem_setor <- ggplot(dados_top_setores, aes(fill = sector, y = total, 
                                                           x = as.factor(year))) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "ano",
       y = "porcentagem",
       fill = "setor") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_fill_brewer(palette = "Accent") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


print(grafico_porcentagem_setor)


###### distribuição por país de other multisector ##############################

View(data_africa)

data_africa %>%
  filter(tolower(sector) == tolower("OTHER MULTISECTOR")) %>%
  group_by(recipient) %>%
  summarise(total_amount = sum(as.numeric(amount), na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = reorder(recipient, -total_amount), y = total_amount)) +
  geom_col() +
  geom_text(aes(label = scales::label_number(scale = 1e-9, accuracy = 0.01, suffix = "B")(total_amount)),
            vjust = -0.3, size = 3) +
  labs(x = "país",
       y = "total (bilhões)",
       title = "") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-9, accuracy = 0.01, suffix = "B"),
                     expand = expansion(mult = c(0, 0.08))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# apenas os resultados 
data_africa %>%
  filter(tolower(sector) == "other multisector") %>%
  group_by(recipient) %>%
  summarise(total_amount = sum(as.numeric(amount), na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_amount)) %>%
  print(n = Inf)


###### distribuição por país de energy #########################################

data_africa %>%
  filter(tolower(sector) == tolower("ENERGY")) %>%
  group_by(recipient) %>%
  summarise(total_amount = sum(as.numeric(amount), na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = reorder(recipient, -total_amount), y = total_amount)) +
  geom_col() +
  geom_text(aes(label = scales::label_number(scale = 1e-9, accuracy = 0.01, suffix = "B")(total_amount)),
            vjust = -0.3, size = 3) +
  labs(x = "país",
       y = "total (bilhões)",
       title = "") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-9, accuracy = 0.01, suffix = "B"),
                     expand = expansion(mult = c(0, 0.08))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

###### distribuição por país de transport and storage #########################################

data_africa %>%
  filter(tolower(sector) == tolower("COMMUNICATIONS")) %>%
  group_by(recipient) %>%
  summarise(total_amount = sum(as.numeric(amount), na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = reorder(recipient, -total_amount), y = total_amount)) +
  geom_col() +
  geom_text(aes(label = scales::label_number(scale = 1e-9, accuracy = 0.01, suffix = "B")(total_amount)),
            vjust = -0.3, size = 3) +
  labs(x = "país",
       y = "total (bilhões)",
       title = "") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-9, accuracy = 0.01, suffix = "B"),
                     expand = expansion(mult = c(0, 0.08))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



View(data_africa)


# lista de paises africanos #####################################################

lista_paises_africanos <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", 
                            "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", 
                      "Comoros", "Congo",'Democratic Republic of the Congo', 
                      "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini",
                      "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau",
                      "Cote d'Ivoire", "Kenya", "Lesotho", "Liberia", "Libya", 
                      "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", 
                      "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", 
                      "Sao Tome and Principe","Senegal", "Seychelles", "Sierra Leone", 
                      "Somalia", "South Africa","South Sudan", "Sudan", 
                      "Tanzania", "Togo", "Tunisia", "Uganda",
                      "Zambia", "Zimbabwe")


setdiff(lista_paises_africanos, data_africa$recipient) # eswatini foi o unico que n recebeu ajuda chinesa no período

# criando objetivo correlacionando os países às regiões ########################

tabela_regioes <- tibble(
  recipient = lista_paises_africanos,
  region = case_when(
    recipient %in% c("Algeria", "Egypt", "Libya","Morocco", "Sudan", "Tunisia") ~ "Northern Africa",
    recipient %in% c("Burundi", "Comoros", "Djibouti", "Eritrea", "Ethiopia",
                     "Kenya", "Madagascar", "Malawi", "Mauritius",
                     "Mozambique", "Rwanda", "Seychelles", "Somalia",
                     "South Sudan", "Tanzania", "Uganda", "Zambia", "Zimbabwe") ~ "Eastern Africa",
    recipient %in% c("Angola", "Cameroon", "Central African Republic", "Chad",
                     "Democratic Republic of the Congo", "Congo",
                     "Equatorial Guinea", "Gabon", "Sao Tome and Principe") ~ "Central Africa",
    recipient %in% c("Botswana", "Eswatini", "Lesotho", "Namibia", "South Africa") ~ "Southern Africa",
    recipient %in% c("Benin","Burkina Faso","Cabo Verde", "Gambia", "Ghana", 
                     "Guinea","Guinea-Bissau", "Cote d'Ivoire", "Liberia", "Mali",
                     "Mauritania", "Niger", "Nigeria","Senegal", "Sierra Leone","Togo") ~ "Western Africa",
  )
)

View(tabela_regioes)

# inserindo os dados a base geral sobre africa #################################


data_africa <- data_africa %>%
  select(-region) %>% # excluindo a coluna "region" que so tinha "africa em todas as linhas
  left_join(tabela_regioes, by = "recipient") # adicionado uma nova coluna "Region" agora com as regiões


write.csv2(data_africa, file = "data_africa.csv") 

### agrupando por regiao #######################################################


data_africa %>%
  group_by(region) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = reorder(region, -total), y = total)) +
  geom_col(width = 0.5, fill = "#2C77B8") +
  scale_y_continuous(labels = label_number(scale = 1e-9, suffix = "B")) +
  labs(
    x = "região",
    y = "ajuda externa (bilhões)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.title  = element_text(face = "bold")
  )


# só o resultado 
data_africa %>%
  group_by(region) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>%
  mutate(total = scales::comma(total, scale = 1e-9, suffix = "B")) %>%
  select(region, total) %>%
  print(n = Inf)


# soma apenas um país ##########################################################
format(
  sum(data_africa$amount[data_africa$recipient == "Namibia"], na.rm = TRUE),
  scientific   = FALSE,
  big.mark     = ".",
  decimal.mark = ","
)

# explorando os NA na variavel dependente ######################################

# contagem de Na por grupo (país, ano, setor)

contagem_na <- data_africa %>%
  group_by(recipient, year, sector) %>%
  summarise(na_count = sum(is.na(amount)))


soma_na_por_grupo <- contagem_na %>%
  group_by(recipient, year) %>%
  summarise(soma_na = sum(na_count)) %>%
  arrange(desc(soma_na))


sum(is.na(data_africa$amount)) # comparando para ver se a soma está correta

sum(soma_na_por_grupo$soma_na) # comparando para ver se a soma está correta

length(data_africa$amount)
sum(is.na(data_africa$amount))
sprintf("%.2f%%", mean(is.na(data_africa$amount)) * 100)

# os que tem NA incluem assistência como doações de produtos e pessoal


# flow type ####################################################################

# apenas as porcentagens 


data_africa %>%
  group_by(flow_type) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(percentual = total / sum(total) * 100) %>%
  arrange(desc(percentual)) %>%
  mutate(
    total = scales::comma(total, scale = 1e-9, suffix = "B"),
    percentual = paste0(round(percentual, 2), "%")
  ) %>%
  select(flow_type, total, percentual) %>%
  print(n = Inf)

# flow_type por país específico

data_africa %>%
  filter(recipient == "Somalia") %>%
  group_by(flow_type) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(percentual = total / sum(total) * 100) %>%
  arrange(desc(percentual)) %>%
  mutate(
    total = scales::comma(total, scale = 1e-9, suffix = "B"),
    percentual = paste0(round(percentual, 2), "%")
  ) %>%
  select(flow_type, total, percentual) %>%
  print(n = Inf)


# flow_class por país específico

data_africa %>%
  filter(recipient == "Somalia") %>%
  group_by(flow_class) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(percentual = total / sum(total) * 100) %>%
  arrange(desc(percentual)) %>%
  mutate(
    total = scales::comma(total, scale = 1e-9, suffix = "B"),
    percentual = paste0(round(percentual, 2), "%")
  ) %>%
  select(flow_class, total, percentual) %>%
  print(n = Inf)


# porcentagem por tipo e classe
data_africa %>%
  group_by(flow_type, flow_class) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(percentual = total / sum(total) * 100) %>%
  arrange(desc(percentual)) %>%
  mutate(
    total = scales::comma(total, scale = 1e-9, suffix = "B"),
    percentual = paste0(round(percentual, 2), "%")
  ) %>%
  select(flow_type, flow_class, total, percentual) %>%
  print(n = Inf)



# flow class ###################################################################

data_africa %>%
  group_by(flow_class) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(percentual = total / sum(total) * 100) %>%
  arrange(desc(percentual)) %>%
  mutate(
    total = scales::comma(total, scale = 1e-9, suffix = "B"),
    percentual = paste0(round(percentual, 2), "%")
  ) %>%
  select(flow_class, total, percentual) %>%
  print(n = Inf)

### flow class por país específico 

data_africa %>%
  filter(recipient == "Namibia") %>%         
  group_by(flow_class) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(percentual = total / sum(total) * 100) %>%
  arrange(desc(percentual)) %>%
  mutate(
    total = scales::comma(total, scale = 1e-9, suffix = "B"),
    percentual = paste0(round(percentual, 2), "%")
  ) %>%
  select(flow_class, total, percentual) %>%
  print(n = Inf)


# #####################################################################
# empilhado com o classe de fluxo


data_africa %>%
  filter(!is.na(flow_class)) %>%
  group_by(year, flow_class) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = as.factor(year), y = total, fill = flow_class)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-9, suffix = "B")) +
  labs(
    x = "ano",
    y = "total (bilhões)",
    fill = "Classe de fluxo"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 8)
  )


# quantidade de oda e oof total ################################################

data_africa %>%
  group_by(flow_class) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    percentual = total / sum(total) * 100,
    total = scales::comma(total, scale = 1e-9, suffix = "B"),
    percentual = paste0(round(percentual, 2), "%")
  ) %>%
  arrange(desc(percentual)) %>%
  select(flow_class, total, percentual) %>%
  print(n = Inf)  

# quantidade de oda e oof por ano ##############################################

data_africa %>%
  filter(!is.na(flow_class)) %>%
  group_by(year, flow_class) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    total_formatado = scales::comma(total, scale = 1e-9, suffix = "B")
  ) %>%
  select(year, flow_class, total_formatado) %>%
  print(n = Inf)


# infrastructure ###############################################################

data_africa %>%
  group_by(infrastructure) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    percentual = total / sum(total) * 100,
    total = scales::comma(total, scale = 1e-9, suffix = "B"),
    percentual = paste0(round(percentual, 2), "%")
  ) %>%
  arrange(desc(percentual)) %>%
  select(infrastructure, total, percentual) %>%
  print(n = Inf)


# covid ########################################################################

data_africa %>%
  group_by(covid) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    percentual = total / sum(total) * 100,
    total = scales::comma(total, scale = 1e-9, suffix = "B"),
    percentual = paste0(round(percentual, 2), "%")
  ) %>%
  arrange(desc(percentual)) %>%
  select(covid, total, percentual) %>%
  print(n = Inf)

# funding agency ###############################################################

data_africa %>%
  group_by(funding_agency) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(percentual = total / sum(total) * 100) %>%
  arrange(desc(percentual)) %>%
  mutate(
    total = scales::comma(total, scale = 1e-9, suffix = "B"),
    percentual = paste0(round(percentual, 2), "%")
  ) %>%
  select(funding_agency, total, percentual) %>%
  print(n = Inf)

# funding agency type ##########################################################

data_africa %>%
  group_by(funding_agency_type) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    percentual = total / sum(total) * 100,
    total = scales::comma(total, scale = 1e-9, suffix = "B"),
    percentual = paste0(round(percentual, 2), "%")
  ) %>%
  arrange(desc(percentual)) %>%
  select(funding_agency_type, total, percentual) %>%
  print(n = Inf)  # imprime todos os resultados

# original currancy ############################################################

data_africa %>%
  group_by(original_currency) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    percentual = total / sum(total) * 100,
    total = scales::comma(total, scale = 1e-9, suffix = "B"),
    percentual = paste0(round(percentual, 2), "%")
  ) %>%
  arrange(desc(percentual)) %>%
  select(original_currency, total, percentual) %>%
  print(n = Inf)  

# intent #######################################################################

data_africa %>%
  group_by(intent) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    percentual = total / sum(total) * 100,
    total = scales::comma(total, scale = 1e-9, suffix = "B"),
    percentual = paste0(round(percentual, 2), "%")
  ) %>%
  arrange(desc(percentual)) %>%
  select(intent, total, percentual) %>%
  print(n = Inf)



# id (quantidade de projetos por ano) ##########################################

View(data_africa)

data_africa %>%
  group_by(year) %>%
  summarise(n_id = sum(!is.na(id)), .groups = "drop") %>%
  print(n = Inf)

# quantidade de acordos por ano #####################################################################


data_africa %>%
  filter(!is.na(id)) %>%
  count(year, name = "n_id") %>%
  ggplot(aes(x = as.factor(year), y = n_id)) +
  geom_col(fill = "#2C77B8", width = 0.7) +
  labs(
    x = "ano",
    y = "quantidade de acordos"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8)
  )

View(data_africa)

# Líbia ########################################################################

# soma por ano líbia 
ggplot(
  data_africa %>%
    filter(recipient == "Libya") %>%
    group_by(year) %>%
    summarise(amount = sum(amount, na.rm = TRUE)) %>%
    tidyr::complete(year = 2000:2021, fill = list(amount = 0)),
  aes(x = factor(year, levels = as.character(2000:2021)), y = amount)
) +
  geom_col() +
  geom_text(
    aes(label = ifelse(amount > 0, scales::comma(amount), "")),
    vjust = -0.3, size = 3
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.08)),
    labels = scales::label_number(scale = 1e-6, suffix = " M", accuracy = 1)
  ) +
  labs(title = "", x = "ano", y = "total por ano (em milhões)") +
  theme_minimal()

# só os dados 
data_africa %>%
  filter(recipient == "Libya") %>%
  group_by(year) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  right_join(data.frame(year = 2000:2021), by = "year") %>%
  arrange(year) %>%
  mutate(total = as.integer(coalesce(total, 0L))) %>%
  rename(ano = year) %>%
  select(ano, total) %>%
  print(n = Inf)

# por setor líbia 
ggplot(
  data_africa %>%
    filter(recipient == "Namibia") %>%
    group_by(sector) %>%
    summarise(total = sum(amount, na.rm = TRUE)) %>%
    ungroup(),
  aes(x = reorder(sector, total), y = total)
) +
  geom_col(width = 0.5, fill = "steelblue") +
  geom_text(
    aes(label = scales::label_number(scale = 1e-6, accuracy = 0.1, suffix = "M")(total)),
    hjust = -0.1, size = 3
  ) +
  labs(x = "setor", y = "total (em milhões)", title = "") +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-6, suffix = "M"),
    expand = expansion(mult = c(0, 0.1))
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 12, face = "bold")
  )


# Namibia ######################################################################


# soma por ano namibia (so os dados)

data_africa %>%
  filter(recipient == "Gabon") %>%
  group_by(year) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  right_join(data.frame(year = 2000:2021), by = "year") %>%
  arrange(year) %>%
  mutate(total = as.integer(coalesce(total, 0L))) %>%
  rename(ano = year) %>%
  select(ano, total) %>%
  print(n = Inf)

# soma por ano namibia (grafico)

ggplot(
  data_africa %>%
    filter(recipient == "Namibia") %>%
    group_by(year) %>%
    summarise(amount = sum(amount, na.rm = TRUE)) %>%
    tidyr::complete(year = 2000:2021, fill = list(amount = 0)),
  aes(x = factor(year, levels = as.character(2000:2021)), y = amount)
) +
  geom_col() +
  geom_text(
    aes(
      label = ifelse(
        amount > 0,
        scales::label_number(scale = 1e-6, suffix = " M", accuracy = 0.1)(amount),
        ""
      )
    ),
    vjust = -0.3, size = 3
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.08)),
    labels = scales::label_number(scale = 1e-6, suffix = " M", accuracy = 1)
  ) +
  labs(title = "", x = "ano", y = "total por ano (em milhões)") +
  theme_minimal()

# por setor namibia 


ggplot(
  data_africa %>%
    filter(recipient == "Namibia") %>%
    group_by(sector) %>%
    summarise(total = sum(amount, na.rm = TRUE)) %>%
    ungroup(),
  aes(x = reorder(sector, total), y = total)
) +
  geom_col(width = 0.5, fill = "steelblue") +
  geom_text(
    aes(label = scales::label_number(scale = 1e-6, accuracy = 0.1, suffix = "M")(total)),
    hjust = -0.1, size = 3
  ) +
  labs(x = "setor", y = "total (em milhões)", title = "") +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-6, suffix = "M"),
    expand = expansion(mult = c(0, 0.1))
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 12, face = "bold")
  )

# distribuição por ano dos setores de namibia 

View(
  data_africa %>%
    filter(recipient == "Namibia") %>%
    group_by(year, sector) %>%
    summarise(amount = sum(amount, na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(
      year = 2000:2021,
      sector = unique(data_africa$sector),
      fill = list(amount = 0)
    ) %>%
    group_by(year) %>%
    mutate(
      total_year = sum(amount),
      prop = ifelse(total_year > 0, amount / total_year, 0)
    ) %>%
    ungroup() %>%
    arrange(year, desc(amount))
)



# Angola ########################################################################

# soma por ano Angola
ggplot(
  data_africa %>%
    filter(recipient == "Angola") %>%
    group_by(year) %>%
    summarise(amount = sum(amount, na.rm = TRUE)) %>%
    tidyr::complete(year = 2000:2021, fill = list(amount = 0)),
  aes(x = factor(year, levels = as.character(2000:2021)), y = amount)
) +
  geom_col() +
  geom_text(
    aes(
      label = ifelse(
        amount > 0,
        scales::label_number(scale = 1e-9, suffix = "B", accuracy = 0.1)(amount),
        ""
      )
    ),
    vjust = -0.3, size = 3
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.08)),
    labels = scales::label_number(scale = 1e-9, suffix = "B", accuracy = 1)
  ) +
  labs(title = "", x = "ano", y = "total por ano (em bilhões)") +
  theme_minimal()

# só os dados

data_africa %>%
  filter(recipient == "Angola") %>%
  group_by(year) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  right_join(data.frame(year = 2000:2021), by = "year") %>%
  arrange(year) %>%
  mutate(total = round(coalesce(total, 0))) %>%   # numeric arredondado (sem overflow)
  rename(ano = year) %>%
  select(ano, total) %>%
  print(n = Inf)


# por setor angola


ggplot(
  data_africa %>%
    filter(recipient == "Angola") %>%
    group_by(sector) %>%
    summarise(total = sum(amount, na.rm = TRUE)) %>%
    ungroup(),
  aes(x = reorder(sector, total), y = total)
) +
  geom_col(width = 0.5, fill = "steelblue") +
  geom_text(
    aes(label = scales::label_number(scale = 1e-9, accuracy = 0.1, suffix = "B")(total)),
    hjust = -0.1, size = 3
  ) +
  labs(x = "setor", y = "total (em bilhões)", title = "") +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-9, suffix = "B"),
    expand = expansion(mult = c(0, 0.1))
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 12, face = "bold")
  )

# distribuição por ano dos setores de angola

View(
  data_africa %>%
    filter(recipient == "Ethiopia") %>%
    group_by(year, sector) %>%
    summarise(amount = sum(amount, na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(
      year = 2000:2021,
      sector = unique(data_africa$sector),
      fill = list(amount = 0)
    ) %>%
    group_by(year) %>%
    mutate(
      total_year = sum(amount),
      prop = ifelse(total_year > 0, amount / total_year, 0)
    ) %>%
    ungroup() %>%
    arrange(year, desc(amount))
)

View(data_africa)

###3## agência receptora 

data_africa %>%
  filter(recipient == "Namibia") %>%
  group_by(direct_receiving_agency) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    percentual = paste0(round(total / sum(total) * 100, 2), "%"),
    total = format(total, scientific = FALSE, big.mark = ",", trim = TRUE)
  ) %>%
  arrange(desc(as.numeric(gsub(",", "", total)))) %>%  
  select(direct_receiving_agency, total, percentual) %>%
  print(n = Inf)

############################ teste correlação ##################################

teste_disp <- read_excel("C:/Users/vi0_1/Documents/Tese/Capítulo 5/quanti/teste_correlacao.xlsx")

View(teste_disp)


#### exportação para a china
### teste de normalidade (não são normais)

shapiro.test(teste_disp$voting)
shapiro.test(teste_disp$aid)
hist(teste_disp$export)
hist(teste_disp$aid)
qqnorm(teste_disp$export); qqline(teste_disp$export)
qqnorm(teste_disp$stability); qqline(teste_disp$stability)

plot(teste_disp$voting, teste_disp$aid)


# teste de correlação de spearman 

# defagagem de 1 ano em export_mineral


teste_disp %>%
  arrange(recipient, year) %>%
  filter(!is.na(export_mineral) & !is.na(aid)) %>%
  group_by(recipient) %>%
  mutate(export_mineral_lag1 = lag(export_mineral)) %>%
  ungroup() %>%
  filter(!is.na(export_mineral_lag1)) %>%
  with(cor.test(export_mineral_lag1, aid, method = "spearman", exact = FALSE))

# teste de correlação de kendall 

# defagagem de 1 ano em export mineral


teste_disp %>%
  arrange(recipient, year) %>%
  filter(!is.na(export_mineral) & !is.na(aid)) %>%
  group_by(recipient) %>%
  mutate(export_mineral_lag1 = lag(export_mineral)) %>%
  ungroup() %>%
  filter(!is.na(export_mineral_lag1)) %>%
  with(cor.test(export_mineral_lag1, aid, method = "kendall", exact = FALSE))


##### estabilidade 


# teste de correlação de spearman 

# defagagem de 1 ano em  estabilidade 


teste_disp %>%
  arrange(recipient, year) %>%
  filter(!is.na(stability) & !is.na(aid)) %>%
  group_by(recipient) %>%
  mutate(stability_lag1 = lag(stability)) %>%
  ungroup() %>%
  filter(!is.na(stability_lag1)) %>%
  with(cor.test(stability_lag1, aid, method = "spearman", exact = FALSE))

# teste de correlação de kendall 

# defagagem de 1 ano em estabilidade  


teste_disp %>%
  arrange(recipient, year) %>%
  filter(!is.na(stability) & !is.na(aid)) %>%
  group_by(recipient) %>%
  mutate(stability_lag1 = lag(stability)) %>%
  ungroup() %>%
  filter(!is.na(stability_lag1)) %>%
  with(cor.test(stability_lag1, aid, method = "kendall", exact = FALSE))

#### gpd produto interno bruto

# teste de correlação de spearman  (defagagem de 1 ano em gdp) 


teste_disp %>%
  arrange(recipient, year) %>%
  filter(!is.na(gdp) & !is.na(aid)) %>%
  group_by(recipient) %>%
  mutate(gdp_lag1 = lag(gdp)) %>%
  ungroup() %>%
  filter(!is.na(gdp_lag1)) %>%
  with(cor.test(gdp_lag1, aid, method = "spearman", exact = FALSE))

# teste de correlação de kendall 

# defagagem de 1 ano em gdp


teste_disp %>%
  arrange(recipient, year) %>%
  filter(!is.na(gdp) & !is.na(aid)) %>%
  group_by(recipient) %>%
  mutate(gdp_lag1 = lag(gdp)) %>%
  ungroup() %>%
  filter(!is.na(gdp_lag1)) %>%
  with(cor.test(gdp_lag1, aid, method = "kendall", exact = FALSE))

#### importações 


# teste de correlação de spearman  (defagagem de 1 ano em  import) 


teste_disp %>%
  arrange(recipient, year) %>%
  filter(!is.na(import) & !is.na(aid)) %>%
  group_by(recipient) %>%
  mutate(import_lag1 = lag(import)) %>%
  ungroup() %>%
  filter(!is.na(import_lag1)) %>%
  with(cor.test(import_lag1, aid, method = "spearman", exact = FALSE))

# teste de correlação de kendall 

# defagagem de 1 ano em import


teste_disp %>%
  arrange(recipient, year) %>%
  filter(!is.na(import) & !is.na(aid)) %>%
  group_by(recipient) %>%
  mutate(import_lag1 = lag(import)) %>%
  ungroup() %>%
  filter(!is.na(import_lag1)) %>%
  with(cor.test(import_lag1, aid, method = "kendall", exact = FALSE))


### exportações totais

# spearman (defasagem de 1 ano em export)

teste_disp %>%
  arrange(recipient, year) %>%
  filter(!is.na(export) & !is.na(aid)) %>%
  group_by(recipient) %>%
  mutate(export_lag1 = lag(export)) %>%
  ungroup() %>%
  filter(!is.na(export_lag1)) %>%
  with(cor.test(export_lag1, aid, method = "spearman", exact = FALSE))

# kendall (defasagem de 1 ano em export)

teste_disp %>%
  arrange(recipient, year) %>%
  filter(!is.na(export) & !is.na(aid)) %>%
  group_by(recipient) %>%
  mutate(export_lag1 = lag(export)) %>%
  ungroup() %>%
  filter(!is.na(export_lag1)) %>%
  with(cor.test(export_lag1, aid, method = "kendall", exact = FALSE))

###### voto na assembleia geral


# kendall (defasagem de 1 ano em  voting)

teste_disp %>%
  arrange(recipient, year) %>%
  filter(!is.na(voting) & !is.na(aid)) %>%
  group_by(recipient) %>%
  mutate(voting_lag1 = lag(voting)) %>%
  ungroup() %>%
  filter(!is.na(voting_lag1)) %>%
  with(cor.test(voting_lag1, aid, method = "kendall", exact = FALSE))


# spearman (defasagem de 1 ano em  voting)

teste_disp %>%
  arrange(recipient, year) %>%
  filter(!is.na(voting) & !is.na(aid)) %>%
  group_by(recipient) %>%
  mutate(voting_lag1 = lag(voting)) %>%
  ungroup() %>%
  filter(!is.na(voting_lag1)) %>%
  with(cor.test(voting_lag1, aid, method = "spearman", exact = FALSE))


### tratamento de dados para a regressao #######################################


######### variavel dependente 

View(data_africa)

### total 
reg_data_total <- data_africa %>%
  select(recipient, recipient_iso_3, year, amount)

reg_data_total <- reg_data_total %>%
  group_by(recipient, recipient_iso_3, year) %>%
  summarise(amount = sum(amount, na.rm = TRUE), .groups = "drop")

View(reg_data_total)

### oof-like

reg_data_oof <- data_africa %>%
  mutate(amount = ifelse(flow_class == "OOF-like", amount, 0)) %>%
  group_by(recipient, recipient_iso_3, year) %>%
  summarise(amount = sum(amount, na.rm = TRUE), .groups = "drop")

View(reg_data_oof) 

#### oda-like 

reg_data_oda <- data_africa %>%
  mutate(amount = ifelse(flow_class == "ODA-like", amount, 0)) %>%
  group_by(recipient, recipient_iso_3, year) %>%
  summarise(amount = sum(amount, na.rm = TRUE), .groups = "drop")


View(reg_data_oda)

### loan 

reg_data_loan <- data_africa %>%
  mutate(amount = ifelse(flow_type == "Loan", amount, 0)) %>%
  group_by(recipient, recipient_iso_3, year) %>%
  summarise(amount = sum(amount, na.rm = TRUE), .groups = "drop")

View(reg_data_loan)

### grant 

reg_data_grant <- data_africa %>%
  mutate(amount = ifelse(flow_type == "Grant", amount, 0)) %>%
  group_by(recipient, recipient_iso_3, year) %>%
  summarise(amount = sum(amount, na.rm = TRUE), .groups = "drop")

View(reg_data_grant)

# variáveis independentes 


independent <- read_excel("C:/Users/vi0_1/Documents/Tese/Capítulo 5/quanti/mineral_export.xlsx")

independent <- independent %>%
  left_join(
    read_excel(
      "C:/Users/vi0_1/Documents/Tese/Capítulo 5/quanti/abu_haltam.xlsx",
      sheet = 3
    ) %>%
      select(
        Year,
        `Country Code`,
        voting_similarity_o,
        voting_similarity_sc,
        political_similarity,
        importinthousands,
        exportinthousands,
        politicalstability,
        v2x_polyarchy
      ),
    by = c(
      "year" = "Year",
      "iso"  = "Country Code"
    )
  )

independent %>%
  count(iso, year) %>%
  filter(n > 1)

independent <- independent %>%
  left_join(
    read.csv(
      "C:/Users/vi0_1/Documents/Tese/Capítulo 5/quanti/producao_petroleo_1999.csv",
      sep = ";",
      stringsAsFactors = FALSE
    ) %>%
      select(country_iso3_code, quantity),
    by = c("iso" = "country_iso3_code")
  ) %>%
  mutate(
    oil_production = if_else(
      !is.na(quantity) & quantity > 0,
      1L,
      0L
    )
  ) %>%
  select(-quantity)




debt_gdp <- read_excel(
  "C:/Users/vi0_1/Documents/Tese/Capítulo 5/quanti/debt_gpd.xls"
) %>%
  rename(
    recipient = `Central Government Debt (Percent of GDP)`
  ) %>%
  filter(!is.na(recipient)) %>%
  pivot_longer(
    cols = -recipient,
    names_to = "year",
    values_to = "debt_gdp"
  ) %>%
  mutate(
    year = as.integer(year),
    debt_gdp = as.numeric(gsub(",", ".", debt_gdp)),
    recipient = case_when(
      recipient == "Congo, Republic of" ~ "Republic of the Congo",
      recipient == "Gambia, The" ~ "The Gambia",
      recipient == "Côte d'Ivoire" ~ "Ivory Coast",
      recipient == "São Tomé and Príncipe" ~ "Sao Tomé and Principe",
      recipient == "South Sudan, Republic of" ~ "South Sudan",
      TRUE ~ recipient
    )
  ) %>%
  filter(year >= 1999, year <= 2020) %>%   
  bind_rows(
    expand.grid(
      recipient = c(
        "DRC",
        "Ethiopia",
        "Mauritius",
        "Somalia",
        "Tanzania"
      ),
      year = 1999:2020                    
    ) %>%
      as_tibble() %>%
      mutate(debt_gdp = NA_real_)
  )

View(debt_gdp)


setdiff(
  unique(independent$recipient),
  unique(debt_gdp_long$recipient)
)


View(debt_gdp)

independent <- merge(
  independent,
  debt_gdp[, c("recipient", "year", "debt_gdp")],
  by = c("recipient", "year"),
  all.x = TRUE
)


View(independent)

gdp_percapita <- read_excel(
  "C:/Users/vi0_1/Documents/Tese/Capítulo 5/quanti/gdp_percapita.xls",
  skip = 3,        # pula até a linha 4, onde começam os dados
  col_names = TRUE
) %>%
  select(`Country Code`, everything()[5:ncol(.)]) %>%  # pega Country Code + colunas a partir da coluna E
  rename(country_code = `Country Code`) %>%
  pivot_longer(
    cols = -country_code,
    names_to = "year",
    values_to = "gdp_percapita"
  ) %>%
  mutate(
    year = as.integer(year),
    gdp_percapita = as.numeric(gsub(",", ".", gdp_percapita))
  )

View(gdp_percapita)

independent <- independent %>%
  left_join(
    gdp_percapita,
    by = c("iso" = "country_code", "year" = "year")
  )

View(independent)


population <- read_excel(
  "C:/Users/vi0_1/Documents/Tese/Capítulo 5/quanti/population.xls",
  skip = 3,        # pula até a linha 4, onde começam os dados
  col_names = TRUE
) %>%
  select(`Country Code`, everything()[5:ncol(.)]) %>%  
  rename(country_code = `Country Code`) %>%
  pivot_longer(
    cols = -country_code,
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(
    year = as.integer(year),
    population = as.numeric(gsub(",", ".", population))
  )


View(population)

independent <- independent %>%
  left_join(
    population,
    by = c("iso" = "country_code", "year" = "year")
  )

View(independent)

hist(log(reg_data_total$amount), breaks = 50, freq = FALSE)
lines(density(log(reg_data_total$amount), na.rm = TRUE))

independent$iso[independent$iso == "BEM"] <- "BEN"
independent$iso[independent$iso == "SEM"] <- "SEN"
independent$iso[independent$iso == "Lesotho"] <- "LSO"

setdiff(
  unique(independent$iso),
  unique(reg_data_total$recipient_iso_3)
)

setdiff(
  unique(reg_data_total$recipient_iso_3),
  unique(independent$iso)
)


independent <- independent %>%
  distinct(iso, year, .keep_all = TRUE)

######################################################### especificação do modelo
######################################################### modelo com a dep total 


dados_total <- independent %>%
  mutate(year = year + 1) %>% # X(t-1) re-rotula o ano da variável X
  select(-recipient) %>%
  inner_join(
    reg_data_total,
    by = c("iso" = "recipient_iso_3", "year" = "year")
  )


View(dados_total)

# pooled 

modelo_total <- plm(
  log1p(amount) ~ 
    log1p(mineral_export) +
    log1p(exportinthousands) +
    log1p(importinthousands) +
    oil_production +
    debt_gdp +
    voting_similarity_o +
    voting_similarity_sc +
    political_similarity +
    politicalstability +
    v2x_polyarchy +
    log(gdp_percapita) +
    log(population),
  data = dados_total,
  subset = gdp_percapita > 0 &
    population > 0
)


summary(modelo_total)


# efeitos fixos (ano-país)

modelo_total_fe <- plm(
  log1p(amount) ~ 
    log1p(mineral_export) +
    log1p(exportinthousands) +
    log1p(importinthousands) +
    oil_production +
    debt_gdp +
    voting_similarity_o +
    voting_similarity_sc +
    political_similarity +
    politicalstability +
    v2x_polyarchy +
    log(gdp_percapita) +
    log(population),
  data   = dados_total,
  index  = c("iso", "year"),
  model  = "within",
  effect = "twoways",
  subset = gdp_percapita > 0 &
    population > 0
)

summary(modelo_total_fe)


# efeitos aleatórios

modelo_total_re <- plm(
  log1p(amount) ~ 
    log1p(mineral_export) +
    log1p(exportinthousands) +
    log1p(importinthousands) +
    oil_production +
    debt_gdp +
    voting_similarity_o +
    voting_similarity_sc +
    political_similarity +
    politicalstability +
    v2x_polyarchy +
    log(gdp_percapita) +
    log(population),
  data   = dados_total,
  index  = c("iso", "year"),
  model  = "random",
  effect = "twoways",
  random.method = "walhus",
  subset = gdp_percapita > 0 &
    population > 0
)

summary(modelo_total_re)

########################################################### teste de especificação

pFtest(modelo_total_fe, modelo_total) #testa qual o melhor (pooled vs FE)
plmtest(modelo_total, type = "bp") # testa qual o melhor (pooled vs RE)
phtest(modelo_total_fe, modelo_total_re)

################################################################################
################################################################### pressupostos 

ggplot(data.frame(
  fitted = fitted(modelo_total_fe),
  resid = resid(modelo_total_fe)
), aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red") +
  theme_minimal()


# correlação serial dos residuos 
pbgtest(modelo_total_fe) # não há correlação serial dos resíduos

# não-estacionariedade

dados_tmp <- within(dados_total, {
  
  log_amount <- log1p(amount)
  log_mineral_export <- log1p(mineral_export)
  log_export <- log1p(exportinthousands)
  log_import <- log1p(importinthousands)
  
  log_gdp_pc <- log(gdp_percapita)
  log_pop <- log(population)
  
}) #novo objeto para nao modificar o painel 

vars <- c(
  "log_amount",
  "log_mineral_export",
  "log_export",
  "log_import",
  "oil_production",
  "debt_gdp",
  "voting_similarity_o",
  "voting_similarity_sc",
  "political_similarity",
  "politicalstability",
  "v2x_polyarchy",
  "log_gdp_pc",
  "log_pop"
)

library(tseries)

resultados_adf <- setNames(
  lapply(vars, function(v) {
    
    by(dados_tmp[[v]], dados_tmp$iso, function(x) {
      if (sum(!is.na(x)) > 3) {
        tseries::adf.test(na.omit(x))
      } else {
        NA
      }
    })
    
  }),
  vars
)

sapply(resultados_adf$log_export, function(x)
  if (is.list(x)) x$p.value else NA) # só mudar o rotulo da variável

purtest((amount), data = dados_total, test = "levinlin")
purtest((amount), data = dados_total, test = "ips")




########### heterocedasticidade

bptest(modelo_total_fe) # é heterocedastico

########### dependencia cross sectional 
pcdtest(modelo_total, test = "cd") #dependência cross sectional

############ normalidade dos residuos
shapiro.test(modelo_total_fe$residuals)


############ homocedasticidade 
library(lmtest)
bptest(modelo_total_fe)

pFtest(modelo_total_fe, modelo_total)
phtest(modelo_total_fe, modelo_total_re)
plmtest(modelo_total, type= 'bp', effect = 'individual') # teste de Breusch-Pagan + efeitos individuais
plmtest(modelo_total, type= 'bp', effect = 'twoways')
pwartest(modelo_total_fe)
pcdtest(modelo_total_fe, test = 'cd')



coeftest(modelo_total_fe,
         vcov = vcovDC) # anterior

coeftest(modelo_total_fe,
         vcov = vcovSCC(modelo_total_fe)) # esse é o usado no quatro da tese
summary(modelo_total_fe)


############################################################# modelo com dep oof 

dados_oof <- independent %>%
  mutate(year = year + 1) %>%   # X(t-1) re-rotula o ano da variável X
  inner_join(
    reg_data_oof,
    by = c("iso" = "recipient_iso_3", "year" = "year")
  )

View(reg_data_oof)
# reg

modelo_oof_fe <- plm(
  log1p(amount) ~ 
    log1p(mineral_export) +
    log1p(exportinthousands) +
    log1p(importinthousands) +
    oil_production +
    debt_gdp +
    voting_similarity_o +
    voting_similarity_sc +
    political_similarity +
    politicalstability +
    v2x_polyarchy +
    log(gdp_percapita) +
    log(population),
  data   = dados_oof,
  index  = c("iso", "year"),
  model  = "within",
  effect = "twoways",
  subset = gdp_percapita > 0 &
    population > 0
)

coeftest(modelo_oof_fe,
         vcov = vcovDC)

coeftest(modelo_oof_fe,
         vcov = vcovSCC(modelo_oof_fe)) #usado na tese


############################################################# modelo com dep oda  

dados_oda <- independent %>%
  mutate(year = year + 1) %>%   # X(t-1) re-rotula o ano da variável X
  inner_join(
    reg_data_oda,
    by = c("iso" = "recipient_iso_3", "year" = "year")
  )


View(dados_oda)

# reg

modelo_oda_fe <- plm(
  log1p(amount) ~ 
    log1p(mineral_export) +
    log1p(exportinthousands) +
    log1p(importinthousands) +
    oil_production +
    debt_gdp +
    voting_similarity_o +
    voting_similarity_sc +
    political_similarity +
    politicalstability +
    v2x_polyarchy +
    log(gdp_percapita) +
    log(population),
  data   = dados_oda,
  index  = c("iso", "year"),
  model  = "within",
  effect = "twoways",
  subset = gdp_percapita > 0 &
    population > 0
)

coeftest(modelo_oda_fe,
         vcov = vcovDC)

coeftest(modelo_oda_fe,
         vcov = vcovSCC(modelo_oda_fe)) #usado na tese



############################################################ modelo com dep loan  

dados_loan <- independent %>%
  mutate(year = year + 1) %>%   # X(t-1) re-rotula o ano da variável X
  inner_join(
    reg_data_loan,
    by = c("iso" = "recipient_iso_3", "year" = "year")
  )


View(dados_loan)

# reg

modelo_loan_fe <- plm(
  log1p(amount) ~ 
    log1p(mineral_export) +
    log1p(exportinthousands) +
    log1p(importinthousands) +
    oil_production +
    debt_gdp +
    voting_similarity_o +
    voting_similarity_sc +
    political_similarity +
    politicalstability +
    v2x_polyarchy +
    log(gdp_percapita) +
    log(population),
  data   = dados_loan,
  index  = c("iso", "year"),
  model  = "within",
  effect = "twoways",
  subset = gdp_percapita > 0 &
    population > 0
)

coeftest(modelo_loan_fe,
         vcov = vcovDC)

coeftest(modelo_loan_fe,
         vcov = vcovSCC(modelo_loan_fe)) #usado na tese


########################################################### modelo com dep grant  

dados_grant <- independent %>%
  mutate(year = year + 1) %>%   # X(t-1) re-rotula o ano da variável X
  inner_join(
    reg_data_grant,
    by = c("iso" = "recipient_iso_3", "year" = "year")
  )


View(dados_grant)

# reg

modelo_grant_fe <- plm(
  log1p(amount) ~ 
    log1p(mineral_export) +
    log1p(exportinthousands) +
    log1p(importinthousands) +
    oil_production +
    debt_gdp +
    voting_similarity_o +
    voting_similarity_sc +
    political_similarity +
    politicalstability +
    v2x_polyarchy +
    log(gdp_percapita) +
    log(population),
  data   = dados_grant,
  index  = c("iso", "year"),
  model  = "within",
  effect = "twoways",
  subset = gdp_percapita > 0 &
    population > 0
)

coeftest(modelo_grant_fe,
         vcov = vcovDC)

coeftest(modelo_grant_fe,
         vcov = vcovSCC(modelo_grant_fe)) #usado na tese

