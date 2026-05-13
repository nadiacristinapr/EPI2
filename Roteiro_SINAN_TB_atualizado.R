# =========================================================
# 📌 1. PREPARAÇÃO DO AMBIENTE
# =========================================================

# Define encoding para evitar problemas com acentos
options(encoding = "UTF-8")

# Pacotes necessários
library(dplyr)
library(lubridate)
library(read.dbc)
library(gtsummary)
library(ggplot2)

# =========================================================
# 📌 2. IMPORTAÇÃO DOS DADOS (SINAN TB)
# =========================================================
# Leitura dos arquivos .dbc do SINAN Tuberculose
# Mudar o diretorio para onde estao os bancos

tb21 <- read.dbc("TUBEBR21.dbc")
tb22 <- read.dbc("TUBEBR22.dbc")

# =========================================================
# 📌 3. JUNÇÃO DAS BASES E FILTRO TEMPORAL
# =========================================================
# União dos anos 2021 e 2022
# Criação do ano de notificação (NU_ANO)
# Filtragem apenas dos anos de interesse

tb <- bind_rows(tb21, tb22) %>%
  mutate(NU_ANO = year(DT_NOTIFIC)) %>%
  filter(NU_ANO %in% c(2021, 2022))

# Conferência inicial da estrutura
str(tb)

# =========================================================
# 📌 4. RECODIFICAÇÃO DAS VARIÁVEIS SOCIODEMOGRÁFICAS
# =========================================================
# Transformação de códigos em categorias legíveis

tb_rec <- tb %>%
  mutate(
    CS_SEXO = case_when(
      CS_SEXO == "M" ~ "Masculino",
      CS_SEXO == "F" ~ "Feminino",
      TRUE ~ "Ignorado"
    ),
    
    CS_RACA = case_when(
      CS_RACA == 1 ~ "Branca",
      CS_RACA == 2 ~ "Preta",
      CS_RACA == 3 ~ "Amarela",
      CS_RACA == 4 ~ "Parda",
      CS_RACA == 5 ~ "Indígena",
      TRUE ~ "Ignorado"
    ),
    
    CS_ESCOL_N = case_when(
      CS_ESCOL_N == 1 ~ "1ª a 4ª série incompleta",
      CS_ESCOL_N == 2 ~ "4ª série completa",
      CS_ESCOL_N == 3 ~ "5ª a 8ª incompleta",
      CS_ESCOL_N == 4 ~ "Fundamental completo",
      CS_ESCOL_N == 5 ~ "Médio incompleto",
      CS_ESCOL_N == 6 ~ "Médio completo",
      CS_ESCOL_N == 7 ~ "Superior incompleto",
      CS_ESCOL_N == 8 ~ "Superior completo",
      CS_ESCOL_N == 9 ~ "Ignorado",
      CS_ESCOL_N == 10 ~ "Não se aplica",
      TRUE ~ "Ignorado"
    )
  )

# =========================================================
# 📌 5. RECODIFICAÇÃO DO DESFECHO (SITUAÇÃO DE ENCERRAMENTO)
# =========================================================
# Conversão dos códigos do SINAN TB em categorias clínicas

tb_rec <- tb_rec %>%
  mutate(
    SITUA_ENCE = case_when(
      SITUA_ENCE == 1 ~ "Cura",
      SITUA_ENCE == 2 ~ "Abandono",
      SITUA_ENCE == 3 ~ "Óbito por TB",
      SITUA_ENCE == 4 ~ "Óbito por outras causas",
      SITUA_ENCE == 5 ~ "Transferência",
      SITUA_ENCE == 6 ~ "Mudança de Diagnóstico",
      SITUA_ENCE == 7 ~ "TB-DR",
      SITUA_ENCE == 8 ~ "Mudança de Esquema",
      SITUA_ENCE == 9 ~ "Falência",
      SITUA_ENCE == 10 ~ "Abandono Primário",
      TRUE ~ "Ignorado"
    )
  )

# =========================================================
# 📌 6. DEFINIÇÃO DE ORDENAÇÃO (FACTOR)
# =========================================================
# Garante ordem lógica nas tabelas e gráficos

tb_rec <- tb_rec %>%
  mutate(
    CS_SEXO = factor(CS_SEXO,
                     levels = c("Masculino", "Feminino", "Ignorado")),
    
    CS_RACA = factor(CS_RACA,
                     levels = c("Branca", "Preta", "Amarela",
                                "Parda", "Indígena", "Ignorado")),
    
    CS_ESCOL_N = factor(CS_ESCOL_N,
                        levels = c("1ª a 4ª série incompleta",
                                   "4ª série completa",
                                   "5ª a 8ª incompleta",
                                   "Fundamental completo",
                                   "Médio incompleto",
                                   "Médio completo",
                                   "Superior incompleto",
                                   "Superior completo",
                                   "Não se aplica",
                                   "Ignorado")),
    
    SITUA_ENCE = factor(SITUA_ENCE,
                        levels = c("Cura",
                                   "Abandono",
                                   "Abandono Primário",
                                   "Falência",
                                   "TB-DR",
                                   "Mudança de Esquema",
                                   "Mudança de Diagnóstico",
                                   "Transferência",
                                   "Óbito por TB",
                                   "Óbito por outras causas",
                                   "Ignorado"))
  )

# =========================================================
# 📌 7. TABELA DESCRITIVA COMPLETA (ARTIGO)
# =========================================================
# Distribuição das variáveis por desfecho

tb_rec %>%
  select(CS_SEXO, CS_RACA, CS_ESCOL_N, SITUA_ENCE) %>%
  tbl_summary(
    by = SITUA_ENCE,
    percent = "row",
    missing = "no"
  ) %>%
  add_n() %>%
  bold_labels()

# =========================================================
# 📌 8. AGRUPAMENTO DOS DESFECHOS (SIMPLIFICAÇÃO)
# =========================================================
# Redução em 3 categorias: Cura, Abandono e Óbito

tb_filtrado <- tb_rec %>%
  mutate(
    SITUA_ENCE = case_when(
      SITUA_ENCE == "Cura" ~ "Cura",
      SITUA_ENCE == "Abandono" ~ "Abandono",
      SITUA_ENCE %in% c("Óbito por TB", "Óbito por outras causas") ~ "Óbito",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(SITUA_ENCE))

# Reordenação do fator
tb_filtrado <- tb_filtrado %>%
  mutate(
    SITUA_ENCE = factor(SITUA_ENCE,
                        levels = c("Cura", "Abandono", "Óbito"))
  )

# =========================================================
# 📌 9. TABELA DESCRITIVA SIMPLIFICADA
# =========================================================

tb_filtrado %>%
  select(CS_SEXO, CS_RACA, CS_ESCOL_N, SITUA_ENCE) %>%
  tbl_summary(
    by = SITUA_ENCE,
    percent = "row",
    missing = "no"
  ) %>%
  add_n() %>%
  bold_labels()

# =========================================================
# 📌 10. ANÁLISE TEMPORAL DOS DESFECHOS
# =========================================================
# Cálculo de proporções por ano

tb_resumo <- tb_filtrado %>%
  group_by(NU_ANO, SITUA_ENCE) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(NU_ANO) %>%
  mutate(prop = n / sum(n))

# =========================================================
# 📌 11. GRÁFICO DE TENDÊNCIA
# =========================================================
# Evolução temporal dos desfechos

ggplot(tb_resumo,
       aes(x = NU_ANO,
           y = prop,
           color = SITUA_ENCE,
           group = SITUA_ENCE)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Tendência dos desfechos da Tuberculose",
    x = "Ano",
    y = "Proporção",
    color = "Desfecho"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()