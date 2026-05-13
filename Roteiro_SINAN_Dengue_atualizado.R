# =========================================================
# 📌 1. PREPARAÇÃO DO AMBIENTE
# =========================================================
# Definir encoding para evitar problemas com acentos

options(encoding = "UTF-8")

# Pacotes necessários
library(microdatasus)
library(dplyr)
library(lubridate)
library(gtsummary)

# =========================================================
# 📌 2. IMPORTAÇÃO DOS DADOS (SINAN-DENGUE)
# =========================================================
# Baixa dados de dengue para Acre (2022–2023)

dengue <- fetch_datasus(
  2022, 1, 2023, 12,
  uf = "AC",
  information_system = "SINAN-DENGUE",
  vars = NULL
)

# Visualizar estrutura do banco
str(dengue)

# =========================================================
# 📌 3. RECODIFICAÇÃO DAS VARIÁVEIS
# =========================================================
# Transformação dos códigos em categorias legíveis

dengue_rec <- dengue %>%
  mutate(
    
    # Sexo
    CS_SEXO = case_when(
      CS_SEXO == "M" ~ "Masculino",
      CS_SEXO == "F" ~ "Feminino",
      TRUE ~ "Ignorado"
    ),
    
    # Raça/cor
    CS_RACA = case_when(
      CS_RACA == 1 ~ "Branca",
      CS_RACA == 2 ~ "Preta",
      CS_RACA == 3 ~ "Amarela",
      CS_RACA == 4 ~ "Parda",
      CS_RACA == 5 ~ "Indígena",
      TRUE ~ "Ignorado"
    ),
    
    # Escolaridade
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
    ),
    
    # Evolução do caso
    EVOLUCAO = case_when(
      EVOLUCAO == "1" ~ "Cura",
      EVOLUCAO == "2" ~ "Óbito",
      EVOLUCAO == "3" ~ "Óbito por outra causa",
      EVOLUCAO == "9" ~ "Ignorado",
      TRUE ~ "Ignorado"
    )
  )

# =========================================================
# 📌 4. ORDENAR CATEGORIAS (IMPORTANTE PARA TABELAS)
# =========================================================
# Garante que "Ignorado" fique sempre por último

dengue_rec <- dengue_rec %>%
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
    
    EVOLUCAO = factor(EVOLUCAO,
                      levels = c("Cura",
                                 "Óbito",
                                 "Óbito por outra causa",
                                 "Ignorado"))
  )

# =========================================================
# 📌 5. TABELA DESCRITIVA (ESTILO ARTIGO CIENTÍFICO)
# =========================================================
# Tabela de frequência por EVOLUÇÃO (desfecho)

tabela_dengue <- dengue_rec %>%
  select(CS_SEXO, CS_RACA, CS_ESCOL_N, EVOLUCAO) %>%
  
  tbl_summary(
    by = EVOLUCAO,        # desfecho (colunas)
    percent = "row",      # porcentagem por linha
    missing = "no"        # já tratado como "Ignorado"
  ) %>%
  
  add_n() %>%            # adiciona total (N)
  bold_labels()

# Visualizar tabela final
tabela_dengue