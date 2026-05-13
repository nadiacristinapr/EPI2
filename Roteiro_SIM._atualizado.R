options(encoding = "UTF-8")

#=========================================================

# 📌 1. PACOTES

# =========================================================

library(microdatasus)
library(dplyr)
library(lubridate)
library(gtsummary)
library(gt)

# =========================================================

# 📌 2. DOWNLOAD + PROCESSAMENTO (SIM - ÓBITOS)

# =========================================================
sim_raw <- fetch_datasus(
  year_start = 2022, month_start = 1,
  year_end   = 2023, month_end   = 12,
  uf = "AC",
  information_system = "SIM-DO"
)

sim <- process_sim(sim_raw)

# =========================================================

# 📌 3. EXPLORAÇÃO INICIAL

# =========================================================

str(sim)

# =========================================================

# 📌 4. TABELA DESCRITIVA (ARTIGO)

# =========================================================
tabela_artigo <- sim %>%
  select(SEXO, RACACOR, ESTCIV, ESC) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "ifany",
    label = list(
      SEXO ~ "Sexo",
      RACACOR ~ "Raça/cor",
      ESTCIV ~ "Estado civil",
      ESC ~ "Escolaridade"
    )
  ) %>%
  add_overall() %>%
  bold_labels()

tabela_artigo

# =========================================================

# 📌 5. CRIAÇÃO DO ANO DE ÓBITO

# =========================================================

sim <- sim %>%
  mutate(
    ANO_OBITO = year(DTOBITO)
  )

# =========================================================

# 📌 6. ÓBITOS INFANTIS (< 1 ANO)

# =========================================================

obitos_infantis <- sim %>%
  filter(IDADE < 1) %>%
  group_by(ANO_OBITO) %>%
  summarise(obitos = n(), .groups = "drop")

# =========================================================

# 📌 7. DOWNLOAD + PROCESSAMENTO (SINASC)

# =========================================================
sinasc_raw <- fetch_datasus(
  year_start = 2022, month_start = 1,
  year_end   = 2023, month_end   = 12,
  uf = "AC",
  information_system = "SINASC"
)

sinasc <- process_sinasc(sinasc_raw)

# =========================================================

# 📌 8. NASCIDOS VIVOS POR ANO

# =========================================================

nascidos <- sinasc %>%
  mutate(ANO = year(DTNASC)) %>%
  group_by(ANO) %>%
  summarise(nascidos_vivos = n(), .groups = "drop")

# =========================================================

# 📌 9. CÁLCULO DA TMI

# =========================================================

tmi <- obitos_infantis %>%
  rename(ANO = ANO_OBITO) %>%
  left_join(nascidos, by = "ANO") %>%
  mutate(TMI = (obitos / nascidos_vivos) * 1000)

# =========================================================

# 📌 10. TABELA FINAL

# =========================================================

tmi_final <- tmi %>%
  arrange(ANO) %>%
  mutate(TMI = round(TMI, 2))

tmi_final

# =========================================================

# 📌 11. TABELA PUBLICÁVEL (GT)

# =========================================================

tmi_final %>%
  gt() %>%
  tab_header(
    title = "Taxa de Mortalidade Infantil (TMI)",
    subtitle = "Acre, 2022–2023"
  ) %>%
  cols_label(
    ANO = "Ano",
    obitos = "Óbitos infantis",
    nascidos_vivos = "Nascidos vivos",
    TMI = "TMI (por 1.000 nascidos vivos)"
  )

# =========================================================

# 📌 12. GRÁFICO DA TMI

# =========================================================

library(ggplot2)

ggplot(tmi_final, aes(x = ANO, y = TMI)) +
  geom_line(size = 1.2, color = "blue") +
  geom_point(size = 3) +
  labs(
    title = "Tendência da Taxa de Mortalidade Infantil",
    x = "Ano",
    y = "TMI (por 1.000 NV)"
  ) +
  scale_x_continuous(breaks = unique(tmi_final$ANO)) +
  theme_minimal()

# =========================================================

# 📌 13. MAPA

# =========================================================

library(ggplot2)

sim %>%
  count(munResNome) %>%
  ggplot(aes(x = reorder(munResNome, n), y = n)) +
  geom_col() +
  coord_flip() +
  theme_minimal()
