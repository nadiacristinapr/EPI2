################################################################################
# ESSE SCRIPT SE REFERE A INSTALAÇÃO DE 3 PACOTES PARA RECUPERAÇÃO E 
# PROCESSAMENTO DE DADOS DOS SIS
#. 1 - microdatasus
#. 2 - datasusr
#. 3 - healthbR
################################################################################
# INSTALANDO O PACOTE microdatasus
################################################################################
install.packages("remotes")
install.packages("tidyverse")
install.packages("data.table")
install.packages("devtools")
library(remotes)
library(tidyverse)
library(data.table)
library(devtools)
# Instalando o pacote read.dbc
devtools::install_github("danicat/read.dbc", force = T)
# Instalando o pacote microdatasus
remotes::install_github("rfsaldanha/microdatasus", force = TRUE)
library(microdatasus)
# Baixando os dados dengue
df_dengue_2021 = fetch_datasus(year_start = 2022, 
                                 year_end = 2022, 
                                 information_system = "SINAN-DENGUE")
# Processando os dados dengue
df_process_dengue_2022 = process_sinan_dengue(df_dengue_2022)
# Salvando os dados baixados em arquivo formato CVS
data.table::fwrite(df_process_dengue_2021, "df_dengue_21.csv", 
                   row.names = FALSE)
# Baixando os dados de dengue de vários anos 2018-2024
df_dengue_18_14 = fetch_datasus(year_start = 2018, 
                                year_end = 2024, 
                                information_system = "SINAN-DENGUE")
# Processando os dados de dengue de vários anos 2018-2024
df_process_dengue_18_24 = process_sinan_dengue(df_dengue_18_24)
# Salvando os dados baixados:
\data.table::fwrite(df_process_dengue_18_24, "df_dengue_18_24.csv", 
                   row.names = FALSE)
################################################################################
# INSTALANDO O PACOTE datasusr
################################################################################
install.packages("remotes")
library(remotes)
remotes::install_github("StrategicProjects/datasusr")
library(datasusr)
# dados de dengue 2024
datasus_file_types(source = "SINAN")
files <- datasus_list_files(
  source    = "SINAN",
  file_type = "DENG",
  year      = 2022,
  month     = 1:12,
)
downloads <- datasus_download(files, use_cache = TRUE)
downloads
DENG2022 <- read_datasus_dbc(downloads$local_file[[1]])
names(DENG2022)
################################################################################
# INSTALANDO O PACOTE healthbR
################################################################################
install.packages("pak")
library(pak)
pak::pak("SidneyBissoli/healthbR")
library(healthbR)
list_sources()
# TESTANDO a RECUPERACAO DE DADOS MORTALIDADE
# dados de mortalidade do Acre, 2022
obitos <- sim_data(year = 2022, uf = "AC")
# TESTANDO a RECUPERACAO DE DADOS SINAN
# dados dengue de 2022
dengue <- sinan_data(year = c("2022", "2023"), disease = "DENG")
dengue <- sinan_data(year = 2022,, disease = "DENG")

names(dengue)
# dados tuberculose de 2022
TUBE2024 <- sinan_data(year = 2024, disease = "TUBE")
names(TUBE2024)
# OBSERVANDO VARIAVEIS SINAN
sinan_variables()
sinan_dictionary()
