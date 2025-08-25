library(tidyverse)

compute_popistat <- function(pop_istat) {
  pop <- pop_istat %>%
    select("Codice comune", "Comune", "Età", "Totale maschi", "Totale femmine", "Totale") %>%
    rename(
      Cod    = "Codice comune",
      Comune = "Comune",
      Age    = "Età",
      Male   = "Totale maschi",
      Female = "Totale femmine",
      Tot    = "Totale"
    ) %>%
    mutate(
      Age = na_if(Age, 999),
      Age_group = cut(
        Age,
        breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
        labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                   "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
                   "75-79", "80-84", "85+"),
        right = TRUE,
        include.lowest = TRUE
      )
    ) %>%
    drop_na(Age)
  
  pop %>%
    filter(!is.na(Age_group)) %>%
    group_by(Cod, Comune, Age_group) %>%
    summarise(
      N_female = sum(Female, na.rm = TRUE),
      N_male   = sum(Male, na.rm = TRUE),
      Tot_pop  = sum(Tot, na.rm = TRUE),
      .groups  = "drop"
    )
}
# Jan 1 2023 - ref pop 2022
p2023 <- read_delim("data/POSAS_2023_it_Comuni.csv", skip = 1)
p_2023 <- compute_popistat(p2023)
write_csv(p_2023, file = "results/01_dati_comune_pop2023.csv")

# Jan 1 2022 - ref pop 2021
p2022 <- read_delim("data/POSAS_2022_it_Comuni.csv", skip = 1)
p_2022 <- compute_popistat(p2022)
write_csv(p_2022, file = "results/01_dati_comune_pop2022.csv")

# Jan 1 2021 - ref pop 2020
p2021 <- read_delim("data/POSAS_2021_it_Comuni.csv", skip = 1)
p_2021 <- compute_popistat(p2021)
write_csv(p_2021, file = "results/01_dati_comune_pop2021.csv")

# Jan 1 2020 - ref pop 2019
p2020 <- read_delim("data/POSAS_2020_it_Comuni.csv", skip = 1)
p_2020 <- compute_popistat(p2020)
write_csv(p_2020, file = "results/01_dati_comune_pop2020.csv")

# Jan 1 2019 - ref pop 2018
p2019 <- read_csv("results/01_dati_comune_pop_2019.csv")
p_2019 <- rename(p2019, Comune = NomeComune)

# 2. Rinomina colonne con suffisso anno
rename_year_cols <- function(df, year) {
  df %>%
    mutate(Comune = stringr::str_squish(Comune)) %>%  
    # select(-Comune) %>%  
    rename_with(~ paste0(., "_", year), .cols = c("N_female", "N_male", "Tot_pop"))
}
p_2019r <- rename_year_cols(p_2019, 2019)
p_2020r <- rename_year_cols(p_2020, 2020)
p_2021r <- rename_year_cols(p_2021, 2021)
p_2022r <- rename_year_cols(p_2022, 2022)
p_2023r <- rename_year_cols(p_2023, 2023)

# Join with Cod e Age_group
pop_merged <- list(p_2019r, p_2020r, p_2021r, p_2022r, p_2023r) %>%
  map(~ select(.x, - Comune)) %>%
  reduce(full_join, by = c("Cod", "Age_group")) %>%
  distinct()

comuni_01012019_31012023 <- read_delim("data/Comuni_inizio_fine.csv")
comuni_19_23 <- comuni_01012019_31012023 %>%
  select( 
    `Codice Comune-Data inizio`, 
    `Comune-Data inizio`,
    `Codice Comune-Data fine`, 
    `Comune-Data fine`) %>%
  rename(
    Cod_Inizio = "Codice Comune-Data inizio", 
    Comune_Inizio = `Comune-Data inizio`, 
    Cod_Fine = `Codice Comune-Data fine`,
    Comune_Fine = `Comune-Data fine`
  )

inizio_df <- comuni_19_23 %>%
  filter(!is.na(Cod_Inizio)) %>%
  transmute(
    Cod = Cod_Inizio,
    Comune = Comune_Inizio,
    Origine = "Previous"
  ) %>%
  distinct()

fine_df <- comuni_19_23 %>%
  filter(!is.na(Cod_Fine)) %>%
  transmute(
    Cod = Cod_Fine,
    Comune = Comune_Fine,
    Origine = "Latest"
  ) %>%
  distinct()

comuni_unificati <- bind_rows(inizio_df, fine_df)

# Indentify duplicates Cod + Comune
comuni_finale <- comuni_unificati %>%
  group_by(Cod, Comune) %>%
  summarise(
    Origine = if (n() == 2) "Not_Varied" else first(Origine),
    .groups = "drop"
  ) %>%
  distinct()

# Some of them only slightly changed the name
cod_dup <- comuni_finale$Cod[which(duplicated(comuni_finale$Cod))]
# "005014" "005020" "005056" "006045" "021076" "023052"
comuni_finale[comuni_finale$Cod %in% cod_dup,]

#So when Cod is not unique select the Latest name of the Comune
comuni_codunico <- comuni_finale %>%
  arrange(Cod, desc(Origine == "Latest")) %>%  # put "Latest" on top
  group_by(Cod) %>%
  slice(1) %>%  # pick first row ("Latest" if exist)
  ungroup()

pop_merged <- pop_merged %>% left_join(comuni_codunico, by = "Cod") %>%
  relocate(Comune, .after = Cod)
write_csv(pop_merged, "results/01_popAgeSex_2018_2022.csv")

openxlsx::write.xlsx(pop_merged, "results/01_popAgeSex_2018_2022.xlsx")


