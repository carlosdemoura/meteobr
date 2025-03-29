library(dplyr)
library(stringi)

correct_str = function(x) {
  x %>%
    toupper() %>%
    stringr::str_trim() %>%
    {gsub("\\s+", " ", .)}
}

siglas1 = c(
  "AC" = "Acre",
  "AL" = "Alagoas",
  "AM" = "Amazonas",
  "AP" = "Amapa",
  "BA" = "Bahia",
  "CE" = "Ceara",
  "DF" = "Distrito Federal",
  "ES" = "Espirito Santo",
  "GO" = "Goias",
  "MA" = "Maranhao",
  "MG" = "Minas Gerais",
  "MS" = "Mato Grosso do Sul",
  "MT" = "Mato Grosso",
  "PA" = "Para",
  "PB" = "Paraiba",
  "PE" = "Pernambuco",
  "PI" = "Piaui",
  "PR" = "Parana",
  "RJ" = "Rio de Janeiro",
  "RN" = "Rio Grande do Norte",
  "RO" = "Rondonia",
  "RR" = "Roraima",
  "RS" = "Rio Grande do Sul",
  "SC" = "Santa Catarina",
  "SE" = "Sergipe",
  "SP" = "Sao Paulo",
  "TO" = "Tocantins"
)

siglas2 <- c(
  "Acre" = "AC",
  "Alagoas" = "AL",
  "Amazonas" = "AM",
  "Amapa" = "AP",
  "Bahia" = "BA",
  "Ceara" = "CE",
  "Distrito Federal" = "DF",
  "Espirito Santo" = "ES",
  "Goias" = "GO",
  "Maranhao" = "MA",
  "Minas Gerais" = "MG",
  "Mato Grosso do Sul" = "MS",
  "Mato Grosso" = "MT",
  "Para" = "PA",
  "Paraiba" = "PB",
  "Pernambuco" = "PE",
  "Piaui" = "PI",
  "Parana" = "PR",
  "Rio de Janeiro" = "RJ",
  "Rio Grande do Norte" = "RN",
  "Rondonia" = "RO",
  "Roraima" = "RR",
  "Rio Grande do Sul" = "RS",
  "Santa Catarina" = "SC",
  "Sergipe" = "SE",
  "Sao Paulo" = "SP",
  "Tocantins" = "TO"
)



ibge =
  "_stations_info_handler/RELATORIO_DTB_BRASIL_MUNICIPIO.xls" %>%
  readxl::read_xls() %>%
  select(c(2, 12, 13)) %>%
  `colnames<-`(c("state", "town.id", "town.name")) %>%
  mutate(
    town.name = stri_trans_general(str = town.name, id = "Latin-ASCII") |> toupper(),
    state = siglas2[stri_trans_general(str = state, id = "Latin-ASCII")]
  )


inmet =
  "_stations_info_handler/CatalogoEstaçõesAutomáticas.csv" %>%
  read.csv(sep = ";") %>%
  tibble::as_tibble() %>%
  `colnames<-`(c("station.name", "state", "x", "lat", "lon", "alt", "foundation", "station.id")) %>%
  select(!x) %>%
  rbind(c("TORRES", "RS", -29.35027777, -49.73333333, 8.44, "01/06/2006", "A808")) %>%
  mutate(
    town.name = station.name %>% correct_str(),
    town.name = if_else(state == "DF", "BRASILIA", station.name),
    across(c(lat, lon, alt), ~ {gsub(",", ".", .x) |> as.numeric()}),
    region = case_when(
      state %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      state %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      state %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      state %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
      state %in% c("PR", "RS", "SC") ~ "Sul"
    )
  )

zzz = merge(inmet, ibge, by = c("town.name", "state"), all.x = T)
openxlsx::write.xlsx(zzz, "_stations_info_handler/v1.xlsx")


zzz2 = openxlsx::read.xlsx("_stations_info_handler/v1.xlsx")

ibge_original =
  "_stations_info_handler/RELATORIO_DTB_BRASIL_MUNICIPIO.xls" %>%
  readxl::read_xls() %>%
  select(c(2, 12, 13)) %>%
  `colnames<-`(c("state", "town.id", "town.name"))


stations =
  inmet %>%
  select(!town.id) %>%
  merge(zzz2[c("town.id", "station.id")], by = "station.id") %>%
  select(!c(state, town.name)) %>%
  merge(ibge_original, by = "town.id") %>%
  as_tibble() %>%
  select(station.id, station.name, lat, lon, alt, foundation, town.id, town.name, state, region) %>%
  arrange(region, state, town.id)


usethis::use_data(stations)



saveRDS(stations, "stations_final.rds")





filter(ibge, town.name == toupper("torres"))


View(zzz)
sum(is.na(zzz$town.id))


x = paste(inmet$municipio, "-", inmet$uf) %in% paste(ibge$municipio, "-", ibge$uf)
mean(x)
inmet[!x,] |>
  {\(.) paste(.$municipio, "-", .$uf)}()

stations = merge(inmet, ibge[c("municipio", "municipio.cod")], by = "municipio") %>%
  rename(
    "station.id"   = "estacao",
    "station.name" = "estacao.nome",
    "town.id"      = "municipio.cod",
    "town.name"    = "municipio",
    "state"        = "uf",
    "region"       = "regiao",
    "lat"          = "latitude",
    "lon"          = "longitude",
    "alt"          = "altitude"
  ) %>%
  .[c(
    "station.id",
    "station.name",
    "town.id",
    "town.name",
    "state",
    "region",
    "lat",
    "lon",
    "alt"
    )] %>%
  tibble::as_tibble()

saveRDS(stations, file = "C:/Users/Carlos/Documents/stations.Rdata")


distritos = readxl::read_xls("C:/Users/Carlos/Documents/RELATORIO_DTB_BRASIL_DISTRITO.xls") %>%
  select(c(1, 2, 12, 13, 15, 16)) %>%
  `colnames<-`(c("uf.cod", "uf", "municipio.cod", "municipio", "distrito.cod", "distrito")) %>%
  mutate(
    municipio = stringi::stri_trans_general(str = municipio, id = "Latin-ASCII") %>%
      correct_str(),
    uf = siglas[uf],
    distrito = distrito %>% correct_str()
  )

filter(distritos, uf == "PB", distrito == "MINA DO PALITO")

