# Henter og rydder forsvars- og konfliktdata til SVF-1051 (gruppe 2).
# Kjør skriptet fra mappen "data" for å oppdatere filene.
#
# Kilder:
#   SIPRI Military Expenditure Database (1949-2025), Stockholm International
#   Peace Research Institute. https://www.sipri.org/databases/milex
#   Brukes etter SIPRIs "fair use"-regler: ikke-kommersiell undervisning,
#   under 10 % av datasettet, med kildehenvisning.
#
#   UCDP/PRIO Armed Conflict Dataset versjon 26.1, Uppsala Conflict Data
#   Program. https://ucdp.uu.se/downloads/  Lisens: CC BY 4.0.
#   Gleditsch m.fl. (2002), "Armed Conflict 1946-2001: A New Dataset",
#   Journal of Peace Research 39(5).

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(countrycode)

aar <- 2025

sipri_url <- "https://www.sipri.org/sites/default/files/SIPRI-Milex-data-1949-2025_v1.2.xlsx"
ucdp_url  <- "https://ucdp.uu.se/downloads/ucdpprio/ucdp-prio-acd-261-csv.zip"

mappe <- tempfile("forsvar_")
dir.create(mappe)
sipri_fil <- file.path(mappe, "sipri.xlsx")
ucdp_zip  <- file.path(mappe, "ucdp.zip")
download.file(sipri_url, sipri_fil, mode = "wb", quiet = TRUE)
download.file(ucdp_url, ucdp_zip, mode = "wb", quiet = TRUE)
ucdp_csv <- unzip(ucdp_zip, exdir = mappe)
ucdp_csv <- ucdp_csv[grepl("\\.csv$", ucdp_csv)]

# --- Hjelpefunksjoner -------------------------------------------------------

hovedregioner <- c(
  "Africa" = "Afrika", "Americas" = "Amerika", "Asia & Oceania" = "Asia og Oseania",
  "Europe" = "Europa", "Middle East" = "Midtøsten"
)
underregioner <- c(
  "North Africa", "sub-Saharan Africa", "Central America and the Caribbean",
  "North America", "South America", "Oceania", "South Asia", "East Asia",
  "South East Asia", "Central Asia", "Central Europe", "Eastern Europe",
  "Western Europe"
)
# Land som ikke finnes lenger, og EU (som ikke er et land)
ikke_land <- c("Czechoslovakia", "German Democratic Republic", "Yugoslavia",
               "USSR", "Yemen, North", "European Union")

# Leser ett ark fra SIPRI-filen i langt format: land, region, år, verdi.
# I SIPRI betyr "..." at tallet mangler, og "xxx" at landet ikke fantes.
les_sipri <- function(ark) {
  raa <- read_excel(sipri_fil, sheet = ark, col_names = FALSE, col_types = "text",
                    .name_repair = "minimal")
  topp <- which(raa[[1]] == "Country")[1]
  navn <- as.character(unlist(raa[topp, ]))
  data <- raa[-(1:topp), ]
  names(data) <- ifelse(is.na(navn) | navn == "", paste0("tom", seq_along(navn)), navn)
  data <- data |>
    rename(land_en = Country) |>
    filter(!is.na(land_en)) |>
    mutate(
      region = if_else(land_en %in% names(hovedregioner), land_en, NA_character_)
    ) |>
    fill(region) |>
    filter(!land_en %in% c(names(hovedregioner), underregioner, ikke_land))
  aarskolonner <- grep("^(19|20)[0-9]{2}$", names(data), value = TRUE)
  data |>
    select(land_en, region, all_of(aarskolonner)) |>
    pivot_longer(all_of(aarskolonner), names_to = "aar", values_to = "verdi") |>
    mutate(
      aar = as.integer(aar),
      verdi = suppressWarnings(as.numeric(verdi)),
      region = unname(hovedregioner[region])
    )
}

til_iso3 <- function(navn) {
  countrycode(navn, "country.name", "iso3c", custom_match = c("Kosovo" = "XKX"))
}
til_norsk <- function(iso3) {
  countrycode(iso3, "iso3c", "cldr.short.nb",
              custom_match = c("XKX" = "Kosovo", "MMR" = "Myanmar"))
}

nato <- c(
  "Albania", "Belgium", "Bulgaria", "Canada", "Croatia", "Czechia", "Denmark",
  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland",
  "Italy", "Latvia", "Lithuania", "Luxembourg", "Montenegro", "Netherlands",
  "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "Slovakia",
  "Slovenia", "Spain", "Sweden", "Türkiye", "United Kingdom",
  "United States of America"
)

# --- SIPRI ------------------------------------------------------------------

andel_bnp   <- les_sipri("Share of GDP")
mill_usd    <- les_sipri("Current US$")
per_innb    <- les_sipri("Per capita")
andel_stat  <- les_sipri("Share of Govt. spending")
faste_usd   <- les_sipri("Constant (2024) US$")

stopifnot(all(nato %in% andel_bnp$land_en))

# --- UCDP -------------------------------------------------------------------

ucdp <- read_csv(ucdp_csv, show_col_types = FALSE, col_types = cols(.default = "c"))

# 1. Konfliktfil: én rad per konflikt per år
konflikter <- ucdp |>
  transmute(
    aar = as.integer(year),
    konflikt_id = conflict_id,
    sted = location,
    region = recode(
      sub(",.*$", "", region),   # konflikter mellom stater kan ha flere regioner
      "1" = "Europa", "2" = "Midtøsten", "3" = "Asia og Oseania",
      "4" = "Afrika", "5" = "Amerika"
    ),
    type = recode(
      type_of_conflict,
      "1" = "Kolonikrig", "2" = "Mellom stater",
      "3" = "Intern", "4" = "Intern med utenlandsk innblanding"
    ),
    intensitet = recode(
      intensity_level,
      "1" = "Mindre konflikt (25-999 drepte)", "2" = "Krig (minst 1000 drepte)"
    )
  ) |>
  arrange(aar, konflikt_id)

write_csv(konflikter, "konflikter.csv")

# Antall konflikter per land i valgt år. UCDP knytter hver konflikt til
# staten(e) hvis myndigheter er hovedpart (gwno_loc, kan være flere).
konflikter_land <- ucdp |>
  filter(year == as.character(aar)) |>
  separate_longer_delim(gwno_loc, delim = ",") |>
  mutate(iso3 = countrycode(
    as.integer(trimws(gwno_loc)), "gwn", "iso3c",
    custom_match = c("678" = "YEM", "679" = "YEM", "347" = "XKX")
  )) |>
  count(iso3, name = "konflikter")

stopifnot(!any(is.na(konflikter_land$iso3)))

# --- 2. Landfil -------------------------------------------------------------

land <- andel_bnp |>
  filter(aar == !!aar) |>
  transmute(land_en, region, andel_bnp = round(verdi * 100, 2)) |>
  left_join(mill_usd |> filter(aar == !!aar) |>
              transmute(land_en, utgifter_mill_usd = round(verdi)), by = "land_en") |>
  left_join(per_innb |> filter(aar == !!aar) |>
              transmute(land_en, utgifter_per_innb_usd = round(verdi)), by = "land_en") |>
  left_join(andel_stat |> filter(aar == !!aar) |>
              transmute(land_en, andel_statsbudsjett = round(verdi * 100, 2)), by = "land_en") |>
  mutate(
    iso3 = til_iso3(land_en),
    land = til_norsk(iso3),
    nato = if_else(land_en %in% nato, "Ja", "Nei")
  ) |>
  left_join(konflikter_land, by = "iso3") |>
  mutate(konflikter = replace_na(konflikter, 0L)) |>
  select(land, region, nato, utgifter_mill_usd, utgifter_per_innb_usd,
         andel_bnp, andel_statsbudsjett, konflikter) |>
  arrange(land)

stopifnot(!any(is.na(land$land)), !any(duplicated(land$land)))
# Alle land UCDP knytter til en konflikt, skal finnes i landfilen
stopifnot(all(konflikter_land$iso3 %in% til_iso3(andel_bnp$land_en)))

write_csv(land, paste0("land_", aar, ".csv"), na = "")

# --- 3. Norge over tid ------------------------------------------------------

norge <- andel_bnp |>
  filter(land_en == "Norway") |>
  transmute(aar, andel_bnp = round(verdi * 100, 2)) |>
  left_join(faste_usd |> filter(land_en == "Norway") |>
              transmute(aar, utgifter_mrd_usd_2024 = round(verdi / 1000, 2)),
            by = "aar") |>
  arrange(aar)

write_csv(norge, "forsvar_norge_aar.csv", na = "")
