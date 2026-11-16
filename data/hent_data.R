# Henter og rydder data til SVF-1051 fra SSBs åpne API (Statistikkbanken).
# Kjør skriptet fra mappen "data" for å oppdatere filene.
# Kilder: SSB tabell 12043, 09000, 12044, 12058 og 07459.
# Brann-tallene i 12058 (KOSTRA) bygger på DSBs rapportering fra brannvesenet.

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(readr)

aar <- "2025"

# --- Hjelpefunksjoner -------------------------------------------------------

# Henter en tabell i langt format med koder. `valg` er en navngitt liste:
# variabelkode = vektor med verdier, eller "*" for alle verdier.
hent_ssb <- function(tabell, valg) {
  query <- lapply(names(valg), function(kode) {
    v <- valg[[kode]]
    if (identical(v, "*")) {
      list(code = kode, selection = list(filter = "all", values = list("*")))
    } else {
      list(code = kode, selection = list(filter = "item", values = as.list(v)))
    }
  })
  body <- list(query = query, response = list(format = "json-stat2"))
  svar <- POST(
    paste0("https://data.ssb.no/api/v0/no/table/", tabell),
    body = toJSON(body, auto_unbox = TRUE),
    content_type_json()
  )
  stop_for_status(svar)
  js <- fromJSON(content(svar, as = "text", encoding = "UTF-8"),
                 simplifyVector = FALSE)

  # json-stat2: verdiene ligger i rekkefølge der siste dimensjon varierer raskest
  dims <- unlist(js$id)
  koder <- lapply(dims, function(d) {
    idx <- unlist(js$dimension[[d]]$category$index)
    names(idx)[order(idx)]
  })
  names(koder) <- dims
  rutenett <- expand.grid(rev(koder), stringsAsFactors = FALSE)[, rev(dims), drop = FALSE]
  rutenett$verdi <- vapply(
    js$value, function(x) if (is.null(x)) NA_real_ else as.numeric(x), numeric(1)
  )
  as_tibble(rutenett)
}

# Slår opp tekst for kodene til en variabel i en tabell
hent_tekster <- function(tabell, variabel) {
  meta <- fromJSON(paste0("https://data.ssb.no/api/v0/no/table/", tabell))
  rad <- which(meta$variables$code == variabel)
  tibble(
    kode = meta$variables$values[[rad]],
    tekst = trimws(gsub(" ", " ", meta$variables$valueTexts[[rad]]))
  )
}

# --- 1. Drepte og hardt skadde i veitrafikken, 1946-2025 --------------------

trafikk_aar <- hent_ssb("12043", list(ContentsCode = "*", Tid = "*")) |>
  left_join(hent_tekster("12043", "ContentsCode"), by = c("ContentsCode" = "kode")) |>
  transmute(
    aar = as.integer(Tid),
    navn = recode(tekst, "Drepte" = "drepte", "Hardt skadde" = "hardt_skadde"),
    verdi
  ) |>
  pivot_wider(names_from = navn, values_from = verdi) |>
  arrange(aar)

write_csv(trafikk_aar, "trafikk_aar.csv")

# --- 2. Drepte og skadde etter alder, kjønn, trafikantgruppe og skadegrad ---

trafikk_grupper <- hent_ssb("09000", list(
  Alder2 = "*",
  Kjonn = c("1", "2"),
  Skadegrad = c("01", "15", "04", "05"),
  VegtrTrafGrp = "*",
  ContentsCode = "SkaddDrept",
  Tid = aar
)) |>
  left_join(hent_tekster("09000", "Alder2"), by = c("Alder2" = "kode")) |>
  rename(alder = tekst) |>
  left_join(hent_tekster("09000", "VegtrTrafGrp"), by = c("VegtrTrafGrp" = "kode")) |>
  rename(trafikantgruppe = tekst) |>
  left_join(hent_tekster("09000", "Skadegrad"), by = c("Skadegrad" = "kode")) |>
  rename(skadegrad = tekst) |>
  transmute(
    aar = as.integer(Tid),
    kjonn = if_else(Kjonn == "1", "Mann", "Kvinne"),
    alder,
    trafikantgruppe = recode(
      trafikantgruppe,
      "Bilførere" = "Bilfører",
      "Bilpassasjerer" = "Bilpassasjer",
      "Førere og passasjerer på lett motorsykkel" = "Lett motorsykkel",
      "Førere og passasjerer på annen motorsykkel" = "Tung motorsykkel",
      "Førere og passasjerer på moped" = "Moped",
      "Førere og passasjerer på sykkel" = "Sykkel",
      "Fotgjengere" = "Fotgjenger",
      "Akende o.l." = "Akende o.l.",
      "Andre trafikanter" = "Andre"
    ),
    skadegrad,
    antall = as.integer(verdi)
  )

write_csv(trafikk_grupper, "trafikk_grupper.csv")

# --- 3. Kommunefil: brann, utrykninger og trafikkulykker --------------------

# Folketall 1. januar; gir også listen over dagens kommuner
folketall <- hent_ssb("07459", list(Region = "*", ContentsCode = "Personer1", Tid = aar)) |>
  filter(nchar(Region) == 4, Region != "9999", verdi > 0) |>
  transmute(kommunenr = Region, innbyggere = as.integer(verdi))

regioner <- hent_tekster("07459", "Region")
kommunenavn <- regioner |> filter(nchar(kode) == 4) |> rename(kommunenr = kode, kommune = tekst)
fylkesnavn <- regioner |> filter(nchar(kode) == 2) |> rename(fylkesnr = kode, fylke = tekst)

brann <- hent_ssb("12058", list(
  KOKkommuneregion0000 = folketall$kommunenr,
  ContentsCode = c(
    "KOSboligbranner0000", "KOSbygningsbrann0000", "KOSutrykningerto0000",
    "KOSutrykntotprin0000", "KOSaarsverkialtp0000", "KOSNDUtotinnb0000"
  ),
  Tid = aar
)) |>
  transmute(
    kommunenr = KOKkommuneregion0000,
    navn = recode(
      ContentsCode,
      KOSboligbranner0000 = "boligbranner",
      KOSbygningsbrann0000 = "bygningsbranner",
      KOSutrykningerto0000 = "utrykninger",
      KOSutrykntotprin0000 = "utrykninger_per_1000",
      KOSaarsverkialtp0000 = "aarsverk_brann_per_1000",
      KOSNDUtotinnb0000 = "utgifter_brann_per_innb"
    ),
    verdi
  ) |>
  pivot_wider(names_from = navn, values_from = verdi)

trafikk_kommune <- hent_ssb("12044", list(
  Region = folketall$kommunenr,
  ContentsCode = c("Ulykker", "PersonerDrept", "PersonerSkadd"),
  Tid = aar
)) |>
  pivot_wider(names_from = ContentsCode, values_from = verdi) |>
  transmute(
    kommunenr = Region,
    trafikkulykker = as.integer(Ulykker),
    drepte_skadde_trafikk = as.integer(PersonerDrept + PersonerSkadd)
  )

kommuner <- folketall |>
  left_join(kommunenavn, by = "kommunenr") |>
  mutate(fylkesnr = substr(kommunenr, 1, 2)) |>
  left_join(fylkesnavn, by = "fylkesnr") |>
  left_join(brann, by = "kommunenr") |>
  left_join(trafikk_kommune, by = "kommunenr") |>
  # Beholder bare det norske navnet der SSB oppgir samiske/kvenske parallellnavn,
  # f.eks. "Oslo - Oslove" -> "Oslo". Noen kommuner har det samiske navnet først.
  mutate(
    across(c(kommune, fylke), \(x) sub(" - .*$", "", x)),
    kommune = recode(
      kommune,
      "Snåase" = "Snåsa", "Raarvihke" = "Røyrvik", "Aarborte" = "Hattfjelldal",
      "Hábmer" = "Hamarøy", "Dielddanuorri" = "Tjeldsund", "Loabák" = "Lavangen",
      "Gáivuotna" = "Kåfjord", "Kárásjohka" = "Karasjok",
      "Guovdageaidnu" = "Kautokeino", "Deatnu" = "Tana", "Unjárga" = "Nesseby"
    )
  ) |>
  select(kommunenr, kommune, fylke, innbyggere, everything(), -fylkesnr) |>
  arrange(kommunenr)

write_csv(kommuner, paste0("kommuner_", aar, ".csv"))
