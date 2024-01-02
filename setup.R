library(fpp3)
library(knitr)
library(kableExtra)
library(patchwork)

# Set some defaults
options(digits = 3, width = 88)

# Colours to be viridis for continuous scales and Okabe for discrete scales
options(
  ggplot2.continuous.colour="viridis",
  ggplot2.continuous.fill = "viridis",
  ggplot2.discrete.colour = c("#D55E00", "#0072B2","#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442"),
  ggplot2.discrete.fill = c("#D55E00", "#0072B2","#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442")
)
knitr::opts_chunk$set(
  dev.args = list(bg = grey(0.9), pointsize = 11)
)

# Font for graphics to be Fira Sans
ggplot2::theme_set(
  ggplot2::theme_get() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Fira Sans"),
      plot.background = element_rect(fill = "#fafafa", color = "#fafafa")
    )
)
# Also in base R plots
quartzFonts(
  sans = c("Fira Sans Regular", "Fira Sans Bold", "Fira Sans Italic", "Fira Sans Bold Italic")
)

# Monthly hierarchical tourism data (state/zone/region)
# Read csv file of monthly data
OvernightTrips_Region <- readr::read_csv("tourism/OvernightTrips_2017.csv")[, -(1:3)] |>
  # Replace outlier from Adelaide Hills
  mutate(
    `Adelaide Hills` = case_when(
      `Adelaide Hills` > 80 ~ 10,
      TRUE ~ `Adelaide Hills`
    )
  )
# Convert to tsibble
tourism <- hts::hts(
  ts(OvernightTrips_Region, start = 1998, frequency = 12),
  list(7, c(6, 5, 4, 4, 3, 3, 2), c(2, 2, 1, 4, 4, 1, 3, 1, 3, 6, 7, 3, 4, 3, 2, 3, 3, 4, 2, 3, 1, 1, 1, 2, 2, 3, 4))
) |>
  as_tsibble() |>
  rename(
    state = "Level 1",
    zone = "Level 2",
    region = "Level 3",
    month = index,
    visitors = value
  ) |>
  mutate(
    state = recode(state,
                   A = "NSW",
                   B = "VIC",
                   C = "QLD",
                   D = "SA",
                   E = "WA",
                   F = "TAS",
                   G = "NT"
    ),
    zone = recode(zone,
                  AA = "Metro NSW",
                  AB = "North Coast NSW",
                  AC = "South Coast NSW",
                  AD = "South NSW",
                  AE = "North NSW",
                  AF = "ACT",
                  BA = "Metro VIC",
                  BB = "West Coast VIC",
                  BC = "East Coast VIC",
                  BC = "North East VIC",
                  BD = "North West VIC",
                  CA = "Metro QLD",
                  CB = "Central Coast QLD",
                  CC = "North Coast QLD",
                  CD = "Inland QLD",
                  DA = "Metro SA",
                  DB = "South Coast SA",
                  DC = "Inland SA",
                  DD = "West Coast SA",
                  EA = "West Coast WA",
                  EB = "North WA",
                  EC = "South WA",
                  FA = "South TAS",
                  FB = "North East TAS",
                  FC = "North West TAS",
                  GA = "North Coast NT",
                  GB = "Central NT"
    )
  ) |>
  select(month, everything())


# Monthly hierarchical tourism data (state/zone/region/purpose)
load(here::here("tourism/VN525.RData"))
tourism <- VNdata[,222:NCOL(VNdata)] |>
  hts::gts(characters = list(c(1,1,1),3)) |>
  magrittr::extract2("bts") |>
  as_tsibble() |>
  mutate(
    state = substr(key, 1, 1),
    zone = substr(key, 1, 2),
    region = substr(key, 1, 3),
    purpose = substr(key, 4, 6),
  ) |>
  rename(
    month = index,
    visitors = value
  ) |>
  mutate(
    state = recode(state,
                   A = "NSW",
                   B = "VIC",
                   C = "QLD",
                   D = "SA",
                   E = "WA",
                   F = "TAS",
                   G = "NT"
    ),
    zone = recode(zone,
                  AA = "Metro NSW",
                  AB = "North Coast NSW",
                  AC = "South Coast NSW",
                  AD = "South NSW",
                  AE = "North NSW",
                  AF = "ACT",
                  BA = "Metro VIC",
                  BB = "West Coast VIC",
                  BC = "East Coast VIC",
                  BC = "North East VIC",
                  BD = "North West VIC",
                  CA = "Metro QLD",
                  CB = "Central Coast QLD",
                  CC = "North Coast QLD",
                  CD = "Inland QLD",
                  DA = "Metro SA",
                  DB = "South Coast SA",
                  DC = "Inland SA",
                  DD = "West Coast SA",
                  EA = "West Coast WA",
                  EB = "North WA",
                  EC = "South WA",
                  FA = "South TAS",
                  FB = "North East TAS",
                  FC = "North West TAS",
                  GA = "North Coast NT",
                  GB = "Central NT"
    ),
    region = recode(region,
                    AAA = "Sydney",
                    AAB = "Central Coast",
                    ABA = "Hunter",
                    ABB = "North Coast NSW",
                    ACA = "South Coast",
                    ADA = "Snowy Mountains",
                    ADB = "Capital Country",
                    ADC = "The Murray",
                    ADD = "Riverina",
                    AEA = "Central NSW",
                    AEB = "New England North West",
                    AEC = "Outback NSW",
                    AED = "Blue Mountains",
                    AFA = "Canberra",
                    BAA = "Melbourne",
                    BAB = "Peninsula",
                    BAC = "Geelong",
                    BBA = "Western",
                    BCA = "Lakes",
                    BCB = "Gippsland",
                    BCC = "Phillip Island",
                    BDA = "Central Murray",
                    BDB = "Goulburn",
                    BDC = "High Country",
                    BDD = "Melbourne East",
                    BDE = "Upper Yarra",
                    BDF = "Murray East",
                    BEA = "Mallee",
                    BEB = "Wimmera",
                    BEC = "Western Grampians",
                    BED = "Bendigo Loddon",
                    BEE = "Macedon",
                    BEF = "Spa Country",
                    BEG = "Ballarat",
                    BFA = "Central Highlands",
                    CAA = "Gold Coast",
                    CAB = "Brisbane",
                    CAC = "Sunshine Coast",
                    CBA = "Central Queensland",
                    CBB = "Bundaberg",
                    CBC = "Fraser Coast",
                    CBD = "Mackay",
                    CCA = "Whitsundays",
                    CCB = "Northern",
                    CCC = "Tropical North Queensland",
                    CDA = "Darling Downs",
                    CDB = "Outback",
                    DAA = "Adelaide",
                    DAB = "Barossa",
                    DAC = "Adelaide Hills",
                    DBA = "Limestone Coast",
                    DBB = "Fleurieu Peninsula",
                    DBC = "Kangaroo Island",
                    DCA = "Murraylands",
                    DCB = "Riverland",
                    DCC = "Clare Valley",
                    DCD = "Flinders Range and Outback",
                    DDA = "Eyre Peninsula",
                    DDB = "Yorke Peninsula",
                    EAA = "Australia’s Coral Coast",
                    EAB = "Experience Perth",
                    EAC = "Australia’s SouthWest",
                    EBA = "Australia’s North West",
                    ECA = "Australia’s Golden Outback",
                    FAA = "Hobart and the South",
                    FBA = "East Coast",
                    FBB = "Launceston, Tamar and the North",
                    FCA = "North West",
                    FCB = "Wilderness West",
                    GAA = "Darwin",
                    GAB = "Kakadu Arnhem",
                    GAC = "Katherine Daly",
                    GBA = "Barkly",
                    GBB = "Lasseter",
                    GBC = "Alice Springs",
                    GBD = "MacDonnell"
    ),
    purpose = recode(purpose,
                     Hol = "Holidays",
                     Vis = "Visiting Friends and Relatives",
                     Bus = "Business",
                     Oth = "Other"
    )
  ) |>
  as_tsibble(index = month, key = c(state, zone, region, purpose)) |>
  select(month, state:purpose, visitors)
