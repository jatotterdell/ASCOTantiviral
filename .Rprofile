readRenviron(".Renviron")
ASCOT_DATA <- Sys.getenv("ASCOT_DATA")
ASCOT_DATA_RAW <- file.path(ASCOT_DATA, "raw")
ANTIVIRAL_DATA <- file.path(ASCOT_DATA)

if (!dir.exists("outputs")) {
  dir.create(file.path("outputs", "tables", "baseline"), recursive = TRUE)
  dir.create(file.path("outputs", "tables", "outcomes"), recursive = TRUE)
  dir.create(file.path("outputs", "tables", "outcomes", "primary", "subgroup"), recursive = TRUE)
  dir.create(file.path("outputs", "tables", "outcomes", "secondary"), recursive = TRUE)
  dir.create(file.path("outputs", "tables", "outcomes", "domain"), recursive = TRUE)
  dir.create(file.path("outputs", "tables", "outcomes", "safety"), recursive = TRUE)
  dir.create(file.path("outputs", "figures", "baseline"), recursive = TRUE)
  dir.create(file.path("outputs", "figures", "followup"), recursive = TRUE)
  dir.create(file.path("outputs", "figures", "outcomes", "primary"), recursive = TRUE)
  dir.create(file.path("outputs", "figures", "outcomes", "secondary"), recursive = TRUE)
  dir.create(file.path("outputs", "figures", "outcomes", "domain"), recursive = TRUE)
  dir.create(file.path("outputs", "models", "primary"), recursive = TRUE)
  dir.create(file.path("outputs", "models", "secondary"), recursive = TRUE)
}
if (!dir.exists("stan")) {
  dir.create("stan")
}
