library(ASCOTr)
library(tidyverse)
library(readxl)

ppA <- read_excel(file.path(ASCOT_DATA, "explanation", "ASCOT_ADAPT_ListofPDs.xlsx"))
ppA <- read_excel(file.path(ASCOT_DATA, "explanation", "ASCOT_ADAPT_PerProtocolList.xlsx"))
ppC <- read_excel(file.path(ASCOT_DATA, "explanation", "ASCOT_ADAPT_ListofPDsC.xlsx"))

ppA <- ppA |>
  rename(
    StudyPatientID = StudyID,
    pd_A_text = `Per Protocol?`,
    pd_A_reason = `Reason Why Not`
  ) |>
  mutate(
    pd_A = if_else(pd_A_text == "Not Per Protocol", 1, 0)
  )

ppC <- ppC |>
  rename(
    StudyPatientID = `Patient ID`,
    pd_C_reason = Reason) |>
  mutate(pd_C = 1)

# ppA <- ppA |>
#   select(`Patient ID`, `Type of Deviation`, `Sub-type of Deviation`) |>
#   rename(
#     StudyPatientID = `Patient ID`,
#     pd_A_type = `Type of Deviation`,
#     pd_A_subtype = `Sub-type of Deviation`
#   ) |>
#   mutate(pd_A = 1) |>
#   filter(StudyPatientID != "-")



pd <- full_join(ppA, ppC, by = "StudyPatientID")
saveRDS(pd, file.path(ASCOT_DATA, "pd.rds"))
