library(ASCOTr)
library(tidyverse)
library(readxl)

ppA <- read_excel(file.path(ASCOT_DATA, "explanation", "ASCOT_ADAPT_ListofPDs.xlsx"))
ppC <- read_excel(file.path(ASCOT_DATA, "explanation", "ASCOT_ADAPT_ListofPDsC.xlsx"))

ppC <- ppC |>
  rename(
    StudyPatientID = `Patient ID`,
    pd_C_reason = Reason) |>
  mutate(pd_C = 1)

ppA <- ppA |>
  select(`Patient ID`, `Type of Deviation`, `Sub-type of Deviation`) |>
  rename(
    StudyPatientID = `Patient ID`,
    pd_A_type = `Type of Deviation`,
    pd_A_subtype = `Sub-type of Deviation`
  ) |>
  mutate(pd_A = 1) |>
  filter(StudyPatientID != "-")

pd <- full_join(ppA, ppC, by = "StudyPatientID")
saveRDS(pd, file.path(ASCOT_DATA, "pd.rds"))
