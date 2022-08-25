source("R/read_raw_data.R")
library(lubridate)

read_raw_extracts("enrolled")
enr <- enrolled %>%
  filter(BAssignment == "B0")
enr <- enr |>
  mutate(Date = as_date(RandomisedLocal))
enr_count <- enr |>
  group_by(Date) |>
  summarise(
    N_total = n(),
    N_antiviral = sum(AAssignment != "A0")
  ) |>
  complete(
    Date = seq.Date(min(Date), max(Date), by = "1 day"),
    fill = list(N_total = 0, N_antiviral = 0)
  ) |>
  mutate(
    N_total = cumsum(N_total),
    N_antiviral = cumsum(N_antiviral)
  )
saveRDS(enr_count, file.path("outputs", "antiviral_enrolment.rds"))
