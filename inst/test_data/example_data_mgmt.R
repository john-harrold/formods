library(rio)
library(tidyverse)
library(zoo)
library(assertr)
library(janitor)

d_ex <- rio::import("SDTM_EX.csv")
d_pc <- rio::import("SDTM_PC.csv")
d_dm <- rio::import("SDTM_DM.csv")

# Limit to just the helpful columns (so that there are fewer columns that are
# not needed in the dataset)
d_ex_prep <-
  d_ex |>
  mutate(
    # Correct source data
    EXTRT = "UNPRONOUNCAMAB",
    # Assume that dosing happened at 1pm
    EXSTDTC = lubridate::as_datetime(EXSTDTC) + hms::parse_hm("13:00")
  ) |>
  select(STUDYID, DOMAIN, USUBJID, ATEST = EXTRT, EXDOSE, EXDOSU, EXROUTE = EXDOSFRM, ADTC = EXSTDTC) |>
  mutate(
    AREFDTC = ADTC,
    ATPTREF = "FIRST DOSE",
    EVID = 0,
    AMT = EXDOSE,
    # Because it's IV dosing
    CMT = "central"
  )

d_pc_prep <-
  d_pc |>
  select(STUDYID, DOMAIN, USUBJID, ATEST = PCTEST, PCSTRESC, PCSTRESN, PCLLOQ, PCSTRESU, PCSPEC, ADTC = PCDTC) |>
  mutate(
    AVAL =
      case_when(
        !is.na(PCSTRESN) ~ PCSTRESN,
        PCSTRESC %in% "<BLQ" ~ 0,
        TRUE ~ PCSTRESN
      ),
    AVALU = PCSTRESU,
    EVID = 0,
    AMT = 0,
    CMT = "central"
  ) |>
  verify(!is.na(AVAL))

d_dm_prep <-
  d_dm |>
  select(STUDYID, USUBJID, AGE, AGEU, SEX, RACE, ETHNIC, ARM) |>
  # Ensure one row per participant
  verify(!duplicated(USUBJID))

# Combine all the data together

d_final <-
  bind_rows(d_ex_prep, d_pc_prep) |>
  left_join(d_dm_prep) |>
  arrange(USUBJID, ADTC) |>
  group_by(STUDYID, USUBJID) |>
  mutate(
    EXDOSE = janitor::single_value(EXDOSE),
    EXDOSU = janitor::single_value(EXDOSU),
    ATPTREF = janitor::single_value(ATPTREF),
    # Use LOCF for the time of the most recent dose
    AREFDTC = zoo::na.locf(AREFDTC, na.rm = FALSE)
  ) |>
  ungroup() |>
  mutate(
    ID = USUBJID,
    DV = AVAL,
    TIME = as.numeric(ADTC - AREFDTC, units = "days")
  )
