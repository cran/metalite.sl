## ----message=FALSE------------------------------------------------------------
library(r2rtf)
library(metalite)
library(metalite.ae)
library(metalite.sl)

## -----------------------------------------------------------------------------
adsl <- r2rtf::r2rtf_adsl
adsl$TRTA <- adsl$TRT01A
adsl$TRTA <- factor(adsl$TRTA,
  levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
  labels = c("Placebo", "Low Dose", "High Dose")
)

meta <- meta_adam(
  population = adsl,
  observation = adsl
) |>
  define_plan(plan = plan(
    analysis = "base_char", population = "apat",
    observation = "apat", parameter = "age;gender;race"
  )) |>
  define_population(
    name = "apat",
    group = "TRTA",
    subset = quote(SAFFL == "Y"),
    var = c("USUBJID", "TRTA", "SAFFL", "AGEGR1", "SEX", "RACE")
  ) |>
  define_observation(
    name = "apat",
    group = "TRTA",
    subset = quote(SAFFL == "Y"),
    var = c("USUBJID", "TRTA", "SAFFL", "AGEGR1", "SEX", "RACE")
  ) |>
  define_parameter(
    name = "age",
    var = "AGE",
    label = "Age (years)",
    vargroup = "AGEGR1"
  ) |>
  define_parameter(
    name = "gender",
    var = "SEX",
    label = "Gender"
  ) |>
  define_parameter(
    name = "race",
    var = "RACE",
    label = "Race"
  ) |>
  define_analysis(
    name = "base_char",
    title = "Participant Baseline Characteristics by Treatment Group",
    label = "baseline characteristic table"
  ) |>
  meta_build()

## -----------------------------------------------------------------------------
meta_ae <- meta_ae_example()

## ----eval = TRUE--------------------------------------------------------------
react_base_char(
  metadata_sl = meta,
  metadata_ae = meta_ae,
  ae_subgroup = c("age", "race", "gender"),
  ae_specific = "rel",
  width = 1200
)

## ----eval=TRUE----------------------------------------------------------------
react_base_char(
  metadata_sl = meta,
  metadata_ae = meta_ae,
  ae_subgroup = c("age", "race", "gender"),
  ae_specific = "ser",
  width = 1200
)

