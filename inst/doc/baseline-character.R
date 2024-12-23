## -----------------------------------------------------------------------------
library(metalite)
library(metalite.sl)

## -----------------------------------------------------------------------------
adsl <- r2rtf::r2rtf_adsl
adsl$TRTA <- adsl$TRT01A
adsl$TRTA <- factor(
  adsl$TRTA,
  levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
  labels = c("Placebo", "Low Dose", "High Dose")
)

## -----------------------------------------------------------------------------
meta <- meta_sl(
  dataset_population = adsl,
  population_term = "apat",
  parameter_term = "age;gender;race",
  parameter_var = "AGE^AGEGR1;SEX;RACE",
  treatment_group = "TRTA"
)

## -----------------------------------------------------------------------------
meta

## -----------------------------------------------------------------------------
outdata <- meta |>
  prepare_base_char(parameter = "age;gender;race")

outdata

## -----------------------------------------------------------------------------
outdata$n
outdata$char_n
outdata$char_var
outdata$char_prop

## -----------------------------------------------------------------------------
outdata <-
  outdata |> format_base_char(
    display_col = c("n", "prop", "total"),
    digits_prop = 2
  )

## -----------------------------------------------------------------------------
outdata$tbl

## -----------------------------------------------------------------------------
outdata |> rtf_base_char(
  source = "Source: [CDISCpilot: adam-adsl]",
  path_outdata = tempfile(fileext = ".Rdata"),
  path_outtable = "outtable/base0char.rtf"
)

## ----out.width = "100%", out.height = "400px", echo = FALSE, fig.align = "center"----
knitr::include_graphics("pdf/base0char.pdf")

## -----------------------------------------------------------------------------
adsl <- r2rtf::r2rtf_adsl
adsl$TRTA <- adsl$TRT01A
adsl$TRTA <- factor(
  adsl$TRTA,
  levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
  labels = c("Placebo", "Low Dose", "High Dose")
)

## -----------------------------------------------------------------------------
plan <- plan(
  analysis = "base_char_subgroup",
  population = "apat",
  observation = "apat",
  parameter = "age;gender;race"
)

## -----------------------------------------------------------------------------
meta <- meta_adam(
  population = adsl,
  observation = adsl
) |>
  define_plan(plan = plan) |>
  define_population(
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
    name = "race",
    var = "RACE",
    label = "Race"
  ) |>
  define_analysis(
    name = "base_char_subgroup",
    title = "Participant by Age Category and Sex",
    label = "baseline characteristic sub group table"
  ) |>
  meta_build()

## -----------------------------------------------------------------------------
meta

## -----------------------------------------------------------------------------
outdata <- prepare_base_char_subgroup(
  meta,
  population = "apat",
  parameter = "age;race",
  subgroup_var = "TRTA",
  subgroup_header = c("SEX", "TRTA"),
  display_subgroup_total = TRUE
)

## -----------------------------------------------------------------------------
outdata$out_all$`Placebo`
outdata$out_all$`High Dose`
outdata$out_all$`Low Dose`

## -----------------------------------------------------------------------------
outdata$group
outdata$subgroup

## -----------------------------------------------------------------------------
outdata$out_all$`Placebo`$n
outdata$out_all$`High Dose`$n
outdata$out_all$`Low Dose`$n

## -----------------------------------------------------------------------------
outdata <- format_base_char_subgroup(outdata)

## -----------------------------------------------------------------------------
outdata$tbl

## ----warning=FALSE------------------------------------------------------------
outdata |>
  rtf_base_char_subgroup(
    source = "Source:  [CDISCpilot: adam-adsl]",
    path_outdata = tempfile(fileext = ".Rdata"),
    path_outtable = "outtable/base0charsubgroup.rtf"
  )

## ----out.width = "100%", out.height = "400px", echo = FALSE, fig.align = "center"----
knitr::include_graphics("pdf/base0charsubgroup.pdf")

