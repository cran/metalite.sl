## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  out.width = "100%",
  dpi = 150
)

## ----setup, message=FALSE-----------------------------------------------------
library(metalite)
library(metalite.sl)
library(dplyr)

## ----out.width = "100%", out.height = "400px", echo = FALSE, fig.align = "center"----
knitr::include_graphics("pdf/treatment0compliance.pdf")

## -----------------------------------------------------------------------------
adsl <- r2rtf::r2rtf_adsl

adex <- metalite.ae::metalite_ae_adex

adex1 <- adex |>
  filter(EXNUMDOS > 0 & !(is.na(AENDY))) |>
  group_by(USUBJID) |>
  slice(n()) |>
  select(USUBJID, AENDY) |>
  rename(ADURN = AENDY)

adsl <- merge(adsl, adex1, by = "USUBJID")

adsl <- adsl |>
  mutate(
    TRTDUR = as.numeric(TRTDUR),
    ADURN = as.numeric(ADURN),
    CMPLPCT = round((ADURN / TRTDUR) * 100, 2)
  )

adsl <- adsl |>
  mutate(
    CMPLRNG = case_when(
      CMPLPCT >= 0 & CMPLPCT <= 20 ~ "0% to <=20%",
      CMPLPCT > 20 & CMPLPCT <= 40 ~ ">20% to <=40%",
      CMPLPCT > 40 & CMPLPCT <= 60 ~ ">40% to <=60%",
      CMPLPCT > 60 & CMPLPCT <= 80 ~ ">60% to <=80%",
      CMPLPCT > 80 ~ ">80%"
    ),
    CMPLRNGN = case_when(
      CMPLPCT >= 0 & CMPLPCT <= 20 ~ 1,
      CMPLPCT > 20 & CMPLPCT <= 40 ~ 2,
      CMPLPCT > 40 & CMPLPCT <= 60 ~ 3,
      CMPLPCT > 60 & CMPLPCT <= 80 ~ 4,
      CMPLPCT > 80 ~ 5
    )
  )

head(adsl)

## -----------------------------------------------------------------------------
adsl$TRTA <- adsl$TRT01A
adsl$TRTA <- factor(adsl$TRTA,
  levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
  labels = c("Placebo", "Low Dose", "High Dose")
)

## -----------------------------------------------------------------------------
plan <- plan(
  analysis = "trt_compliance", population = "apat",
  observation = "apat", parameter = "CMPLRNG;CMPLPCT"
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
    var = c("USUBJID", "TRTA", "SAFFL", "CMPLPCT", "CMPLRNG")
  ) |>
  metalite::define_parameter(
    name = "CMPLPCT",
    var = "CMPLPCT",
    label = "Treatment Compliance Percent",
  ) |>
  metalite::define_parameter(
    name = "CMPLRNG",
    var = "CMPLRNG",
    label = "Treatment Compliance Range",
  ) |>
  define_analysis(
    name = "trt_compliance",
    title = "Summary of Treatment Compliance",
    label = "treatment compliance table"
  ) |>
  meta_build()

## -----------------------------------------------------------------------------
meta

## -----------------------------------------------------------------------------
outdata <- prepare_trt_compliance(meta)

## -----------------------------------------------------------------------------
outdata

## -----------------------------------------------------------------------------
outdata$parameter

## -----------------------------------------------------------------------------
outdata$n

## -----------------------------------------------------------------------------
outdata$group

## -----------------------------------------------------------------------------
outdata$char_n

## -----------------------------------------------------------------------------
outdata$char_var

## -----------------------------------------------------------------------------
outdata$char_prop

## -----------------------------------------------------------------------------
outdata <- outdata |> format_trt_compliance()

## -----------------------------------------------------------------------------
outdata$tbl

## -----------------------------------------------------------------------------
tbl <- outdata |> format_trt_compliance(display_stat = c("mean", "sd", "median", "range"), display_col = c("n", "prop", "total"))

## -----------------------------------------------------------------------------
tbl$tbl

## -----------------------------------------------------------------------------
outdata |>
  format_trt_compliance() |>
  rtf_trt_compliance(
    "Source: [CDISCpilot: adam-adsl]",
    path_outtable = "outtable/treatment0compliance.rtf"
  )

## ----out.width = "100%", out.height = "400px", echo = FALSE, fig.align = "center"----
knitr::include_graphics("pdf/treatment0compliance.pdf")

