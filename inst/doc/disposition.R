## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  out.width = "100%",
  dpi = 150
)

## ----setup, message=FALSE-----------------------------------------------------
library(metalite.sl)
library(metalite)
library(dplyr)

## ----out.width = "100%", out.height = "400px", echo = FALSE, fig.align = "center"----
knitr::include_graphics("pdf/disposition.pdf")

## -----------------------------------------------------------------------------
adsl <- r2rtf::r2rtf_adsl
adsl$TRTA <- adsl$TRT01A
adsl$TRTA <- factor(adsl$TRTA,
  levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
  labels = c("Placebo", "Low Dose", "High Dose")
)

# Create a variable EOSSTT indicating the end of end of study status
adsl$EOSSTT <- sample(
  x = c("Participants Ongoing", "Discontinued"),
  size = length(adsl$USUBJID),
  prob = c(0.8, 0.2), replace = TRUE
)
adsl[adsl[["EOSSTT"]] == "Discontinued", "DCSREAS"] <- sample(
  x = c("Adverse Event", "I/E Not Met", "Withdrew Consent", "Lack of Efficacy"),
  size = length(adsl[adsl[["EOSSTT"]] == "Discontinued", "USUBJID"]),
  prob = c(0.7, 0.1, 0.1, 0.1), replace = TRUE
)

# Create a variable EOTSTT1 indicating the end of treatment status part 1
adsl$EOTSTT1 <- ifelse(adsl$DISCONFL == "Y", "Discontinued", "Completed")
adsl$DCTREAS <- ifelse(adsl$EOTSTT1 == "Discontinued", adsl$DCREASCD, NA)
head(adsl)

## -----------------------------------------------------------------------------
plan <- plan(
  analysis = "disp", population = "apat",
  observation = "apat", parameter = "disposition;medical-disposition"
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
    subset = quote(SAFFL == "Y")
  ) |>
  define_parameter(
    name = "disposition",
    var = "EOSSTT",
    label = "Trial Disposition",
    var_lower = "DCSREAS"
  ) |>
  define_parameter(
    name = "medical-disposition",
    var = "EOTSTT1",
    label = "Participant Study Medication Disposition",
    var_lower = "DCTREAS"
  ) |>
  define_analysis(
    name = "disp",
    title = "Disposition of Participant",
    label = "disposition table"
  ) |>
  meta_build()

## -----------------------------------------------------------------------------
meta

## -----------------------------------------------------------------------------
outdata <- prepare_disposition(meta)

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
outdata <- outdata |> format_disposition()

## -----------------------------------------------------------------------------
outdata$tbl

## -----------------------------------------------------------------------------
outdata$tbl <- outdata$tbl %>%
  mutate(name = ifelse(trimws(name) == "Status Not Recorded", "    Status Not Recorded", name))

outdata |>
  rtf_disposition(
    "Source: [CDISCpilot: adam-adsl]",
    path_outdata = tempfile(fileext = ".Rdata"),
    path_outtable = "outtable/disposition.rtf"
  )

## ----out.width = "100%", out.height = "400px", echo = FALSE, fig.align = "center"----
knitr::include_graphics("pdf/disposition.pdf")

