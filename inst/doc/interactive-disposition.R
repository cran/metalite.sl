## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  out.width = "100%",
  dpi = 150
)

## ----setup, message=FALSE-----------------------------------------------------
library(metalite.sl)
library(metalite.ae)
library(metalite)
library(dplyr)

## -----------------------------------------------------------------------------
adsl <- r2rtf::r2rtf_adsl
adsl$TRTA <- adsl$TRT01A
adsl$TRTA <- factor(adsl$TRTA,
  levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
  labels = c("Placebo", "Low Dose", "High Dose")
)

# Create a variable EOSSTT indicating the end of end of study status
metadata_ae <- metalite.ae::meta_ae_example()
adae <- metadata_ae$data_observation

# If no value populate for AEACN in AE, then generate dummy values
if (length(unique(adae$AEACN)) == 1) {
  adae$AEACN <- sample(
    x = c("DOSE NOT CHANGED", "DRUG INTERRUPTED", "DRUG WITHDRAWN", "NOT APPLICABLE", "UNKNOWN"),
    size = length(adae$USUBJID),
    prob = c(0.7, 0.1, 0.05, 0.1, 0.05), replace = TRUE
  )
}

metadata_ae$data_observation <- adae

# Treatment Disposition
# For discontinued due to AE
adaedisc <- subset(adae[which(adae$AEACN == "DRUG WITHDRAWN"), ], select = c(USUBJID, AEACN))
adaedisc <- adaedisc[!duplicated(adaedisc), ]

adsl <- merge(adsl, adaedisc, by = "USUBJID", all.x = TRUE)
adsl$EOTSTT <- ifelse(adsl$AEACN == "DRUG WITHDRAWN", "Discontinued", NA)
adsl$DCTREAS <- ifelse(adsl$EOTSTT == "Discontinued", "Adverse Event", NA)

# sample assignment cannot be NA value
adsl$EOTSTT[is.na(adsl$EOTSTT)] <- "temp"
adsl$DCTREAS[is.na(adsl$DCTREAS)] <- "temp"

adsl[adsl$EOTSTT != "Discontinued", "EOTSTT"] <- sample(
  x = c("Complete", "Discontinued", "Participants Ongoing"),
  size = length(adsl[adsl[["EOTSTT"]] != "Discontinued", "USUBJID"]),
  prob = c(0.6, 0.2, 0.2), replace = TRUE
)

adsl[adsl$EOTSTT == "Discontinued" & adsl$DCTREAS != "Adverse Event", "DCTREAS"] <- sample(
  x = c("Withdrawal By Subject", "Lack of Efficacy", "Lost to Follow-Up"),
  size = length(adsl[adsl$EOTSTT == "Discontinued" & adsl$DCTREAS != "Adverse Event", "USUBJID"]),
  prob = c(0.3, 0.4, 0.3), replace = TRUE
)

adsl[adsl[["EOTSTT"]] != "Discontinued", "DCTREAS"] <- NA

# Trial Disposition
adsl$EOSSTT <- adsl$EOTSTT
adsl$DCSREAS <- adsl$DCTREAS

adsl$DCSREAS <- ifelse(adsl$DCSREAS == "Adverse Event", "Other", adsl$DCTREAS)

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
    var = "EOTSTT",
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
react_disposition(
  metadata_sl = meta,
  metadata_ae = metadata_ae,
  width = 1200
)

