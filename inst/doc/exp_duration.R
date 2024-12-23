## -----------------------------------------------------------------------------
library(metalite)
library(metalite.sl)

## ----out.width = "100%", out.height = "400px", echo = FALSE, fig.align = "center"----
knitr::include_graphics("pdf/exp0duration.pdf")

## -----------------------------------------------------------------------------
adsl <- r2rtf::r2rtf_adsl
adexsum <- data.frame(USUBJID = adsl$USUBJID)
adexsum$TRTA <- factor(adsl$TRT01A,
  levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
  labels = c("Placebo", "Low Dose", "High Dose")
)

adexsum$APERIODC <- "Base"
adexsum$APERIOD <- 1

set.seed(123) # Set a seed for reproducibility
adexsum$AVAL <- sample(x = 0:(24 * 7), size = length(adexsum$USUBJID), replace = TRUE)
adexsum$EXDURGR <- "not treated"
adexsum$EXDURGR[adexsum$AVAL >= 1] <- ">=1 day"
adexsum$EXDURGR[adexsum$AVAL >= 7] <- ">=7 days"
adexsum$EXDURGR[adexsum$AVAL >= 28] <- ">=28 days"
adexsum$EXDURGR[adexsum$AVAL >= 12 * 7] <- ">=12 weeks"
adexsum$EXDURGR[adexsum$AVAL >= 24 * 7] <- ">=24 weeks"

adexsum$EXDURGR <- factor(adexsum$EXDURGR,
  levels = c("not treated", ">=1 day", ">=7 days", ">=28 days", ">=12 weeks", ">=24 weeks")
)
unique(adexsum$EXDURGR)

## -----------------------------------------------------------------------------
plan <- metalite::plan(
  analysis = "exp_dur", population = "apat",
  observation = "apat", parameter = "expdur"
)

meta <- metalite::meta_adam(
  population = adexsum,
  observation = adexsum
) |>
  metalite::define_plan(plan) |>
  metalite::define_population(
    name = "apat",
    group = "TRTA",
    subset = quote(APERIOD == 1 & AVAL > 0)
  ) |>
  metalite::define_parameter(
    name = "expdur",
    var = "AVAL",
    label = "Exposure Duration (Days)",
    vargroup = "EXDURGR"
  ) |>
  metalite::define_analysis(
    name = "exp_dur",
    title = "Summary of Exposure Duration",
    label = "exposure duration table"
  ) |>
  metalite::meta_build()

## -----------------------------------------------------------------------------
outdata <- prepare_exp_duration(meta)
outdata

## -----------------------------------------------------------------------------
outdata$n[, 1:5]

## -----------------------------------------------------------------------------
charn <- data.frame(outdata$char_n[1])
head(charn[, 1:5], 6)

## -----------------------------------------------------------------------------
charp <- data.frame(outdata$char_prop[1])
head(charp[, 1:5], 6)

## -----------------------------------------------------------------------------
chars <- data.frame(outdata$char_n[1])
tail(chars[, 1:5], 8)

## -----------------------------------------------------------------------------
tbl <- format_exp_duration(outdata, display_col = c("n", "prop", "total"))
head(tbl$tbl)

## ----warning=FALSE------------------------------------------------------------
outdata <- format_exp_duration(outdata, display_col = c("n", "prop", "total")) |>
  rtf_exp_duration(
    source = "Source:  [CDISCpilot: adam-adexsum]",
    path_outdata = tempfile(fileext = ".Rdata"),
    path_outtable = "outtable/exp0duration.rtf"
  )

## ----out.width = "100%", out.height = "400px", echo = FALSE, fig.align = "center"----
knitr::include_graphics("pdf/exp0duration.pdf")

