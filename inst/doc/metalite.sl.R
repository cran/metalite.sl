## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  out.width = "100%",
  dpi = 150,
  eval = TRUE
)

## -----------------------------------------------------------------------------
library(metalite)
library(metalite.sl)

## ----eval = FALSE-------------------------------------------------------------
#  meta_sl_example() |>
#    prepare_base_char(
#      population = "apat",
#      analysis = "base_char",
#      parameter = "age;gender"
#    ) |>
#    format_base_char() |>
#    rtf_base_char(
#      source = "Source: [CDISCpilot: adam-adsl]",
#      path_outdata = tempfile(fileext = ".Rdata"),
#      path_outtable = tempfile(fileext = ".rtf")
#    )

## ----out.width = "100%", out.height = "400px", echo = FALSE, fig.align = "center"----
knitr::include_graphics("pdf/base0char.pdf")

## -----------------------------------------------------------------------------
react_base_char(
  metadata_sl = meta_sl_example(),
  metadata_ae = metalite.ae::meta_ae_example(),
  population = "apat",
  observation = "wk12",
  display_total = TRUE,
  sl_parameter = "age;race",
  ae_subgroup = c("age", "race"),
  ae_specific = "rel",
  width = 1200
)

