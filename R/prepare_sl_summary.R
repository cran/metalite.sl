# Copyright (c) 2024 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
# All rights reserved.
#
# This file is part of the metalite.sl program.
#
# metalite.sl is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Prepare data for baseline characteristic table
#'
#' @param meta A metadata object created by metalite.
#' @param population A character value of population term name.
#'   The term name is used as key to link information.
#' @param analysis A character value of analysis term name.
#'   The term name is used as key to link information.
#' @param parameter A character value of parameter term name.
#'   The term name is used as key to link information.
#'
#' @return A list of analysis raw datasets.
#'
#' @export
#'
#' @examples
#' meta <- meta_sl_example()
#' meta |> prepare_sl_summary(population = "apat", analysis = "base_char")
prepare_sl_summary <- function(
    meta,
    population,
    analysis,
    parameter = paste(names(meta$parameter), collapse = ";")) {
  parameters <- unlist(strsplit(parameter, ";"))
  observation <- population
  # obtain variables
  pop_var <- metalite::collect_adam_mapping(meta, population)$var
  par_var <- do.call(c, lapply(parameters, function(x) {
    metalite::collect_adam_mapping(meta, x)$var
  }))

  par_var_group <- do.call(c, lapply(parameters, function(x) {
    metalite::collect_adam_mapping(meta, x)$vargroup
  }))

  par_var_lower <- do.call(c, lapply(parameters, function(x) {
    metalite::collect_adam_mapping(meta, x)$var_lower
  }))

  pop_group <- metalite::collect_adam_mapping(meta, population)$group

  pop_id <- metalite::collect_adam_mapping(meta, population)$id

  # obtain data
  pop <- metalite::collect_population_record(meta, population, var = c(par_var, par_var_group, par_var_lower))

  # obtain group names
  group <- unique(pop[[pop_group]])

  # count the number of subjects in each arms
  n_pop <- metalite::n_subject(id = pop[[pop_id]], group = pop[[pop_group]])
  names(n_pop) <- do.call(
    c,
    lapply(
      factor(names(n_pop), levels = levels(pop[[pop_group]])) |> as.numeric(),
      function(x) {
        paste0("n_", x)
      }
    )
  )
  n_pop$n_9999 <- sum(n_pop[1, ])
  n_pop$name <- "Participants in population"
  n_pop <- n_pop[, c(length(group) + 2, 1:(length(group) + 1))]
  n_pop$var_label <- "-----"

  # Get the baseline characteristics variables in adsl
  # char_var <- collect_adam_mapping(meta, analysis)$var_name
  char_var <- par_var

  # Get the baseline characteristics counts
  char_n <- lapply(parameters, function(x) {
    collect_baseline(meta, population, x)[[2]]
  })

  # Get the baseline characteristics propositions
  char_prop <- lapply(parameters, function(x) {
    collect_baseline(meta, population, x)[[3]]
  })

  # Get the variable date type
  var_type <- lapply(parameters, function(x) {
    collect_baseline(meta, population, x)[[4]]
  })

  ans <- metalite::outdata(
    meta, population, observation, parameter,
    n = n_pop,
    order = NULL, group = pop_group, reference_group = NULL,
    char_n = char_n,
    char_var = char_var,
    char_prop = char_prop,
    var_type = var_type,
    group_label = unique(pop[[pop_group]]),
    analysis = analysis
  )

  return(ans)
}
