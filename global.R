
# Title: DCE2B global settings
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2023-01-02
# Copyright (c) 2023 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Setup -------------------------------------------------------------------

library(shiny) # import packages
library(shinyjs)
library(rdrop2)
library(tidyverse)
library(idefix)
library(tmvtnorm)
library(kableExtra)
source("helpers.R")


## Options ----

exp_id <- "jrp_test"
drop_token <- readRDS("droptoken.rds") 
drop_path <- "Projects/vaccinations/Shiny/kuba_savings/jrp_test/"


# 
# options(
#   # whenever there is one account token found, use the cached token
#   gargle_oauth_email = TRUE,
#   # specify auth tokens should be stored in a hidden directory ".secrets"
#   gargle_oauth_cache = "C:/Users/jkrawie1/Dropbox/Projects/bad_3_n_disc/Shiny/bad_3_n_disc/.secrets"
# )

# Get the ID of the sheet for writing programmatically
# This should be placed at the top of your shiny app
# sheet_id <- drive_get("test")$id


## Design ----

### Procedure ----

nudge_str <- "WBW"
nudge_msg <- "Większość wybiera tę opcję"
intro_text <- readLines("intro.txt", encoding = "UTF-8")
outro_text <- readLines("outro.txt", encoding = "UTF-8")

### Attributes ----

no_choice <- NULL
lower <- NULL
upper <- NULL
parallel <- FALSE
reduce <- TRUE
buttons <- NULL
load("design.RData")
des <- des$design
atts <- list(
  "Risk index" = c("1-2", "3-5", "6-7"),
  "Projected return/Interest rates" = c("3-5%", "6-8%", "9-10%"),
  "Historical trend" = c("graph 1", "graph 2", "graph 3"),
  "SRI" = c("High", "Low"),
  "Rating level" = c("Low", "Medium", "High"),
  "How many of your friends/relatives are investing?" = c("10%", "20%", "30%")
)

# Nazwy atrybutów
atts_names <- stringi::stri_trans_general(names(atts), "Latin-ASCII") # Zamiana specjalnych znaków
atts_labs <- as.list(lapply(atts, function(x) x[!is.na(x)])) # attribute level names
atts_lvls <- as.numeric(lengths(atts_labs)) # attribute levels
names(atts_lvls) <- atts_names # add attribute names to levels
atts_coding <- rep("E", length(atts)) # # attribute types
n_atts <- length(atts_labs) # number of attributes
alts = c("Program A", "Program B") # option labels
n_alts = length(alts) # num options
alt_cte <- c(0, 0) # alternative-specific constants?

### Sets ----

n_sets = nrow(des) / n_alts # number of total choice sets
n_total = 35 # number of presented choice sets
n_draws = 50 # number of draws

### Initialize ----

cand_set <- Profiles(lvls = atts_lvls, coding = atts_coding) # candidate combinations
prior_mean <- rep(0, ncol(cand_set)) # naive priors
prior_covar <- diag(length(prior_mean))
algorithm = "CEA"

if (is.null(des)) {
  n_init <- 0
} else {
  n_init <- nrow(des) / n_alts
  if (!isTRUE(all.equal(n_init, as.integer(n_init)))) {
    stop("the number of rows of 'des' are not a multiple of length(alts)")
  }
}

if (is.null(alt_cte) || all(alt_cte == 0)) {
  alt_cte <- rep(0, n_alts)
  n_cte <- 0
  cte_des <- NULL
} else {
  # Error 
  if (length(alt_cte) != n_alts) {
    stop("length(alts) does not match length(alt.cte)")
  }
  if (!all(alt_cte %in% c(0, 1))) {
    stop("'alt_cte' should only contain 0's or 1's.")
  }
  if (!any(alt_cte == 0)) {
    stop("'alt_cte' should at least contain 1 zero")
  }
  n_cte <- sum(alt_cte)
  if (!is.null(des)) {
    cte_des <- AltSpec(alt_cte = alt_cte, n_sets = n_init)
    if (!isTRUE(all.equal(cte_des, matrix(des[, 1:n_cte], ncol = n_cte)))) {
      stop("the first column(s) of 'des' are different from what is expected based on 'alt.cte'")
    }
  }
}

# Error handling (no_choice)
if (!is.null(no_choice)) {
  if (!is.numeric(no_choice)) {
    stop("'no_choice' should be an integer indicating the no choice alternative")
  }
  if (!no_choice %% 1 == 0) {
    stop("'no_choice' should be an integer")
  }
  if (any(isTRUE(no_choice > (n_alts + .2)), isTRUE(no_choice < .2))) {
    stop("'no_choice' does not indicate one of the alternatives")
  }
  if (!isTRUE(all.equal(alt_cte[no_choice], 1))) {
    stop("the location of the 'no_choice' option in the 'alt_cte' vector should correspond with 1")
  }
}

# Error handling (adaptive)
if (n_total > n_init) {
  if (is.null(lower)) {
    lower <- rep(-Inf, length(prior_mean))
  }
  if (is.null(upper)) {
    upper <- rep(Inf, length(prior_mean))
  }
  if (!any(c(isTRUE(all.equal(length(prior_mean), length(lower))), isTRUE(all.equal(length(prior_mean), length(upper)))))) {
    stop("length 'prior_mean' should equal 'upper' and 'lower'")
  }
  if (algorithm == "MOD") {
    if (any(c(is.null(prior_mean), is.null(prior_covar), is.null(cand_set), is.null(n_draws)))) {
      stop("When n_total is larger than the number of sets in argument des, arguments prior_mean, prior_covar, cand_set, and n_draws should be specified")
    }
    if (length(prior_mean) != ncol(cand_set) + sum(alt_cte)) {
      stop("Number of parameters in prior_mean does not match with cand_set + alt_cte")
    }
  } else if (algorithm == "CEA") {
    if (any(c(is.null(prior_mean), is.null(prior_covar), is.null(n_draws)))) {
      stop("When n_total is larger than the number of sets in argument des, arguments prior_mean, prior_covar, and n_draws should be specified")
    }
  }
  if (!isTRUE(all.equal(length(prior_mean), ncol(prior_covar)))) {
    stop("length(prior_mean) differs from ncol(prior_covar)")
  }
} else {
  if (!is.null(prior_mean)) {
    warning("prior_mean will be ignored, since there are no adaptive sets")
  } 
  if (!is.null(prior_covar)) {
    warning("'prior_covar' will be ignored, since there are no adaptive sets")
  }
  if (algorithm == "MOD" & !is.null(cand_set)) {
    warning("'cand_set' will be ignored, since there are no adaptive sets")
  }
  if (!is.null(lower) || !is.null(upper)) {
    warning("lower and upper bound will be ignored, since there are no adaptive sets")
  }
  if (!is.null(n_draws)) {
    warning("n_draws' will be ignored, since there are no adaptive sets")
  }
}

if (is.null(des)) {
  if (algorithm == "MOD") {
    fulldes <- matrix(data = NA, nrow = (n_alts * n_total), ncol = ncol(cand_set))
  } else {
    fulldes <- matrix()
  }
} else {
  bs <- seq(1, (nrow(des) - n_alts + 1), n_alts)
  es <- c((bs - 1), nrow(des))[-1] 
  rowcol <- RowColNames(n_sets = n_init, n_alts = n_alts, alt_cte = alt_cte, no_choice = FALSE)
  rownames(des) <- rowcol[[1]]
  if (is.null(colnames(des))) {
    colnames(des) <- c(rowcol[[2]], paste("par", 1:(ncol(des) - n_cte), sep = "_"))
  }
  fulldes <- des
  # Error handling (des)
  if (length(bs) != n_init) {
    stop("The number of rows in 'des' is not a multiple of length(atts)")
  }
  if ("no_choice_cte" %in% colnames(des)) {
    if (is.null(no.choice)) {
      warning("no_choice_cte column name detected in des while no_choice = NULL")
    }
  }
}

# Error handling (algorithm)
if (!(algorithm %in% c("MOD", "CEA"))) {
  stop("algorithm should be 'MOD' or 'CEA'")
}

