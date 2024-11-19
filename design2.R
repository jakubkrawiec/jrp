
# Title: Creating D-optimal designs
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2023-01-02
# Copyright (c) 2023 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Setup -------------------------------------------------------------------
# library(rdrop2)
# drop_auth()
# # This will launch your browser and request access to your Dropbox account. You will be prompted to log in if you aren't already logged in.
# # Once completed, close your browser window and return to R to complete authentication. 
# # The credentials are automatically cached (you can prevent this) for future use.
# 
# # If you wish to save the tokens, for local/remote use
# 
# token <- drop_auth()
# saveRDS(token, file = "droptoken.rds")


# drop_auth(rdstoken = "C:/Users/jkrawie1/Dropbox/Projects/bad_3_n_disc/Shiny/bad_3_n_disc/droptoken.rds")

# Then in any drop_* function, pass `dtoken = token
# Tokens are valid until revoked.
setwd("C:/Users/jkrawie1/Dropbox/vaccinations/Shiny/kuba_savings/jrp_test")


source("R/globals.R") # Set globals
library(tidyverse) # import packages
library(idefix)
source("R/helpers.R") # import helpers

exp_id <- "jrp_test" # exp design to estimate
set.seed(42)

# Design ------------------------------------------------------------------


## Attributes ----




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
atts_labs <- atts # Lista poziomów atrybutów
atts_lvls <- lengths(atts_labs) # Liczba poziomów każdego atrybutu
names(atts_lvls) <- atts_names # Dodanie nazw atrybutów do liczby poziomów
atts_coding <- rep("E", length(atts)) # Typy atrybutów (zakładam "E" - ciągłe)
n_atts <- length(atts_labs) # Liczba atrybutów
alts <- c("Scenariusz A", "Scenariusz B") # Etykiety alternatyw
n_alts <- length(alts) # Liczba alternatyw
alt_cte <- c(0, 0) # Specyficzne stałe dla alternatyw
# check_dup <- names(atts[9]) # check duplicate levels in all sets for this attribute (set to NULL to disable)

## Sets ----

n_sets = 35 # number of total choice sets
n_total = 35 # number of presented choice sets
n_draws = 50 # number of draws

## Optimize ----

des_file_path <- paste0(dir_shiny, exp_id, "/design.RData")
if (!file.exists(des_file_path)) {
  profiles <- Profiles(lvls = atts_lvls, coding = atts_coding) # candidate combinations
  mu <- rep(0, ncol(profiles)) # naive priors
  sigma <- diag(length(mu))
  pd <- MASS::mvrnorm(10, mu, sigma)
  
  des <- CEA( # estimate design (coordinate exchange)
    lvls = atts_lvls, coding = atts_coding, 
    n.sets = n_sets, n.alts = n_alts, par.draws = pd,
    alt.cte = alt_cte, parallel = TRUE, best = TRUE
  )
  
  save(des, file = des_file_path)
} else {
  load(des_file_path)
}

# Inspect design
des
des_decode <- Decode(des = des$design, n.alts = n_alts, lvl.names = atts_labs, coding = atts_coding)
des_bal <- des_decode$lvl.balance
des_cond <- rownames_to_column(set_names(des_decode$design, atts_names), var = "set")
head(des_cond, 20)
des_bal

# Rule-of-thumb calculation (include two-way interactions)
atts_lvls_prods <- max(combn(atts_lvls, 2, function(x) x[1] * x[2]))
N2 <- 500 * atts_lvls_prods / (n_total * n_alts)
round(N2)

# Test --------------------------------------------------------------------

intro_text <- readLines(paste0("Shiny/", exp_id, "/intro.txt"), encoding = "UTF-8")
buttons_text <- "Choose preferred option"
end_text <- readLines(paste0("Shiny/", exp_id, "/outro.txt"), encoding = "UTF-8")

SurveyApp(
  des = des$design, n.total = n_total, alts = alts,
  atts = atts_names, lvl.names = atts_labs, coding = atts_coding,
  buttons.text = buttons_text, intro.text = intro_text, end.text = end_text,
  prior.mean = mu, prior.covar = sigma, cand.set = profiles, n.draws = n_draws,
  data.dir = paste0(dir_datbase)
)
library(rsconnect)
# options(rsconnect.locale.cache = FALSE, rsconnect.locale = "pl_PL.UTF-8")
rsconnect::deployApp("C:/Users/jkrawie1/Dropbox/vaccinations/Shiny/kuba_savings/jrp_test/Shiny/jrp_test")
