
# Title: DCE2B helpers
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2023-01-02
# Copyright (c) 2023 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Data ----
#nudge = NA
SaveDataDrop <- function(data, rtime, tstart, tend, id = NA, cond = NA, time = NA, order = NA, row1 = NA, row2 = NA, data_dir, n_atts, token) {
  rt <- rep(rtime, each = n_alts)
  unc_rt <- rep(rtime, each = n_atts)
  unc_ord <- rep(order, times = n_sets)
  # Transform 
  d <- as.data.frame(cbind(id = id, cond = cond, time = time, data$design, resp = data$bin_responses, rt = rt, row1 = row1, row2 = row2))
  unc_resp <- rep(data$responses, each = n_atts) 
  unc_setnr <- rep(1:length(data$responses), each = n_atts)
  unc_d <- cbind(id = id, cond = cond, time = time, set = unc_setnr, data$survey, order = unc_ord, resp = unc_resp, rt = unc_rt, row1 = row1, row2 = row2) 
  # Create unique file names
  numname <- sprintf("%s_%s_num_data.txt", as.integer(tstart), as.integer(tend))
  charname <- sprintf("%s_%s_char_data.txt", as.integer(tstart), as.integer(tend))
  # # Create column names
  # v_c <- lapply(seq_along(atts_labs), function(x) 1:(length(atts_labs[[x]]) - 1))
  # v_n <- lapply(seq_along(atts_labs), function(x) sprintf(paste0(names(atts_labs[x]), "_%i"), v_c[[x]]))
  # colnames(d) <- c("id", "cond", unlist(v_n), "resp")
  # Write files to data_dir
  file_path_num <- file.path(tempdir(), numname)
  write.table(d, file_path_num, row.names = TRUE, quote = FALSE, sep = "\t", col.names = NA)
  drop_upload(file_path_num, path = data_dir, dtoken = token)
  file_path_char <- file.path(tempdir(), charname)
  write.table(unc_d, file_path_char, row.names = TRUE, quote = FALSE, sep = "\t", col.names = NA)
  drop_upload(file_path_char, path = data_dir, dtoken = token)
}


SaveDataGoogleSheets <- function(data, rtime, tstart, tend, id = NA, cond = NA, time = NA, order = NA, row1 = NA, row2 = NA, n_atts, sheet_id) {
  rt <- rep(rtime, each = n_alts)
  unc_rt <- rep(rtime, each = n_atts)
  unc_ord <- rep(order, times = n_sets)
  
  d <- as.data.frame(cbind(id = id, cond = cond, time = time, data$design, resp = data$bin_responses, rt = rt, row1 = row1, row2 = row2))
  unc_resp <- rep(data$responses, each = n_atts) 
  unc_setnr <- rep(1:length(data$responses), each = n_atts)
  unc_d <- cbind(id = id, cond = cond, time = time, set = unc_setnr, data$survey, order = unc_ord, resp = unc_resp, rt = unc_rt, row1 = row1, row2 = row2) 
  
  # read from the sheet
  values <- read_sheet(ss = sheet_id, sheet = "main")
  
  # check if the sheet has any existing data
  # if not, write to it and set up column names 
  # otherwise, append to it
  if (nrow(values) == 0) {
    sheet_write(data = d, ss = sheet_id, sheet = "main")
    sheet_write(data = unc_d, ss = sheet_id, sheet = "second_sheet") # You might want to adjust this to your requirements
  } else {
    sheet_append(data = d, ss = sheet_id, sheet = "main")
    sheet_append(data = unc_d, ss = sheet_id, sheet = "second_sheet") # You might want to adjust this to your requirements
  }
}



# Transforms ----

RowColNames <- function(n_sets, n_alts, alt_cte, no_choice) {
  # Creates row and column names for designs
  # Rownames
  r_s <- rep(1:n_sets, each = n_alts)
  r_a <- rep(1:n_alts, n_sets)
  r_names <- paste(paste("set", r_s, sep = ""), paste("alt", r_a, sep = ""), sep = "_")
  if(no_choice){
    ncsek <- seq(n_alts, (n_sets * n_alts), n_alts)  
    r_names[ncsek] <- "no_choice"
  }
  # Colnames alternative specific constants
  if(sum(alt_cte) > .2) {
    cte_names <- paste(paste("alt", which(alt_cte == 1), sep = ""), "_cte", sep = "") 
  } else {
    cte_names <- NULL
  }
  return(list(r_names, cte_names))
}

CharBin <- function (resp, alts, n_alts, no_choice = FALSE) {
  # Error resp not in alts
  if (!all(resp %in% alts)) {
    stop("1 or more responses do not match the possible response options")
  }
  # Error alts
  if (length(alts) != (n_alts + no_choice)) {
    stop("Number of response options is not correct")
  }
  map <- match(resp, alts)
  l <- list()
  for(i in 1:length(map)){
    l[[i]] <- rep(0, n_alts)
    if (no_choice) {
      l[[i]][map[i] - 1] <- 1
    } else {
      l[[i]][map[i]] <- 1
    }
  }
  v <- unlist(l)
  return(v)
}

AltSpec <- function(alt_cte, n_sets) {
  if(!any(alt_cte == 0)){
    stop("alt_cte should at least contain 1 zero")
  }
  # Create matrix
  mat <- diag(length(alt_cte))
  n_zero <- which(alt_cte == 0)
  mat[n_zero, n_zero] <- 0
  # Delete zero columns
  del_col <- c(which(apply(mat, 2,   function(x) all(x == 0))))
  mat <- mat[, -del_col]
  # bind rows for full design 
  mat <- as.matrix(mat)
  cte_mat <- do.call(rbind, replicate(n_sets, mat, simplify = FALSE))
  return(cte_mat)
}

# Choices ----

Select <- function(V, des, bs, atts, atts_lvls, atts_coding, n_atts,
                   alts, n_alts, alts_names, no_choice, alt_cte,
                   n_draws, n_total, n_init, 
                   prior_mean, prior_covar, lower, upper, 
                   reduce, parallel) {
  # Selects current choice set
  if (V$sn <= n_total) {
    # For initial sets 
    if (V$sn <= n_init) {
      set <- des[bs[V$sn]:es[V$sn], ]
    } else {
      # Sample drawing for adaptive sets
      # If First set
      if (V$sn == 1) {
        # Sample draws from prior
        s <- rtmvnorm(
          n = n_draws, mean = prior_mean, 
          sigma = prior_covar, lower = lower, 
          upper = upper
        )
        w <- rep(1, nrow(s)) / nrow(s)
        if (sum(alt_cte) > .2) {
          s <- list(
            as.matrix(s[ , 1:sum(alt_cte)], ncol = sum(alt_cte)), 
            s[ , -c(1:sum(alt_cte))]
          )
        }
        # From second set
      } else {
        # Sample draws from updated posterior
        sam <- ImpsampMNL(
          n.draws = n_draws, prior.mean = prior_mean, 
          prior.covar = prior_covar,
          des = V$fulldes, n.alts = n_alts, y = V$y_bin, 
          alt.cte = alt_cte, lower = lower, upper = upper
        )
        s <- sam$sample
        w <- sam$weights
      }
      # Selecting set
      if (algorithm == "MOD") {
        # Select new set based on Modfed
        setobj <- SeqMOD(
          des = des, cand.set = cand_set, n.alts = n_alts, 
          par.draws = s, prior.covar = prior_covar, 
          alt.cte = alt_cte, weights = w, 
          no.choice = no_choice, parallel = parallel, 
          reduce = reduce
        )
      } else if (algorithm == "CEA") {
        # Select new set based on coordinate exchange
        setobj <- SeqCEA(
          des = des, lvls = atts_lvls, coding = atts_coding,
          n.alts = n_alts, par.draws = s, 
          prior.covar = prior_covar, alt.cte = alt_cte,
          weights = w, no.choice = no_choice, 
          parallel = parallel, reduce = reduce
        )
      }
      set <- setobj$set
      db <- setobj$db
      # Design storage
      if (V$sn == 1) {
        rowcol <- RowColNames(n_sets = 1, n_alts = n_alts, alt_cte = alt_cte, no_choice = FALSE)
        rownames(set) <- rownames(set, do.NULL = FALSE, prefix = paste(paste("set", sn , sep = ""), "alt", sep = "_"))
        colnames(set) <- c(rowcol[[2]], paste("par", 1:(ncol(set) - n_cte), sep = "_"))
        V$fulldes <- set
      } else {
        rowcol <- RowColNames(n_sets = 1, n_alts = n_alts, alt_cte = alt_cte, no_choice = FALSE)
        rownames(set) <- rownames(set, do.NULL = FALSE, prefix = paste(paste("set", sn , sep = ""), "alt", sep = "_"))
        colnames(set) <- c(rowcol[[2]], paste("par", 1:(ncol(set) - n_cte), sep = "_"))
        V$fulldes <- rbind(V$fulldes, set)
      }
    }
    # Transform coded set to attribute level character set
    choice_set <- Decode(
      des = set, n.alts = n_alts, lvl.names = V$atts_cond_labs, coding = atts_coding, 
      alt.cte = alt_cte, no.choice = no_choice
    )[[1]]
    choice_set <- t(choice_set[ , 1:n_atts])
    # Fill in attribute names and alternatives names
    colnames(choice_set) <- alts
    rownames(choice_set) <- V$atts_cond_names
    
    # Swap duplicate levels on last attribute
    chkd <- identical(choice_set[9, 1], choice_set[9, 2])
    if (chkd) {
      optd <- unique(c(choice_set[9, ]))
      optr <- setdiff(atts_labs[[length(atts_labs)]], choice_set[9, ])
      choice_set[9, sample(1:2, 1)] <- optr
    }
    
    # Return design 
    if (!is.null(no_choice)) {
      V$choice_set <- choice_set[, -no_choice]
    } else {
      V$choice_set <- choice_set
    }
  }
}
