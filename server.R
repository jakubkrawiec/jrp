
# Title: DCE2B server
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2023-01-02
# Copyright (c) 2023 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This is the server logic of a Shiny web application for DCE data collection

# Set up choice set selection
server <- function(input, output, session) {
  
  # Shuffle attribute order for this participant
  atts_order <- isolate({sample(atts_names)})
  
  # Values for current user
  V <- reactiveValues(
    time_start = NA,
    time_end = NA,
    subjID = NA,
    cond = "Default",
    time = NA,
    atts_cond = NA,
    atts_cond_names = vector(mode = "list"),
    atts_cond_labs = vector(mode = "list"),
    sn = 0,
    fulldes = fulldes,
    sdata = vector(mode = "list"),
    survey_data = vector(mode = "list"),
    y_bin = vector("numeric"),
    resp = vector("character"),
    choice_set = NA,
    choice_sets = matrix(data = NA, nrow = (n_total * n_alts), ncol = n_atts),
    buttons_text = "Undefined. This is the default message.",
    ts_click = NA,
    rt = NULL
  )
  
  # Query condition
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[["id"]])) {
      V$subjID <- query[["id"]]
    }
    if (!is.null(query[["condition"]])) {
      V$cond <- query[["condition"]]
    }
    if (!is.na(V$cond)) {
      V$atts_cond <- read_csv("atts.csv", locale = locale(encoding = "UTF-8")) # read attributes and levels
      V$atts_cond_names <- names(V$atts_cond)
      V$atts_cond_labs <- as.list(lapply(V$atts_cond, function(x) x[!is.na(x)]))
      if (V$cond == "child") {
        V$buttons_text <- "In which situation would you be more likely to enroll in a savings program? The choice is yours:"
      } else {
        V$buttons_text <- "In which situation would you be more likely to enroll in a savings program? The choice is yours:"
      }
    }
    if (!is.null(query[["time"]])) {
      V$time <- query[["time"]]
    }
  })
  
  # Intro text
  output$intro <- renderText(isolate(intro_text))
  
  # Toggling action button
  
  observeEvent(input$survey, {
    # Enable action button after answer
    enable("OK")
  })
  
  observe({
    if (V$sn > n_total) {
      # Enable action button for end text
      enable("OK")
    }
  })
  
  # When action button is clicked
  observeEvent(input$OK, {
    
    # Disable action button after click
    disable("OK")
    
    delay(1000, {
      
      # Count set number
      V$sn <- V$sn + 1
      
      # Set click time stamp
      if (V$sn == 1) {
        V$ts_click <- Sys.time()
      }
      if (V$sn > 1 && V$sn <= (n_total + 1)) {
        V$ts_click <- append(V$ts_click, Sys.time())
        V$rt <- append(V$rt, as.numeric(Sys.time() - V$ts_click[V$sn - 1]))
      }
      
      # Start phase
      
      output$intro <- renderText(NULL)
      
      # Set num display
      if (V$sn <= n_total) {
        output$set_nr <- renderText(isolate(paste(c("Choice:", V$sn, "/", n_total))))
      } else {
        output$set_nr <- renderText(NULL)
      }
      
      # Survey phase 
      
      # Get current choice set
      if (V$sn <= n_total) {
        selected <- reactive(
          Select(
            V, des, bs, atts, atts_lvls, atts_coding, n_atts,
            alts, n_alts, alts_names, no_choice, alt_cte,
            n_draws, n_total, n_init, 
            prior_mean, prior_covar, lower, upper, 
            reduce, parallel
          )
        )
        
        # Rate-limiter
        # selected_d <- debounce(selected, 500)
        
        # Display new choice set
        output$choice_set <- function() {
          isolate({
            sel <- selected()
            
            sel <- sel[order(match(atts_names, atts_order)), ]
            
            sel <-
              sel %>%
              kable("html") %>%
              kable_styling()
            return(sel)
            })
          }
      }
      
      # Store uncoded choice set
      if (V$sn == 2) {
        V$choice_sets <- V$choice_set
      } else if (V$sn > 2 && V$sn <= (n_total + 1)) {
        V$choice_sets <- rbind(V$choice_sets, V$choice_set)
      }
      if (V$sn > 1 && V$sn <= (n_total + 1)) {
        V$resp  <- c(V$resp, input$survey)
        V$y_bin <- CharBin(resp = V$resp, alts = alts, n_alts = n_alts)
        V$sdata[["bin_responses"]] <- V$y_bin
        V$sdata[["responses"]] <- V$resp
        V$sdata[["design"]] <- V$fulldes
        V$sdata[["survey"]] <- V$choice_sets
        V$survey_data <- V$sdata 
      } 
      
      # End phase 
      
      if (V$sn > n_total) {
        # Don't show choice set
        output$choice_set <-  renderTable(NULL)
      }
      
      # Display end text at end of survey
      if (V$sn > n_total) {
        # Display end text 
        output$end <- renderText(isolate(outro_text))
      }
      
      # Get start and end time
      if (V$sn == 1) {
        V$time_start <- Sys.time()
      }
      if (V$sn == (n_total + 1)) {
        V$time_end <- Sys.time()
      }
      
      if (V$sn == (n_total + 1)) {
        # Write data to file
        if (!is.null(drop_path)) {
          SaveDataDrop(
            data = V$survey_data, rtime = V$rt,
            tstart = V$time_start, tend = V$time_end,
            id = V$subjID, cond = V$cond, time = V$time,
            order = match(atts_names, atts_order),
            data_dir = paste0(drop_path, "data/base/", exp_id, "/"),
            n_atts = n_atts, token = drop_token
          )
        }
      }
      # Redirect 
      if (V$sn > (n_total + 1)) {
        runjs(sprintf("window.location.href = 'https://psychodpt.fra1.qualtrics.com/jfe/form/SV_5omLD5XXcDF59pc';"))
      }
    })
  })
  
  # Output response options after first action button click
  output$buttons <- renderUI({
    # Radiobuttons
    if (V$sn > 0 && V$sn <= n_total) {
      return(list(radioButtons("survey", V$buttons_text, alts, inline = TRUE, selected = "None")))
    }
  })
}
