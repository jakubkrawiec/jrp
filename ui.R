
# Title: DCE2B UI
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2023-01-02
# Copyright (c) 2023 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This is the UI logic of a Shiny web application for DCE data collection

ui <- fluidPage(
    useShinyjs(),
    column(12, align = "center", style = "padding-top:0px; padding-bottom:5px", textOutput("set_nr")), # set num
    column(12, align = "center", tableOutput("choice_set")), # design
    column(12, align = "center", br()),
    column(12, align = "center", uiOutput("buttons")), # choice options
    column(12, align = "center", style = "padding-bottom:10px", htmlOutput("intro")), # intro text
    column(12, align = "center", actionButton("OK", "OK")), # action button
    column(12, align = "center", style = "padding-top:10px", htmlOutput("end")), # end text
    tags$head(tags$style("#nudge_option{position:relative}"))
    )