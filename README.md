# JRP: Survey Experiment Application

This repository contains the files and scripts necessary to run a **Survey Experiment Application** using R and the `shiny` package. The project is set up for designing, implementing, and analyzing choice experiments.

---

## Project Structure

### Main Files
- **`atts.xlsx`**  
  Contains the attributes and their levels for the choice experiment.  
  **Format**: Excel file with columns representing different attributes and their levels.

- **`design.RData`**  
  Stores the experimental design generated using the `idefix` package or similar.  
  **Format**: Binary RData file.

- **`design2.R`**  
  R script to generate an experimental design based on the attributes from `atts.xlsx`.  
  Outputs a new `design.RData` file.

---

### Application Files
- **`global.R`**  
  Sets up global variables and loads required datasets for the Shiny app.  
  Reads in attributes, levels, and the experimental design.

- **`helpers.R`**  
  Contains helper functions used throughout the application, such as data processing, decoding levels, and utility functions for the choice experiment.

- **`server.R`**  
  Defines the server logic for the Shiny app.  
  Manages user inputs, processes experimental designs, and dynamically updates the choice sets.

- **`ui.R`**  
  Defines the user interface for the Shiny app.  
  Includes input components (e.g., radio buttons) and outputs (e.g., tables and text).

---

### Supporting Files
- **`intro.txt`**  
  Introduction text shown to participants at the start of the survey.

- **`outro.txt`**  
  Closing text shown to participants at the end of the survey.

---

## How to Run the Application

1. **Install Required Packages**  
   Ensure you have the following R packages installed:
   - `shiny`
   - `readr`
   - `idefix`
   - `stringi`

   Install missing packages using:
   ```R
   install.packages(c("shiny", "readr", "idefix", "stringi"))
