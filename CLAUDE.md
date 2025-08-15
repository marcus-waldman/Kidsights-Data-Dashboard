# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R Shiny application called "Kidsights Data Dashboard" that monitors sampling progress for early childhood development research. The dashboard tracks data collection metrics, provides real-time visualization of sampling targets, and includes an experimental AI-powered plotting feature.

## Running the Application

- **Start the Shiny app**: Open `ui.R` or `server.R` in RStudio and click "Run App", or use `shiny::runApp()`
- **Package management**: Run `source("utils/utils-packages.R")` then `check_packages_installed()` to install required dependencies
- **Deploy to shinyapps.io**: The app is configured for deployment (see `rsconnect/` folder)

## Architecture

### Core Structure
- **ui.R**: User interface definition with navigation tabs for API, Vetting, Sampling Strata, and AI Plotter
- **server.R**: Server logic handling data processing, reactive elements, and plot generation
- **utils/**: Modular utility functions organized by purpose:
  - `utils-packages.R`: Package installation and management
  - `utils-init.R`: Initialization functions and data category definitions
  - `utils-etl.R`: Data extraction, transformation, and loading
  - `utils-server.R`: Server-side helper functions including `make_crosstab_table()` for demographic cross-tabulation
  - `utils-eligibility.R`: Participant eligibility criteria
  - `utils-ai.R`: AI-powered plot generation functionality
  - `Kidsights_ggtheme.R`: Custom ggplot2 theme

### Data Pipeline
1. **API Authentication**: REDCap API credentials uploaded via file input
2. **Data Download**: `download_vet_responses()` retrieves survey data
3. **Eligibility Filtering**: `include_exclude()` applies study criteria
4. **Variable Recoding**: `recode_it()` transforms raw data
5. **Metadata Creation**: `create_variable_metadata()` generates variable descriptions
6. **Visualization**: Multiple plotting functions for different demographic strata

### Key Data Files
- `data/calibdat.rds`: Calibration dataset
- `data/form.rds`: Form structure data
- `data/ne_counties.rds`: Nebraska county geographic data
- `data/zcta.rds`: ZIP code tabulation area data
- `data/codebook.xlsx`: Variable definitions and metadata

## Key Features

### Sampling Monitoring
- Education level stratification
- Race/ethnicity demographics
- Federal poverty level analysis
- Geographic distribution mapping
- Cross-tabulation analysis with age filtering

### AI Integration
- Experimental AI plotting using Anthropic Claude API
- Dynamic plot generation based on natural language prompts
- Integration with `ellmer` package for model access
- Variable selection interface for focused analysis

### Data Security
- API keys loaded from external CSV files (not committed to repo)
- REDCap integration for secure data access
- Local keyring backend for credential management

## Development Notes

- **R Project**: Uses RStudio project configuration (`.Rproj` file)
- **Code Style**: 2-space indentation, UTF-8 encoding
- **Dependencies**: Heavy reliance on tidyverse, shiny ecosystem, and spatial analysis packages
- **Deployment**: Configured for shinyapps.io hosting

## Key Functions

### Cross-tabulation Analysis
- **`make_crosstab_table(df, var1, var2, years_filter = NULL)`**: Creates demographic cross-tabulation tables
  - Supports three variables: `raceG` (Race/Ethnicity), `fplcat` (Federal Poverty Level), `educ4_max` (Education)
  - Optional age filtering using `years_filter` parameter (ages 0-5)
  - Automatically calculates marginal totals
  - Returns formatted table with proper variable labels

### UI Components
- **Crosstab tab**: Located under Sampling Strata navbar menu
- **Variable selectors**: Two dropdown menus for cross-tab variables
- **Age filter**: Checkbox group for filtering by child age (0-5 years)
- **Export functionality**: Built-in CSV, Excel, PDF export options

## Data Categories

The application works with these main data categories (defined in `utils-init.R`):
- Demographics (race, education, income)
- Survey completion tracking
- Mental health variables
- Childcare experiences
- Geographic indicators