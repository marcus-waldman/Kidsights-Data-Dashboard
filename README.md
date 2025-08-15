# Kidsights Data Dashboard

A real-time R Shiny application for monitoring sampling progress in early childhood development research.

## Overview

This dashboard provides data collection teams with real-time insights into recruitment progress, demographic representation, and sampling targets for the Kidsights early childhood development study. It integrates with REDCap APIs to provide automated monitoring and visualization capabilities.

## Quick Start

### Prerequisites

-   R (\>= 4.0.0)
-   RStudio (recommended)
-   REDCap API access credentials

### Installation

1.  Clone the repository:

``` bash
git clone <repository-url>
cd Kidsights-Data-Dashboard
```

2.  Install dependencies:

``` r
source("utils/utils-packages.R")
check_packages_installed()
```

3.  Run the application:

``` r
# In RStudio: Open ui.R or server.R and click "Run App"
# Or use:
shiny::runApp()
```

## Configuration

### REDCap API Setup

1.  Obtain REDCap API credentials

2.  Create a CSV file with your API key:

    ``` csv
    api
    your_api_key_here
    ```

3.  Upload via the dashboard's API tab

### Anthropic AI Integration (Optional)

For the experimental AI plotting feature: 1. Get an Anthropic API key 2. Create a CSV file: `csv    api    your_anthropic_key_here` 3. Upload via the AI Plotter tab

## Architecture

### Core Components

-   **`ui.R`**: User interface with navigation tabs for API, Vetting, Sampling Strata, and AI features
-   **`server.R`**: Server logic handling reactive data processing and plot generation
-   **`utils/`**: Modular utility functions:
    -   `utils-packages.R`: Dependency management
        -   `cran_packages()`, `github_packages()`, `install_if()`, `check_packages_installed()`
    -   `utils-init.R`: Data category definitions and initialization
        -   `init__()` - returns eligibility criteria, demographic categories, and variable descriptions
    -   `utils-etl.R`: Data extraction, transformation, loading (13 functions)
        -   `download_vet_responses()`, `value_labels()`, `recode__()`, `create_variable_metadata()`, `recode_it()`, `cpi_ratio_1999()`, `get_poverty_threshold()`, `clean_mental_health_ace_data()`, `clean_childcare_variables()`
    -   `utils-server.R`: Server-side helpers
        -   `make_retention_table()`, `make_sample_sizes_barcharts()`, `make_geography_plot()`, `make_crosstab_table()`, `mobins2yrs()`
    -   `utils-eligibility.R`: Participant eligibility logic (15 functions)
        -   `passes_cid1()` through `passes_cid9()`, `check_eligibility_authenticity()`, `include_exclude()`, `filter_include_exclude()`, `get_KH_acceptable_zipcodes()`
    -   `utils-ai.R`: AI-powered plot generation
        -   `init_system_msg()`, `anthropic_dynamic_plot()`
    -   `Kidsights_ggtheme.R`: Custom ggplot theme and color schemes
        -   `theme_Kidsights()`, `scale_color_Kidsights_qualitative()`, `scale_fill_Kidsights_qualitative()`

### Data Pipeline

1.  **Authentication**: REDCap API credentials via file upload
2.  **Data Retrieval**: `download_vet_responses()` fetches survey data
3.  **Processing**: Eligibility filtering and variable recoding
4.  **Visualization**: Demographic stratification and geographic mapping

## Features

### Sampling Monitoring

-   **Demographics**: Education, race/ethnicity, income stratification
-   **Geography**: County-level recruitment mapping
-   **Retention**: Participant flow tracking
-   **Quality Control**: Data completeness monitoring
-   **Cross-tabulation**: Interactive demographic cross-analysis with age filtering

### AI-Powered Analysis

-   Natural language plot generation using Anthropic Claude
-   Dynamic visualization based on text prompts
-   Automated variable selection and chart creation

### Cross-tabulation Analysis

The Crosstab tab provides interactive demographic analysis:

-   **Variable Selection**: Choose any two variables from Race/Ethnicity, Federal Poverty Level, or Education
-   **Age Filtering**: Filter results by child age (0-5 years) using checkboxes
-   **Marginal Totals**: Automatic calculation of row and column totals
-   **Export Options**: Download filtered cross-tabs in multiple formats
-   **Real-time Updates**: Tables update instantly when selections change

Example usage:
```r
# Cross-tabulation function
make_crosstab_table(
  df = filtered_data, 
  var1 = "raceG", 
  var2 = "fplcat", 
  years_filter = c(0, 1, 2)
)
```

### Export Capabilities

-   Interactive data tables with CSV/Excel export
-   Print-ready visualizations
-   Summary statistics

## Development

### Project Structure

```         
├── ui.R                    # Shiny UI definition
├── server.R                # Shiny server logic
├── utils/                  # Utility functions
│   ├── utils-packages.R    # Package management
│   ├── utils-init.R        # Initialization functions
│   ├── utils-etl.R         # Data processing
│   ├── utils-server.R      # Server helpers
│   ├── utils-eligibility.R # Eligibility criteria
│   ├── utils-ai.R          # AI integration
│   └── Kidsights_ggtheme.R # Custom ggplot theme
├── data/                   # Data files and cache
├── branding/               # UI assets
└── rsconnect/              # Deployment configuration
```

### Key Dependencies

**Core Shiny Stack:** - `shiny`, `shinycssloaders`, `DT`, `shinyWidgets`

**Data Processing:** - `tidyverse`, `REDCapR`, `readxl`, `writexl`

**Visualization:** - `ggplot2`, `ggthemes`, `ggiraph`

**Geospatial:** - `tigris`, `sf`

**AI Integration:** - `ellmer`, `shinychat`

### Development Workflow

1.  **Local Development**:

    ``` r
    # Start development server
    shiny::runApp()
    ```

2.  **Testing**:

    -   Test with sample REDCap data
    -   Verify geographic mapping functionality
    -   Validate AI plotting features
    -   Test cross-tabulation with different variable combinations and age filters

3.  **Deployment**:

    ``` r
    # Deploy to shinyapps.io
    rsconnect::deployApp()
    ```

## API Integration

### REDCap Data Flow

``` r
# Simplified data pipeline
api_data <- download_vet_responses(my_API, codebook)
processed_data <- api_data$data %>%
  include_exclude(dict = api_data$dictionary) %>%
  recode_it(dict = api_data$dictionary)
```

### Data Security

-   API keys stored externally (not in repository)
-   Secure keyring backend for credential management
-   Local file-based authentication

## Deployment

### shinyapps.io

The application is configured for deployment to shinyapps.io:

``` r
rsconnect::deployApp(
  appName = "Kidsights-NE2025",
  account = "marcus-waldman"
)
```

### Local Server

For local deployment:

``` r
shiny::runApp(host = "0.0.0.0", port = 3838)
```

## Contributing

1.  Fork the repository
2.  Create a feature branch
3.  Make changes following R style conventions
4.  Test thoroughly with sample data
5.  Submit a pull request

## License

See LICENSE file for details.

## Author

**Marcus Waldman**\
University of Colorado Anschutz Medical Center [marcus.waldman\@cuanschutz.edu](mailto:marcus.waldman@cuanschutz.edu){.email}
