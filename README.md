# Data Mining Project Template

This repository contains the code for a small data mining project developed as part of the course:

**Data Access and Data Mining for Social Sciences**

University of Lucerne

Student Name  
Course: Data Mining for the Social Sciences using R
Term: Spring 2026

## Project Goal

The goal of this project is to collect and analyze data from an online source (API or web scraping) in order to answer a research question relevant to political or social science.

The project should demonstrate:

- Identification of a suitable data source
- Automated data collection (API or scraping)
- Data cleaning and preparation
- Reproducible analysis

This project investigates how climate change affects the economic viability of Swiss ski resorts. The central goal is to identify which resorts are most vulnerable to declining snow reliability and to assess how well climate, geographic, infrastructural, and tourism data can predict future decline in winter tourism performance.

Rather than treating “closure” as a vague concept, the project defines measurable outcomes that capture economic stress and vulnerability. These outcomes are then modeled using a combination of statistical and machine-learning methods.

## Research Question

Which Swiss ski resorts are most vulnerable to declining snow reliability, and how accurately can climate and tourism data predict future economic decline in winter tourism?

## Empirical focus and preliminary hypotesis

The project does not directly observe actual ski resort closures for all resorts. Instead, it operationalizes vulnerability through predictive outcomes that can be measured from available data.

Preliminary hypothesis: 
1- resorts at lower altitude are more likely to experience declining snow reliability
2- declining snow reliability reduces winter overnight stays
3- infrastructure and accessibility moderate this relationship

## Proposed Pipeline 

The workflow of the project is as follows:

1. Data collection
extract Swiss ski resort locations and infrastructure from OpenStreetMap / OpenSkiMap
collect climate and snow data for each resort from Open-Meteo
collect tourism statistics from the FSO

2. Data integration
harmonize identifiers and spatial units
link resorts to municipalities or tourism destinations
create a combined yearly panel dataset

3. Building trend/measures/index
construct climate trends
construct snow reliability measures
construct tourism growth indicators
build geographic accessibility and infrastructure indicators

4. Outcome definition
define binary decline outcome
define continuous tourism outcome

5. Modelling
estimate baseline regression models
compare predictive performance

6. Evaluation
use train/test split or cross-validation
evaluate out-of-sample prediction
identify which resorts are predicted to be most vulnerable

7. Interpretation
rank resorts by predicted vulnerability
examine which predictors contribute most to the predictions
discuss implications for adaptation and regional inequality

## Current limitations

 The main challenges are:

Get more accurate data/better handling/cleaning
defining the most appropriate tourism outcome
matching resorts to tourism statistics consistently
constructing reliable winter-specific tourism indicators
dealing with missingness and uneven data coverage across resorts



## Data Source

- API:[ https://example-api.com](https://open-meteo.com/)
- Documentation: [https://example-api.com/docs](https://open-meteo.com/en/docs)
- Access method: api request by From r and the openmeteo package

- API
- Documentation:
- Access method:

- API:
- Documentation:
- Access Method:


## Repository Structure

/code     scripts used to collect/process data
/data     output datasets (not tracked/pushed by git)
README.md   project description


## Reproducibility

To reproduce this project:

1. Clone the repository
2. Install required R packages
3. Run the scripts in the `code/` folder (i tried to be clean having scripts clearly used for api/sraping and one with all the main analysis/plotting)

All data should be generated automatically by the scripts.


## Good Practices

Please follow these guidelines:

- Do **not upload raw datasets** to GitHub.
- Store **API keys outside the repository** (e.g., environment variables).
- Write scripts that run **from start to finish**.
- Commit your work **frequently**.
- Use **clear commit messages**.

Example commit messages:
added API request
cleaned dataset structure
added visualization
fixed JSON parsing


## Notes

Large datasets should not be pushed to GitHub.  
If necessary, provide instructions for downloading the data instead.
