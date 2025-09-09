# Fire Incident Counts Forecasting for London Boroughs

A spatio-temporal analysis project developed as part of the "Spatial-temporal Data Analysis and Data Mining" module at UCL. This project looks at forecasting fire incident patterns across London boroughs using ARIMA and STARIMA time-series models to see how the two models perform. This project features geospatial analysis with temporal focus, statistical modelling, and data visualisation techniques (e.g. graphs, maps) that were applied for urban emergency fire service planning.

## Project Overview

Fire incidents cause over 300,000 deaths globally per year, making accurate forecasting crucial for emergency resource allocation. This analysis uses London Fire Brigade data (2009-2017) to identify spatio-temporal patterns and compare the effectiveness of traditional ARIMA models versus spatial extension STARIMA models for fire incident prediction.

### Key Findings

- ARIMA models successfully captured seasonal patterns and overall trends
- STARIMA models showed limited improvement due to weak spatial autocorrelation in London fire data
- Tower Hamlets (high-risk area) showed better model responsiveness than Kingston-upon-Thames (low-risk area)
- Clear seasonal trends with summer peaks and winter lows across all boroughs

### Datasets

Due to the large dataset file of the "London Fire Brigade Records", to complete the work you will need to download the datasets and add the files for the specific datasets into the `data` folder, where:

"London Fire Brigade Incident Records" files are stored in `data/london-fire-incident`
and
"London Boroughs" are stored in `data/london-borough-shp`

The datasets can be found using these links:

- London Fire Brigade Incident Records (<https://data.london.gov.uk/dataset/london-fire-brigade-incident-records/>) - download `LFB Incident Data from 2009 to 2017.csv` csv file (<https://data.london.gov.uk/download/e26c8de5-3cdc-40e7-af62-1cabefa00038/73728cf4-b70e-48e2-9b97-4e4341a2110d/LFB%20Incident%20data%20from%202009%20-%202017.csv>)
  - Can also download supporting `Metadata.xlsx` for further understanding of dataset features
- London Boroughs (<https://data.london.gov.uk/dataset/london_boroughs/>) - download `London Boroughs.gpkg` geopackage file (<https://data.london.gov.uk/download/2e1659aa-4384-474c-a154-947b61f3747c/9502cdec-5df0-46e3-8aa1-2b5c5233a31f/London_Boroughs.gpkg>)

## Project Structure

```bash
london-fire-incident-forecasting/
│   README.md
│   requirements.txt
│
├───data
│   │   starima_package.R
│   │
│   ├───london-borough-shp
│   │       London_Boroughs.gpkg
│   │
│   └───london-fire-incident
│           LFB Incident data from 2009 - 2017.csv
│           Metadata.xlsx
│
├───docs
│       Fire Incident Counts Forecasting Model for London Boroughs.pdf
│
├───results
│
└───src
        fire_analysis.R
```
