# covid-lockdown

This repository contains all analyses and results relating to our investigation of lockdown timing during the first wave of COVID-19 across Europe.

## Table of contents

### **Data/** folder

This folder contains all external data used in the analyses.

The **Data/Unformatted/** sub-folder contains original source files from the following repositories:

1. COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (https://github.com/CSSEGISandData/COVID-19)
    + *csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv*
    + *csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv*
2. Oxford COVID-19 Government Response Tracker (OxCGRT) (https://github.com/OxCGRT/covid-policy-tracker)
    + *data/OxCGRT_latest.csv*
3. World Bank (https://data.worldbank.org, accessed via the the R package 'wbstats')
    + Indicators: *AG.LND.TOTL.K2*, *SP.POP.TOTL*

The **Data/Formatted/** sub-folder contains formatted source files.

### **Script/** folder

This folder contains all R scripts and custom functions used in the analyses.

The following analytical scripts are included:

1. *Data_Processing.R*: Download and format all source data, and process for use in all analyses.
2. *Analysis_Within_Country.R*: Analyse within-country effects of lockdown timing across Europe.
3. *Analysis_Between_Country.R*: Analyse between-country effects of lockdown timing across Europe.
4. *Table_Formatting.R*: Format tables for use in manuscript.
5. *figure_aesthetics.R*: Define aesthetic specifications for figures and tables.

The **Script/Functions/** sub-folder contains all custom functions called by these scripts.

### **Output/** folder

This folder contains all analytical output.

The **Simulations/** sub-folder contains all simulated data produced by the script *Analysis_Within_Country.R*.

The **Figures/** sub-folder contains all figures.

The **Tables/** sub-folder contains formatted tables.

### **packrat/** folder

This folder contains all package management files. 

## Usage

All analyses were conducted in R (v.4.0.3). Opening the RStudio project *covid-lockdown.Rproj* ensures that the working directory is automatically set to the project directory; else, the working directory will have to be manually specified.

All packages are managed via packrat, which stores package dependencies inside the project directory (**./packrat/**) as a private package library. This ensures portability and reproducibility across different computers and platforms. At the beginning of each analytical script, the most recent 'snapshot' of the package library is loaded.

