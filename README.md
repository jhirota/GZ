# Investigating the epidemiological and economic effects of a third-party certification for restaurants and bars with COVID-19 prevention measures

## About this repository
We publish the data　and codes for analyses of the policy effects of the Yamanashi Green Zone Certification System, which was implemented as a countermeasure against new coronavirus infections in Yamanashi Prefecture since July 2020. The effects to be verified are (1) economic effects (increase or decrease in sales and number of customers per restaurant) and (2) infection control effects (increase or decrease in the number of new cases of novel coronavirus infection) resulting from the spread of the Green Zone Certification in restaurants and bars in Yamanashi Prefecture.

### 01_admin
* An outline of the folder structure for 02_bring, 03_build, and 04_analyze.
* Master code (Run.R)

### 02_bring
All raw data used for analyses is stored here.

* Google_mobility
Human mobility data for each facility
* Pop
Population data in each prefecture
* Vresas
Data on views of restraunt's websites and intra/inter prefectural human mobility 
* Weather
Data on average temperature and rainfall
* Covid_cases
Data on new infection cases in each prefecture
* Tests
Data on COVID-19 tests in each prefecture 
* Stayhome_rate
Data on changes in the amount of human mobility compared to normal human mobility
* Dummy_vars
Data on dummy variables of gathering restriction and school closure


### 03_build
This folder process the raw data and make it available for analysis.

* GZlist
Number of GZ-certified places by city in Yamanashi Prefecture, and by facility

* Controls
Cleaned data on number of COVID-19 tests, weather, and infectious mobility from 47 prefectures in Japan into prefectures concerned

* GZ_covid
Dataset on number of GZ-certified restaurants and new infections cases by date

* weekSIR
Dataset combined with 3 built data above by weeek

* Counterfactual
Dataset to create non-intervention scenario plots

* Robust_check
Dataset to implement robustness check (stayhome-rate, mobility, restraunts' views etc.)

### 04_analyze
This folder performs regression analysis and data visualization by using built data.

* GZlist
Visualizing the cumulative number of GZ-certified restaurants
* Vresas
Performing regression analyses on restraunts' views and human mobility
* Google Mobility
Performing regression analyses on human mobility in each facility
* Counterfactual
Visualizing the non-intervention scenario in terms of economic effects and infection prevention effects
* Postas
Performing regression analyses on sales and the number of customers per restaurant
* Covid
Performing regression analyses on the new infection cases
* Stayhome_rate
Performing regression analyses on the stay-home rate
* Summary_stat
Formatting summary statistics of key variables

### 05_report
Reporting Supplementary Information

