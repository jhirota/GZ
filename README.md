# Investigating the epidemiological and economic effects of a third-party certification policy for restaurants with COVID-19 prevention measures

## About this repository
We publish the data and codes for analyses of the policy effects of the Yamanashi Green Zone Certification System, which was implemented as a countermeasure against COVID-19 infections in Yamanashi Prefecture since July 2020. The effects to be verified are (1) economic effects (changes in sales and the number of customers per restaurant) and (2) infection prevention effects (changes in the number of new cases of COVID-19 infection) resulting from the spread of the Green Zone Certification in restaurants in Yamanashi Prefecture.

## 01_admin
* An outline of the folder structure for 02_bring, 03_build, and 04_analyze.
* Master code (Run.R)

## 02_bring
All raw data used for the analyses is stored in this folder.

* Google_mobility <br>
Human mobility data for each facility
* Pop <br>
Population data in each prefecture
* Vresas <br>
Data on views of restraunt's websites and intra/inter prefectural human mobility 
* Weather <br>
Data on average temperature and rainfall
* Covid_cases <br>
Data on new infection cases in each prefecture
* Tests <br>
Data on COVID-19 tests in each prefecture 
* Stayhome_rate <br>
Data on changes in the amount of human mobility compared to normal human mobility
* Dummy_vars <br>
Data on dummy variables of gathering restriction and school closure


## 03_build
This folder processes the raw data and makes them available for analyses in 04_analyze.

* GZlist <br>
Number of GZ-certified places by city in Yamanashi Prefecture, and by facility

* GZ_covid <br>
Dataset on number of the GZ-certified restaurants and new infections cases by date with main control variables

* Controls <br>
Cleaned data on number of COVID-19 tests, population, and infectious mobility from 47 prefectures in Japan into prefectures concerned

* weekSIR <br>
Dataset combined with 3 built data above by week

* Postas <br>
Dataset for analyzing economic effects (sales and number of customers) <br>
Dataset for analyzing prevention effects (merged with weekSIR dataset)

* Counterfactual <br>
Dataset to create the non-intervention scenario plots

* Robust_check <br>
Dataset to implement robustness check (stayhome-rate, mobility, restraunts' views, school closure and gathering restriction)


## 04_analyze
This folder performs regression analyses and data visualization by using the built data in 03_build.

* GZlist <br>
Visualizing the cumulative number of the GZ-certified restaurants

* Vresas <br>
Performing regression analyses on restraunt information views online and human mobility

* Google Mobility <br>
Performing regression analyses on human mobility in each facility

* Counterfactual <br>
Visualizing the non-intervention scenario in terms of economic effects and infection prevention effects

* Postas <br>
Performing regression analyses on sales and the number of customers per restaurant

* Covid <br>
Performing regression analyses on the new COVID-19 cases

* Stayhome_rate <br>
Performing regression analyses on the stay-home rate

* Summary_stat <br>
Creating a table of summary statistics of key variables

## 05_report
This folder creates Supplementary Information document.

