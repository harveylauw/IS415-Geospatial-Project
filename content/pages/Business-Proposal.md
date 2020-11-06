---
date: "2020-10-26"
title: Project Proposal
---

## Introduction

Dengue fever has always been observed in Singapore ever since the first outbreak was reported back in 1901. The number of cases has increased significantly over the past few decades and in 2005, the country experienced the worst health crisis since the SARS epidemic which occurred in year 2003. The mosquito-borne tropical disease has no proper cure nor vaccine yet. 

Ever since Singapore first went into circuit breaker period on 7th April, the number of COVID-19 confirmed cases has been dropping but on contrary, the number of dengue cases spiked up to the extent that it crossed the 10,000 mark in June.
Experts claimed that the surge in dengue cases is an expected result of circuit breaker due to several reasons. Firstly, commercial areas and construction sites have been neglected due to the lack of activities which make perfect spots for Aedes aegypti mosquito to breed. Secondly, with more people staying at home, there is higher risk of dengue transmission in domestic settings. 

As of October 2020, a total of 28 people have passed away from dengue fever this year, which is the highest number of fatalities within the past 15 years. As the majority of the fatalities observed in 2020 occurred in the active dengue clusters, it is essential for us to focus on the identified cluster areas so that better measures can be implemented for these areas. 

## Motivation & Objectives

Our project goal is to study the dengue clusters across Singapore from February 2020 to July 2020, which is inclusive of the circuit breaker period. Our application will allow users to interact within the application to analyse the different geospatial aspects of how dengue spread in Singapore during the time period of February 2020 to July 2020. We hope to produce insightful results of dengue clusters and analyse how weather changes have contributed to the trend in dengue cases as well as its temporal changes over the past few months by implementing appropriate geospatial analysis methods. We hope that the results will be able to assist the relevant authorities in preparing the nation for the more dengue-prone season in the near future.

## Literature review

As recommended by Prof Kam, we studied one of the last year's IS484 projects which explored spatial-temporal analysis of dengue transmission in Taiwan using 20 years' worth of data. 

With the use of 4 functions (Gaussian, Box, Tri and Triweight) of Kernel Density Estimation, the team's application allows users to identify patterns of dengue clusters by adjusting the bandwidth instead of leveraging an adaptive bandwidth. 

In their study, they pointed out that it is equally important to analyse the temporal aspects besides looking into the overall cumulative cases in each region. Thus one of the functions allowed users to explore different kernel functions to animate the dengue outbreak distribution, along with other parameters such as regions, density's sigma and number of bins. 

The team's project also focused on finding out how dengue spread within Taiwan over time.  Exploring their application and the research paper was indeed insightful as their spatio-temporal analysis showed where and when it stopped and how it actually spread. 

![](/posts/Business-Proposal_files/lit_review.JPG)

## Methodology

### Data Wrangling

15 Feb 2020 to 10 July 2020 weekly record of dengue cases dataset (CSV)

- Convert latitude and longitude variables to geometry points
- Transform CRS to EPSG 3414 (SVY 21)
- Check for NA values
- Check for invalid geometry
- Prepare dataset for Spatial Point Pattern analysis: Converting to spatstat’s ppp format & Checking for duplicated points by applying the jittering approach to handle duplicated points
- Ready to be combined with study area for analysis

Source: [Figshare](https://figshare.com/articles/dataset/The_unprecedented_outbreak_of_dengue_in_Singapore_a_dataset_of_spatiotemporal_dengue_cases_covering_the_nationwide_lockdown_period_in_2020/12821153)

Singapore Coastal Outline Without Islands (SHP)
- Transform CRS to EPSG 3414 (SVY 21)
- Check for NA values
- Check for invalid geometry
- Prepare dataset for Spatial Point Pattern analysis: Convert to owin & combine with dengue data points for analysis

Source: Credits to [Professor Kam Tin Seong (SMU SIS)](https://www.smu.edu.sg/faculty/profile/9618/KAM-Tin-Seong) for kindly modifying the dataset to suit our analysis needs

Singapore Temperature & Rainfall (CSV)
- Check for NA values
- Check for invalid geometry
- Transform CRS to EPSG 3414 (SVY 21)
- Prepare dataset for Spatial Point Pattern analysis: 
  - Perform Krigging 
  - Convert data set into Thiessen Polygon format to generate interpolation raster

Source: [Weather Gov SG](http://www.weather.gov.sg/climate-historical-daily/)

### Spatial Point Pattern Analysis

Study of spatial arrangements of dengue events/data points in Singapore.

First-order Analysis:
- Analyses the intensity and spatial density by measuring the distribution of dengue cases in Singapore.
- First order effect: Observations vary from place to place due to changes in the underlying property.
- First order properties are described by the intensity of the observations.
- Possible techniques:
  - Density-based:
    - Quadrant analysis
    - Kernel density estimation
  - Distance-based:
    - Nearest Neighbour Index
    
Second-order Analysis:
- Analyses the interaction between dengue data points to identify any possible relationship. 
- Second order effect: Observations vary from place to place due to interaction effects between observations.
- Second order properties are described by the relationship between the observations.
- Conduct Complete Spatial Randomness (CSR) test on the dengue cases event points to evaluate if there are signs of clustering, dispersion, or randomness.  
- Possible techniques:
  - G function
  - F function
- Ripley's K function
  - L function
- Complete Spatial Randomness is satisfied when (1) any event has equal probability of being in any location (first order effect) and (2) the location of one event is independent of the location of another event (second order effect) 

### Spatio-temporal Point Pattern Analysis

In addition to looking at the spatial point patterns of the dengue cases in Singapore, we will be looking at the "time" aspect as well. The dengue dataset contains weekly records of dengue cases occurrence over a span of a few months. Having the temporal data could reveal underlying phenomenon of the spread of dengue over time, that might have been overlooked by purely analysing spatial point patterns. We will be exploring the possible methods of "analyzing, simulating and displaying space-time point patterns" through the use of the STPP package. The specific analysis methods that we will be applying is ASTIKhat and LISTAhat.

### Geographically weighted regression (GWR)

Perform Geospatial Interpolation to obtain the raster results of Temperature and Rainfall data from Weather.gov.sg which will be used as one of the independent variable for Geographically weighted regression(GWW). GWR is a spatial statistical technique that takes non-stationary variables into consideration (e.g., climate; demographic factors; physical environment characteristics) and models the local relationships between these independent variables and an outcome of interest (also known as dependent variable) which are the number of cases by each subzones in Singapore.

## Application Design Storyboard

![](/posts/Business-Proposal_files/storyboard_dengue.JPG)

## Project Scope of Work

![](/posts/Business-Proposal_files/scope.JPG)

## Application System Architecture

![](/posts/Business-Proposal_files/SA.JPG)

## Tools & Packages

![](/posts/Business-Proposal_files/packages.JPG)

## Challenges 

![](/posts/Business-Proposal_files/challenges.JPG)