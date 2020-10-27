---
date: "2020-10-26"
title: Project Proposal
---

## Introduction

Australia has experienced a regular occurrence of bushfires every year which played a major role in defining the country's geographical characteristics that we see now. Due to its high temperatures and low humidity, Australia's climate has become more ideal for widespread bushfires. 

In 2019, it experienced one of the worst bushfire seasons, which increasingly worsened throughout the season to the extent that the situation went out of control. Also known as Black Summer, this particular period in 2019 caused approximately 72,000 square miles of land to be burnt in the process. This led to various detrimental effects on the environment. Some of the significant impacts would be the loss of millions of lives of animals and their habitats as well as worsening air quality in larger cities due to harmful smoke. The hazardous air quality affected Australia's neighbouring countries such as New Zealand, as such impact cannot be strictly retained within borders. 

## Motivation & Objectives

Our project goal is to study the spread of bushfires across all the states in  Australia during the bushfire season, through the use of data points from 1 August 2019 to 30 September 2019 and produce insightful results after implementing appropriate geospatial analysis methods. We hope that the results will be able to assist the relevant authorities in preparing the nation for the annually occurring bushfire seasons in the near future by studying the changes over time. 

We noticed that past researches only covered certain states in Australia and they were more relevant for less intense bushfires. Most of them do not show much in-depth analysis of the Black Summer in 2019 and were mostly focused on the relationship between weather factors and the severity of bushfire without analysing the spread of bushfire over time. Thus, our group felt the need to expand the scope of the research to the whole of Australia while covering the bushfire season that occurred in 2019. 

## Literature review

Geographical pattern analysis of bushfire severity in Southern Australia by University of Tasmania. 
This research paper focuses on the bushfires in Tasmania that occurred in 2013 and how its temporal patterns of fire severity are related to changes in the fire weather. 

![](/posts/Business-Proposal_files/lit_review.JPG)

As shown in the above figure, it is evident that the fire severities pattern is in tandem with that of Forest Fire Danger Index(FFDI), which takes in factors that attribute towards forest fires such as humidity level, rainfall and temperature. 

## Methodology

### Data Wrangling
August to September bushfire dataset (CSV) and October to January bushfire dataset (CSV)

- Convert latitude and longitude variables to geometry points
- Assign CRS to match Australian States dataset (EPSG 4326)
- Check for NA values
- Check for invalid geometry
- Prepare dataset for Spatial Point Pattern analysis: Converting to spatstat’s ppp format & Checking for duplicated points byApply jittering approach to handle duplicated points
Ready to be combined with study area for analysis

Source: https://www.kaggle.com/carlosparadis/fires-from-space-australia-and-new-zeland

Australian States (SHP)
- Check for NA values
- Check for invalid geometry
- Prepare dataset for Spatial Point Pattern analysis: Convert to owin & combine with bushfire data points for analysis

Source: https://www.arcgis.com/home/item.html?id=66e2eac498084e218dee3a8a7f625f5f

### Spatial Point Pattern Analysis
Study of spatial arrangements of bushfire events/data points in Australia.

First-order Analysis:
- Analyses the intensity and spatial density by measuring the distribution of bushfires in the Australia.
- First order effect: Observations vary from place to place due to changes in the underlying property.
- First order properties are described by the intensity of the observations.
- Possible techniques:
  - Density-based:
    - Quadrant analysis
    - Kernel density estimation
  - Distance-based:
    - Nearest Neighbour Index
    
Second-order Analysis:
- Analyses the interaction between bushfire data points to identify any possible relationship. 
- Second order effect: Observations vary from place to place due to interaction effects between observations?.
- Second order properties are described by the relationship between the observations.
- Conduct Complete Spatial Randomness (CSR) test on the bushfire point events to evaluate if there are signs of clustering, dispersion, or random.  
- Possible techniques:
  - G function
  - F function
- Ripley's K function
  - L function
- Complete Spatial Randomness is satisfied when (1) any event has equal probability of being in any location (first order effect) and (2) the location of one event is independent of the location of another event (second order effect) 

### Spatio-temporal Point Pattern Analysis
In addition to looking at the spatial point patterns of the bushfires in Australia, we will be looking at the "time" aspect as well. The bushfires dataset contains daily records of bushfire occurrence over a span of a few months. Having the temporal data could reveal underlying phenomenon of the spread of bushfires over time that might have been overlooked by purely analysing spatial point patterns. We will be exploring the possible methods of "analyzing, simulating and displaying space-time point patterns" through the use of the spacetime package.  
## Application Design Storyboard

![](/posts/Business-Proposal_files/storyboard.JPG)

## Project Scope of Work

![](/posts/Business-Proposal_files/scope.JPG)

## Application System Architecture

![](/posts/Business-Proposal_files/SA.JPG)

## Tools & Packages

![](/posts/Business-Proposal_files/packages.JPG)

## Challenges 

![](/posts/Business-Proposal_files/challenges.JPG)