# K-Means-Clustering
Overview
This repository contains the analysis code and documentation for a research project investigating physician Electronic Health Record (EHR) usage patterns using K-means clustering. The study analyzes various metrics including in-basket burden, after-hours usage, and response times to understand physician workflow efficiency and potential burnout factors.


# Requirements
R (version 4.0.0 or higher)
Required R packages:
tidyverse
readxl
effectsize
factoextra
pwr


# Input Data Requirements
The analysis scripts expect input data with the following variables:
Demographic information (specialty, gender, years since graduation)

EHR usage metrics:
Time on unscheduled days
After-hours usage ("pajama time")
Time outside standard hours (7 AM to 7 PM)
In-basket time per day
Message volume metrics
Response turnaround time


# Analysis Components
1. K-means Clustering (ResearchProject.R)
Data preprocessing and scaling
Optimal cluster number determination
Cluster visualization
Outlier detection and handling

2. Full Sample Analysis (FullSampleAnalysisV4.R)
Demographic comparisons
EHR metric summaries
Pre-clustering analysis of sex and specialty differences
Missing data analysis

3. Power Analysis (PowerAnalysisV3.R)
Between-cluster power calculations
Within-cluster comparisons for sex and specialty
Minimum detectable effect size calculations
Bonferroni-corrected significance testing


# Statistical Methods
K-means clustering with scaled variables
Two-sample t-tests for metric comparisons
Chi-square tests for categorical variables
Effect size calculations (Cohen's d, Cramer's V)
Power analysis with Bonferroni correction



License
All rights reserved. This code is shared for research transparency and reproducibility purposes but is not licensed for redistribution or commercial use.

Acknowledgments
This research was conducted at an academic health center with support from the clinical informatics department.
