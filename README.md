# MedicalErrors-IncomeGroups

This repository contains the R scripts used for the bibliometric and meta-analysis of medical errors across World Bank income groups.

## Purpose  
This repository provides the code used to analyze the relationship between medical error publications, bibliometric metrics, and health indicators across different income groups. The analyses aim to:  
- Evaluate the volume of research on medical errors in different income groups.  
- Assess associations between bibliometric metrics (publications, citations, H-index) and health indicators.  
- Perform linear regression models to explore these relationships.  
- Conduct a meta-analysis to synthesize results across income groups.  
- Visualize findings through heatmaps.

## Required R packages  
The following R packages are necessary to execute the analyses:  
- **dplyr**  
- **tidyr**  
- **readxl**  
- **pheatmap**  
- **ggplot2**  
- **ggsci**  
- **metafor**  
- **wbstats**  

## Analyses included  
This repository contains scripts that perform the following analyses:  
- **Bibliometric analysis**: Summary of publications, citations, and H-index by income group.  
- **Linear regression models**: Associations between bibliometric metrics and health indicators, stratified by income groups (High, Upper-Middle, Lower-Middle, and Low Income).  
- **Hierarchical clustering and heatmaps**: Identification of patterns in regression coefficients using Euclidean distance and complete linkage clustering.  
- **Meta-analysis**: Random-effects meta-analysis of regression coefficients across income groups using the REML method.

## How to use  
1. Download or clone this repository.  
2. Prepare the dataset (`data.xlsx`) with the format described in the script.  
3. Open the script (`main_analysis.R`) in RStudio.  
4. Install the required R packages (see above).  
5. Run the script to replicate the analyses.  

## Data availability  
The dataset (`data.xlsx`) used in this study is not publicly available but can be provided upon reasonable request. The script is designed to demonstrate the analytical workflow without requiring access to the raw data.  

## License  
This repository is licensed under the **MIT License**. Users are free to use, modify, and distribute the code, provided that appropriate credit is given to the original author.  
