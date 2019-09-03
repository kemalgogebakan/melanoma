# Microsimulation Melanoma Early Detection

Package includes documentation/codes to model the survival benefit of a melanoma screening and/or treatment intervention in a virtual population  The model posits a simple stage-shift mechanism of screening benefit.

# Model details can be obtained by installing the package for the Breast Cancer Initiative at Fred Hutch:

devtools::install_github('cancerpolicy/bcimodel', build_vignettes=TRUE)

# See the introduction page of the package. 

# To understand the code, start with ui. See the corresponding server page, for construction of the model. simpolicies_melanoma.R under R codes. Simulation is done under simpolicies_mel function. The simulation method of this function is under simpolicies_melanoma.R code. For function map, see function_map file (This function_map is from the package of bcimodel. But I use the same function structure.)


# Data Section:

1. agestructure: Age distribution of U.S. Source: WHO 2000-2025 Standard via SEER 2013
2. ex1: The input data for the example are pre-loaded into the package in an object called ex1, a list with four elements: $pol, $nh, map, and $tx. These are policies, natural history, stage shift map and treatment. For detailed explanation, see introduction page for the Breast Cancer Initiative at Fred Hutch. For the melanoma, I update these elements. You can see in server page of our shiny model. 
3. incidence_mel: Clinical Melanoma Incidence by Age, for Year Of Diagnosis 2005-2009. Source: SEER Stat.
4. othercausemort: Other Cause Mortality rate data by Age, is taken from Human Mortality Database, for Year 2009.

