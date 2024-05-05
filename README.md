# In-Out-Public
This repository is in association with my bachelor's thesis investigating individual differences in the articulatory in-out effect. 
We did so by employing a Bayesian modelling approach. 

# Some notes for transparency and readability

Model specifications can be found in the respective STAN files and in the model specification.qmd-file. 

In scripts study1.R through to study15.R, modelling, sampling and computation of Bayes factors was done. Note that here, the computed Bayes factor is log transformed, whereas in the Bachelor's thesis, Bayes factors were reported on their original scale (back-transformed, essentially, see get_preferred_model.R for reference). The same applies to the reported Bayes factors for the sensitivity analyses. 


In my modelling workflow, I always started with the study1.R, as this initialized a "bayes_factors2.csv" file in which the Bayes factors are stored and which is loaded as a dataframe in the other modelling scripts. If you want to change this workflow (start with study3.R for instance), you can simply follow the approach at the beginning of study1.R to make the code run. 
Note that samples for the positive effects model were obtained using the encompassing prior approach by Klugkist et al. (2005). 

The script "get_preferred_model.R" contains code for computation of the Bayes factor between the preferred model and the other two models, respectively. It produces the "preferred_model_table.csv" file for the original analyses and analogue files for each prior setting (see folder "prior_sensitivity_analysis"")

The script "combined_analyses1314.R" is a modelling script that follows the exact same approach as the other modelling scripts. study13 and study14 were simply combined for sample size reasons. This further analysis was done for exploratory reasons and built an argumentation block in the discussion. 

The "plots_tables" folder contains code for the generation of plots (estimation plots, plots for sensitivity analysis and the model-prediction plot) and tables in LaTeX. 

The "prior_sensitivity_analysis" folder contains the analyses for both prior settings used in the sensitivity analyses, model specifications, modelling scripts and Bayes factor tables, respectively.

The "descriptive_analyses" folder contains code for descriptive analyses, data management and a overview of the descriptive results in a csv-file.


# References 

Klugkist, I., Kato, B., & Hoijtink, H. (2005). Bayesian model selection using encompassing priors. Statistica Neerlandica, 59 (1), 57–69. 
https://doi.org/10.1111/j.1467-9574.2005.00279.x
