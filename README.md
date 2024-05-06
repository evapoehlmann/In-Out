# In-Out-Public
This repository is in association with my bachelor's thesis investigating individual differences in the articulatory in-out effect. 
We did so by employing a Bayesian modelling approach. 

# Some notes on readability and what this code does

Model specifications can be found in the respective STAN files (one_model.stan and u_model.stan for the main analysis). An overview can also be found in the model specification.qmd-file. 

In scripts study1.R through to study15.R, modelling, sampling and computation of Bayes factors was done. Note that here, the computed Bayes factor is log transformed, whereas in the Bachelor's thesis, Bayes factors were reported on their original scale (back-transformed, essentially, see get_preferred_model.R for reference). The same applies to the reported Bayes factors for the sensitivity analyses. 
Note that samples for the positive effects model were obtained using the encompassing prior approach by Klugkist et al. (2005). 

In my modelling workflow, I always started with the study1.R, as this initialized a "bayes_factors2.csv" file in which the Bayes factors are stored and which is loaded as a dataframe in the other modelling scripts. If you want to change this workflow (start with study3.R for instance), you can simply follow the approach at the beginning of study1.R to make the code run. 


The script "get_preferred_model.R" contains code for computation of the Bayes factor between the preferred model and the other two models, respectively. It produces the "preferred_model_table.csv" file for the original analyses and analogue files for each prior setting (see folder "prior_sensitivity_analysis")

The script "combined_analyses1314.R" is a modelling script that follows the exact same approach as the other modelling scripts. study13 and study14, whose data sets come from the same study and follow the same methodology, were simply combined for sample size reasons. This further analysis was done for exploratory reasons and built an argumentation block in the discussion.

The "plots_tables" folder contains code for the generation of plots (plots for sensitivity analysis (in folder "prior_sensitivity") and the model-prediction plot (in folder "prediction_plot"), the plots themselves (including the estimation plots) and tables in LaTeX. 

The "prior_sensitivity_analysis" folder contains the analyses for both prior settings ("high_setting" and "low_setting") used in the sensitivity analyses: A STAN file for the unconstrained model with the respective prior specifications, modelling scripts and Bayes factor tables, respectively.

The "descriptive_analyses" folder contains code for descriptive analyses, data management and a overview of the descriptive results in a csv-file.


# If you want to use this code

If you clone this repo, I would recommend setting up a new R-project from this github repository and set it as the working directory. All scripts will work fine in regards to file paths if you do so.

# References 

Klugkist, I., Kato, B., & Hoijtink, H. (2005). Bayesian model selection using encompassing priors. Statistica Neerlandica, 59 (1), 57–69. 
https://doi.org/10.1111/j.1467-9574.2005.00279.x
