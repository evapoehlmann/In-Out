# In-Out-Public
This repository is in association with my bachelor's thesis investigating individual differences in the articulatory in-out effect. 
We did so by employing a Bayesian modelling approach. 

# Some notes for transparency and readability

Model specifications can be found in the respective STAN files. Note that samples for the positive effects model were obtained using the encompassing prior approach by Klugkist et al. (2005). 

In scripts study1.R through to study15.R, modelling, sampling and computation of Bayes factors was done. Note that here, the computed Bayes factor is log transformed, whereas in the Bachelor's thesis, Bayes factors were reported on their original scale (back-transformed, essentially, see get_preferred_model.R for reference). The same applies to the reported Bayes factors for the sensitivity analyses. 

# References 

Klugkist, I., Kato, B., & Hoijtink, H. (2005). Bayesian model selection using encompassing priors. Statistica Neerlandica, 59 (1), 57–69. 
https://doi.org/10.1111/j.1467-9574.2005.00279.x
