# Optimal-Experimental-Designs-for-Hypothesis-Testing-with-Multiple-Factors
How to select factor levels in different types of experiments to maximize power 

## Table of Contents

* Full manuscript describing experiment 

* Data generated from JMP Pro

* R script used to analyze data 

## Background

Hello there! My name is Colin Lynch, and I am an animal behavior Ph.D. candidate at Arizona State University. I primarily leverage industrial engineering techniques to design optimal experiments for the study of emergence in complex adaptive systems, and I am looking to use these techniques to aquire a data science internship in R&D. I am specifically interested in reducing research costs by developing methods that minimize sample size while still having a rich enough dataset to perform hypothesis tests and predictive analytics. Here, I explore the relationship between power and experimental design in two different contexts. 1) When a researcher wants to determine which factors have a significant effect on the response variable of interest and will therefore conduct a screening experiment to fit a first-order model. This is a regression model with only the main effects of each factor. 2) When a researcher wants to measure the nonlinear relationship between factors and the response variable and will therefore fit a second order model. This is a regression model with main effects, interactions, and quadratic effects. My goal is to find experimental designs which maximize power in both of these contexts. 

## Methods

* Using JMP Pro, I calculated the power levels for different coefficients 

* I calculate power for the intercept and the main effect for terms first-order models, and these coefficients plus coeffients for interactions and quadratic effects

* I do this for different factor numbers, effect sizes, and sample sizes for each experimental design.

* I calculate power for full factorial, fractional factorial, and definitive screening designs for the first order case. For the second order I case I invetigate Alias, A, I, D optimal, Box-Behnken, orthogonal and rotatable central composite designs. 

## Results 

* First order designs yield the same levels of power on a per-run basis, however they do so for differeing numbers of factors. 

* Definitive screening designs will therefore allow researchers to reject the highest number of factors. 

Fig 3

* Central composite designs yield the highest power levels for second order models, even controlling for sample size.

* Power analyses should be done with designs in mind. Sample size, effect size, alpha and beta levels alone do not determine power. 

Fig 1

## Contact Information

| Contact Method | URL |
| --- | --- |
| Email | cmlynch2@asu.edu |
| LinkedIn | https://www.linkedin.com/in/colinmichaellynch/ |
| Fiverr | https://www.fiverr.com/colinlynch |

## Acknowledgements

I would like to thank my collaborator Dr. Douglas Montgomery for his comments on this manuscript and his insights into DOE. 
