# Optimal-Experimental-Designs-for-Hypothesis-Testing-with-Multiple-Factors
How to select factor levels in different types of experiments to maximize power 

## Table of Contents

* [Full manuscript describing experiment](https://github.com/colinmichaellynch/Optimal-Experimental-Designs-for-Hypothesis-Testing-with-Multiple-Factors/blob/main/Optimal%20Experimental%20Design%20for%20Hypothesis%20Testing%20with%20Multiple%20Factors.docx)

* Data generated from JMP Pro ([first order designs](https://github.com/colinmichaellynch/Optimal-Experimental-Designs-for-Hypothesis-Testing-with-Multiple-Factors/blob/main/PowerSheet2.csv), [second order designs](https://github.com/colinmichaellynch/Optimal-Experimental-Designs-for-Hypothesis-Testing-with-Multiple-Factors/blob/main/PowerSheet1.csv))

* [R script used to analyze data](https://github.com/colinmichaellynch/Optimal-Experimental-Designs-for-Hypothesis-Testing-with-Multiple-Factors/blob/main/PowerOptimality.R)

## Background

A common assumption of power analyses is that power only depends on the effect size, the selected alpha level, and sample size. However, when there are multiple factors, then the design of the experiment also influences power. That is, the way the factor levels are set in samples can strongly influence an experimenter's ability to test hypotheses. Here, I explore the relationship between power and experimental design in two different contexts. 1) When a researcher wants to determine which factors have a significant effect on the response variable of interest and will therefore conduct a screening experiment to fit a first-order model. This is a regression model with only the main effects of each factor. 2) When a researcher wants to measure the nonlinear relationship between factors and the response variable and will therefore fit a second order model. This is a regression model with main effects, interactions, and quadratic effects. My goal is to find experimental designs which maximize power in both of these contexts. 

## Methods

* Using JMP Pro, I calculated the power levels for different coefficients 

* I calculate power for the intercept and the main effect for terms first-order models, and these coefficients plus coeffients for interactions and quadratic effects

* I do this for different factor numbers, effect sizes, and sample sizes for each experimental design.

* I calculate power for full factorial, fractional factorial, and definitive screening designs for the first order case. For the second order I case I invetigate Alias, A, I, D optimal, Box-Behnken, orthogonal and rotatable central composite designs. 

## Results 

* First order designs yield the same levels of power on a per-run basis, however they do so for differeing numbers of factors. 

* Definitive screening designs will therefore allow researchers to reject the highest number of factors. 

<p align="center">
  <img src=/Images/Rplot.png>
</p>

* Central composite designs yield the highest power levels for second order models, even controlling for sample size.

* Power analyses should be done with designs in mind. Sample size, effect size, alpha and beta levels alone do not determine power. 

<p align="center">
  <img src=/Images/powerDesigns.png>
</p>

## Acknowledgements

I would like to thank my collaborator Dr. Douglas Montgomery for his comments on this manuscript and his insights into DOE. 
