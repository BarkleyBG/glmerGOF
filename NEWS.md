# glmerGOF 0.1.0

* Initial (Pre-)Release of the glmerGOF package.

The goal of glmerGOF is to provide a goodness of fit test of the presumed Gaussian
distribution of the random effect in logistic mixed models fit with 
`lme4::glmer(family = "binomial")`. The method implemented is introduced in 
Tchetgen Tchetgen and Coull (2006):

Tchetgen Tchetgen, E. J., & Coull, B. A. (2006) _A Diagnostic Test for the Mixing Distribution in a Generalised Linear Mixed Model_. Biometrika, 93(4), 1003-1010. DOI: [10.1093/biomet/93.4.1003](https://doi.org/10.1093/biomet/93.4.1003)
