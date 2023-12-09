Codes to reproduce the results in "Risk Sharing with Limited Commitment and Preference Heterogeneity: Structural Estimation and Testing" by Sarolta Laczó

Most codes run in the open-source software R, see www.r-project.org, and one code runs in Stata. The codes have to be executed in the specified order.

The codes are for village 1 only (Aurepalle). The analysis can be performed for the other villages similarly, by changing the variable vilcode (now equal to 1) appropriately, etc. These codes are for the baseline estimations, i.e., with measurement error in last period's consumption and in current income as well but without unobserved heterogeneity in the curvature of the utility function, as in the main text.

- 'IncHbase1.R' loads the R data file 'allest' and estimates and discretizes the income processes for four groups of households, saves 'allIncH1.'

- 'rPrs1me_nog.R' estimates the perfect risk sharing models.

- 'rHom1IncHsol.R' solves the limited commitment model with homogenous preferences on a grid of parameters.

- 'rHom1incHme.R' estimates the limited commitment model with homogenous preferences.

- 'rHom1IncHsol.R' solves the limited commitment model with heterogenous preferences on a grid of parameters.

- 'rHet1incHme_nog.R' estimates the limited commitment model with heterogenous preferences.

- 'testing1me.R' performs the model selection tests.

- 'preds1me.R' computes the predicted consumption allocation and produces the numbers in Tables 3-4, and computes inequality measures for the data and the four models. Note that the robust standard errors in Table 3 are computed in Stata, see below. (I also provide the R data file Preds1 with all the estimation and prediction results.)

- 'preds_ineq.R' produces the numbers in Table 5 (I provide the R data files Preds2 and Preds3, which are used here).

- 'preds1red.R' simulates the counterfactual tax and transfer policy and produces the numbers in Table 6.

- Finally, the Stata script conschange_preds.do provides the reduced-form results of Section 4.2, as well the results in Table 3.





 