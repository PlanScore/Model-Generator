26 February 2026

  





  









## Methodology: Models with 2024 Presidential Vote

This is the model we use when we have the latest geographic election
data for a particular state. When we do not have such data, we use a
slightly different model described in README_model_no_2024_pvote.md. At
present we only have fully updated data for Congress, so only
congressional results are reported on this methods page. See the
companion page for state legislative results, and for the results of a
modified version of the congressional model for states that do not have
fully updated data.

To run this model for Congress, use the code in
planscore_models_congress.R in the section headed “CONGRESS: 2024
PRESIDENTIAL VOTE AVAILABLE.” The code asks the user at the top of the
file to designate a pathname for saving output. The code otherwise
automatically draws the necessary data from this github page.

### The Big Picture

We use the correlation between the presidential vote on the one hand and
state legislative or congressional votes on the other to predict how new
districts will likely vote and so how biased a plan will be. Our
correlations come from the last 14 years of elections and are estimated
separately for state legislatures and Congress. They factor in how much
each state’s and election year’s results might differ from others
and—where appropriate—any extra advantage incumbents might have. We also
allow our predictions to be imperfect by quantifying how much our method
missed the actual outcomes of past elections, including the degree to
which partisan tides have changed party performance from one election to
the next. This enables us to generate the most accurate, data-driven,
and transparent prediction we can.

### The Details

We use a Bayesian hierarchical model of district-level election returns,
run on either state legislatures or congressional delegations (depending
on the outcome of interest), for the elections from 2011 through 2024.
Formally, the model is:

$$y_i \sim \mathcal{N}(\boldsymbol{X_i\beta} + \boldsymbol{X_i\beta_{s(i)}} + \boldsymbol{X_i\beta_{c(i)}}, \sigma^2_y)$$

$$\begin{pmatrix}
  \beta_{0s} \\
  \vdots \\
  \beta_{ks}
\end{pmatrix}
\sim
\mathcal{N} \begin{pmatrix}
\begin{pmatrix} 
  0 \\ 
  \vdots \\
  0
\end{pmatrix}
,
\begin{pmatrix} 
  \sigma_{\beta_{0s}}^2 \\ 
  \vdots \\ 
  \sigma_{\beta_{ks}}^2
\end{pmatrix}
\end{pmatrix}$$

$$\begin{pmatrix}
  \beta_{0c} \\
  \vdots \\
  \beta_{kc}
\end{pmatrix}
\sim
\mathcal{N} \begin{pmatrix}
\begin{pmatrix} 
  0 \\ 
  \vdots \\
  0
\end{pmatrix}
,
\begin{pmatrix} 
  \sigma_{\beta_{0c}}^2 & \ldots & \rho\sigma_{\beta_{0c}}\sigma_{\beta_{kc}} \\ 
  \vdots & \ddots & \vdots  \\ 
  \rho\sigma_{\beta_{0c}}\sigma_{\beta_{kc}} & \ldots & \sigma_{\beta_{2c}}^2
\end{pmatrix}
\end{pmatrix}$$

where

- $i$ indexes district level elections

- $s$ indexes states, with $s(i)$ denoting the state of district
  election $i$

- $c$ indexes election cycles, with $c(i)$ denoting the election cycle
  of district election $i$

- $k \in [1,2]$ indexes covariates, with $0$ identifying intercepts

- $y_i$ is the Democratic share of the two-party vote in district
  election $i$

- $\boldsymbol{X_i}$ is a matrix of covariate values for district
  election $i$

- $\boldsymbol{\beta}$ is a matrix of the population-level intercept and
  the slopes corresponding to covariates $\boldsymbol{X}$

- $\boldsymbol{\beta_{s(i)}}$ and $\boldsymbol{\beta_{c(i)}}$ are
  matrices of coefficients for the state and election cycle,
  respectively, of district election $i$

- $\sigma_y$ is the residual population-level error term

The model allows the slope for all our covariates—as well as the
corresponding intercept—to vary across both states and election cycles.
Based on exploration of different model specifications, we allow for
correlated random effects across cycles but assume no such correlation
across states to facilitate convergence.

We run separate models for state legislative and congressional outcomes
and with and without incumbency as a covariate. PlanScore identifies a
plan as state legislative or congressional based on the number of seats
in the plan and the state for which it is submitted.

$k$ ranges between $1$ and $2$: if a user designates incumbency for any
seat in a plan, predictions come from the model that includes both
presidential vote and incumbency as covariates; if all seats are left
open, predictions come from a model with only presidential vote.
Presidential vote is the two-party district-level Democratic
presidential vote share, centered around its global mean ($0.515$),
while incumbency status in district election $i$ is coded -1 for
Republican, 0 for open, and 1 for Democratic. We do not have the 2020
presidential vote for estimating new plans in two states—Kentucky and
South Dakota—so we used the 2016 presidential vote in the model for
those states. In the small number of remaining state-cycle combinations
that were missing presidential vote we used the presidential vote for
the same district in the next presidential election (or the previous
presidential election where the next one was not available).

When generating predictions, PlanScore draws 1000 samples from the
posterior distribution of model parameters, and uses them to calculate
means and probabilities. We also add in the offsets for the 2024
presidential election cycle, and then also add in samples from the
covariance matrix of cycle random effects to allow the uncertainty of
predicting for an unknown election cycle to propagate into our
predictions. This has the effect of predicting for an election like 2024
in most respects, but with error bounds that encompass the full range of
partisan tides that occurred over the last decade.

Full results for our four separate models can be found below.

### Congress prediction model with incumbency ($k=2$)

| Term | Estimate | 95% Credible Interval |
|:---|---:|---:|
| **POPULATION-LEVEL** |  |  |
| Intercept ($\beta_0$) | 0.51 | \[0.49, 0.54\] |
| Presidential vote ($\beta_1$) | 0.87 | \[0.80, 0.93\] |
| Incumbency ($\beta_2$) | 0.04 | \[0.02, 0.05\] |
| **STATE-LEVEL** *Standard Deviations* |  |  |
| Intercept ($\sigma_{\beta_{0s}}$) | 0.01 | \[0.01, 0.01\] |
| Presidential vote ($\sigma_{\beta_{1s}}$) | 0.08 | \[0.06, 0.11\] |
| Incumbency ($\sigma_{\beta_{2s}}$) | 0.01 | \[0.01, 0.01\] |
| **CYCLE-LEVEL** *Standard Deviations* |  |  |
| Intercept ($\sigma_{\beta_{0c}}$) | 0.03 | \[0.01, 0.06\] |
| Presidential vote ($\sigma_{\beta_{1c}}$) | 0.07 | \[0.03, 0.14\] |
| Incumbency ($\sigma_{\beta_{2c}}$) | 0.02 | \[0.01, 0.03\] |
| **CYCLE-LEVEL** *Correlations* |  |  |
| Intercept – Pres. vote ($\rho\,\sigma_{\beta_{0c}}\sigma_{\beta_{1c}}$) | -0.09 | \[-0.72, 0.65\] |
| Intercept – Incumbency ($\rho\,\sigma_{\beta_{0c}}\sigma_{\beta_{2c}}$) | -0.37 | \[-0.87, 0.34\] |
| Pres. vote – Incumbency ($\rho\,\sigma_{\beta_{1c}}\sigma_{\beta_{2c}}$) | -0.59 | \[-0.94, 0.16\] |

### Congress prediction model without incumbency ($k=1$)

| Term | Estimate | 95% Credible Interval |
|:---|---:|---:|
| **POPULATION-LEVEL** |  |  |
| Intercept ($\beta_0$) | 0.51 | \[0.49, 0.53\] |
| Presidential vote ($\beta_1$) | 1.04 | \[0.99, 1.09\] |
| **STATE-LEVEL** *Standard Deviations* |  |  |
| Intercept ($\sigma_{\beta_{0s}}$) | 0.02 | \[0.01, 0.02\] |
| Presidential vote ($\sigma_{\beta_{1s}}$) | 0.08 | \[0.06, 0.11\] |
| **CYCLE-LEVEL** *Standard Deviations* |  |  |
| Intercept ($\sigma_{\beta_{0c}}$) | 0.03 | \[0.01, 0.05\] |
| Presidential vote ($\sigma_{\beta_{1c}}$) | 0.05 | \[0.03, 0.11\] |
| **CYCLE-LEVEL** *Correlations* |  |  |
| Intercept – Pres. vote ($\rho\,\sigma_{\beta_{0c}}\sigma_{\beta_{1c}}$) | -0.55 | \[-0.95, 0.29\] |

> Note: Model estimated in brms for R. Model based on 4 MCMC chains run
> for 6000 iterations each with a 2000 iteration warm-up. All model
> parameters converged well with $\hat{R}\le 1.01$.
