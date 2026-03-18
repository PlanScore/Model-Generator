library(tidyverse)
library(dplyr)
library(plyr)
library(brms)
library(parallel)

##NOTE: The 2024 presidential vote by district was not available for state##
##legislatures at the time of these model runs, so the 2020 presidential  ##
##vote was repeated for 2022 and 2024 in the data for estimation. Prior   ##
##to 2022 the model uses the average presidential vote in each district   ##
##over the entire redistricting cycle. Variations in intercepts and slopes##
##over time are captured with separate random intercepts and coefficients ##
##for each election cycle.                                                ##

output.folder <- "/vol/" #DESIGNATE A LOCATION TO SAVE OUTPUT

######################
##STATE LEGISLATURES##
######################

##STATE LEG: LOAD AND FORMAT DATA##
#state legislative outcome data, lower house
setwd("/vol/")
d.lower <- read_csv("statehouse_elections_imputations_2026.csv")[,-1] %>%
  filter(cycle>=2011) %>%
  mutate(winner_dem=party_winner=="d",
         dpres=pres_dem_prop)
#state legislative outcome data, upper house
d.upper <- read_csv("statesenate_elections_imputations_2026.csv")[,-1] %>%
  filter(cycle>=2011) %>%
  mutate(winner_dem=party_winner=="d",
         dpres=pres_dem_prop)

d.leg <- rbind.fill(d.lower, d.upper) %>%
  mutate(dpres_mn=dpres-mean(dpres, na.rm=TRUE)) #mean-deviate pres vote for easier calculations; mean(dpres)=0.4949748

setwd(output.folder)
d.leg <- sample_frac(d.leg)
n <- 10
nr <- nrow(d.leg)
d.leg$grp <- c(1:n) * rep(1, nr)
write_csv(d.leg, "leg_2011_2024_crossval_cycles.csv") #set up the file for cross-validation

##STATE LEG: FULL MODEL WITH INCUMBENCY##
##This model is engaged on PlanScore when users choose to identify incumbency##
##values. (Only 2020 presidential vote data are available for state          ##
##legislatures in the latest redistricting cycle.)                           ##

setwd(output.folder)
d.leg <- read_csv("leg_2011_2024_crossval_cycles.csv") %>%
  filter(!is.na(incumb))
start <- proc.time()

#SAMPLE OUT OF THE FILE#
d.leg <- sample_frac(d.leg, size=0.1) #sampling out 10%

m <- brm(bf(dem_share_imputed ~ dpres_mn + incumb +
              (1 + dpres_mn + incumb || stateabrev) +
              (1 + dpres_mn + incumb | cycle)), 
         data=d.leg, 
         prior=c(set_prior("normal(0.78,0.2)",class="b",coef="dpres_mn"),
                 set_prior("normal(0.05,0.05)",class="b",coef="incumb"),
                 set_prior("normal(0.49,0.1)",class="Intercept"),
                 set_prior("student_t(3, 0.03, 0.1)", class="sd",coef="Intercept",
                           group="cycle"),
                 set_prior("student_t(3, 0.14, 0.1)", class="sd",coef="dpres_mn",
                           group="cycle"),
                 set_prior("student_t(3, 0.02, 0.1)", class="sd",coef="incumb",
                           group="cycle"),
                 set_prior("student_t(3, 0.02, 0.1)", class="sd",coef="Intercept",
                           group="stateabrev"),
                 set_prior("student_t(3, 0.11, 0.1)", class="sd",coef="dpres_mn",
                           group="stateabrev"),
                 set_prior("student_t(3, 0.02, 0.05)", class="sd",coef="incumb",
                           group="stateabrev")),
         cores=detectCores(), chains=4, control=list(adapt_delta=0.99999, max_treedepth=12),
         warmup=20, iter=60, refresh=10, thin=16) #warmup and iter for number of cycles
proc.time() - start
saveRDS(m, "full_model_2025A_incumbency_stateleg.rds")

##generating coefficient matrix of posterior samples for PlanScore##
setwd(output.folder)
m <- readRDS("full_model_2025A_incumbency_stateleg.rds")

#population level effects#
cycle.select <- function(i, mat) {
  yr <- sample(seq(2012,2024,by=2), 1)
  rows <- rownames(mat)
  mat[rows[str_detect(rows, as.character(yr))],i]
}

cycles <- paste0("cycle\\[", rownames(ranef(m)$cycles)) #selector for cycle random effects
states <- paste0("stateabrev\\[", rownames(ranef(m)$stateabrev)) #selector for state random effects
C <- t(posterior_samples(m, pars=c("^b",states,cycles), as.matrix=T)) #coefficient matrix

sigma <- VarCorr(m, summary=F)$residual__$sd
nsims <- dim(C)[2]
ndists <- 500

E <- sapply(1:nsims, function(i) rnorm(ndists, 0, sd=sigma)) #unexplained error matrix

write.csv(C, "C_matrix_full-2025A-incumbency-stateleg.csv")
write.csv(E, "E_matrix_full-2025A-incumbency-stateleg.csv")

##STATE LEG: SMALLER MODEL WITH PRES VOTE ONLY##
##This model is engaged on PlanScore when users choose to leave all seats open.##
##(Only 2020 presidential vote data are available for state legislatures in the##
##latest redistricting cycle.)                                                 ##
setwd(output.folder)
d.leg <- read_csv("leg_2011_2024_crossval_cycles.csv") %>%
  filter(!is.na(incumb))

#SAMPLE OUT OF THE FILE#
d.leg <- sample_frac(d.leg, size=0.1) #sampling out 10%

start <- proc.time()
m <- brm(bf(dem_share_imputed ~ dpres_mn +
              (1 + dpres_mn || stateabrev) +
              (1 + dpres_mn | cycle)), 
         data=d.leg, 
         prior=c(set_prior("normal(0.9,0.2)",class="b",coef="dpres_mn"),
                 set_prior("normal(0.5,0.1)",class="Intercept"),
                 set_prior("student_t(3, 0.04, 0.1)", class="sd",coef="Intercept",
                           group="cycle"),
                 set_prior("student_t(3, 0.14, 0.1)", class="sd",coef="dpres_mn",
                           group="cycle"),
                 set_prior("student_t(3, 0.03, 0.1)", class="sd",coef="Intercept",
                           group="stateabrev"),
                 set_prior("student_t(3, 0.11, 0.1)", class="sd",coef="dpres_mn",
                           group="stateabrev")),
         cores=detectCores(), chains=4, control=list(adapt_delta=0.99999, max_treedepth=12),
         warmup=20, iter=60, refresh=10, thin=16) #warmup and iter for number of cycles
proc.time() - start
saveRDS(m, "full_model_2025A_openseat_stateleg.rds")

##generating coefficient matrix of posterior samples for PlanScore##
setwd(output.folder)
m <- readRDS("full_model_2025A_openseat_stateleg.rds")

#population level effects#
cycle.select <- function(i, mat) {
  yr <- sample(seq(2012,2024,by=2), 1)
  rows <- rownames(mat)
  mat[rows[str_detect(rows, as.character(yr))],i]
}

cycles <- paste0("cycle\\[", rownames(ranef(m)$cycles)) #selector for cycle random effects
states <- paste0("stateabrev\\[", rownames(ranef(m)$stateabrev)) #selector for state random effects
C <- t(posterior_samples(m, pars=c("^b",states,cycles), as.matrix=T)) #creating C matrix

sigma <- VarCorr(m, summary=F)$residual__$sd
nsims <- dim(C)[2]
ndists <- 500

E <- sapply(1:nsims, function(i) rnorm(ndists, 0, sd=sigma)) #unexplained error matrix

write.csv(C, "C_matrix_full-2025A-openseat-stateleg.csv")
write.csv(E, "E_matrix_full-2025A-openseat-stateleg.csv")

