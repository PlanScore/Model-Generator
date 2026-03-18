library(tidyverse)
library(dplyr)
library(plyr)
library(brms)
library(parallel)

##NOTE: The 2024 presidential vote by precinct was available for the most recent##
##redistricting cycle for most but not all states. The data were incorporated   ##
##where available but substituted with the 2020 presidential vote where         ##
##unavailable. Most of the consequences of this approach are absorbed into the  ##
##separate intercepts and coefficients for each election cycle that are part of ##
##each multilevel model.                                                        ##

output.folder <- "/vol/" #DESIGNATE A LOCATION TO SAVE OUTPUT

##############################################
##CONGRESS: 2024 PRESIDENTIAL VOTE AVAILABLE##
##############################################

##CONGRESS: YES 2024 PVOTE: LOAD AND FORMAT DATA##

#2024 presidential vote data for the 2022 and 2024 elections#
setwd("https://raw.githubusercontent.com/PlanScore/Model-Generator/refs/heads/migurski/update-model-code-and-data/2025/")
p24 <- read_csv("pres_vote_by_district_2020_2024.csv") %>%
  mutate(cycle=2022,
         dpres24=100*d24/(d24+r24)) %>%
  dplyr::select(cycle, stpost, district, dpres24)
p24 <- rbind.fill(p24,
                  p24 %>% mutate(cycle=2024))

#congressional outcome data#
setwd("https://raw.githubusercontent.com/PlanScore/Model-Generator/refs/heads/migurski/update-model-code-and-data/2025/")
d.cong <- read_csv("congress_elections_imputations_2025.csv")[,-1] %>%
  filter(cycle >= 2012) %>%
  mutate(district=parse_number(str_sub(stcd, -2, -1))) %>%
  left_join(p24, by=c("cycle","stateabrev"="stpost","district")) %>% #add 2024 presidential vote
  mutate(dpres=ifelse(cycle %in% c(2022,2024), dpres24, dpres)) %>%
  group_by(stcd2) %>%
  mutate(
    dpres = dpres / 100,
    dpres16 = dplyr::first(dpres[cycle == 2016], default = NA_real_) #create separate variable of 2016 presidential vote
  ) %>%
  ungroup() %>%
  mutate(stateabrev=tolower(stateabrev)) %>%
  mutate(dpres=ifelse(stateabrev %in% c("ky","sd") & cycle %in% 2020, dpres16, dpres)) %>% #replace missing 2020 presidential vote with 2016
  dplyr::rename(distz=stcd) %>%
  mutate(dpres_mn=dpres-mean(dpres)) #mean-deviate pres vote for easier calculations; mean(dpres)=0.5149009

setwd(output.folder)
d.cong <- sample_frac(d.cong)
n <- 10
nr <- nrow(d.cong)
d.cong$grp <- c(1:n) * rep(1, nr)
write_csv(d.cong, "cong_2011_2024_crossval_cycles_2024data.csv") #set up the file for cross-validation

##CONGRESS: YES 2024 PVOTE: FULL MODEL WITH INCUMBENCY##
##This model is engaged on PlanScore when users choose to identify incumbency##
##values and when a state has 2024 presidential vote data available for      ##
##predictions in the latest redistricting cycle.                             ##

setwd(output.folder)
d.cong <- read_csv("cong_2011_2024_crossval_cycles_2024data.csv") %>%
  filter(!is.na(incumb))

start <- proc.time()
m <- brm(bf(dem_share_imputed ~ dpres_mn + incumb +
              (1 + dpres_mn + incumb || stateabrev) +
              (1 + dpres_mn + incumb | cycle)), 
         data=d.cong, 
         prior=c(set_prior("normal(0.85,0.2)",class="b",coef="dpres_mn"),
                 set_prior("normal(0.05,0.05)",class="b",coef="incumb"),
                 set_prior("normal(0.5,0.1)",class="Intercept"),
                 set_prior("student_t(3, 0.02, 0.1)", class="sd",coef="Intercept",
                           group="cycle"),
                 set_prior("student_t(3, 0.1, 0.1)", class="sd",coef="dpres_mn",
                           group="cycle"),
                 set_prior("student_t(3, 0.02, 0.1)", class="sd",coef="incumb",
                           group="cycle"),
                 set_prior("student_t(3, 0.02, 0.1)", class="sd",coef="Intercept",
                           group="stateabrev"),
                 set_prior("student_t(3, 0.1, 0.1)", class="sd",coef="dpres_mn",
                           group="stateabrev"),
                 set_prior("student_t(3, 0.02, 0.05)", class="sd",coef="incumb",
                           group="stateabrev")),
         cores=detectCores(), chains=4, control=list(adapt_delta=0.99999, max_treedepth=12),
         warmup=2000, iter=6000, refresh=10, thin=16)
proc.time() - start
saveRDS(m, "full_model_2025B_incumbency_congress.rds")

##generating coefficient matrix of posterior samples for PlanScore##
setwd(output.folder)
m <- readRDS("full_model_2025B_incumbency_congress.rds")

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

write.csv(round(C, 4), "C_matrix_full-2025B-incumbency-congress.csv")
write.csv(round(E, 4), "E_matrix_full-2025B-incumbency-congress.csv")

##CONGRESS: YES 2024 PVOTE: SMALLER MODEL WITH PVOTE ONLY##
##This model is engaged on PlanScore when users choose to leave all seats open##
##and a state has 2024 presidential vote data available for predictions in the##
##latest redistricting cycle.                                                 ##

setwd(output.folder)
d.cong <- read_csv("cong_2011_2024_crossval_cycles_2024data.csv") %>%
  filter(!is.na(incumb))
start <- proc.time()
m <- brm(bf(dem_share_imputed ~ dpres_mn +
              (1 + dpres_mn || stateabrev) +
              (1 + dpres_mn | cycle)), 
         data=d.cong, 
         prior=c(set_prior("normal(1.0,0.2)",class="b",coef="dpres_mn"),
                 set_prior("normal(0.5,0.1)",class="Intercept"),
                 set_prior("student_t(3, 0.02, 0.1)", class="sd",coef="Intercept",
                           group="cycle"),
                 set_prior("student_t(3, 0.1, 0.1)", class="sd",coef="dpres_mn",
                           group="cycle"),
                 set_prior("student_t(3, 0.04, 0.1)", class="sd",coef="Intercept",
                           group="stateabrev"),
                 set_prior("student_t(3, 0.07, 0.1)", class="sd",coef="dpres_mn",
                           group="stateabrev")),
         cores=detectCores(), chains=4, control=list(adapt_delta=0.99999, max_treedepth=12),
         warmup=2000, iter=6000, refresh=10, thin=16)
proc.time() - start
saveRDS(m, "full_model_2025B_openseat_congress.rds")

##generating coefficient matrix of posterior samples for PlanScore##
setwd(output.folder)
m <- readRDS("full_model_2025B_openseat_congress.rds")

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

write.csv(round(C, 4), "C_matrix_full-2025B-openseat-congress.csv")
write.csv(round(E, 4), "E_matrix_full-2025B-openseat-congress.csv")

##################################################
##CONGRESS: 2024 PRESIDENTIAL VOTE NOT AVAILABLE##
##################################################

##CONGRESS: NO 2024 PVOTE: LOAD AND FORMAT DATA##

#2020 presidential vote data for the 2022 and 2024 elections#
setwd("https://raw.githubusercontent.com/PlanScore/Model-Generator/refs/heads/migurski/update-model-code-and-data/2025/")
p24 <- read_csv("pres_vote_by_district_2020_2024.csv") %>%
  mutate(cycle=2022,
         dpres20=d20/(d20+r20)) %>%
  dplyr::select(cycle, stpost, district, dpres20)
p24 <- rbind.fill(p24,
                  p24 %>% mutate(cycle=2024))

#congressional outcome data#
setwd("https://raw.githubusercontent.com/PlanScore/Model-Generator/refs/heads/migurski/update-model-code-and-data/2025/")
d.cong <- read_csv("congress_elections_imputations_2025.csv")[,-1] %>%
  filter(cycle >= 2012) %>%
  mutate(district=parse_number(str_sub(stcd, -2, -1))) %>%
  left_join(p24, by=c("cycle","stateabrev"="stpost","district")) %>%
  group_by(stcd2) %>%
  mutate(
    dpres = dpres / 100,
    dpres16 = dplyr::first(dpres[cycle == 2016], default = NA_real_)
  ) %>%
  ungroup() %>%
  mutate(stateabrev=tolower(stateabrev)) %>%
  mutate(dpres=ifelse(stateabrev %in% c("ky","sd") & cycle %in% 2020, dpres16, 
                      ifelse(cycle %in% c(2022, 2024), dpres20, dpres))) %>%
  dplyr::rename(distz=stcd) %>%
  mutate(dpres_mn=dpres-mean(dpres)) #mean-deviate pres vote for easier calculations; mean(dpres)=0.5226811

setwd(output.folder)
d.cong <- sample_frac(d.cong)
n <- 10
nr <- nrow(d.cong)
d.cong$grp <- c(1:n) * rep(1, nr)
write_csv(d.cong, "cong_2011_2024_crossval_cycles.csv")

##CONGRESS: NO 2024 PVOTE: FULL MODEL WITH INCUMBENCY##
##This model is engaged on PlanScore when users choose to identify incumbency##
##values and when a state only has 2020 data available for predictions in the##
##latest redistricting cycle.                                                ##

setwd(output.folder)
d.cong <- read_csv("cong_2011_2024_crossval_cycles.csv") %>%
  filter(!is.na(incumb))

start <- proc.time()
m <- brm(bf(dem_share_imputed ~ dpres_mn + incumb +
              (1 + dpres_mn + incumb || stateabrev) +
              (1 + dpres_mn + incumb | cycle)), 
         data=d.cong, 
         prior=c(set_prior("normal(0.85,0.2)",class="b",coef="dpres_mn"),
                 set_prior("normal(0.05,0.05)",class="b",coef="incumb"),
                 set_prior("normal(0.5,0.1)",class="Intercept"),
                 set_prior("student_t(3, 0.02, 0.1)", class="sd",coef="Intercept",
                           group="cycle"),
                 set_prior("student_t(3, 0.1, 0.1)", class="sd",coef="dpres_mn",
                           group="cycle"),
                 set_prior("student_t(3, 0.02, 0.1)", class="sd",coef="incumb",
                           group="cycle"),
                 set_prior("student_t(3, 0.02, 0.1)", class="sd",coef="Intercept",
                           group="stateabrev"),
                 set_prior("student_t(3, 0.1, 0.1)", class="sd",coef="dpres_mn",
                           group="stateabrev"),
                 set_prior("student_t(3, 0.02, 0.05)", class="sd",coef="incumb",
                           group="stateabrev")),
         cores=detectCores(), chains=4, control=list(adapt_delta=0.99999, max_treedepth=12),
         warmup=2000, iter=6000, refresh=10, thin=16)
proc.time() - start
saveRDS(m, "full_model_2025A_incumbency_congress.rds")

##generating coefficient matrix of posterior samples for PlanScore##
setwd(output.folder)
m <- readRDS("full_model_2025A_incumbency_congress.rds")

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

write.csv(round(C, 4), "C_matrix_full-2025A-incumbency-congress.csv")
write.csv(round(E, 4), "E_matrix_full-2025A-incumbency-congress.csv")

##CONGRESS: NO 2024 PVOTE: SMALLER MODEL WITH PVOTE ONLY##
##This model is engaged on PlanScore when users choose to leave all seats open##
##and a state only has 2020 presidential vote data available for predictions  ##
##in the latest redistricting cycle.                                          ##

setwd(output.folder)
d.cong <- read_csv("cong_2011_2024_crossval_cycles.csv") %>%
  filter(!is.na(incumb))
start <- proc.time()
m <- brm(bf(dem_share_imputed ~ dpres_mn +
              (1 + dpres_mn || stateabrev) +
              (1 + dpres_mn | cycle)), 
         data=d.cong, 
         prior=c(set_prior("normal(1.0,0.2)",class="b",coef="dpres_mn"),
                 set_prior("normal(0.5,0.1)",class="Intercept"),
                 set_prior("student_t(3, 0.02, 0.1)", class="sd",coef="Intercept",
                           group="cycle"),
                 set_prior("student_t(3, 0.1, 0.1)", class="sd",coef="dpres_mn",
                           group="cycle"),
                 set_prior("student_t(3, 0.04, 0.1)", class="sd",coef="Intercept",
                           group="stateabrev"),
                 set_prior("student_t(3, 0.07, 0.1)", class="sd",coef="dpres_mn",
                           group="stateabrev")),
         cores=detectCores(), chains=4, control=list(adapt_delta=0.99999, max_treedepth=12),
         warmup=2000, iter=6000, refresh=10, thin=16)
proc.time() - start
saveRDS(m, "full_model_2025A_openseat_congress.rds")

##generating coefficient matrix of posterior samples for PlanScore##
setwd(output.folder)
m <- readRDS("full_model_2025A_openseat_congress.rds")

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

write.csv(round(C, 4), "C_matrix_full-2025A-openseat-congress.csv")
write.csv(round(E, 4), "E_matrix_full-2025A-openseat-congress.csv")

