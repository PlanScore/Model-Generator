FROM ubuntu:24.04

# https://stackoverflow.com/questions/8671308/non-interactive-method-for-dpkg-reconfigure-tzdata
ENV DEBIAN_FRONTEND=noninteractive DEBCONF_NONINTERACTIVE_SEEN=true
COPY preseed.txt /etc/preseed.txt
RUN debconf-set-selections /etc/preseed.txt

# http://sites.psu.edu/theubunturblog/installing-r-in-ubuntu/
# https://stackoverflow.com/questions/45719942/how-to-install-tidyverse-on-ubuntu-16-04-and-17-04
# Updated again 2026-03, now using devtools just as a way to shortcut later transitive dependencies
RUN apt-get update -y \
 && apt-get install -y libssl-dev libxml2-dev libcurl4-openssl-dev \
        r-base r-base-dev r-cran-devtools

# Required packages for run_planscore_model.R
RUN R -e '\
    for (pkg in c("Rcpp", "tidyverse", "plyr", "parallel", "dplyr", "brms")) { \
        install.packages(pkg, repos="https://cloud.r-project.org"); \
        if (!require(pkg, character.only=TRUE)) quit(status=1) \
    }'

COPY 2025/planscore_models_congress.R /usr/local/lib/R/planscore_models_congress.R
COPY 2025/planscore_models_state_leg.R /usr/local/lib/R/planscore_models_state_leg.R
