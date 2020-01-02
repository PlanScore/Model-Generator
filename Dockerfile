FROM ubuntu:18.04

# https://stackoverflow.com/questions/8671308/non-interactive-method-for-dpkg-reconfigure-tzdata
ENV DEBIAN_FRONTEND=noninteractive DEBCONF_NONINTERACTIVE_SEEN=true
COPY preseed.txt /etc/preseed.txt
RUN debconf-set-selections /etc/preseed.txt

# http://sites.psu.edu/theubunturblog/installing-r-in-ubuntu/
# https://stackoverflow.com/questions/45719942/how-to-install-tidyverse-on-ubuntu-16-04-and-17-04
RUN apt-get update -y \
 && apt-get install -y libssl-dev libxml2-dev libcurl4-openssl-dev \
        r-base r-base-dev r-cran-devtools

# Required packages for run_planscore_model.R
RUN R -e 'require(devtools); \
    install_version("plyr", version="1.8.5"); \
    install_version("tidyverse", version="1.3.0"); \
    install_version("stringr", version="1.4.0"); \
    install_version("arm", version="1.10-1"); \
    install_version("mvtnorm", version="1.0-7"); \
    install_version("msm", version="1.6.8")'

COPY run_planscore_model.R /usr/local/lib/R/run_planscore_model.R
COPY run-planscore-model.sh /usr/local/bin/run-planscore-model.sh
