FROM ubuntu:18.04

# https://stackoverflow.com/questions/8671308/non-interactive-method-for-dpkg-reconfigure-tzdata
ENV DEBIAN_FRONTEND=noninteractive DEBCONF_NONINTERACTIVE_SEEN=true
COPY preseed.txt /etc/preseed.txt
RUN debconf-set-selections /etc/preseed.txt

# http://sites.psu.edu/theubunturblog/installing-r-in-ubuntu/
# https://stackoverflow.com/questions/45719942/how-to-install-tidyverse-on-ubuntu-16-04-and-17-04
RUN apt-get update -y \
 && apt-get install -y libssl-dev libxml2-dev libcurl4-openssl-dev r-base r-base-dev

RUN R -e 'install.packages("plyr")'
RUN R -e 'install.packages("tidyverse")'
RUN R -e 'install.packages("stringr")'
RUN R -e 'install.packages("arm")'
RUN R -e 'install.packages("msm")'
RUN R -e 'install.packages("pbapply")'
