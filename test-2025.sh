#!/bin/bash -ex
docker build -t planscore-r:update-model-code-and-data .
docker run -v `pwd`/2025:/vol --rm planscore-r:update-model-code-and-data Rscript --vanilla /usr/local/lib/R/planscore_models_state_leg.R
