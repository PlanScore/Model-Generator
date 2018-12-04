library(plyr)
library(tidyverse)
library(stringr)
library(arm)
library(msm)

#############
##FUNCTIONS##
#############

#imputations#
impute <- function(i, data, newvar, fixed, sigma) {
  ivs <- colnames(fixed)[-1]
  data$intercept <- 1
  ivs <- c("intercept",ivs)
  upper.bound <- ifelse(str_detect(newvar, ".pc"), 1, Inf)
  data[,newvar] <- rtnorm(dim(data)[1], 
                       as.matrix(data[,ivs]) %*% fixed[i,], sigma[i],
                                         lower=0, upper=upper.bound)
  data$intercept <- NULL
  data[,c("cntyname","precinct","psid",newvar)]
}

#random incumbency offsets#
inc.offsets <- function(i, data, newvar, fixed) {
  ivs <- colnames(fixed)
  data[,newvar] <- as.matrix(data[,ivs]) %*% fixed[i,]
  data[,c("cntyname","precinct","psid",newvar)]
}

#transformations from raw votes to proportions#
party.pc <- function(var.root, d) {
  names2 <- names(d)
  vars <- names2[str_detect(names2, paste0(var.root, "[.]([d r])"))]
  if(length(vars)>1) {
    dem <- vars[str_detect(vars, paste0(var.root, ".d"))]
    rep <- vars[str_detect(vars, paste0(var.root, ".r"))]
    d[,paste0(var.root, ".t")] <- d[,dem] + d[,rep]
    d[,paste0(var.root, ".pc")] <- d[,dem]/(d[,dem] + d[,rep])
    select <- (d[,paste0(var.root, ".pc")] == 1) | (d[,paste0(var.root, ".pc")] == 0)
    select[is.na(select)] <- FALSE
    d[select,paste0(var.root, ".pc")] <- NA
    select <- is.na(d[,paste0(var.root, ".pc")])
    d[select,paste0(var.root, ".t")] <- NA
  }
  return(d)
}

##############
##FORMATTING##
##############

args <- commandArgs(trailingOnly=TRUE)
# test if there is at least one argument: if not, return an error
if (length(args)<3) {
  stop("Must provide 3 arguments: input csv file, state postal code, and chamber identifier", call.=FALSE)
} else {
  d.name <- args[1]
  stpost <- args[2]
  chamber <- args[3]
}

#load the precinct data and merge the different years together#
d <- read.csv(d.name, header=T, stringsAsFactors=F)[,-1]

#calculate D vote proportions for every race
names <- names(d) %>% .[str_detect(., "[.]([d r])")] %>%
  str_replace("[.]([d r])", "") %>% unique(.)
for(i in 1:length(names)) {
  d <- party.pc(names[i], d)
}

#misc recodes and filters#
d <- filter(d, !is.na(us.pres.pc), 
         !str_detect(incumb, ";")) %>%
  mutate(incumb.r=as.integer(incumb=="R"),
         incumb.d=as.integer(incumb=="D"),
         us.pres.t=us.pres.t/100,
         v.t=v.t/100,
         incRXpres=us.pres.pc*incumb.r,
         incDXpres=us.pres.pc*incumb.d) %>%
  filter(!is.na(us.pres.pc), !is.na(us.pres.t), incumb!="")

############
##ANALYSIS##
############

nsims <- 1000 #number of simulations
set.seed(1)

##turnout##
model <- lm(v.t ~ us.pres.t, data=d)
d$v.to.hat <- predict(model, d)
display(model)
coefs <- sim(model, nsims)
fixed.coefs <- coef(coefs)
sigma <- sigma.hat(coefs)
output1 <- lapply(1:nsims, function(v,w,x,y,z) impute(v,w,x,y,z), d, "v.t.est", 
                  fixed.coefs, sigma)
turnout <- Reduce(function(x,y) #recursively merges the simulations together
  merge(x, y, by=c("cntyname","precinct","psid")), output1)
write.csv(turnout, paste0(stpost,"_precinct_model_",chamber,"_turnout.csv"))

##D proportion of vote##
model <- lm(v.pc ~ us.pres.pc + incumb.r + incumb.d + incRXpres + incDXpres, 
              data=d)
d$v.pc.hat <- predict(model, d)
display(model)
coefs <- sim(model, nsims)
fixed.coefs <- coef(coefs)
sigma <- sigma.hat(coefs)

#open seat simulations#
output2 <- lapply(1:nsims, function(v,w,x,y,z) impute(v,w,x,y,z), d, "v.pc.est", 
                  fixed.coefs[,c("(Intercept)","us.pres.pc")], sigma)
proportion <- Reduce(function(x,y) 
  merge(x, y, by=c("cntyname","precinct","psid")), output2)
write.csv(proportion, paste0(stpost,"_precinct_model_",chamber,"_open.csv"))

#D incumbent offset simulations#
d.incD <- mutate(d, incumb.d=1, incDXpres=us.pres.pc) #version of d with only Dem incs
incD <- lapply(1:nsims, function(w,x,y,z) inc.offsets(w,x,y,z), d.incD, "add.incD",
               fixed.coefs[,c("incumb.d", "incDXpres")]) #produce random Dem inc offsets
incD <- Reduce(function(x,y)
  merge(x, y, by=c("cntyname","precinct","psid")), incD)
write.csv(incD, paste0(stpost,"_precinct_model_",chamber,"_incD.csv"))

#R incumbent offset simulations#
d.incR <- mutate(d, incumb.r=1, incRXpres=us.pres.pc) #version of d with only Rep incs
incR <- lapply(1:nsims, function(w,x,y,z) inc.offsets(w,x,y,z), d.incR, "add.incR",
               fixed.coefs[,c("incumb.r", "incRXpres")]) #produce random Rep inc offsets
incR <- Reduce(function(x,y)
  merge(x, y, by=c("cntyname","precinct","psid")), incR)
write.csv(incR, paste0(stpost,"_precinct_model_",chamber,"_incR.csv"))

##model check##
pred <- ddply(d, .(district), summarise, v.hat=sum(v.pc.hat*v.to.hat)/sum(v.to.hat),
              v=sum(v.d)/(100*sum(v.t))) %>%
  mutate(district=as.numeric(district)) %>%
  arrange(district) %>%
  mutate(v.new=ifelse(is.na(v), v.hat, v),
         s=v.new>=0.5,
         v.alt = v.new - (mean(v.new) - 0.5),
         s.alt = v.alt>=0.5)

png(paste0(stpost,"_pred_v_actual_",chamber,".png"), width=8,height=4, 
    units="in", res=300)
par(mar=c(4,4,2,1))
min.x <- min(pred$v.hat, na.rm=T)
min.y <- min(pred$v, na.rm=T)
max.x <- max(pred$v.hat, na.rm=T)
max.y <- max(pred$v, na.rm=T)
plot(pred$v.hat, pred$v, xlab="Predicted district vote share", 
     ylab="Actual district vote share", pch=19)
abline(a=0, b=1, lwd=2)
title(main=paste0(stpost, " ", chamber, " prediction validation"))
rmse <- sqrt(mean((pred$v.hat-pred$v)^2, na.rm=T))
text(min.x+(max.x-min.x)/10, max.y-(max.y-min.y)/10, pos=4, 
       paste0("RMSE = ", round(rmse, 3)))
dev.off()

#EG#
print(paste0("Efficiency gap: ", round(mean(pred$s) - 0.5 - 2*(mean(pred$v.new) - 0.5), 3)))

#Mean Median#
print(paste0("Mean median: ", round(median(pred$v.new) - mean(pred$v.new), 3)))

#Gelman-King Bias#
print(paste0("Gelman-King bias: ", round(mean(pred$s.alt) - 0.5, 3)))
