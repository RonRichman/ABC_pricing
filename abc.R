require(foreign)
require(data.table)
require(plyr)
require(maggritr)
require(reshape2)
require(broom)
require(data.table)
require(reshape2)
require(rgl)
require(stringr)
require(gnm)
require(dplyr)
require(ggplot2)
require(RColorBrewer)
require(gridExtra)
require(scales)
require(foreign)
require(lubridate)
require(mgcv)
require(reshape)
library(rvest)
require(lme4)
require(splines)
require(gridExtra)

dat = fread("c:/r/prices_motor.csv")
my_optim = function(pars,lr=lr ,dat=dat ){
  freq = pars[1]
  sev = pars[2]
  dat = dat %>% data.table()
  dat[,Priced := freq*(sev-Excess)/lr/12]
  return(dat[,sum(abs(Premium/Priced-1))])
}
my_optim(c(0.25, 25000), 0.75, dat)

naive = optim(c(0.25, 25000),  my_optim, lr = 0.7, dat=dat)
naive$par
### Frequency

mean_freq = 0.25
sd_freq = 0.075
mean_beta = function(a,b) a/(a+b)
sd_beta = function(a,b) ((a*b/((a+b)^2 *(a+b+1)))^0.5)

beta_optim = function(pars){
  a = pars[1]
  b = pars[2]
  mean_dev = abs(mean_freq/mean_beta(a,b)-1)
  sd_dev= abs(sd_freq/sd_beta(a,b)-1)
  return(mean_dev + sd_dev)
}

beta_par = optim(c(0.1,0.1),  beta_optim)
a=beta_par$par[1]
b=beta_par$par[2]

rbeta(10000, a, b) %>% hist

### Severity - Mean

mean_sev = 20000
sd_sev = 2500

mean_gamma = function(a,b) a/b
sd_gamma = function(a,b) ((a/(b)^2)^0.5)

gamma_optim = function(pars){
  a = exp(pars[1])
  b = exp(pars[2])
  mean_dev = abs(mean_sev/mean_gamma(a,b)-1)
  sd_dev= abs(sd_sev/sd_gamma(a,b)-1)
  return(mean_dev+sd_dev)
}

gamma_par = optim(c(1,1),  gamma_optim, "BFGS", control = list(maxit=10000))

alpha = exp(gamma_par$par[1])
beta =exp(gamma_par$par[2])

rgamma(10000, alpha, beta) %>% hist

### Severity - sd

sd_sev = 10000
sd_sd_sev = 2500

mean_gamma = function(a,b) a/b
sd_gamma = function(a,b) ((a/(b)^2)^0.5)

gamma_optim = function(pars){
  a = exp(pars[1])
  b = exp(pars[2])
  mean_dev = abs(sd_sev/mean_gamma(a,b)-1)
  sd_dev= abs(sd_sd_sev/sd_gamma(a,b)-1)
  return(mean_dev+sd_dev)
}

gamma_par = optim(c(1,1),  gamma_optim, "BFGS", control = list(maxit=10000))

alpha_sd = exp(gamma_par$par[1])
beta_sd =exp(gamma_par$par[2])

rgamma(10000, alpha_sd, beta_sd) %>% hist

### log-norm

lnorm_par = function(mean, sd) {
  cv = sd/mean
  sigma2 = log(cv^2+1)
  mu = log(mean)-sigma2/2
  results = list(mu,sigma2)
  results
  
}

lnorm_par = lnorm_par %>% Vectorize()

### Loss ratio

mean_freq_lr = 0.7
sd_freq_lr = 0.025
mean_beta = function(a,b) a/(a+b)
sd_beta = function(a,b) ((a*b/((a+b)^2 *(a+b+1)))^0.5)

beta_optim = function(pars){
  a = pars[1]
  b = pars[2]
  mean_dev = abs(mean_freq_lr/mean_beta(a,b)-1)
  sd_dev= abs(sd_freq_lr/sd_beta(a,b)-1)
  return(mean_dev + sd_dev)
}

beta_lr_par = optim(c(0.1,0.1),  beta_optim)
a_lr=beta_lr_par$par[1]
b_lr=beta_lr_par$par[2]

rbeta(10000, a_lr, b_lr) %>% hist

#### compound
N_sims = 100000

claims = data.table(freq = rbeta(N_sims, a, b), 
                    acpc = rgamma(N_sims, alpha, beta), 
                    acpc_sd = rgamma(N_sims, alpha_sd, beta_sd),
                    LR = rbeta(N_sims, a_lr, b_lr))
claims[,run:=.I]
my_pois = function(lambda) rpois(1, lambda)
my_pois = my_pois %>% Vectorize()

claims[, n:= my_pois(freq)]
claims[,c("mu", "sigma2"):=lnorm_par(acpc, acpc_sd),by=run]

compound = function(dat) {
  n=dat$n
  mu = dat$mu
  sigma2 = dat$sigma2
  excess = dat$excess
  
  if(n ==0 ) {0}
  else {
    claims = rlnorm(n , mu, sigma2)
    net_claims = claims - excess
    net_claims = ifelse(net_claims<0,0,net_claims)
    return(sum(net_claims))
  }
}

claims[,freq_bin :=ntile(freq,9)]
claims[,sev_bin :=ntile(acpc,9)]
claims[,sev_sd_bin :=ntile(acpc_sd,9)]
claims[,lr_bin :=ntile(LR,9)]
claims[,id:=paste0(freq_bin, sev_bin, sev_sd_bin, lr_bin)]

claims[,excess:=9845]
claims$prem_9845 = ddply(claims, .(run), compound)$V1/LR/12

claims[,excess:=7620]
claims$prem_7620 = ddply(claims, .(run), compound)$V1/LR/12

claims[,excess:=4840]
claims$prem_4840 = ddply(claims, .(run), compound)$V1/LR/12

claims[,excess:=4580]
claims$prem_4580 = ddply(claims, .(run), compound)$V1/LR/12

claims[,excess:=3920]
claims$prem_3920 = ddply(claims, .(run), compound)$V1/LR/12

claims[,excess:=4515]
claims$prem_4515 = ddply(claims, .(run), compound)$V1/LR/12

claims_melt = claims %>% melt(measure.vars = c("prem_9845", "prem_7620", "prem_4840", 
                                              "prem_4580", "prem_3920", "prem_4515")) %>% data.table()


reasonable = claims_melt[,.(N=.N, mean = mean(value)), keyby = .(id, variable)]

dat[,variable:=paste0("prem_", Excess)]
dat %>% setkey(variable)
reasonable %>% setkey(variable)
reasonable = reasonable %>% merge(dat[,c(1,3),  with=F], allow.cartesian = T)
reasonable[,distance:=abs(Premium/mean-1)]
reasonable = reasonable[,.(dist_mean = mean(distance), dist_median = median(distance)), keyby = id]
reasonable = reasonable[order(dist_median)]

reasonable %>% ggplot(aes(x = dist_mean, y = dist_median)) + geom_point()


reasonable = reasonable[dist_median<0.08]

reasonable_pars = claims[id %in% reasonable$id][,c(1:4),with=F]
reasonable_pars[,type:="posterior"]

prior_draws = reasonable_pars[,.N]
prior = data.table(freq = rbeta(prior_draws, a, b),
                   acpc = rgamma(prior_draws, alpha, beta),
                   acpc_sd = rgamma(prior_draws, alpha_sd, beta_sd),
                   LR = rbeta(prior_draws, a_lr, b_lr))
prior[,type:="prior"]
reasonable_pars = rbind(reasonable_pars,prior)

#require(GGally)
#ggpairs(reasonable_pars[type == "posterior"], aes( alpha = 0.2, fill=type))

reasonable_pars[,run:=.I]
reasonable_pars = reasonable_pars %>% melt(id.vars = c("run", "type")) %>% data.table

reasonable_pars %>% ggplot(aes(x = value, fill = type, alpha=.5))+geom_density()+facet_wrap(~variable, scales='free')+
    scale_alpha_continuous(guide=F)
ggsave("c:/r/posterior.jpg", device = "jpg")


results = claims_melt[id %in% reasonable$id,.(N=.N, mean = mean(value)), keyby = .(variable)]
results = merge(dat,results)

ggplot()+geom_point(data=results, aes(x = variable, y = Premium))+
  geom_point(data=results, aes(x = variable, y = mean, colour = "red"))+
  scale_color_discrete(guide=F)
ggsave("c:/r/prices.jpg", device = "jpg")