require(data.table)
require(ggplot2)
require(ggpubr)
require(patchwork)
require(moments)

####### Appendix

## get gaussian with stochastic variance - p = 0.5

dat = data.table(id = 1:1000000, unif = runif(1000000), mixing =  runif(1000000))
dat[, gaussian1 := qnorm(unif, 0, 2)]
dat[, gaussian2 := qnorm(unif, 0, 1)]
dat[, stoch_var := ifelse(mixing> 0.5, gaussian2, gaussian1)]

dat %>% sample_frac(0.1) %>% melt.data.table(measure.vars = c("gaussian1", "gaussian2", "stoch_var")) %>% 
  ggplot(aes(x = value))+geom_density(aes(fill = variable), alpha = 0.4) + ggtitle("Density")+theme_pubr()

  dat %>% melt.data.table(measure.vars = c("gaussian1", "gaussian2", "stoch_var")) %>% .[, kurtosis(value), keyby =variable] %>% 
    fwrite("c:/r/kurt.csv")

## now stochastize the mean too - war and peace

dat = data.table(id = 1:1000000, unif = runif(1000000), mixing =  runif(1000000))
dat[, gaussian1 := qnorm(unif, -10, 1)]
dat[, gaussian2 := qnorm(unif, 10, 2)]
dat[, stoch_var := ifelse(mixing> 0.3, gaussian2, gaussian1)]

# for tail plot
dat %>% sample_frac(0.1) %>% melt.data.table(measure.vars = c("gaussian1", "gaussian2", "stoch_var")) %>% 
  ggplot(aes(x = value))+geom_density(aes(fill = variable), alpha = 0.2) + ggtitle("Density")+theme_pubr()

dat %>% melt.data.table(measure.vars = c("gaussian1", "gaussian2", "stoch_var")) %>% .[, kurtosis(value), keyby =variable]%>% 
  fwrite("c:/r/kurt2.csv")

## now stochastize the mean too - bond returns

dat = data.table(id = 1:1000000, unif = runif(1000000))
dat[, gaussian1 := qnorm(unif, -100, 0.1)]
dat[, gaussian2 := qnorm(unif, 101.06, 0.001)]
dat[, stoch_var := ifelse(unif> 0.2, gaussian2, gaussian1)]


# for tail plot
dat %>% sample_frac(0.1) %>% melt.data.table(measure.vars = c("gaussian1", "gaussian2", "stoch_var")) %>% 
  ggplot(aes(x = value))+geom_density(aes(fill = variable), alpha = 0.2) + ggtitle("Density")+theme_pubr()

dat %>% melt.data.table(measure.vars = c("gaussian1", "gaussian2", "stoch_var")) %>% .[, kurtosis(value), keyby =variable]%>% 
  fwrite("c:/r/kurt3.csv")


### markov chains

require(markovchain)

MC_default <- new("markovchain", states = c("default", "repay"),
                  transitionMatrix = matrix(data = c(1, 0,
                                                     0.01,0.99), byrow = T, nrow = 2),
                  name = "default")


MC_default %>% plot

sim_n = function(n, MC_default = MC_default) {
  temp = (MC_default^n)@transitionMatrix %>% data.table() %>% melt.data.table()
  temp[, variable := c("P(Default|Default)", "P(Default|Repay)", "P(Repay|Default)", "P(Repay|Repay)")]
  temp[, sims := n]
  }

all_sims = rbindlist(lapply(c(1,5,10,100), function(x) sim_n(x, MC_default)))

all_sims %>% ggplot(aes(x = variable, y = value))+geom_col(aes(fill = variable))+facet_wrap(~sims)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
####### 7.1 - 7.2
dat = data.table(id = 1:1000000, unif = runif(1000000))
dat[, gaussian := qnorm(unif, 0, 1)]

numeric = numeric(1000)
for (i in 1:1000) numeric[i] =  dat %>% sample_n(1000) %>% .[, mean(gaussian)]

p1 = numeric %>% data.table() %>% ggplot(aes(x=.)) + geom_histogram() + ggtitle("Mean of 1000 random draws, Gaussian")

numeric = numeric(1000)
for (i in 1:1000) numeric[i] =  dat %>% sample_n(10000) %>% .[, mean(gaussian)]

p2 = numeric %>% data.table() %>% ggplot(aes(x=.)) + geom_histogram() + ggtitle("Mean of  10000 random draws, Gaussian")

p1 + p2

numeric = numeric(1000)
for (i in 1:1000) numeric[i] =  dat %>% sample_n(1000) %>% .[, sum(gaussian)]
numeric %>% data.table() %>% ggplot(aes(x=.)) + geom_histogram() + ggtitle("1000 sums of 1000 draws from Gaussian")
