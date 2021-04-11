######## Chapter 5

require(data.table)
require(ggplot2)
require(ggpubr)
require(patchwork)

## get invented distribution

dat = data.table(id = 1:1000000, unif = runif(1000000))
dat[, gaussian := qnorm(unif)]
dat[, invented_k1 := -atanh(1 - 2*unif)/1]
dat[, unif_rescale := (ifelse(unif<0.5, 1-unif,unif)-0.5)*2]
dat[, gaussian_abs := abs(qnorm(unif))]
dat[, invented_k1_abs := abs(-atanh(1 - 2*unif)/1)]

# for tail plot
p1 = dat %>% sample_frac(0.1) %>% melt.data.table(measure.vars = c("gaussian", "invented_k1")) %>% 
  ggplot(aes(x = value))+geom_density(aes(fill = variable), alpha = 0.4) + ggtitle("Density")+theme_pubr()

p2 = dat[unif>0.99] %>% sample_frac(0.1) %>% melt.data.table(measure.vars = c("gaussian", "invented_k1")) %>% 
  ggplot(aes(x = value))+geom_density(aes(fill = variable), alpha = 0.4)+ ggtitle("Density of u > 0.99")+theme_pubr()

p1 + p2

dat %>% sample_frac(0.01) %>% melt.data.table(measure.vars = c("invented_k1_abs", "gaussian_abs")) %>% 
  ggplot(aes(x = (value),y = (1-unif_rescale)))+geom_point(aes(color = variable))+ scale_y_log10() + scale_x_log10()+theme_pubr()

### lnorm

dat[, lnorm_1 := qlnorm(unif, sdlog = 1)]
dat[, lnorm_2 := qlnorm(unif, sdlog = 2)]
dat[, lnorm_3 := qlnorm(unif, sdlog = 3)]

p1 = dat %>% sample_frac(0.01) %>% melt.data.table(measure.vars = c("lnorm_1", "lnorm_2", "lnorm_3")) %>% 
  ggplot(aes(x = log(value)))+geom_density(aes(fill = variable), alpha = 0.4)+ ggtitle("Density")+theme_pubr()

p2 = dat %>% sample_frac(0.01) %>% melt.data.table(measure.vars = c("lnorm_1", "lnorm_2", "lnorm_3")) %>% 
  ggplot(aes(x = (value),y = (1-unif)))+geom_point(aes(color = variable))+ scale_y_log10() + scale_x_log10()+ 
  ggtitle("Log-log plot")+theme_pubr()

p1 + p2

require(Pareto)

dat[, pareto_3 := Pareto::qPareto(p = unif, alpha = 3, t=0.1)]
dat[, pareto_2 := Pareto::qPareto(p = unif, alpha = 2, t=0.1)]
dat[, pareto_1 := Pareto::qPareto(p = unif, alpha = 1, t=0.1)]
dat %>% sample_frac(0.01) %>% melt.data.table(measure.vars = c("pareto_3", "pareto_2", "pareto_1")) %>% 
  ggplot(aes(x = value))+geom_density(aes(fill = variable), alpha = 0.4) + ggtitle("Density")+theme_pubr()


p1 = dat %>% sample_frac(0.01) %>% melt.data.table(measure.vars = c("pareto_1", "pareto_2", "pareto_3")) %>% 
  ggplot(aes(x = log(value)))+geom_density(aes(fill = variable), alpha = 0.4)+ ggtitle("Density")+theme_pubr()

p2 =dat[unif>0.99] %>% sample_frac(0.1) %>% melt.data.table(measure.vars = c("pareto_1", "pareto_2", "pareto_3")) %>% 
  ggplot(aes(x = log(value)))+geom_density(aes(fill = variable), alpha = 0.4)+ ggtitle("Density of u > 0.99")+theme_pubr()

p3 = dat %>% sample_frac(0.01) %>% melt.data.table(measure.vars = c("pareto_1", "pareto_2", "pareto_3")) %>% 
  ggplot(aes(x = (value),y = (1-unif)))+geom_point(aes(color = variable))+ scale_y_log10() + scale_x_log10()+ 
  ggtitle("Log-log plot")+theme_pubr()

(p1 + p2)/p3

### transformations of Pareto

dat[, pareto_2_sqr := pareto_2^2]

dat %>% sample_frac(0.01) %>% melt.data.table(measure.vars = c("pareto_2", "pareto_2_sqr")) %>% 
  ggplot(aes(x = (value),y = (1-unif)))+geom_point(aes(color = variable))+ scale_y_log10() + scale_x_log10()+ 
  ggtitle("Log-log plot")+theme_pubr()


dat[, pareto_2_log := exp(pareto_2)]

dat %>% sample_frac(0.01) %>% melt.data.table(measure.vars = c("pareto_2", "pareto_2_log")) %>% 
  ggplot(aes(x = (value),y = (1-unif)))+geom_point(aes(color = variable))+ scale_y_log10() + scale_x_log10()+ 
  ggtitle("Log-log plot")+theme_pubr()
