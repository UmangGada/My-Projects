
#Url List
library(lubridate)
library(dplyr)
library(rstan)

urls <- list(
  'https://data.boston.gov/dataset/ac9e373a-1303-4563-b28e-29070229fdfe/resource/5a48ca2f-015c-478e-a2c7-a79982c2ee47/download/january.2020-bostonfireincidentopendata.csv', #Jan 2020
  'https://data.boston.gov/dataset/ac9e373a-1303-4563-b28e-29070229fdfe/resource/1659a94d-005f-4944-a2f9-5f03c91c1fd4/download/february.2020-bostonfireincidentopendata.csv', #Feb 2020
  'https://data.boston.gov/dataset/ac9e373a-1303-4563-b28e-29070229fdfe/resource/c179386d-9113-4b4a-9ccc-1c537809da83/download/march.2020-bostonfireincidentopendata.csv', #Mar 2020
  'https://data.boston.gov/dataset/ac9e373a-1303-4563-b28e-29070229fdfe/resource/1172f322-88ea-43a6-8fcd-abda6a47193f/download/april.2020-bostonfireincidentopendata.csv', #Apr 2020
  'https://data.boston.gov/dataset/ac9e373a-1303-4563-b28e-29070229fdfe/resource/5535d65f-7ec7-48bb-ab88-57dec2f31c46/download/may.2020-bostonfireincidentopendata.csv', #May 2020
  'https://data.boston.gov/dataset/ac9e373a-1303-4563-b28e-29070229fdfe/resource/f4d650ac-35bc-42c9-aef1-406d550eb592/download/june.2020-bostonfireincidentopendata.csv', #Jun 2020
  'https://data.boston.gov/dataset/ac9e373a-1303-4563-b28e-29070229fdfe/resource/28f50aae-1dc4-4945-bd8a-0f22914f6c6d/download/july.2020-bostonfireincidentopendata.csv', #Jul 2020
  'https://data.boston.gov/dataset/ac9e373a-1303-4563-b28e-29070229fdfe/resource/6040caa3-8337-45d3-9a6d-24d163b95584/download/august.2020-bostonfireincidentopendata.csv', #Aug 2020
  'https://data.boston.gov/dataset/ac9e373a-1303-4563-b28e-29070229fdfe/resource/3f04e41e-5d46-4074-9f68-b541a17ae32a/download/september.2020-bostonfireincidentopendata.csv', #Sep 2020
  'https://data.boston.gov/dataset/ac9e373a-1303-4563-b28e-29070229fdfe/resource/b84d47ca-26f7-4edc-b6f6-d4c1df7bf63c/download/october.2020-bostonfireincidentopendata.csv', #Oct 2020
  'https://data.boston.gov/dataset/ac9e373a-1303-4563-b28e-29070229fdfe/resource/28d79368-c444-42c7-8ffe-617b1fe38b63/download/november.2020-bostonfireincidentopendata.csv', #Nov 2020
  'https://data.boston.gov/dataset/ac9e373a-1303-4563-b28e-29070229fdfe/resource/a2a832c9-d76c-4e5a-9a98-dbcbad6229a0/download/december.2020-bostonfireincidentopendata.csv', #Dec 2020
  'https://data.boston.gov/dataset/ac9e373a-1303-4563-b28e-29070229fdfe/resource/18723afa-d808-42ec-9020-10357215b6b7/download/january.2021-bostonfireincidentopendata.csv', #Jan 2021
  'https://data.boston.gov/dataset/ac9e373a-1303-4563-b28e-29070229fdfe/resource/5d03feb9-260c-4b11-ada1-cf7ad65a5e08/download/february.2021-bostonfireincidentopendata.csv', #Feb 2021
  'https://data.boston.gov/dataset/ac9e373a-1303-4563-b28e-29070229fdfe/resource/119105f6-901e-4d67-8dee-5d2368a0757c/download/march.2021-bostonfireincidentopendata.csv' #Mar 2021
)

#Create unified raw data frame (takes about a minute)
for (i in 1:length(urls)){
  if (i ==1) {
    fire.dat<-  read.csv(urls[[i]],stringsAsFactors = F)
  }else {
    new.dat <- read.csv(urls[[i]],stringsAsFactors = F)
    fire.dat <- rbind(fire.dat,new.dat)
  }
}

#set up data for analysis
fire.dat$Date <- as.Date(fire.dat$Alarm.Date,format="%m/%d/%Y")
fire.dat$month <- month(fire.dat$Date)
fire.dat$year <- year(fire.dat$Date)

fire.summarized <- fire.dat %>% group_by(year,month,Date) %>% summarise(incidents=n()) 

#Quick data checks

print(head(fire.summarized))

cat('\n Incidents Summary \n')
print(summary(fire.summarized$incidents))


# Format data for stan
fire.training.data <- fire.summarized[fire.summarized$year == 2020,]
print(tail(fire.training.data))

J<-12

y.ji <- fire.training.data$incidents
month <- fire.training.data$month
print(month[1:5])
n<- length(y.ji)
cat('\n Incidents Vector Head \n')
print(y.ji[1:5])

n.j <- (fire.training.data %>% group_by(month) %>% summarize(days=n()))$days
#Important to remember 2020 was a leap year
cat('\n Days per Month \n')
n.j


#THINK THIS MIGHT WORK
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
hierString = " 
data {
  int<lower=0> N;
  int<lower=0> NGroups;
  int y[N];
  int group[N];
}

parameters {
  real<lower=0> mu[NGroups];
  real<lower=0> alpha0;
  real<lower=0> beta0;
  real logphi;
}

model {
  alpha0 ~ student_t(11,0, 20);
  beta0 ~ student_t(11,0, 20);
  mu ~ gamma(alpha0, beta0);
  
  for(i in 1:N)
    y[i] ~ neg_binomial_2(mu[group[i]], exp(logphi) );
}

"
stanDSO = stan_model(model_code=hierString)

hierFit = sampling(object=stanDSO,
                   data=list(N=n,NGroups =J, y=y.ji,group=month),
                   chains=5, warmup=500, iter=4000, seed=202004132)
summary(hierFit)$summary


#Plot posteriors
plot(hierFit, show_density = TRUE, ci_level = 0.85, fill_color = "mistyrose4", pars = paste0("lambda[", 1:12, "]"))

stan.samples <- as.data.frame(hierFit)



stan.samples <- as.data.frame(hierFit)
y_sample <- matrix(data = NA, nrow = nrow(stan.samples), ncol = 12, byrow = FALSE,
                   dimnames = NULL)
y_sample[1:5,1:5]

# y1 <- rep(NA,nrow(stan.samples))
for (j in 1:12){
  for (i in 1:nrow(stan.samples)){
    y_sample[i,j]<- rpois(1,stan.samples[i,j])
  }
}
y_sample[1:5,1:5]

par(mfrow=c(4,3))
for (j in 1:12){
  hist(y_sample[,j],main=paste0('Month = ',j),col='blue', freq = FALSE)
  lines(density(fire.training.data$incidents[fire.training.data$month==j]), col = 2, lwd = 2, lty = 2)
  # cat('\n Month = ',j ,' Quantiles \n')
  # print(quantile(y_sample[,j],probs=c(.025,.5,.975)))
  abline(v=quantile(y_sample[,j],probs=c(.025,.5,.975)),lwd=c(2,2,2))
  axis(side=1, at=quantile(y_sample[,j],probs=c(.025)), labels = FALSE)
  text(x=quantile(y_sample[,j],probs=c(.025)),  par("usr")[3]
       , labels =   quantile(y_sample[,j],probs=c(.025)), srt = 45, pos = 1, xpd = TRUE)
  
  axis(side=1, at=quantile(y_sample[,j],probs=c(.5)), labels = FALSE)
  text(x=quantile(y_sample[,j],probs=c(.5)),  par("usr")[3]
       , labels =   quantile(y_sample[,j],probs=c(.5)), srt = 45, pos = 1, xpd = TRUE)
  
  axis(side=1, at=quantile(y_sample[,j],probs=c(.975)), labels = FALSE)
  text(x=quantile(y_sample[,j],probs=c(.975)),  par("usr")[3]
       , labels =   quantile(y_sample[,j],probs=c(.975)), srt = 45, pos = 1, xpd = TRUE)
  
}

for (j in 1:12){
  cat('\n Month = ',j ,' Quantiles \n')
  print(quantile(y_sample[,j],probs=c(.025,.5,.975)))
  
}

library(bayesplot)
posterior_cp = as.array(hierFit)

## 
color_scheme_set("mix-brightblue-gray")
mcmc_trace(posterior_cp, pars = "logphi", np = nuts_params((hierFit))) + 
  xlab("Fire incidents iteration")


fit_cp_bad_rhat <- sampling(stanDSO, data = list(N=n,NGroups =J, y=y.ji,group=month), 
                            iter = 4000, init_r = 10, seed = 20212504)
rhats = rhat(fit_cp_bad_rhat)
print

### rhats
#Looking at the rhats plot, we can see that all the rhat values are pretty close to 1. This suggests that all the chains are in equillibrium
#This in turn means that the chains have converged to a common distribution. We can easily check that by reducing the number of iterations in fit_cp_bad_rhat to 50 to find rhat values greater than 1.

color_scheme_set("brightblue") # see help("color_scheme_set")
mcmc_rhat(rhats) + yaxis_text(hjust = 1)


#neff
#Because the draws within a Markov chain are not independent if there is autocorrelation,
#the effective sample size, neff, is usually smaller than the total sample size, N (although it may be larger in some cases). The larger the ratio of neff to N the better

#Since we have pretty high ratios all greater than 0.5, we don't need to worry about the effective sample size.

ratios_cp <- neff_ratio(hierFit)
print(ratios_cp)

mcmc_neff(ratios_cp, size = 2)

#acf
#For the selected parameters, these functions show the autocorrelation for each Markov chain separately up to a user-specified number of lags.
#Positive autocorrelation is bad (it means the chain tends to stay in the same area between iterations) and you want it to drop quickly to zero with increasing lag.
#Negative autocorrelation is possible and it is useful as it indicates fast convergence of sample mean towards true mean.

#We can see that for each chain, the acf function drops to 0 as lag increases for our hyperparameters.
#Whereas the acf function for our parameters drops even more quickly, even showing negative autocorrelation.
mcmc_acf_bar(posterior_cp, pars = c("mu[1]","mu[2]","mu[3]","mu[4]","mu[5]","mu[6]","mu[7]","mu[8]","mu[9]","mu[10]","mu[11]","mu[12]","logphi"), lags = 12)
mcmc_acf_bar(posterior_cp, pars = c("alpha0","beta0"), lags = 12)

// generated quantities {
  //  vector[N] y_rep;
  //  for (n in 1:N) {
    //    y_rep[n] = neg_binomial_2_rng(alpha0, beta0);
    //  }