library(R2jags)

house_vec = c(1,1,1,1,1,1,2,2,2,2) # 6 points for house 1, 4 for house 2
ind_vec = c(1,1,2,2,3,3,4,4,5,5) # everyone has two observations
N = 10
x = c(1,4,5,7,3,5,9,8,7,6)
y = x*3 + rnorm(10,0,1)

mod.photo <- "model
{
for(i in 1:N){
  mu_house[house_vec[i]] ~ dnorm(0, taua)
  mu_ind[ind_vec[i]] ~ dnorm(mu_house[house_vec[i]], taub_a)
  mu[i] <- beta1*x + mu_ind[ind_vec[i]]
  y[i] ~ dnorm(mu[i],tau)
  }

# priors
taua ~ dgamma(0.01, 0.01) # precision
sda <- 1 / sqrt(taua) # derived standard deviation
taub_a ~ dgamma(0.01, 0.01) # precision
sdb_a <- 1 / sqrt(taub_a) # derived standard deviation
beta0 ~ dnorm(0,.001)
beta1 ~ dnorm(0,.001)
tau <- sigma^-2 #coverts sd to precision
sigma ~ dunif(0, 100)  #uniform prior for standard deviation
}" #end model
  write(mod.photo, "model.txt")
  
  #input lists for JAGS
  params = c("beta1") #parameters to monitor
  inits = function() list(beta1=rnorm(1)) #starting values of fitted parameters
  input = list(N=N,x=x,y=y,house_vec=house_vec,ind_vec=ind_vec)
  
  #run JAGS model
  jags.p <- jags(model = "model.txt",data = input,inits=inits,param=params,
                 n.chains = 3, #number of separate MCMC chains
                 n.iter =3000, #number of iterations per chain
                 n.thin=3, #thinning
                 n.burnin = 500) #number of initial iterations to discard
  