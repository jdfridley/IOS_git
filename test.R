library(R2jags)

<<<<<<< HEAD
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
=======
#generic nested slope function for JAGS

dat = read.csv("/Users/fridley/Documents/academic/projects/IOS_FranceJapan/licor_files/Japan_licor_rev.csv")
dat$Ci_Pa = dat$Ci * dat$Press/ 1000

spp1 = "Bidens frondosa"
spp2 = "Chenopodium album"
df = dat[dat$species==spp1|dat$species==spp2,]
df = df[df$PARi>1010,]
df = df[df$Ci>=0,]
N = dim(df)[1]
ind = as.numeric(as.factor(df$filename)) #grouping vector (individual)
N.indiv = max(ind)
spp = as.numeric(as.factor(df$species))
N.spp = max(spp)
X = log(df$Ci_Pa)
Y = df$Photo
plot(X,Y)

ind.spp = c(1,1,1,1,1,2,2,2,2) #species membership of inds 1-9

mod <- "model
{
  for(i in 1:N) {
    Y[i] ~ dnorm(mu[i],tau)
    mu[i] <- b0 + b1[ind[i]]*X[i]
  }

  for(i in 1:N.indiv) {
    b1[i] ~ dnorm(mu.spp[ind.spp[i]],tau.ind)
  }

  for(i in 1:N.spp) {
    mu.spp[i] ~ dnorm(0,tau.spp)
  }

  b0 ~ dnorm(0,0.001)
  tau <- sigma^-2 #coverts sd to precision
  sigma ~ dunif(0, 100)  #uniform prior for standard deviation
  tau.ind <- sigma.ind^-2 #coverts sd to precision
  sigma.ind ~ dunif(0, 100)  #uniform prior for standard deviation
  tau.spp <- sigma.spp^-2 #coverts sd to precision
  sigma.spp ~ dunif(0, 100)  #uniform prior for standard deviation

}"
  write(mod,"model.txt")

  params = c("b0","b1","mu.spp")
  inits = function() list(b0=0)
  input = list(N=N,N.indiv=N.indiv,Y=Y,ind=ind,X=X,ind.spp=ind.spp,N.spp=N.spp)
  
  jm <- jags(model = "model.txt",data = input,inits=inits,param=params,
                n.chains = 3, #number of separate MCMC chains
                n.iter =15000, #number of iterations per chain
                n.thin=3, #thinning
                n.burnin = 5000) #number of initial iterations to discard
  
  attach.jags(jm)
  b0 = mean(b0)
  b1 = apply(b1,2,mean)
  plot(X,Y)
  for(i in 1:5) {abline(b0,b1[i],col="red")}
  for(i in 6:9) {abline(b0,b1[i],col="black")}
  
  
  
  
  
  
  dat = read.csv("/Users/fridley/Documents/academic/courses/StatsLifeSci/Stats2021/treegrowth.csv")
  
  
  N = length(dat$light)
  G = 3 #number of sites
  
  mod3 <- "model
{
    for(i in 1:N) {
        growth[i] ~ dnorm(mu[i],tau)
        mu[i] <- b0 + b1[site[i]]*light[i]  #note nested index for random slope
        + b.site[site[i]]           #group random intercept; note nested index!
                                    #could also do b0[site[i]] for random intercept
    }

    #site random effects
    for(i in 1:G) {
        b.site[i] ~ dnorm(0,tau.group)  #intercept varies by site
        b1[i] ~ dnorm(0,tau.light)      #light effect varies by site
    }

    #priors, fixed effects
    b0 ~ dnorm(0, 0.00001)  #flat normal prior for intercept
    #b1 ~ dnorm(0, 0.00001)  #flat normal prior for light effect

    #priors, variances
    tau <- sigma^-2 #coverts sd to precision
    sigma ~ dunif(0, 100)  #uniform prior for standard deviation
    tau.group <- sigma.group^-2 #coverts sd to precision
    sigma.group ~ dunif(0, 100)  #uniform prior for standard deviation
    tau.light <- sigma.light^-2 #coverts sd to precision
    sigma.light ~ dunif(0, 100)  #uniform prior for standard deviation

}" #end model
  
  write(mod3, "model.txt")
  #input lists for JAGS
  params = c("b0","b1","sigma","b.site","sigma.group","sigma.light") #parameters to monitor
  inits = function() list(b0=rnorm(1)) #starting values of fitted parameters
  input = list(N=N,G=G,growth=dat$growth,light=dat$light,site=dat$site) #input data
  
  #run JAGS model
  jags3 <- jags(model = "model.txt",data = input,inits=inits,param=params,
                n.chains = 3, #number of separate MCMC chains
                n.iter =15000, #number of iterations per chain
                n.thin=3, #thinning
                n.burnin = 5000) #number of initial iterations to discard
  
  jags3
>>>>>>> 3bdf405710fdcb11bcb5065d3ad3d02129828921
  