#Deriving photosynthetic properties, NSF-IOS project, version 1
#JDF 2-18-22

#### Libraries


#### Dataset: on OneDrive

#Japan
dat = read.csv("C:\\Users\\Fridley\\OneDrive - Syracuse University\\IOS_data\\Japan_licor_rev.csv")
str(dat)


#### Variable inspection

  #Anet (umol CO2 per m2 per s)
  hist(dat$Photo) #values >40 exceptional
  plot(dat$Area,dat$Photo) #don't seem to be caused by errors of leaf Area (except one extreme value ~100)

  #Conductance to H2O (mol H20 per m2 per s)
  hist(dat$Cond) #substantial right skew
  
  #Intercelluar CO2 conc (umol CO2 per mol)
  hist(dat$Ci) #seems good
  
    #convert Ci to Pa units if needed
    dat$Ci_Pa = dat$Ci * dat$Press/ 1000
  
  #Transpiration rate (mm H2O per m2 per s)
  hist(dat$Trmmol) #ok
  
  #VPD based on leaf temp (kPa)
  hist(dat$VpdL) #ok
  
  #Leaf area (cm2)
  hist(dat$Area) #ok
  
  #Stomatal Ratio (0-1; 1 is all stomata on underside of leaf)
  summary(dat$StmRat) #all assumed 1
  
  #Boundary layer conductance (mol per m2 per s)
  hist(dat$BLCond) #are values > 3 due to smaller leaf area?
  plot(dat$Area,dat$BLCond) #yes, linear decrease for some
  
  #Air temp
  hist(dat$Tair)
  quantile(prob=c(.025,.975),dat$Tair) #95% within 28 and 33C 
  
  #Leaf temp
  hist(dat$Tleaf)
  quantile(prob=c(.025,.975),dat$Tleaf) #95% within 26 and 34.5; need to consider in curve fitting?
    length(dat$Tleaf[dat$Tleaf>28&dat$Tleaf<32])/length(dat$Tleaf)
    #BUT 83% within 2C of 30; 
    
  #ignoring TBlk, CO2R, CO2S, H2OR, H2OS, RH_R, RH_S
    
  #Flow rate
  hist(dat$Flow) #constant at 500 umol per s
  
  #PAR on leaf (umol photos per m2 per s)
  hist(dat$PARi) #ok
  
  #ambient PAR (umol photos per m2 per s)
  hist(dat$PARo) #majority of samples in shade or heavy cloud
  
  #Atm pressure (kPa)
  hist(dat$Press) #std atm is 101.3; some relatively low values must be higher elevation
  
  #Stability status of licor
  table(dat$Status) #all 111115 except 4 are 111135
  
  #Labels:
  table(dat$filename)
  table(dat$date)
  table(dat$site)
  table(dat$species)  
  table(dat$sppcode)    

#### Temperature adjusted coefficients (Mason's code)
  
  # Constants published in Sharkey et al (2007) Plant Cell Env 30: 1035-1040 
  # Measured using transgenic tobacco (ASSUMED to be similar across higher plants)
  # Ci units in Pa; Sharkey et al (2007) recommend partial pressures
  # **Be sure units are correct for your input data** (Ci is in Pa or ppm?)
  
  R=0.008314 #(kJ mol^-1 K^-1)
  dat$Kc=exp(35.9774-80.99/(R*(dat$Tleaf+273.15))) #Michaelis-Menten constant for Rubisco for O2 (Pa)
  dat$Ko=exp(12.3772-23.72/(R*(dat$Tleaf+273.15))) #Michaelis-Menten constant for Rubisco for CO2 (kPa)
  dat$GammaStar=exp(11.187-24.46/(R*(dat$Tleaf+273.15))) #Photorespiration compensation point (Pa)
  dat$O=21 #oxygen (O2) partial pressure (kPa)  
    

#### Example A-Ci analysis 1

#loop over species
  spp = unique(dat$species)
  par(mfrow=c(3,3),oma=c(1.5,1.5,3,1),mar=c(3,3,0,0))
  
  for(i in 2:5) {  
  eg = dat[dat$species==spp[i],]
  table(eg$filename)
  #eg = eg[eg$filename==levels(as.factor(eg$filename))[1],]
  eg = eg[eg$PARi>1010,] #only saturaing light
    
  #par(mar=c(3,3,0,0),oma=c(1.5,1.5,3,1))
  plot(eg$Ci_Pa,eg$Photo,ylab="", xlab="",cex.lab=1.2,cex.axis=1.5,cex=2)
  mtext(expression("Intercellular "*CO[2]*" Pressure (Pa)"),side=1,line=3.3,cex=1.5)
  mtext(expression(A[net]*" ("*mu*"mol "*CO[2]*" "*m^-2*s^-1*")"),side=2,line=2.5,cex=1.5)
  points(eg$Ci_Pa,eg$Photo,col=as.numeric(as.factor(eg$filename)),pch=19,cex=2)
  mtext(spp[i],line=-20,at=100,cex=2)
  
  # ---RuBisCO limited portion---
  #(Vcmax*(Ci_Pa-GammaStar))/(Ci_Pa+(Kc*(1+(O/Ko)))))-Rd
  
  # ---RUBP limited portion---
  #((J*(Ci_Pa-GammaStar))/((4*Ci_Pa)+(8*GammaStar)))-Rd
  
  # Simultaneous estimation method described by Dubois et al. 2007 New Phyt 176:402-414
  # Could change optimization algorithm (default here is Gauss-Newton)
  # Could also do a "grid search" if estimates are sensitive to starting values
  
    #Version 1: nls (all individuals fit together, no REs), using Mason's code
  
    aci.fit <- nls(Photo~ifelse(((Vcmax*(Ci_Pa-GammaStar))/(Ci_Pa+(Kc*(1+(O/Ko)))))<((J*(Ci_Pa-GammaStar))/((4*Ci_Pa)+(8*GammaStar))),((Vcmax*(Ci_Pa-GammaStar))/(Ci_Pa+(Kc*(1+(O/Ko))))),((J*(Ci_Pa-GammaStar))/((4*Ci_Pa)+(8*GammaStar))))-Rd,start=list(Vcmax=50,J=100,Rd=0.5),data=eg) 
  
    summary(aci.fit)
  
    Vcmax<-summary(aci.fit)$coef[1,1]
    J<-summary(aci.fit)$coef[2,1]
    Rd<-summary(aci.fit)$coef[3,1]
    nls.param = c(Vcmax,J,Rd)
  
    par(mar=c(3,3,0,0),oma=c(1.5,1.5,1,1))
    plot(eg$Ci_Pa,eg$Photo,ylab="", xlab="",cex.lab=1.2,cex.axis=1.5,cex=2)
    mtext(expression("Intercellular "*CO[2]*" Pressure (Pa)"),side=1,line=3.3,cex=1.5)
    mtext(expression("Net photosynthetic rate (umol "* CO[2]* m^-2*s^-1*")"),side=2,line=2.5,cex=1.5)
    curve(ifelse(((Vcmax*(x+mean(eg$GammaStar)))/(x+(mean(eg$Kc)*(1+(O/mean(eg$Ko))))))<((J*(x+mean(eg$GammaStar)))/((4*x)+(8*mean(eg$GammaStar)))),((Vcmax*(x+mean(eg$GammaStar)))/(x+(mean(eg$Kc)*(1+(O/mean(eg$Ko)))))),((J*(x+mean(eg$GammaStar)))/((4*x)+(8*mean(eg$GammaStar)))))-Rd,add=T)

    #Version 2: inclue random intercepts for individuals using nlme
    
    library(nlme)
    eg$O = O
    aci.fit2<-nlme(model=Photo~ifelse(((Vcmax*(Ci_Pa-GammaStar))/(Ci_Pa+(Kc*(1+(O/Ko)))))<((J*(Ci_Pa-GammaStar))/((4*Ci_Pa)+(8*GammaStar))),((Vcmax*(Ci_Pa-GammaStar))/(Ci_Pa+(Kc*(1+(O/Ko))))),((J*(Ci_Pa-GammaStar))/((4*Ci_Pa)+(8*GammaStar))))-Rd,start=c(Vcmax=nls.param[1],J=nls.param[2],Rd=nls.param[3]),data=eg,groups=~filename,fixed=Vcmax+J+Rd~1,random=Vcmax+J+Rd~1,control=list(msMaxIter=1000)) 
  
    summary(aci.fit2)  
  
    Vcmax<-aci.fit2$coef$fixed[1]
    J<-aci.fit2$coef$fixed[2]
    Rd<-aci.fit2$coef$fixed[3]
    
    curve(ifelse(((Vcmax*(x+mean(eg$GammaStar)))/(x+(mean(eg$Kc)*(1+(O/mean(eg$Ko))))))<((J*(x+mean(eg$GammaStar)))/((4*x)+(8*mean(eg$GammaStar)))),((Vcmax*(x+mean(eg$GammaStar)))/(x+(mean(eg$Kc)*(1+(O/mean(eg$Ko)))))),((J*(x+mean(eg$GammaStar)))/((4*x)+(8*mean(eg$GammaStar)))))-Rd,add=T,col="red")
 
    par(add=F)
}    
  
  #many curves aren't working
  #variance requires light levels: need to estimate params individually; use nlme predict?
  
  #fit all data simultaneously       
  library(nlme)
  
  aci.fit2 <- nlme(
    model=Photo~ifelse(((Vcmax*(Ci_Pa-GammaStar))/(Ci_Pa+(Kc*(1+(O/Ko)))))<((J*(Ci_Pa-GammaStar))/((4*Ci_Pa)+(8*GammaStar))),((Vcmax*(Ci_Pa-GammaStar))/(Ci_Pa+(Kc*(1+(O/Ko))))),((J*(Ci_Pa-GammaStar))/((4*Ci_Pa)+(8*GammaStar))))-Rd,
    start=c(Vcmax=50,J=100,Rd=.5),
    data=dat[200:400,],
    #groups=~species,
    fixed=Vcmax+J+Rd~1,
    random=Vcmax+J+Rd~1|species/filename,
    control=list(msMaxIter=1000)  )

  summary(aci.fit2) 
  
  #breaks easily
  
  ##Try ecophys package: works well; note BETH is light curves only
  library(plantecophys)

  spp = unique(dat$species)
for(i in 1:length(spp)) {
  df = dat[dat$species==spp[i],]
  df = df[df$PARi>1010,]
  df = df[df$Ci>=0,]
  
  if(spp[i]=="Berberis thunbergii") next #only light curves for this species
  
  f = fitacis(df,
             varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci", PPFD = "PARi", Rd = "Rd", Patm = "Press"),
             Tcorrect=F,
             Patm = mean(dat$Press),
             group = "filename",
             fitmethod = "bilinear" #interestingly, bilinear works and default doesn't
            )
  par(mar=c(5,5,5,5),mfrow=c(1,1))
  plot(f,how="oneplot")
  title(spp[i])
  summary(f)
  coef(f)
  readline()
}  
    