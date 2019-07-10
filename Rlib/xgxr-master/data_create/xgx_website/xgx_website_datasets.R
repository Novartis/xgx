#libraries ----
library(gridExtra)
library(grid)
library(ggplot2)
library(dplyr)
library(RxODE)
library(caTools)
library(reshape)
library(tidyr)
rename = dplyr::rename
group_by = dplyr::group_by

#define the ode ----
ode <- "
Concentration = centr/V2;
C3 = peri/V3;
C4 = peri2/V4;
d/dt(depot) = -KA*depot;
d/dt(centr) =  KA*depot - (CL+Q+Q2)*Concentration + Q *C3 + Q2*C4;
d/dt(peri)  =                     Q*Concentration - Q *C3;
d/dt(peri2) =                    Q2*Concentration - Q2*C4;
d/dt(eff)   = Kin*(1+Emax*Concentration/(EC50 + Concentration)) - Kout*(1)*eff;
"

work <- tempfile("Rx_intro-")
mod1 <- RxODE(model = ode, modName = "mod1")

cat(gsub(pattern="\nConcentration",replacement="Concentration",x=gsub(pattern=";\n",replacement=" \n", x = ode)))

# Single Ascending Dose PK Dataset ----
set.seed(12345666)
ndose <- 5
nsub <- 10 # number of subproblems

SEX <- rep(c(rep("Male",ceiling(nsub/2)),rep("Female",floor(nsub/2))),ndose)
WT0 <- (SEX=="Male")*runif(length(SEX),60,110)+(SEX=="Female")*runif(length(SEX),50,100)
VCOV <- (WT0/70)*(1-0.5*(SEX=="Female"))
CLCOV <- (WT0/70)*(1-0.5*(SEX=="Female"))

CL <- 0.02*exp(rnorm(nsub*ndose,0,.4^2))*CLCOV
KA <- 0.5*exp(rnorm(nsub*ndose,0,.4^2))
V2 <- 30*exp(rnorm(nsub*ndose,0,.4^2))*VCOV
DOSE <- sort(rep(c(100,200,400,800,1600),nsub))
theta.all <- 
  cbind(KA=KA, CL=CL, V2=V2,
        Q=10*CLCOV, V3=100*VCOV, V4 = 10000*VCOV, Q2 = 10*CLCOV,
        Kin=0.5, Kout=0.05, EC50 = 1, Emax = 4)

# Set nominal times for sampling schedule
SAMPLING <- c(-0.1,0.1,0.5,1,2,4,8,12,18,23.9,36,47.9,71.9)

DOSING <- c(0)
NT <- data.frame(NT = c(SAMPLING, DOSING), label = c(rep("SAMPLING",length(SAMPLING)),rep("DOSING",length(DOSING))))
NT <- NT[order(NT$NT),]

ev.all <- list()
for(idose in 1:length(DOSE)){
  ev.all[[idose]] <- eventTable(amount.units='mg', time.units='hours')
  # ev.all[[idose]]$add.dosing(dose = DOSE[idose], nbr.doses = 1, dosing.interval = 24)
  # ev.all[[idose]]$add.sampling(c(c(-0.1,0.1,0.5,1,2,4,8,12,18,23.9,36,47.9,71.9)))
  
  # Include some noise in the sampling & dosing schedules
  temp <- NT
  temp$time <- cumsum(c(temp$NT[1] + 0.1*rnorm(1), abs(temp$NT[2:length(temp$NT)]-temp$NT[1:length(temp$NT)-1] + 0.1*rnorm(length(temp$NT)-1))))
  temp$time <- temp$time - temp$time[temp$NT==0] # center at first dose
  
  ev.all[[idose]]$add.dosing(dose = DOSE[idose], start.time = temp$time[temp$label=="DOSING"])
  ev.all[[idose]]$add.sampling(temp$time[temp$label=="SAMPLING"])
  
}

x.all.df <- NULL
ID = 0
for(i in 1:length(ev.all)){
  #   for(i in 1:nsub){
  ID = ID + 1
  theta <- theta.all[i,]
  inits <- c(depot = 0, centr = 0, peri = 0, peri2 = 0, eff=theta["Kin"]/theta["Kout"])
  x <- mod1$solve(theta, ev.all[[i]], inits = inits)
  
  x.df <- data.frame(x)
  x.df$NT <- SAMPLING
  
  temp <- data.frame(ev.all[[i]]$get.dosing())
  temp$NT <- DOSING
  
  temp[,setdiff(names(x.df),names(temp))]<- NA
  x.df[,setdiff(names(temp),names(x.df))]<- NA
  x.df <- rbind(x.df,temp)
  
  x.df$ID <- ID
  x.df$DOSE <- DOSE[i]
  x.df$WT0 <- WT0[i]
  x.df$SEX <- SEX[i]
  
  if(is.null(x.all.df)){
    x.all.df <- x.df
  }else{
    x.all.df <- rbind(x.all.df, x.df)
  }
}
x.all.df$DV <- x.all.df$Concentration*(exp(rnorm(length(x.all.df$Concentration),0,0.6^2))) + 
  0.01*rnorm(length(x.all.df$Concentration))
x.all.df$DV[x.all.df$DV<0.05]=0.05
x.all.df$DV[x.all.df$Concentration==0]=NA
x.all.df$BLQ=0
x.all.df$BLQ[x.all.df$DV==0.05]=1

my.data <- x.all.df[!is.na(x.all.df$amt),]
my.data$CMT <- 1
my.data$CMT_label <- "Dosing"

temp <- x.all.df[is.na(x.all.df$amt),]
temp$CMT <- 2
temp$CMT_label <- "PK Concentration"
my.data <- rbind(my.data,temp)

my.data$DV <- signif(my.data$DV,3)
my.data$WT0 <- signif(my.data$WT0,3)
my.data$time <- round(my.data$time,3)

my.data <- my.data[order(my.data$ID,my.data$time, my.data$CMT),]

my.data$DOSE_label <- paste(my.data$DOSE,"mg")
my.data$DOSE_label[my.data$DOSE==0] <- "Placebo"
my.data$DOSE_label <- factor(my.data$DOSE_label,levels = c("Placebo",paste(unique(my.data$DOSE[my.data$DOSE!=0]),"mg")))

Single_Ascending_Dose_Dataset <- my.data[,c("ID","time","NT","amt","DV","CMT","CMT_label","BLQ","evid","WT0","SEX","DOSE","DOSE_label")]
#write.csv(Single_Ascending_Dose_Dataset,"data_create/xgx_website/Single_Ascending_Dose_old.csv",row.names = FALSE)

Single_Ascending_Dose_Dataset = Single_Ascending_Dose_Dataset %>%
  mutate(MDV = ifelse(CMT==1 | (CMT==2 & time<0),1,0))

Single_Ascending_Dose_Dataset2 <- Single_Ascending_Dose_Dataset
Single_Ascending_Dose_Dataset2$PROFTIME <- Single_Ascending_Dose_Dataset2$NT
Single_Ascending_Dose_Dataset2$NOMTIME <- Single_Ascending_Dose_Dataset2$NT
Single_Ascending_Dose_Dataset2$YTYPE <- Single_Ascending_Dose_Dataset2$CMT
Single_Ascending_Dose_Dataset2$amt[is.na(Single_Ascending_Dose_Dataset2$amt)] <- 0 
Single_Ascending_Dose_Dataset2 <- Single_Ascending_Dose_Dataset2[!(Single_Ascending_Dose_Dataset2$DOSE_label=="Placebo"&
                                                                     Single_Ascending_Dose_Dataset2$CMT==2),] 
Single_Ascending_Dose_Dataset2$BLQ[Single_Ascending_Dose_Dataset2$DOSE_label=="Placebo"&
                                     Single_Ascending_Dose_Dataset2$CMT==2] <- 1L
Single_Ascending_Dose_Dataset2$evid <- plyr::mapvalues(Single_Ascending_Dose_Dataset2$evid, c(NA,101), c(0,1))
Single_Ascending_Dose_Dataset2$TIMEUNIT <- "Hours"
Single_Ascending_Dose_Dataset2$EVENTU[Single_Ascending_Dose_Dataset2$CMT==1] <- "mg"
Single_Ascending_Dose_Dataset2$EVENTU[Single_Ascending_Dose_Dataset2$CMT==2] <- "ng/mL"

Single_Ascending_Dose_Dataset2 <- Single_Ascending_Dose_Dataset2[,c("ID","time","NOMTIME","TIMEUNIT","amt","DV","MDV","CMT",
                                                                    "CMT_label","EVENTU","BLQ","evid","WT0","SEX","DOSE_label",
                                                                    "DOSE")]
names(Single_Ascending_Dose_Dataset2) <- c("ID","TIME","NOMTIME","TIMEUNIT","AMT","LIDV","MDV","CMT",
                                           "NAME","EVENTU","CENS","EVID","WEIGHTB","SEX","TRTACT",
                                           "DOSE")

Single_Ascending_Dose = Single_Ascending_Dose_Dataset2
write.csv(Single_Ascending_Dose,"data_create/xgx_website/Single_Ascending_Dose.csv",row.names = FALSE, quote = FALSE)
write.csv(Single_Ascending_Dose,           "inst/extdata/Single_Ascending_Dose.csv",row.names = FALSE, quote = FALSE)
usethis::use_data(Single_Ascending_Dose, overwrite = TRUE)


DT::datatable(Single_Ascending_Dose, rownames = FALSE, options = list(autoWidth = TRUE, scrollX=TRUE)) 

# Multiple Ascending Dose PK & PD Dataset ----
set.seed(12345666)
ndose <- 6
nsub <- 10 # number of subproblems

SEX <- rep(c(rep("Male",ceiling(nsub/2)),rep("Female",floor(nsub/2))),ndose)
WT0 <- (SEX=="Male")*runif(length(SEX),60,110)+(SEX=="Female")*runif(length(SEX),50,100)
VCOV <- (WT0/70)*(1-0.5*(SEX=="Female"))
CLCOV <- (WT0/70)*(1-0.5*(SEX=="Female"))

CL <- 0.02*exp(rnorm(nsub*ndose,0,.4^2))*CLCOV
KA <- 0.5*exp(rnorm(nsub*ndose,0,.4^2))
V2 <- 30*exp(rnorm(nsub*ndose,0,.4^2))*VCOV

Kin <- 0.1*exp(rnorm(nsub*ndose,0,0.4^2))
DOSE <- sort(rep(c(0,100,200,400,800,1600),nsub))
theta.all <- 
  cbind(KA=KA, CL=CL, V2=V2,
        Q=10*CLCOV, V3=100*VCOV, V4 = 10000*VCOV, Q2 = 10*CLCOV,
        Kin=Kin, Kout=0.01, EC50 = 1, Emax = 2+2*(SEX=="Female"))

# Nominal times for sampling schedule
SAMPLING <- c(c(-24,-0.1,0.1,0.5,1,2,4,8,12,18,23.9),23.9+seq(1,4)*24,
              5*24 + c(0.1,0.5,1,2,4,8,12,18,23.9,36,47.9,71.9,95.9))
DOSING <- seq(0,5*24,24)
NT <- data.frame(NT = c(SAMPLING, DOSING), label = c(rep("SAMPLING",length(SAMPLING)),rep("DOSING",length(DOSING))))
NT <- NT[order(NT$NT),]

ev.all <- list()
for(idose in 1:length(DOSE)){
  ev.all[[idose]] <- eventTable(amount.units='mg', time.units='hours')
  # ev.all[[idose]]$add.dosing(dose = DOSE[idose], nbr.doses = 6, dosing.interval = 24)
  # ev.all[[idose]]$add.sampling(c(c(-24,-0.1,0.1,0.5,1,2,4,8,12,18,23.9),23.9+seq(1,4)*24,
  #                                5*24 + c(0.1,0.5,1,2,4,8,12,18,23.9,36,47.9,71.9,95.9)))
  
  # Include some noise in the sampling & dosing schedules
  temp <- NT
  temp$time <- cumsum(c(temp$NT[1] + 0.1*rnorm(1), abs(temp$NT[2:length(temp$NT)]-temp$NT[1:length(temp$NT)-1] + 0.1*rnorm(length(temp$NT)-1))))
  temp$time <- temp$time - temp$time[temp$NT==0] # center at first dose
  
  ev.all[[idose]]$add.dosing(dose = DOSE[idose], start.time = temp$time[temp$label=="DOSING"])
  ev.all[[idose]]$add.sampling(temp$time[temp$label=="SAMPLING"])
  
}

x.all.df <- NULL
ID = 0
for(i in 1:length(ev.all)){
  ID = ID + 1
  theta <- theta.all[i,]
  inits <- c(depot = 0, centr = 0, peri = 0, peri2 = 0, eff=theta["Kin"]/theta["Kout"])
  x <- mod1$solve(theta, ev.all[[i]], inits = inits)
  
  x.df <- data.frame(x)
  x.df$NT <- SAMPLING
  
  temp <- data.frame(ev.all[[i]]$get.dosing())
  temp$NT <- DOSING
  temp[,setdiff(names(x.df),names(temp))]<- NA
  x.df[,setdiff(names(temp),names(x.df))]<- NA
  x.df <- rbind(x.df,temp)
  
  x.df$ID <- ID
  x.df$DOSE <- DOSE[i]
  x.df$WT0 <- WT0[i]
  x.df$SEX <- SEX[i]
  
  if(is.null(x.all.df)){
    x.all.df <- x.df
  }else{
    x.all.df <- rbind(x.all.df, x.df)
  }
}
x.all.df$DV <- x.all.df$Concentration*(exp(rnorm(length(x.all.df$Concentration),0,0.6^2))) + 
  0.01*rnorm(length(x.all.df$Concentration))
x.all.df$DV[x.all.df$DV<0.05]=0.05
x.all.df$DV[x.all.df$Concentration==0]=NA
x.all.df$BLQ <- 0
x.all.df$BLQ[x.all.df$DV==0.05] <- 1

temp <- exp(rnorm(length(x.all.df$eff),0,0.4^2))
x.all.df$eff2 <- x.all.df$eff*temp + rnorm(length(x.all.df$eff),0,2) + 10*x.all.df$time/(72 + x.all.df$time)

temp <- runif(length(x.all.df$eff),0,1)
x.all.df$Response <- 0
x.all.df$Response[0.2*x.all.df$time/(72 + x.all.df$time) + exp((x.all.df$eff2-30))/(1+exp((x.all.df$eff2-30))) > temp] <- 1 


x.all.df$Severity <- 3
x.all.df$Severity[0.2*x.all.df$time/(72 + x.all.df$time) + exp((x.all.df$eff2-18))/(1+exp((x.all.df$eff2-18))) > temp] <- 2
x.all.df$Severity[0.2*x.all.df$time/(72 + x.all.df$time) + exp((x.all.df$eff2-28))/(1+exp((x.all.df$eff2-28))) > temp] <- 1
x.all.df$Severity_label <- plyr::mapvalues(x.all.df$Severity,c(1,2,3),c("mild","moderate","severe"))
x.all.df$Severity_label <- factor(x.all.df$Severity_label, levels = unique(x.all.df$Severity_label[order(x.all.df$Severity)]))

x.all.df$Count <- NA
options(warn=-1)
  x.all.df$Count[!is.na(x.all.df$eff2)]<-rpois(length(na.omit(x.all.df$eff2)), na.omit(10*( 0.5/(1 + x.all.df$time/24) + 0.5*exp(-((x.all.df$eff2)-28))/(1+exp(-((x.all.df$eff2)-28))))) )
options(warn=1)

my.data <- x.all.df[!is.na(x.all.df$amt),]
my.data$CMT_label <- "Dosing"
my.data$CMT <- 1

temp <- x.all.df[is.na(x.all.df$amt),]
temp$CMT <- 2
temp$CMT_label <- "PK Concentration"
my.data <- rbind(my.data,temp)

temp <- x.all.df[x.all.df$NT%in%c(-0.1,23.9,(23.9+seq(1,9)*24)),]
temp$DV <- temp$eff2
temp$CMT <- 3
temp$CMT_label <- "PD - Continuous"
my.data <- rbind(my.data,temp)

temp <- x.all.df[x.all.df$NT%in%c(-0.1,23.9,23.9+seq(1,9)*24),]
temp$DV <- temp$Count
temp$CMT <- 4
temp$CMT_label <- "PD - Count"
my.data <- rbind(my.data,temp)

temp <- x.all.df[x.all.df$NT%in%c(-24,23.9,23.9+seq(1,9)*24),]
temp$DV <- temp$Severity
temp$CMT <- 5
temp$CMT_label <- "PD - Ordinal"
my.data <- rbind(my.data,temp)

temp <- x.all.df[x.all.df$NT%in%c(-0.1,23.9,(23.9+seq(1,9)*24)),]
temp$DV <- temp$Response
temp$CMT <- 6
temp$CMT_label <- "PD - Binary"
my.data <- rbind(my.data,temp)


my.data$DV <- signif(my.data$DV,3)
my.data$WT0 <- signif(my.data$WT0,3)
my.data$time <- round(my.data$time,3)

my.data <- my.data[order(my.data$ID,my.data$time, my.data$CMT),]

my.data$DOSE_label <- paste(my.data$DOSE,"mg")
my.data$DOSE_label[my.data$DOSE==0] <- "Placebo"
my.data$DOSE_label <- factor(my.data$DOSE_label,levels = c("Placebo",paste(unique(my.data$DOSE[my.data$DOSE!=0]),"mg")))

my.data$DAY <- floor(my.data$NT/24)+1
my.data$DAY_label <- paste("Day",ceiling(my.data$DAY))
my.data$DAY_label[my.data$DAY<=0]<-"Baseline"
my.data$DAY_label <- factor(my.data$DAY_label,
                            levels = c("Baseline",paste("Day",sort(unique(ceiling(my.data$DAY))))))
my.data$CYCLE <- my.data$DAY
my.data$CYCLE[my.data$CYCLE>6] <- 6

Multiple_Ascending_Dose_Dataset <- my.data[,c("ID","time","NT","amt","DV","CMT","CMT_label","BLQ","evid","WT0","SEX","DOSE_label","DOSE","DAY","DAY_label","Response","Severity","Severity_label","Count","CYCLE")]

#write.csv(Multiple_Ascending_Dose_Dataset,"data_create/xgx_website/Multiple_Ascending_Dose_old.csv",row.names = FALSE)

Multiple_Ascending_Dose_Dataset2 <- Multiple_Ascending_Dose_Dataset
Multiple_Ascending_Dose_Dataset2$PROFTIME <- Multiple_Ascending_Dose_Dataset2$NT - (Multiple_Ascending_Dose_Dataset2$CYCLE-1)*24
Multiple_Ascending_Dose_Dataset2$NOMTIME <- Multiple_Ascending_Dose_Dataset2$NT
Multiple_Ascending_Dose_Dataset2$YTYPE <- Multiple_Ascending_Dose_Dataset2$CMT
Multiple_Ascending_Dose_Dataset2$amt[is.na(Multiple_Ascending_Dose_Dataset2$amt)] <- 0 
Multiple_Ascending_Dose_Dataset2 <- Multiple_Ascending_Dose_Dataset2[!(Multiple_Ascending_Dose_Dataset2$DOSE_label=="Placebo"&
                                                                         Multiple_Ascending_Dose_Dataset2$CMT==2),] 
Multiple_Ascending_Dose_Dataset2$BLQ[Multiple_Ascending_Dose_Dataset2$DOSE_label=="Placebo"&
                                       Multiple_Ascending_Dose_Dataset2$CMT==2] <- 1L
Multiple_Ascending_Dose_Dataset2$evid <- plyr::mapvalues(Multiple_Ascending_Dose_Dataset2$evid, c(NA,101), c(0,1))
Multiple_Ascending_Dose_Dataset2$TIMEUNIT <- "Hours"
Multiple_Ascending_Dose_Dataset2$EVENTU[Multiple_Ascending_Dose_Dataset2$CMT==1] <- "mg"
Multiple_Ascending_Dose_Dataset2$EVENTU[Multiple_Ascending_Dose_Dataset2$CMT==2] <- "ng/mL"
Multiple_Ascending_Dose_Dataset2$EVENTU[Multiple_Ascending_Dose_Dataset2$CMT==3] <- "IU/L"
Multiple_Ascending_Dose_Dataset2$EVENTU[Multiple_Ascending_Dose_Dataset2$CMT==4] <- "count"
Multiple_Ascending_Dose_Dataset2$EVENTU[Multiple_Ascending_Dose_Dataset2$CMT==5] <- "severity"
Multiple_Ascending_Dose_Dataset2$EVENTU[Multiple_Ascending_Dose_Dataset2$CMT==6] <- "response"

Multiple_Ascending_Dose_Dataset2 = Multiple_Ascending_Dose_Dataset2 %>%
  mutate(MDV = ifelse(CMT==1 | (CMT==2 & time<0),1,0))

Multiple_Ascending_Dose_Dataset2 <- Multiple_Ascending_Dose_Dataset2[,c("ID","time","NOMTIME","TIMEUNIT","amt","DV","MDV","CMT",
                                                                        "CMT_label","EVENTU","BLQ","evid","WT0","SEX","DOSE_label",
                                                                        "DOSE","DAY","PROFTIME","CYCLE")]
names(Multiple_Ascending_Dose_Dataset2) <- c("ID","TIME","NOMTIME","TIMEUNIT","AMT","LIDV","MDV","CMT",
                                             "NAME","EVENTU","CENS","EVID","WEIGHTB","SEX","TRTACT",
                                             "DOSE","PROFDAY","PROFTIME","CYCLE")

Multiple_Ascending_Dose = Multiple_Ascending_Dose_Dataset2

write.csv(Multiple_Ascending_Dose,"data_create/xgx_website/Multiple_Ascending_Dose.csv",row.names = FALSE, quote = FALSE)
write.csv(Multiple_Ascending_Dose,           "inst/extdata/Multiple_Ascending_Dose.csv",row.names = FALSE, quote = FALSE)
usethis::use_data(Multiple_Ascending_Dose, overwrite = TRUE)


DT::datatable(Multiple_Ascending_Dose, rownames = FALSE, options = list(autoWidth = TRUE, scrollX=TRUE) )

#NCA dataset ----
my.data = Multiple_Ascending_Dose_Dataset2
NCA_0_24 = my.data %>%
  filter(CMT==2, NOMTIME>0, NOMTIME<=24) %>%
  group_by(ID) %>%
  summarize(AUC_0_24     = trapz(TIME,LIDV),
            Cmax_0_24    = max(LIDV),
            Ctrough_0_24 = LIDV[length(LIDV)])

NCA_tau = my.data %>%
  filter(CMT==2, NOMTIME>120, NOMTIME<=144) %>%
  group_by(ID) %>%
  summarize(AUC_tau     = trapz(TIME,LIDV),
            Cmax_tau    = max(LIDV),
            Ctrough_tau = LIDV[length(LIDV)])

NCA = left_join(NCA_0_24,NCA_tau,by="ID") %>%
  gather(PARAM,VALUE,-ID) %>%
  left_join(my.data[,c("ID","DOSE","TRTACT","SEX","WEIGHTB")],by="ID")

Multiple_Ascending_Dose_NCA = NCA
write.csv(NCA,"data_create/xgx_website/Multiple_Ascending_Dose_NCA.csv",row.names = FALSE, quote = FALSE)
write.csv(NCA,           "inst/extdata/Multiple_Ascending_Dose_NCA.csv",row.names = FALSE, quote = FALSE)
usethis::use_data(Multiple_Ascending_Dose_NCA, overwrite = TRUE)


#MAD with missing and duplicate values ----
my.data = Multiple_Ascending_Dose_Dataset2
PK.indices = which(my.data$CMT==2 & my.data$TIME > 0)

#add some missing PK data for realism
ind.missing = sample(PK.indices,7)
my.data$LIDV[ind.missing] = NA

#add some duplicated time points for realism
ind.duplicate = sample(PK.indices,8)
my.data = bind_rows(my.data,my.data[ind.duplicate,])
my.data = my.data %>%
  arrange(ID,TIME,CMT)

# Define order for factors
Multiple_Ascending_Dose_Missing_Duplicates = my.data
write.csv(my.data,"data_create/xgx_website/Multiple_Ascending_Dose_Missing_Duplicates.csv",row.names = FALSE, quote = FALSE)
write.csv(my.data,           "inst/extdata/Multiple_Ascending_Dose_Missing_Duplicates.csv",row.names = FALSE, quote = FALSE)
usethis::use_data(Multiple_Ascending_Dose_Missing_Duplicates, overwrite = TRUE)

#check the data and also add it to the package
if (FALSE) {
  x = xgx_check_data(my.data,c("WEIGHTB","SEX")) 
  
  xgx_data_pkpd_mad = my.data
  usethis::use_data(xgx_data_pkpd_mad,overwrite = TRUE)
}

#also add theophylline to the dataset
nlmixr_theo_sd = read.csv("./inst/extdata/nlmixr_theo_sd.csv")
usethis::use_data(nlmixr_theo_sd,overwrite=TRUE)


                     