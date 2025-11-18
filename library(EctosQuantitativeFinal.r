library(EctosQuantitativeFinal.csv)
DF<-read.table(file="cjz-2022-0107supplb.csv",header=T,sep=",",na.strings="NA")
DF$YYYY<-as.factor(DF$YYYY)
DF$MM2<-as.factor(DF$MM2)
DF$GRID<-as.factor(DF$GRID)
DF$HANDLER_FIELD<-as.factor(DF$HANDLER_FIELD)
DF$ID<-as.factor(DF$ID)
DF$REPRO1<-as.factor(DF$REPRO1)


########SCALING VARIABLES##########################
DF$T_LOADz<-scale(DF$T_LOAD)
DF$MASSz  <-scale(DF$MASS_FIELD)
DF$Tempz  <-scale(DF$MeanTemp)
DF$Precipz<-scale(DF$TotPrecip)
DF$DENSdz <-scale(DF$DENSd)
DF$F_LOADz<-scale(DF$F_LOAD)
DF$B_LOADz<-scale(DF$B_LOAD)
DF$M_LOADz<-scale(DF$M_LOAD)

############################## MAIN analysis ####################################
############################## MAIN analysis ####################################
############################## MAIN analysis ####################################
############################## MAIN analysis ####################################
############################## MAIN analysis ####################################
############################## MAIN analysis ####################################
############################## MAIN analysis ####################################
#fit HURDLE model
fit_h_nbinom2<-glmmTMB(T_LOAD~DENSyc+MM2+Toosoon+GRID+Tempz+Precipz+SEXc+AGEc+REPROc+MASSz+DENSdz+F_LOADz+B_LOADz+M_LOADz+
                         (1|YYYY)+(1|ID)+(1|HANDLER_FIELD),
                       family="truncated_nbinom2"(link="log"),
                       data = DF,
                       ziformula=~DENSyc+MM2+Toosoon+GRID+Tempz+Precipz+SEXc+AGEc+REPROc+MASSz+DENSdz+F_LOADz+B_LOADz+M_LOADz+
                         (1|YYYY)+(1|ID)+(1|HANDLER_FIELD))
summary(fit_h_nbinom2)



#re-fit HURDLE model, but with a different variable for reproductive status
REPRO_model<-glmmTMB(T_LOAD~DENSyc+MM2+Toosoon+GRID+Tempz+Precipz+SEXc+AGEc+REPRO1+MASSz+DENSdz+F_LOADz+B_LOADz+M_LOADz+
                         (1|YYYY)+(1|ID)+(1|HANDLER_FIELD),
                       family="truncated_nbinom2"(link="log"),
                       data = DF,
                       ziformula=~DENSyc+MM2+Toosoon+GRID+Tempz+Precipz+SEXc+AGEc+REPRO1+MASSz+DENSdz+F_LOADz+B_LOADz+M_LOADz+
                         (1|YYYY)+(1|ID)+(1|HANDLER_FIELD))
summary(REPRO_model)

