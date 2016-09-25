c
c..##########################################################
c..#                                                        #
c..#  Decalaration of GLOBAL VARIABLES and COMMON BLOCKS .. #
c..#                                                        #
c..##########################################################
c
        INTEGER            Nc,nEvForMCInteg,nEvForMaxWt,ranGenType,
     &                     decPart,uPart,dPart,mcLUN,nTrial,nEv,channl
        DOUBLE PRECISION   sw,cw,sw2,cw2,pi,GFermi,alphaI
        DOUBLE PRECISION   spV(0:3),bet(0:3),Q(0:3)
        DOUBLE PRECISION   mz,mb,qb,mf1,mf2,qf1,qf2,wIS,vf,af,qf,mf
        DOUBLE PRECISION   k0Max,k0MaxKin,tHmu2,tLmu2,softWT,sumWt2,sumWt,
     &                     maxWt,Event(0:2,0:3)
        DOUBLE PRECISION   EWC,scheme,omega,mU,mD,zm,wm,hm
        LOGICAL            firstTime,interPrints,hardEvnt,softEvnt         
        CHARACTER*10       evntType
c...
        COMMON /EwcOmega/   
     &       EWC,
     &       scheme,
     &       omega,
     &       mU,
     &       mD,
     &       zm,
     &       wm,
     &       hm
        COMMON /physical_constants/     
     &       pi,
     &       GFermi,
     &       sw,
     &       cw,
     &       sw2,
     &       cw2,
     &       alphaI,
     &       Nc
        COMMON /Kleiss_Stirling/        
     &       spV,
     &       bet       
        COMMON /MC_entrance_parameters/ 
     &       mz,
     &       qb,
     &       Q,
     &       mb,
     &       mf1,
     &       mf2,
     &       qf1,
     &       qf2,
     &       wIS,
     &       vf,
     &       af,
     &       qf,
     &       mf,
     &       k0Max,
     &       k0MaxKin,
     &       tHmu2,
     &       tLmu2,
     &       softWT,
     &       decPart,
     &       uPart,
     &       dPart,
     &       mcLUN
        COMMON /others/                 
     &       sumWt2,
     &       sumWt,
     &       maxWt,
     &       Event,
     &       nTrial,
     &       nEv,
     &       ranGenType,
     &       nEvForMCInteg,
     &       nEvForMaxWt,
     &       firstTime,
     &       interPrints,
     &       hardEvnt,
     &       softEvnt,
     &       evntType
