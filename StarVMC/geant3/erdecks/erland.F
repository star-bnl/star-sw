*
* $Id: erland.F,v 1.1.1.2 2009/02/01 17:10:15 fisyak Exp $
*
* $Log: erland.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:15  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:46:28  fisyak
* Merge with macos version
*
* Revision 1.2  2007/03/31 13:47:09  brun
*  From Andrea Fontana and Alberto Rotondi
* Added new calculation for sigma of straggling in energy loss to include
* in Geane the Urban/Landau approximation, as explained in the Geant
* manual and related papers.
* The model parametrization can be controlled with a new user variable
* (GCALPHA) in GCCUTS common block: 1 is for old gaussian model valid for
* dense materials, other values (see the enclosed report) are for gaseous
* materials.
*
* Revision 1.1.1.1  2002/07/24 15:56:26  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:35  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:15  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 15:37:34  mclareni
* Add geane321 source directories
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.49  by  S.Giani
*-- Author :
      SUBROUTINE ERLAND (STEP, Z, A, RHO, P, E, XMASS, DEDX, DEDX2)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *  Calculates energy straggling using Gaussian theory in a step  *
C.    *                                                                *
C.    *  Input  STEP   =  current step-length (cm)                     *
C.    *         Z      =  Z of the material                            *
C.    *         A      =  A of the material                            *
C.    *         RHO    =  density of the material                      *
C.    *         P      =  momentum of the particle                     *
C.    *         E      =  energy   of the particle                     *
C.    *         XMASS  =  mass     of the particle                     *
C.    *                                                                *
C.    *  Output DEDX2  =  mean square of the straggling in G           *
C.    *                                                                *
C.    *    ==>Called by : ERTRCH                                       *
C.    *       Author    E.Nagy  *********                              *
C.    *       Original routine : GLANDO                                *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gconst.inc"
#include "geant321/gccuts.inc"
#include "geant321/gcmore.inc"
*
      REAL*4 CMED, DE, ION, RU, FU1, FU2, EU1, EU2, EU2F, DEDX
      REAL*4 EUMED,SIG1, SIG2, SIG3, SIG4,CN1, CN2, CN3, XUMASS 
      REAL*4 EUMED2, E99, A99 
      REAL*4 SALPHA
*
      IF (STEP.LT.1.E-7) THEN
         DEDX2=0.
         RETURN
      ENDIF
*
*     Calculate xi factor (KeV).
      BETA   = P/E
      GAMMA  = E/XMASS
      XI     = (153.5*Z*STEP*RHO)/(A*BETA*BETA)
*
*     Maximum energy transfer to atomic electron (KeV).
      ETA    = BETA*GAMMA
      ETASQ  = ETA*ETA
      RATIO  = EMASS/XMASS
      F1     = 2.*EMASS*ETASQ
      F2     = 1.+2.*RATIO*GAMMA+RATIO*RATIO
      EMAX   = 1.E+6*F1/F2
       
*     old GEANE standard  straggling sigma in Gev**2
      A99 = GCALPHA    
      IF (A99.EQ.1.) THEN  
          DEDX2 = XI*EMAX*(1.-(BETA*BETA/2.))*1.E-12
c          PRINT *,XI/EMAX,A99,A,Z,RHO,DEDX2
          RETURN           	   		   
      ENDIF
      
C      new calculation for sigma of straggling
C      by A. Fontana and A. Rotondi  March 2007
      SALPHA=0.
      IF(XI/EMAX .GT. 0.005)  THEN
C           Vavilov-Gaussian regime:  sigma^2(E)  in GeV^2
         DEDX2 = XI*EMAX*(1.-(BETA*BETA/2.))*1.E-12
      ELSE 
C
C          Urban/Landau approximation
C
C
C          energies in eV
C
         ION=16.*Z**0.9
         RU=0.4
         FU2= 0
         IF(Z .GT. 2) FU2=2./Z
         FU1= 1.-FU2
         EU2 =10.*Z*Z
         EU2F=EU2**FU2
         EU1= (ION/EU2F)**(1./FU1)  

C          energies in keV
         ION=ION/1000.
         EU1=EU1/1000.
         EU2=EU2/1000.
         XUMASS = XMASS*1.E+06
         CMED=DEDX*1.E+06/STEP
	 E99 = ION/(1.- A99*EMAX/(EMAX+ION))
         
         SIG1=(CMED*FU1/EU1)*(ALOG(2.*XUMASS*((BETA*GAMMA)**2)/EU1)
     +        - BETA*BETA)
         SIG1= (1-RU)*SIG1/(ALOG(2.*XUMASS*((BETA*GAMMA)**2)/ION)
     +        -BETA*BETA)
         SIG2=(CMED*FU2/EU2)*(ALOG(2.*XUMASS*((BETA*GAMMA)**2)/EU2) 
     +        - BETA*BETA)
         SIG2= (1-RU)*SIG2/(ALOG(2.*XUMASS*((BETA*GAMMA)**2)/ION)
     +        -BETA*BETA)
         SIG3= CMED*RU*EMAX/(ION*(EMAX+ION)*ALOG((EMAX+ION)/ION))
C         number of collisions
         CN1=SIG1*STEP
         CN2=SIG2*STEP
         CN3=SIG3*STEP
         IF(CN1+CN2+CN3 .LT. 50.) THEN
C            Urban model
            EUMED= (ION*(EMAX+ION)/EMAX)*ALOG(E99/ION)
	    EUMED2 = (ION*(EMAX+ION)/EMAX)*(E99-ION)
	    SIG4= EUMED2 - EUMED*EUMED
            DEDX2=CN1*EU1*EU1 + CN2*EU2*EU2 + CN3*EUMED*EUMED 
     +             + CN3*SIG4*(CN3+1)	     
         ELSE
C            Landau distribution with cut A99
C            truncation percentage of the Urban distribution  
            SALPHA = RSIG(A99)
            DEDX2 = (SALPHA*XI)**2         
C	    DEDX2 = XI*EMAX*(1.-(BETA*BETA/2.))
         END IF
C         sigma^2(E) in  GeV^2
         DEDX2 = DEDX2*1.E-12
      END IF
C      PRINT *,XI/EMAX,A,Z,RHO
c      PRINT *, SALPHA, A99, CN1+CN2+CN3, SQRT(DEDX2)
*
      END
 
 
      REAL*4 FUNCTION RSIG(AL)     
C
C       function to calculate the sigma corresponding to a 
C       truncated Landau function
C       A. Fontana and A. Rotondi  March 2007
C      
 
      REAL*4 AL
           
      IF(0.90 .LE. AL .AND. AL .LT. 0.95) THEN
        RSIG=236.98 - 533.167*AL + 303.333*AL**2
      
      ELSE IF(0.95 .LE. AL .AND. AL .LT. 0.99) THEN
        RSIG= -136.607 + 148.25*AL 
	 
      ELSE IF(0.99 .LE. AL .AND. AL .LT. 0.995) THEN
        RSIG= 88263.6 - 178582*AL + 90340.6*AL**2
	 	  
      ELSE IF(0.995 .LE. AL .AND. AL .LT. 0.999) THEN
        RSIG= 1.2539919E+06 -2.51982E+06*AL +1.26587E+06*AL**2
	 	  
      ELSE IF(0.999 .LE. AL .AND. AL .LT. 0.9996) THEN
        RSIG= -30621.1 + 30683.3*AL
	 	  
      ELSE IF(0.9996 .LE. AL .AND. AL .LT. 0.9999) THEN
        RSIG= -166550. + 166667.*AL 	 
	  
      ELSE IF(0.9999 .LE. AL .AND. AL .LT. 1.) THEN
        RSIG= -1564430. + 1564680.*AL 
      END IF	
      
      END 	
