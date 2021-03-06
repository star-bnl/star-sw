*
* $Id: akeka.F,v 1.1.1.2 2009/02/01 17:10:16 fisyak Exp $
*
* $Log: akeka.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:16  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:46:39  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:23  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:24  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:36  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:15  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:03  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.44  by  S.Giani
*-- Author :
*$ CREATE AKEKA.FOR
*COPY AKEKA
*                                                                      *
*=== akeka ============================================================*
*                                                                      *
      FUNCTION AKEKA(IT,TO,AMSS)
 
#include "geant321/dblprc.inc"
#include "geant321/dimpar.inc"
#include "geant321/iounit.inc"
C********************************************************************
C     VERSION BY                     J. RANFT
C                                    LEIPZIG
C     LAST CHANGE 05. DECEMBER 83    PERTTI AARNIO
C                                    HELSINKI UNIVERSITY OF
C                                    TECHNOLOGY, FINLAND
C
C
C     TO BE CALLED FROM HIGH ENERGY PRODUCTION
C
C
C     SLOPE OF THE ENERGY DISTRIBUTION OF THE HIGHER ENERGY PARTICLES
C     OF THE INTRANUCLEAR CASCADE PROTONS AND NEUTRONS (ALFA-2P
C     AND ALFA-2N). NOTE THAT IT IS ASSUMED IN EKEKA AND RAKEKA
C     THAT ALFA-2N=6*ALFA-1N AND ALFA-2P=6*ALFA-1P.
C
C     NOTE!!!!!!! NON-STANDARD PARTICLE NUMBERING
C
C     INPUT VARIABLES:
C        IT   = TYPE OF THE SECONDARY; 1=PROTON, 2=NEUTRON
C        TO   = KINETIC ENERGY OF THE PRIMARY HADRON IN GEV
C        AMSS = ATOMIC WEIGHT OF THE NUCLEUS
C
C     SEE RANFT/ROUTTI PARTICLE ACC. VOL 4 P 105
C********************************************************************
C
      DIMENSION A(2),B(2)
      SAVE A,B
      DATA  A/0.11D0,0.1D0/
      DATA B/0.21D0,0.20D0/
      IF (TO .LT. 10.D0) THEN
         AKEKA=(1.D0-0.001D0*AMSS)*(A(IT)+0.01D0*TO)
      ELSE
         AKEKA=B(IT)*(1.D0-0.001D0*AMSS)
      END IF
      RETURN
      END
