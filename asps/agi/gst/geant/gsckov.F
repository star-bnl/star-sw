*
* $Id: gsckov.F,v 1.2 1998/04/16 16:59:40 fisyak Exp $
*
* $Log: gsckov.F,v $
* Revision 1.2  1998/04/16 16:59:40  fisyak
* 2nd pass with gstar
*
*
#include "geant321/pilot.h"
*CMZ :          09/02/98  16.09.29  by  Pavel Nevski
*CMZ :  3.21/02 21/03/94  17.01.28  by  Rene Brun
*-- Author :
      SUBROUTINE GSCKOV(ITMED, NPCKOV, PPCKOV, ABSCO, EFFIC, RINDEX)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    Stores the tables for UV photon tracking in medium ITMED    *
C.    *    Please note that it is the user's responsability to         *
C.    *    provide all the coefficients:                               *
C.    *                                                                *
C.    *                                                                *
C.    *       ITMED       Tracking medium number                       *
C.    *       NPCKOV      Number of bins of each table                 *
C.    *       PPCKOV      Value of photon momentum (in GeV)            *
C.    *       ABSCO       Absorbtion coefficents                       *
C.    *                   dielectric: absorbtion length in cm          *
C.    *                   metals    : absorbtion fraction (0<=x<=1)    *
C.    *       EFFIC       Detection efficiency for UV photons          *
C.    *       RINDEX      Refraction index (if=0 metal)                *
C.    *                                                                *
C.    *       Called by : <USER>                                       *
C.    *                                                                *
C.    *       Authors: F.Carminati, R.Jones  ************              *
C.    *                                                                *
C.    *       Modifications:                                           *
C.    *       PN, 9-feb-98:   multiple calls subsitute same banks      *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gcunit.inc"
C
      DIMENSION PPCKOV(*), ABSCO(*), EFFIC(*), RINDEX(*)
C.
C.    ------------------------------------------------------------------
C.
      JTM = 0
      IF(ITMED.GT.0) THEN
         IF(JTMED.GT.0) THEN
            IF(IQ(JTMED-2).GE.ITMED) THEN
               JTM = LQ(JTMED-ITMED)
            ENDIF
         ENDIF
      ENDIF
      IF(JTM.LE.0) THEN
         WRITE(CHMAIL,10000)
10000   FORMAT(' **** GSCKOV Tracking medium ',I10,' not defined')
         CALL GMAIL(0,0)
         GO TO 999
      ENDIF
      IF(IQ(JTM-2).LT.3) THEN
         CALL MZPUSH(IXSTOR,JTM,3-IQ(JTM-2),0,' ')
      ENDIF
      JTCKOV=LQ(JTM-3)
      IF (JTCKOV.le.0)
     +   CALL MZBOOK(IXCONS,JTCKOV,JTM, -3,'CKOV',4,4,NPCKOV+1,3,-1)
      JABSCO=LQ(JTCKOV-1)
      IF (JABSCO.le.0)
     +   CALL MZBOOK(IXCONS,JABSCO,JTCKOV,-1,'ABSC',0,0,NPCKOV,3,-1)
      JEFFIC=LQ(JTCKOV-2)
      IF (JEFFIC.le.0)
     +   CALL MZBOOK(IXCONS,JEFFIC,JTCKOV,-2,'EFFI',0,0,NPCKOV,3,-1)
      IF(RINDEX(1).GE.1.) THEN
         JINDEX=LQ(JTCKOV-3)
         IF (JINDEX.le.0)
     +    CALL MZBOOK(IXCONS,JINDEX,JTCKOV,-3,'RIND',0,0,NPCKOV,3,-1)
         JCURIN=LQ(JTCKOV-4)
         IF (JCURIN.le.0)
     +    CALL MZBOOK(IXCONS,JCURIN,JTCKOV,-4,'ABCU',0,0,NPCKOV,3,-1)
      ENDIF
      Q(JTCKOV+1)=NPCKOV
      DO 10 J=1, NPCKOV
         Q(JTCKOV+1+J) = PPCKOV(J)
         Q(JABSCO+  J) = MAX(1E-10,ABSCO(J))
         Q(JEFFIC+  J) = EFFIC(J)
         IF(RINDEX(1).GE.1.) THEN
            Q(JINDEX+ J) = RINDEX(J)
            IF(J.GT.1) THEN
               Q(JCURIN+J)=0.5*((1/RINDEX(J-1)**2)+(1./RINDEX(J)**2))
               Q(JCURIN+J)=Q(JCURIN+J-1)+(PPCKOV(J)-PPCKOV(J-1))*
     +                     Q(JCURIN+J)
            ELSE
               Q(JCURIN+J)=0.
            ENDIF
         ENDIF
   10 CONTINUE
*
  999 END
