*
* $Id: gpdcay.F,v 1.2 2010/06/24 17:41:26 jwebb Exp $
*
* $Log: gpdcay.F,v $
* Revision 1.2  2010/06/24 17:41:26  jwebb
* Added the ids of the decay daughters to the output
*
* Revision 1.1.1.1  2004/01/12 23:49:40  potekhin
*
*
* Revision 1.2  2001/06/15 23:00:20  nevski
* if-endif corrected
*
* Revision 1.1  2001/02/27 10:15:39  nevski
*  first working release
*
#include "geant321/pilot.h"
*
      SUBROUTINE GPDCAY  (IPART)
*-
*-    ******************************************************************
*-    *                                                                *
*-    *  Print branching ratios and decay modes for GEANT particles.   *
*-    *                                                                *
*-    *    ==>Called by : <USER>                                       *
*-    *       Author    P.Nevski                                       *
*-    *                                                                *
*-    ******************************************************************
*-
#include "commons/typing.inc"
#include "geant321/gcbank.inc"
#include "geant321/gcnum.inc"
#include "geant321/gcunit.inc"
      REAL      BRATIO
      INTEGER   LENOCC,IPART,IP1,IP2,IP,JPA,
     >          MODE,JDK1,JDK2,L1,L,LL,I,JP,M1,
     >          ID1, ID2, ID3
      CHARACTER CNAME*20,CMODE*200
*-
*-    ------------------------------------------------------------------
*-
      IF (IPART.EQ.0) THEN
         IP1=1
         IP2=NPART
      ELSE
         IP1=IPART
         IP2=IPART
      ENDIF
*
      DO IP=IP1,IP2
         IF (IP.LE.0)      GOTO 90
         IF (IP.GT.NPART)  GOTO 90
         JPA = LQ(JPART-IP)
         IF (JPA.LE.0)     GOTO 90
         JDK1=LQ(JPA-1) !  PABR bank
         IF (JDK1.LE.0)    GOTO 90
         JDK2=LQ(JPA-2) !  PAMO bank
         IF (JDK2.LE.0)    GOTO 90
*
         CALL UHTOC(IQ(JPA+1),4,CMODE,20)
         L1=LENOCC(CMODE(1:20))
         WRITE(LOUT,1000) IP,CMODE(1:L1)
 1000    FORMAT(I5,1x,A,'  Decay(s):')
*
*    COPY branching ratios & decay modes.
*
         DO I  = 1,6

            CMODE = CMODE(1:L1)//' => '
            LL     = L1+4
            BRATIO = Q(JDK1+I)
            IF (BRATIO.LE.0) GO TO 60
            MODE   = IQ(JDK2+I)

            ID1 = MOD(MODE/    1 ,  100)
            ID2 = MOD(MODE/  100 ,  100)
            ID3 = MOD(MODE/10000 ,  100)

            DO WHILE (MODE.GT.0)
               M1  = MOD(MODE,100)
               IF (M1.LE.0) GOTO 50
               JP  = LQ(JPART-M1)
               IF (JP.LE.0) GO TO 50
               CALL UHTOC(IQ(JP+1),4,CNAME,20)
               L   = LENOCC(CNAME)
               CMODE=CMODE(1:LL)//CNAME(1:L)//' & '
               LL  = LL+L+3
  50           MODE=MODE/100
            ENDDO

            IF ( ID3 > 0 ) THEN
               WRITE(LOUT,1001) BRATIO, CMODE(1:LL-3), ID1, ID2, ID3            
 1001          FORMAT(6x,'Branching(%)=',F8.3,' Channel: ',A,' Mode: ',
     >              I3, I3, I3)

            ELSE
               WRITE(LOUT,1002) BRATIO, CMODE(1:LL-3), ID1, ID2
 1002          FORMAT(6x,'Branching(%)=',F8.3,' Channel: ',A,' Mode: ',
     >              I3, I3)

            ENDIF


  60     ENDDO
C
  90     CONTINUE
      ENDDO
      END






