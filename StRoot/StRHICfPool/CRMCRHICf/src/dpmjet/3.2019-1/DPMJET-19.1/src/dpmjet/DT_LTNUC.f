
      SUBROUTINE DT_LTNUC(Pin,Ein,Pout,Eout,Mode)
 
C***********************************************************************
C Lorentz-transformations.                                             *
C   PIN        longitudnal momentum       (input)                      *
C   EIN        energy                     (input)                      *
C   POUT       transformed long. momentum (output)                     *
C   EOUT       transformed energy         (output)                     *
C   MODE = 1(-1)    projectile rest syst.   --> Lab (back)             *
C        = 2(-2)    projectile rest syst.   --> nucl.-nucl.cms (back)  *
C        = 3(-3)    target rest syst. (=Lab)--> nucl.-nucl.cms (back)  *
C This version dated 01.11.95 is written by  S. Roesler.               *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION bdum1 , bdum2 , bg , dum1 , dum2 , dum3 , Ein , 
     &                 Eout , pdum1 , pdum2 , Pin , Pout , ZERO
      INTEGER Mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0)
 
C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
 
      bdum1 = ZERO
      bdum2 = ZERO
      pdum1 = ZERO
      pdum2 = ZERO
      IF ( ABS(Mode).EQ.1 ) THEN
         bg = -SIGN(BGLab,DBLE(Mode))
         CALL DT_DALTRA(GALab,bdum1,bdum2,-bg,pdum1,pdum2,Pin,Ein,dum1,
     &                  dum2,dum3,Pout,Eout)
      ELSE IF ( ABS(Mode).EQ.2 ) THEN
         bg = SIGN(BGCms(1),DBLE(Mode))
         CALL DT_DALTRA(GACms(1),bdum1,bdum2,bg,pdum1,pdum2,Pin,Ein,
     &                  dum1,dum2,dum3,Pout,Eout)
      ELSE IF ( ABS(Mode).EQ.3 ) THEN
         bg = -SIGN(BGCms(2),DBLE(Mode))
         CALL DT_DALTRA(GACms(2),bdum1,bdum2,bg,pdum1,pdum2,Pin,Ein,
     &                  dum1,dum2,dum3,Pout,Eout)
      ELSE
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) Mode
99010    FORMAT (1X,'LTNUC: not supported mode (MODE = ',I3,')')
         Eout = Ein
         Pout = Pin
      END IF
 
      END SUBROUTINE
