
      SUBROUTINE DT_EVTHIS(Nevt)
 
C***********************************************************************
C Dump content of temorary histograms into /DTHIS1/. This subroutine   *
C is called after each event and for the last event before any call    *
C to OUTHGR.                                                           *
C         NEVT   number of events dumped, this is only needed to       *
C                get the normalization after the last event            *
C This version dated 23.4.95 is written  by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION avx , ONE , TINY , ZERO
      INTEGER i , j , ncevt , Nevt
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      LOGICAL lnoety
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TINY=1.0D-10)
 
C histograms
 
 
      INCLUDE 'inc/dthis1'
C auxiliary common for histograms
      INCLUDE 'inc/dthis2'
 
      DATA ncevt/0/
 
      ncevt = ncevt + 1
      Nevt = ncevt
 
      DO i = 1 , IHIsl
         lnoety = .TRUE.
         DO j = 1 , IBIns(i)
            IF ( TMPhis(1,i,j).GT.ZERO ) THEN
               lnoety = .FALSE.
               HISt(2,i,j) = HISt(2,i,j) + ONE
               HISt(7,i,j) = HISt(7,i,j) + TMPhis(1,i,j)
               DENtry(2,i) = DENtry(2,i) + TMPhis(1,i,j)
               avx = TMPhis(2,i,j)/TMPhis(1,i,j)
               HISt(3,i,j) = HISt(3,i,j) + TMPhis(3,i,j)*avx
               HISt(4,i,j) = HISt(4,i,j) + TMPhis(3,i,j)*avx**2
               HISt(5,i,j) = HISt(5,i,j) + TMPhis(3,i,j)
               HISt(6,i,j) = HISt(6,i,j) + TMPhis(3,i,j)**2
               TMPhis(1,i,j) = ZERO
               TMPhis(2,i,j) = ZERO
               TMPhis(3,i,j) = ZERO
            END IF
         END DO
         IF ( .NOT.(lnoety) ) THEN
            DENtry(1,i) = DENtry(1,i) + ONE
         ELSE IF ( TMPufl(i).GT.ZERO ) THEN
            UNDerf(i) = UNDerf(i) + ONE
            TMPufl(i) = ZERO
         ELSE IF ( TMPofl(i).GT.ZERO ) THEN
            OVErf(i) = OVErf(i) + ONE
            TMPofl(i) = ZERO
         END IF
      END DO
 
      END SUBROUTINE
