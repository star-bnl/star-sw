
      SUBROUTINE DT_COM2CR
 
C***********************************************************************
C COMbine q-aq chains to Color Ropes (qq-aqaq).                        *
C        CUTOF      parameter determining minimum number of not        *
C                   combined q-aq chains                               *
C This subroutine replaces KKEVCC etc.                                 *
C This version dated 11.01.95 is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER i , IDT_NPOISS , idxaq , idxqa , mo1 , mo2 , naq , 
     &        nchmin , nqa
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C statistics
      INCLUDE 'inc/dtsta1'
C various options for treatment of partons (DTUNUC 1.x)
C (chain recombination, Cronin,..)
      INCLUDE 'inc/dtchai'
 
      DIMENSION idxqa(248) , idxaq(248)
 
      ICChai(1,9) = ICChai(1,9) + 1
      nqa = 0
      naq = 0
C scan DTEVT1 for q-aq, aq-q chains
      DO i = NPOint(3) , NHKk
C skip "chains" which are resonances
         IF ( (IDHkk(i).EQ.88888) .AND. (IDRes(i).EQ.0) ) THEN
            mo1 = JMOhkk(1,i)
            mo2 = JMOhkk(2,i)
            IF ( (ABS(IDHkk(mo1)).LE.6) .AND. (ABS(IDHkk(mo2)).LE.6) )
     &           THEN
C q-aq, aq-q chain found, keep index
               IF ( IDHkk(mo1).GT.0 ) THEN
                  nqa = nqa + 1
                  idxqa(nqa) = i
               ELSE
                  naq = naq + 1
                  idxaq(naq) = i
               END IF
            END IF
         END IF
      END DO
 
C minimum number of q-aq chains requested for the same projectile/
C target
      nchmin = IDT_NPOISS(CUTof)
 
C combine q-aq chains of the same projectile
      CALL DT_SCN4CR(nqa,idxqa,nchmin,1)
C combine q-aq chains of the same target
      CALL DT_SCN4CR(nqa,idxqa,nchmin,2)
C combine aq-q chains of the same projectile
      CALL DT_SCN4CR(naq,idxaq,nchmin,1)
C combine aq-q chains of the same target
      CALL DT_SCN4CR(naq,idxaq,nchmin,2)
 
      END SUBROUTINE
