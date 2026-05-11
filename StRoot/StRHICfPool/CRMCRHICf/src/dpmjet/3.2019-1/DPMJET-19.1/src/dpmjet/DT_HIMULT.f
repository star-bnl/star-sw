
      SUBROUTINE DT_HIMULT(Mode)
 
C***********************************************************************
C Tables of average energies/multiplicities.                           *
C This version dated 30.08.2000 is written by S. Roesler               *
C Last change  5.5.2012 by S. Roesler.                                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION ave , avmult , avpt , avswm , betblc , betgre , 
     &                 ONE , SWMEXP , TINY14 , TWO , ZERO
      INTEGER i , iavpt , ivel , j , Mode , NOPART
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TWO=2.0D0,TINY14=1.0D-14)
 
      PARAMETER (SWMEXP=1.7D0)
 
      CHARACTER*8 anameh(4)
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C temporary storage for one final state particle
      INCLUDE 'inc/dtfspa'
C event flag used for histograms
      INCLUDE 'inc/dtnorm'
C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
 
      PARAMETER (NOPART=210)
      DIMENSION avmult(4,NOPART) , ave(4,NOPART) , avswm(4,NOPART) , 
     &          avpt(4,NOPART) , iavpt(4,NOPART)
      DATA anameh/'DEUTERON' , '3-H     ' , '3-HE    ' , '4-HE    '/
 
      IF ( Mode.EQ.2 ) THEN
 
C------------------------------------------------------------------
C filling of histogram with event-record
         IF ( PE.LT.0.0D0 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*) ' HIMULT:  PE < 0 ! ' , PE
            RETURN
         END IF
         IF ( .NOT.LFRag ) THEN
            ivel = 2
            IF ( LGRey ) ivel = 3
            IF ( LBLack ) ivel = 4
            ave(1,IDBjt) = ave(1,IDBjt) + PE
            ave(ivel,IDBjt) = ave(ivel,IDBjt) + PE
            avpt(1,IDBjt) = avpt(1,IDBjt) + PT
            avpt(ivel,IDBjt) = avpt(ivel,IDBjt) + PT
            iavpt(1,IDBjt) = iavpt(1,IDBjt) + 1
            iavpt(ivel,IDBjt) = iavpt(ivel,IDBjt) + 1
            avswm(1,IDBjt) = avswm(1,IDBjt) + PE**SWMEXP
            avswm(ivel,IDBjt) = avswm(ivel,IDBjt) + PE**SWMEXP
            avmult(1,IDBjt) = avmult(1,IDBjt) + ONE
            avmult(ivel,IDBjt) = avmult(ivel,IDBjt) + ONE
            IF ( IDBjt.LT.207 ) THEN
C   total energy, multiplicity
               ave(1,30) = ave(1,30) + PE
               ave(ivel,30) = ave(ivel,30) + PE
               avpt(1,30) = avpt(1,30) + PT
               avpt(ivel,30) = avpt(ivel,30) + PT
               iavpt(1,30) = iavpt(1,30) + 1
               iavpt(ivel,30) = iavpt(ivel,30) + 1
               avswm(1,30) = avswm(1,30) + PE**SWMEXP
               avswm(ivel,30) = avswm(ivel,30) + PE**SWMEXP
               avmult(1,30) = avmult(1,30) + ONE
               avmult(ivel,30) = avmult(ivel,30) + ONE
C   charged energy, multiplicity
               IF ( ICHar.LT.0 ) THEN
                  ave(1,26) = ave(1,26) + PE
                  ave(ivel,26) = ave(ivel,26) + PE
                  avpt(1,26) = avpt(1,26) + PT
                  avpt(ivel,26) = avpt(ivel,26) + PT
                  iavpt(1,26) = iavpt(1,26) + 1
                  iavpt(ivel,26) = iavpt(ivel,26) + 1
                  avswm(1,26) = avswm(1,26) + PE**SWMEXP
                  avswm(ivel,26) = avswm(ivel,26) + PE**SWMEXP
                  avmult(1,26) = avmult(1,26) + ONE
                  avmult(ivel,26) = avmult(ivel,26) + ONE
               END IF
               IF ( ICHar.NE.0 ) THEN
                  ave(1,27) = ave(1,27) + PE
                  ave(ivel,27) = ave(ivel,27) + PE
                  avpt(1,27) = avpt(1,27) + PT
                  avpt(ivel,27) = avpt(ivel,27) + PT
                  iavpt(1,27) = iavpt(1,27) + 1
                  iavpt(ivel,27) = iavpt(ivel,27) + 1
                  avswm(1,27) = avswm(1,27) + PE**SWMEXP
                  avswm(ivel,27) = avswm(ivel,27) + PE**SWMEXP
                  avmult(1,27) = avmult(1,27) + ONE
                  avmult(ivel,27) = avmult(ivel,27) + ONE
               END IF
            END IF
         END IF
 
         RETURN
      ELSE IF ( Mode.EQ.3 ) THEN
 
C------------------------------------------------------------------
C output
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010)
99010    FORMAT (/,1X,'HIMULT:',21X,'particle - statistics',/,29X,
     &           '---------------------',/)
         IF ( MULdef.EQ.1 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A,/)')
     &            'fast/grey/black: EMU-def.'
         ELSE
            betgre = 0.7D0
            betblc = 0.23D0
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99020) betgre , betgre , 
     &           betblc , betblc
99020       FORMAT (1X,'fast:  beta > ',F4.2,'    grey:  ',F4.2,
     &              ' > beta > ',F4.2,'    black:  beta < ',F4.2,/)
         END IF
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99030) SWMEXP
C    &      '       grey     black      K      f(',F3.1,')',/,1X,
99030    FORMAT (1X,'particle    |',12X,'average multiplicity',/,13X,
     &           '|     total         fast',
     &           '       grey     black    <pt>     f(',F3.1,')',/,1X,
     &           '------------+--------------',
     &           '-------------------------------------------------')
         DO i = 1 , NOPART
            DO j = 1 , 4
               avmult(j,i) = avmult(j,i)/DBLE(MAX(ICEvt,1))
               ave(j,i) = ave(j,i)/DBLE(MAX(ICEvt,1))/EPRoj
               avpt(j,i) = avpt(j,i)/DBLE(MAX(iavpt(j,i),1))
               avswm(j,i) = avswm(j,i)/DBLE(MAX(ICEvt,1))/EPRoj**SWMEXP
            END DO
            IF ( i.LT.207 ) THEN
 
C    &                       AVE(1,I),AVSWM(1,I)
               IF ( LPRi.GT.4 ) WRITE (LOUt,99040) ANAme(i) , i , 
     &              avmult(1,i) , avmult(2,i) , avmult(3,i) , 
     &              avmult(4,i) , avpt(1,i) , avswm(1,i)
            ELSE
 
C    &                       AVE(1,I),AVSWM(1,I)
               IF ( LPRi.GT.4 ) WRITE (LOUt,99040) anameh(i-206) , i , 
     &              avmult(1,i) , avmult(2,i) , avmult(3,i) , 
     &              avmult(4,i) , avpt(1,i) , avswm(1,i)
            END IF
         END DO
         GOTO 99999
      END IF
 
C------------------------------------------------------------------
C initialization
      DO i = 1 , NOPART
         DO j = 1 , 4
            avmult(j,i) = ZERO
            ave(j,i) = ZERO
            avswm(j,i) = ZERO
            avpt(j,i) = ZERO
            iavpt(j,i) = 0
         END DO
      END DO
 
      RETURN
99040 FORMAT (1X,A8,I4,'| ',2F13.6,2F9.5,2F9.5)
C*temporary
C     WRITE(LOUT,'(A,F7.3)') ' number of charged heavy particles: ',
C    &               AVMULT(3,27)+AVMULT(4,27)
C*
 
99999 END SUBROUTINE
