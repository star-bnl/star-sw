
      SUBROUTINE DT_FOZOCA(Lfzc,Irej)
 
C***********************************************************************
C This subroutine treats the complete FOrmation ZOne supressed intra-  *
C nuclear CAscade.                                                     *
C               LFZC = .true.  cascade has been treated                *
C                    = .false. cascade skipped                         *
C This is a completely revised version of the original FOZOKL.         *
C This version dated 18.11.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION DLARGE , DT_RNDM , FM2MM , OHALF , RNUCLE , ZERO
      INTEGER i , iprcl , ipzrcl , Irej , irej1 , itrcl , itzrcl , j , 
     &        ncas , ncwoun , nend , nstart
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (DLARGE=1.0D10,OHALF=0.5D0,ZERO=0.0D0)
      PARAMETER (FM2MM=1.0D-12,RNUCLE=1.12D0)
 
      LOGICAL lstart , lcas , Lfzc
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C rejection counter
      INCLUDE 'inc/dtrejc'
C properties of interacting particles
      INCLUDE 'inc/dtprta'
C Glauber formalism: collision properties
      INCLUDE 'inc/dtglcp'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C final state after intranuclear cascade step
      INCLUDE 'inc/dtpaul'
C parameter for intranuclear cascade
      INCLUDE 'inc/dtfoti'
 
      DIMENSION ncwoun(2)
 
      DATA lstart/.TRUE./
 
      Lfzc = .TRUE.
      Irej = 0
 
C skip cascade if hadron-hadron interaction or if supressed by user
      IF ( .NOT.(((IP.EQ.1) .AND. (IT.EQ.1)) .OR. (KTAuge.LT.1)) ) THEN
C skip cascade if not all possible chains systems are hadronized
         DO i = 1 , 8
            IF ( .NOT.LHAdro(i) ) GOTO 200
         END DO
 
         IF ( lstart ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99010) KTAuge , TAUfor , INCmod
99010       FORMAT (/,1X,'FOZOCA:  intranuclear cascade treated for a ',
     &              'maximum of',I4,' generations',/,10X,
     &              'formation time ','parameter:',F5.1,'  fm/c',9X,
     &              'modus:',I2)
 
            IF ( LPRi.GT.4 .AND. ITAuve.EQ.1 ) WRITE (LOUt,99020)
99020       FORMAT (10X,'p_t dependent formation zone',/)
 
            IF ( LPRi.GT.4 .AND. ITAuve.EQ.2 ) WRITE (LOUt,99030)
99030       FORMAT (10X,'constant formation zone',/)
            lstart = .FALSE.
         END IF
 
C in order to avoid wasting of cpu-time the DTEVT1-indices of nucleons
C which may interact with final state particles are stored in a seperate
C array - here all proj./target nucleon-indices (just for simplicity)
         NOInc = 0
         DO i = 1 , NPOint(1) - 1
            NOInc = NOInc + 1
            IDXinc(NOInc) = i
         END DO
 
C initialize Pauli-principle treatment (find wounded nucleons)
         NWOund(1) = 0
         NWOund(2) = 0
         ncwoun(1) = 0
         ncwoun(2) = 0
         DO j = 1 , NPOint(1)
            DO i = 1 , 2
               IF ( ISThkk(j).EQ.10+i ) THEN
                  NWOund(i) = NWOund(i) + 1
                  EWOund(i,NWOund(i)) = PHKk(4,j)
                  IF ( IDHkk(j).EQ.2212 ) ncwoun(i) = ncwoun(i) + 1
               END IF
            END DO
         END DO
 
C modify nuclear potential for wounded nucleons
         iprcl = IP - NWOund(1)
         ipzrcl = IPZ - ncwoun(1)
         itrcl = IT - NWOund(2)
         itzrcl = ITZ - ncwoun(2)
         CALL DT_NCLPOT(ipzrcl,iprcl,itzrcl,itrcl,ZERO,ZERO,1)
 
         nstart = NPOint(4)
         nend = NHKk
 
 50      DO i = nstart , nend
 
            IF ( (ABS(ISThkk(i)).EQ.1) .AND. (IDCh(i).LT.KTAuge) ) THEN
C select nucleus the cascade starts first (proj. - 1, target - -1)
               ncas = 1
C   projectile/target with probab. 1/2
               IF ( (INCmod.EQ.1) .OR. (IDCh(i).GT.0) ) THEN
                  IF ( DT_RNDM(TAUfor).GT.OHALF ) ncas = -ncas
C   in the nucleus with highest mass
               ELSE IF ( INCmod.EQ.2 ) THEN
                  IF ( IP.GT.IT ) THEN
                     ncas = -ncas
                  ELSE IF ( IP.EQ.IT ) THEN
                     IF ( DT_RNDM(TAUfor).GT.OHALF ) ncas = -ncas
                  END IF
C the nucleus the cascade starts first is requested to be the one
C moving in the direction of the secondary
               ELSE IF ( INCmod.EQ.3 ) THEN
                  ncas = INT(SIGN(1.0D0,PHKk(3,i)))
               END IF
C check that the selected "nucleus" is not a hadron
               IF ( ((ncas.EQ.1) .AND. (IP.LE.1)) .OR. 
     &              ((ncas.EQ.-1) .AND. (IT.LE.1)) ) ncas = -ncas
 
C treat intranuclear cascade in the nucleus selected first
               lcas = .FALSE.
               CALL DT_INUCAS(IT,IP,i,lcas,ncas,irej1)
               IF ( irej1.NE.0 ) GOTO 100
C treat intranuclear cascade in the other nucleus if this isn't a had.
               ncas = -ncas
               IF ( ((ncas.EQ.1) .AND. (IP.GT.1)) .OR. 
     &              ((ncas.EQ.-1) .AND. (IT.GT.1)) ) THEN
                  IF ( lcas ) CALL DT_INUCAS(IT,IP,i,lcas,ncas,irej1)
                  IF ( irej1.NE.0 ) GOTO 100
               END IF
 
            END IF
 
         END DO
         nstart = nend + 1
         nend = NHKk
         IF ( nstart.LE.nend ) GOTO 50
 
         RETURN
 
C reject this event
 100     IRInc = IRInc + 1
         Irej = 1
      END IF
 
C intranucl. cascade not treated because of interaction properties or
C it is supressed by user or it was rejected or...
 200  Lfzc = .FALSE.
C reset flag characterizing direction of motion in n-n-cms
C*sr14-11-95
C     DO 9990 I=NPOINT(5),NHKK
C        IF (ISTHKK(I).EQ.-1) ISTHKK(I)=1
C9990 CONTINUE
 
      END SUBROUTINE
