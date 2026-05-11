
      SUBROUTINE DT_AAEVT(Nevts,Epn,Npmass,Npchar,Ntmass,Ntchar,Idp,
     &                    Iglau)
 
C***********************************************************************
C This version dated 22.03.96 is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION dum , Epn
      INTEGER i , idmnyr , Idp , ievt , Iglau , irej , kkmat , Nevts , 
     &        nmsg , Npchar , Npmass , Ntchar , Ntmass
      SAVE 
 
      INCLUDE 'inc/dtflka'
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C event flag
      INCLUDE 'inc/dtevno'
 
      INCLUDE 'inc/dtflg1'
 
C      COMMON /PRTANU/ IAPROJ,IATARG
 
      CHARACTER*8 date , hhmmss
      DIMENSION idmnyr(3)
 
      kkmat = 1
      nmsg = MAX(Nevts/100,1)
 
C initialization of run-statistics and histograms
      CALL DT_STATIS(1)
 
      CALL PHO_PHIST(1000,dum)
 
C initialization of Glauber-formalism
      IF ( NCOmpo.LE.0 ) THEN
         CALL DT_SHMAKI(Npmass,Npchar,Ntmass,Ntchar,Idp,Epn,Iglau)
      ELSE
         DO i = 1 , NCOmpo
            CALL DT_SHMAKI(Npmass,Npchar,IEMuma(i),IEMuch(i),Idp,Epn,0)
         END DO
      END IF
      CALL DT_SIGEMU
 
      CALL IDATE(idmnyr)
      WRITE (date,'(I2,''/'',I2,''/'',I2)') idmnyr(1) , idmnyr(2) , 
     &       MOD(idmnyr(3),100)
      CALL ITIME(idmnyr)
      WRITE (hhmmss,'(I2,'':'',I2,'':'',I2)') idmnyr(1) , idmnyr(2) , 
     &       idmnyr(3)
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,99010) date , hhmmss
99010 FORMAT (/,' DT_AAEVT: Initialisation finished. ( Date: ',A8,
     &        '   Time: ',A8,' )')
 
C generate NEVTS events
      DO ievt = 1 , Nevts
 
C  print run-status message
         IF ( MOD(ievt,nmsg).EQ.0 ) THEN
            CALL IDATE(idmnyr)
            WRITE (date,'(I2,''/'',I2,''/'',I2)') idmnyr(1) , idmnyr(2)
     &             , MOD(idmnyr(3),100)
            CALL ITIME(idmnyr)
            WRITE (hhmmss,'(I2,'':'',I2,'':'',I2)') idmnyr(1) , 
     &             idmnyr(2) , idmnyr(3)
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99020) ievt - 1 , Nevts , 
     &           date , hhmmss
99020       FORMAT (/,1X,I8,' out of ',I8,' events sampled ( Date: ',A,
     &              '   Time: ',A,' )',/)
C           WRITE(LOUT,1000) IEVT-1
C1000       FORMAT(1X,I8,' events sampled')
         END IF
         NEVent = ievt
C  treat nuclear emulsions
C  composite targets only
         IF ( IEMul.GT.0 ) CALL DT_GETEMU(Ntmass,Ntchar,kkmat,0)
         kkmat = -kkmat
C  sample this event
         CALL DT_KKINC(Npmass,Npchar,Ntmass,Ntchar,Idp,Epn,kkmat,irej)
 
 
C       chain fusion
C       WRITE(6,*)' IFUSION,IAPROJ,IATARG ',IFUSION,IAPROJ,IATARG
         IF ( (IFUsion.EQ.1) .AND. (Npmass.GT.12) .AND. (Ntmass.GT.12) )
     &        CALL DT_DENSITY
C
C
         CALL PHO_PHIST(2000,dum)
 
      END DO
 
C print run-statistics and histograms to output-unit 6
 
      CALL PHO_PHIST(3000,dum)
 
      CALL DT_STATIS(2)
 
      END SUBROUTINE
