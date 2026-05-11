
      SUBROUTINE DT_EVTPLO(Irange,Mode)
 
C***********************************************************************
C            MODE  = 1  plot content of complete DTEVT1 to out. unit   *
C                    2  plot entries of DTEVT1 given by IRANGE         *
C                    3  plot entries of extended DTEVT1 (DTEVT2)       *
C                    4  plot entries of DTEVT1 and DTEVT2              *
C                    5  plot rejection counter                         *
C This version dated 11.12.94 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER i , idchk , Irange , iremc , irsea , kf , Mode , nc
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      CHARACTER*16 chau
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C rejection counter
      INCLUDE 'inc/dtrejc'
 
      DIMENSION Irange(NMXHKK)
 
      IF ( (Mode.EQ.1) .OR. (Mode.EQ.4) ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99050)
         DO i = 1 , NHKk
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99060) i , ISThkk(i) , IDHkk(i)
     &           , JMOhkk(1,i) , JMOhkk(2,i) , JDAhkk(1,i) , JDAhkk(2,i)
     &           , PHKk(1,i) , PHKk(2,i) , PHKk(3,i) , PHKk(4,i) , 
     &           PHKk(5,i)
C1011       FORMAT(I5,I5,I6,4I5,2E15.5)
         END DO
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*)
C        DO 4 I=1,NHKK
C           WRITE(LOUT,1006) I,ISTHKK(I),
C    &                    VHKK(1,I),VHKK(2,I),VHKK(3,I),WHKK(1,I),
C    &                    WHKK(2,I),WHKK(3,I)
C1006       FORMAT(1X,I4,I6,6E10.3)
C   4    CONTINUE
      END IF
 
      IF ( Mode.EQ.2 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99050)
         nc = 0
 50      nc = nc + 1
         IF ( Irange(nc).EQ.-100 ) GOTO 99999
         i = Irange(nc)
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99060) i , ISThkk(i) , IDHkk(i) , 
     &        JMOhkk(1,i) , JMOhkk(2,i) , JDAhkk(1,i) , JDAhkk(2,i) , 
     &        PHKk(1,i) , PHKk(2,i) , PHKk(3,i) , PHKk(4,i) , PHKk(5,i)
         GOTO 50
      END IF
 
      IF ( (Mode.EQ.3) .OR. (Mode.EQ.4) ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010)
99010    FORMAT (/,1X,'EVTPLO:',14X,
     &           ' content of COMMON /DTEVT1/,/DTEVT2/',/,15X,
     &           '        -----------------------------------',/,/,
     &           '       ST    ID   M1   M2   D1   D2  IDR  IDXR',
     &           ' NOBAM IDCH    M',/)
         DO i = 1 , NHKk
C           IF ((ISTHKK(I).GT.10).OR.(ISTHKK(I).EQ.1)) THEN
            kf = IDHkk(i)
            idchk = kf/10000
            IF ( (((idchk.EQ.7) .OR. (idchk.EQ.8)) .AND. (kf.NE.80000))
     &           .OR. (IDHkk(i).EQ.99999) ) kf = 92
 
            CALL PYNAME(kf,chau)
 
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99020) i , ISThkk(i) , IDHkk(i)
     &           , JMOhkk(1,i) , JMOhkk(2,i) , JDAhkk(1,i) , JDAhkk(2,i)
     &           , IDRes(i) , IDXres(i) , NOBam(i) , IDCh(i) , PHKk(5,i)
     &           , chau
99020       FORMAT (I5,I5,I6,4I5,4I4,F8.4,2X,A)
C           ENDIF
         END DO
      END IF
 
      IF ( Mode.EQ.5 ) THEN
Caf: 2 lines just to make the compiler happy
         iremc = 0
         irsea = 0
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99030)
99030    FORMAT (/,1X,'EVTPLO:',14X,'    content of COMMON /DTREJC/',/,
     &           15X,'           --------------------------',/)
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99040) IRPt , IRHha , IRRes , 
     &        LOMres , LOBres , iremc , IRFrag , irsea , IRCron
99040    FORMAT (1X,'IRPT   = ',I5,'  IRHHA = ',I5,/,1X,'IRRES  = ',2I5,
     &           '  LOMRES = ',I5,'  LOBRES = ',I5,/,1X,'IREMC  = ',
     &           10I5,/,1X,'IRFRAG = ',I5,'  IRSEA = ',I5,' IRCRON = ',
     &           I5,/)
      END IF
99050 FORMAT (/,1X,'EVTPLO:',14X,'    content of COMMON /DTEVT1/',/,15X,
     &        '           --------------------------',/,/,
     &        '       ST    ID  M1   M2   D1   D2     PX     PY',
     &        '     PZ      E       M',/)
C           WRITE(LOUT,1011) I,ISTHKK(I),IDHKK(I),JMOHKK(1,I),
C    &                       JMOHKK(2,I),JDAHKK(1,I),JDAHKK(2,I),
C    &                       PHKK(3,I),PHKK(4,I)
C           WRITE(LOUT,'(4E15.4)')
C    &         VHKK(1,I),VHKK(2,I),VHKK(3,I),VHKK(4,I)
99060 FORMAT (I5,I5,I6,4I5,3F12.3,F12.3,F8.4)
 
99999 END SUBROUTINE
