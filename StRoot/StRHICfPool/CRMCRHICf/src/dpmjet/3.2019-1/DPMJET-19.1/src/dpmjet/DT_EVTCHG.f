
      SUBROUTINE DT_EVTCHG(Id,Mode,Ipos,Irej)
 
C***********************************************************************
C Charge conservation check.                                           *
C        ID       identity of particle (PDG-numbering scheme)          *
C        MODE = 1 initialization                                       *
C             =-2 subtract ID-charge                                   *
C             = 2 add ID-charge                                        *
C             = 3 check charge cons.                                   *
C        IPOS     flag to give position of call of EVTCHG to output    *
C                 unit in case of violation                            *
C This version dated 10.01.95 is written by S. Roesler                 *
C Last change: s.r. 21.01.01                                           *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER ibar , ich , Id , idd , IDT_ICIHAD , IPHO_BAR3 , 
     &        IPHO_CHR3 , Ipos , Irej , Mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
 
      Irej = 0
 
      IF ( Mode.EQ.1 ) THEN
         ich = 0
         ibar = 0
         RETURN
      END IF
 
      IF ( Mode.EQ.3 ) THEN
         IF ( (ich.NE.0) .OR. (ibar.NE.0) ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A,I3,A,2I3,A,I8)')
     &            'EVTCHG: charge/baryo.-cons. failure at pos. ' , 
     &           Ipos , '! ICH/IBAR= ' , ich , ibar , ' event ' , NEVhkk
            CALL PHO_PREVNT(2)
            CALL DT_EVTOUT(1)
            ich = 0
            ibar = 0
 
            Irej = 1
            GOTO 99999
         END IF
         ich = 0
         ibar = 0
         RETURN
      END IF
 
 
      IF ( Id.EQ.0 ) RETURN
      idd = IDT_ICIHAD(Id)
C modification 21.1.01: use intrinsic phojet-functions to determine charge
C and baryon number
C     IF (IDD.GT.0) THEN
C        IF (MODE.EQ.2) THEN
C           ICH  = ICH+IICH(IDD)
C           IBAR = IBAR+IIBAR(IDD)
C        ELSEIF (MODE.EQ.-2) THEN
C           ICH  = ICH-IICH(IDD)
C           IBAR = IBAR-IIBAR(IDD)
C        ENDIF
C     ELSE
C        WRITE(LOUT,'(1X,A,3I6)') 'EVTCHG: (IDD = 0 !), IDD,ID=',IDD,ID
C        CALL DT_EVTOUT(4)
C        STOP
C     ENDIF
      IF ( Mode.EQ.2 ) THEN
         ich = ich + IPHO_CHR3(Id,1)/3
         ibar = ibar + IPHO_BAR3(Id,1)/3
      ELSE IF ( Mode.EQ.-2 ) THEN
         ich = ich - IPHO_CHR3(Id,1)/3
         ibar = ibar - IPHO_BAR3(Id,1)/3
      END IF
 
99999 END SUBROUTINE
