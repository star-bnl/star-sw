
      SUBROUTINE PHO_REJSTA(Imode)
C********************************************************************
C
C     MC rejection counting
C
C     input IMODE    -1   initialization
C                    -2   output of statistics
C
C********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  internal rejection counters
      INCLUDE 'inc/poloop'
 
      INTEGER Imode
 
      INTEGER i
 
C  initialization
      IF ( Imode.EQ.-1 ) THEN
         DO i = 1 , NMXJ
            IFAil(i) = 0
         END DO
C
         REJtit(1) = 'PARTON ALL'
         REJtit(2) = 'STDPAR ALL'
         REJtit(3) = 'STDPAR DPO'
         REJtit(4) = 'POMSCA ALL'
         REJtit(5) = 'POMSCA INT'
         REJtit(6) = 'POMSCA KIN'
         REJtit(7) = 'DIFDIS ALL'
         REJtit(8) = 'POSPOM ALL'
         REJtit(9) = 'HRES.DIF.1'
         REJtit(10) = 'HDIR.DIF.1'
         REJtit(11) = 'HRES.DIF.2'
         REJtit(12) = 'HDIR.DIF.2'
         REJtit(13) = 'DIFDIS INT'
         REJtit(14) = 'HADRON SP2'
         REJtit(15) = 'HADRON SP3'
         REJtit(16) = 'HARDIR ALL'
         REJtit(17) = 'HARDIR INT'
         REJtit(18) = 'HARDIR KIN'
         REJtit(19) = 'MCHECK BAR'
         REJtit(20) = 'MCHECK MES'
         REJtit(21) = 'DIF.DISS.1'
         REJtit(22) = 'DIF.DISS.2'
         REJtit(23) = 'STRFRA ALL'
         REJtit(24) = 'MSHELL CHA'
         REJtit(25) = 'PARTPT SOF'
         REJtit(26) = 'PARTPT HAR'
         REJtit(27) = 'INTRINS KT'
         REJtit(28) = 'HACHEK DIR'
         REJtit(29) = 'HACHEK RES'
         REJtit(30) = 'STRING ALL'
         REJtit(31) = 'POMSCA INT'
         REJtit(32) = 'DIFF SLOPE'
         REJtit(33) = 'GLU2QU ALL'
         REJtit(34) = 'MASCOR ALL'
         REJtit(35) = 'PARCOR ALL'
         REJtit(36) = 'MSHELL PAR'
         REJtit(37) = 'MSHELL ALL'
         REJtit(38) = 'POMCOR ALL'
         REJtit(39) = 'DB-POM KIN'
         REJtit(40) = 'DB-POM ALL'
         REJtit(41) = 'SOFTXX ALL'
         REJtit(42) = 'SOFTXX PSP'
 
C  write output
      ELSE IF ( Imode.EQ.-2 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A,/,1X,A)')
     &         'PHO_REJSTA: rejection statistics' , 
     &        '--------------------------------'
         DO i = 1 , NMXJ
            IF ( IFAil(i).GT.0 .AND. LPRi.GT.4 )
     &            WRITE (LO,'(1X,I3,1X,A,5X,I15)') i , REJtit(i) , 
     &           IFAil(i)
         END DO
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I3)')
     &         'PHO_REJSTA: invalid mode ' , Imode
      END IF
 
      END SUBROUTINE
