
      SUBROUTINE PHO_SELCOL(Ico1,Ico2,Icoa1,Icoa2,Icob1,Icob2,Imode)
C********************************************************************
C
C    color combinatorics
C
C    input:         ICO1,2   colors of incoming particle
C                   IMODE    -2  output of initialization status
C                            -1  initialization
C                                   ICINP(1) selection mode
C                                            0   QCD
C                                            1   large N_c expansion
C                                   ICINP(2) max. allowed color
C                            0   clear internal color counter
C                            1   hadron into two colored objects
C                            2   quark into quark gluon
C                            3   gluon into gluon gluon
C                            4   gluon into quark antiquark
C
C    output:        ICOA1,2  colors of first outgoing particle
C                   ICOB1,2  colors of second outgoing particle
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , dum
      INTEGER ici1 , ici2 , Ico1 , Ico2 , Icoa1 , Icoa2 , Icob1 , 
     &        Icob2 , ii , Imode , maxcol , method
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
 
      DATA method/0/ , ii/0/
 
      ici1 = Ico1
      ici2 = Ico2
      IF ( method.NE.0 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I5)')
     &         'PHO_SELCOL:ERROR:unsupported method selected' , method
         CALL PHO_ABORT
 
      ELSE IF ( Imode.EQ.1 ) THEN
         ii = ii + 1
         IF ( ii.GT.maxcol )
     &        ii = INT(MIN(DT_RNDM(dum)*DBLE(maxcol)+1.00001D+00,
     &        DBLE(maxcol)))
         Icoa1 = ii
         Icoa2 = 0
         Icob1 = -ii
         Icob2 = 0
      ELSE IF ( Imode.EQ.2 ) THEN
         ii = ii + 1
         IF ( ii.GT.maxcol )
     &        ii = INT(MIN(DT_RNDM(dum)*DBLE(maxcol)+1.00001D+00,
     &        DBLE(maxcol)))
         Icoa2 = 0
         IF ( ici1.GT.0 ) THEN
            Icoa1 = ii
            Icob1 = ici1
            Icob2 = -ii
         ELSE
            Icoa1 = -ii
            Icob1 = ii
            Icob2 = ici1
         END IF
      ELSE IF ( Imode.EQ.3 ) THEN
         ii = ii + 1
         IF ( ii.GT.maxcol )
     &        ii = INT(MIN(DT_RNDM(dum)*DBLE(maxcol)+1.00001D+00,
     &        DBLE(maxcol)))
         IF ( DT_RNDM(dum).GT.0.5D0 ) THEN
            Icoa1 = ici1
            Icoa2 = -ii
            Icob1 = ii
            Icob2 = ici2
         ELSE
            Icob1 = ici1
            Icob2 = -ii
            Icoa1 = ii
            Icoa2 = ici2
         END IF
      ELSE IF ( Imode.EQ.4 ) THEN
         Icoa1 = ici1
         Icoa2 = 0
         Icob1 = ici2
         Icob2 = 0
      ELSE IF ( Imode.EQ.0 ) THEN
         ii = 0
      ELSE IF ( Imode.EQ.-1 ) THEN
         method = ici1
         maxcol = ici2
      ELSE IF ( Imode.EQ.-2 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I5)')
     &         'PHO_SELCOL: METHOD,MAXCOL' , method , maxcol
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I5)')
     &         'PHO_SELCOL:ERROR: unsupported mode' , Imode
         CALL PHO_ABORT
 
      END IF
 
      ii = ABS(ii)
      IF ( IDEb(75).GE.10 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I5,I12,I5)')
     &         'PHO_SELCOL: IMODE,MAXCOL,II' , Imode , maxcol , ii
         IF ( LPRi.GT.4 ) WRITE (LO,'(10X,A,2I5)') 'input  colors' , 
     &        ici1 , ici2
         IF ( LPRi.GT.4 ) WRITE (LO,'(10X,A,4I5)') 'output colors' , 
     &        Icoa1 , Icoa2 , Icob1 , Icob2
      END IF
 
      END SUBROUTINE
