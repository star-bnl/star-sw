      PROGRAM PHOJET
C**********************************************************************
C
C     example how to call PHOJET using input cards
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      LINP = 5
      CALL PHO_INIT(LINP,6,IREJ)
      IF(IREJ.NE.0) WRITE(6,'(/1X,A,I8)') 'Error in PHO_INIT',IREJ

      END

      SUBROUTINE PHO_PHIST(IMODE,WEIGHT)
C**********************************************************************
C
C     example histrograms using PHOJET /POHEP1/ and /POHEP2/
C
C     input:  IMODE      -1  initialization
C                        -2  output of results
C                         1  take event statistics
C             WEIGHT     weight of event (if necessary)
C                        for final output: corresponding cross section
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE

      PARAMETER (ZERO   =  0.D0,
     &           IZERO  =  0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           PI     =  3.14159265359D0,
     &           TWOPI  =  6.28318530718D0,
     &           TINY   =  1.D-10,
     &           DEPS   =  1.D-10)

C  event debugging information
      INCLUDE 'inc/podebg'

C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'

C  general process information
      INCLUDE 'inc/poprcs'


C  standard particle data interface
      INCLUDE 'inc/poevt1'

C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'

      CHARACTER*72 TITLE
      DIMENSION XLIMB(40)

C
C  extract statistics
C
      IF(IMODE.EQ.1) THEN

C  main particle loop
        DO 100 I=1,NHEP
C  final particle
          IF(ISTHEP(I).EQ.1) THEN
C  particle charge
            ICHAR = IPHO_CHR3(I,2)/3
            PT2 = PHEP(1,I)**2+PHEP(2,I)**2
            IF(PT2.LT.1.D-8) GOTO 99
            PABS = SQRT(PT2+PHEP(3,I)**2)
            IF(PABS.LT.1.D-8) GOTO 99
            PT = SQRT(PT2)
            IF(ABS(PHEP(3,I)).GE.PABS) GOTO 99
            THETA = PHEP(3,I)/PABS
            IF(ABS(THETA).GE.1.D0) GOTO 99
            THETA = ACOS(THETA)
            IF((THETA.LT.1.D-8)
     &        .OR.(PI-THETA.LT.1.D-8)) GOTO 99
C  pseudorapidity
            ETA = -LOG(TAN(THETA/TWO))
C  rapidity
            YY = -1.D+10
            PPLUS  = PHEP(4,I)+PHEP(3,I)
            PMINUS = PHEP(4,I)-PHEP(3,I)
            IF((PPLUS*PMINUS).GT.DEPS) YY = 0.5D0*LOG(PPLUS/PMINUS)
C  transverse energy
            ET = PHEP(4,I)*SIN(THETA)

C  histogramming ....


          ENDIF

 99       CONTINUE
 100    CONTINUE




C
C  initialization
C
      ELSE IF(IMODE.EQ.-1) THEN


C
C  final output
C
      ELSE IF(IMODE.EQ.-2) THEN

C  weight is the cross section of the generated events
      SIGMA = WEIGHT  

      ENDIF

      END



      SUBROUTINE PHO_LHIST(IMODE,WEIGHT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      END