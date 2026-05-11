
      SUBROUTINE PHO_PARREM(Indx,Iout,Irem,Irej)
C**********************************************************************
C
C     selection of particle remnant flavour(s) (quark or diquark)
C
C     input:    INDX   index of particle in /POEVT1/
C               IOUT   parton which was taken out
C
C     output:   IREM   remnant according to valence flavours
C               IREJ   0  flavour combination possible
C                      1  flavour combination impossible
C
C     all particle ID are given according to PDG conventions
C
C**********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      INTEGER Indx , Iout , Irem , Irej
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  general particle data
      INCLUDE 'inc/popar2'
 
C  external functions
      INTEGER IPHO_DIQU
 
C  local variables
      INTEGER id , is , id1 , id2 , i , k , k1 , k2 , iqua , idq
      DIMENSION iqua(3) , idq(2)
 
      id1 = IDHep(Indx)
      id2 = IMPart(Indx)
      Irej = 0
 
      IF ( id2.EQ.0 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I6)')
     &         'PHO_PARREM: no CPC ID available for' , Indx
         CALL PHO_ABORT
      END IF
 
C**anfe more general treatment of particles with flavor mixing
      IF ( (id1.EQ.22) .OR. (id1.EQ.990) .OR. (id1.EQ.110) ) THEN
C  photon, pomeron or reggeon
         Irem = -Iout
         GOTO 100
      END IF
 
C  ordinary hadron
      id = ABS(id2)
      is = SIGN(1,id2)
      iqua(1) = IQ_list(1,id)*is
      iqua(2) = IQ_list(2,id)*is
      iqua(3) = IQ_list(3,id)*is
 
      IF ( (iqua(3).EQ.0) .AND. (iqua(2).EQ.-iqua(1)) ) THEN
C meson with flavor mixing
         Irem = -Iout
         IF ( (LPRi.GT.4) .AND. (IDEb(72).GE.10) ) WRITE (6,*)
     &         'PHO_PARREM: particle has flavor mixing ' , id , iqua(1)
     &        , iqua(2) , Irem
         GOTO 100
      END IF
 
 
C  compare to flavour content
      IF ( ABS(Iout).LT.1000 ) THEN
C  single quark requested
         IF ( iqua(1).EQ.Iout ) THEN
            k1 = 2
            k2 = 3
         ELSE IF ( iqua(2).EQ.Iout ) THEN
            k1 = 1
            k2 = 3
         ELSE IF ( iqua(3).EQ.Iout ) THEN
            k1 = 1
            k2 = 2
         ELSE
            GOTO 200
         END IF
         IF ( iqua(3).EQ.0 ) THEN
            Irem = iqua(k1)
         ELSE
            Irem = IPHO_DIQU(iqua(k1),iqua(k2))
         END IF
      ELSE IF ( iqua(3).NE.0 ) THEN
C  diquark requested from baryon
         idq(1) = Iout/1000
         idq(2) = (Iout-idq(1)*1000)/100
         DO i = 1 , 2
            DO k = 1 , 3
               IF ( idq(i).EQ.iqua(k) ) THEN
                  iqua(k) = 0
                  GOTO 50
               END IF
            END DO
            GOTO 200
 50      END DO
         Irem = iqua(1) + iqua(2) + iqua(3)
      END IF
 
C  debug output
 100  IF ( LPRi.GT.4 .AND. IDEb(72).GE.10 ) WRITE (LO,'(1X,A,5I6)')
     &      'PHO_PARREM: INDX,ID-PDG,ID-BAM,IOUT,IREM' , Indx , id1 , 
     &     id2 , Iout , Irem
      RETURN
 
C  rejection
 200  Irej = 1
      IF ( LPRi.GT.4 .AND. IDEb(72).GE.2 ) WRITE (LO,'(1X,A,5I7)')
     &      'PHO_PARREM: rejection IDPDG,Q1-3,IOUT' , IDHep(Indx) , 
     &     iqua , Iout
 
      END SUBROUTINE
