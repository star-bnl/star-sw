
      SUBROUTINE PHO_REGFLA(Jm1,Jm2,Iflr1,Iflr2,Irej)
C**********************************************************************
C
C     selection of reggeon flavours
C
C     input:    JM1,JM2      position index of mother hadrons
C
C     output:   IFLR1,IFLR2  valence flavours according to
C                            PDG conventions and JM1,JM2
C               IREJ         0  reggeon possible
C                            1  reggeon impossible
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DEPS , e1 , EPS
      INTEGER ifla1 , ifla2 , iflb1 , iflb2 , Iflr1 , Iflr2 , Irej , 
     &        iter , Jm1 , Jm2
      SAVE 
 
      PARAMETER (EPS=0.1D0,DEPS=1.D-15)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
      IF ( Jm1.GT.0 ) THEN
         Irej = 0
         iter = 0
C  available energy
         e1 = SQRT((PHEp(4,Jm1)+PHEp(4,Jm2))
     &        **2-(PHEp(1,Jm1)+PHEp(1,Jm2))**2-(PHEp(2,Jm1)+PHEp(2,Jm2))
     &        **2-(PHEp(3,Jm1)+PHEp(3,Jm2))**2)/2.D0
 50      iter = iter + 1
         IF ( iter.GT.50 ) THEN
            Irej = 1
C  debug output
            IF ( LPRi.GT.4 .AND. IDEb(41).GE.2 )
     &            WRITE (LO,'(/1X,A,2I7,1P,E12.4)')
     &            'PHO_REGFLA: rejection, no reggeon found for' , 
     &           IDHep(Jm1) , IDHep(Jm2) , e1
            RETURN
         END IF
 
         CALL PHO_VALFLA(Jm1,ifla1,ifla2,e1,e1)
         CALL PHO_VALFLA(Jm2,iflb1,iflb2,e1,e1)
         IF ( ifla1.EQ.-iflb1 ) THEN
            Iflr1 = ifla2
            Iflr2 = iflb2
         ELSE IF ( ifla1.EQ.-iflb2 ) THEN
            Iflr1 = ifla2
            Iflr2 = iflb1
         ELSE IF ( ifla2.EQ.-iflb1 ) THEN
            Iflr1 = ifla1
            Iflr2 = iflb2
         ELSE IF ( ifla2.EQ.-iflb2 ) THEN
            Iflr1 = ifla1
            Iflr2 = iflb1
         ELSE
C  debug output
            IF ( LPRi.GT.4 .AND. IDEb(41).GE.25 )
     &            WRITE (LO,'(/1X,A,3I4)')
     &            'PHO_REGFLA: int.rejection JM1,JM2,ITRY' , Jm1 , Jm2 , 
     &           iter
            GOTO 50
         END IF
C  debug output
         IF ( LPRi.GT.4 .AND. IDEb(41).GE.10 )
     &         WRITE (LO,'(1X,A,/5X,2I4,2I6,2I5,1PE10.3)')
     &         'PHO_REGFLA: JM1/2,PDG-ID1/2,IFLR1/2,MASS' , Jm1 , Jm2 , 
     &        IDHep(Jm1) , IDHep(Jm2) , Iflr1 , Iflr2 , e1
      ELSE IF ( Jm1.EQ.-1 ) THEN
C  initialization
      ELSE IF ( Jm1.NE.-2 ) THEN
C  output of statistics
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I10)')
     &         'PHO_REGFLA: invalid mother particle (JM1)' , Jm1
         CALL PHO_ABORT
      END IF
 
      END SUBROUTINE
