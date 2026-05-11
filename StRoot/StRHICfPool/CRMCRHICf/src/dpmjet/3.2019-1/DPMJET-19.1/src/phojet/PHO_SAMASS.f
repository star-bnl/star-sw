
      SUBROUTINE PHO_SAMASS(Ifla,Rmass)
C**********************************************************************
C
C     resonance mass sampling of quasi elastic processes
C
C     input:   IFLA       PDG number of particle
C              IFLA   -1  initialization
C              IFLA   -2  output of statistics
C
C     output:  RMASS      particle mass (in GeV)
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION deltam , DT_RNDM , EPS , PHO_PMASS , rma , 
     &                 Rmass , sum , xi , xma , xmc
      INTEGER i , icall , Ifla , k , kk , kmax , kmin , kp , nstep , 
     &        NTABM
      SAVE 
 
      PARAMETER (EPS=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  parameters of the "simple" Vector Dominance Model
      INCLUDE 'inc/posvdm'
 
      PARAMETER (NTABM=50)
      DIMENSION xma(4,NTABM) , xmc(4,NTABM) , rma(4,NTABM)
      DIMENSION sum(4) , icall(4)
 
C*****************************************************************
C  initialization of tables
      IF ( Ifla.EQ.-1 ) THEN
C
         nstep = NTABM
         DO i = 1 , 4
            icall(i) = 0
 
            deltam = (RMAx(i)-RMIn(i))/DBLE(nstep-1)
            DO k = 1 , nstep
               rma(i,k) = RMIn(i) + deltam*DBLE(k-1)
            END DO
         END DO
C  calculate table of dsig/dm
         CALL PHO_DSIGDM(rma,xma,nstep)
C  output of table
         IF ( IDEb(35).GE.1 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/5X,A)')
     &            'table:   mass (GeV)  DSIG/DM (mub/GeV)'
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/1X,A)') 
     &        '  (m,  rho,     m,  omega,      m,   phi,    m,  pi+pi-)'
     &        , 
     &        ' -------------------------------------------------------'
            DO k = 1 , nstep
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,8E12.3)') rma(1,k) , 
     &              xma(1,k) , rma(2,k) , xma(2,k) , rma(3,k) , xma(3,k)
     &              , rma(4,k) , xma(4,k)
            END DO
         END IF
C  make second table for sampling
         DO i = 1 , 4
            sum(i) = 0.D0
            DO k = 2 , nstep
               sum(i) = sum(i) + (xma(i,k-1)+xma(i,k))/2.D0
               xmc(i,k) = sum(i)
            END DO
         END DO
C  normalization
         DO k = 1 , nstep
            DO i = 1 , 4
               xmc(i,k) = xmc(i,k)/xmc(i,nstep)
            END DO
         END DO
         IF ( IDEb(35).GE.10 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/5X,A)')
     &            'PHO_DSIGDM: normalized summed table:'
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/1X,A)') 
     &        '  (m,  rho,     m,  omega,      m,   phi,    m,  pi+pi-)'
     &        , 
     &        ' -------------------------------------------------------'
            DO k = 1 , nstep
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,8E12.3)') rma(1,k) , 
     &              xmc(1,k) , rma(2,k) , xmc(2,k) , rma(3,k) , xmc(3,k)
     &              , rma(4,k) , xmc(4,k)
            END DO
         END IF
C
C**************************************************
C  output of statistics
      ELSE IF ( Ifla.EQ.-2 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(2(/1X,A))')
     &         'PHO_SAMASS: statistics' , '----------------------'
         IF ( LPRi.GT.4 ) WRITE (LO,'(4(/8X,A,I10))') 'rho:   ' , 
     &        icall(1) , 'omega: ' , icall(2) , 'phi:   ' , icall(3) , 
     &        'pi+pi-:' , icall(4)
 
C
C********************************************************
C  sampling of RMASS
      ELSE
C  quasi-elastic vector meson production
         IF ( Ifla.EQ.113 ) THEN
            kp = 1
         ELSE IF ( Ifla.EQ.223 ) THEN
            kp = 2
         ELSE IF ( Ifla.EQ.333 ) THEN
            kp = 3
         ELSE IF ( Ifla.EQ.92 ) THEN
            kp = 4
C  quasi-elastic production of h*
         ELSE IF ( Ifla.EQ.91 ) THEN
            Rmass = 0.35D0
            RETURN
C  elastic hadron scattering
         ELSE
            Rmass = PHO_PMASS(Ifla,1)
            IF ( LPRi.GT.4 .AND. IDEb(35).GE.20 )
     &            WRITE (LO,'(1X,A,I7,E12.3)') 'PHO_SAMASS: IFLA,MASS' , 
     &           Ifla , Rmass
            RETURN
         END IF
C
C  sample mass of vector mesonsn / two-pi background
         xi = DT_RNDM(Rmass) + EPS
C  binary search
         IF ( (xmc(kp,1).LE.xi) .AND. (xmc(kp,nstep).GE.xi) ) THEN
            kmin = 1
            kmax = nstep
            DO WHILE ( (kmax-kmin).NE.1 )
               kk = (kmax+kmin)/2
               IF ( xi.LE.xmc(kp,kk) ) THEN
                  kmax = kk
               ELSE
                  kmin = kk
               END IF
            END DO
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &            'PHO_SAMASS:ERROR:XI out of range'
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I7,I6,3E12.4)')
     &            'EVENT,IFLA,XI,XImin,XImax' , KEVent , Ifla , xi , 
     &           xmc(kp,1) , xmc(kp,nstep)
            CALL PHO_ABORT
         END IF
C  fine interpolation
         Rmass = rma(kp,kmin) + (rma(kp,kmax)-rma(kp,kmin))
     &           /(xmc(kp,kmax)-xmc(kp,kmin))*(xi-xmc(kp,kmin))
         IF ( IDEb(35).GE.20 ) THEN
            IF ( LPRi.GT.4 .AND. IDEb(35).GE.25 )
     &            WRITE (LO,'(1X,A,3E15.3)')
     &            'PHO_SAMASS: MLEFT,MRIGHT,RMASS' , rma(kp,kmin) , 
     &           rma(kp,kmax) , Rmass
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I7,E12.3)')
     &            'PHO_SAMASS: IFLA,MASS' , Ifla , Rmass
         END IF
         icall(kp) = icall(kp) + 1
 
      END IF
      END SUBROUTINE
