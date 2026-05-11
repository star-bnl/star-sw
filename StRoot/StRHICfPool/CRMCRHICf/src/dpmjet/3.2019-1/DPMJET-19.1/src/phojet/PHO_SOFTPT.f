
      SUBROUTINE PHO_SOFTPT(Isoft,Ptcut,Ptmax,Xv,Iv,Ptsof)
C***********************************************************************
C
C    select pt of soft string ends
C
C    input:    ISOFT          number of soft partons
C                    -1       initialization
C                    >=0      sampling of p_t
C                    -2       output of statistics
C              PTCUT          cutoff for soft strings
C              PTMAX          maximal allowed PT
C              XV             field of x values
C              IV             0    sea quark
C                             1    valence quark
C
C    output:   /POINT3/       containing parameters AAS,BETAS
C              PTSOF          filed with soft pt values
C
C    note:     ISWMDL(3/4) = 0  dNs/dP_t = P_t ASS * exp(-BETA*P_t**2)
C              ISWMDL(3/4) = 1  dNs/dP_t = P_t ASS * exp(-BETA*P_t)
C              ISWMDL(3/4) = 2  photon wave function
C              ISWMDL(3/4) = 10 no soft P_t assignment
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION beta , betab , calls , cog , DEPS , Ptcut , 
     &                 Ptmax , pts , ptsmax , ptsmin , Ptsof , ptxs , 
     &                 ptys , sig , Xv
      INTEGER i , imode , Isoft , Iv
      SAVE 
 
      PARAMETER (DEPS=1.D-15)
 
      DIMENSION Ptsof(0:2,*) , Xv(*)
      DIMENSION Iv(*)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  data on most recent hard scattering
      INCLUDE 'inc/pockin'
C  data needed for soft-pt calculation
      INCLUDE 'inc/point3'
 
      DIMENSION betab(100)
 
C  selection of pt
      IF ( Isoft.GE.0 ) THEN
         calls = calls + 1.D0
C  sample according to model ISWMDL(3-6)
         IF ( Isoft.GT.1 ) THEN
 20         ptxs = 0.D0
            ptys = 0.D0
            DO i = 2 , Isoft
               imode = ISWmdl(3)
C  valence partons
               IF ( Iv(i).EQ.1 ) THEN
                  beta = BETas(1)
C  photon/pomeron valence part
                  IF ( IPAmdl(5).EQ.1 ) THEN
                     IF ( Xv(i).GE.0.D0 ) THEN
                        IF ( (IDPdg1.EQ.22) .OR. (IDPdg1.EQ.990) ) THEN
                           imode = ISWmdl(4)
                           beta = BETas(3)
                        END IF
                     ELSE IF ( (IDPdg2.EQ.22) .OR. (IDPdg2.EQ.990) )
     &                  THEN
                        imode = ISWmdl(4)
                        beta = BETas(3)
                     END IF
                  ELSE IF ( IPAmdl(5).EQ.2 ) THEN
                     beta = PARmdl(20)
                  ELSE IF ( IPAmdl(5).EQ.3 ) THEN
                     beta = BETas(3)
                  END IF
C  sea partons
               ELSE IF ( Iv(i).EQ.0 ) THEN
                  beta = BETas(3)
C  hard scattering remnant
               ELSE IF ( IPAmdl(6).EQ.0 ) THEN
                  beta = BETas(1)
               ELSE IF ( IPAmdl(6).EQ.1 ) THEN
                  beta = BETas(3)
               ELSE
                  beta = PARmdl(20)
               END IF
               beta = MAX(beta,0.01D0)
               CALL PHO_SELPT(Xv(i),0.D0,Ptcut,pts,beta,imode)
               pts = MIN(Ptmax,pts)
               CALL PHO_SFECFE(sig,cog)
               Ptsof(0,i) = pts
               Ptsof(1,i) = cog*pts
               Ptsof(2,i) = sig*pts
               ptxs = ptxs + Ptsof(1,i)
               ptys = ptys + Ptsof(2,i)
               betab(i) = beta
            END DO
C  balancing of momenta
            pts = SQRT(ptxs**2+ptys**2)
            IF ( pts.GE.Ptmax ) GOTO 20
            Ptsof(0,1) = pts
            Ptsof(1,1) = -ptxs
            Ptsof(2,1) = -ptys
            betab(1) = 0.D0
C
C400      CONTINUE
C
C  single parton only
         ELSE
            imode = ISWmdl(3)
C  valence partons
            IF ( Iv(1).EQ.1 ) THEN
               beta = BETas(1)
C  photon/Pomeron valence part
               IF ( IPAmdl(5).EQ.1 ) THEN
                  IF ( Xv(1).GE.0.D0 ) THEN
                     IF ( (IDPdg1.EQ.22) .OR. (IDPdg1.EQ.990) ) THEN
                        imode = ISWmdl(4)
                        beta = BETas(3)
                     END IF
                  ELSE IF ( (IDPdg2.EQ.22) .OR. (IDPdg2.EQ.990) ) THEN
                     imode = ISWmdl(4)
                     beta = BETas(3)
                  END IF
               ELSE IF ( IPAmdl(5).EQ.2 ) THEN
                  beta = PARmdl(20)
               ELSE IF ( IPAmdl(5).EQ.3 ) THEN
                  beta = BETas(3)
               END IF
C  sea partons
            ELSE IF ( Iv(1).EQ.0 ) THEN
               beta = BETas(3)
C  hard scattering remnant
            ELSE IF ( IPAmdl(6).EQ.1 ) THEN
               beta = BETas(3)
            ELSE
               beta = PARmdl(20)
            END IF
            beta = MAX(beta,0.01D0)
            CALL PHO_SELPT(Xv(1),0.D0,Ptcut,pts,beta,imode)
            pts = MIN(Ptmax,pts)
            CALL PHO_SFECFE(sig,cog)
            Ptsof(0,1) = pts
            Ptsof(1,1) = cog*pts
            Ptsof(2,1) = sig*pts
            betab(1) = beta
         END IF
 
C  debug output
         IF ( IDEb(29).GE.10 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I4)')
     &           'PHO_SOFTPT: ISOFT' , Isoft
            IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A)')
     &            'TABLE OF  I, IV, XV, PT, PT-X, PT-Y, BETA'
            DO i = 1 , Isoft
               IF ( LPRi.GT.4 ) WRITE (LO,'(10X,2I3,1P,5E12.3)') i , 
     &              Iv(i) , Xv(i) , Ptsof(0,i) , Ptsof(1,i) , Ptsof(2,i)
     &              , betab(i)
            END DO
         END IF
 
C  initialization of statistics and parameters
 
      ELSE IF ( Isoft.EQ.-1 ) THEN
         ptsmin = 0.D0
         ptsmax = Ptcut
 
         imode = -100 + ISWmdl(3)
         CALL PHO_SELPT(ECMp,ptsmin,ptsmax,pts,BETas(3),imode)
 
C  output of statistics
 
      ELSE IF ( Isoft.NE.-2 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,I2)') 'PHO_SOFTPT:ERROR: ' , 
     &        'unsupported ISOFT ' , Isoft
         STOP
      END IF
      END SUBROUTINE
