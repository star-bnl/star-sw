
      DOUBLE PRECISION FUNCTION PHO_ALPHAS(Q2,Imode)
C**********************************************************************
C
C     calculation of ALPHA_S
C
C     input:    IMODE = 1         lambda_QCD**2 for PDF 1 evolution
C                       2         lambda_QCD**2 for PDF 2 evolution
C                       3         lambda_QCD**2 for hard scattering
C               Q2      scale in GeV**2
C
C     initialization needed:
C               IMODE = 0         lambda values taken from PDF table
C                       -1        given Q2 is 4-flavour lambda 1
C                       -2        given Q2 is 4-flavour lambda 2
C                       -3        given Q2 is 4-flavour lambda 3
C
C
C**********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      DOUBLE PRECISION Q2 , PHO_CT14ALPHAS , regfac
      INTEGER Imode
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  hard scattering parameters used for most recent hard interaction
      INCLUDE 'inc/pohapa'
C  currently activated parton density parametrizations
      INCLUDE 'inc/poppdf'
 
      INCLUDE 'inc/pohscl'
 
      INTEGER i
 
      PHO_ALPHAS = 0.D0
      IF ( Imode.GT.0 ) THEN
         IF ( (IPAmdl(7).EQ.4) .AND. 
     &        .NOT.((AQQal.LT.1.D0) .OR. (AQQal.GT.1.D0)) .AND. 
     &        (NQQal.EQ.1) ) THEN
            regfac = Q2/(PARmdl(261)**2+Q2)
         ELSE IF ( IPAmdl(7).EQ.4 ) THEN
            WRITE (LO,*) AQQal , PARmdl(261) , NQQal
            WRITE (LO,*) 
     &                'PHO_ALPHAS: MISUSE OF REGULARIZATION PARAMETERS.'
            CALL PHO_ABORT
         ELSE
            regfac = 1.D0
         END IF
 
         IF ( Q2.LT.PARmdl(148) ) THEN
            NFBeta = 1
         ELSE IF ( Q2.LT.PARmdl(149) ) THEN
            NFBeta = 2
         ELSE IF ( Q2.LT.PARmdl(150) ) THEN
            NFBeta = 3
         ELSE
            NFBeta = 4
         END IF
 
         IF ( ((IEXt(1).EQ.0) .AND. (IGRp(1).EQ.2)) .AND. 
     &        ((IEXt(2).EQ.0) .AND. (IGRp(2).EQ.2)) ) THEN
            PHO_ALPHAS = PHO_CT14ALPHAS(SQRT(Q2)+PARmdl(261))
         ELSE
            PHO_ALPHAS = BQCd(NFBeta)
     &                   /LOG((Q2+PARmdl(261)**2)/ALQcd2(Imode,NFBeta))
         END IF
 
         PHO_ALPHAS = regfac*PHO_ALPHAS
         NFBeta = NFBeta + 2
 
      ELSE IF ( Imode.EQ.0 ) THEN
 
         DO i = 1 , 3
            IF ( i.EQ.3 ) THEN
               ALQcd2(i,2) = PDFlam(1)*PDFlam(2)
            ELSE
               ALQcd2(i,2) = PDFlam(i)*PDFlam(i)
            END IF
            ALQcd2(i,1) = PARmdl(148)*(ALQcd2(i,2)/PARmdl(148))
     &                    **(BQCd(1)/BQCd(2))
            ALQcd2(i,3) = PARmdl(149)*(ALQcd2(i,2)/PARmdl(149))
     &                    **(BQCd(3)/BQCd(2))
            ALQcd2(i,4) = PARmdl(150)*(ALQcd2(i,2)/PARmdl(150))
     &                    **(BQCd(4)/BQCd(2))
 
         END DO
 
      ELSE IF ( Imode.LT.0 ) THEN
 
         IF ( Imode.EQ.-4 ) THEN
            i = 3
            ALQcd2(i,2) = SQRT(ALQcd2(1,2)*ALQcd2(2,2))
         ELSE
            i = -Imode
            ALQcd2(i,2) = Q2
         END IF
         ALQcd2(i,1) = PARmdl(148)*(ALQcd2(i,2)/PARmdl(148))
     &                 **(BQCd(1)/BQCd(2))
         ALQcd2(i,3) = PARmdl(149)*(ALQcd2(i,2)/PARmdl(149))
     &                 **(BQCd(3)/BQCd(2))
         ALQcd2(i,4) = PARmdl(150)*(ALQcd2(i,2)/PARmdl(150))
     &                 **(BQCd(4)/BQCd(2))
 
      END IF
 
      END FUNCTION
