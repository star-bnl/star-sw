
      SUBROUTINE PHO_DIFPRO(Ip,Icut,Id1,Id2,Xmass,P2v1,P2v2,Sprob,Iproc,
     &                      Isam,Jsam,Ksam,Idir)
C*********************************************************************
C
C     sampling of diffraction dissociation process
C
C     input:  IP       particle combination
C             ICUT     user imposed limitations
C             ID1/2    PDG particle code of scattering particles
C             XMASS    diffractively produced mass (GeV)
C             P2V1/2   virtuality of scattering particles (Gev**2)
C             SPROB    suppression factor for resolved single and
C                      double diffraction dissociation
C
C     output: IRPOC    process ID
C             ISAM     number of cut pomerons (soft)
C             JSAM     number of cut reggeons
C             KSAM     number of cut pomerons (hard)
C             IDIR     direct hard interaction
C
C*********************************************************************
      IMPLICIT NONE
      INTEGER Icut , Id1 , Id2 , Idir , Ip , Iproc , Isam , Jsam , Ksam
      DOUBLE PRECISION P2v1 , P2v2 , spro , Sprob , Xmass
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  general process information
      INCLUDE 'inc/poprcs'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C**anfe COMMON not needed here
C  energy-interpolation table
C      include 'inc/potabl'
 
      Isam = 0
      Jsam = 0
      Ksam = 0
      Idir = 0
 
      IF ( Xmass.GT.3.D0 ) THEN
C  rapidity gap survival probability
         spro = 1.D0
         IF ( ISWmdl(28).GE.1 ) spro = Sprob
C  sample interaction
         Iproc = 0
         CALL PHO_SAMPRO(Ip,Id1,Id2,Xmass,P2v1,P2v2,spro,Iproc)
      ELSE
         Iproc = 1
      END IF
      IF ( Iproc.EQ.1 ) CALL PHO_SAMPRB(Xmass,Ip,Isam,Jsam,Ksam)
C  non-diffractive hadron-pomeron interaction
      IF ( (Iproc.EQ.1) .OR. (Iproc.EQ.8) ) THEN
C  option for suppression of multiple interaction
         IF ( Icut.EQ.0 ) THEN
            Iproc = 1
            IF ( Isam+Ksam+Idir.GT.0 ) THEN
               Isam = 1
               Jsam = 0
            ELSE
               Jsam = 1
            END IF
            Ksam = 0
            Idir = 0
         ELSE IF ( Icut.EQ.1 ) THEN
            IF ( Idir.GT.0 ) THEN
            ELSE IF ( Ksam.GT.0 ) THEN
               Ksam = 1
               Isam = 0
               Jsam = 0
            ELSE IF ( Isam.GT.0 ) THEN
               Isam = 1
               Jsam = 0
            ELSE
               Jsam = 1
            END IF
         ELSE IF ( Icut.EQ.2 ) THEN
            Ksam = MIN(Ksam,1)
         ELSE IF ( Icut.EQ.3 ) THEN
            Isam = MIN(Isam,1)
         END IF
      END IF
      END SUBROUTINE
