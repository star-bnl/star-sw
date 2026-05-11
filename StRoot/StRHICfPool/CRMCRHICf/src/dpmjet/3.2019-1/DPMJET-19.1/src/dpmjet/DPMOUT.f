
      SUBROUTINE DPMOUT(Ieve)
      IMPLICIT NONE
      INTEGER IDPmev , idum , Ieve , IHEhad , IHEnuc , IHIjpr , IHMapr , 
     &        IHMata
 
C***********************************************************************
C     Version March 2004                                               *
C     Last change                                                      *
C                                                                      *
C     This subroutine is part of the FLUKA interface to DPMJET 3.      *
C     Printout of DPMJET 3 event generation statistics.                *
C                                                                      *
C***********************************************************************
C
 
C histogram indices for Fluka-interface related statistics
      CHARACTER*72 header
      COMMON /DTFLHX/ IHMapr , IHMata , IHIjpr , IHEnuc , IHEhad , 
     &                IDPmev
 
      INCLUDE 'inc/dtflka'
      INCLUDE 'inc/dtsta1'
 
 
      IF ( LPRi.LT.3 .OR. ICRequ.LT.1 ) RETURN
 
      IF ( Ieve.EQ.5 ) CALL DT_DTUOUT
C
C Write histograms with interaction parameter distributions
C
      CALL DT_EVTHIS(idum)
      WRITE (LOUt,*) ' DPMOUT: Normalization factor = ' , IDPmev
      header = ' A_projectile'
      CALL DT_OUTHGR(IHMapr,0,0,0,0,0,header,0,IDPmev,1D+00,0,0,-1)
      header = ' A_target'
      CALL DT_OUTHGR(IHMata,0,0,0,0,0,header,0,IDPmev,1D+00,0,0,-1)
      header = ' Id_projectile'
      CALL DT_OUTHGR(IHIjpr,0,0,0,0,0,header,0,IDPmev,1D+00,0,0,-1)
      header = ' dN/dE (nuclei)'
      CALL DT_OUTHGR(IHEnuc,0,0,0,0,0,header,0,IDPmev,1D+00,0,1,-1)
      header = ' dN/dE (hadrons)'
      CALL DT_OUTHGR(IHEhad,0,0,0,0,0,header,0,IDPmev,1D+00,0,1,-1)
C
C=== Dpmout ===========================================================*
      END SUBROUTINE
