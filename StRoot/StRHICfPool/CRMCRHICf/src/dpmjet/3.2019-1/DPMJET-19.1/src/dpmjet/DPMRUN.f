
      SUBROUTINE DPMRUN(Elab,Ijdpm,Iap,Izp,Iat,Izt,Ldpmrj)
 
C***********************************************************************
C                                                                      *
C     Version September 2001      by   Stefan Roesler                  *
C     Last change  on  05-nov-01  by   Stefan Roesler                  *
C                                                                      *
C     This subroutine is part of the FLUKA interface to DPMJET 3.      *
C     Call to DPMJET 3 for event generation.                           *
C                                                                     *
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION dum , Elab , riap , riat , ridp
      INTEGER Iap , Iat , idp , IDPmev , IHEhad , IHEnuc , IHIjpr , 
     &        IHMapr , IHMata , Ijdpm , irej , Izp , Izt , kkmat
      SAVE 

#ifdef FOR_FLUKA
      INCLUDE '(IOUNIT)'
#else
      INCLUDE 'IOUNIT'
#endif

C event flag
      INCLUDE 'inc/dtevno'
C histogram indices for Fluka-interface related statistics
      COMMON /DTFLHX/ IHMapr , IHMata , IHIjpr , IHEnuc , IHEhad , 
     &                IDPmev
 
      INCLUDE 'inc/dtflka'
 
      LOGICAL Ldpmrj
 
      INCLUDE 'inc/dtevt1'
      INCLUDE 'inc/dtevt2'
Cc
Cc -----------------------------------------------------------------------------
      idp = Ijdpm
      IF ( idp.LT.0 ) idp = 1
Cc ---------------------------------
Cc
C     WRITE(6,*)' DPMRUN(ELAB,IJDPM,IAP,IZP,IAT,IZT,LDPMRJ)',
C    *ELAB,IJDPM,IAP,IZP,IAT,IZT,LDPMRJ
 
C  patch for photons: assume pi0 instead (should be obsolete)
      IF ( idp.EQ.7 ) idp = 23
      IF ( idp.EQ.26 ) idp = 23
C  patch for hadrons which cannot (yet) be handled by Dpmjet:
      IF ( idp.GT.26 ) THEN
C   for virtual vector mesons assume pi0 (as in eventv)
         IF ( Ijdpm.EQ.30 ) THEN
            idp = 23
         ELSE
C   otherwise assume pi instead
            WRITE (LUNDPM,*) 
     &                  ' EVENTD: Particle cannot be handled by Dpmjet '
     &                  , Ijdpm , idp
            idp = 23
         END IF
      END IF
Cc -----------------------------------------------------------------------------
Cc
C     write(0,*) ' -x-> ',IJDPM,iap,izp,iat,izt,ELAB
C     write(0,*) ' ----> eventd()    ',IJDPM,IJDPM,' - ',IAP,IAT,ELAB
 
Cc   ------------------------
      kkmat = -2
      NEVent = NEVent + 1
 
      CALL DT_KKINC(Iap,Izp,Iat,Izt,idp,Elab,kkmat,irej)
 
      Ldpmrj = irej.NE.0
Cc   ------------------------
 
 
C     if (lpri.gt.5) then
C     write(0,*) ' ----> eventd()    ',LDPMRJ,NHKK
C     DO I = 1, NHKK
C       IF ( ISTHKK(I) .EQ. 1000 ) ISTHKK(I) = 1001
C       if (ISTHKK(i).eq.1001.or.ISTHKK(i).eq.1) then
C     write(0,'(2I4,I6,4I4,5E13.5,2I3,I2,I4,a5)') i,ISTHKK(i),
C    *    IDHKK(i),JMOHKK(1,i),JMOHKK(2,i),JDAHKK(1,i),JDAHKK(2,i),
C    *    (PHKK(LL,i),LL=1,5),IDRES(i),IDXRES(i),NOBAM(i),IDBAM(i),
C    .    ' --o-'
C         if (i.eq.nhkk) write(0,*) '           ------- <<-o-<< ----'
C         endif
C       enddo
C       endif
 
 
 
      IF ( LPRi.LT.3 ) RETURN
C
C Internal statistics and call to usrhis
C
      CALL PHO_PHIST(2000,dum)
C
C Fill histograms with parameters of this interaction
C
      IDPmev = IDPmev + 1
      IF ( Iap.GT.1 ) THEN
         riap = DBLE(Iap)
         CALL DT_FILHGR(riap,1D0,IHMapr,IDPmev)
         CALL DT_FILHGR(Elab,1D0,IHEnuc,IDPmev)
      ELSE
         ridp = DBLE(idp)
         CALL DT_FILHGR(ridp,1D0,IHIjpr,IDPmev)
         CALL DT_FILHGR(Elab,1D0,IHEhad,IDPmev)
      END IF
      riat = DBLE(Iat)
      CALL DT_FILHGR(riat,1D0,IHMata,IDPmev)
 
C=== End of subroutine Dpmrun =========================================*
      END SUBROUTINE
