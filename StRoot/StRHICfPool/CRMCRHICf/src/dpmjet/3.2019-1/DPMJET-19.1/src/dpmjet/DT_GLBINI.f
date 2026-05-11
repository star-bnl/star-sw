
      SUBROUTINE DT_GLBINI(What)
 
C***********************************************************************
C Pre-initialization of profile function                               *
C This version dated 28.11.00 is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION debin , e , ecm , ecmini , ehi , elab , elo , 
     &                 ONE , plab , q2i , TINY14 , What , xi , ZERO
      INTEGER i , i0 , iasav , ibsav , idx , ie , ij , ijpini , ioffst , 
     &        iproj , itarg , j , jpeach , jpstep , k , KBACC , kproj , 
     &        MAXMSS , MAXOFF , nasav
      INTEGER nbsav , nebin , nlines , nproj , ntarg
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TINY14=1.D-14)
 
      LOGICAL lcms
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C properties of interacting particles
      INCLUDE 'inc/dtprta'
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C Glauber formalism: flags and parameters for statistics
      INCLUDE 'inc/dtglgp'

#ifdef FOR_CORSIKA
cdh  datadir for path to the data sets to be read in by dpmjet/phojet
      COMMON /DATADIR/ DATADIR
      CHARACTER*132    DATADIR
#endif

C number of data sets other than protons and nuclei
C at the moment = 2 (pions and kaons)
      PARAMETER (MAXOFF=2)
      DIMENSION ijpini(5) , ioffst(25)
      DATA ijpini/13 , 15 , 0 , 0 , 0/
C Glauber data-set to be used for hadron projectiles
C (0=proton, 1=pion, 2=kaon)
      DATA (ioffst(k),k=1,25)/0 , 0 , -1 , -1 , -1 , -1 , -1 , 0 , 0 , 
     &      -1 , -1 , 2 , 1 , 1 , 2 , 2 , 0 , 0 , 2 , 0 , 0 , 0 , 1 , 
     &      2 , 2/
C Acceptance interval for target nucleus mass
      PARAMETER (KBACC=6)
 
      PARAMETER (MAXMSS=100)
      DIMENSION iasav(MAXMSS) , ibsav(MAXMSS)
      DIMENSION What(6)
 
      DATA jpeach , jpstep/18 , 5/
 
C temporary patch until fix has been implemented in phojet:
C  maximum energy for pion projectile
C       DATA ECMXPI / 100000.0D0 /
C
C--------------------------------------------------------------------------
C general initializations
C
C  steps in projectile mass number for initialization
      IF ( What(4).GT.ZERO ) jpeach = INT(What(4))
      IF ( What(5).GT.ZERO ) jpstep = INT(What(5))
C
C  energy range and binning
      elo = ABS(What(1))
      ehi = ABS(What(2))
      IF ( elo.GT.ehi ) elo = ehi
      nebin = MAX(INT(What(3)),1)
      IF ( elo.EQ.ehi ) nebin = 0
      lcms = (What(1).LT.ZERO) .OR. (What(2).LT.ZERO)
      IF ( lcms ) THEN
         ecmini = ehi
      ELSE
         ecmini = SQRT(AAM(IJProj)**2+AAM(IJTarg)**2+2.0D0*AAM(IJTarg)
     &            *ehi)
      END IF
C
C  default arguments for Glauber-routine
      xi = ZERO
      q2i = ZERO
C
C  initialize nuclear parameters, etc.
 
Cc    CALL BERTTP
Cc    CALL INCINI
 
C
C  open Glauber-data output file
      idx = INDEX(CGLb,' ')
      k = 8
      IF ( idx.GT.1 ) k = idx - 1
#ifndef FOR_CORSIKA
      OPEN (LDAt,FILE=CGLb(1:k)//'.glb',STATUS='UNKNOWN')
#else
c  modification for use with corsika using path to data file in DATADIR
      OPEN(LDAT,STATUS='UNKNOWN',
     &  FILE=DATADIR(1:INDEX(DATADIR,' ')-1)//CGLB(1:K)//'.glb')
#endif
C
C--------------------------------------------------------------------------
C Glauber-initialization for proton and nuclei projectiles
C
C  initialize phojet for proton-proton interactions
      elab = ZERO
      plab = ZERO
      CALL DT_LTINI(IJProj,IJTarg,elab,plab,ecmini,1)
      CALL DT_PHOINI
C
C  record projectile masses
      nasav = 0
      nproj = MIN(IP,jpeach)
      DO kproj = 1 , nproj
         nasav = nasav + 1
         IF ( nasav.GT.MAXMSS ) STOP ' GLBINI: NASAV > MAXMSS ! '
         iasav(nasav) = kproj
      END DO
      IF ( IP.GT.jpeach ) THEN
         nproj = DBLE(IP-jpeach)/DBLE(jpstep)
         IF ( nproj.EQ.0 ) THEN
            nasav = nasav + 1
            IF ( nasav.GT.MAXMSS ) STOP ' GLBINI: NASAV > MAXMSS ! '
            iasav(nasav) = IP
         ELSE
            DO iproj = 1 , nproj
               kproj = jpeach + iproj*jpstep
               nasav = nasav + 1
               IF ( nasav.GT.MAXMSS ) STOP ' GLBINI: NASAV > MAXMSS ! '
               iasav(nasav) = kproj
            END DO
            IF ( kproj.LT.IP ) THEN
               nasav = nasav + 1
               IF ( nasav.GT.MAXMSS ) STOP ' GLBINI: NASAV > MAXMSS ! '
               iasav(nasav) = IP
            END IF
         END IF
      END IF
C
C  record target masses
      nbsav = 0
      ntarg = 1
      IF ( NCOmpo.GT.0 ) ntarg = NCOmpo
      DO itarg = 1 , ntarg
         nbsav = nbsav + 1
         IF ( nbsav.GT.MAXMSS ) STOP ' GLBINI: NBSAV > MAXMSS ! '
         IF ( NCOmpo.GT.0 ) THEN
            ibsav(nbsav) = IEMuma(itarg)
         ELSE
            ibsav(nbsav) = IT
         END IF
      END DO
C
C  print masses
      WRITE (LDAt,99010) nebin , ': ' , SIGN(elo,What(1)) , 
     &                   SIGN(ehi,What(2))
99010 FORMAT (I4,A,1P,2E13.5)
      nlines = DBLE(nasav)/18.0D0
      IF ( nlines.GT.0 ) THEN
         DO i = 1 , nlines
            IF ( i.EQ.1 ) THEN
               WRITE (LDAt,'(I4,A,18I4)') nasav , ': ' , 
     &                (iasav(j),j=1,18)
            ELSE
               WRITE (LDAt,'(6X,18I4)') (iasav(j),j=18*i-17,18*i)
            END IF
         END DO
      END IF
      i0 = 18*nlines + 1
      IF ( i0.LE.nasav ) THEN
         IF ( i0.EQ.1 ) THEN
            WRITE (LDAt,'(I4,A,18I4)') nasav , ': ' , 
     &             (iasav(j),j=i0,nasav)
         ELSE
            WRITE (LDAt,'(6X,18I4)') (iasav(j),j=i0,nasav)
         END IF
      END IF
      nlines = DBLE(nbsav)/18.0D0
      IF ( nlines.GT.0 ) THEN
         DO i = 1 , nlines
            IF ( i.EQ.1 ) THEN
               WRITE (LDAt,'(I4,A,18I4)') nbsav , ': ' , 
     &                (ibsav(j),j=1,18)
            ELSE
               WRITE (LDAt,'(6X,18I4)') (ibsav(j),j=18*i-17,18*i)
            END IF
         END DO
      END IF
      i0 = 18*nlines + 1
      IF ( i0.LE.nbsav ) THEN
         IF ( i0.EQ.1 ) THEN
            WRITE (LDAt,'(I4,A,18I4)') nbsav , ': ' , 
     &             (ibsav(j),j=i0,nbsav)
         ELSE
            WRITE (LDAt,'(6X,18I4)') (ibsav(j),j=i0,nbsav)
         END IF
      END IF
C
C  calculate Glauber-data for each energy and mass combination
C
C   loop over energy bins
      elo = LOG10(elo)
      ehi = LOG10(ehi)
      debin = (ehi-elo)/MAX(DBLE(nebin),ONE)
      DO ie = 1 , nebin + 1
         e = elo + DBLE(ie-1)*debin
         e = 10**e
         IF ( lcms ) THEN
            e = MAX(2.0D0*AAM(IJProj)+0.1D0,e)
            ecm = e
         ELSE
            plab = ZERO
            ecm = ZERO
            e = MAX(AAM(IJProj)+0.1D0,e)
            CALL DT_LTINI(IJProj,IJTarg,e,plab,ecm,0)
         END IF
C
C   loop over projectile and target masses
         DO itarg = 1 , nbsav
            DO iproj = 1 , nasav
               CALL DT_XSGLAU(iasav(iproj),ibsav(itarg),IJProj,xi,q2i,
     &                        ecm,1,1,-1)
            END DO
         END DO
C
      END DO
C
C--------------------------------------------------------------------------
C Glauber-initialization for pion, kaon, ... projectiles
C
      DO ij = 1 , MAXOFF
C
C  initialize phojet for this interaction
         elab = ZERO
         plab = ZERO
         IJProj = ijpini(ij)
         IP = 1
         IPZ = 1
 
C* newer PHOJET versions initialize new proj/targ combinations dynamically
C* no need to call the initialization again
         CALL DT_LTINI(IJProj,IJTarg,elab,plab,ecmini,1)
C
C  calculate Glauber-data for each energy and mass combination
C
C   loop over energy bins
         DO ie = 1 , nebin + 1
            e = elo + DBLE(ie-1)*debin
            e = 10**e
            IF ( lcms ) THEN
               e = MAX(2.0D0*AAM(IJProj)+TINY14,e)
               ecm = e
            ELSE
               plab = ZERO
               ecm = ZERO
               e = MAX(AAM(IJProj)+TINY14,e)
               CALL DT_LTINI(IJProj,IJTarg,e,plab,ecm,0)
            END IF
C
C   loop over projectile and target masses
            DO itarg = 1 , nbsav
               CALL DT_XSGLAU(1,ibsav(itarg),IJProj,xi,q2i,ecm,1,1,-1)
            END DO
C
         END DO
C
      END DO
 
C--------------------------------------------------------------------------
C close output unit(s), etc.
C
      CLOSE (LDAt)
 
      END SUBROUTINE
