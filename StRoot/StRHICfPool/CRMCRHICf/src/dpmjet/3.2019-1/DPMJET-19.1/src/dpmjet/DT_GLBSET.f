
      SUBROUTINE DT_GLBSET(Idproj,Na,Nb,Elab,Mode)
C***********************************************************************
C Interpolation of pre-initialized profile functions                   *
C This version dated 28.11.00 is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION b , bmx , bpr0 , bpr1 , bpro , bpro0 , bpro1 , 
     &                 bprofl , bstp , bstp0 , bstp1 , debin , 
     &                 DT_RNCLUS , e , ecm , ehi , Elab , elo , facb , 
     &                 face
      DOUBLE PRECISION facna , ONE , rada , radb , xe , xe0 , xe1 , 
     &                 xerr , xs , xs0 , xs1 , xsig , ZERO
      INTEGER i , i0 , ia , iabin , iaidx , ib , ibbin , Idproj , idx , 
     &        idx0 , idx1 , idxoff , idy0 , idy1 , ie0 , ie1 , ijpini , 
     &        ioffst , ip , ipold
      INTEGER isiteb , istatb , j , k , ka0 , ka1 , kabin , kb , KBACC , 
     &        MAXBIN , MAXOFF , MAXSET , Mode , Na , nabin , naidx , 
     &        Nb , nbacc , nbbin , nbdiff
      INTEGER nebin , nlines , nset , nset0
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0)
 
      LOGICAL lcms , lread , lfrst1 , lfrst2
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C Glauber formalism: flags and parameters for statistics
      INCLUDE 'inc/dtglgp'
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C Glauber formalism: parameters
      INCLUDE 'inc/dtglam'
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'

#ifdef FOR_CORSIKA
cdh  datadir for path to the data sets to be read in by dpmjet/phojet
      COMMON /DATADIR/ DATADIR
      CHARACTER*132    DATADIR
#endif

C     number of data sets other than protons and nuclei
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
      PARAMETER (MAXSET=5000,MAXBIN=100)
      DIMENSION xsig(MAXSET,6) , xerr(MAXSET,6) , bprofl(MAXSET,KSITEB)
      DIMENSION iabin(MAXBIN) , ibbin(MAXBIN) , xs(6) , xe(6) , 
     &          bpro0(KSITEB) , bpro1(KSITEB) , bpro(KSITEB) , iaidx(10)
 
      DATA lread , lfrst1 , lfrst2/.FALSE. , .TRUE. , .TRUE./
C
C read data from file
C
      IF ( Mode.EQ.0 ) THEN
 
 
         IF ( lread ) RETURN
         DO i = 1 , MAXSET
            DO j = 1 , 6
               xsig(i,j) = ZERO
               xerr(i,j) = ZERO
            END DO
            DO j = 1 , KSITEB
               bprofl(i,j) = ZERO
            END DO
         END DO
         DO i = 1 , MAXBIN
            iabin(i) = 0
            ibbin(i) = 0
         END DO
         DO i = 1 , KSITEB
            bpro0(i) = ZERO
            bpro1(i) = ZERO
            bpro(i) = ZERO
         END DO
 
         idx = INDEX(CGLb,' ')
         k = 12
         IF ( idx.GT.1 ) k = idx - 1
#ifndef FOR_CORSIKA
         OPEN (LDAt,FILE=CGLb(1:k)//'.glb',STATUS='UNKNOWN')
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) CGLb(1:k)//'.glb'
99010    FORMAT (/,' GLBSET: impact parameter distributions read from ',
     &           'file ',A12,/)
#else
c  modification for use with corsika using path to data file in DATADIR
         IF (LPRI.GT.4)
     &     WRITE(LOUT,*)'DT_GLBSET:read glauber parameter from file ',
     &     DATADIR(1:INDEX(DATADIR,' ')-1)//CGLB(1:K),'.glb',' K=',K

         OPEN(LDAT,STATUS='UNKNOWN',
     &     FILE=DATADIR(1:INDEX(DATADIR,' ')-1)//CGLB(1:K)//'.glb')
#endif

C
C  read binning information
         READ (LDAt,'(I4,2X,2E13.5)') nebin , elo , ehi
C  return lower energy threshold to Fluka-interface
         Elab = elo
         lcms = elo.LT.ZERO
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A)')
     &         ' equidistant logarithmic energy binning:'
         IF ( lcms ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99030) '(cms)' , ABS(elo) , 
     &           ABS(ehi) , nebin
         ELSE
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99030) '(lab)' , ABS(elo) , 
     &           ABS(ehi) , nebin
         END IF
         elo = LOG10(ABS(elo))
         ehi = LOG10(ABS(ehi))
         debin = (ehi-elo)/ABS(DBLE(nebin))
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(/,1X,A)')
     &         ' projectiles: (mass number)'
         READ (LDAt,'(I4,2X,18I4)') nabin , (iabin(j),j=1,18)
         IF ( nabin.LT.18 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(6X,18I4)')
     &           (iabin(j),j=1,nabin)
         ELSE
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(6X,18I4)') (iabin(j),j=1,18)
         END IF
         IF ( nabin.GT.MAXBIN ) STOP ' GLBSET: NABIN > MAXBIN !'
         IF ( nabin.GT.18 ) THEN
            nlines = DBLE(nabin-18)/18.0D0
            IF ( nlines.GT.0 ) THEN
               DO i = 1 , nlines
                  i0 = 18*(i+1) - 17
                  READ (LDAt,'(6X,18I4)') (iabin(j),j=i0,i0+17)
 
                  IF ( LPRi.GT.4 ) WRITE (LOUt,'(6X,18I4)')
     &                 (iabin(j),j=i0,i0+17)
               END DO
            END IF
            i0 = 18*(nlines+1) + 1
            IF ( i0.LE.nabin ) THEN
               READ (LDAt,'(6X,18I4)') (iabin(j),j=i0,nabin)
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,'(6X,18I4)')
     &              (iabin(j),j=i0,nabin)
            END IF
         END IF
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(/,1X,A)')
     &         ' targets: (mass number)'
         READ (LDAt,'(I4,2X,18I4)') nbbin , (ibbin(j),j=1,18)
         IF ( nbbin.LT.18 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(6X,18I4)')
     &           (ibbin(j),j=1,nbbin)
         ELSE
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(6X,18I4)') (ibbin(j),j=1,18)
         END IF
         IF ( nbbin.GT.MAXBIN ) STOP ' GLBSET: NBBIN > MAXBIN !'
         IF ( nbbin.GT.18 ) THEN
            nlines = DBLE(nbbin-18)/18.0D0
            IF ( nlines.GT.0 ) THEN
               DO i = 1 , nlines
                  i0 = 18*(i+1) - 17
                  READ (LDAt,'(6X,18I4)') (ibbin(j),j=i0,i0+17)
 
                  IF ( LPRi.GT.4 ) WRITE (LOUt,'(6X,18I4)')
     &                 (ibbin(j),j=i0,i0+17)
               END DO
            END IF
            i0 = 18*(nlines+1) + 1
            IF ( i0.LE.nbbin ) THEN
               READ (LDAt,'(6X,18I4)') (ibbin(j),j=i0,nbbin)
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,'(6X,18I4)')
     &              (ibbin(j),j=i0,nbbin)
            END IF
         END IF
C  number of data sets to follow in the Glauber data file
C   this variable is used for checks of consistency of projectile
C   and target mass configurations given in header of Glauber data
C   file and the data-sets which follow in this file
         nset0 = (nebin+1)*(nabin+MAXOFF)*nbbin
C
C  read profile function data
         nset = 0
         naidx = 0
         ipold = 0
 50      nset = nset + 1
         IF ( nset.GT.MAXSET ) STOP ' GLBSET: NSET > MAXSET ! '
         READ (LDAt,99020,END=100) ip , ia , ib , istatb , isiteb , ecm
99020    FORMAT (5I10,E15.5)
         IF ( (ip.NE.1) .AND. (ip.NE.ipold) ) THEN
            naidx = naidx + 1
            IF ( naidx.GT.10 ) STOP ' GLBSET: NAIDX > 10 !'
            iaidx(naidx) = ip
            ipold = ip
         END IF
         READ (LDAt,'(6E12.5)') (xsig(nset,i),i=1,6)
         READ (LDAt,'(6E12.5)') (xerr(nset,i),i=1,6)
         nlines = INT(DBLE(isiteb)/7.0D0)
         IF ( nlines.GT.0 ) THEN
            DO i = 1 , nlines
               READ (LDAt,'(7E11.4)') (bprofl(nset,j),j=7*i-6,7*i)
            END DO
         END IF
         i0 = 7*nlines + 1
         IF ( i0.LE.isiteb ) READ (LDAt,'(7E11.4)')
     &        (bprofl(nset,j),j=i0,isiteb)
         GOTO 50
 100     nset = nset - 1
         IF ( nset.NE.nset0 ) STOP ' GLBSET: NSET.NE.NSET0 !'
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(/,1X,A)') 
     &    ' projectiles other than protons and nuclei: (particle index)'
         IF ( naidx.GT.0 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(6X,18I4)')
     &           (iaidx(j),j=1,naidx)
         ELSE
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(6X,A)') 'none'
         END IF
C
         CLOSE (LDAt)
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*)
         lread = .TRUE.
 
         IF ( NCOmpo.EQ.0 ) THEN
            DO j = 1 , nbbin
               NCOmpo = NCOmpo + 1
               IEMuma(NCOmpo) = ibbin(j)
               IEMuch(NCOmpo) = IEMuma(NCOmpo)/2
               EMUfra(NCOmpo) = 1.0D0
            END DO
            IEMul = 1
         END IF
C
C calculate profile function for certain set of parameters
C
      ELSE
 
C        write(*,*) 'glbset called for ',IDPROJ,NA,NB,ELAB,MODE
C
C check for type of projectile and set index-offset to entry in
C Glauber data array correspondingly
         IF ( Idproj.GT.25 ) STOP ' GLBSET: IDPROJ > 25 !'
         IF ( ioffst(Idproj).EQ.-1 ) THEN
            STOP ' GLBSET: no data for this projectile !'
         ELSE IF ( ioffst(Idproj).GT.0 ) THEN
            idxoff = (nebin+1)*(nabin+ioffst(Idproj)-1)*nbbin
         ELSE
            idxoff = 0
         END IF
C
C get energy bin and interpolation factor
         IF ( lcms ) THEN
            e = SQRT(AAM(Idproj)**2+AAM(1)**2+2.0D0*AAM(1)*Elab)
         ELSE
            e = Elab
         END IF
         e = LOG10(e)
         IF ( e.LT.elo ) THEN
            IF ( lfrst1 ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &               ' GLBSET: Too low energy! (E_lo,E) ' , elo , e
               lfrst1 = .FALSE.
            END IF
            e = elo
         END IF
         IF ( e.GT.ehi ) THEN
            IF ( lfrst2 ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &               ' GLBSET: Too high energy! (E_hi,E) ' , ehi , e
               lfrst2 = .FALSE.
            END IF
            e = ehi
         END IF
         ie0 = (e-elo)/debin + 1
         ie1 = ie0 + 1
         face = (e-(elo+DBLE(ie0-1)*debin))/debin
C
C get target nucleus index
         kb = 0
         nbacc = KBACC
         DO i = 1 , nbbin
            nbdiff = ABS(Nb-ibbin(i))
            IF ( Nb.EQ.ibbin(i) ) THEN
               kb = i
               GOTO 150
            ELSE IF ( nbdiff.LE.nbacc ) THEN
               kb = i
               nbacc = nbdiff
            END IF
         END DO
         IF ( kb.EQ.0 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &            ' GLBSET: data not found for target ' , Nb
            STOP
         END IF
C
C get projectile nucleus bin and interpolation factor
 150     ka0 = 0
         ka1 = 0
         facna = 0
         IF ( idxoff.GT.0 ) THEN
            ka0 = 1
            ka1 = 1
            kabin = 1
         ELSE
            IF ( Na.GT.iabin(nabin) )
     &            STOP ' GLBSET: NA > IABIN(NABIN) !'
            DO i = 1 , nabin
               IF ( Na.EQ.iabin(i) ) THEN
                  ka0 = i
                  ka1 = i
                  GOTO 160
               ELSE IF ( Na.LT.iabin(i) ) THEN
                  ka0 = i - 1
                  ka1 = i
                  GOTO 160
               END IF
            END DO
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &            ' GLBSET: data not found for projectile ' , Na
            STOP
 160        IF ( ka0.NE.ka1 ) facna = DBLE(Na-iabin(ka0))
     &           /DBLE(iabin(ka1)-iabin(ka0))
            kabin = nabin
         END IF
C
C interpolate profile functions for interactions ka0-kb and ka1-kb
C for energy E separately
         idx0 = idxoff + 1 + (ie0-1)*kabin*nbbin + (kb-1)
     &          *kabin + (ka0-1)
         idx1 = idxoff + 1 + (ie1-1)*kabin*nbbin + (kb-1)
     &          *kabin + (ka0-1)
         idy0 = idxoff + 1 + (ie0-1)*kabin*nbbin + (kb-1)
     &          *kabin + (ka1-1)
         idy1 = idxoff + 1 + (ie1-1)*kabin*nbbin + (kb-1)
     &          *kabin + (ka1-1)
         DO i = 1 , isiteb
            bpro0(i) = bprofl(idx0,i)
     &                 + face*(bprofl(idx1,i)-bprofl(idx0,i))
            bpro1(i) = bprofl(idy0,i)
     &                 + face*(bprofl(idy1,i)-bprofl(idy0,i))
         END DO
         radb = DT_RNCLUS(Nb)
         bstp0 = 2.0D0*(DT_RNCLUS(iabin(ka0))+radb)/DBLE(isiteb-1)
         bstp1 = 2.0D0*(DT_RNCLUS(iabin(ka1))+radb)/DBLE(isiteb-1)
C
C interpolate cross sections for energy E and projectile mass
         DO i = 1 , 6
            xs0 = xsig(idx0,i) + face*(xsig(idx1,i)-xsig(idx0,i))
            xs1 = xsig(idy0,i) + face*(xsig(idy1,i)-xsig(idy0,i))
            xs(i) = xs0 + facna*(xs1-xs0)
            xe0 = xerr(idx0,i) + face*(xerr(idx1,i)-xerr(idx0,i))
            xe1 = xerr(idy0,i) + face*(xerr(idy1,i)-xerr(idy0,i))
            xe(i) = xe0 + facna*(xe1-xe0)
         END DO
C
C interpolate between ka0 and ka1
         rada = DT_RNCLUS(Na)
         bmx = 2.0D0*(rada+radb)
         bstp = bmx/DBLE(isiteb-1)
         bpro(1) = ZERO
         DO i = 1 , isiteb - 1
            b = DBLE(i)*bstp
C
C   calculate values of profile functions at B
            idx0 = b/bstp0 + 1
            IF ( idx0.GT.isiteb ) idx0 = isiteb
            idx1 = MIN(idx0+1,isiteb)
            facb = (b-DBLE(idx0-1)*bstp0)/bstp0
            bpr0 = bpro0(idx0) + facb*(bpro0(idx1)-bpro0(idx0))
            idx0 = b/bstp1 + 1
            IF ( idx0.GT.isiteb ) idx0 = isiteb
            idx1 = MIN(idx0+1,isiteb)
            facb = (b-DBLE(idx0-1)*bstp1)/bstp1
            bpr1 = bpro1(idx0) + facb*(bpro1(idx1)-bpro1(idx0))
C
            bpro(i+1) = bpr0 + facna*(bpr1-bpr0)
         END DO
C
C fill common dtglam
         NSIteb = isiteb
         RASh(1) = rada
         RBSh(1) = radb
         BMAx(1) = bmx
         BSTep(1) = bstp
         DO i = 1 , KSITEB
            BSIte(0,1,1,i) = bpro(i)
         END DO
C
C fill common dtglxs
         XSTot(1,1,1) = xs(1)
         XSEla(1,1,1) = xs(2)
         XSQep(1,1,1) = xs(3)
         XSQet(1,1,1) = xs(4)
         XSQe2(1,1,1) = xs(5)
         XSPro(1,1,1) = xs(6)
         XETot(1,1,1) = xe(1)
         XEEla(1,1,1) = xe(2)
         XEQep(1,1,1) = xe(3)
         XEQet(1,1,1) = xe(4)
         XEQe2(1,1,1) = xe(5)
         XEPro(1,1,1) = xe(6)
 
      END IF
99030 FORMAT (2X,A5,'  E_lo = ',1P,E9.3,'  E_hi = ',1P,E9.3,4X,
     &        'No. of bins:',I5,/)
 
      END SUBROUTINE
