
      SUBROUTINE PHO_DSIGDT(Ee,Xta,Nfill)
C*********************************************************************
C
C     calculation of unitarized amplitude
C                    and differential cross section
C
C     input:   EE       cm energy (GeV)
C              XTA(1,*) t values (GeV**2)
C              NFILL    entries in t table
C
C     output:  XTA(2,*)  DSIG/DT  g p --> g h/V (mub/GeV**2)
C              XTA(3,*)  DSIG/DT  g p --> rho0 h/V
C              XTA(4,*)  DSIG/DT  g p --> omega0 h/V
C              XTA(5,*)  DSIG/DT  g p --> phi h/V
C              XTA(6,*)  DSIG/DT  g p --> pi+ pi- h/V (continuum)
C
C*********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION bmax , DEPS , Ee , etmp , fac , PHO_BESSJ0 , 
     &                 THOUS , wg , wght , xpnt , Xta , xx
      INTEGER i , i1 , i2 , iamp , ITHREE , ITWO , j , j1 , j2 , k , 
     &        k1 , k2 , l , l1 , l2 , Nfill
      SAVE 
 
      PARAMETER (ITWO=2,ITHREE=3,THOUS=1.D3,DEPS=1.D-20)
 
      DIMENSION Xta(6,Nfill)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  some constants
      INCLUDE 'inc/pocons'
C  integration precision for hard cross sections (obsolete)
      INCLUDE 'inc/pogaup'
C  event debugging information
      INCLUDE 'inc/podebg'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  complex Born graph amplitudes used for unitarization
      INCLUDE 'inc/point4'
 
      COMPLEX*16 xt , amp , czero
      DIMENSION amp(5) , xpnt(96) , wght(96) , xt(5,100)
      CHARACTER*12 fna
 
 
      czero = DCMPLX(0.D0,0.D0)
 
      etmp = ECM
      ECM = Ee
 
      IF ( Nfill.GT.100 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I4)')
     &         'PHO_DSIGDT:ERROR: too many entries in table' , Nfill
         STOP
      END IF
C
      DO k = 1 , Nfill
         DO l = 1 , 5
            xt(l,k) = czero
         END DO
      END DO
C
C  impact parameter integration
C     BMAX=12.D0*SQRT(MAX(BPOM,BREG))
      bmax = 10.D0
      CALL PHO_GAUSET(0.D0,bmax,NGAuso,xpnt,wght)
      iamp = 5
      IF ( (IFPap(1).EQ.22) .AND. (IFPap(2).NE.22) ) THEN
         i1 = 1
         i2 = 0
      ELSE IF ( (IFPap(1).NE.22) .AND. (IFPap(2).EQ.22) ) THEN
         i1 = 0
         i2 = 1
      ELSE IF ( (IFPap(1).EQ.22) .AND. (IFPap(2).EQ.22) ) THEN
         i1 = 1
         i2 = 1
      ELSE
         i1 = 0
         i2 = 0
         iamp = 1
      END IF
      j1 = i1*2
      k1 = i1*3
      l1 = i1*4
      j2 = i2*2
      k2 = i2*3
      l2 = i2*4
C
      DO i = 1 , NGAuso
         wg = wght(i)*xpnt(i)
C  calculate amplitudes
         IF ( i.EQ.1 ) THEN
            CALL PHO_EIKON(1,-1,xpnt(i))
         ELSE
            CALL PHO_EIKON(1,1,xpnt(i))
         END IF
         amp(1) = AMPel
         amp(2) = AMPvm(i1,i2)
         amp(3) = AMPvm(j1,j2)
         amp(4) = AMPvm(k1,k2)
         amp(5) = AMPvm(l1,l2)
C
         DO j = 1 , Nfill
            xx = xpnt(i)*SQRT(Xta(1,j)/GEV2mb)
            fac = PHO_BESSJ0(xx)*wg
            DO k = 1 , iamp
               xt(1,j) = xt(1,j) + amp(k)*fac
            END DO
         END DO
      END DO
C
C  change units to mb/GeV**2
      fac = 4.D0*PI/GEV2mb
      fna = '(mb/GeV**2) '
      IF ( i1+i2.EQ.1 ) THEN
         fac = fac*THOUS
         fna = '(mub/GeV**2)'
      ELSE IF ( i1+i2.EQ.2 ) THEN
         fac = fac*THOUS*THOUS
         fna = '(nb/GeV**2) '
      END IF
      IF ( IDEb(56).GE.5 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,A12,/1X,A)')
     &         'table:  -T (GeV**2)   DSIG/DT ' , fna , 
     &        '------------------------------------------'
      END IF
      DO j = 1 , Nfill
         DO k = 1 , iamp
            Xta(k+1,j) = ABS(xt(k,j))**2*fac
         END DO
         IF ( IDEb(56).GE.5 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,6E12.3)')
     &           (Xta(i,j),i=1,iamp+1)
         END IF
      END DO
 
      ECM = etmp
      END SUBROUTINE
