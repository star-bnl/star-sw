      INTEGER*4 FUNCTION FITTER(
     & ntpc, xcir, ycir, zcir, xerr, yerr, zerr, param, chisquare,
     & covar, hypoid, npfit,good_hit)

C* INPUT
C* ntpc - number of points to fit
C* xcir,ycir,zcir - x,y,z of the point
C* xerr,yerr,zerr - errors on the point
C* hypoid - mass hypothesis for fit
C* minfit - minimum number of points

C* INPUT-OUTPUT
C* param   - fitting parameters
C* param(1) is theta0
C* param(2) is z0
C* param(3) is psi
C* param(4) is tan(lambda)
C* param(5) is q/pt

C* the correspondence between the COV(15) and COV(5,5) is
C* ( 1  2  3  4  5)  -  phi*R
C* ( 2  6  7  8  9)  -  z
C* ( 3  7 10 11 12)  -  tanl
C* ( 4  8 11 13 14)  -  psi
C* ( 5  9 12 14 15)  -  q/p

C* OUTPUT
C* chisquare
C* covar - covariance matrix


      IMPLICIT NONE
#include "math_constants.inc"
#include "phys_constants.inc"

      INTEGER IGPART ! GEANT particle ID
      COMMON/GPARID/IGPART

      INTEGER MXPNT
      PARAMETER (MXPNT = 50)

      REAL BFIELD(3),XYZ(3)

      DOUBLE PRECISION    xcir(mxpnt),ycir(mxpnt),zcir(mxpnt)
      DOUBLE PRECISION    xerr(mxpnt),yerr(mxpnt),zerr(mxpnt),pnt(mxpnt)
      DOUBLE PRECISION    chisquare, param(5)
      DOUBLE PRECISION    covar(5,5)

      REAL xfirst,yfirst,zfirst,xlast,ylast,zlast
      REAL x,y,z,xer,yer,zer,phi,psi,lambda,psii,psif,psir,parmass
      REAL tanl,cosl,q
      REAL r,x0,y0,dpsi,twopi
      REAL TRKGEN(6),TRKREF(6),TPC(6,MXPNT),TPCCH2(MXPNT),TRKTPC(23)
      INTEGER ihitseq(mxpnt)

      INTEGER minfit,iret,nevt,ntrk,ntpc,i,npfit,ic,ih,hypoid
      INTEGER ntpcused,nunused,j
      LOGICAL good_hit(*)

      real a,b

      XYZ(1) = 0.0
      XYZ(2) = 0.0
      XYZ(3) = 0.0
      CALL GUFLD(XYZ,BFIELD)

      CALL TPCINI(hypoid,0.001,0.01)

        twopi = C_2PI
C Fill the point arrays
        i=0
        nunused=0
      do j =1,ntpc
       if(ntpc.le.mxpnt)then
        i=i+1
        ihitseq(i)=j
        if(i.eq.1)then
          xfirst=xcir(j)
          yfirst=ycir(j)
          zfirst=zcir(j)
        endif
        x=xcir(j)
        y=ycir(j)
        z=zcir(j)
        xer = xerr(j)
        yer = yerr(j)
        zer = zerr(j)

        tpc(1,i) = sqrt(x**2+y**2) ! R
        phi = atan2(y,x) ! phi
C       convert to 0-2pi
        phi = amod(phi,twopi)
        if(phi.lt.0)phi=phi+C_2PI

        tpc(2,i) = phi*tpc(1,i) ! phi*R
        tpc(3,i) = z
        tpc(4,i) = ((xer*abs(y)+yer*abs(x))/tpc(1,i))**2 ! err of phi*R
        tpc(5,i) = 0.0 ! cov of phi*R,z
        tpc(6,i) = zer**2 ! err of z
        if(i.gt.1)then
         if(tpc(1,i).lt.tpc(1,i-1))then ! test for monotonicity
          good_hit(j)=.false.
          i = i - 1
          nunused = nunused + 1
         else
          xlast=xcir(j)
          ylast=ycir(j)
          zlast=zcir(j)
         endif ! tpc(
        endif ! i.gt
       endif ! ntpc.le
      enddo
      ntpcused = ntpc - nunused
C parameters at beginning of track
      x = xfirst
      y = yfirst
      z = zfirst
      q = sign(1.0,param(5)) ! charge
      trkgen(1) = sqrt(x**2+y**2) ! R
      phi = param(1)
      trkgen(2) = phi*trkgen(1) ! phi*R
      trkgen(3) = param(2) ! z
      tanl = param(4)
      trkgen(4) = tanl ! tanl
      trkgen(5) = param(3) ! psi
      trkgen(6) = param(5) ! q/pt

      psi = param(3)+sign(1.0,param(5))*C_PI_2 !mangled psi
      r = 1.0/(C_D_CURVATURE*bfield(3)*abs(param(5)))

      x0 = x - r*cos(psi)
      y0 = y - r*sin(psi)

C parameters at end of track
      x = xlast
      y = ylast
      z = zlast
      trkref(1) = sqrt(x**2+y**2) ! R
      phi = atan2(y,x)
C convert to 0-2pi
      phi = amod(phi,twopi)
      if(phi.lt.0)phi=phi+C_2PI
      trkref(2) = phi*trkref(1) ! phi*R
      psif = atan2(y-y0,x-x0)
      psii = atan2(yfirst-y0,xfirst-x0)
      dpsi = psif-psii
C -- put dpsi in 0-2pi range
      dpsi = amod(dpsi,real(C_2PI))
      psir = psi+dpsi - sign(1.0,param(5))*C_PI_2
C convert to 0-2pi
      psir = amod(psir,twopi)
      if(psir.lt.0)psir=psir+C_2PI

      trkref(4) = trkgen(4) ! tanl
      trkref(5) = psir
      trkref(6) = trkgen(6) ! q/pt

C -- check for rotation direction
      if(q*bfield(3).lt.0)then
        if(dpsi.lt.0)then
          dpsi = (C_2PI+dpsi)
        endif
      else
        if(dpsi.gt.0)then
           dpsi = -(C_2PI-dpsi)
        endif
      endif

      trkref(3) = param(2)-dpsi*r*sign(1.0,param(5))*tanl ! z

      CALL TPCTRK(NEVT,NTRK,TRKGEN,TRKREF,NTPCUSED,TPC,TPCCH2,TRKTPC,IRET)
C DEBUG
      IF(IRET.ne.0)then
        a=b
      endif
C return the new parameters and covariance matrix
      IF(IRET.eq.0)THEN
        param(1)=trktpc(2)/trktpc(1) ! theta
        param(2)=trktpc(3) ! z
C convert to 0-2pi
        psi = amod(trktpc(5),twopi)
        if(psi.lt.0)psi=psi+C_2PI
        param(3)=psi ! psi
        param(4)=trktpc(4) ! tanl
        param(5)=trktpc(6) ! q/pt 
        chisquare = trktpc(22)/trktpc(23) ! chisq/degf
        npfit = (ifix(trktpc(23))+5)/2 ! (ndegf+5)/2
        do ih =1,ntpcused ! tag the removed hits
          if(tpcch2(ih).ne.0)good_hit(ihitseq(ih))=.false.
        enddo
c fill in covar here
        ic = 6

        covar(1,1) = trktpc(ic+1) ! phi*r*phi*r
        covar(2,2) = trktpc(ic+6) ! z*z
        covar(3,3) = trktpc(ic+10)! tanl*tanl
        covar(4,4) = trktpc(ic+13)! psi*psi
        covar(5,5) = trktpc(ic+15)! q/pt*q/pt

        covar(1,2) = trktpc(ic+2) ! r*phi*z
        covar(1,3) = trktpc(ic+3) ! r*phi*tanl
        covar(1,4) = trktpc(ic+4) ! r*phi*psi
        covar(1,5) = trktpc(ic+5) ! r*phi*q/pt

        covar(2,1) = covar(1,2)
        covar(2,3) = trktpc(ic+7) ! z*tanl
        covar(2,4) = trktpc(ic+8) ! z*psi
        covar(2,5) = trktpc(ic+9) ! z*q/pt

        covar(3,1) = covar(1,3)
        covar(3,2) = covar(2,3)
        covar(3,4) = trktpc(ic+11)! tanl*psi
        covar(3,5) = trktpc(ic+12)! tanl*q/pt

        covar(4,1) = covar(1,4)
        covar(4,2) = covar(2,4)
        covar(4,3) = covar(3,4)
        covar(4,5) = trktpc(ic+14)! psi*q/pt

        covar(5,1) = covar(1,5)
        covar(5,2) = covar(2,5)
        covar(5,3) = covar(3,5)
        covar(5,4) = covar(4,5)

      ENDIF

      FITTER = IRET
      IF(IRET.NE.0)FITTER = -IRET

      RETURN
      END



      SUBROUTINE TPCINI(HYPOID,OUTFIL,OUTSMO)
*------------------------------------------------------------------------------
*--
*--   Initialise TPCFIT
*--
*--   Input:
*--     PARMASS .. Particle mass
*--     OUTFIL ... Outlier cut during filtering (Suggestion: 0.001)
*--     OUTSMO ... Outlier cut during smoothing (Suggestion: 0.01 )
*--
*--   Dietrich Liko
*--
*------------------------------------------------------------------------------
*--

#include "math_constants.inc"
#include "phys_constants.inc"

      PARAMETER (CONS=0.299792458D-3)
*--
***      CHARACTER*(*) CHTOP
      CHARACTER*80  CHSAVE
*--
        REAL PMASS
        COMMON/HMASS/PMASS

        DOUBLE PRECISION CONSB
        COMMON /VXMAGF/CONSB
      PARAMETER (MTPC=70)
      CHARACTER*80 TPCNAM
      COMMON/TPCRES/TPCNAM,CH2TOT,NDEF,CH2FIL,CH2SMO,NOUT,SP(5,MTPC),
     &              FRES(2,MTPC),FPUL(2,MTPC),FCH2(MTPC),
     &              SRES(2,MTPC),SPUL(2,MTPC),SCH2(MTPC)

      Real bfield(3),xyz(3)
      Integer hypoid

      PMASS = M_PION_MINUS ! Default
      if(hypoid.eq.2)PMASS = M_ELECTRON ! Electron mass
      if(hypoid.eq.3)PMASS = M_ELECTRON ! Positron mass
      if(hypoid.eq.5)PMASS = M_MUON_PLUS ! Muon mass
      if(hypoid.eq.6)PMASS = M_MUON_PLUS ! Muon mass
      if(hypoid.eq.11)PMASS = M_KAON_PLUS ! Kaon mass
      if(hypoid.eq.12)PMASS = M_KAON_PLUS ! Kaon mass
      if(hypoid.eq.14)PMASS = M_PROTON ! Proton mass
      if(hypoid.eq.15)PMASS = M_PROTON ! AntiProton mass
      if(hypoid.eq.45)PMASS = M_DEUTERON ! Deuteron mass


      IGPART = hypoid
*--
*--   magnetic field (in kGauss=0.1 Tesla)
*--
      xyz(1) = 0.0
      xyz(2) = 0.0
      xyz(3) = 0.0
      call gufld(xyz,bfield)
      CONSB=CONS*bfield(3)
*--
* CHISIN returns chisq(P,N) where P is the probability (0 - 1.0)
*        and N is the degrees of freedom
      IF ((OUTFIL.LE.0.).OR.(OUTFIL.GE.1.)) THEN
CCC         CH2FIL=1.E20
         CH2FIL=OUTFIL
      ELSE
         CH2FIL=CHISIN(1.-OUTFIL,2)
      ENDIF
      IF ((OUTSMO.LE.0.).OR.(OUTSMO.GE.1.)) THEN
CCC         CH2SMO=1.E20
         CH2SMO=OUTSMO
      ELSE
         CH2SMO=CHISIN(1.-OUTSMO,2)
      ENDIF
*--
*      IF (CHTOP.NE.' ') THEN
*         TPCNAM='//PAWC/'//CHTOP
*         CALL HCDIR(CHSAVE,'R')
*         CALL HMDIR(TPCNAM,'S')
*--
*         CALL TPCNT1(1100,CHTOP)
*         CALL TPCNT2(1200,CHTOP)
*--
*         CALL HCDIR(CHSAVE,' ')
*      ELSE
*         TPCNAM=' '
*      ENDIF
*--
      RETURN
*__
      END
      SUBROUTINE TPCNT1(NTID,CHRZ)
*------------------------------------------------------------------------------
*--
*--   Book Track Ntuple
*--
*--   Input:
*--     NTID ... Ntuple ID
*--     CHRZ ... Name of TopDir
*--
*--   Dietrich Liko
*------------------------------------------------------------------------------
*--
*      PARAMETER (NTAG=31)
*--
*      CHARACTER*(*) CHRZ
*--
*      CHARACTER*4 CHTAG(NTAG)
*--
*      DATA CHTAG/'NEVT','NTRK','NTPC','NOUT','PROB','R',
*     &           'P1','P2','P3','P4','P5',
*     &           'C11','C12','C13','C14','C15',
*     &           'C22','C23','C24','C25','C33',
*     &           'C34','C35','C44','C45','C55',
*     &           'S1','S2','S3','S4','S5'/
*--
*      CALL HBOOKN(NTID,'TPC track',NTAG,CHRZ,1000,CHTAG)
*--
*      RETURN
*--
      END
      SUBROUTINE TPCNT2(NTID,CHRZ)
*------------------------------------------------------------------------------
*--
*--   Book TPC Hit Ntuple
*--
*--   Input:
*--     NTID ... Ntuple ID
*--     CHRZ ... Name of TopDir
*--
*--   Dietrich Liko
*------------------------------------------------------------------------------
*--
*      PARAMETER (NTAG=20)
*--
*      CHARACTER*(*) CHRZ
*--
*      CHARACTER*4 CHTAG(NTAG)
*      DATA CHTAG/'NEVT','NTRK','NTPC','NHIT','R',
*     &           'P1','P2','P3','P4','P5',
*     &           'FRS1','FRS2','FPL1','FPL2','FPRB',
*     &           'SRS1','SRS2','SPL1','SPL2','SPRB'/
*--
*      CALL HBOOKN(NTID,'TPC Hit',NTAG,CHRZ,1000,CHTAG)
*--
      RETURN
*--
      END
      SUBROUTINE TPCEND
*------------------------------------------------------------------------------
*--
*--   Clean up HBOOK
*--
*--   Dietrich Liko
*--
*------------------------------------------------------------------------------
*--
*      PARAMETER (MTPC=70)
*      CHARACTER*80 TPCNAM
*      COMMON/TPCRES/TPCNAM,CH2TOT,NDEF,CH2FIL,CH2SMO,NOUT,SP(5,MTPC),
*     &              FRES(2,MTPC),FPUL(2,MTPC),FCH2(MTPC),
*     &              SRES(2,MTPC),SPUL(2,MTPC),SCH2(MTPC)
*--
*      CHARACTER*80 CHSAVE
*--
*      IF (TPCNAM.NE.' ') THEN
*         CALL HCDIR(CHSAVE,'R')
*         CALL HCDIR('//'//TPCNAM(8:),' ')
*         CALL HROUT(1100,IC,' ')
*         CALL HDELET(1100)
*         CALL HROUT(1200,IC,' ')
*         CALL HDELET(1200)
*         CALL HCDIR(CHSAVE,' ')
*      ENDIF
*--
*      RETURN
*--
      END
      SUBROUTINE TPCTRK(NEVT,NTRK,
     &                  TRKGEN,TRKREF,NTPC,TPC,TPCCH2,TRKTPC,IRET)
*------------------------------------------------------------------------------
*--
*--   fit a TPC track
*--
*--   Input:
*--      TRKGEN(6)   ... track at refrence surface, generation parameter
*--      TRKREF(6)   ... reference track
*--      NHIT        ... Number of TPC hits
*--      TPC(6,MHIT) ... TPC hits (1 hit/layer)
*--                      (R,R*PHI,z,c11,c12,c21)
*--
*--   Output:
*--      TRKTPC      ... track at reference surface within the beam tube
*--                      (R,R*PHI,z,theta,phi,1/p,c11,..,c55,chi2,ndef)
*--      IRET        ... return code
*--
*--   Dietrich Liko
*------------------------------------------------------------------------------
*--
      PARAMETER (PI = 3.14159265359)
*--
      PARAMETER (MTPC=70)
      CHARACTER*80 TPCNAM
      COMMON/TPCRES/TPCNAM,CH2TOT,NDEF,CH2FIL,CH2SMO,NOUT,SP(5,MTPC),
     &              FRES(2,MTPC),FPUL(2,MTPC),FCH2(MTPC),
     &              SRES(2,MTPC),SPUL(2,MTPC),SCH2(MTPC)
*--
      REAL TRKGEN(6),TRKREF(6),TPC(6,*),TPCCH2(*),TRKTPC(23)
      CHARACTER*80 CHSAVE
*--
      REAL TRK(31),HIT(20)
*--
      CALL TPCFIT(TRKREF(1),NTPC,TPC(1,1),TPCCH2(1),TRKTPC(1),IERR)
      IF (IERR.NE.0) THEN
         IRET = IERR
         RETURN
      ENDIF

*--   some checks ...

      R = TRKTPC(1)
      IF (TRKTPC(2).LT.0)       WRITE(6,*) 'TRKTPC(2)-',TRKTPC(2)
      IF (TRKTPC(2).GT.2.*PI*R) WRITE(6,*) 'TRKTPC(2)+',TRKTPC(2)
CCC      IF (TRKTPC(4).LT.0)       WRITE(6,*) 'TRKTPC(4)-',TRKTPC(4)
CCC      IF (TRKTPC(4).GT.PI)      WRITE(6,*) 'TRKTPC(4)+',TRKTPC(4)
      IF (TRKTPC(5).LT.0)       WRITE(6,*) 'TRKTPC(5)-',TRKTPC(5)
      IF (TRKTPC(5).GT.2.*PI)   WRITE(6,*) 'TRKTPC(5)+',TRKTPC(5)
*--
*--   Fill Ntuples
*--
*      IF (TPCNAM.NE.' ') THEN
*--
*         CALL HCDIR(CHSAVE,'R')
*         CALL HCDIR(TPCNAM,' ')
*         TRK(1) = REAL( NEVT )
*         TRK(2) = REAL( NTRK )
*         TRK(3) = REAL( NTPC )
*         TRK(4) = REAL( NOUT )
*         TRK(5) = PROB( TRKTPC(22), INT( TRKTPC(23) ) )
*         CALL UCOPY(TRKTPC(1),TRK(6),21)
*         CALL UCOPY(TRKGEN(2),TRK(27),5)
*         IF ((TRK(27)-TRK(7)).GT. PI*TRK(6))
*     &      TRK(27)=TRK(27)-2.*PI*TRK(6)
*         IF ((TRK(27)-TRK(7)).LT.-PI*TRK(6))
*     &      TRK(27)=TRK(27)+2.*PI*TRK(6)
*         IF ((TRK(30)-TRK(10)).GT. PI) TRK(30)=TRK(30)-2.*PI
*         IF ((TRK(30)-TRK(10)).LT.-PI) TRK(30)=TRK(30)+2.*PI
*         CALL HFN(1100,TRK(1))
*--
*         HIT(1) = REAL( NEVT )
*         HIT(2) = REAL( NTRK )
*         HIT(3) = REAL( NTPC )
*         II = 0
*         DO 1000 I = NTPC,1,-1
*            IF (TPCCH2(I).GE.0.) THEN
*               II = II + 1
*               HIT( 4) = REAL( II )
*               HIT( 5) = TPC(1,I)
*               CALL UCOPY(SP(1,I),HIT(6),5)
*               HIT(11) = FRES(1,I)
*               HIT(12) = FRES(2,I)
*               HIT(13) = FPUL(1,I)
*               HIT(14) = FPUL(2,I)
*               HIT(15) = PROB( FCH2(I), 2)
*               HIT(16) = SRES(1,I)
*               HIT(17) = SRES(2,I)
*               HIT(18) = SPUL(1,I)
*               HIT(19) = SPUL(2,I)
*               HIT(20) = PROB( SCH2(I), 2)
*               CALL HFN(1200,HIT(1))
*            ENDIF
* 1000    CONTINUE
*--
*         CALL HCDIR(CHSAVE,' ')
*      ENDIF
      IRET = 0
      RETURN
*--
      END
      SUBROUTINE TPCFIT(TRKREF,NTPC,TPC,TPCCH2,TRKTPC,IRET)
*------------------------------------------------------------------------------
*--
*--   fit a TPC track
*--
*--   Input:
*--      TRKREF(6)   ... reference track
*--      NHIT        ... Number of TPC hits
*--      TPC(6,MHIT) ... TPC hits (1 hit/layer)
*--                      (R,R*PHI,z,c11,c12,c21)
*--
*--   Output:
*--      TRKTPC      ... track at reference surface within the beam tube
*--                      (R,R*PHI,z,theta,phi,1/p,c11,..,c55,chi2,ndef)
*--      IRET        ... return code
*--
*--   Dietrich Liko
*------------------------------------------------------------------------------
*--
      PARAMETER (PI = 3.14159265359)
*--
      PARAMETER (MTPC=70)
      CHARACTER*80 TPCNAM
      COMMON/TPCRES/TPCNAM,CH2TOT,NDEF,CH2FIL,CH2SMO,NOUT,SP(5,MTPC),
     &              FRES(2,MTPC),FPUL(2,MTPC),FCH2(MTPC),
     &              SRES(2,MTPC),SPUL(2,MTPC),SCH2(MTPC)
*--
      REAL TRKGEN(6),TRKREF(6),TPC(6,*),TPCCH2(*),TRKTPC(23)
*--
      REAL PR(5),P(5),C(15),V(2),CV(3),CINI(15)
CCC      DATA CINI/1.E2 ,0.   ,0.   ,0.   ,0.   ,
CCC     &                1.E1 ,0.   ,0.   ,0.   ,
CCC     &                      1.E-1,0.   ,0.   ,
CCC     &                            1.E-1 ,0.   ,
CCC     &                                  1.E0 /
      DATA CINI/1.E0 ,0.   ,0.   ,0.   ,0.   ,
     &                1.E0 ,0.   ,0.   ,0.   ,
     &                      5.E-4 ,0.   ,0.   ,
     &                            4.E-2 ,0.   ,
     &                                  1.E-2 /
*--
*--   PR ... reference track
*--   P  ... track parameter
*--   C  ... cov. matrix
*--
      IF (NTPC.LE.3) THEN
         IRET = 1000
         RETURN
      ENDIF
*--
*--   Initialise track fit at outer TPC hit
*--
*--
      CALL UCOPY(TRKREF(2),PR(1),5)
      CALL KTINFI(PR(1),PR(1),CINI(1))
*--
*--   first filtering ...
*--
      CALL VZERO(TPCCH2(1),NTPC)
      NOUT=0
*--
      RL = TRKREF(1)
      DO 1000 I=NTPC,1,-1
         R=TPC(1,I)
         CALL TPCACT(RL,PR(1),P(1),C(1),R,TPC(2,I),TPC(4,I),CH2,IERR)
         IF (IERR.GT.0) THEN
            IRET=1000+IERR
            RETURN
         ENDIF
         IF (CH2.GT.CH2FIL) THEN
            CALL KTFOUT(P(1),C(1))
            TPCCH2(I)=-1.
            NOUT=NOUT+1
         ENDIF
 1000 CONTINUE
*--
*--   Initialise smoothing
*--
      CALL KTINSM(SP(1,1),C(1))
      IF (TPCCH2(1).GE.0) THEN
         CALL KTSCH2(SRES(1,1),SPUL(1,1),SCH2(1),IERR)
         IF (IERR.NE.0) THEN
            IRET=2000+IERR
            RETURN
         ENDIF
         IF (SCH2(1).GT.CH2SMO) THEN
            CALL KTSOUT(SP(1,1),C(1),IERR)
            IF (IERR.NE.0) THEN
               IRET=3000+IERR
               RETURN
            ENDIF
            TPCCH2(1)=-1.
            NOUT=NOUT+1
         ENDIF
      ENDIF
*--
      DO 2000 I=2,NTPC
         CALL KTSMOO(SP(1,I),C(1),IERR)
         IF (IERR.NE.0) THEN
            IRET=4000+IERR
            RETURN
         ENDIF
         IF (TPCCH2(I).GE.0.) THEN
            CALL KTSCH2(SRES(1,I),SPUL(1,I),SCH2(I),IERR)
            IF (IERR.NE.0) THEN
               IRET=5000+IERR
               RETURN
            ENDIF
            IF (SCH2(I).GT.CH2SMO) THEN
               CALL KTSOUT(SP(1,I),C(1),IERR)
               IF (IERR.NE.0) THEN
                  IRET=6000+IERR
                  RETURN
               ENDIF
               TPCCH2(I)=-1.
               NOUT=NOUT+1
            ENDIF
         ENDIF
 2000 CONTINUE
*--
      IF ((NTPC-NOUT).LE.3) THEN
         IRET = 1001
         RETURN
      ENDIF
*--
*--
      CH2TOT=0.
      NDEF=-5
      CALL KTINRF(P(1),C(1))
      DO 3000 I=NTPC,1,-1
         CALL KTREPR(P(1),IERR)
         IF (IERR.NE.0) THEN
            IRET = 7000+IERR
            RETURN
         ENDIF
         CALL KTFILT(P(1),C(1),IERR)
         IF (IERR.NE.0) THEN
            IRET = 8000+IERR
            RETURN
         ENDIF
         IF (TPCCH2(I).GE.0) THEN
            CALL KTFCH2(FRES(1,I),FPUL(1,I),FCH2(I),IERR)
            IF (IERR.GT.0) THEN
               IRET = 9000+IERR
               RETURN
            ENDIF
            CH2TOT=CH2TOT+FCH2(I)
            NDEF=NDEF+2
         ELSE
            CALL KTFOUT(P(1),C(1))
         ENDIF
 3000    CONTINUE
*--
      TRKTPC( 1) = R
      CALL UCOPY(P(1),TRKTPC(2),5)
      CALL UCOPY(C(1),TRKTPC(7),15)
      IF (TRKTPC(2).LT.0)       TRKTPC(2) = TRKTPC(2) + 2.*PI*R
      IF (TRKTPC(2).GT.2.*PI*R) TRKTPC(2) = TRKTPC(2) - 2.*PI*R
CCC      IF (TRKTPC(5).LT.0)       TRKTPC(5) = TRKTPC(5) + 2.*PI
CCC      IF (TRKTPC(5).GT.2.*PI)   TRKTPC(5) = TRKTPC(5) - 2.*PI
      TRKTPC(22) = CH2TOT
      TRKTPC(23) = NDEF
*--
*--   After re-filtering, go back to smoothing ....
*--
*--   Initialise smoothing
*--
      CALL KTINSM(SP(1,1),C(1))
      IF (TPCCH2(1).GE.0) THEN
         CALL KTSCH2(SRES(1,1),SPUL(1,1),SCH2(1),IERR)
         IF (IERR.NE.0) THEN
            IRET=10000+IERR
            RETURN
         ENDIF
      ENDIF
*--
      DO 4000 I=2,NTPC
         CALL KTSMOO(P(1),C(1),IERR)
         IF (IERR.NE.0) THEN
            IRET=11000+IERR
            RETURN
         ENDIF
         IF (TPCCH2(I).GE.0.) THEN
            CALL KTSCH2(SRES(1,I),SPUL(1,I),SCH2(I),IERR)
            IF (IERR.NE.0) THEN
               IRET=12000+IERR
               RETURN
            ENDIF
         ENDIF
 4000 CONTINUE
*--
      IRET = 0
      RETURN
*--
      END
      SUBROUTINE TPCACT(RL,PR,P,C,R,V,CV,CH2,IRET)
*-----------------------------------------------------------------------------
*--
*--   tracking to an active surface surface
*--
*--   Input:
*--     RL    ... current radius
*--     PR(5) ... current reference track
*--     P(5)  ... current parameter vector
*--     C(15) ... current cov. matrix
*--     R     ... new radius (<=RL)
*--     V(2)  ... TPC hit (R*PHI,z)
*--     CV(3) ... Cov. matrix
*--     CH2   ... Contribution of Hit
*--
*--   Output:
*--     PR(5) ... new reference track
*--     P(5)  ... new parameter vector
*--     C(15) ... new cov. matrix
*--     RL    ... updated to R
*--
*--   Dietrich Liko
*--
*------------------------------------------------------------------------------
*--
      PARAMETER (PI = 3.14159265359)
*--
      REAL PR(5),P(5),C(15),V(2),CV(3),RES(2),PUL(2),CMS(15)
*--
      REAL DER(5,5),HT(5,2)
      DATA HT/1.,0.,0.,0.,0.,
     &        0.,1.,0.,0.,0./
*--
      IF (RL.LT.R) THEN
         IRET=100
         RETURN
      ENDIF

      Do i=1,5
       P(i) = PR(i)
      enddo

*--
*--   track model along reference surface
*--
      CALL VXBTRD(RL,PR(1),R,PR(1),DER(1,1),IERR)
      IF (IERR.GT.0) THEN
        IRET = 200+IERR
        RETURN
      ENDIF
*--
*--   TPC is filled with P10 
*--
CCC      COSL=1./SQRT(1.+PR(3)**2)
CCC      X = (RL - R)/(COSL*11600.)
CCC      CALL TPCSCA(R,PR(1),X,CMS(1))
CCC      CALL TPCSCA(R,RL,PR(1),P(1),X,CMS(1))
      CALL TPCSCA(R,RL,PR(1),P(1),CMS(1))
      CALL KTPREM(PR(1),P(1),DER(1,1),CMS(1),IERR)
      IF (IERR.GT.0) THEN
        IRET = 400+IERR
        RETURN
      ENDIF
*--
      IF ((PR(1)-V(1)).GT.( PI * R)) V(1) = V(1) + 2. * PI * R
      IF ((PR(1)-V(1)).LT.(-PI * R)) V(1) = V(1) - 2. * PI * R
*--
      CALL KTMEAS(2,V(1),CV(1),HT(1,1),IERR)
      IF (IERR.GT.0) THEN
        IRET = 500+IERR
        RETURN
      ENDIF
*--
      CALL KTFILT(P(1),C(1),IERR)
      IF (IERR.GT.0) THEN
        IRET = 600+IERR
        RETURN
      ENDIF

*--
      CALL KTFCH2(RES(1),PUL(1),CH2,IERR)
      IF (IERR.GT.0) THEN
        IRET = 700+IERR
        RETURN
      ENDIF
*--
      RL = R
      IRET = 0
      RETURN
*--
      END
      SUBROUTINE TPCSCA(R,RL,P,PL,C)
*-----------------------------------------------------------------------------
*--
*--   Calculate MS Cov. Matrix
*--
*--   Input:
*--     R ... Radius
*--     P ... Parameter Vector
*--     RL... Last radius
*--     PL... Last parameter vector
*--
*--   Output:
*--     C ... MS cov matrix (to be added)
*--
*--   Dietrich Liko
*--   Repaired by A. C. Saulys
*--
*-----------------------------------------------------------------------------
*--
*--   assume pion mass (GeV)
*--
***      PARAMETER (PMASS = 0.139)

        REAL PMASS
        COMMON/HMASS/PMASS

*** radius - momentum conversion factor

        DOUBLE PRECISION CONSB
        COMMON /VXMAGF/CONSB

#include "math_constants.inc"
#include "phys_constants.inc"

*--
      REAL P(5),C(15),PL(5),PMS(5)
*--
      CALL VZERO(C(1),15)
*--
CCC      IF(X.LE.0.0)RETURN

      COSL = SQRT(1./(1.+P(3)**2))

      BETM2 = 1. + (PMASS * P(5)/COSL)**2
      rad = 1.0/(CONSB*abs(p(5)))
      dpsi = p(4)-pl(4)

C -- check for rotation direction
CCC      if(p(5)*CONSB.lt.0)then
CCC        if(dpsi.lt.0)then
CCC          dpsi = (C_2PI+dpsi)
CCC        endif
CCC      else
CCC        if(dpsi.gt.0)then
CCC           dpsi = -(C_2PI-dpsi)
CCC        endif
CCC      endif
C -- put dpsi in 0-2pi range
CCC      dpsi = amod(dpsi,real(C_2PI))
CCC      pathl = abs(dpsi*rad/cosl)
      DXSQ = (R*cos(p(1)/R)-RL*cos(pl(1)/RL))**2
      DYSQ = (R*sin(p(1)/R)-RL*sin(pl(1)/RL))**2
      DZSQ = (p(2)-pl(2))**2

*-- fraction of radiation length for P10
      XLTRUE = SQRT(DXSQ+DYSQ+DZSQ)/11600.
CCC      XLTRUE = pathl/11600.

      IF (XLTRUE.GT.1E-8) THEN
         CORR = 1. + 0.11111111 * ALOG10(XLTRUE)
      ELSE
         CORR = 0.0
      ENDIF

*-- theta rms squared in plane
      THPLSQ = 1.9881E-4 * (P(5)*CORR)**2 * XLTRUE * BETM2
      C(1)  = 0.3333*(R-RL)**2*thplsq ! r*phi term 

      C(6)  = 0.3333*(DXSQ+DYSQ)*thplsq ! z term
CCC      C(10) = thplsq ! atan(tanl) term
      C(10) = thplsq/cosl**2 ! tanl term
      C(13) = thplsq ! psi term
*-- track radius
CCC      tanl = tan(C_PI_2-p(3))
CCC      tanl = p(3)
CCC      cosl = sqrt(1./(1.+tanl**2))
      
      drorsq = 0.3333*thplsq*dpsi**2 
      C(15) = drorsq*p(5)**2 ! q/pt term

*--
      RETURN
*--
      END















