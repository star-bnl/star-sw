      integer FUNCTION svm_am(svtin_h,svtin,tpcin_h,tpcin,
     >     vvctrl_h,vvctrl,evtin_h,evtin)
      implicit none
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                     module:   svm_am
c            Vector - Vector matching code for svt - tpc tracks
c                            version 3
c
c               Standardized svt and tpc helix parameters (lbl form)
c               with error covariance matrix.
c
c
c  Description:
c Package svm has been developed to enable matching between tracks identified
c in the svt with those identified in the tpc.  The method involves a two-level
c algorithm.  In the first level all svt and tpc tracks (assumed to be helices)
c are extrapolated into the intermediate region to some fixed matching radius,
c 40 cm giving the best results.  Track position in z and phi, momentum 
c magnitude and direction, as well as the uncertainties due to multiple 
c coulomb scattering  and errors in the track parameters, are computed
c for each track at the matching radius.
c  For each svt track, the best tpc track candidate is determined based on
c chi-square.  Similarly, for each tpc track, the best (based on chi-square)
c svt  track candidate is found.  If these agree then the svt and tpc track 
c pair is assigned as a match and removed from the pool of tracks.
c
c      For the remaining tracks a second level matching algorithm is applied.
c For this method three of the helix parameters (x-center-of-circle,
c y-center-of-circle, and pitch) are compared.  For each remaining svt track
c the tpc tracks whose parameters lie within certain cuts are selected based
c on chi-square for these three parameters.  Best chi-square constitutes a
c match.
c
c     Author:     Lanny Ray
c                 Dept. of Physics
c                 Univ. of Texas at Austin
c                 Austin, Texas  78712
c                 (512) 471-6107
c                 ray@utaphy.ph.utexas.edu
c
c Status:    The module has been tested in tas and verified to run with full
c            track input in the two input track tables.  It has also been
c            tested in TAS with tracking output from stk and tpt.
c
c  Dates:      Module first installed in tas:  nov. 17, 1993
c              Modified and re-installed in tas: dec. 16, 1993 where;
c                  the tpc track parameter tptrack.invp was changed to
c                  1.0/p(transverse).
c              Modified and re-installed in tas: apr. 13, 1994 where;
c               a. 1.0/p(transverse) for svt tracks included using parameter
c                  name svt_track.invpt
c               b. reverse magnetic field allowed
c               c. error in track extrapolation routine corrected
c               d. inward extrapolation of tpc tracks from arbitrary point
c                  in tpc allowed.
c               e. all track angle parameters are in degrees.
c
c              Modified and re-installed in tas: June 6, 1994 where:
c               a. Loop controls included to reduce bad matching
c                  assignments.
c               b. Matching table evt_match initialized to zero and
c                  all new variables filled.
c               c. Charge sign check included; low tpc pt cut-off incl., 
c                  chi-square cut-off included.
c               d. tpc tracking flag checked (tptrack.flag .gt 0 to use trk.)
c
c              Modified and re-installed in TAS: April 13, 1995 where:
c               a. For matched TPC tracks the svt_track.id and the chi-square
c                  for the next best SVT match are found and recorded in
c                  table evt_match.  Two new variables were added to the
c                  match table (evt_match), these being idsvt_2 and quality_2
c                  (see following for further explanation).  This information
c                  was added for the case where two close tracks are separated
c                  in the SVT but merged in the TPC.
c
c
c   Arguments:
c
c                 svtin_h   header to table     svt_track
c                 svtin    rows of table       svt_track
c                 tpcin_h   header to table     tptrack
c                 tpcin    rows of table       tptrack
c                 vvctrl_h  header to table     svm_ctrl
c                 vvctrl   rows of table       svm_ctrl
c                 evtin_h   header to table     evt_match
c                 evtin    rows of table       evt_match
c
c TAS tables:
c       the module reads the svt track table:  svt_track in library stk
c       the module reads the tpc track table:  tptrack   in library tpt
c       the module reads the static control table: svm_ctrl in library svm
c       the module outputs to table:        evt_match in library svm
c
c   Variables:
c    The variables for the two svm tables are (with additional
c    comments):
c
c !--------------------------------------------------------------------------
c !   table: svm_ctrl
c ! ktrl1 = control switch 1: allows errors in helix
c         = 0 errors not included
c         = 1 errors included
c            integer*4 ktrl1
c
c ! ktrl2 = 0 to compute error for chi-sq as e1*e2 (default).
c         = 1 to compute error for chi-sq as (e1**2 + e2**2).  
c
c ! ktrl3 = control switch 3:
c         = 0 for no level 2 matching
c         = 2 for level 2 matching
c            integer*4 ktrl3
c
c ! ktrl4 = control switch 4: selects mcs formula
c         = 0 from rev. mod. phys. 56, s50 (1984).
c         = 1 from particle data handbook.
c            integer*4 ktrl4
c
c ! ktrl5 .ne. 0 to limit the number of loops in the first bi-directional
c                level matching, i.e. to not require that all svt
c                tracks have at least one tpc track match candidate.
c         if .ne. 0 ktrl5 is the max. no. of loops thru first level
c                matcher.
c
c ! arfinc = search area increase factor (typically = 4.0)
c            real*4  arfinc
c
c ! bmag = magnetic field (t)
c          if .gt. 0.0 is assumed along the +z-axis
c          if .lt. 0.0 is assumed along the -z-axis
c          bmag must not be 0.0, no field calculations require
c               changes in the code.
c            real*4  bmag
c
c ! efaca =  use this to change the relative weight on the directional
c                       part of chi-square  compared to the phi,z
c                       position part. Does not affect search area.
c            real*4  efaca
c
c ! efacp =  invpt cut-off for matched tpc tracks 
c            real*4  efacp
c
c ! efacz =  minimum chi-square cut-off
c            real*4  efacz
c
c ! pmerr = initial size of search area: momentum (frac. of total p)
c            real*4  pmerr
c
c ! rifc = radial position of tpc ifc (cm)
c            real*4  rifc
c
c ! rlgas = radiation length of gas (cm)
c            real*4  rlgas
c
c ! rlifc = radiation length of tpc ifc (cm)
c            real*4  rlifc
c
c ! rlsdd3 = rad. length for sdd outermost layer (cm)
c            real*4  rlsdd3
c
c ! rltube = rad. length of svt support tube (cm)
c            real*4  rltube
c
c ! rmatch = matching radius from beam axis (cm)
c            real*4  rmatch
c
c ! rming = inner radial position of gas vessel (cm)
c            real*4  rming
c
c ! rsdd3 = radial position of sdd outer layer (cm)
c            real*4  rsdd3
c
c ! rtube = radial position of svt support tube (cm)
c            real*4  rtube
c
c ! slpcut = dip angle cut-off, 2nd level matching
c            real*4  slpcut
c
c ! tgas = thickness of gas vessel region (cm)
c            real*4  tgas
c
c ! tifc = thickness of tpc inner field cage (cm)
c            real*4  tifc
c
c ! tsdd3 = thickness of sdd outermost layer (cm)
c            real*4  tsdd3
c
c ! ttube = thickness of svt support tube (cm)
c            real*4  ttube
c
c ! xcut = x cut-off for 2nd level matching (cm)
c            real*4  xcut
c
c ! ycut = y cut-off for 2nd level matching (cm)
c            real*4  ycut
c
c ! ttpc_gas = thickness of tpc gas region (cm)
c            real*4  ttpc_gas
c
c ! rtpc_gas = innermost radius of tpc gas region (cm)
c            real*4  rtpc_gas
c
c ! rltpc_gas = radiation length of tpc gas (cm)
c            real*4  rltpc_gas
c
c !--------------------------------------------------------------------------
c
c
c
c
c !----------------------------------------------------------------------
c !   Table: evt_match
c ! id = id of matched pair of svt - tpc tracks
c           INTEGER*4 id
c ! idsvt = id no. of svt track from svt_track table
c           INTEGER*4 idsvt
c ! idsvt_2 = id no. of next best svt track match for the given tpc track
c             from svt_track table
c           INTEGER*4 idsvt_2
c ! idtpc = id no. of tpc track from tptrack table
c           INTEGER*4 idtpc
c ! loop1 = matching in bi-direc., number of tries
c           INTEGER*4 loop1
c ! loop2 = matching done in second pass
c           INTEGER*4 loop2
c ! mc_check = mc evaluation:(0)not used,(1)cor,-1wrong
c           INTEGER*4 mc_check
c ! pid = Geant PID # of tpc seg. of matched trks.
c           INTEGER*4 pid
c ! quality = int(1000.0*chi-square)
c           INTEGER*4 quality
c ! quality_2 = int(1000.0*chi-square of next best svt track match)
c           INTEGER*4 quality_2
c ! eta = pseudorapidity of tpc seg. at 1st sp.pt.
c           REAL*4  eta
c ! invpt = 1/pt of tpc segment of matched tracks
c           REAL*4  invpt
c
c !--------------------------------------------------------------------------
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc
ccc  staf include statements
ccc
#include "svm_am.inc"

ccc
ccc  variable declarations
ccc
      real   ang,angerr,arfac,chisq,chimax,chimin,chisav,chisav_2
      real   chitmp,chi5,cosphi,delpj
      real   dirsvt,dirtpc,dirvec,dirdif,dsp,eangsv,eangtp
      real   errsvt,errtpc,eta,fac1,phidf2,phidif,phier1
      real   phier2,phierr,pi,pmass,pmdif,pthgas,pthifc,pthsdd
      real   partal,pcov,persvt,pertpc,phisvt,phitpc,pin
      real   pmsvt,pmtpc,poserr,pout,psvt,ptmp,ptpc,pthtpc_gas
      real   pthtot,pthtub,td,tmcs,rad_tpc,rad,slpdif,themcs
      real   xdif,ydif,zdif,zerr,zerr1,zerr2,zeta,zp
      real   zersvt,zertpc,zint,zsvt,ztpc

      integer  i,ibest,iercod,ichk,idsvt,idtpc,iflag,iskip
      integer  isvt,isvt1,isvt2,jchk,j,jbest,jflag,jskip,jtpc
      integer  jtpc1,j5,jtmp,k,kp,ksvt,ksw,ktr,ktpc,ktrset
      integer  l,loop_count,loop_1,loop_2,m,nbad,nmat
      integer  nunasg,nsvt,ntpc,pidsvt,pidtpc
      integer NO_TKS_MAX

      parameter (NO_TKS_MAX=10000)
      
      dimension jtmp(NO_TKS_MAX),j5(5),chitmp(NO_TKS_MAX),chi5(5)
      dimension idsvt(NO_TKS_MAX),psvt(6,NO_TKS_MAX)
      dimension pidsvt(NO_TKS_MAX),pmsvt(NO_TKS_MAX)
      dimension zersvt(NO_TKS_MAX),persvt(NO_TKS_MAX)
      dimension zsvt(NO_TKS_MAX),phisvt(NO_TKS_MAX)
      dimension dirsvt(3,NO_TKS_MAX),eangsv(NO_TKS_MAX)
      dimension idtpc(NO_TKS_MAX),ptpc(6,NO_TKS_MAX)
      dimension pidtpc(NO_TKS_MAX),pmtpc(NO_TKS_MAX),zertpc(NO_TKS_MAX)
      dimension pertpc(NO_TKS_MAX),ztpc(NO_TKS_MAX),phitpc(NO_TKS_MAX)
      dimension dirtpc(3,NO_TKS_MAX),eangtp(NO_TKS_MAX)
      dimension ichk(NO_TKS_MAX),jchk(NO_TKS_MAX)
      dimension ang(3),td(3),cosphi(3),zint(3),tmcs(3),
     1 chisq(NO_TKS_MAX,5),jtpc(NO_TKS_MAX,5),ntpc(NO_TKS_MAX),
     2 iflag(NO_TKS_MAX),jflag(NO_TKS_MAX),nsvt(NO_TKS_MAX),
     3 isvt(NO_TKS_MAX,5),jtpc1(NO_TKS_MAX),isvt1(NO_TKS_MAX),
     4 iskip(NO_TKS_MAX),jskip(NO_TKS_MAX),pin(6),
     5 pout(7),pcov(6,6),ptmp(6),dirvec(3),partal(3,6),
     6 poserr(3),nbad(2),nmat(2),nunasg(2),
     7 loop_1(NO_TKS_MAX),loop_2(NO_TKS_MAX),chisav(NO_TKS_MAX),
     8 isvt2(NO_TKS_MAX),chisav_2(NO_TKS_MAX)

c
      print *,'Begin matching'
c
ccc
ccc   set parameters:
ccc
      pi = 3.141592654
      rad = 180.00/pi
      fac1 = 0.0029980
      rad_tpc = rad
ccc
ccc   initialize arrays to zero:
ccc
      do 201 i = 1,NO_TKS_MAX
      idsvt(i) = 0
      pidsvt(i) = 0
      pmsvt(i) = 0.00
      zersvt(i) = 0.00
      persvt(i) = 0.00
      zsvt(i) = 0.00
      phisvt(i) = 0.00
      eangsv(i) = 0.00
c
      idtpc(i) = 0
      pidtpc(i) = 0
      pmtpc(i) = 0.00
      zertpc(i) = 0.00
      pertpc(i) = 0.00
      ztpc(i) = 0.00
      phitpc(i) = 0.00
      eangtp(i) = 0.00
c
      chitmp(i) = 0.0
      jtmp(i) = 0
      chisav(i) = 0.0
      chisav_2(i) = 0.0
c
      do 202 j = 1,3
      dirsvt(j,i) = 0.00
202   dirtpc(j,i) = 0.00
      do 203 j = 1,6
      psvt(j,i) = 0.00
203   ptpc(j,i) = 0.00
c
      do 204 j = 1,5
      chisq(i,j) = 0.00
      jtpc(i,j) = 0
204   isvt(i,j) = 0
c
      ntpc(i) = 0
      iflag(i) = 0
      jflag(i) = 0
      loop_1(i) = 0
      loop_2(i) = 0
      nsvt(i) = 0
      jtpc1(i) = 0
      isvt1(i) = 0
      isvt2(i) = 0
      jchk(i) = 0
      ichk(i) = 0
      iskip(i) = 0
      jskip(i) = 0
201   continue
      do 205 i = 1,6
      pin(i) = 0.00
      pout(i) = 0.00
      ptmp(i) = 0.00
      do 206 j = 1,6
206   pcov(i,j) = 0.00
      do 207 k = 1,3
      dirvec(k) = 0.00
      poserr(k) = 0.00
207   partal(k,i) = 0.00
205   continue
      pout(7) = 0.0


ccc
ccc   Set evt_match.nok to zero and all rows to zero
ccc
      do i = 1,evtin_h.maxlen
      evtin(i).id = 0
      evtin(i).idsvt = 0
      evtin(i).idsvt_2 = 0
      evtin(i).idtpc = 0
      evtin(i).loop1 = 0
      evtin(i).loop2 = 0
      evtin(i).mc_check = 0
      evtin(i).pid   = 0
      evtin(i).quality = 0
      evtin(i).quality_2 = 0
      evtin(i).eta   = 0.0
      evtin(i).invpt = 0.0
      end do
      evtin_h.nok = 0

      ksvt = 0
      do 70 i = 1,svtin_h.nok
      if(svtin(i).invpt .gt. 0.0001 .and. svtin(i).invpt
     &   .lt. 1000.0) go to 771
      if(svtin(i).invpt .gt. -1000.0 .and. svtin(i).invpt
     &   .lt. -0.0001) go to 771
      go to 70
771   continue
      ksvt = ksvt + 1
      if(ksvt .gt. NO_TKS_MAX) then
         svm_am = STAFCV_BAD
         return
      end if
      pin(1) = svtin(i).r0
      pin(2) = svtin(i).phi0/rad
      pin(3) = svtin(i).z0
      pin(4) = svtin(i).psi/rad
      pin(5) = svtin(i).tanl
      pin(6) = abs(svtin(i).invpt)/sqrt(1.0 + pin(5)*pin(5))
ccc
ccc   For the svt tracking table invpt = sgn(charge)/p-transverse
ccc   as of march 3, 1994
ccc   
      do 78 l = 1,6
      pcov(4,l) = 0.0
      pcov(l,4) = 0.0
78    continue
      pcov(1,1) = svtin(i).cov(6)
      pcov(1,2) = svtin(i).cov(5)/rad
      pcov(1,3) = svtin(i).cov(13)
      pcov(1,5) = svtin(i).cov(9)
      pcov(1,6) = svtin(i).cov(4)
      pcov(2,1) = svtin(i).cov(5)/rad
      pcov(2,2) = svtin(i).cov(3)/(rad*rad)
      pcov(2,3) = svtin(i).cov(12)/rad
      pcov(2,5) = svtin(i).cov(8)/rad
      pcov(2,6) = svtin(i).cov(2)/rad
      pcov(3,1) = svtin(i).cov(13)
      pcov(3,2) = svtin(i).cov(12)/rad
      pcov(3,3) = svtin(i).cov(15)
      pcov(3,5) = svtin(i).cov(14)
      pcov(3,6) = svtin(i).cov(11)
      pcov(5,1) = svtin(i).cov(9)
      pcov(5,2) = svtin(i).cov(8)/rad
      pcov(5,3) = svtin(i).cov(14)
      pcov(5,5) = svtin(i).cov(10)
      pcov(5,6) = svtin(i).cov(7)
      pcov(6,1) = svtin(i).cov(4)
      pcov(6,2) = svtin(i).cov(2)/rad
      pcov(6,3) = svtin(i).cov(11)
      pcov(6,5) = svtin(i).cov(7)
      pcov(6,6) = svtin(i).cov(1)
      idsvt(ksvt) = svtin(i).id
      if(svtin(i).invpt .gt. 0.0) pidsvt(ksvt) = 8
      if(svtin(i).invpt .lt. 0.0) pidsvt(ksvt) = 9
      pmsvt(ksvt) = 1.00/pin(6)
      call pid(pidsvt(ksvt),pmass,zp)
      call cnvrt(pin,pout,zp,vvctrl(1).bmag,fac1,pi)
      do 71 k = 1,6
71    psvt(k,ksvt) = pout(k)
      call ipt(pout,vvctrl(1).rsdd3,zp,vvctrl(1).bmag,ang(1),td(1),
     1   cosphi(1),zint(1),iercod,dirvec)
      call ipt(pout,vvctrl(1).rtube,zp,vvctrl(1).bmag,ang(2),td(2)
     1     ,cosphi(2),zint(2),iercod,dirvec)
      call ipt(pout,vvctrl(1).rmatch,zp,vvctrl(1).bmag,ang(3),td(3),
     1     cosphi(3),zint(3),iercod,dirvec)
      if(iercod.eq.1) then
      iflag(ksvt) = -1
      go to 70
      end if
      pthsdd = vvctrl(1).tsdd3/abs(cosphi(1))
      pthtub = vvctrl(1).ttube/abs(cosphi(2))
      pthgas = (psvt(4,ksvt)*(td(2)-td(3)))**2+(zint(2)-zint(3))**2
      pthgas = sqrt(pthgas)
      pthtot = (psvt(4,ksvt)*(td(1)-td(3)))**2 + (zint(1)-zint(3))**2
      pthtot = sqrt(pthtot)
      tmcs(1) = themcs(pmass,zp,pmsvt(ksvt),pthsdd,vvctrl(1).rlsdd3,
     >     vvctrl(1).ktrl4)
      tmcs(2) = themcs(pmass,zp,pmsvt(ksvt),pthtub,vvctrl(1).rltube,
     >     vvctrl(1).ktrl4)
      tmcs(3) = themcs(pmass,zp,pmsvt(ksvt),pthgas,vvctrl(1).rlgas,
     >     vvctrl(1).ktrl4)
      errsvt = (tmcs(1)*pthtot)**2
      errsvt = errsvt + (tmcs(2)*pthgas)**2
      errsvt = errsvt + ((tmcs(3)*pthgas)**2)/3.00
      errsvt = sqrt(errsvt)
      eangsv(ksvt) = tmcs(1)**2 + tmcs(2)**2 + tmcs(3)**2
      eangsv(ksvt) = sqrt(eangsv(ksvt))*sqrt(2.00)
ccc
ccc   Compute position error region on imaginary cylinder at matching
ccc   radius.
ccc
      dsp = -dirvec(1)*sin(ang(3)) + dirvec(2)*cos(ang(3))
ccc
ccc  Save momentum direction unit vectors at matching point:
ccc
      dirsvt(1,ksvt) = dirvec(1)
      dirsvt(2,ksvt) = dirvec(2)
      dirsvt(3,ksvt) = dirvec(3)
      eta = atan(abs(dirvec(3)/dsp))
      zerr1 = abs(errsvt*cos(eta))
      zerr2 = abs(errsvt*sin(eta)/cosphi(3))
      zersvt(ksvt) = max(zerr1,zerr2)
      phier1 = abs(errsvt*sin(eta))
      phier2 = abs(errsvt*cos(eta)/cosphi(3))
      persvt(ksvt) = max(phier1,phier2)
      persvt(ksvt) = persvt(ksvt)/vvctrl(1).rmatch
ccc
ccc    Add z, phi position and zeta direction errors due to helix parameter
ccc    errors.  compute partial derivatives for z-intercept, phi-intercept,
ccc    and angle between perturbed and unperturbed vector directions at the
ccc    intercept point at r-match.
ccc
      if(vvctrl(1).ktrl1.eq.0) go to 77
      do 73 j = 1,6
      ptmp(1) = svtin(i).r0
      ptmp(2) = svtin(i).phi0/rad
      ptmp(3) = svtin(i).z0
      ptmp(4) = svtin(i).psi/rad
      ptmp(5) = svtin(i).tanl
      ptmp(6) = abs(svtin(i).invpt)
      if(ptmp(j) .eq. 0.0) then
         ptmp(j) = 0.01
         delpj   = 0.01
      else
         delpj   = 0.01*ptmp(j)
         ptmp(j) = 1.01*ptmp(j)
      end if
      ptmp(6) = ptmp(6)/sqrt(1.0 + ptmp(5)*ptmp(5))
      call cnvrt(ptmp,pout,zp,vvctrl(1).bmag,fac1,pi)
      call ipt(pout,vvctrl(1).rmatch,zp,vvctrl(1).bmag,ang(1),td(1),
     1     cosphi(1),zint(1),iercod,dirvec)
      zeta = dirsvt(1,ksvt)*dirvec(1) + dirsvt(2,ksvt)*dirvec(2)
     1     + dirsvt(3,ksvt)*dirvec(3)
      if(zeta.gt.1.00000) then
      zeta = 0.00
      else
      zeta = acos(zeta)
      end if
      partal(1,j) = (ang(1)-ang(3))/delpj
      partal(2,j) = (zint(1)-zint(3))/delpj
      partal(3,j) = zeta/delpj
73    continue
      do 75 m = 1,3
      poserr(m) = 0.00
      do 76 k = 1,6
      do 76 kp = 1,6
      poserr(m) = poserr(m) + partal(m,k)*partal(m,kp)*pcov(k,kp)
76    continue
75    continue
      zersvt(ksvt) = sqrt(zersvt(ksvt)*zersvt(ksvt) + poserr(2))
      persvt(ksvt) = sqrt(persvt(ksvt)*persvt(ksvt) + poserr(1))
      eangsv(ksvt) = sqrt(eangsv(ksvt)*eangsv(ksvt) + poserr(3))
77    continue
      zsvt(ksvt) = zint(3)
      phisvt(ksvt) = ang(3)
ccc
ccc   For the ksvt-th svt track the (z,phi) intercept points with an imag.
ccc   cylinder at the matching radius (vvctrl(1).rmatch) are stored in 
ccc   zsvt(ksvt)
ccc   and phisvt(ksvt).  The (z,phi) position errors are in zersvt(ksvt) and
ccc   persvt(ksvt).   The svt track momentum direction unit vector at the
ccc   point of intercept with the matching cylinder are in dirsvt(1-3,ksvt)
ccc   where indices 1-3 correspond to x,y,z .  The (space) error in
ccc   direction angle is in eangsv(ksvt).
ccc
70    continue
      write(6,401) ksvt
401   format(5x,'number of svt tracks(ksvt) = ', i8)
      write(6,402)
402   format(5x,'svt track set-up loop finished')
ccc
ccc   Read tpc helix parameters and
ccc   begin setup loop for all tpc tracks:
ccc
      ktpc = 0
      do 80 i = 1,tpcin_h.nok
*** VERY TEMPORARY FIX
      tpcin(i).id_globtrk = 0
*****
      if(tpcin(i).flag .lt. 0) go to 80
      if(tpcin(i).invp .eq. 0.0) go to 80
      ktpc = ktpc + 1
      if(ktpc .gt. NO_TKS_MAX) then
         svm_am = STAFCV_BAD
         return
      end if
      pin(1) = tpcin(i).r0
      pin(2) = tpcin(i).phi0/rad_tpc
      pin(3) = tpcin(i).z0
      pin(4) = tpcin(i).psi/rad_tpc
      pin(5) = tpcin(i).tanl
      pin(6) = tpcin(i).invp/sqrt(1.0 + pin(5)*pin(5))
cc
cc   As of april 11, 1994 tpc track table tptrack defines (1.0/p-transverse)
cc   to be tptrack.invp,  angle parameters are assumed to be in degrees.
cc
      do 88 l = 1,6
      pcov(4,l) = 0.0
      pcov(l,4) = 0.0
88    continue
      pcov(1,1) = tpcin(i).cov(6)
      pcov(1,2) = tpcin(i).cov(5)/rad_tpc
      pcov(1,3) = tpcin(i).cov(13)
      pcov(1,5) = tpcin(i).cov(9)
      pcov(1,6) = tpcin(i).cov(4)
      pcov(2,1) = tpcin(i).cov(5)/rad_tpc
      pcov(2,2) = tpcin(i).cov(3)/(rad_tpc*rad_tpc)
      pcov(2,3) = tpcin(i).cov(12)/rad_tpc
      pcov(2,5) = tpcin(i).cov(8)/rad_tpc
      pcov(2,6) = tpcin(i).cov(2)/rad_tpc
      pcov(3,1) = tpcin(i).cov(13)
      pcov(3,2) = tpcin(i).cov(12)/rad_tpc
      pcov(3,3) = tpcin(i).cov(15)
      pcov(3,5) = tpcin(i).cov(14)
      pcov(3,6) = tpcin(i).cov(11)
      pcov(5,1) = tpcin(i).cov(9)
      pcov(5,2) = tpcin(i).cov(8)/rad_tpc
      pcov(5,3) = tpcin(i).cov(14)
      pcov(5,5) = tpcin(i).cov(10)
      pcov(5,6) = tpcin(i).cov(7)
      pcov(6,1) = tpcin(i).cov(4)
      pcov(6,2) = tpcin(i).cov(2)/rad_tpc
      pcov(6,3) = tpcin(i).cov(11)
      pcov(6,5) = tpcin(i).cov(7)
      pcov(6,6) = tpcin(i).cov(1)
      idtpc(ktpc) = tpcin(i).id
      if(tpcin(i).q .gt. 0) pidtpc(ktpc) = 8
      if(tpcin(i).q .lt. 0) pidtpc(ktpc) = 9
      pmtpc(ktpc) = 1.00/pin(6)
      call pid(pidtpc(ktpc),pmass,zp)
      call cnvrt(pin,pout,zp,vvctrl(1).bmag,fac1,pi)
      do 81 k = 1,6
81    ptpc(k,ktpc) = pout(k)
      call ipt(pout,tpcin(i).r0 - 0.1,zp,vvctrl(1).bmag,ang(1),
     1     td(1),cosphi(1),zint(1),iercod,dirvec)
      call ipt(pout,vvctrl(1).rifc,zp,vvctrl(1).bmag,ang(2),td(2)
     1     ,cosphi(2),zint(2),iercod,dirvec)
      call ipt(pout,vvctrl(1).rmatch,zp,vvctrl(1).bmag,ang(3),td(3),
     1     cosphi(3),zint(3),iercod,dirvec)
      if(iercod.eq.1) then
      jflag(ktpc) = -1
      go to 80
      end if
      if(tpcin(i).invp .gt. vvctrl(1).efacp) then
      jflag(ktpc) = -1
      go to 80
      end if
      pthtpc_gas = (ptpc(4,ktpc)*(td(1) - td(2)))**2
     1             + (zint(1) - zint(2))**2
      pthtpc_gas = sqrt(pthtpc_gas)
      pthifc = vvctrl(1).tifc/abs(cosphi(2))
      pthgas = (ptpc(4,ktpc)*(td(2)-td(3)))**2 + (zint(2)-zint(3))**2
      pthgas = sqrt(pthgas)
      tmcs(1) = themcs(pmass,zp,pmtpc(ktpc),pthtpc_gas,
     >     vvctrl(1).rltpc_gas,vvctrl(1).ktrl4)
      tmcs(2) = themcs(pmass,zp,pmtpc(ktpc),pthifc,vvctrl(1).rlifc,
     >     vvctrl(1).ktrl4)
      tmcs(3) = themcs(pmass,zp,pmtpc(ktpc),pthgas,vvctrl(1).rlgas,
     >     vvctrl(1).ktrl4)
      errtpc = ((tmcs(1)*pthtpc_gas)**2)/3.0
      errtpc = errtpc + (tmcs(1)*pthgas)**2
      errtpc = errtpc + (tmcs(2)*pthgas)**2
      errtpc = errtpc + ((tmcs(3)*pthgas)**2)/3.0
      errtpc = sqrt(errtpc)
      eangtp(ktpc) = tmcs(1)**2 + tmcs(2)**2  + tmcs(3)**2
      eangtp(ktpc) = sqrt(eangtp(ktpc))*sqrt(2.00)
ccc
ccc   Compute position error region on imaginary cylinder at matching
ccc   radius.
ccc
      dsp = -dirvec(1)*sin(ang(3)) + dirvec(2)*cos(ang(3))
ccc
ccc  Save momentum direction unit vectors at matching point:
ccc
      dirtpc(1,ktpc) = dirvec(1)
      dirtpc(2,ktpc) = dirvec(2)
      dirtpc(3,ktpc) = dirvec(3)
      eta = atan(abs(dirvec(3)/dsp))
      zerr1 = abs(errtpc*cos(eta))
      zerr2 = abs(errtpc*sin(eta)/cosphi(3))
      zertpc(ktpc) = max(zerr1,zerr2)
      phier1 = abs(errtpc*sin(eta))
      phier2 = abs(errtpc*cos(eta)/cosphi(3))
      pertpc(ktpc) = max(phier1,phier2)
      pertpc(ktpc) = pertpc(ktpc)/vvctrl(1).rmatch
ccc
ccc    Add z, phi position and zeta direction errors due to helix parameter
ccc    errors.  Compute partial derivatives for z-intercept, phi-intercept,
ccc    and angle between perturbed and unperturbed vector directions at the
ccc    intercept point at r-match.
ccc
      if(vvctrl(1).ktrl1.eq.0) go to 870
      do 83 j = 1,6
      ptmp(1) = tpcin(i).r0
      ptmp(2) = tpcin(i).phi0/rad_tpc
      ptmp(3) = tpcin(i).z0
      ptmp(4) = tpcin(i).psi/rad_tpc
      ptmp(5) = tpcin(i).tanl
      ptmp(6) = tpcin(i).invp
      if(ptmp(j) .eq. 0.0) then
         ptmp(j) = 0.01
         delpj   = 0.01
      else
         delpj   = 0.01*ptmp(j)
         ptmp(j) = 1.01*ptmp(j)
      end if
      ptmp(6) = ptmp(6)/sqrt(1.0 + ptmp(5)*ptmp(5))
      call cnvrt(ptmp,pout,zp,vvctrl(1).bmag,fac1,pi)
      call ipt(pout,vvctrl(1).rmatch,zp,vvctrl(1).bmag,ang(1),td(1),
     1     cosphi(1),zint(1),iercod,dirvec)
      zeta = dirtpc(1,ktpc)*dirvec(1) + dirtpc(2,ktpc)*dirvec(2)
     1     + dirtpc(3,ktpc)*dirvec(3)
      if(zeta.gt.1.00000) then
      zeta = 0.00
      else
      zeta = acos(zeta)
      end if
      partal(1,j) = (ang(1)-ang(3))/delpj
      partal(2,j) = (zint(1)-zint(3))/delpj
      partal(3,j) = zeta/delpj
83    continue
      do 85 m = 1,3
      poserr(m) = 0.00
      do 86 k = 1,6
      do 86 kp = 1,6
      poserr(m) = poserr(m) + partal(m,k)*partal(m,kp)*pcov(k,kp)
86    continue
85    continue
      zertpc(ktpc) = sqrt(zertpc(ktpc)*zertpc(ktpc) + poserr(2))
      pertpc(ktpc) = sqrt(pertpc(ktpc)*pertpc(ktpc) + poserr(1))
      eangtp(ktpc) = sqrt(eangtp(ktpc)*eangtp(ktpc) + poserr(3))
870   continue
      ztpc(ktpc) = zint(3)
      phitpc(ktpc) = ang(3)
ccc
ccc   For the ktpc-th tpc track the (z,phi) intercept points with an imag.
ccc   cylinder at the matching radius (vvctrl(1).rmatch)
ccc    are stored in ztpc(ktpc)
ccc   and phitpc(ktpc).  The (z,phi) position errors are in zertpc(ktpc) and
ccc   pertpc(ktpc).   The tpc track momentum direction unit vector at the
ccc   point of intercept with the matching cylinder are in dirtpc(1-3,ktpc)
ccc   where indices 1-3 correspond to x,y,z .  The (space) error in
ccc   direction angle is in eangtp(ktpc).
ccc
80    continue

      write(6,403) ktpc
403   format(5x,'number of tpc tracks (ktpc) = ', i8)
      write(6,404)
404   format(5x,'tpc track set-up loop finished')
ccc
ccc   do initial track matching:
ccc
      loop_count = 0
      arfac = 1.00
90    continue
      loop_count = loop_count + 1
      do 91 i = 1,ksvt
      if(iflag(i).eq.1) go to 91
      if(iflag(i).eq.-1) go to 91
      ktr = 0
      do 92 j = 1,ktpc
      if(jflag(j).eq.-1) go to 92
ccc
ccc  Check the consistency of the signs of the charges for the svt
ccc  and tpc tracks.
ccc
      if(pidsvt(i) .ne. pidtpc(j)) go to 92
      phidif = abs(phisvt(i) - phitpc(j))
      phidf2 = abs(2.00*pi - phidif)
      phidif = min(phidif,phidf2)
      phierr = persvt(i) + pertpc(j)
      if(phidif.gt.(phierr*arfac)) go to 92
      zdif   = abs(zsvt(i) - ztpc(j))
      zerr = zersvt(i) + zertpc(j)
      if(zdif.gt.(zerr*arfac))    go to 92
      pmdif = abs(pmsvt(i) - pmtpc(j))/(0.50*(pmsvt(i) + pmtpc(j)))
      if(pmdif.gt.(vvctrl(1).pmerr*arfac)) go to 92
      dirdif = dirsvt(1,i)*dirtpc(1,j) + dirsvt(2,i)*dirtpc(2,j)
     1       + dirsvt(3,i)*dirtpc(3,j)
      if(dirdif.gt.1.00000) then
      dirdif = 0.00
      else
      dirdif = acos(dirdif)
      end if
      angerr = eangsv(i) + eangtp(j)
      if(dirdif.gt.(angerr*arfac)) go to 92
      ktr = ktr + 1
      if(vvctrl(1).ktrl2.eq.0) then
      chitmp(ktr) = zdif*zdif/(zersvt(i)*zertpc(j))
     1             + phidif*phidif/(persvt(i)*pertpc(j))
     2       + vvctrl(1).efaca*dirdif*dirdif/(eangsv(i)*eangtp(j))
     3             + (pmdif/vvctrl(1).pmerr)**2
      else if(vvctrl(1).ktrl2.eq.1) then
      chitmp(ktr) = zdif*zdif/(zersvt(i)**2 + zertpc(j)**2)
     1             + phidif*phidif/(persvt(i)**2 + pertpc(j)**2)
     2       + vvctrl(1).efaca*dirdif*dirdif/(eangsv(i)**2 
     3        + eangtp(j)**2) + (pmdif/vvctrl(1).pmerr)**2
      end if
      jtmp(ktr) = j
92    continue
      if(ktr.gt.5) then
      call fcmin(chitmp,jtmp,chi5,j5,ktr,NO_TKS_MAX)
      do 901 k = 1,5
      chisq(i,k) = chi5(k)
901   jtpc(i,k) = j5(k)
      ktr = 5
      else if(ktr.gt.0) then
      do 902 k = 1,ktr
      chisq(i,k) = chitmp(k)
902   jtpc(i,k) = jtmp(k)
      end if
      ntpc(i) = ktr
91    continue
ccc
ccc  Check for svt tracks with no tpc matches.
ccc
      ksw = 0
      do 93 i = 1,ksvt
      if(iflag(i).eq.0 .and. ntpc(i).gt.0) loop_1(i) = loop_count
      if ( iflag(i) .ne. -1) then
         if (ntpc(i).eq.0) then
            iflag(i) = 0
            ksw = 1
         else
            iflag(i) = 1
         endif
      endif
93    continue
      if(vvctrl(1).ktrl5 .ne. 0 .and. loop_count .ge.
     +   vvctrl(1).ktrl5)  go to 772
      if(ksw.eq.1) arfac = arfac + vvctrl(1).arfinc
      if(ksw.eq.1) go to 90
772   continue
      write(6,405)
405   format(5x,'initial matching loop finished')
ccc
ccc  At this point there is at least one tpc track matched with each svt
ccc   track, however this depends on vvctrl(1).ktrl5 option which may 
ccc   be used to limit    
ccc   the number of loops in Level 1 and thereby cause some of the svt
ccc   tracks to have no tpc matching candidates.
      do 95 i = 1,ksvt
      if(ntpc(i).eq.0) go to 95
      do 96 ktr = 1,ntpc(i)
      jflag(jtpc(i,ktr)) = 1
96    continue
95    continue
ccc
ccc   Next, locate and count the svt matches for each tpc track:
ccc
      do 110 j = 1,ktpc
      nsvt(j) = 0
      if(jflag(j).eq.-1) go to 110
      do 111 i = 1,ksvt
      if(ntpc(i).eq.0) go to 111
      do 112 k = 1,ntpc(i)
      if(j.eq.jtpc(i,k)) then
      nsvt(j) = nsvt(j) + 1
      chitmp(nsvt(j)) = chisq(i,k)
      jtmp(nsvt(j)) = i
       end if
112   continue
111   continue
      if(nsvt(j).gt.5) then
      ktr = nsvt(j)
      call fcmin(chitmp,jtmp,chi5,j5,ktr,NO_TKS_MAX)
      nsvt(j) = 5
      do 1119 k = 1,5
1119  isvt(j,k) = j5(k)
      else if(nsvt(j).gt.0) then
      do 1118 k = 1,nsvt(j)
1118  isvt(j,k) = jtmp(k)
      end if
110   continue
      write(6,406)
406   format(5x,'do 110 loop finished')
ccc
ccc   Record best tpc match for each svt track:
ccc
      do i = 1,NO_TKS_MAX
      chisav(i) = 0.0
      end do

ccc   Find largest chi-square
      chimax = 0.00
      do 120 i = 1,ksvt
      if(ntpc(i).eq.0) go to 120
      do 1207 k = 1,ntpc(i)
      if(chisq(i,k).gt.chimax) chimax = chisq(i,k)
1207  continue
120   continue
      if(chimax.lt.3.00) chimax = 3.00
      do 121 i = 1,ksvt
      if(ntpc(i).eq.0) go to 121
      chimin = chimax
      do 122 k = 1,ntpc(i)
      if(chisq(i,k).lt.chimin) jtpc1(i) = jtpc(i,k)
      if(chisq(i,k).lt.chimin) chimin = chisq(i,k)
122   continue
      chisav(i) = chimin
121   continue
ccc
ccc   Record the best svt match for each tpc track:
ccc
      do 123 j = 1,ktpc
      if(nsvt(j).eq.0) go to 123
      chimin = chimax
      do 124 k = 1,nsvt(j)
      if(ntpc(isvt(j,k)).eq.0) go to 124
      do 125 ktr = 1,ntpc(isvt(j,k))
125   if(j.eq.jtpc(isvt(j,k),ktr)) ktrset = ktr
      if(chisq(isvt(j,k),ktrset).lt.chimin) isvt1(j) = isvt(j,k)
      if(chisq(isvt(j,k),ktrset).lt.chimin) chimin = chisq(isvt(j,k),
     1   ktrset)
124   continue
123   continue

ccc   Record the second best svt match for each tpc track:
ccc
      do 1123 j = 1,ktpc
      if(nsvt(j).le.1) go to 1123
      chimin = chimax
      do 1124 k = 1,nsvt(j)
      if(ntpc(isvt(j,k)).eq.0) go to 1124
      if(isvt(j,k) .eq. isvt1(j)) go to 1124
      do 1125 ktr = 1,ntpc(isvt(j,k))
1125  if(j.eq.jtpc(isvt(j,k),ktr)) ktrset = ktr
      if(chisq(isvt(j,k),ktrset).lt.chimin) isvt2(j) = isvt(j,k)
      if(chisq(isvt(j,k),ktrset).lt.chimin) chimin = chisq(isvt(j,k),
     1   ktrset)
1124  continue
      if(isvt2(j) .gt. 0) chisav_2(j) = chimin
1123  continue
ccc
ccc    Check track matching results:
ccc   Record level 1 bi-directional matching results
ccc           jchk = -1,  unmatched track
ccc           jchk =  1,  matched track
ccc           jchk = 86,  bad track (does not intercept matching
ccc                       radius).
ccc
      do 129 j = 1,ktpc
129   jchk(j) = 86
      nbad(1) = 0
      nmat(1) = 0
      nunasg(1) = 0
      do 130 j = 1,ktpc
      if(jflag(j).eq.-1) then
      nbad(1) = nbad(1) + 1
      go to 130
      end if
      ibest = isvt1(j)
      if(ibest.eq.0) then
      jbest = 0
      else
      jbest = jtpc1(ibest)
      end if
      if(j.eq.jbest) then
      nmat(1) = nmat(1) + 1
      jchk(j) = 1
      else
      nunasg(1) = nunasg(1) + 1
      jchk(j) = -1
      end if
130   continue
      do 131 i = 1,ksvt
131   ichk(i) = 86
      nmat(2) = 0
      nunasg(2) = 0
      nbad(2) = 0
      do 132 i = 1,ksvt
      jbest = jtpc1(i)
      if(jbest.eq.0) then
      ibest = 0
      else
      ibest = isvt1(jbest)
      end if
      if(i.eq.ibest) then
      nmat(2) = nmat(2) + 1
      ichk(i) = 1
      else
      nunasg(2) = nunasg(2) + 1
      ichk(i) = -1
      end if
132   continue
      write(6,407)
407   format(5x,'bi-directional matching finished')
ccc
ccc   Remove all bad and all matched tracks from pool.
ccc
      if(vvctrl(1).ktrl3.eq.0) go to 87
      do 134 i = 1,ksvt
134   if(ichk(i).eq.86.or.ichk(i).eq.1) iskip(i) = 1
      do 135 j = 1,ktpc
135   if(jchk(j).eq.86.or.jchk(j).eq.1) jskip(j) = 1
      if(vvctrl(1).ktrl3.eq.2) go to 300
      go to 87
ccc
ccc   Alternate secondary matching loop.  Check remaining tracks.  Compare
ccc   helix parameters xc, yc, and slope (tan-theta).
ccc
300   continue
      do 301 i = 1,ksvt
      if(iskip(i).eq.1) go to 301
      ktr = 0
      do 302 j = 1,ktpc
      if(jskip(j).eq.1) go to 302
      if(jflag(j).eq.-1) go to 302
      xdif = abs(psvt(1,i) - ptpc(1,j))
      if(xdif.gt.vvctrl(1).xcut) go to 302
      ydif = abs(psvt(2,i) - ptpc(2,j))
      if(ydif.gt.vvctrl(1).ycut) go to 302
      slpdif = abs(psvt(5,i) - ptpc(5,j))
      if(slpdif.gt.vvctrl(1).slpcut) go to 302
      ktr = ktr + 1
      chitmp(ktr) = (xdif/vvctrl(1).xcut)**2 + 
     1   (ydif/vvctrl(1).ycut)**2 + (slpdif/vvctrl(1).slpcut)**2
      jtmp(ktr) = j
302   continue
      if(ktr.gt.5) then
      call fcmin(chitmp,jtmp,chi5,j5,ktr,NO_TKS_MAX)
      do 303 k = 1,5
      chisq(i,k) = chi5(k)
303   jtpc(i,k) = j5(k)
      ktr = 5
      else if(ktr.gt.0) then
      do 304 k = 1,ktr
      chisq(i,k) = chitmp(k)
304   jtpc(i,k) = jtmp(k)
      end if
      ntpc(i) = ktr
301   continue
ccc
ccc   Record best tpc match for each svt (unmatched) track:
ccc
      do 305 i = 1,ksvt
      if(iskip(i).eq.1) go to 305
      chimin = 1000000.
      if(ntpc(i).eq.0) go to 305
      do 306 k = 1,ntpc(i)
      if(chisq(i,k).lt.chimin) jtpc1(i) = jtpc(i,k)
      if(chisq(i,k).lt.chimin) chimin   = chisq(i,k)
306   continue
      chisav(i) = chimin
      isvt1(jtpc1(i)) = i
305   continue
      do 307 i = 1,ksvt
      if(iskip(i).eq.1) go to 307
      if(ntpc(i).eq.0) go to 307
      ichk(i) = 1
      jchk(jtpc1(i)) = 1
      loop_2(i) = 1
307   continue
87    continue
      write(6,408)
408   format(5x,'secondary matching finished')
ccc
ccc    Load matching results into tas variables:
ccc
      ktr = 0
      do 150 i = 1,ksvt
      if(ichk(i).eq.1) then
      if(chisav(i) .gt. vvctrl(1).efacz) go to 150
      ktr = ktr + 1
      if(ktr .gt. evtin_h.maxlen) then
      svm_am = STAFCV_BAD
      return
      end if
      evtin(ktr).id = ktr
      evtin(ktr).idsvt = idsvt(i)
      evtin(ktr).idtpc = idtpc(jtpc1(i))
      evtin(ktr).loop1 = loop_1(i)
      evtin(ktr).loop2 = loop_2(i)
      evtin(ktr).quality = int(1000.0*chisav(i))
           if(isvt2(jtpc1(i)).gt.0) then
                evtin(ktr).idsvt_2   = idsvt(isvt2(jtpc1(i)))
                evtin(ktr).quality_2 = int(1000.0*chisav_2(jtpc1(i)))
           else
                evtin(ktr).idsvt_2   = 0
                evtin(ktr).quality_2 = 0
           end if
      end if
150   continue
      write(6,409) ktr
409   format(5x,'matching results loaded into evt_match: ',i8,
     1  ' matches found')
      evtin_h.nok = ktr
      
      svm_am = STAFCV_OK
      return
      end
      subroutine cnvrt(pin,pout,q,bmag,fac1,pi)
      implicit none
ccc
ccc   This subr: takes the LBL helix parameters: (r0, phi0, z0, psi,
ccc   tan(theta), and p**(-1) ) and converts to the Wayne State form:
ccc
ccc   q = charge
ccc   bmag = magnetic field in tesla, .gt. 0.0 if along +z-axis
ccc   fac1 = factor (see notes)
ccc   pi = 3.14...
ccc
ccc   variable and array type declarations
ccc
      real bmag,cth,fac1,pi,q,radius,sgn,t0,xctr,yctr,pin,pout
      dimension pin(6), pout(7)
      sgn = (q/abs(q))*(bmag/abs(bmag))
      cth = atan(pin(5))
      radius = cos(cth)/(abs(q)*abs(bmag)*fac1*pin(6))
      xctr = pin(1)*cos(pin(2)) + radius*cos(pin(4) - sgn*pi
     1       /2.00)
      yctr = pin(1)*sin(pin(2)) + radius*sin(pin(4) - sgn*pi
     1       /2.00)
      t0 = atan2(pin(1)*sin(pin(2)) - yctr,
     1           pin(1)*cos(pin(2)) - xctr)
      pout(1) = xctr
      pout(2) = yctr
      pout(3) = pin(3)
      pout(4) = radius
      pout(5) = pin(5)
      pout(6) = t0
      pout(7) = pin(1)
      return
      end
      subroutine ipt(p,rd,zp,bmag,ang,xid,cosphi,zint,iercod,dirvec)
      implicit none
ccc
ccc   New subr: ipt
ccc
ccc    (modulo 2.0*pi error corrected 4/11/94).
ccc        Uses the Wayne State helix parameters xctr, yctr, etc. passed
ccc        as arguments.
ccc
ccc        This takes the input helix parameters and computes the intercept
ccc        point with an imaginary cylinder at radius rd (cm).
ccc
ccc    p(i=1,6) = array of input parameters, plus p(7) is r0
ccc    rd  = radius for point of intercept.
ccc    zp = charge
ccc    bmag = magnetic field along +z-axis (is .lt. if along -z axis).
ccc    ang = intercept azimuthal angle
ccc    xid = path angle from point of origin of track to intercept point.
ccc          ( is .gt. 0.0 if going forward in time, .lt.0.0 if going
ccc          backward in time to get to intercept point.)
ccc    cosphi = cosine of angle between particle momentum vector at intercept
ccc             and normal vector to cylinder at intercept point.
ccc    zint = z value of intercept point.
ccc    iercod = 0 , track is intercepted ok.
ccc           = 1 , helix does not intercept the cylinder
ccc    dirvec(i=1,3) = (x,y,z) components of unit vector in direction of
ccc                    particle momentum at the intercept point.
ccc
ccc  variable and array type declarations
ccc
      real ang,cosphi,ddd,dsx,dsy,dsz,eq,fac1,fac2,r,rd,sl,t0
      real xc,yc,z0,zint,zp,dirvec,p
      real bmag,xint,yint,xintpp,yintpp,tint,xid,pi,angint
      integer iercod
      dimension p(7), dirvec(3)
      iercod = 0
      pi = 3.141592654
      eq = (zp/abs(zp))*(bmag/abs(bmag))
      xc = p(1)
      yc = p(2)
      z0 = p(3)
      r  = p(4)
      sl = p(5)
      t0 = p(6)
      fac1 = xc*xc + yc*yc
      if(abs(sqrt(fac1) - r).gt.rd) then
      iercod = 1
      return
      end if
      if(abs(sqrt(fac1) + r) .lt. rd) then
      iercod = 1
      return
      end if
      fac2 = (rd*rd + fac1 - r*r)/(2.00*rd*sqrt(fac1))
      ang  = atan2(yc,xc) + eq*acos(fac2)
      xint = rd*cos(ang)
      yint = rd*sin(ang)
      xint = xint - xc
      yint = yint - yc
      xintpp = cos(t0)*xint + sin(t0)*yint
      yintpp =-sin(t0)*xint + cos(t0)*yint
      tint = atan2(yintpp,xintpp)
      xid = -eq*tint
      if(p(7).le.rd) then
           if(xid.lt.0.0) xid = 2.0*pi + xid
      else
           if(xid.gt.0.0) xid = xid - 2.0*pi
      end if
      zint = z0 + r*sl*xid
      angint = -eq*xid + t0
      cosphi = eq*sin(angint - ang)/sqrt(1.0 + sl*sl)
      dsx = eq*sin(angint)
      dsy = -eq*cos(angint)
      dsz = sl
      ddd = sqrt(1.0 + sl*sl)
      dirvec(1) = dsx/ddd
      dirvec(2) = dsy/ddd
      dirvec(3) = dsz/ddd
      return
      end
      function themcs(pmass,zp,plab,al,alr,ktrl4)
      implicit none
ccc
ccc   variable and array type declarations
ccc
      real al,alr,beta,elab,fac1,fac2,fac3,gam,plab,pmass,zp
       real themcs
      integer ktrl4
      if(al.eq.0.00) then
      themcs = 0.00
      else
      fac1 = al/alr
      elab = sqrt(plab*plab + pmass*pmass)
      gam = elab/pmass
      beta = sqrt(1.00 - 1.00/(gam*gam))
      if(ktrl4.eq.0) then
      fac2 = log10(fac1)/9.00
      fac3 = 14.10
      else
      fac2 = 0.038*log(fac1)
      fac3 = 13.60
      end if
      themcs = fac3*zp*sqrt(fac1)*(1.00 + fac2)/(1000.00*plab*beta)
      end if
      return
      end
      subroutine pid(n,pm,z)
      implicit none
      integer n
      real pm,z
ccc
ccc   particle identification code of geant:
ccc
ccc   see geant users guide, geant 3.10, cons 300
ccc
ccc     truncated to include only pi+ and pi- for all particles.
ccc     this is only for the matching algorithm
ccc
      pm = 0.139567
      z = 0.0
      if(n.eq.8) z = 1.0
      if(n.eq.9) z = -1.0
      return
      end
      subroutine fcmin(chitmp,jtmp,chi5,j5,ktr,NO_TKS_MAX)
      implicit none
ccc
ccc    Subroutine fcmin:  find chi-square minimum
ccc
ccc    This subroutine finds the five lowest chi-square values among
ccc    those listed in array chitmp and puts these chi-square values
ccc    in increasing order into array chi5.  The corresponding j-index
ccc    values are put into array j5.
ccc
ccc    variable and array type declarations
ccc
      integer NO_TKS_MAX
      integer i,iskip,j,jm1,k,imin,jtmp(NO_TKS_MAX),j5(5),ktr
      real chimax, chimin,chitmp(NO_TKS_MAX),chi5(5)


      dimension imin(5)
      do 9 k = 1,5
9     imin(k) = 0
      chimax = 0.00
      do 10 i = 1,ktr
10    if(chitmp(i).gt.chimax) chimax = chitmp(i)
      do 12 j = 1,5
      jm1 = j - 1
      if(j.eq.1) jm1 = 1
      chimin = chimax
      do 11 i = 1,ktr
      iskip = 0
      do 13 k = 1,jm1
13    if(i.eq.imin(k)) iskip = 1
      if(iskip.eq.1) go to 11
      if(chitmp(i).lt.chimin) then
      chimin = chitmp(i)
      imin(j) = i
      end if
11    continue
      chi5(j) = chimin
      j5(j) = jtmp(imin(j))
12    continue
      return
      end
