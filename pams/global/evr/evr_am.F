CC:>--------------------------------------------------------------------
CC: FILE:       evr_am.F
CC: HISTORY:    
CC:           see below
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION EVR_AM(
     1            evrpar_h,            evrpar ,
     2           globtrk_h,           globtrk ,
     3           privert_h,           privert ) 
      IMPLICIT NONE
#include "evr_am.inc"
#include "math_constants.inc"
#include "phys_constants.inc"
#include "StDetectorId.inc"
#include "StVertexId.inc"

CC:>----------------------------------------------------------------------
CC: ROUTINE:    EVR
CC: DESCRIPTION: Vertex fit, least square method with outlier removal
CC: AUTHOR:     S. Margetis  Created  MARCH-1992
CC: ARGUMENTS:
CC:          IN:
CC:	       	evrpar   = Parameters controlling vertex finding
CC:       	globtrk  = Track information from egr package; uses the
CC:                        first instance of the global track table
CC:                        corresponding to the fit or refit of only the
CC:                        space points from the tracking detectors.
CC:         OUT:
CC:	       	privert  = Primary Vertex Information
CC:      UPDATE:
CC:             globtrk.id_start_vertex -- used to indicate primary
CC:                        track candidates based on 3D DCA cut from
CC:                        the found primary vertex; also used to indicate
CC:                        which tracks were used in the final primary
CC:                        vertex fit (see following explanation of
CC:                        packing algorithm).
CC:             globtrk.impact -- Three dimensional distance of closest
CC:                               approach (3D DCA) to the final primary
CC:                               vertex for all good global tracks (i.e.
CC:                               those with iflag > 0) which have
CC:                               TPC and/or SVT and/or SSD space points
CC:                               (not FTPC tracks).
CC:
CC:     Output variables in table privert:
CC:     ----------------------------------
CC:     privert().id        = Primary key; value of privert_h.nok when
CC:                           row is filled
CC:               iflag     = 1 for normal, successfully found primary vertex
CC:                             during the 3rd iteration
CC:                         = -3 for initial seed value
CC:                         = -2 for first iteration value
CC:                         = -1 for second iteration value
CC:                         = -4 for failed fit with Determinant of G = 0.0,
CC:                              occuring during any iteration.
CC:                         = -5 for failed error covariance matrix evaluation
CC:                              during fit with Determinant of E = 0.0,
CC:                              occuring during any iteration.
CC:               det_id    = Detectors contributing to all tracks used for
CC:                           primary vertex finding/fitting:
CC:                           This quantity, based on the definitions in
CC:                           $STAR/pams/global/inc/StDetectorDefinitions.h,
CC:                           indicates the full
CC:                           list of tracking detectors which contributed
CC:                           to all the tracks used in determinig the primary
CC:                           vertex.  This might include the SVT, SSD and
CC:                           TPC.  The FTPCs are not included in this, nor
CC:                           in the primary vertex finding process.  The
CC:                           tracks from the initial global track selection
CC:                           loop (do 20 loop) are used for this determination.
CC:                           The 'det_id' from this initial, selected set of
CC:                           tracks is reported for each primary vertex
CC:                           iteration. The following algorithm is used:
CC:                           
CC:                           Detectors contributing        Value assigned to
CC:                              to tracks used             privert().det_id
CC:                           -----------------------------------------------
CC:                           TPC only                      kTpcId
CC:                           SVT only                      kSvtId
CC:                           SSD only (nonsense case)      kSsdId
CC:                           TPC & SVT, not SSD            kTpcSvtId
CC:                           TPC & SSD, not SVT            kTpcSsdId
CC:                           SSD & SVT, not TPC            kSsdSvtId
CC:                           SVT, SSD and TPC              kTpcSsdSvtId
CC:                           -----------------------------------------------
CC:               vtx_id    = kEventVtxId (defined in $STAR/pams/global/inc/
CC:                           StVertexDefinitions.h),
CC:                           indicates the primary vertex
CC:             n_daughters = number of tracks used at each iteration in the
CC:                           fit; only good global tracks with SVT and/or
CC:                           SSD, and/or TPC segments are used and counted.
CC:                           FTPC tracks are not used and are not counted.
CC:             x,y,z       = Primary vertex coords in STAR global c.s. (cm)
CC:          covar(1,2...6) = Covariance error matrix for vrtx position (cm^2)
CC:                           defined in DST document for table dst_vertex.
CC:                chisq(2) = Chi-Square of fit per d.o.f. (number of tracks
CC:                           used minus number of varied quantities, the latter
CC:                           corresponds to one vertex position, so #d.o.f. =
CC:                           ntrks-used - 1).  The second value is the 
CC:                           probability of chi-square calculated using the
CC:                           CERNLIB function PROB(X,N).
CC:            id_aux_ent   = 0; used by ev0 for auxiliary vertex table
CC:                              reference, not used by evr.
CC:
CC:     Update variables in table globtrk:
CC:     ---------------------------------
CC:         id_start_vertex = {0, if track is not used in final vertex fit;
CC:                           OR 1, if track is used in final vertex fit}
CC:                           + {10 * privert(final prim. vrtx row#).id,
CC:                              if track passes 3D DCA cut}
CC:                           
CC:                           This marks all global tracks with respect to
CC:                           usage in final primary vertex fit and with
CC:                           respect to a 3D DCA cut from the found primary
CC:                           vertex using the distance cut in
CC:                           evrpar(1).vcut.
CC:         impact          = Three dimensional distance of closest
CC:                           approach (3D DCA) to the final primary
CC:                           vertex for all good global tracks (i.e.
CC:                           those with iflag > 0) which have
CC:                           TPC and/or SVT and/or SSD space points
CC:                           (not FTPC tracks).
CC:                           
CC: RETURNS:    STAF Condition Value
CC:     The code returns one of the following condition values:
CC:
CC:          STAFCV_OK      - indicates normal, successful completion of
CC:                           vertex finding all the way to the final, third
CC:                           fit iteration; indicates to the calling
CC:                           program that event processing should continue.
CC:                           
CC:          STAFCV_BAD     - occurs if number of global tracks < 5, or if
CC:                           number of global tracks < ntrkcut (=50) and
CC:                           number of selected tracks at any step in the
CC:                           processing is < 5.  This indicates to the
CC:                           calling program that the event multiplicity
CC:                           is too small for this module to work.  An
CC:                           alternate, small event primary vertex finder
CC:                           should be called instead of evr_am.  For example,
CC:                           the peripheral collision group's primary vertex
CC:                           module.  This return value may also indicate 
CC:                           that the vertex table (privert) had an
CC:                           insufficient number of rows such that the
CC:                           primary vertex could not be written into it.
CC:                           The calling program should increase the
CC:                           number of assigned rows to table privert and
CC:                           call evr_am again.
CC:                           
CC:          STAFCV_ERR     - occurs if the number of global tracks => ntrkcut
CC:                           (equal to 50) and the number of selected tracks
CC:                           at any step in the processing is < 5.  This
CC:                           indicates to the calling program that a serious
CC:                           failure in evr processing has occured, no
CC:                           primary vertex could be found even though the
CC:                           event multiplicity was large; event 
CC:                           processing should be terminated.
CC:                           
CC: ERROR CONDITIONS	: None-Yet
CC: STATUS		: Tested
CC: MODIFIED TO WORK IN TAS : E. G. Judd, November 1993
CC:   Added switches so the vertex finder can use TPC, SVT or global tracks.
CC:         Also added cuts on pt, and whether or not the track has been
CC:         matched, to improve track selection : E. G. Judd, April 1994
CC: MODIFIED TO WORK IN STAF : S. Margetis, November 1996
CC:
CC:  Modified for greater accepted range in z for vertex position; array
CC:  bounds checking; unused array cleanup; and bug fixes by L. Ray on
CC:  Feb. 22, 1999
CC:
CC:  Following modifications by R.L.Ray during May, 1999:
CC:  (1)  Include optional re-calculation of helix track projection and
CC:       distance of closest approach (DCA) determination for new
CC:       vertex position at each iteration.
CC:  
CC:  (2)  Save initial vertex seed position as well as iterations 1 and 2
CC:       values in privert table with iflag values set as noted above.
CC:  
CC:  (3)  Compute primary vertex covariance matrix and save diagonal
CC:       components in table privert.
CC:       CHANGE July 1999: Save complete primary vertex position covariance
CC:                         matrix in privert().covar[6]
CC:  
CC:  (4)  Estimate multiple Coulomb scattering (MCS) error contribution to
CC:       the uncertainty (sigma) in the extrapolated track position at the
CC:       DCA point.  Use in vertex fit.  Assumes all tracks are TPC only
CC:       and that MCS in TPC inner field cage is dominant effect.  Uses
CC:       spatial rms MCS angle.  Assumes pion mass for MCS angle calculation
CC:       for all tracks.
CC:
CC:       NOTE:  This part will need to be generalized when global tracks
CC:              include SVT tracks and SVT-TPC matched tracks.
CC:       CHANGE July 1999: code checks for SVT or SSD track segments and
CC:                         if these occur an effective SVT radiation 
CC:                         thickness for the first SVT layer is used to
CC:                         estimate the MCS sigma. 
CC:  
CC:  (5)  Modified/corrected helix projection subroutine.
CC:  
CC:  (6)  Include straight track (no B field case) track projection.
CC:  
CC:  (7)  Modified/corrected 3-D DCA subroutine for special case.
CC:  
CC:  (8)  General cleanup and QA verification.
CC:  
CC:  (9)  Use input global track covariance matrix to calculate the
CC:       track position uncertainty at the extrapolated DCA point.
CC:       Use this as an error (sigma) in the i-th track DCA point
CC:       and add in quadrature with the MCS error.
CC:       CHANGE July 1999: Use the Kalman filter/fitter definition; use
CC:                         new global track table (dst_track) with full
CC:                         track fit covariance matrix; use of old
CC:                         globtrk_aux removed.
CC:
CC: (10)  Compute chi-square per track used in fit and save in
CC:       privert().pchi2
CC:       CHANGE July 1999: Compute chi-square per degree of freedom and
CC:                         probability of chi-square and save in
CC:                         privert().chisq(1,2), respectively.
CC:
CC:  Additional changes made in July 1999 by R. L. Ray:
CC:
CC: (11)  Use Detector ID and Vertex ID codes defined in $STAR/pams/global/inc/
CC:       StDetectorDefinitions.h and StVertexDefinitions.h
CC:
CC: (12)  Modified code for no magnetic field case.  If abs(Bz).le.0.001 kGauss,
CC:       Bz is set to 0.0.  In order to have some estimate of the MCS error
CC:       an arbitary value of pT = 0.4 GeV/c is assumed.  The charge is also
CC:       assumed to be +1 for the no B field runs.
CC:
CC: (13)  Calculate 3D DCA to final primary vertex for all good global tracks
CC:       and load into globtrk().impact.
CC:
CC: (14)  Modified for new global track parameters r0,phi0 - 9/29/99 (L.Ray)
CC:
CC:  METHOD:
CC:  Minimization of the perpendicular distances from the vectors to a point.
CC:
CC:  Consider an arbitrary vector Vi, with a unit vector in the direction
CC:  The distance from a point P to the vector Vi is the crossproduct of the 
CC:  unit vector e(Vi)^ with any line connecting P to any point Q on the line.
CC:  Line PQ = (x0-xi)x^+(y0-yi)y^+(z0-zi)z^
CC:	P=(x0,y0,z0)    Q=(xi,yi,zi)
CC:  The distance is: |d|=|PQ X e(Vi)^|
CC:
CC:  A least square minimization employs a merit function (chi square) to 
CC:  assess quality of fit.  The optimum values of the variables are 
CC:  determined by where the partial derivatives of the merit function are
CC:  equal to zero.  For this case, the merit function is the summation of
CC:  the squares of the perpendicular distances from the vectors (Vi) to a 
CC:  point (Xo, Yo, Zo) [divided by the squares of the uncertanty
CC:  errors?] (sumation over vector number).  The partial derivatives are taken
CC:  with respect to x0, y0, and Z0.  We are left with three equations 
CC:  for three unknowns (Xo,Yo,Zo).  The evaluation is done through matrix 
CC:  manipulation.
CC:
CC:>----------------------------------------------------------------------

CC:  Variables in common with Subroutines:
      INTEGER   maxtr
      PARAMETER (maxtr=20000)
      REAL      vtrv(6,maxtr)
      COMMON    /FITVERT/ vtrv
      REAL      XV,YV,ZV,cov(6),chisq
      COMMON    /VERTEX/ XV,YV,ZV,cov,chisq

CC  Local Variable Type Declarations:
      REAL dr,x0(maxtr),y0(maxtr),z0(maxtr),phi0(maxtr),tanl(maxtr)
      REAL sig,sigma(maxtr),vertex_pass(3,3)
      REAL psi,ppt,xx0,yy0,b,zz0,tanla
      REAL xp(2),x(4),xxv(3),xa0(3),pa0(3),x3d0(3)
      REAL zsum(800),seed,zmax,LIFC,LRAD,LSVT1
      REAL xlocal(3),bfield(3),ptdefault,bfield_min,pptdef
      REAL ntrkratio, beampipecut

      INTEGER   i,j,k,ipnt,ntrk,index,gindex(maxtr),icharge
      INTEGER   fitoption,covariance,ntrkcut
      INTEGER   maxdettypes
      PARAMETER (maxdettypes = 30)
      INTEGER   detIdOK,dettrkcounter(maxdettypes)
      INTEGER   DetectorId, DetectorIdSave

CC  Declare new variables used in track cov() matrix contribution to sigma:
      REAL h(5,5),xsave(3),p(5),pnew(5),del(5),pcall(8)
      REAL deriv(3,5),derrorsq,derror,r0save,phi0save
      REAL sigdef,sigmin,sigmax,sigbig
      REAL COSD,SIND
      INTEGER  ii,jj,ialpha,ibeta

CC  Declare called CERNLIB Function:
      REAL PROB

CC  Load Constants:
CC    DATA LIFC  /0.0133/   ! TPC Inner Field Cage radiation thickness
CC                          ! fraction from STAR CDR (1992).
      DATA LIFC  /0.0055/   ! TPC Inner Field Cage radiation thickness
CC                          ! fraction from GSTAR data (1999).
      DATA LSVT1 /0.017/    ! Effective SVT first layer radiation thickness
CC                          ! to roughly approximate MCS effects on track
CC                          ! DCA position uncertainty with SVT and/or SSD hits.
      DATA beampipecut /4.0/  ! Beam pipe radius (cm), used for track DCA
CC                            ! transverse position cut on initial pass.
      DATA ntrkcut  /50/    ! Min. track number for event abort decision
      DATA ptdefault /0.4/  ! Default pT value in GeV/c for use in B_field = 0
CC                          ! cases for estimating MCS error contribution to
CC                          ! the uncertainty in the track's DCA to prim. vrtx.
      DATA bfield_min /0.001/ ! Lower cutoff on abs(B_field, z component) in
CC                            ! kGauss, below which this case is taken to be
CC                            ! a zero magnetic field run; b is set to 0.000 .

CC  The following parameters are used for the extrapolated track position
CC  uncertainty (sigma) at the DCA point to the primary vertex due to the
CC  errors in the track helix parameters.
      DATA sigdef  /1.0/    ! Default track position sigma (cm) at DCA point
      DATA sigmin  /0.01/   ! Minimum track position sigma (cm) at DCA point
      DATA sigmax  /10.0/   ! Maximum track position sigma (cm) at DCA point
      DATA sigbig  /100.0/  ! Upper cutoff track position sigma (cm), DCA point
CC  If the computed track position uncertainty ('error') using the helix
CC  parameter covariance matrix is:
CC
CC  (1)  crazy (i.e. 'error'**2 .le. 0.0 OR 'error' .gt. sigbig)
CC       then the track position sigma is set to the default, sigdef.
CC  (2)  less than sigmin, then 'error' is set to sigmin
CC  (3)  between sigmax and sigbig, then 'error' is set to sigmax
CC  (4)  between sigmin and sigmax, then 'error' is set to the computed value.  

CC  Vertex_pass(i,j) contains the vertex position from the initial
CC  estimate and the first and second passes, where i = pass#, j=x,y,z.

CC  Initialize arrays:

        CALL VZERO(zsum,800)
	ntrk = 0
        ntrkratio = 0.0
        do i = 1,maxtr
           gindex(i) = 0
           do j = 1,6
              vtrv(j,i) = 0.0
           end do
           x0(i)    = 0.0
           y0(i)    = 0.0
           z0(i)    = 0.0
           phi0(i)  = 0.0
           tanl(i)  = 0.0
           sigma(i) = 0.0
        end do
        do i = 1,maxdettypes
           dettrkcounter(i) = 0
        end do

CC  Initialize vertex position values:
      xv = 0.0
      yv = 0.0
      zv = 0.0

CC  Verify range of values of Detector ID codes for use in filling
CC  the array 'dettrkcounter,' for track/detector segment counting:
      if(kTpcId       .gt. 0 .and. kTpcId       .le.maxdettypes .and.
     1   kSvtId       .gt. 0 .and. kSvtId       .le.maxdettypes .and.
     2   kSsdId       .gt. 0 .and. kSsdId       .le.maxdettypes .and.
     3   kTpcSsdId    .gt. 0 .and. kTpcSsdId    .le.maxdettypes .and.
     4   kTpcSvtId    .gt. 0 .and. kTpcSvtId    .le.maxdettypes .and.
     5   kTpcSsdSvtId .gt. 0 .and. kTpcSsdSvtId .le.maxdettypes .and.
     6   kSsdSvtId    .gt. 0 .and. kSsdSvtId    .le.maxdettypes) then 
         Continue
      else
         write(6,*) 'Detector ID values out of range (1,30) - STOP'
         evr_am = STAFCV_BAD
         Return
      end if

c B-field obtained via gufld call
      xlocal(1) = 0.
      xlocal(2) = 0.
      xlocal(3) = 0.

      call gufld(xlocal,bfield)
      write(6,*) 'Using a field of',bfield(3),' KGauss'
      b = bfield(3)

CC  Check value of the magnetic field, z-component.
CC  If b .ge. bfield_min (equal to 0.001 kGauss), then proceed with calc.
CC  But, if b .lt. bfield_min, then set b = 0.0 and set the default pT
CC  value to the greater of ptdefault (equal to 0.4 GeV/c) or
CC  evrpar(1).ptmin, the pT minimum cutoff for the initial track acceptance.
CC  The pT value is needed for the no-field case in order to estimate the
CC  MCS contribution to the DCA position error (sigma) for each track.
CC  A finite value .ge. evrpar(1).ptmin is also needed so that tracks will
CC  pass the initial track selection cut in the do-20 loop below.
      if(abs(b) .lt. bfield_min) then
         b = 0.0
         pptdef = ptdefault
         if(pptdef .lt. evrpar(1).ptmin) then
            pptdef = evrpar(1).ptmin
         end if
      end if

CC  Load control switches (Temporary):
C      fitoption  = 0   !  Turn off when evrpar(1).fitoption available
C      covariance = 0   !  Turn off when evrpar(1).covariance available
      fitoption  = evrpar(1).fitoption ! Activate when evrpar(1).fitoption ok
      covariance = evrpar(1).covariance ! Activate when evrpar(1).covariance ok

CCC  Initial global track selection and setup loop:
C=== use only global tracks, i.e. dst_track instances ====

        ntrk = globtrk_h.nok  
        k = 0

        if (ntrk.lt.5) then
           write(6,*) 'Not enough initial global tracks to find '
           write(6,*) 'the vertex, ntrk=',ntrk
           evr_am = STAFCV_BAD  
           return
        endif

        do 20 i = 1,ntrk
           psi = globtrk(i).psi
           icharge = globtrk(i).icharge
           if (globtrk(i).invpt.ne.0.) then
              ppt = 1./abs(globtrk(i).invpt)
           else
              ppt = 99999.
           endif
           if(b.eq.0.0) then
              icharge = 1
              ppt = pptdef
           end if
           tanla = globtrk(i).tanl
           xx0 = globtrk(i).r0*cosd(globtrk(i).phi0)
           yy0 = globtrk(i).r0*sind(globtrk(i).phi0)
           zz0 = globtrk(i).z0
           globtrk(i).id_start_vertex = 0
           if(globtrk(i).det_id .eq. kTpcId       .or.
     1        globtrk(i).det_id .eq. kSvtId       .or.
     2        globtrk(i).det_id .eq. kSsdId       .or.
     3        globtrk(i).det_id .eq. kTpcSsdId    .or.
     4        globtrk(i).det_id .eq. kTpcSvtId    .or.
     5        globtrk(i).det_id .eq. kTpcSsdSvtId .or.
     6        globtrk(i).det_id .eq. kSsdSvtId) then
              detIdOK = 1
           else
              detIdOK = 0
           end if
           
           if (ppt.ge.evrpar(1).ptmin .and. ppt.ne. 99999. .and.
     1     detIdOK.eq.1 .and. globtrk(i).iflag.gt.0 .and.
     2     icharge.ne.0) then
              k = k + 1
              if(k .le. maxtr) then

CC  Fill Detector Type - Track counter array for this global track:
              dettrkcounter(globtrk(i).det_id) =
     1           dettrkcounter(globtrk(i).det_id) + 1

CC  Determine if the track is purely TPC or if it contains SVT and/or SSD
CC  space points; then set the appropriate radiation thickness to use for
CC  estimating the MCS contribution to the extrapolated track position 
CC  uncertainty at the DCA to the primary vertex.
              if(globtrk(i).det_id .eq. kTpcId) then
                 LRAD = LIFC
              else
                 LRAD = LSVT1
              end if

              xp(1) = 0.0
              xp(2) = 0.0
              call evr_project_track(psi,icharge,ppt,tanla,xx0,yy0,
     1                               b,zz0,xp,x,sig,LRAD)
              gindex(k) = i
	      x0(k) = x(1)
	      y0(k) = x(2)
	      z0(k) = x(3)
              phi0(k)= x(4)
              tanl(k) = tanla
              sigma(k) = sig

CCC  Include track fit error covariance matrix contributions to the
CCC  track position uncertainty at the DCA point:

              derror = 0.0
              if(covariance .eq. 1) then
                 xsave(1) = x(1)
                 xsave(2) = x(2)
                 xsave(3) = x(3)
CCC  Initialize covariance matrix to zero:
                 do ii = 1,5
                    do jj = 1,5
                       h(ii,jj) = 0.0
                    end do
                 end do
CCC  Load track covariance matrix, first fill diagonal elements:
                 h(1,1) = globtrk(i).covar(1)
                 h(2,2) = globtrk(i).covar(6)
                 h(3,3) = globtrk(i).covar(10)
                 h(4,4) = globtrk(i).covar(13)
                 h(5,5) = globtrk(i).covar(15)
CCC  Load off-diagonal elements:
                 h(2,1) = globtrk(i).covar(2)
                 h(3,1) = globtrk(i).covar(3)
                 h(3,2) = globtrk(i).covar(7)
                 h(4,1) = globtrk(i).covar(4)
                 h(4,2) = globtrk(i).covar(8)
                 h(4,3) = globtrk(i).covar(11)
                 h(5,1) = globtrk(i).covar(5)
                 h(5,2) = globtrk(i).covar(9)
                 h(5,3) = globtrk(i).covar(12)
                 h(5,4) = globtrk(i).covar(14)
                 do ii = 2,5
                    do jj = 1,ii-1
                       h(jj,ii) = h(ii,jj)
                    end do
                 end do
CCC  Calculate derivatives using same track parameters as assumed for
CCC  covariance matrix.  This uses the Kalman filter track covariance
CCC  matrix definition.  See comments for table dst_track.
CCC  Compute the 5 track parameters using the Kalman filter definition:
                 r0save = sqrt(xx0*xx0 + yy0*yy0)
                 if(r0save.eq.0.0) then
                    phi0save = 0.0
                 else
                    phi0save = ATAN2(yy0,xx0)*180.0/C_PI
                 end if
                 p(1) = r0save*phi0save
                 p(2) = zz0
                 p(3) = tanla
                 p(4) = psi
                 if(icharge.eq.0) then
                    p(5) = 1.0/ppt
                 else
                    p(5) = float(icharge)/ppt
                 end if
CCC  Set the shifted values of each track parameter needed for calculating
CCC  the partial derivatives numerically.  Use amounts 
CCC  appropriate for SVT, SSD and/or TPC tracks.
                 if(r0save.eq.0.0) then
                    pnew(1) = p(1) + 1.8/C_PI      ! Set r0 = 1 cm
                    pnew(2) = p(2) + 0.01*(1.0 + tanla*tanla)
                 else
                    pnew(1) = p(1) + (1.8/C_PI)*r0save
                    pnew(2) = p(2) + 0.01*r0save*(1.0 + tanla*tanla)
                 end if
                 if((pnew(2)-p(2)).gt.2.0) pnew(2) = p(2) + 2.0
                 pnew(3) = p(3) + 0.01*(1.0 + tanla*tanla)
                 if((pnew(3)-p(3)).gt.0.03) pnew(3) = p(3) + 0.03
                 pnew(4) = p(4) + 1.8/C_PI
                 pnew(5) = 1.01*p(5)
CC  Load parameter differences:
                 do ii = 1,5 
                    del(ii) = pnew(ii) - p(ii)
                 end do
CC  Compute partial derivatives:
                 do ii = 1,5
                    do jj = 1,5
                       pcall(jj) = p(jj)
                    end do
                    pcall(ii) = pnew(ii)
                    if(icharge.eq.0) then          ! Get pcall(8) = pt
                       pcall(8) = 1.0/pcall(5)
                    else
                       pcall(8) = float(icharge)/pcall(5)
                    end if
                    if(r0save.eq.0.0) then         ! Get pcall(6,7)=x0,y0
                       if(ii.eq.1) then
                          pcall(6) = cosd(pcall(1)) ! use shifted r*phi
                          pcall(7) = sind(pcall(1))
                       else
                          pcall(6) = 0.0           ! use original x0,y0    
                          pcall(7) = 0.0
                       end if
                    else
                       pcall(6) = r0save*cosd(pcall(1)/r0save)
                       pcall(7) = r0save*sind(pcall(1)/r0save)
                    end if
                    Call evr_project_track(pcall(4),icharge,pcall(8),
     1                   pcall(3),pcall(6),pcall(7),b,pcall(2),
     2                   xp,x,sig,0.0)
                    do jj = 1,3
                       deriv(jj,ii) = (x(jj)-xsave(jj))/del(ii)
                    end do
                 end do
                 derrorsq = 0.0
                 do ialpha = 1,5
                    do ibeta = 1,5
                       do jj = 1,3
                          derrorsq = derrorsq + deriv(jj,ialpha)*
     1                               deriv(jj,ibeta)*h(ialpha,ibeta)
                       end do
                    end do
                 end do
                 if(derrorsq .le. 0.0) then
                    write(6,*) 'Track position error-sq .le. 0.0 ',
     1                 'using covar()-USE Default:',derrorsq
                    derror = sigdef
                 else
                    derror = sqrt(derrorsq)
                    if(derror.eq.0.0.or.derror.gt.sigbig) then
                       derror = sigdef
                    else if(derror.gt.0.0 .and.
     1                      derror.lt.sigmin) then
                       derror = sigmin
                    else if(derror.gt.sigmax .and.
     1                      derror.le.sigbig) then
                       derror = sigmax
                    end if
                 end if
              end if   !  END Track covariance matrix option
             
CCC   Assign final error (sigma) to extrapolated track position:
              if(sigma(k).eq.0.0 .and. derror.eq.0.0) then
                 sigma(k) = sigdef
              else
                 sigma(k) = sqrt(sigma(k)*sigma(k) + derror*derror)
              end if

              if(abs(z0(k)).lt.200. .and. sqrt(x0(k)*x0(k) +
     1           y0(k)*y0(k)) .le. beampipecut) then
CCC              Modified to correct fat bin at 0.0
                 zsum(int(2.0*(z0(k)+200.0))+1) =      
     1           zsum(int(2.0*(z0(k)+200.0))+1) + 1.   
              endif
              else if (k.gt.maxtr) then
                 k = maxtr
                 GO TO 21     ! Jump out of DO 20 track selection loop;
CC                            ! the remainder of tracks are skipped.
              endif   ! END local array overflow check, maxtr
           endif      ! END track selection cut option
20      continue      ! END initial global track selection and setup loop
21      continue      ! GO TO point for # tracks.gt.maxtr skip out from Do20

        ntrk = k

        if (ntrk.lt.5) then
           if(globtrk_h.nok .ne. 0) then
              ntrkratio = float(ntrk)/float(globtrk_h.nok)
           end if
           write(6,*) 'Not enough global tracks after ptmin,z '
           write(6,*) 'and beampipe cuts to find the vertex:'
           write(6,*) 'ntrk,ntrkratio = ',ntrk,ntrkratio
           if(globtrk_h.nok .lt. ntrkcut) then
              evr_am = STAFCV_BAD
           else
c              evr_am = STAFCV_ERR
           end if
           return
        endif

CCC  Determine initial, rough z-coordinate of vertex and put in 'seed'
        seed=0.
        zmax=0.
        do i=1,800
         if(zsum(i).gt.zmax) then
           zmax=zsum(i)
           seed=0.5*float(i) - 200.25      ! Put seed at center of bin.   
         endif
        enddo   

CC  Load default vertex position:
        xv = xp(1)
        yv = xp(2)
        zv = seed

CC  Save vertex seed value:
        vertex_pass(1,1) = xp(1)
        vertex_pass(1,2) = xp(2)
        vertex_pass(1,3) = seed

CC  Determine most complete list of detectors which contribute space points
CC  to all the tracks selected in the initial pass (DO 20 Loop):
        DetectorIdSave = DetectorId(maxdettypes,dettrkcounter)

CCC  Load initial seed value into table privert:
        index = privert_h.nok + 1
        if(index.le.privert_h.maxlen) then
           privert_h.nok = index
           privert(index).id = index
           privert(index).iflag = -3
           privert(index).det_id = DetectorIdSave
           privert(index).vtx_id = kEventVtxId
           privert(index).n_daughters = k
           privert(index).x           = vertex_pass(1,1)
           privert(index).y           = vertex_pass(1,2)
           privert(index).z           = vertex_pass(1,3)
           do i = 1,6
              privert(index).covar(i) = 0.0
           end do
           privert(index).chisq(1) = 0.0
           privert(index).chisq(2) = 0.0
           privert(index).id_aux_ent  = 0
        else
           write(6,*) 'Privert table is full - cannot load vertex:',
     1                 index
           evr_am = STAFCV_BAD
           Return
        end if

CCC  Select tracks based on transverse x-y and z cuts and load
CCC  track parameters into temporary arrays:
	k=0
	do 10 i=1,ntrk
	   dr=sqrt( x0(i)*x0(i)+y0(i)*y0(i) )
	   if(dr.lt.evrpar(1).cutxy) then
	      if( abs(z0(i)-seed).lt.evrpar(1).cutz) then
	         k=k+1
	         vtrv(1,k)=phi0(i)
	         vtrv(2,k)=tanl(i)
	         vtrv(4,k)=x0(i)
	         vtrv(5,k)=y0(i)
	         vtrv(6,k)=z0(i)
                 vtrv(3,k)=sigma(i)
	      endif
	   endif
10	continue

        if (k.lt.5) then
           if(globtrk_h.nok .ne. 0) then
              ntrkratio = float(k)/float(globtrk_h.nok)
           end if
           write(6,*) 'EVR_AM: too few trks for EVR_VERTEX_FIT'
           write(6,*) 'after cutxy,cutz; k,ratio=',k,ntrkratio
           if(globtrk_h.nok .lt. ntrkcut) then
              evr_am = STAFCV_BAD
           else
c              evr_am = STAFCV_ERR
           end if
           return
        endif

	CALL EVR_VERTEX_FIT(k)
        xxv(1)=xv
        xxv(2)=yv
        xxv(3)=zv

CC  Save first pass vertex:
        vertex_pass(2,1) = xv
        vertex_pass(2,2) = yv
        vertex_pass(2,3) = zv

CCC  Load first iteration vertex value into table privert:
        index = privert_h.nok + 1
        if(index.le.privert_h.maxlen) then
           privert_h.nok = index
           privert(index).id = index
           if(xv .eq. -99999.) then
              privert(index).iflag = -4
           else if(yv .eq. -99999.) then
              privert(index).iflag = -5
           else
              privert(index).iflag = -2
           end if
           privert(index).det_id = DetectorIdSave
           privert(index).vtx_id = kEventVtxId
           privert(index).n_daughters = k
           privert(index).x           = vertex_pass(2,1)
           privert(index).y           = vertex_pass(2,2)
           privert(index).z           = vertex_pass(2,3)
           do i = 1,6
              privert(index).covar(i) = 0.0
           end do
           privert(index).chisq(1) = chisq/float(k-1)
           privert(index).chisq(2) = 0.0
           privert(index).id_aux_ent  = 0
        else
           write(6,*) 'Privert table is full - cannot load vertex:',
     1                 index
           evr_am = STAFCV_BAD
           Return
        end if

CC  Optional Re-calculation of helix track projection to new vertex position:

        if(fitoption .eq. 1) then
           do i = 1,ntrk
              k = gindex(i)
              psi = globtrk(k).psi
              if(b .ne. 0.0) then
                 icharge = globtrk(k).icharge
                 ppt = 1.0/abs(globtrk(k).invpt)
              else if(b .eq. 0.0) then
                 icharge = 1
                 ppt = pptdef
              end if
              tanla = globtrk(k).tanl
              xx0 = globtrk(k).r0*cosd(globtrk(k).phi0)
              yy0 = globtrk(k).r0*sind(globtrk(k).phi0)
              zz0 = globtrk(k).z0
              xp(1) = xxv(1)
              xp(2) = xxv(2)
              Call evr_project_track(psi,icharge,ppt,tanla,xx0,yy0,
     1                               b,zz0,xp,x,sig,LIFC)
              x0(i) = x(1)
              y0(i) = x(2)
              z0(i) = x(3)
              phi0(i) = x(4)
              tanl(i) = tanla
C              sigma(i) = sig    !  Do not update track position error
           end do
        end if

CCC  Select tracks based on tighter 3D DCA cut (cut2) and load
CCC  track parameters into temporary arrays:
	k=0
	do 105 i=1,ntrk
        xa0(1)=x0(i)
        xa0(2)=y0(i)
        xa0(3)=z0(i)
C** straight line tracks, i.e. can put e.g. pt=1 then
        pa0(1)=cosd(phi0(i))
        pa0(2)=sind(phi0(i))
        pa0(3)=tanl(i)
        call ThreeD_dca(xxv,xa0,pa0,x3d0)
	dr=sqrt( (x3d0(1)-xv)*(x3d0(1)-xv)+(x3d0(2)-yv)*(x3d0(2)-yv)
     +          +(x3d0(3)-zv)*(x3d0(3)-zv) )
	if(dr.lt.evrpar(1).cut2) then
	k=k+1
	vtrv(1,k)=phi0(i)
	vtrv(2,k)=tanl(i)
	vtrv(4,k)=x0(i)
	vtrv(5,k)=y0(i)
	vtrv(6,k)=z0(i)
        vtrv(3,k)=sigma(i)
	endif
105	continue

        if (k.lt.5) then
           if(globtrk_h.nok .ne. 0) then
              ntrkratio = float(k)/float(globtrk_h.nok)
           end if
           write(6,*) 'EVR_AM: too few trks for EVR_VERTEX_FIT'
           write(6,*) 'after cut2; k,ratio=',k,ntrkratio
           if(globtrk_h.nok .lt. ntrkcut) then
              evr_am = STAFCV_BAD
           else
c              evr_am = STAFCV_ERR
           end if
           return
        endif

	CALL EVR_VERTEX_FIT(k)
        xxv(1)=xv
        xxv(2)=yv
        xxv(3)=zv

CC  Save second pass vertex:
        vertex_pass(3,1) = xv
        vertex_pass(3,2) = yv
        vertex_pass(3,3) = zv

CCC  Load second iteration vertex value into table privert:
        index = privert_h.nok + 1
        if(index.le.privert_h.maxlen) then
           privert_h.nok = index
           privert(index).id = index
           if(xv .eq. -99999.) then
              privert(index).iflag = -4
           else if(yv .eq. -99999.) then
              privert(index).iflag = -5
           else
              privert(index).iflag = -1
           end if
           privert(index).det_id = DetectorIdSave
           privert(index).vtx_id = kEventVtxId
           privert(index).n_daughters = k
           privert(index).x           = vertex_pass(3,1)
           privert(index).y           = vertex_pass(3,2)
           privert(index).z           = vertex_pass(3,3)
           do i = 1,6
              privert(index).covar(i) = 0.0
           end do
           privert(index).chisq(1) = chisq/float(k-1)
           privert(index).chisq(2) = 0.0
           privert(index).id_aux_ent  = 0
        else
           write(6,*) 'Privert table is full - cannot load vertex:',
     1                 index
           evr_am = STAFCV_BAD
           Return
        end if

CC  Optional Re-calculation of helix track projection to new vertex position:

        if(fitoption .eq. 1) then
           do i = 1,ntrk
              k = gindex(i)
              psi = globtrk(k).psi
              if(b .ne. 0.0) then
                 icharge = globtrk(k).icharge
                 ppt = 1.0/abs(globtrk(k).invpt)
              else if(b .eq. 0.0) then
                 icharge = 1
                 ppt = pptdef
              end if
              tanla = globtrk(k).tanl
              xx0 = globtrk(k).r0*cosd(globtrk(k).phi0)
              yy0 = globtrk(k).r0*sind(globtrk(k).phi0)
              zz0 = globtrk(k).z0
              xp(1) = xxv(1)
              xp(2) = xxv(2)
              Call evr_project_track(psi,icharge,ppt,tanla,xx0,yy0,
     1                               b,zz0,xp,x,sig,LIFC)
              x0(i) = x(1)
              y0(i) = x(2)
              z0(i) = x(3)
              phi0(i) = x(4)
              tanl(i) = tanla
C              sigma(i) = sig    !  Do not update track position error
           end do
        end if

CCC  Final track selection based on 3D DCA cut (cut3) and loading
CCC  track parameters into temporary arrays:
	k=0
	do 205 i=1,ntrk
        xa0(1)=x0(i)
        xa0(2)=y0(i)
        xa0(3)=z0(i)
C** straight line tracks, i.e. can put e.g. pt=1 then
        pa0(1)=cosd(phi0(i))
        pa0(2)=sind(phi0(i))
        pa0(3)=tanl(i)
        call ThreeD_dca(xxv,xa0,pa0,x3d0)
	dr=sqrt( (x3d0(1)-xv)*(x3d0(1)-xv)+(x3d0(2)-yv)*(x3d0(2)-yv)
     +          +(x3d0(3)-zv)*(x3d0(3)-zv) )
	if(dr.lt.evrpar(1).cut3) then
	k=k+1
        globtrk(gindex(i)).id_start_vertex = 1  ! mark global tracks used
CC                                              ! in final vertex fit
	vtrv(1,k)=phi0(i)
	vtrv(2,k)=tanl(i)
	vtrv(4,k)=x0(i)
	vtrv(5,k)=y0(i)
	vtrv(6,k)=z0(i)
        vtrv(3,k)=sigma(i)
	endif
205	continue

        if (k.lt.5) then
           if(globtrk_h.nok .ne. 0) then
              ntrkratio = float(k)/float(globtrk_h.nok)
           end if
           write(6,*) 'EVR_AM: too few trks for EVR_VERTEX_FIT'
           write(6,*) 'after cut3; k,ratio=',k,ntrkratio
           if(globtrk_h.nok .lt. ntrkcut) then
              evr_am = STAFCV_BAD
           else
c              evr_am = STAFCV_ERR
           end if
           return
        endif

	CALL EVR_VERTEX_FIT(k)
        xxv(1)=xv
        xxv(2)=yv
        xxv(3)=zv
C
C save results
C

CC  Load final iteration vertex value into table privert:

        index=privert_h.nok + 1  
      if(index.le.privert_h.maxlen) then
        privert_h.nok = index    
        privert(index).id = index 
        if(xv .eq. -99999.) then
           privert(index).iflag = -4
        else if(yv .eq. -99999.) then
           privert(index).iflag = -5
        else
           privert(index).iflag = 1       ! Final primary vertex marker
        end if
        privert(index).det_id = DetectorIdSave ! global reconstruction
        privert(index).vtx_id  = kEventVtxId   ! primary vertex indicator
        privert(index).n_daughters  = k 
        privert(index).x = xv
        privert(index).y = yv
        privert(index).z = zv

CC  Fill covariance error matrix in units of cm^2:
        if(cov(1) .gt. 0.0) then
           privert(index).covar(1)  = cov(1)
        else
           if (DetectorIdSave.eq.kTpcId) then
              privert(index).covar(1)  = (0.0150)**2
           else
              privert(index).covar(1)  = (0.0050)**2
           end if
        end if

        if(cov(3) .gt. 0.0) then
           privert(index).covar(3)  = cov(3)
        else
           if (DetectorIdSave.eq.kTpcId) then
              privert(index).covar(3)  = (0.0150)**2
           else
              privert(index).covar(3)  = (0.0050)**2
           end if
        end if

        if(cov(6) .gt. 0.0) then
           privert(index).covar(6)  = cov(6)
        else
           if (DetectorIdSave.eq.kTpcId) then
              privert(index).covar(6)  = (0.0200)**2
           else
              privert(index).covar(6)  = (0.0050)**2
           end if
        end if

        privert(index).covar(2) = cov(2)
        privert(index).covar(4) = cov(4)
        privert(index).covar(5) = cov(5)
        privert(index).chisq(1) = chisq/float(k-1)
        privert(index).chisq(2) = PROB(chisq,k-1)
        privert(index).id_aux_ent  = 0
      else
           write(6,*) 'Privert table is full - cannot load vertex:',
     1                 index
           evr_am = STAFCV_BAD
           Return
      end if

C--mark default primary tracks if valid primary vertex has been found
CC:  Also load 3D DCA into globtrk().impact.
CC:  NOTE: This only marks tracks that have pT .ne. 99999.
CC:        AND detIdOK.eq.1 (see below)  AND iflag .gt. 0

      if(privert(index).iflag .eq. 1 .and.
     1   privert(index).vtx_id .eq. kEventVtxId) then
        j = 0   ! Count tracks with pT.ne. 99999.  AND 
C               ! detIdOK.eq.1  AND iflag .gt. 0 AND
C               ! DCA to primary vertex .lt.evrpar(1).vcut
        do i = 1,globtrk_h.nok  
           psi = globtrk(i).psi
           icharge = globtrk(i).icharge
           if (globtrk(i).invpt.ne.0.) then
              ppt = 1./abs(globtrk(i).invpt)
           else
              ppt = 99999.
           endif
           if(b .eq. 0.0) then
              icharge = 1
              ppt = pptdef
           end if
           tanla = globtrk(i).tanl
           xx0 = globtrk(i).r0*cosd(globtrk(i).phi0)
           yy0 = globtrk(i).r0*sind(globtrk(i).phi0)
           zz0 = globtrk(i).z0
           if(globtrk(i).det_id .eq. kTpcId       .or.
     1        globtrk(i).det_id .eq. kSvtId       .or.
     2        globtrk(i).det_id .eq. kSsdId       .or.
     3        globtrk(i).det_id .eq. kTpcSsdId    .or.
     4        globtrk(i).det_id .eq. kTpcSvtId    .or.
     5        globtrk(i).det_id .eq. kTpcSsdSvtId .or.
     6        globtrk(i).det_id .eq. kSsdSvtId) then
              detIdOK = 1
           else
              detIdOK = 0
           end if
           
           if (ppt.ne. 99999. .and. detIdOK .eq. 1
     1        .and. globtrk(i).iflag .gt. 0) then
              xp(1) = xxv(1)
              xp(2) = xxv(2)
              Call evr_project_track(psi,icharge,ppt,tanla,xx0,yy0,b,
     1                               zz0,xp,x,sig,LIFC)
              xa0(1) = x(1)
              xa0(2) = x(2)
              xa0(3) = x(3)
              pa0(1) = cosd(x(4))
              pa0(2) = sind(x(4))
              pa0(3) = tanla
              Call ThreeD_dca(xxv,xa0,pa0,x3d0)
              dr = sqrt((x3d0(1)-xxv(1))**2 + (x3d0(2)-xxv(2))**2
     1                 +(x3d0(3)-xxv(3))**2)
              globtrk(i).impact = dr
              if(dr .lt. evrpar(1).vcut) then
                 j = j + 1
                 globtrk(i).id_start_vertex =
     1           globtrk(i).id_start_vertex + 10*privert(index).id
              end if
           end if
        end do

CCC  Write out the counted number of primary track candidates:
        write(6,*) 'EVR_AM: #primary track candidates = ',j

      end if

        evr_am = STAFCV_OK   

      RETURN
      END


C----------------------------------------------------------------------
      SUBROUTINE EVR_PROJECT_TRACK(psi,q,pt,tanl,x0,y0,bfld,z0,xp,x,
     1           sigma,LIFC)
      IMPLICIT NONE

#include "phys_constants.inc"

CC-----------------------------------------------------------------------
C
C     Input Arguments:
C
C          psi  = azimuthal angle for pt at first point (degrees)
C          q    = particle charge in units of (e)
C          pt   = transverse momentum (GeV/c)
C          tanl = tangent of dip angle at first point
C          x0   = x coordinate of first point (cm)
C          y0   = y coordinate of first point (cm)
C          z0   = z coordinate of first point (cm)
C          bfld = z-component of magnetic field (kilo-gauss)
C          xp   = x & y coordinates of line (parallel to z-axis) to be
C                 approached (cm)
C          LIFC = Radiation length fraction of TPC Inner Field Cage
C                 OR other (e.g. effective SVT first layer) for cases
C                 with SVT and/or SSD space points.
C
C     Output Arguments:
C
C          x(1,2,3,4) = x,y,z coordinates of position on track
C                       corresponding to the distance of closest approach
C                       to the line thru point xp(1),xp(2);
C                       x(4) is the azimuthal angle of pt at this point
C                       in degrees.
C          sigma      = Track position uncertainty estimate at DCA pt. (cm)
C
C     Description:
C
C          This short driver routine determines track curvature and
C          calls the appropriate routine for (sufficiently) curved
C          or straight tracks as needed.  It also estimates the extrap-
C          olated track position uncertainty (sigma) for use in the
C          fitting and error estimate.  This uses the variable trans_pathL
C          where:
C
C          trans_pathL= Transverse (i.e. in x-y plane) path length (cm) along
C                       trajectory from initial point back to the DCA pt.
C
C     Author:   May 1999; R. L. Ray
C     Modified:
C     Error Condition:
C     Status:  Testing (May 99), Done
C
CC-----------------------------------------------------------------------

CC  Local Variable Type Declarations:

      REAL  psi,pt,tanl,x0,y0,z0,bfld,ka,trans_pathL,LIFC
      REAL  xp(2),x(4),epsq,R,RMAX,xc(2),xi
      REAL  sigma, sigma_estimate 
      REAL COSD,SIND 
      INTEGER  q

C      DATA ka  /0.000299792458/
      DATA RMAX /1.0E+05/

      ka = C_D_CURVATURE
CC    Compute epsq = charge*B_z

      epsq = float(q)*bfld

      if(epsq .eq. 0.0) then    ! If neutral and/or B_z = 0 
         Call PROJECT_ST_TRACK(psi,tanl,x0,y0,z0,xp,x)
         trans_pathL = sqrt((x0-x(1))**2 + (y0-x(2))**2)
         sigma = sigma_estimate(pt,x0,y0,psi,tanl,trans_pathL,LIFC)
      else
         R = pt/abs(ka*epsq)
         if(R .le. RMAX) then
            Call PROJECT_HELIX(psi,q,pt,tanl,x0,y0,z0,bfld,xp,x
     1                         ,R,xc,xi)
            trans_pathL = abs(R*xi)
            sigma = sigma_estimate(pt,x0,y0,psi,tanl,trans_pathL,LIFC)
         else
            Call PROJECT_ST_TRACK(psi,tanl,x0,y0,z0,xp,x)
            trans_pathL = sqrt((x0-x(1))**2 + (y0-x(2))**2)
            sigma = sigma_estimate(pt,x0,y0,psi,tanl,trans_pathL,LIFC)
         end if
      end if

      Return
      END

C------------------------------------------------------------------------
      SUBROUTINE PROJECT_HELIX(psi,q,pt,tanl,x0,y0,z0,bfld,xp,x
     1                         ,R,xc,xi)  
C------------------------------------------------------------------------
      implicit none
#include "math_constants.inc"
#include "phys_constants.inc"

CC-----------------------------------------------------------------------
C
C     Input Arguments:
C
C          psi  = azimuthal angle for pt at first point (degrees)
C          q    = particle charge in units of (e)
C          pt   = transverse momentum (GeV/c)
C          tanl = tangent of dip angle at first point
C          x0   = x coordinate of first point (cm)
C          y0   = y coordinate of first point (cm)
C          z0   = z coordinate of first point (cm)
C          bfld = z-component of magnetic field (kilo-gauss)
C          xp   = x & y coordinates of line (parallel to z-axis) to be
C                 approached (cm)
C
C     Output Arguments:
C
C          x(1,2,3,4) = x,y,z coordinates of position on helical track
C                       corresponding to the distance of closest approach
C                       to the line thru point xp(1),xp(2);
C                       x(4) is the azimuthal angle of pt at this point
C                       in degrees.
C          R          = radius of circle projection of helix onto x-y plane (cm)
C          xc(1,2)    = x,y coordinates of center of circle (cm)
C          xi         = phase angle from initial point to DCA point (radians)
C
C     Description:
C
C          This code finds the position along an arbitrary helix defined
C          by the input parameters (psi,q,pt,tanl,x0,y0,z0,bfld) which
C          is closest to an arbitrary line parallel to the z-axis going
C          thru a point at x=xp(1), y=xp(2).  It is assumed that the DCA
C          point on the helix occurs when the DCA (x,y) point is first
C          reached in going backwards along the helix trajectory from
C          the initial point on the track.  The center of the circle
C          (projection of helix onto x-y plane) is found first, then
C          the DCA point in x,y is located, then the phase angle in the
C          x-y plane from the initial point back to the DCA point is 
C          evaluated, then the z-coord at the DCA point is computed,
C          and finally the azimuthal angle for the pt momentum direction
C          in the x-y plane at the DCA point (i.e. x(4)) is calculated.
C
C     Author:   May 1999; R. L. Ray
C     Modified:
C     Error Condition:
C     Status:  Testing (May 99), Done
C
CC-----------------------------------------------------------------------

CC  Local Variable Type Declarations:

      REAL  psi,pt,tanl,x0,y0,z0,bfld,ka
      REAL  xp(2),x(4),xc(2)
      REAL  R,a,b,c,invc,signa,signb,cut
      REAL  phi0,phiDCA,xi,epsq
      REAL COSD,SIND    
      INTEGER  q

      DATA cut /1.0E-03/
C      DATA ka  /0.000299792458/

      ka = C_D_CURVATURE
CC    Compute epsq = sign of charge*B_z

      epsq = float(q)*bfld
      if(epsq .eq. 0.0) then    ! If neutral or B_z = 0 then return defaults
         x(1) = 0.0
         x(2) = 0.0
         x(3) = 0.0
         x(4) = 0.0
         RETURN
      else
         epsq = epsq/abs(epsq)
      end if

CC    Find radius of helix, center of circle projection onto x-y plane,
CC    and x,y coordinates of DCA point on helix.

      R     = pt/abs(ka*float(q)*bfld)
      xc(1) = x0 + R*cosd(psi - epsq*90.0)
      xc(2) = y0 + R*sind(psi - epsq*90.0)
      a = xp(1) - xc(1)
      b = xp(2) - xc(2)
      if(abs(a) .eq. 0.0 .and. abs(b) .eq. 0.0) then  ! Line coincides
C                                                     ! with axis of helix
C                                                     ! Return initial pt.
         x(1) = x0
         x(2) = y0
         x(3) = z0
         x(4) = psi
         xi   = 0.0
         RETURN
      else if(abs(b) .eq. 0.0 .and. abs(a) .gt. 0.0) then
         signa = a/abs(a)
         x(1) = xc(1) + signa*R
         x(2) = xc(2)
      else if(abs(a) .eq. 0.0 .and. abs(b) .gt. 0.0) then
         signb = b/abs(b)
         x(1) = xc(1)
         x(2) = xc(2) + signb*R
      else
         c = a/b
         invc = b/a
         signa = a/abs(a)
         signb = b/abs(b)
         x(2) = xc(2) + signb*R/sqrt(1.0 + c*c)
         if(abs(invc) .le. cut) then
            x(1) = xc(1) +signa*R*(1.0 - 0.5/(1.0 + c*c))
         else
            x(1) = xc(1) + c*(x(2) - xc(2))
         end if
      end if

CC    Find the phase angle to go from initial point back to the DCA point x;
CC    then calculate the azimuthal direction angle of p_T at the DCA point x.

      phi0 = ATAN2(y0-xc(2),x0-xc(1))
      phiDCA = ATAN2(x(2)-xc(2),x(1)-xc(1)) 
      xi   = epsq*(phi0 - phiDCA)
      if(xi .gt. 0.0) then
         xi = xi - C_2PI
      end if
      x(3) = z0 + R*tanl*xi
      x(4) = phiDCA*180.0/C_PI - epsq*90.0

      RETURN
      END

C------------------------------------------------------------------------
      SUBROUTINE PROJECT_ST_TRACK(psi,tanl,x0,y0,z0,xp,x)
C------------------------------------------------------------------------
      implicit none

CC-----------------------------------------------------------------------
C
C     Input Arguments:
C
C          psi  = azimuthal angle for pt at first point (degrees)
C          tanl = tangent of dip angle at first point
C          x0   = x coordinate of first point (cm)
C          y0   = y coordinate of first point (cm)
C          z0   = z coordinate of first point (cm)
C          xp   = x & y coordinates of line (parallel to z-axis) to be
C                 approached (cm)
C
C     Output Arguments:
C
C          x(1,2,3,4) = x,y,z coordinates of position on (straight) track
C                       corresponding to the distance of closest approach
C                       to the line thru point xp(1),xp(2);
C                       x(4) is the azimuthal angle of pt at this point
C                       which for straight tracks is identical to psi.
C
C     Description:
C
C          This code finds the position along an arbitrary straight line
C          defined by coordinates x0,y0,z0 and direction determined by
C          psi and tanl, which is closest to an arbitrary line parallel to
C          the z-axis going thru a point at x=xp(1), y=xp(2).
C
C     Author:   Mar 1999; R. L. Ray
C     Modified:
C     Error Condition:
C     Status:  Testing (Mar 99), Done
C
CC-----------------------------------------------------------------------

CC  Local Variable Type Declarations:

      REAL  psi,tanl,x0,y0,z0,xp(2),x(4)
      REAL  px,py,cut,d,m1,m2,pdu
      REAL COSD,SIND   
      DATA cut /1.0E-7/

CC  Determine if special cases apply:

      px = cosd(psi)
      py = sind(psi)

      if(abs(px) .le. cut) then
         x(1) = x0
         x(2) = xp(2)
         x(3) = z0 + py*(x(2) - y0)*tanl

      else if (abs(py) .le. cut) then
         x(1) = xp(1)
         x(2) = y0
         x(3) = z0 + px*(x(1) - x0)*tanl

      else
         m2 = py/px           ! slope of pt in x-y plane
         m1 = -1.0/m2         ! slope of line from xp->x @ dca (perp.)
         x(1) = (m1*xp(1)-m2*x0-xp(2)+y0)/(m1-m2)
         x(2) = (m1*m2*(xp(1)-x0) + m1*y0 - m2*xp(2))/(m1-m2)
         d = sqrt((x(1)-x0)**2 + (x(2)-y0)**2)
         if(d .gt. cut) then
            pdu = px*(x(1)-x0)/d + py*(x(2)-y0)/d
            if(abs(abs(pdu)-1.0) .gt. 100.0*cut) then
               if(pdu .ge. 0.0) then
                  pdu = 1.0
               else
                  pdu = -1.0
               end if
            end if

            if(abs(abs(pdu)-1.0) .gt. 0.0001) then
               write(6,*) 'EVR_AM: Direction cosines .ne. +/-1,=',pdu
            end if

            x(3) = z0 + pdu*d*tanl
         else
            x(3) = z0
         end if
      end if

CC    For straight tracks:

      x(4) = psi

      Return
      END


C------------------------------------------------------------------------
      Real*4 Function sigma_estimate(pt,x0,y0,psi,tanl,trans_pathL,LIFC)
C------------------------------------------------------------------------
      Implicit none
#include "math_constants.inc"
#include "phys_constants.inc"

CC-----------------------------------------------------------------------
C
C     Input Arguments:
C
C          pt   = transverse momentum (GeV/c)
C          x0   = x coordinate of first point (cm) 
C          y0   = y coordinate of first point (cm) 
C          psi  = azimuthal angle for pt at first point (degrees)
C          tanl = tangent of dip angle at first point
C          trans_pathL = transverse path length (cm) of trajectory
C                        from initial point back to the DCA point.
C          LIFC = Radiation length fraction of TPC Inner Field Cage
C                 OR other, e.g. effective SVT first layer.
C
C     Output:   sigma_estimate = estimated sigma (cm) of position
C                                uncertainty of projected track at
C                                DCA point.
C
C     Description:
C
C          This code assumes all tracks are either:
C             (1) pure TPC space points only, or
C             (2) have SVT - SSD points.
C          For the former the algorithm assumes the TPC only track  
C          starts near the TPC IFC and that MCS in the TPC-IFC is the
C          dominant effect on the projected track position uncertainty
C          at the DCA point.  This neglects the SVT-SSD contribution
C          to MCS if this detector material is present.  This could be
C          of order half the position error squared from the TPC-IFC.
C          The beam pipe contribution is negligible for a beryllium pipe. 
C          
C          For tracks with SVT and/or SSD space points the algorithm
C          assumes the track's first point is in the first layer of the
C          SVT and uses an effective radiation length fraction of one-third
C          of the total of 5% (latter amount from GSTAR geometry and material
C          plots - 1999).  The beam pipe contribution is negligible
C          for a beryllium pipe.  The actual path length from the initial
C          point on the track (in any SVT layer or in the SSD 4th layer)
C          to the extrapolated DCA point is used however.
C          
C          In both cases the position uncertainty is assumed to be the
C          spatial rms MCS angle uncertainty [sqrt(2.0)*theta_0] times
C          the path length.
C
C     Author:   May 1999; R. L. Ray
C     Modified: Physics and Math constants added July 1999, R.L.Ray
C     Error Condition:
C     Status:  Testing (May 99), Done
C
CC-----------------------------------------------------------------------

CC  Local Variable Type Declarations:

      REAL pt,x0,y0,psi,tanl,trans_pathL
      REAL fac,ptot,phi0,cospr,Leff,LIFC
      REAL theta_mcs,sqrt2
      REAL COSD,SIND
C      DATA sqrt2  /1.4142135/

      sqrt2 = C_SQRT2
      fac = sqrt(1.0 + tanl*tanl)
      ptot = pt*fac
      phi0 = ATAN2(y0,x0)
      cospr = (cosd(psi)*cos(phi0) + sind(psi)*sin(phi0))/fac

      if(cospr .eq. 0.0) then
         sigma_estimate = trans_pathL*fac*sqrt2*
     1         theta_mcs(M_PION_PLUS,1.0,ptot,LIFC,1.0,1)
      else
         Leff = LIFC/abs(cospr)
         sigma_estimate = trans_pathL*fac*sqrt2*
     1         theta_mcs(M_PION_PLUS,1.0,ptot,Leff,1.0,1)
      end if

      Return
      END
 
C---------------------------------------------------------------------- 
      Real*4 Function theta_mcs(mass,charge,momentum,L,LR,
     1       formula_type)
      implicit none
C----------------------------------------------------------------------

CCC   Calculation of the Multiple Coulomb Scattering rms planar angle theta0
C     using the formula in the Particle Data Group's article in Phys. Lett.
C     B204 (1988), pg. 65; or the alternate formula by the Particle Data
C     Group in "Particle Physics Booklet," AIP Press, July 1996, p. 167.
C
C     INPUT:
C
C          formula_type = 0 to use the Phys. Lett. formula given by
C
C                         theta0 = (14.1 MeV/c /(p*beta)) Z_inc sqrt{L/LR}
C                                * [1 + (1/9)log10(L/LR)] in radians
C                       where (1/9)log10(x) = 0.048254ln(x)
C
C          formula_type = 1 to use the Particle Physics Booklet version,
C
C                         theta0 = (13.6 MeV/c /(p*beta)) Z_inc sqrt{L/LR}
C                                * [1 + 0.038 ln(L/LR)] in radians
C          Note: the default is set to 1.
C
C          mass     - particle mass in GeV.
C          charge   - particle charge in |e|.
C          momentum - particle momentum relative to medium in GeV/c.
C          L        - path length thru medium in cm.
C          LR       - radiation length for medium in cm.
C
C     OUTPUT:
C
C          theta_mcs = theta0, rms angle in plane in radians.
CCC

CCC   Variable Declarations:

      real*4 mass,charge,momentum,L,LR
      real*4 theta_fac0, theta_fac1, LOGFAC0, LOGFAC1
      real*4 E, gamma, beta, LFAC
      integer*4 formula_type

      parameter(theta_fac0 = 14.1)
      parameter(theta_fac1 = 13.6)
      parameter(LOGFAC0    = 0.048254)
      parameter(LOGFAC1    = 0.038)

CCC   Begin Calculation, if L or LR = 0.0, set theta_mcs to zero and return:

      if(L .le. 0.0 .or. LR .le. 0.0) then
         theta_mcs = 0.0
      else
         LFAC = L/LR
         E = sqrt(momentum*momentum + mass*mass)
         gamma = E/mass
         beta = sqrt(1.0 - 1.0/(gamma*gamma))

         if(formula_type .eq. 0) then
            theta_mcs = theta_fac0*abs(charge)*sqrt(LFAC)
     1      *(1.0 + LOGFAC0*log(LFAC))/(1000.0*momentum*beta)
         else
            theta_mcs = theta_fac1*abs(charge)*sqrt(LFAC)
     1      *(1.0 + LOGFAC1*log(LFAC))/(1000.0*momentum*beta)
         end if

      end if

      Return
      END 

C-------------------------------------------------------------------
	SUBROUTINE EVR_VERTEX_FIT(NT)
	IMPLICIT NONE
CC:>--------------------------------------------------------------------
CC:Passed parameters:  NT = number of tracks involved in the fit
CC:returned param:    
CC:		      Xv,Yv,Zv = the vertex co-ordinants
CC:		      NT = number of tracks used for the vertex fitting
CC:			   (note this variable is passed and returned.)
CC:		       the pointer list is reordered to reflect which
CC:		       tracks were used in the fit
CC:
CC:Description: It performs the least square fit to find a common vertex
CC:for NT tracks whose parameters are stored in the /fitvert/ common.
CC:The vertex information is returned as coordinants.
CC:
CC:Written by Daniel Cebra
CC:Modified for STAR by S. Margetis
CC: Modified by R. L. Ray, May 1999, add errors and clean-up unused 
CC: stuff.  Add calculation of error covariance matrix for 3D vertex
CC: position.  The cov(1,2...6) matrix is defined by:
CC:
CC:           cov(#) |   x   y   z
CC:           ----------------------
CC:              x   |   1
CC:              y   |   2   3
CC:              z   |   4   5   6
CC:           ----------------------
CC:
CC: Also the Chi-Square of the fit is computed and returned to the
CC: calling program via the common block /VERTEX/.
CC:
CC:   Status:   Testing in May, 1999, Done.
CC:>--------------------------------------------------------------------

	integer ngmx
	PARAMETER (NGMX=20000)
	REAL vtrv(6,ngmx)
	common /fitvert/ vtrv
	real Xv,Yv,Zv,cov(6),chisq
	COMMON /VERTEX/XV,YV,ZV,cov,chisq
c  passed parameters:
	integer NT

c  Local variables:
	integer i,j,k
	real x0,y0,z0
	real sinV,cosV,sinL,cosL
	real sigma(3,3),H(3),G(3,3),Txx,Txy,Txz,Tyy,Tyz,Tzz,G_inv(3,3),det
	real sigma2 
        real E(3,3),Exx,Exy,Exz,Eyy,Eyz,Ezz,E_inv(3,3),detE
        real Sigxv,Sigyv,Sigzv
        real ex,ey,ez
        REAL COSD,SIND

c  Convention: L=angle lambda, V=angle phi

200	do i=1,3			! Clear the transformation matrices
	  H(i)=0.0
	  do j=1,3
	    G(i,j)=0.0
            E(i,j)=0.0
	  end do
	end do

CC  Initialize all output quantities:
        xv = 0.0
        yv = 0.0
        zv = 0.0
        chisq = 0.0
        do i = 1,6
           cov(i) = 0.0
        end do

      if(nt.le.0) then
         xv = -99999.
         Return
      end if

	do k=1,nt			! Master loop over tracks
	  sinV=sind(vtrv(1,k))
	  cosV=cosd(vtrv(1,k))
	  cosL=1.0/(sqrt(1.0 + vtrv(2,k)**2))
	  sinL=cosL*vtrv(2,k)

	  X0= vtrv(4,k)
	  Y0= vtrv(5,k)
	  Z0= vtrv(6,k)
          sigma2 = vtrv(3,k)*vtrv(3,k)
          if(sigma2 .eq. 0.0) sigma2 = 1.0

c  Calculate the matrix elements of Ti (individual transformation matrices)
c  in the document Mi is the same as Ti
	  Txx= sinL**2+(sinV*cosL)**2
	  Txy=-cosL*cosL*cosV*sinV
	  Txz=-cosV*cosL*sinL
	  Tyy= sinL**2+(cosL*cosV)**2
	  Tyz=-cosL*sinL*sinV
	  Tzz= cosL*cosL
          Txx = Txx/sigma2
          Txy = Txy/sigma2
          Txz = Txz/sigma2
          Tyy = Tyy/sigma2
          Tyz = Tyz/sigma2
          Tzz = Tzz/sigma2
c  Compute inverse of covariance matrix:
          Exx = (1.0 - (cosV*cosL)**2)/sigma2
          Eyy = (1.0 - (sinV*cosL)**2)/sigma2
          Ezz = (1.0 - sinL**2)/sigma2
          Exy = Txy
          Exz = Txz
          Eyz = Tyz
c  The G matrix is the summations of the individual transformation matrices 
	  G(1,1)=G(1,1)+Txx
	  G(1,2)=G(1,2)+Txy
	  G(1,3)=G(1,3)+Txz
	  G(2,2)=G(2,2)+Tyy
	  G(2,3)=G(2,3)+Tyz
	  G(3,3)=G(3,3)+Tzz
c  sum the inverse covariance matrix:
          E(1,1) = E(1,1) + Exx
          E(1,2) = E(1,2) + Exy
          E(1,3) = E(1,3) + Exz
          E(2,2) = E(2,2) + Eyy
          E(2,3) = E(2,3) + Eyz
          E(3,3) = E(3,3) + Ezz
c  The H vector is the summation of the Qi times Ti
	  H(1)=H(1)+X0*Txx+Y0*Txy+Z0*Txz
	  H(2)=H(2)+X0*Txy+Y0*Tyy+Z0*Tyz
	  H(3)=H(3)+X0*Txz+Y0*Tyz+Z0*Tzz
	end do
c  assign the reflexive elements
	G(2,1)=G(1,2)
	G(3,1)=G(1,3)
	G(3,2)=G(2,3)
        E(2,1)=E(1,2)
        E(3,1)=E(1,3)
        E(3,2)=E(2,3)
c  Invert the G matrix and calculate the determinant
c  Calcualate the determinant
	det=G(1,1)*(G(2,2)*G(3,3)-G(2,3)*G(3,2))-
     >      G(2,1)*(G(1,2)*G(3,3)-G(1,3)*G(3,2))+
     >      G(3,1)*(G(1,2)*G(2,3)-G(1,3)*G(2,2))
	IF(abs(DET).LT.1.0E-25) THEN
	    write(6,*) ' V_F:  determinant too small   det=',det
	    write(6,*) (G(1,k),k=1,3)
	    write(6,*) (G(2,k),k=1,3)
	    write(6,*) (G(3,k),k=1,3)
            xv = -99999.    !  Identify this as a failed fit attempt
	    GO TO 999
	END IF
	G_inv(1,1)=+(G(2,2)*G(3,3)-G(2,3)*G(3,2))/det
	G_inv(1,2)=-(G(1,2)*G(3,3)-G(1,3)*G(3,2))/det
	G_inv(1,3)=+(G(1,2)*G(2,3)-G(1,3)*G(2,2))/det
	G_inv(2,1)=-(G(2,1)*G(3,3)-G(2,3)*G(3,1))/det
	G_inv(2,2)=+(G(1,1)*G(3,3)-G(1,3)*G(3,1))/det
	G_inv(2,3)=-(G(1,1)*G(2,3)-G(1,3)*G(2,1))/det
	G_inv(3,1)=+(G(2,1)*G(3,2)-G(2,2)*G(3,1))/det
	G_inv(3,2)=-(G(1,1)*G(3,2)-G(1,2)*G(3,1))/det
	G_inv(3,3)=+(G(1,1)*G(2,2)-G(1,2)*G(2,1))/det

c  The vertex vector = The inverted G matrix times the H vector

	Xv=G_inv(1,1)*H(1)+G_inv(1,2)*H(2)+G_inv(1,3)*H(3)
	Yv=G_inv(2,1)*H(1)+G_inv(2,2)*H(2)+G_inv(2,3)*H(3)	
	Zv=G_inv(3,1)*H(1)+G_inv(3,2)*H(2)+G_inv(3,3)*H(3)	
        write(6,*) ' Estimated Vertex:',Xv,Yv,Zv

CC  Compute Chi-Square of fit to vertex, return the total chi-square value:
        do k = 1,nt
           sinV = sind(vtrv(1,k))
           cosV = cosd(vtrv(1,k))
           cosL = 1.0/sqrt(1.0 + vtrv(2,k)**2)
           ex = cosV*cosL
           ey = sinV*cosL
           ez = vtrv(2,k)*cosL
           X0 = vtrv(4,k)
           Y0 = vtrv(5,k)
           Z0 = vtrv(6,k)
           sigma2 = vtrv(3,k)*vtrv(3,k)
           if(sigma2 .eq. 0.0) sigma2 = 1.0
           chisq = chisq
     1           + (((YV - Y0)*ez - (ZV- Z0)*ey)**2
     2           +  ((XV - X0)*ey - (YV- Y0)*ex)**2
     3           +  ((ZV - Z0)*ex - (XV- X0)*ez)**2)/sigma2
        end do
C        chisq = chisq/float(nt)   !  Return the Total Chi Square

c  Invert the E matrix and calculate the determinant
c  Calcualate the determinant
       detE=E(1,1)*(E(2,2)*E(3,3)-E(2,3)*E(3,2))-
     >      E(2,1)*(E(1,2)*E(3,3)-E(1,3)*E(3,2))+
     >      E(3,1)*(E(1,2)*E(2,3)-E(1,3)*E(2,2))
        IF(abs(detE).LT.1.0E-25) THEN
            write(6,*) 'COV :  determinant too small   det=',detE
            write(6,*) (E(1,k),k=1,3)
            write(6,*) (E(2,k),k=1,3)
            write(6,*) (E(3,k),k=1,3)
            yv = -99999.  !  Identify this as a failed fit for the
CCC                       !  covariance matrix calculation.
            GO TO 999
        END IF
        E_inv(1,1)=+(E(2,2)*E(3,3)-E(2,3)*E(3,2))/detE
        E_inv(1,2)=-(E(1,2)*E(3,3)-E(1,3)*E(3,2))/detE
        E_inv(1,3)=+(E(1,2)*E(2,3)-E(1,3)*E(2,2))/detE
        E_inv(2,1)=-(E(2,1)*E(3,3)-E(2,3)*E(3,1))/detE
        E_inv(2,2)=+(E(1,1)*E(3,3)-E(1,3)*E(3,1))/detE
        E_inv(2,3)=-(E(1,1)*E(2,3)-E(1,3)*E(2,1))/detE
        E_inv(3,1)=+(E(2,1)*E(3,2)-E(2,2)*E(3,1))/detE
        E_inv(3,2)=-(E(1,1)*E(3,2)-E(1,2)*E(3,1))/detE
        E_inv(3,3)=+(E(1,1)*E(2,2)-E(1,2)*E(2,1))/detE

CC  Load the covariance matrix elements:

        cov(1) = E_inv(1,1)
        cov(2) = E_inv(2,1)
        cov(3) = E_inv(2,2)
        cov(4) = E_inv(3,1)
        cov(5) = E_inv(3,2)
        cov(6) = E_inv(3,3)
        Sigxv = 0.0
        Sigyv = 0.0
        Sigzv = 0.0
        if(cov(1) .ge. 0.0) Sigxv = sqrt(cov(1))
        if(cov(3) .ge. 0.0) Sigyv = sqrt(cov(3))
        if(cov(6) .ge. 0.0) Sigzv = sqrt(cov(6))
        write(6,*) ' Diagonal Errors:', Sigxv,Sigyv,Sigzv
        write(6,*) ' COV=',cov(1),cov(2),cov(3),cov(4),cov(5),cov(6)
999	Return
	END

C----------------------------------------------------------------------
      SUBROUTINE ThreeD_dca(xv,x,p,x0)
C----------------------------------------------------------------------
      IMPLICIT NONE
C   Input arguments     xv      ! 3-dimensional position vector 
C                               ! of the main vertex
C                       x       ! 3-dimensional position vector 
C                               ! of the secondary track
C                       p       ! 3-dimensional momentum vector of the 
C                               ! reconstructed secondary particle
C   Output arguments :  x0      ! coordinates of closest approach
C
C   Functional Description:     Finds the coordinates of the point of
C                               closest approach from the main vertex
C                               The input vectors must be calculated
C                               at a point close to the vertex.
C   Created  JUN-1992  S.Margetis
C   Modified: 4/2/99 R.L.Ray
C   Error conditions            : Error for p(1)=0 fixed (4/2/99 RLR)
C   Status                      : Tested
C----------------------------------------------------------------------
      REAL      xv(3),x(3),p(3),x0(3),nom,cut,det
      DATA  cut /1.0E-06/
*----------------------------------------------------------------------

        nom=(p(2)*p(2)+p(3)*p(3))*x(1)-(x(2)*p(2)+x(3)*p(3))*p(1) 
        nom=nom+(xv(1)*p(1)+xv(2)*p(2)+xv(3)*p(3))*p(1)
        x0(1)=nom/( p(1)*p(1)+p(2)*p(2)+p(3)*p(3) )

C       IF(p(1).ne.0.) THEN
        IF(abs(p(1)) .gt. cut) THEN              ! RLR (4/99)
         x0(2)=x(2)+(p(2)/p(1))*(x0(1)-x(1))
         x0(3)=x(3)+(p(3)/p(1))*(x0(1)-x(1))
        ELSE
         det = p(2)*p(2) + p(3)*p(3)             ! RLR (4/99)
         x0(2)=(x(2)*p(3)*p(3)+xv(2)*p(2)*p(2)+  ! RLR (4/99)
     1         (xv(3)-x(3))*p(2)*p(3))/det       ! RLR (4/99)
         x0(3)=(x(3)*p(2)*p(2)+xv(3)*p(3)*p(3)+  ! RLR (4/99)
     1         (xv(2)-x(2))*p(2)*p(3))/det       ! RLR (4/99)

C        x0(3)=p(2)*(x(3)*p(2)-x(2)*p(3))/(p(2)*p(2)+p(3)*p(3))
C               IF(p(2).eq.0.) THEN
C                x0(2)=x(2)
C               ELSE
C                x0(2)=-x0(3)*p(3)/p(2)
C               ENDIF

        ENDIF

      RETURN
      END

C----------------------------------------------------------------------
      INTEGER*4 FUNCTION DetectorId(ndim,D)
C----------------------------------------------------------------------
      IMPLICIT NONE
#include "StDetectorId.inc"

CC   This function determines the complete list of detectors among
CC   the TPC, SVT and SSD (not FTPC) that contribute hits to all the 
CC   tracks selected in the initial track selection loop.  The return
CC   value indicates the detectors used via the Detector ID definitions
CC   in StDetectorDefinitions.h

      INTEGER ndim
      INTEGER D(ndim)

CC   TPC tracks only:
      if(D(kTpcId) .gt. 0 .and. D(kSvtId)       .eq. 0
     1                    .and. D(kSsdId)       .eq. 0
     2                    .and. D(kTpcSsdId)    .eq. 0
     3                    .and. D(kTpcSvtId)    .eq. 0
     4                    .and. D(kTpcSsdSvtId) .eq. 0
     5                    .and. D(kSsdSvtId)    .eq. 0) then
         DetectorId = kTpcId
         Return

CC   SVT tracks only:
      else if(D(kSvtId) .gt. 0 .and. D(kTpcId)       .eq. 0
     1                         .and. D(kSsdId)       .eq. 0
     2                         .and. D(kTpcSsdId)    .eq. 0
     3                         .and. D(kTpcSvtId)    .eq. 0
     4                         .and. D(kTpcSsdSvtId) .eq. 0
     5                         .and. D(kSsdSvtId)    .eq. 0) then
         DetectorId = kSvtId
         Return

CC   SSD tracks only (nonsensical case but who knows, maybe ?):
      else if(D(kSsdId) .gt. 0 .and. D(kTpcId)       .eq. 0
     1                         .and. D(kSvtId)       .eq. 0
     2                         .and. D(kTpcSsdId)    .eq. 0
     3                         .and. D(kTpcSvtId)    .eq. 0
     4                         .and. D(kTpcSsdSvtId) .eq. 0
     5                         .and. D(kSsdSvtId)    .eq. 0) then
         DetectorId = kSsdId
         Return

CC   SVT-TPC tracks:
      else if(((D(kTpcId).gt.0 .and. D(kSvtId).gt.0) .or.
     1   D(kTpcSvtId).gt.0)    .and. D(kSsdId)       .eq. 0
     2                         .and. D(kTpcSsdId)    .eq. 0
     3                         .and. D(kTpcSsdSvtId) .eq. 0
     4                         .and. D(kSsdSvtId)    .eq. 0) then
         DetectorId = kTpcSvtId
         Return

CC   SSD-TPC tracks:
      else if(((D(kTpcId).gt.0 .and. D(kSsdId).gt.0) .or.
     1   D(kTpcSsdId).gt.0)    .and. D(kSvtId)       .eq. 0
     2                         .and. D(kTpcSvtId)    .eq. 0
     3                         .and. D(kTpcSsdSvtId) .eq. 0
     4                         .and. D(kSsdSvtId)    .eq. 0) then
         DetectorId = kTpcSsdId
         Return

CC   SSD-SVT tracks:
      else if(((D(kSsdId).gt.0 .and. D(kSvtId).gt.0) .or.
     1   D(kSsdSvtId).gt.0)    .and. D(kTpcId)       .eq. 0
     2                         .and. D(kTpcSsdId)    .eq. 0
     3                         .and. D(kTpcSvtId)    .eq. 0
     4                         .and. D(kTpcSsdSvtId) .eq. 0) then
         DetectorId = kSsdSvtId
         Return

CC   TPC-SSD-SVT tracks:
      else if((D(kTpcId).gt.0 .and. D(kSsdId).gt.0 .and.
     1         D(kSvtId).gt.0)
     2   .or. (D(kTpcId).gt.0 .and. D(kSsdSvtId).gt.0)
     3   .or. (D(kSsdId).gt.0 .and. D(kTpcSvtId).gt.0)
     4   .or. (D(kSvtId).gt.0 .and. D(kTpcSsdId).gt.0)
     5   .or. (D(kTpcSsdSvtId).gt.0)) then
         DetectorId = kTpcSsdSvtId
         Return

      else
         DetectorId = kUnknownId
         Return
      end if

      END
