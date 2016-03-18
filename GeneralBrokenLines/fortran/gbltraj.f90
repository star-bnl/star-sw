!

! Code converted using TO_F90 by Alan Miller
! Date: 2012-03-03  Time: 16:59:41

! C. Kleinwort,  DESY-FH1, www.terascale.de
! March 2011, Analysis Centre: Statistics Tools Group

!> \mainpage General information
!!
!!  \section intro_sec Introduction
!!
!!  For a track with an initial trajectory from a prefit of the
!!  measurements (internal seed) or an external prediction
!!  (external seed) the description of multiple scattering is
!!  added by offsets in a local system. Along the initial
!!  trajectory points are defined with can describe a measurement
!!  or a (thin) scatterer or both. Measurements are arbitrary
!!  functions of the local track parameters at a point (e.g. 2D:
!!  position, 4D: slope+position). The refit provides corrections
!!  to the local track parameters (in the local system) and the
!!  corresponding covariance matrix at any of those points.
!!  Outliers can be down-weighted by use of M-estimators.
!!
!!  The broken lines trajectory is defined by (2D) offsets at the
!!  first and last point and all points with a scatterer. The
!!  prediction for a measurement is obtained by interpolation of
!!  the enclosing offsets and for triplets of adjacent offsets
!!  kink angles are determined. This requires for all points the
!!  jacobians for propagation to the previous and next offset.
!!  These are calculated from the point-to-point jacobians along
!!  the initial trajectory.
!!
!!  Additional local or global parameters can be added and the
!!  trajectories can be written to special binary files for
!!  calibration and alignment with Millepede-II.
!!  (V. Blobel, NIM A, 566 (2006), pp. 5-13).
!!
!!  The conventions for the coordinate systems follow:
!!  Derivation of Jacobians for the propagation of covariance
!!  matrices of track parameters in homogeneous magnetic fields
!!  A. Strandlie, W. Wittek, NIM A, 566 (2006) 687-698.
!!
!!  \section call_sec Calling sequence
!!    -# Initialize trajectory:\n
!!            <tt>CALL gblini(..)</tt>
!!    -# For all points on initial trajectory:
!!        - Create points and add appropriate attributes:\n
!!           - <tt>CALL gbladp(..)</tt>
!!           - <tt>CALL gbladm(..)</tt>
!!           - Add additional local or global parameters to measurement:\n
!!             - <tt>CALL gbladl(..)</tt>
!!             - <tt>CALL gbladg(..)</tt>
!!           - <tt>CALL gblads(..)</tt>
!!    -# Optionally add external seed:\n
!!            <tt>CALL gbladx(..)</tt>
!!    -# Construct and fit trajectory,
!!       get Chi2, Ndf (and weight lost by M-estimators):\n
!!            <tt>CALL gblfit(..)</tt>
!!    -# For any point on initial trajectory:
!!        - Get corrections and covariance matrix for track parameters:\n
!!            <tt>CALL gblres(..)</tt>
!!    -# Optionally write trajectory to MP binary file:\n
!!            <tt>CALL gblmp2(..)</tt>
!!
!!  \section impl_sec Implementation
!!
!!  Linear algebra routines are taken from Millepede-II
!!  (by V. Blobel, University Hamburg).
!!  Only 2D (or 1D) measurements are implemented.
!!
!!  \section ref_sec References
!!    - V. Blobel, C. Kleinwort, F. Meier,
!!      Fast alignment of a complex tracking detector using advanced track models,
!!      Computer Phys. Communications (2011), doi:10.1016/j.cpc.2011.03.017
!!    - C. Kleinwort, General Broken Lines as advanced track fitting method,
!!      NIM A, 673 (2012), 107-110, doi:10.1016/j.nima.2012.01.024
!!
!! \file
!! Trajectory data and procedures.
!!
!! \author Claus Kleinwort, DESY, 2011 (Claus.Kleinwort@desy.de)
!!
!! \copyright
!! Copyright (c) 2011 - 2016 Deutsches Elektronen-Synchroton,
!! Member of the Helmholtz Association, (DESY), HAMBURG, GERMANY \n\n
!! This library is free software; you can redistribute it and/or modify
!! it under the terms of the GNU Library General Public License as
!! published by the Free Software Foundation; either version 2 of the
!! License, or (at your option) any later version. \n\n
!! This library is distributed in the hope that it will be useful,
!! but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!! GNU Library General Public License for more details. \n\n
!! You should have received a copy of the GNU Library General Public
!! License along with this program (see the file COPYING.LIB for more
!! details); if not, write to the Free Software Foundation, Inc.,
!! 675 Mass Ave, Cambridge, MA 02139, USA.

!> Definitions and data fields; construction and fitting.
MODULE gbltraj
    IMPLICIT NONE
    SAVE
    INTEGER, PARAMETER  :: maxFitPar=250  !< max. number of fit parameters
    INTEGER, PARAMETER  :: maxFitSymMatSize=(maxFitPar*maxFitPar+maxFitPar)/2
    INTEGER, PARAMETER  :: maxBandWidth=10   !< max. band width
    INTEGER, PARAMETER  :: maxBorderSize=10   !< max. border size
    INTEGER, PARAMETER  :: maxTrackPar=maxBorderSize+5  !< max. number of local track parameters
    INTEGER, PARAMETER  :: maxTrackSymMatSize=(maxTrackPar*maxTrackPar+maxTrackPar)/2
    INTEGER, PARAMETER  :: maxPoints=1000 !< max. points
    INTEGER, PARAMETER  :: sizeDataBuffer=10*maxPoints !< max. integrated size of data blocks
    !
    INTEGER :: trajLevel   !< level of (preparation of) trajectory (fit)
    INTEGER :: printLevel    !< print level
    INTEGER :: offsetDimension   !< dimension of offsets
                      !! (2: 2D offsets for track in 3D, 1: 1D offsets for track in 2D)
    INTEGER :: numPoints   !< number of points
    INTEGER :: numMeas  !< number (of points) with measurement
    INTEGER :: numScat  !< number (of points) with scatterer
    INTEGER :: numOffsets   !< number (of points) with offsets (as fit parameter)
    INTEGER :: numKinks   !< number of (multiple scattering) kinks
    INTEGER :: lastOffset   !< last point with offset
    INTEGER :: lastScatterer  !< last point with scatterer
    INTEGER :: numFitPar   !< number of fit parameters
    INTEGER :: numCurv   !< flag (0/1) for usage of Q/P as fit parameter
    INTEGER :: bandSize   !< band size
    INTEGER :: numAddLocPar   !< number of additional local track parameters
    INTEGER :: extSeedPoint   !< point with external seed (or 0)
    INTEGER :: firstActiveCoord  !< first active offset coordinate (0: u_1, 1: u_2)
    INTEGER :: lastDownWeightMethod   !< last used down-weighting method
    DOUBLE PRECISION, DIMENSION(maxFitPar)  :: vecb        !< right hand side of linear equation system (A*x=b)
    DOUBLE PRECISION, DIMENSION(maxFitSymMatSize) :: matA  !< (sym.) matrix of linear equation system (A*x=b)
    DOUBLE PRECISION, DIMENSION(maxTrackSymMatSize) :: extSeedMat        !< external seed (precision matrix)
    DOUBLE PRECISION, DIMENSION(maxTrackPar*maxTrackPar) :: jacTrackToFit   !< jacobian for transformation fit to track parameter
    !> Point on trajectory.
    TYPE GblPoint
        INTEGER :: offset       !< offset at point (or from previous point)
        INTEGER :: indMeas      !< measurement flag
        INTEGER :: indScat      !< scatterer flag
        INTEGER :: indKink      !< kink index
        DOUBLE PRECISION, DIMENSION(5,5) :: jacPointToPoint     !< point-to-point jacobian (from previous point)
        DOUBLE PRECISION, DIMENSION(2,2) :: prevJ   !< matrix 'J', offset part of jacobian to previous point with offset
        DOUBLE PRECISION, DIMENSION(2,2) :: prevS   !< matrix 'S', slope part of jacobian to previous point with offset
        DOUBLE PRECISION, DIMENSION(2)   :: prevd   !< vector 'd', Q/P part of jacobian to previous point with offset
        DOUBLE PRECISION, DIMENSION(2,2) :: nextJ   !< matrix 'J', offset part of jacobian to next point with offset
        DOUBLE PRECISION, DIMENSION(2,2) :: nextS   !< matrix 'S', slope part of jacobian to next point with offset
        DOUBLE PRECISION, DIMENSION(2)   :: nextd   !< vector 'd', Q/P part of jacobian to next point with offset
        DOUBLE PRECISION, DIMENSION(2,2) :: matProj !< projection matrix from measurement to local coordinate system
        INTEGER, DIMENSION(2) :: indLocal  !< index of local derivatives
        INTEGER, DIMENSION(2) :: indGlobal !< index of global derivatives
        REAL, DIMENSION(2) :: measValue    !< residual (for measurement)
        REAL, DIMENSION(2) :: measPrec     !< precision (for measurement, 1/sigma**2)
        REAL, DIMENSION(2) :: scatValue    !< residual (for kink)
        REAL, DIMENSION(2) :: scatPrec     !< precision (for kink, 1/sigma**2)
    END TYPE GblPoint
    TYPE(GblPoint), DIMENSION(maxPoints) :: points  !< list of GblPoints

    INTEGER :: numDataBlocks  !< number of data blocks with measurements or kinks
    INTEGER :: integDataSize  !< integrated size of data blocks
    INTEGER :: maxDataSize  !< max. integrated size
    INTEGER, DIMENSION(sizeDataBuffer) :: intData  !< integer part of data blocks (lower part, 1..mdat)
                                       !! or local/global derivatives (upper part, mdat+1..mxdat)
    REAL, DIMENSION(sizeDataBuffer)    :: floatData  !< float part of data blocks or local/global derivatives

    INTEGER :: maxUsedFitPar = 0  !< max. number of fit parameters used
    INTEGER :: lastNumFitPar = 0  !< number of fit parameters from last fit
    INTEGER :: lastBorderSize = 0 !< border size from last fit
    INTEGER :: lastBandWidth = 0  !< band width from last fit
!
CONTAINS

    !> Perform fit of trajectory.
    !!
    !! Optionally iterate for outlier down-weighting.
    !! \param [in] CDW    string defining iterations for outlier down weighting,
    !!                    one char per iteration (C: Cauchy, H: Huber, T: Tukey)
    !! \param [out] MRANK rank of measurements
    !! \param [out] NP    number of track parameter at given point
    !! \param [out] NDF   degrees of freedom
    !! \param [out] CHI2  Chi2
    !! \param [out] WLS   lost measurements: N-sum(weight)
    !!
    SUBROUTINE gblfit(cdw,mrank,np,ndf,chi2,wls)
        IMPLICIT NONE
        REAL    :: als
        INTEGER :: idwm
        INTEGER :: inv
        INTEGER :: iter
        INTEGER :: lcdw
        INTEGER :: nrank
        REAL    :: swgt

        CHARACTER (LEN=*), INTENT(IN)            :: cdw
        INTEGER, INTENT(OUT)                     :: mrank
        INTEGER, INTENT(OUT)                     :: np
        INTEGER, INTENT(OUT)                     :: ndf
        REAL, INTENT(OUT)                        :: chi2
        REAL, INTENT(OUT)                        :: wls

        mrank=0
        np=0
        ndf=-1
        chi2=0.
        wls=0.
        lcdw=len_trim(cdw)
        iter=0

        CALL gblprp
        IF (trajLevel < 3) RETURN

        iterate: DO
            CALL gblmat
            mrank=numCurv+2*offsetDimension ! rank of trajectory
            np   =5+numAddLocPar      ! number of track parameters

            IF (printLevel > 1) PRINT *, ' GBLFIT: NPAR, NBND, NBDR ', numFitPar, bandSize, numCurv+numAddLocPar
            inv=1 ! calc band part of covariance matrix
            CALL sqmibb(matA,vecb,numFitPar,numCurv+numAddLocPar,bandSize,inv,nrank)
            IF (nrank /= numFitPar) THEN ! rank deficit
                IF (printLevel > 1) PRINT *, ' GBLFIT: rank deficit ', nrank, numFitPar
                mrank=-mrank
                chi2=0.
                ndf=0
                als=0.
                RETURN
            END IF

            iter=iter+1
            idwm=0 ! down weighting method (Tukey, Huber, Cauchy)
            IF (iter <= lcdw) idwm=(INDEX('TtHhCc',cdw(iter:iter))+1)/2

            CALL gblch2(idwm,chi2,swgt)
            ndf=numDataBlocks-numFitPar
            ! iterate ?
            IF (idwm <= 0) EXIT
        END DO iterate

        wls=FLOAT(numDataBlocks)-swgt
        trajLevel=4 ! fitted

        RETURN
    END SUBROUTINE gblfit

    !> Prepare broken lines trajectory.
    !!

    SUBROUTINE gblprp
        IMPLICIT NONE
        INTEGER :: i
        INTEGER :: ijsym
        INTEGER :: j

        ! index in symmetric matrix
        ijsym(i,j)=MIN(i,j)+(MAX(i,j)*MAX(i,j)-MAX(i,j))/2

        IF (trajLevel == 1) CALL gblcjc ! calc jacobians to prev/next offset
        IF (trajLevel /= 2) RETURN      ! no points with jacobians added
        IF (numOffsets < 2) RETURN       ! too few parameters
        ! number of parameters
        numAddLocPar=MIN(numAddLocPar,maxBorderSize-numCurv) ! limit number of (external) local par.
        numFitPar=numOffsets*offsetDimension+numCurv+numAddLocPar  ! number of parameters
        IF (numFitPar > maxFitPar) RETURN ! too many parameters

        CALL gbldat

        trajLevel=3 ! prepared

        RETURN
    END SUBROUTINE gblprp

    !> Calculate broken lines from point to point jacobians.
    !!
    SUBROUTINE gblcjc
        IMPLICIT NONE
        
        INTEGER :: iPoint
        INTEGER :: nstep
        INTEGER :: i
        INTEGER :: j
        INTEGER :: lastPoint

        DOUBLE PRECISION :: ajaci(5,5)
        DOUBLE PRECISION :: ajacn(5,5)
        DOUBLE PRECISION :: ajact(5,5)

        !     forward propagation (all)

        nstep=0
        lastPoint=1                  ! last point with offset

        DO iPoint=2,numPoints
            !        full jacobian (to next point)
            ajacn=points(iPoint)%jacPointToPoint

            IF (nstep == 0) THEN  ! start integrated jacobian
                ajaci=ajacn
            ELSE                  ! update integrated jacobian
                FORALL (i=1:5,j=1:5) ajact(i,j)=sum(ajacn(i,:)*ajaci(:,j))
                ajaci=ajact ! MATMUL(ajacn,ajaci) is slower
            END IF
            nstep=nstep+1
            CALL gblinu(ajaci,ajact)
            !        IPNT -> PREV
            points(iPoint)%prevJ=ajact(4:5,4:5) ! J-
            points(iPoint)%prevS=ajact(4:5,2:3) ! S-
            points(iPoint)%prevd=ajact(4:5,1)   ! d-

            IF (points(iPoint)%offset > 0) THEN
                !           LPNT -> NEXT
                points(lastPoint)%nextJ=ajaci(4:5,4:5) ! J+
                points(lastPoint)%nextS=ajaci(4:5,2:3) ! S+
                points(lastPoint)%nextd=ajaci(4:5,1)   ! d+

                nstep=0    ! restart integration
                lastPoint=iPoint
            END IF
        END DO

        !     backward propagation (without scatterers)

        nstep=0

        DO iPoint=numPoints-1,1,-1

            IF (points(iPoint)%offset > 0) THEN ! skip offsets
                nstep=0
                CYCLE
            END IF
            !        full jacobian (to next point)
            ajacn=points(iPoint+1)%jacPointToPoint

            IF (nstep == 0) THEN  ! start integrated jacobian
                ajaci=ajacn
            ELSE                  ! update integrated jacobian
                FORALL (i=1:5,j=1:5) ajact(i,j)=sum(ajaci(i,:)*ajacn(:,j))
                ajaci=ajact ! MATMUL(ajaci,ajacn) is slower
            END IF
            nstep=nstep+1
            !        IPNT -> NEXT
            points(iPoint)%nextJ=ajaci(4:5,4:5) ! J+
            points(iPoint)%nextS=ajaci(4:5,2:3) ! S+
            points(iPoint)%nextd=ajaci(4:5,1)   ! d+
        END DO

        trajLevel=2
        RETURN
    END SUBROUTINE gblcjc

    !> Initialize.
    !!
    !! \param [in] LPRNT  print level
    !! \param [in] ICOORD coordinate (1: u_1, 2: u_2) to use
    !!

    SUBROUTINE gblini(lprnt,icoord)
        IMPLICIT NONE
        INTEGER :: ifirst

        INTEGER, INTENT(IN)                      :: lprnt
        INTEGER, OPTIONAL, INTENT(IN)            :: icoord

        DATA ifirst / 1 /

        offsetDimension=2  ! 2D offsets for track in 3D
        firstActiveCoord=0

        IF (PRESENT(icoord)) THEN
            IF (icoord > 0) THEN
                offsetDimension=1  ! 1D offsets for track in 2D
                firstActiveCoord=0
                IF (icoord > 1) firstActiveCoord=1
            END IF
        END IF

        trajLevel=0
        printLevel=lprnt
        numCurv=1  ! with Q/P
        numAddLocPar=0
        numPoints=0
        lastOffset=0
        numMeas=0
        numScat=0
        lastScatterer=0
        numOffsets=0
        numKinks=0
        maxDataSize=sizeDataBuffer
        extSeedPoint=0  ! external seed parameter offset

        IF (ifirst == 1) THEN
            ifirst=0
            IF (printLevel > 0) PRINT *, ' GBL $Rev: 115 $'
        END IF

        RETURN
    END SUBROUTINE gblini

    !> Add point to trajectory.
    !!
    !! \param [in]  AJAC   jacobian from previous point
    !! \param [out] IPOINT identifier
    !!

    SUBROUTINE gbladp(ajac,ipoint)
        IMPLICIT NONE
        
        DOUBLE PRECISION, INTENT(IN)             :: ajac(5,5)
        INTEGER, INTENT(OUT)                     :: ipoint

        ipoint=0
        IF (numPoints >= maxPoints) THEN
            IF (printLevel > 0) PRINT *, ' GBLADP: too many points, ignoring new point '
            RETURN
        END IF
        numPoints=numPoints+1
        points(numPoints)%offset=0
        points(numPoints)%indMeas=0
        points(numPoints)%indScat=0
        points(numPoints)%indKink=0
        points(numPoints)%indLocal=0
        points(numPoints)%indGlobal=0
        ipoint=numPoints

        points(numPoints)%jacPointToPoint=ajac

        points(numPoints)%prevJ=0.0D0
        points(numPoints)%prevS=0.0D0
        points(numPoints)%prevd=0.0D0
        points(numPoints)%nextJ=0.0D0
        points(numPoints)%nextS=0.0D0
        points(numPoints)%nextd=0.0D0

        IF (lastOffset > 0) THEN
            IF (points(lastOffset)%indScat <= 0.AND.numOffsets > 1) THEN
                numOffsets=numOffsets-1
                points(lastOffset)%offset=-points(lastOffset)%offset
            END IF
        END IF
        numOffsets=numOffsets+1
        points(numPoints)%offset=numOffsets
        lastOffset=numPoints
        IF (numOffsets > 2.AND.lastScatterer > 0) THEN
            numKinks=numKinks+1
            points(lastScatterer)%indKink=numKinks
            lastScatterer=0
        END IF

        trajLevel=1 ! adding points
        RETURN
    END SUBROUTINE gbladp

    !> Add 2D measurement to current point.
    !!
    !! \param [in] PROJ   projection matrix of measurement directions
    !!                    into local system (dm/du)
    !! \param [in] RES    residuals (m)
    !! \param [in] PREC   diagonal of inverse covariance matrix
    !!

    SUBROUTINE gbladm(proj,res,prec)
        IMPLICIT NONE

        DOUBLE PRECISION, INTENT(IN)             :: proj(2,2)
        REAL, INTENT(IN)                         :: res(2)
        REAL, INTENT(IN)                         :: prec(2)

        IF (numPoints <= 0) THEN
            IF (printLevel > 0) PRINT *, ' GBLADM: no point defined'
            RETURN
        END IF
        IF (numMeas+numScat >= maxPoints) THEN
            IF (printLevel > 0) PRINT *,  &
            ' GBLADM: too many measurement+scatterers ', numMeas+numScat
            RETURN
        END IF
        IF (points(numPoints)%indMeas <= 0) THEN
            numMeas=numMeas+1
            points(numPoints)%indMeas=1
            points(numPoints)%matProj=proj
            points(numPoints)%measValue=res
            points(numPoints)%measPrec=prec
        ELSE
            IF (printLevel > 0) PRINT *,  &
            ' GBLADM: measurement already defined for point ', numPoints
        END IF

        RETURN
    END SUBROUTINE gbladm

    !> Add local derivatives to measurement.
    !!
    !! \param [in]  NDER    number of local derivatives
    !! \param [in]  DER     local derivatives
    !! \param [out] IRET    number of non zero derivatives added

    SUBROUTINE gbladl(nder,der,iret)
        IMPLICIT NONE
        INTEGER :: im
        INTEGER :: k
        INTEGER :: mdat0

        INTEGER, INTENT(IN)                      :: nder
        REAL, INTENT(IN)                         :: der(2,nder)
        INTEGER, INTENT(OUT)                     :: iret

        iret=0
        IF (numPoints <= 0) THEN
            IF (printLevel > 0) PRINT *, ' GBLADL: no point defined'
            RETURN
        END IF

        DO im=1,2
  
            IF (points(numPoints)%indLocal(im) == 0) THEN      ! local derivs not yet defined
    
                IF (integDataSize > maxDataSize-nder) RETURN  ! enough space left
                mdat0=maxDataSize
                DO k=nder,1,-1
                    IF (der(im,k) /= 0.0) THEN
                        numAddLocPar=MAX(numAddLocPar,k)
                        intData(maxDataSize)=k
                        floatData(maxDataSize)=der(im,k)
                        maxDataSize=maxDataSize-1
                    END IF
                END DO
    
                points(numPoints)%indLocal(im)=maxDataSize
                intData(maxDataSize)=mdat0-maxDataSize
                floatData(maxDataSize)=0.0
                maxDataSize=maxDataSize-1
                iret=iret+mdat0-maxDataSize
    
            ELSE
                IF (printLevel > 0) PRINT *,  &
                ' GBLADL: local derivatives already defined for point ', numPoints
            END IF
  
        END DO

        RETURN
    END SUBROUTINE gbladl

    !> Add global derivatives to measurement.
    !!
    !! \param [in]  NDER    number of local derivatives
    !! \param [in]  LDER    labels for global derivatives
    !! \param [in]  DER     local derivatives
    !! \param [out] IRET    number of non zero derivatives added

    SUBROUTINE gbladg(nder,lder,der,iret)
        IMPLICIT NONE
        INTEGER :: im
        INTEGER :: k
        INTEGER :: mdat0

        INTEGER, INTENT(IN)                      :: nder
        INTEGER, INTENT(IN)                      :: lder(nder)
        REAL, INTENT(IN)                         :: der(2,nder)
        INTEGER, INTENT(OUT)                     :: iret

        iret=0
        IF (numPoints <= 0) THEN
            IF (printLevel > 0) PRINT *, ' GBLADG: no point defined'
            RETURN
        END IF

        DO im=1,2
  
            IF (points(numPoints)%indGlobal(im) == 0) THEN      ! local derivs not yet defined
    
                IF (integDataSize > maxDataSize-nder) RETURN  ! enough space left
                mdat0=maxDataSize
                DO k=nder,1,-1
                    IF (der(im,k) /= 0.0) THEN
                        intData(maxDataSize)=lder(k)
                        floatData(maxDataSize)=der(im,k)
                        maxDataSize=maxDataSize-1
                    END IF
                END DO
    
                points(numPoints)%indGlobal(im)=maxDataSize
                intData(maxDataSize)=mdat0-maxDataSize
                floatData(maxDataSize)=0.0
                maxDataSize=maxDataSize-1
                iret=iret+mdat0-maxDataSize
    
            ELSE
                IF (printLevel > 0) PRINT *,  &
                ' GBLADG: global derivatives already defined for point ', numPoints
            END IF
  
        END DO

        RETURN
    END SUBROUTINE gbladg

    !> Add (thin) scatterer to current point.
    !!
    !! Changes local track direction.
    !!
    !! \param [in] RES    values for initial kinks (in case of iterating)
    !! \param [in] PREC   diagonal of inverse (multiple scattering)
    !!                    covariance matrix

    SUBROUTINE gblads(res,prec)
        IMPLICIT NONE
        INTEGER :: jscat

        REAL, INTENT(IN)                         :: res(2)
        REAL, INTENT(IN)                         :: prec(2)

        IF (numPoints <= 0) THEN
            IF (printLevel > 0) PRINT *, ' GBLADS: no point defined'
            RETURN
        END IF
        IF (numMeas+numScat >= maxPoints) THEN
            IF (printLevel > 0) PRINT *,  &
            ' GBLADS: too many measurement+scatterers ', numMeas+numScat
            RETURN
        END IF
        IF (prec(1) <= 0.0.OR.prec(2) <= 0.0) THEN
            IF (printLevel > 0) PRINT *, ' GBLADS: invalid scattering precision ', prec
            RETURN
        END IF

        IF (points(numPoints)%indScat <= 0) THEN
            jscat=maxPoints-numScat
            numScat=numScat+1
            points(numPoints)%indScat=1
            lastScatterer=numPoints
            points(numPoints)%scatValue=res
            points(numPoints)%scatPrec=prec
        ELSE
            IF (printLevel > 0) PRINT *, ' GBLADM: scatterer already defined for point ', numPoints
        END IF

        RETURN
    END SUBROUTINE gblads

    !> Dump trajectory definition.

    SUBROUTINE gbldmp
        IMPLICIT NONE
        INTEGER :: i

        IF (numPoints <= 0) THEN
            PRINT *, ' GBLDMP no trajectory defined '
            RETURN
        END IF

        PRINT *
        PRINT *, '    GBLDMP trajectory definition '
        PRINT *, '-------------------------------------'
        PRINT *, ' number of points       ', numPoints
        PRINT *, ' number of offsets      ', numOffsets
        PRINT *, ' number of scatterers   ', numScat
        PRINT *, ' number of measurements ', numMeas
        PRINT *, ' number of kinks        ', numKinks
        IF (numAddLocPar > 0) PRINT *, ' number of local par.   ', numAddLocPar
        PRINT *

        DO i=1,numPoints
            PRINT *, ' Point          ', i
            IF (points(i)%offset > 0)  PRINT *, '    Offset      ', points(i)%offset
            IF (points(i)%indScat > 0) PRINT *, '    Scatterer   ', points(i)%indScat
            IF (points(i)%indMeas > 0) THEN
                IF (points(i)%offset > 0) THEN
                    PRINT *, '    Measurement using offset  ',points(i)%offset
                ELSE
                    PRINT *, '    Measurement using offsets ', &
                    -points(i)%offset-1,' ,',-points(i)%offset
                END IF
            END IF
            IF (points(i)%indKink > 0)  PRINT *, '    Kink        ', points(i)%indKink,  &
            ' using offsets ',points(i)%offset-1,'..',points(i)%offset+1
            IF (points(i)%indMeas <= 0.AND.points(i)%indScat <= 0) THEN
                IF (points(i)%offset > 0) THEN
                    PRINT *, '    Prediction               ', ' using offset  ',points(i)%offset
      
                ELSE
                    PRINT *, '    Prediction               ',  &
                    ' using offsets ',-points(i)%offset-1,' ,',-points(i)%offset
                END IF
            END IF
        END DO

        RETURN
    END SUBROUTINE gbldmp

    !> Generate DATa record.

    SUBROUTINE gbldat
        IMPLICIT NONE
        INTEGER :: i
        INTEGER :: idx
        INTEGER :: ilcl
        INTEGER :: ioff1
        INTEGER :: ip0
        INTEGER :: ipar0
        INTEGER :: ipmn
        INTEGER :: ipoff
        INTEGER :: j
        INTEGER :: jmeas
        INTEGER :: joff
        INTEGER :: jpnt
        INTEGER :: jscat
        INTEGER :: k
        INTEGER :: kdat
        INTEGER :: kdat0
        INTEGER :: l
        INTEGER :: l0
        INTEGER :: ldat0
        INTEGER :: ltem
        INTEGER :: m
        INTEGER :: nb
        INTEGER :: nlcl
        INTEGER :: nplc
        INTEGER :: npsd

        DOUBLE PRECISION :: tc2l(2,2)
        DOUBLE PRECISION :: wp(2,2)
        DOUBLE PRECISION :: wn(2,2)
        DOUBLE PRECISION :: wjp(2,2)
        DOUBLE PRECISION :: wjn(2,2)
        DOUBLE PRECISION :: dwp(2)
        DOUBLE PRECISION :: dwn(2)
        DOUBLE PRECISION :: wjs(2,2)
        DOUBLE PRECISION :: wji(2,2)
        DOUBLE PRECISION :: pn(2,2)
        DOUBLE PRECISION :: wpp(2,2)
        DOUBLE PRECISION :: wpn(2,2)
        DOUBLE PRECISION :: dws(2)
        DOUBLE PRECISION :: dps(2)
        DOUBLE PRECISION :: det
        DOUBLE PRECISION :: diag(maxTrackPar)
        DOUBLE PRECISION :: eigen(maxTrackPar*maxTrackPar)
        DOUBLE PRECISION :: work(maxTrackPar)
        DOUBLE PRECISION :: seed(maxTrackPar*maxTrackPar)
        INTEGER          :: iwork(maxTrackPar)

        l0=firstActiveCoord

        lastDownWeightMethod=0 ! start without downweighting
        ! loop over points
        numDataBlocks=0 ! number of data items
        integDataSize=0 ! length of data
        ipmn=numFitPar+1 ! minimum parameter with non zero derivatives
        DO jpnt=1,numPoints
  
            joff=points(jpnt)%offset               ! offset at point
            ipar0=offsetDimension*(joff-1)+numCurv+numAddLocPar ! offset in parameter number
            ! kink at point ?
            IF (points(jpnt)%indKink > 0) THEN
                jscat=points(jpnt)%indScat
    
                CALL gblder(jpnt,-1,wp,wjp,dwp)
                CALL gblder(jpnt, 2,wn,wjn,dwn)
                dws=dwp+dwn ! W- * d- + W+ * d+
                wjs=wjp+wjn ! W- * J- + W+ * J+
    
                DO m=1,offsetDimension ! pseudo measurements
                    kdat=maxDataSize-(numCurv+3*offsetDimension)
                    IF (integDataSize+3 > kdat) RETURN
                    kdat0=kdat
      
                    ldat0=integDataSize
                    numDataBlocks=numDataBlocks +1
                    intData(integDataSize+1)=3             ! length of item
                    intData(integDataSize+2)=jpnt          ! point
                    intData(integDataSize+3)=-m            ! pseudo measurement
                    floatData(integDataSize+1)=points(jpnt)%scatValue(m) ! (start) value
                    floatData(integDataSize+2)=points(jpnt)%scatPrec(m)  ! precision (1/error**2)
                    floatData(integDataSize+3)=1.0         ! (additional) weight
                    integDataSize=integDataSize+3
      
                    intData(kdat+1)= 1
                    floatData(kdat+1)= -SNGL(dws(m))
                    kdat=kdat+numCurv
                    DO l=1,offsetDimension
                        idx=kdat+l
                        intData(idx)= ipar0+l-offsetDimension
                        floatData(idx)= SNGL(  wp(m,l+l0))
                        idx=idx+offsetDimension
                        intData(idx)= ipar0+l
                        floatData(idx)= SNGL(-wjs(m,l+l0))
                        idx=idx+offsetDimension
                        intData(idx)= ipar0+l+offsetDimension
                        floatData(idx)= SNGL(  wn(m,l+l0))
                    END DO
                    kdat=kdat+3*offsetDimension

                    DO k=kdat0+1,kdat
                        IF (floatData(k) /= 0.0) THEN ! copy non zero derivatives
                            integDataSize=integDataSize+1
                            intData(integDataSize)=intData(k)
                            floatData(integDataSize)=floatData(k)
                            ipmn=MIN(ipmn,intData(integDataSize))
                        END IF
                    END DO
                    intData(ldat0+1)=integDataSize-ldat0
                END DO

            END IF
            ! measurement at point ?
            jmeas=points(jpnt)%indMeas        ! measurement at point
            IF (jmeas > 0) THEN
                tc2l=points(jpnt)%matProj !  P
                ipar0=offsetDimension*(joff-1)+numCurv+numAddLocPar       ! offset in parameter number
                IF (joff < 0) THEN                 ! need interpolation
                    ipar0=offsetDimension*(-joff-2)+numCurv+numAddLocPar   ! offset in parameter number
                    CALL gblder(jpnt,-1,wp,wjp,dwp)
                    CALL gblder(jpnt, 2,wn,wjn,dwn)
                    dws=dwp+dwn !  W- * d- + W+ * d+
                    wjs=wjp+wjn !  W- * J- + W+ * J+
                    det=wjs(1,1)*wjs(2,2)-wjs(1,2)*wjs(2,1)              ! (W- * J- + W+ * J+)^-1 (=N)
                    wji(1,1)= wjs(2,2)/det;wji(1,2)=-wjs(1,2)/det
                    wji(2,1)=-wjs(2,1)/det;wji(2,2)= wjs(1,1)/det
                    FORALL (i=1:2,j=1:2) pn(i,j)=sum(tc2l(i,:)*wji(:,j)) !  P * N
                    FORALL (i=1:2,j=1:2) wpp(i,j)=sum(pn(i,:)*wp(:,j))   !  P * N * W-
                    FORALL (i=1:2,j=1:2) wpn(i,j)=sum(pn(i,:)*wn(:,j))   !  P * N * W+
                    FORALL (i=1:2) dps(i)=sum(dws(:)*pn(i,:))            !  P * N * (W+ * d+ + W- * d-)

                END IF

                DO m=1,2
                    IF (points(jpnt)%measPrec(m) <= 0.0) CYCLE ! no precision ?
                    kdat=maxDataSize-(numCurv+2*offsetDimension)
                    IF (integDataSize+3 > kdat) RETURN
                    kdat0=kdat

                    ldat0=integDataSize
                    numDataBlocks=numDataBlocks+1
                    intData(integDataSize+1)=3               ! length of item
                    intData(integDataSize+2)=jpnt            ! point
                    intData(integDataSize+3)=m               ! measurement
                    floatData(integDataSize+1)=points(jpnt)%measValue(m)  ! value
                    floatData(integDataSize+2)=points(jpnt)%measPrec(m)   ! precision (1/error**2)
                    floatData(integDataSize+3)=1.0             ! (additional) weight
                    integDataSize=integDataSize+3

                    IF (joff > 0) THEN         ! measurement at offset
                        DO l=1,offsetDimension
                            intData(kdat+1)= ipar0+l
                            floatData(kdat+1)= SNGL(tc2l(m,l+l0))
                            kdat=kdat+1
                        END DO
                    ELSE                         ! interpolation between offsets

                        intData(kdat+1)= 1
                        floatData(kdat+1)= -SNGL(dps(m))
                        kdat=kdat+numCurv
                        DO l=1,offsetDimension
                            idx=kdat+l
                            intData(idx)= ipar0+l
                            floatData(idx)= SNGL(wpp(m,l+l0))
                            idx=idx+offsetDimension
                            intData(idx)= ipar0+l+offsetDimension
                            floatData(idx)= SNGL(wpn(m,l+l0))
                        END DO
                        kdat=kdat+2*offsetDimension
                    END IF
                    DO k=kdat0+1,kdat
                        IF (floatData(k) /= 0.0) THEN ! copy non zero derivatives
                            integDataSize=integDataSize+1
                            intData(integDataSize)=intData(k)
                            floatData(integDataSize)=floatData(k)
                            ipmn=MIN(ipmn,intData(integDataSize))
                        END IF
                    END DO
                    intData(ldat0+1)=integDataSize-ldat0
                    ! check for local derivatives
                    ilcl=points(jpnt)%indLocal(m)
                    IF (ilcl <= 0) CYCLE
                    nlcl=intData(ilcl)
                    IF (integDataSize+nlcl > maxDataSize) CYCLE
                    DO k=1,nlcl
                        IF (intData(ilcl+k) <= numAddLocPar) THEN
                            integDataSize=integDataSize+1
                            intData(integDataSize)=intData(ilcl+k)+numCurv
                            floatData(integDataSize)=floatData(ilcl+k)
                            ipmn=MIN(ipmn,intData(integDataSize))
                        END IF
                    END DO
                    intData(ldat0+1)=integDataSize-ldat0
                END DO
    
            END IF
  
        END DO

        IF (extSeedPoint /= 0) THEN
            IF (extSeedMat(1) > 0.0D0) ipmn=1 ! external 'seed' for curvature

            nb=numCurv+numAddLocPar
            npsd=nb+2*offsetDimension      ! fit parameters
            nplc=5+numAddLocPar         ! local track parameters

            CALL gbljac(extSeedPoint,1,ioff1)  ! get transposed jacobian broken lines -> local
            CALL devrot(nplc,diag,eigen,extSeedMat,work,iwork)
            FORALL (i=1:nplc,j=1:npsd) seed((j-1)*nplc+i)=sum(eigen((i-1)*nplc+1:i*nplc) &
            *jacTrackToFit((j-1)*nplc+1:j*nplc)) ! eigen^T * jacTrackToFit
            ip0=(ioff1-1)*offsetDimension

            DO i=1, nplc
                IF (diag(i) > 0.0) THEN
                    IF (integDataSize+3+npsd > maxDataSize) RETURN
                    ldat0=integDataSize
                    numDataBlocks=numDataBlocks+1            ! 'virtual' measurement
                    intData(integDataSize+1)=3               ! length of item
                    intData(integDataSize+2)=extSeedPoint    ! point
                    intData(integDataSize+3)=i               ! measurement
                    floatData(integDataSize+1)=0.0           ! value
                    floatData(integDataSize+2)=SNGL(diag(i)) ! precision (1/error**2)
                    floatData(integDataSize+3)=1.0           ! (additional) weight
                    integDataSize=integDataSize+3
                    DO j=1,npsd
                        IF (seed((j-1)*nplc+i) /= 0.0) THEN
                            integDataSize=integDataSize+1
                            IF (j > nb) THEN
                                intData(integDataSize)=j+ip0
                            ELSE
                                intData(integDataSize)=j
                            END IF
                            floatData(integDataSize)=SNGL(seed((j-1)*nplc+i))
                        END IF
                    END DO
                    intData(ldat0+1)=integDataSize-ldat0
                END IF
            END DO
        END IF

        !     check minimum parameter with non zero derivatives
        IF (ipmn > numCurv) THEN          ! curvature undefined
            ipoff=numCurv
            numCurv=0
            !        correct parameter indices
            IF (ipoff > 0) THEN
                numFitPar=numFitPar-ipoff
                integDataSize=0
                DO i=1,numDataBlocks
                    ltem=intData(integDataSize+1)
                    FORALL (j=integDataSize+4:integDataSize+ltem) intData(j)=intData(j)-ipoff
                    integDataSize=integDataSize+ltem
                END DO
    
            END IF
        END IF
        RETURN
    END SUBROUTINE gbldat

    !> get (matrices and vectors for) derivatives.
    !!
    !! \param [in]  IPOINT   point
    !! \param [in]  IDIR     direction
    !! \param [out] W        W (=(+/-)S^-1)
    !! \param [out] WJ       W*J
    !! \param [out] DW       W*d

    SUBROUTINE gblder(ipoint,idir,w,wj,dw)
        IMPLICIT NONE

        INTEGER, INTENT(IN OUT)                  :: ipoint
        INTEGER, INTENT(IN)                      :: idir
        DOUBLE PRECISION, INTENT(OUT)            :: w(2,2)
        DOUBLE PRECISION, INTENT(OUT)            :: wj(2,2)
        DOUBLE PRECISION, INTENT(OUT)            :: dw(2)

        DOUBLE PRECISION :: jm(2,2)
        DOUBLE PRECISION :: sm(2,2)
        DOUBLE PRECISION :: dv(2)
        DOUBLE PRECISION :: det
        INTEGER :: i
        INTEGER :: j

        ! (parts of) jacobians to previous/next offset
        IF (idir < 0) THEN
            jm=points(ipoint)%prevJ
            sm=-points(ipoint)%prevS
            dv=points(ipoint)%prevd
        ELSE
            jm=points(ipoint)%nextJ
            sm=points(ipoint)%nextS
            dv=points(ipoint)%nextd
        ENDIF

        det=sm(1,1)*sm(2,2)-sm(1,2)*sm(2,1)               ! W
        w(1,1)= sm(2,2)/det;w(1,2)=-sm(1,2)/det
        w(2,1)=-sm(2,1)/det;w(2,2)= sm(1,1)/det
        FORALL (i=1:2,j=1:2) wj(i,j)=sum(w(i,:)*jm(:,j))  ! W*J
        FORALL (i=1:2) dw(i)=sum(dv(:)*w(i,:))            ! W*d

        RETURN
    END SUBROUTINE gblder

    !> Build MATrix and rhs vector.

    SUBROUTINE gblmat
        IMPLICIT NONE
        INTEGER :: i
        INTEGER :: ij
        INTEGER :: ij0
        INTEGER :: ijsym
        INTEGER :: ik
        INTEGER :: j
        INTEGER :: jk
        INTEGER :: k
        INTEGER :: ltem
        INTEGER :: mpar2
        INTEGER :: nbdr
        INTEGER :: npar2

        DOUBLE PRECISION :: dval
        DOUBLE PRECISION :: dwgh

        ! index in symmetric matrix
        ijsym(i,j)=(i*i-i)/2+j

        vecb(1:numFitPar)=0.0D0
        !     'smart' clear
        IF (numFitPar > maxUsedFitPar) THEN
            mpar2=(maxUsedFitPar*maxUsedFitPar+maxUsedFitPar)/2
            matA(mpar2+1:npar2)=0.0D0
            maxUsedFitPar=numFitPar
        END IF

        IF (lastNumFitPar > 0) THEN
            DO i=1,lastNumFitPar
                ij0=(i*i-i)/2
                FORALL (j=1:MIN(lastBorderSize,i)) matA(ij0+j)=0.0D0 ! clear border
                FORALL (j=1:MAX(i-lastBandWidth,1):i) matA(ij0+j)=0.0D0 ! clear band
            END DO
        END IF

        integDataSize=0
        nbdr=numCurv+numAddLocPar
        DO i=1,numDataBlocks
            ltem=intData(integDataSize+1)
            dval=DBLE(floatData(integDataSize+1))              ! value
            dwgh=DBLE(floatData(integDataSize+3)*floatData(integDataSize+2)) ! (total) weight
            DO j=integDataSize+4,integDataSize+ltem ! update matrix
                ij=intData(j)
                vecb(ij)=vecb(ij)+DBLE(floatData(j))*dval*dwgh
                DO k=integDataSize+4,j
                    ik=intData(k)
                    jk=ijsym(ij,ik) ! parameter labels must be sorted: IK<=IJ !
                    matA(jk)=matA(jk)+DBLE(floatData(j))*DBLE(floatData(k))*dwgh
                    IF (ik > nbdr) bandSize=MAX(bandSize,ij-ik) ! update band width
                END DO
            END DO
            integDataSize=integDataSize+ltem
        END DO

        lastNumFitPar=numFitPar
        lastBorderSize=nbdr
        lastBandWidth=bandSize

        RETURN
    END SUBROUTINE gblmat

    !> Calculate Chi2.
    !!
    !! \param [in]  IDWM  down-weighting method (0-3)
    !! \param [out] CHI2  Chi2
    !! \param [out] SWGT  sum of weights

    SUBROUTINE gblch2(idwm,chi2,swgt)
        IMPLICIT NONE
        INTEGER :: i
        INTEGER :: j
        INTEGER :: ltem
        REAL :: brl
        REAL :: dwint
        REAL :: sres
        REAL :: val
        REAL :: wgh

        INTEGER, INTENT(IN)                      :: idwm
        REAL, INTENT(OUT)                        :: chi2
        REAL, INTENT(OUT)                        :: swgt

        DIMENSION dwint(0:3) ! Integral(weight*normal_distribution)
        DATA dwint / 1.0, 0.8737, 0.9326, 0.8228 /

        chi2=0.0
        swgt=0.0

        integDataSize=0
        DO i=1,numDataBlocks
            ltem=intData(integDataSize+1)
            val=floatData(integDataSize+1)      ! value
            wgh=floatData(integDataSize+3)      ! down weighting
            brl=0.0               ! predction from broken lines
            DO j=integDataSize+4,integDataSize+ltem
                brl=brl+SNGL(vecb(intData(j))*floatData(j))
            ENDDO
            sres=ABS(brl-val)*SQRT(floatData(integDataSize+2))
            chi2=chi2+sres*sres*wgh
            swgt=swgt+wgh
            !       down weighting
            IF (idwm == 1) THEN
                IF(sres < 4.6851) THEN          ! Tukey
                    wgh=(1.0-0.045558*sres*sres)**2  ! 1/4.6851**2
                ELSE
                    wgh=0.0
                END IF
            ELSE IF (idwm == 2) THEN
                IF (sres < 1.345) THEN          ! Huber
                    wgh=1.0
                ELSE
                    wgh=1.345/sres
                END IF
            ELSE IF (idwm == 3) THEN
                wgh=1.0/(1.0+(sres/2.3849)**2)   ! Cauchy
            END IF
            floatData(integDataSize+3)=wgh
            integDataSize=integDataSize+ltem
        END DO

        chi2=chi2/dwint(lastDownWeightMethod) ! renormalize Chi2
        lastDownWeightMethod=idwm

        RETURN
    END SUBROUTINE gblch2

    !> generate Millepede-II record
    !!
    !! \param [out] IRET  number MillePede measurements in record

    SUBROUTINE gblmp2(iret)
        IMPLICIT NONE
        INTEGER :: i
        INTEGER :: igbl
        INTEGER :: im
        INTEGER :: ipnt
        INTEGER :: j
        INTEGER :: ltem
        INTEGER :: ngbl
        REAL :: sig
        REAL :: derlc

        INTEGER, INTENT(OUT)                     :: iret

        DIMENSION derlc(maxFitPar)

        iret=0
        CALL gblprp
        IF (trajLevel < 3) RETURN

        integDataSize=0
        DO i=1,numDataBlocks
            ltem=intData(integDataSize+1)
            ipnt=intData(integDataSize+2)
            im  =intData(integDataSize+3)
            ! local derivatives
            derlc(1:numFitPar)=0.0
            FORALL (j=integDataSize+4:integDataSize+ltem) derlc(intData(j))=floatData(j)
            igbl=0
            ngbl=0
            IF (im > 0) igbl=points(ipnt)%indGlobal(im)
            IF (igbl > 0) ngbl=intData(igbl)
            sig=1.0/SQRT(floatData(integDataSize+2))
  
            CALL mille(numFitPar,derlc,ngbl,floatData(igbl+1),intData(igbl+1),  &
            floatData(integDataSize+1),sig) ! add data
  
            integDataSize=integDataSize+ltem
        END DO

        CALL endle ! complete, write record (many sets)
        iret=numDataBlocks

        RETURN
    END SUBROUTINE gblmp2

    !> Get jacobian (transposed).
    !! broken lines parameter (q/p,..,u_i,..) to local parameter (q/p,alpha,u)
    !!
    !! \param [in]  IPOINT  (signed) point
    !! \param [in]  ITRANS  =0 not transposed, =1 transposed
    !! \param [out] IOFF1   offsets IOFF1,IOFF1+1 needed
    !!                      to define offset and slope at IPOINT

    SUBROUTINE gbljac(ipoint,itrans,ioff1)
        IMPLICIT NONE
        INTEGER :: i
        INTEGER :: idir
        INTEGER :: indx
        INTEGER :: ioff2
        INTEGER :: ip1
        INTEGER :: ip2
        INTEGER :: j
        INTEGER :: joff
        INTEGER :: jpnt
        INTEGER :: k
        INTEGER :: koff
        INTEGER :: l
        INTEGER :: l0
        INTEGER :: mp
        INTEGER :: np

        INTEGER, INTENT(IN)                      :: ipoint
        INTEGER, INTENT(IN)                      :: itrans
        INTEGER, INTENT(OUT)                     :: ioff1

        DOUBLE PRECISION :: w(2,2)
        DOUBLE PRECISION :: wj(2,2)
        DOUBLE PRECISION :: dw(2)
        DOUBLE PRECISION :: wp(2,2)
        DOUBLE PRECISION :: wjp(2,2)
        DOUBLE PRECISION :: wn(2,2)
        DOUBLE PRECISION :: wjn(2,2)
        DOUBLE PRECISION :: dwp(2)
        DOUBLE PRECISION :: dwn(2)
        DOUBLE PRECISION :: wjs(2,2)
        DOUBLE PRECISION :: wji(2,2)
        DOUBLE PRECISION :: wip(2,2)
        DOUBLE PRECISION :: win(2,2)
        DOUBLE PRECISION :: dip(2)
        DOUBLE PRECISION :: din(2)
        DOUBLE PRECISION :: wpp(2,2)
        DOUBLE PRECISION :: wpn(2,2)
        DOUBLE PRECISION :: dpp(2)
        DOUBLE PRECISION :: dpn(2)
        DOUBLE PRECISION :: det
        DOUBLE PRECISION :: sgn

        indx(i,j)=((i-1)*np+j)*(1-itrans)+((j-1)*mp+i)*itrans

        np=numCurv+numAddLocPar+2*offsetDimension
        mp=5+numAddLocPar
        jpnt=IABS(ipoint)

        joff=points(jpnt)%offset

        jacTrackToFit(1:mp*np)=0.0D0 ! reset Jacobi matrix

        l0=firstActiveCoord
        IF (joff > 0) THEN
            ! at offset
            IF (ipoint > 0) THEN ! forward
                idir=2
                IF (joff == numOffsets) idir=-1
            ELSE                  ! backward
                idir=-1
                IF (joff == 1) idir=2
            END IF
            koff=joff+2*IABS(idir)-3 ! 2nd offset for slope measurement
            CALL gblder(jpnt,idir,w,wj,dw)
  
            IF (idir > 0) THEN
                ioff1=joff
                ioff2=koff
                ip1=numCurv+numAddLocPar
                ip2=numCurv+numAddLocPar+offsetDimension
                sgn=1.0
            ELSE
                ioff2=joff
                ioff1=koff
                ip2=numCurv+numAddLocPar
                ip1=numCurv+numAddLocPar+offsetDimension
                sgn=-1.0
            END IF
  
            DO l=1,offsetDimension
                jacTrackToFit(indx(2  ,ip1+l))=-sgn*wj(1,l+l0)
                jacTrackToFit(indx(3  ,ip1+l))=-sgn*wj(2,l+l0)
                jacTrackToFit(indx(2  ,ip2+l))= sgn*w(1,l+l0)
                jacTrackToFit(indx(3  ,ip2+l))= sgn*w(2,l+l0)
                jacTrackToFit(indx(3+l,ip1+l))= 1.0D0
            END DO
            IF (numCurv > 0) THEN                        ! correct for curvature
                jacTrackToFit(indx(2,1)) = -sgn*dw(1)
                jacTrackToFit(indx(3,1)) = -sgn*dw(2)
            END IF
  
        ELSE
            ! between offsets
            CALL gblder(jpnt,-1,wp,wjp,dwp)
            CALL gblder(jpnt, 2,wn,wjn,dwn)
            ioff1=-joff-1
            ioff2=-joff
            ip1=numCurv+numAddLocPar
            ip2=numCurv+numAddLocPar+offsetDimension
  
            wjs=wjp+wjn                                          !  W- * J- + W+ * J+
            det=wjs(1,1)*wjs(2,2)-wjs(1,2)*wjs(2,1)              ! (W- * J- + W+ * J+)^-1 (=N)
            wji(1,1)= wjs(2,2)/det;wji(1,2)=-wjs(1,2)/det
            wji(2,1)=-wjs(2,1)/det;wji(2,2)= wjs(1,1)/det
            !        derivatives for u_int
            FORALL (i=1:2,j=1:2) wip(i,j)=sum(wji(i,:)*wp(:,j))  !  N * W-
            FORALL (i=1:2,j=1:2) win(i,j)=sum(wji(i,:)*wn(:,j))  !  N * W+
            FORALL (i=1:2) dip(i)=sum(wji(i,:)*dwp(:))           !  N * W- * d-
            FORALL (i=1:2) din(i)=sum(wji(i,:)*dwn(:))           !  N * W+ * d+
            !        derivatives for alpha_int
            FORALL (i=1:2,j=1:2) wpp(i,j)=sum(wjn(i,:)*wip(:,j)) !  W+ * J+ * N * W-
            FORALL (i=1:2,j=1:2) wpn(i,j)=sum(wjp(i,:)*win(:,j)) !  W- * J- * N * W+
            FORALL (i=1:2) dpp(i)=sum(wjn(i,:)*dip(:))           !  W+ * J+ * N * W- * d-
            FORALL (i=1:2) dpn(i)=sum(wjp(i,:)*din(:))           !  W- * J- * N * W+ * d+

            DO l=1,offsetDimension
                !           du_int/du-
                jacTrackToFit(indx(4,ip1+l))= wip(1,l+l0)
                jacTrackToFit(indx(5,ip1+l))= wip(2,l+l0)
                !           du_int/du+
                jacTrackToFit(indx(4,ip2+l))= win(1,l+l0)
                jacTrackToFit(indx(5,ip2+l))= win(2,l+l0)
                !           dalpha_int/du-
                jacTrackToFit(indx(2,ip1+l))=-wpp(1,l+l0)
                jacTrackToFit(indx(3,ip1+l))=-wpp(2,l+l0)
                !           dalpha_int/du+
                jacTrackToFit(indx(2,ip2+l))= wpn(1,l+l0)
                jacTrackToFit(indx(3,ip2+l))= wpn(2,l+l0)
            END DO
            IF (numCurv > 0) THEN              ! correct for curvature
                !           du_int/dQbyP
                jacTrackToFit(indx(4,1)) =-dip(1)-din(1)
                jacTrackToFit(indx(5,1)) =-dip(2)-din(2)
                !           dalpha_int/dQbyP
                jacTrackToFit(indx(2,1)) = dpp(1)-dpn(1)
                jacTrackToFit(indx(3,1)) = dpp(2)-dpn(2)
            END IF
  
        END IF

        IF (numCurv > 0) THEN   ! curvature
            jacTrackToFit(indx(1,1))=1.0D0
        END IF

        DO k=1, numAddLocPar
            jacTrackToFit(indx(k+5,numCurv+k))= 1.0D0 ! local parameters
        END DO

        RETURN
    END SUBROUTINE gbljac

    !> Get parameters and covariance matrix at point.
    !!
    !! \param [in]  IPOINT  (signed) point
    !!                      (<0: side towards previous point,
    !!                       >0: side towards next point)
    !! \param [out] DPAR    corrections (NP double precision values)
    !!                      (NP is number of track parameters: 5 + local par.)
    !! \param [out] DCOV    covariance matrix (NP2 double precision values,
    !!                      symmetric storage mode, NP2=(NP+1)*NP/2)

    SUBROUTINE gblres(ipoint,dpar,dcov)
        IMPLICIT NONE
        INTEGER :: i
        INTEGER :: io
        INTEGER :: ioff1
        INTEGER :: ip0
        INTEGER :: j
        INTEGER :: jpnt
        INTEGER :: k
        INTEGER :: ko
        INTEGER :: mp
        INTEGER :: mp2
        INTEGER :: nb
        INTEGER :: nb2
        INTEGER :: np

        INTEGER, INTENT(IN OUT)                  :: ipoint
        DOUBLE PRECISION, INTENT(OUT)            :: dpar(*)
        DOUBLE PRECISION, INTENT(OUT)            :: dcov(*)

        DOUBLE PRECISION :: caux(maxTrackSymMatSize)
        DOUBLE PRECISION :: baux(maxTrackPar)

        nb=numCurv+numAddLocPar
        nb2=(nb*nb+nb)/2
        np=nb+2*offsetDimension
        mp=5+numAddLocPar
        mp2=(mp*mp+mp)/2

        jpnt=IABS(ipoint)
        IF (jpnt < 1.OR.jpnt > numPoints) THEN
            IF (printLevel > 0) PRINT *, ' GBLRES invalid point ', ipoint
            dpar(1:mp)=0.0D0
            dcov(1:mp2)=0.0D0
            RETURN
        END IF

        IF (trajLevel < 4) RETURN ! fit not yet performed or failed

        CALL gbljac(ipoint,0,ioff1) ! get jacobian broken lines -> local
        !     get compressed covariance matrix, result vector
        baux(1:nb)=vecb(1:nb)  ! border
        caux(1:nb2)=matA(1:nb2) ! border
        ip0=(ioff1-1)*offsetDimension
        k=nb2
        DO i=nb+1,np
            io=i+ip0
            ko=(io*io-io)/2
            baux(i)=vecb(io) ! band part
            DO j=1,nb        ! mixed part
                k=k+1
                caux(k)=matA(ko+j)
            END DO
            ko=ko+ip0
            DO j=nb+1,i      ! band part
                k=k+1
                caux(k)=matA(ko+j)
            END DO
        END DO

        CALL dbgax(jacTrackToFit,baux,dpar,mp,np)
        CALL dbavat(caux,jacTrackToFit,dcov,np,mp)

        RETURN
    END SUBROUTINE gblres

    !> Add (inverse covariance matrix from) external seed.
    !!
    !! \param [in]  IPOINT  (signed) point
    !!                      (<0: side towards previous point,
    !!                       >0: side towards next point)
    !! \param [in]  DPRC    precision matrix (inverse covariance) from
    !!                      external seed (NP2 double precision values,
    !!                      symmetric storage mode, NP2=(NP+1)*NP/2,
    !!                      NP is number of track parameters: 5 + local par.)

    SUBROUTINE gbladx(ipoint,dprc)
        IMPLICIT NONE
        INTEGER :: jpnt
        INTEGER :: mp
        INTEGER :: mp2

        INTEGER, INTENT(IN)                      :: ipoint
        DOUBLE PRECISION, INTENT(IN)             :: dprc(*)


        jpnt=IABS(ipoint)
        IF (jpnt < 1.OR.jpnt > numPoints) THEN
            IF (printLevel > 0) PRINT *, ' GBLADX invalid point ', ipoint
            RETURN
        END IF

        IF (trajLevel >= 3) RETURN ! fit already prepared or performed

        mp=5+numAddLocPar
        mp2=(mp*mp+mp)/2
        extSeedPoint=ipoint
        extSeedMat(1:mp2)=dprc(1:mp2)

        RETURN
    END SUBROUTINE gbladx

    !> Calculate offset part of inverse jacobian.
    !!
    !! \param [in]  A (5*5) matrix A
    !! \param [out] B (5*5) matrix B with last 2 rows of inverse(A)
    !!
    SUBROUTINE gblinu(a,b)
        IMPLICIT NONE

        DOUBLE PRECISION, INTENT(IN)             :: a(5,5)
        DOUBLE PRECISION, INTENT(OUT)            :: b(5,5)
        DOUBLE PRECISION :: a33(3,3)
        DOUBLE PRECISION :: a23(2,3)
        DOUBLE PRECISION :: a22(2,2)
        DOUBLE PRECISION :: det

        !     A33 = A(1..3,1..3)^-1 * det(A(1..3,1..3)
        a33(1,1) = a(2,2)*a(3,3)-a(2,3)*a(3,2)
        a33(2,1) = a(2,3)*a(3,1)-a(2,1)*a(3,3)
        a33(3,1) = a(2,1)*a(3,2)-a(2,2)*a(3,1)
        a33(1,2) = a(1,3)*a(3,2)-a(1,2)*a(3,3)
        a33(2,2) = a(1,1)*a(3,3)-a(1,3)*a(3,1)
        a33(3,2) = a(1,2)*a(3,1)-a(1,1)*a(3,2)
        a33(1,3) = a(1,2)*a(2,3)-a(1,3)*a(2,2)
        a33(2,3) = a(1,3)*a(2,1)-a(1,1)*a(2,3)
        a33(3,3) = a(1,1)*a(2,2)-a(1,2)*a(2,1)
        det=a(1,1)*a33(1,1)+a(1,2)*a33(2,1)+a(1,3)*a33(3,1)
        !     A23 = A(4..5,1..3) * A33 / det(A(1..3,1..3))
        a23(1,1) = (a(4,1)*a33(1,1)+a(4,2)*a33(2,1)+a(4,3)*a33(3,1))/det
        a23(1,2) = (a(4,1)*a33(1,2)+a(4,2)*a33(2,2)+a(4,3)*a33(3,2))/det
        a23(1,3) = (a(4,1)*a33(1,3)+a(4,2)*a33(2,3)+a(4,3)*a33(3,3))/det
        a23(2,1) = (a(5,1)*a33(1,1)+a(5,2)*a33(2,1)+a(5,3)*a33(3,1))/det
        a23(2,2) = (a(5,1)*a33(1,2)+a(5,2)*a33(2,2)+a(5,3)*a33(3,2))/det
        a23(2,3) = (a(5,1)*a33(1,3)+a(5,2)*a33(2,3)+a(5,3)*a33(3,3))/det
        !     A22 = A(4..5,4..5) - A23 * A((1..3,4..5)
        a22(1,1) = a(4,4)-a23(1,1)*a(1,4)-a23(1,2)*a(2,4)-a23(1,3)*a(3,4)
        a22(1,2) = a(4,5)-a23(1,1)*a(1,5)-a23(1,2)*a(2,5)-a23(1,3)*a(3,5)
        a22(2,1) = a(5,4)-a23(2,1)*a(1,4)-a23(2,2)*a(2,4)-a23(2,3)*a(3,4)
        a22(2,2) = a(5,5)-a23(2,1)*a(1,5)-a23(2,2)*a(2,5)-a23(2,3)*a(3,5)
        !     (4..5,4..5) of A^-1 = A22^-1
        det = a22(1,1)*a22(2,2)-a22(1,2)*a22(2,1)
        b(4,4) = a22(2,2)/det
        b(4,5) =-a22(1,2)/det
        b(5,4) =-a22(2,1)/det
        b(5,5) = a22(1,1)/det
        !     (4..5,1..3) of A^-1 = -A22^-1 * A23
        b(4,1) = -b(4,4)*a23(1,1)-b(4,5)*a23(2,1)
        b(4,2) = -b(4,4)*a23(1,2)-b(4,5)*a23(2,2)
        b(4,3) = -b(4,4)*a23(1,3)-b(4,5)*a23(2,3)
        b(5,1) = -b(5,4)*a23(1,1)-b(5,5)*a23(2,1)
        b(5,2) = -b(5,4)*a23(1,2)-b(5,5)*a23(2,2)
        b(5,3) = -b(5,4)*a23(1,3)-b(5,5)*a23(2,3)
        RETURN
    END SUBROUTINE gblinu
END MODULE gbltraj

