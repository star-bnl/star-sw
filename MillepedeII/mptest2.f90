
! Code converted using TO_F90 by Alan Miller
! Date: 2012-03-16  Time: 11:08:55

!> \file
!! MC for simple 10 layer silicon strip tracker.
!!
!! \author Claus Kleinwort, DESY, 2009
!!
!! \copyright
!! Copyright (c) 2009 - 2015 Deutsches Elektronen-Synchroton,
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
!!
!! No B-field, straight tracks. Selected with command line option '-t=track-model'
!! The \a track-models differ in the implementation of multiple scattering (errors):
!! - \c SL0: Ignore multiple scattering. Fit 4 track parameters.
!! - \c SLE: Ignore correlations due to multiple scattering, use only diagonal of
!!   m.s. covariance matrix. Fit 4 track parameters.
!! - \c BP: Intoduce explicit scattering angles at each scatterer.
!!   Fit 4+2*(\ref mptest2::nmlyr "nmlyr"-2) parameters.
!!   Matrix of corresponding linear equation system is full and solution
!!   is obtained by inversion (time ~ parameters^3).
!! - \c BRLF: Use (fine) broken lines (see \ref ref_sec). Multiple scattering kinks
!!   are described by triplets of offsets at scatterers as track parameters.
!!   Fit 4+2*(\ref mptest2::nmlyr "nmlyr"-2) parameters. Matrix of corresponding
!!   linear equation system has band structure and solution
!!   is obtained by root-free Cholesky decomposition (time ~ parameters).
!! - \c BRLC: Use (coarse) broken lines. Similar to \c BRLF, but with stereo layers
!!   combined into single layer/scatterer. Fit 4+2*(\ref mptest2::nlyr "nlyr"-2) parameters.
!!
!! MC for simple silicon strip tracker:
!! - 10 silicon detector layers
!! - 50 modules per layer (1*2cm)
!! - 10 cm spacing, no B-field
!! - layers 1,4,7,10 have additional +/-5deg stereo modules
!! - intrinsic resolution 20mu, 2% X0 per strip module
!! - uniform track offsets/slopes
!! - momentum: log10(p) 10..100 GeV uniform
!!
!! Global parameters:
!! - Position offsets (2D) in measurement plane per module (alignment).
!!

!> Parameters and data.
MODULE mptest2
    USE mpdef

    IMPLICIT NONE
    SAVE

    INTEGER(mpi), PARAMETER :: nlyr=10            !< number of detector layers
    INTEGER(mpi), PARAMETER :: nmlyr=14           !< number of measurement layers
    INTEGER(mpi), PARAMETER :: nmx=10             !< number of modules in x direction
    INTEGER(mpi), PARAMETER :: nmy=5              !< number of modules in y direction
    INTEGER(mpi), PARAMETER :: ntot=nlyr*nmx*nmy  !< total number of modules
    !     define detector geometry
    REAL(mps), PARAMETER :: dets= 10.0            !< arclength of first plane
    REAL(mps), PARAMETER :: diss= 10.0            !< distance between planes
    REAL(mps), PARAMETER :: thck= 0.02            !< thickness of plane (X0)
    REAL(mps), PARAMETER :: offs=  0.5            !< offset of stereo modules
    REAL(mps), PARAMETER :: stereo=0.08727        !< stereo angle
    REAL(mps), PARAMETER :: sizel= 20.0           !< size of layers
    REAL(mps), PARAMETER :: sigl =0.002           ! <resolution

    INTEGER(mpi) :: nhits                         !< number of hits
    REAL(mps) :: the0                             !< multiple scattering error
    INTEGER(mpi), DIMENSION(nmlyr) :: islyr       !< (detector) layer
    INTEGER(mpi), DIMENSION(nmlyr) :: ihits       !< module number
    REAL(mps), DIMENSION(ntot) :: sdevx           !< shift in x (alignment parameter)
    REAL(mps), DIMENSION(ntot) :: sdevy           !< shift in y (alignment parameter)
    REAL(mps), DIMENSION(nmlyr) :: sarc           !< arc length
    REAL(mps), DIMENSION(nmlyr) :: ssig           !< resolution
    REAL(mps), DIMENSION(2,nmlyr) :: spro         !< projection of measurent direction in (XY)
    REAL(mps), DIMENSION(nmlyr) :: xhits          !< position perp. to plane (hit)
    REAL(mps), DIMENSION(nmlyr) :: yhits          !< measured position in plane (hit)
    REAL(mps), DIMENSION(nmlyr) :: sigma          !< measurement sigma (hit)

END MODULE mptest2

!> Generate test files.
!!
!! Create text and binary files.
!!
!!      unit  8: textfile mp2str.txt   = steering file
!!      unit  9: textfile mp2con.txt   = constraint file
!!      unit 51: binary file mp2test.bin, written using CALL MILLE(.)
!!      existing file are removed
!!
!! \param [in] imodel track model
!!
!!           0: 'straight line', ignoring multiple scattering
!!           1: 'straight line', using diagonal of m.s. error matrix
!!           2: 'break points'
!!           3: 'broken lines', fine
!!           4: 'broken lines', coarse (stereo layers combined)
!!

SUBROUTINE mptst2(imodel)         ! generate test files
    USE mptest2
    IMPLICIT NONE
    REAL(mps) :: cmbbrl
    REAL(mps) :: dispxm
    REAL(mps) :: dispym
    REAL(mps) :: dn
    REAL(mps) :: dp
    REAL(mps) :: gran
    REAL(mps) :: one
    REAL(mps) :: p
    REAL(mps) :: s
    REAL(mps) :: sgn
    REAL(mps) :: sbrl
    REAL(mps) :: sold
    REAL(mps) :: uran
    REAL(mps) :: wbrl
    INTEGER(mpi) :: i
    INTEGER(mpi) :: ibrl
    INTEGER(mpi) :: icount
    INTEGER(mpi) :: im
    INTEGER(mpi) :: ios
    INTEGER(mpi) :: ip
    INTEGER(mpi) :: j
    INTEGER(mpi) :: k
    INTEGER(mpi) :: l
    INTEGER(mpi) :: labelt
    INTEGER(mpi) :: layer
    INTEGER(mpi) :: lb
    INTEGER(mpi) :: lbrl
    INTEGER(mpi) :: luns
    INTEGER(mpi) :: lunt
    INTEGER(mpi) :: lyr
    INTEGER(mpi) :: nalc
    INTEGER(mpi) :: nbrl
    INTEGER(mpi) :: ncount
    INTEGER(mpi) :: ncx
    INTEGER(mpi) :: nmxy
    INTEGER(mpi) :: nrecds
    INTEGER(mpi) :: nthits

    INTEGER(mpi), INTENT(IN)                      :: imodel

    REAL(mps) :: derlc(nmlyr*2+3)
    REAL(mps) :: dergl(nmlyr*2+3)
    INTEGER(mpi) :: label(2)
    LOGICAL :: ex1
    LOGICAL :: ex2
    LOGICAL :: ex3
    !     for broken lines: 1=fine, 2=coarse
    DIMENSION nbrl(2),lbrl(nmlyr,2),sbrl(nmlyr,2),wbrl(nmlyr,2), cmbbrl(2)
    DATA cmbbrl / 0.0, 1.0 / ! cut for combining layers
    !     ...
    !CC      CALL RNTIME
    INQUIRE(FILE='mp2str.txt',IOSTAT=ios,EXIST=ex1) ! keep, if existing
    INQUIRE(FILE='mp2con.txt',IOSTAT=ios,EXIST=ex2) ! keep, if existing

    INQUIRE(FILE='mp2tst.bin',IOSTAT=ios,EXIST=ex3) ! remove, if existing

    WRITE(*,*) ' '
    WRITE(*,*) 'Generating test data for mp II...'
    WRITE(*,*) ' '
    !     file management
    IF(ex3) CALL system('rm mp2tst.bin')   ! remove old file

    IF(.NOT.ex1) OPEN(UNIT=7,ACCESS='SEQUENTIAL',FORM='FORMATTED',  &
        FILE='mp2str.txt')
    IF(.NOT.ex2) OPEN(UNIT=9,ACCESS='SEQUENTIAL',FORM='FORMATTED',  &
        FILE='mp2con.txt')
    OPEN(UNIT=51,ACCESS='SEQUENTIAL',FORM='UNFORMATTED', FILE='mp2tst.bin')

    s=dets
    i=0
    sgn=1.0
    DO layer=1,10
        i=i+1
        islyr(i)=layer       ! layer
        sarc(i)=s            ! arclength
        ssig(i)=sigl         ! resolution
        spro(1,i)=1.0        ! module measures 'X'
        spro(2,i)=0.0
        IF (MOD(layer,3) == 1) THEN
            i=i+1
            islyr(i)=layer       ! layer
            sarc(i)=s+offs       ! arclength stereo module
            ssig(i)=sigl         ! resolution
            spro(1,i)=SQRT(1.0-stereo**2)
            spro(2,i)=stereo*sgn ! module measures both 'X' and 'Y'
            sgn=-sgn             ! stereo orientation
        END IF
        s=s+diss
    END DO

    ! define broken lines
    sold=-1000.
    nbrl(1)=0
    nbrl(2)=0
    DO k=1,2
        DO i=1, nmlyr
            IF (ABS(sarc(i)-sold) > cmbbrl(k)) nbrl(k)=nbrl(k)+1
            lb=nbrl(k)
            lbrl(i,k)=lb
            sbrl(lb,k)=sbrl(lb,k)+sarc(i)
            wbrl(lb,k)=wbrl(lb,k)+1.0
            sold=sarc(i)
        END DO
        DO i=1,nbrl(k)
            sbrl(i,k)=sbrl(i,k)/wbrl(i,k)
            wbrl(i,k)=SQRT(wbrl(i,k))
        END DO
    END DO
    ibrl=imodel-2

    !     misalign detector modules -----------------------------------------

    dispxm=0.01           ! module displacement in X .05 mm * N(0,1)
    dispym=0.01           ! module displacement in Y .05 mm * N(0,1)

    DO i=0,nlyr-1
        DO k=0,nmy-1
            DO l=1,nmx
                sdevx(((i*nmy+k)*nmx+l))=dispxm*gran()     ! shift in x
                sdevy(((i*nmy+k)*nmx+l))=dispym*gran()     ! shift in y
            END DO
        END DO
    END DO
    !     write text files -------------------------------------------------

    IF(.NOT.ex1) THEN
        luns=7                           ! steerfile
        WRITE(luns,101) '*            Default test steering file'
        WRITE(luns,101) 'fortranfiles ! following bin files are fortran'
        WRITE(luns,101) 'mp2con.txt   ! constraints text file '
        WRITE(luns,101) 'mp2tst.bin   ! binary data file'
        WRITE(luns,101) 'Cfiles       ! following bin files are Cfiles'
        !      WRITE(LUNS,101) '*outlierrejection 100.0 ! reject if Chi^2/Ndf >'
        !      WRITE(LUNS,101) '*outliersuppression 3   ! 3 local_fit iterations'

        WRITE(luns,101) '*hugecut 50.0     !cut factor in iteration 0'
        WRITE(luns,101) '*chisqcut 1.0 1.0 ! cut factor in iterations 1 and 2'
        WRITE(luns,101) '*entries  10 ! lower limit on number of entries/parameter'
        WRITE(luns,101)  &
            '*pairentries 10 ! lower limit on number of parameter pairs',  &
            '                ! (not yet!)'
        WRITE(luns,101) '*printrecord   1  2      ! debug printout for records'
        WRITE(luns,101)  &
            '*printrecord  -1 -1      ! debug printout for bad data records'
        WRITE(luns,101)  &
            '*outlierdownweighting  2 ! number of internal iterations (> 1)'
        WRITE(luns,101) '*dwfractioncut      0.2  ! 0 < value < 0.5'
        WRITE(luns,101) '*presigma           0.01 ! default value for presigma'
        WRITE(luns,101) '*regularisation 1.0      ! regularisation factor'
        WRITE(luns,101) '*regularisation 1.0 0.01 ! regularisation factor, pre-sigma'

        WRITE(luns,101) ' '
        WRITE(luns,101) '*bandwidth 0         ! width of precond. band matrix'
        WRITE(luns,101) 'method diagonalization 3 0.001 ! diagonalization      '
        WRITE(luns,101) 'method fullMINRES       3 0.01 ! minimal residual     '
        WRITE(luns,101) 'method sparseMINRES     3 0.01 ! minimal residual     '
        WRITE(luns,101) '*mrestol      1.0D-8          ! epsilon for MINRES'
        WRITE(luns,101) 'method inversion       3 0.001 ! Gauss matrix inversion'
        WRITE(luns,101) '* last method is applied'
        WRITE(luns,101) '*matiter      3  ! recalculate matrix in iterations'
        WRITE(luns,101) ' '
        WRITE(luns,101) 'end ! optional for end-of-data'
    END IF

    ! constraints: fix center modules in first/last layer

    ncx=(nmx+1)/2
    nmxy=nmx*nmy
    lunt=9
    one=1.0
    DO i=1,nlyr,nlyr-1
        IF(.NOT.ex2) WRITE(lunt,*) 'Constraint  0.0'
        DO k=0,nmy-1
            labelt=(i*nmy+k)*nmx+ncx-1
            IF(.NOT.ex2) WRITE(lunt,103) labelt,one
            sdevx(((i-1)*nmy+k)*nmx+ncx)=0.0      ! fix center modules at 0.
        END DO
        IF(.NOT.ex2) WRITE(lunt,*) 'Constraint  0.0'
        DO k=0,nmy-1
            labelt=(i*nmy+k)*nmx+ncx+1000-1
            IF(.NOT.ex2) WRITE(lunt,103) labelt,one
            sdevy(((i-1)*nmy+k)*nmx+ncx)=0.0      ! fix center modules at 0.
        END DO
    END DO

    !     record loop ------------------------------------------------------

    ncount=10000
    nthits=0
    nrecds=0

    DO icount=1,ncount
        !      10..100 GeV
        p=10.0**(1.+uran())
        the0=SQRT(thck)*0.014/p
        ip=0
        !       IF (ICOUNT.LE.3) IP=1
        CALL genln2(ip)      ! generate hits
  
  
        DO i=1,nhits
            ! simple straight line
            lyr=ihits(i)/nmxy+1
            im =MOD(ihits(i),nmxy)
            nalc=4
            derlc(1)=spro(1,lyr)
            derlc(2)=spro(2,lyr)
            derlc(3)=xhits(i)*spro(1,lyr)
            derlc(4)=xhits(i)*spro(2,lyr)
            dergl(1)=spro(1,lyr)
            dergl(2)=spro(2,lyr)
            label(1)=im+nmxy*islyr(lyr)
            label(2)=im+nmxy*islyr(lyr)+1000
            ! add multiple scattering errors (no correlations)
            IF (imodel == 1) THEN
                DO j=i,nhits
                    sigma(j)=SQRT(sigma(j)**2+((xhits(j)-xhits(i))*the0)**2)
                END DO
            END IF
            ! add 'break points' for multiple scattering
            IF (imodel == 2.AND.i > 1) THEN
                DO j=1,i-1
                    ! 2 scattering angles from each layer in front of current
                    nalc=nalc+1
                    derlc(nalc)=(xhits(i)-xhits(j))*spro(1,lyr)
                    nalc=nalc+1
                    derlc(nalc)=(xhits(i)-xhits(j))*spro(2,lyr)
                END DO
            END IF
            ! add 'broken lines' offsets for multiple scattering
            IF (imodel >= 3) THEN
                nalc=2*nbrl(ibrl)
                DO k=1, nalc
                    derlc(k)=0.0
                END DO
                ! 2 offsets
                lb=lbrl(lyr,ibrl)
                derlc(lb*2-1)=spro(1,lyr)
                derlc(lb*2  )=spro(2,lyr)
            END IF

            CALL mille(nalc,derlc,2,dergl,label,yhits(i),sigma(i))
            nthits=nthits+1  ! count hits
        END DO
        ! additional measurements from MS
        IF (imodel == 2) THEN
            DO i=1,(nhits-1)*2
                nalc=i+4
                DO k=1,nalc
                    derlc(k)=0.0
                END DO
                derlc(nalc)=1.0
                CALL mille(nalc,derlc,0,dergl,label,0.0,the0)
            END DO
        END IF

        IF (imodel >= 3) THEN
            DO i=2,nbrl(ibrl)-1
                dp=1.0/(sbrl(i,ibrl)-sbrl(i-1,ibrl))
                dn=1.0/(sbrl(i+1,ibrl)-sbrl(i,ibrl))
                nalc=(i+1)*2
                DO l=-1,0
                    DO k=1,nalc
                        derlc(k)=0.0
                    END DO
                    derlc(2*(i-1)+l)= dp
                    derlc(2* i   +l)=-dp-dn
                    derlc(2*(i+1)+l)= dn
                    CALL mille(nalc,derlc,0,dergl,label,0.0,the0*wbrl(i,ibrl))
                END DO
            END DO
        END IF

        CALL endle
        nrecds=nrecds+1   ! count records
    END DO

    !     ------------------------------------------------------------------
    IF(.NOT.ex1) THEN
        REWIND  (7)
        CLOSE   (7)
    END IF
    IF(.NOT.ex2) THEN
        REWIND  (9)
        CLOSE   (9)
    END IF
    REWIND (51)
    CLOSE  (51)

    !      WRITE(*,*) ' '
    !      WRITE(*,*) 'Shifts and drift velocity deviations:'
    !      DO I=1,NPLAN
    !       WRITE(*,102) I,DEL(I),DVD(I)
    !      END DO


    WRITE(*,*) ' '
    WRITE(*,*) ' '
    WRITE(*,*) ncount,' tracks generated with ',nthits,' hits.'
    WRITE(*,*) nrecds,' records written.'
    WRITE(*,*) ' '
101 FORMAT(a)
    ! 102  FORMAT(I6,2F10.5)
103 FORMAT(i8,f10.5)
END SUBROUTINE mptst2

!> Generate line and measurements.
!!
!! \param [in] ip print flag

SUBROUTINE genln2(ip)
    USE mptest2

    IMPLICIT NONE
    REAL(mps) :: ds
    REAL(mps) :: dx
    REAL(mps) :: dy
    REAL(mps) :: gran
    INTEGER(mpi) :: i
    INTEGER(mpi) :: ihit
    INTEGER(mpi) :: imx
    INTEGER(mpi) :: imy
    INTEGER(mpi) :: ioff
    REAL(mps) :: sold
    REAL(mps) :: uran
    REAL(mps) :: x
    REAL(mps) :: xexit
    REAL(mps) :: xl
    REAL(mps) :: xnull
    REAL(mps) :: xs
    REAL(mps) :: xslop
    REAL(mps) :: y
    REAL(mps) :: yexit
    REAL(mps) :: yl
    REAL(mps) :: ynull
    REAL(mps) :: ys
    REAL(mps) :: yslop


    INTEGER(mpi), INTENT(IN)                      :: ip

    !     track parameters
    xnull=sizel*(uran()-0.5)   ! uniform vertex
    ynull=sizel*(uran()-0.5)   ! uniform vertex
    xexit=sizel*(uran()-0.5)   ! uniform exit point
    yexit=sizel*(uran()-0.5)   ! uniform exit point
    xslop=(xexit-xnull)/sarc(nmlyr)
    yslop=(yexit-ynull)/sarc(nmlyr)
    IF(ip /= 0) THEN
        WRITE(*,*) ' '
        WRITE(*,*) ' Track ', xnull, ynull, xslop, yslop
    END IF

    nhits=0
    x=xnull
    y=ynull
    dx=xslop
    dy=yslop
    sold=0.0

    DO  i=1,nmlyr
        ds=sarc(i)-sold
        sold=sarc(i)
        !      position with parameters 1. hit
        xs=xnull+sarc(i)*xslop
        ys=ynull+sarc(i)*yslop
        !      true track position
        x=x+dx*ds
        y=y+dy*ds
        !      multiple scattering
        dx=dx+gran()*the0
        dy=dy+gran()*the0
  
        imx=INT((x+sizel*0.5)/sizel*REAL(nmx,mps),mpi)
        IF (imx < 0.OR.imx >= nmx) CYCLE
        imy=INT((y+sizel*0.5)/sizel*REAL(nmy,mps),mpi)
        IF (imy < 0.OR.imy >= nmy) CYCLE
  
        ihit=((i-1)*nmy+imy)*nmx+imx
        ioff=((islyr(i)-1)*nmy+imy)*nmx+imx+1
        nhits=nhits+1
        ihits(nhits)=ihit
        xl=x-sdevx(ioff)
        yl=y-sdevy(ioff)
        xhits(nhits)=sarc(i)
        yhits(nhits)=(xl-xs)*spro(1,i)+(yl-ys)*spro(2,i)+gran()*ssig(i)
        sigma(nhits)=ssig(i)
  
        IF(ip /= 0) THEN
            WRITE(*,101) nhits,i,ihit,x,y,xhits(nhits), yhits(nhits),sigma(nhits)
        END IF
    END DO
101 FORMAT(3I3,5F8.4)
END SUBROUTINE genln2

