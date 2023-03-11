
! Code converted using TO_F90 by Alan Miller
! Date: 2012-03-16  Time: 11:06:29

!> \file
!! Line search.
!!
!! \author Volker Blobel, University Hamburg, 2005-2009 (initial Fortran77 version)
!! \author Claus Kleinwort, DESY (maintenance and developement)
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
!! Line search routine with sufficient decrease of slope.
!!
!! In many minimization problems the objective function is close to
!! quadratic, except far from the solution. Close to the minimum the
!! behaviour may be almost quadratic or, due to round-off errors,
!! it may have a non-smooth behaviour, which often complicates any
!! further progress and the recognition of convergence.
!! Round-off errors affect the function value, which may be large and
!! small parameter changes result in small relative changes of the
!! function value. Close to the minimum the gradient becomes small
!! and the behaviour is not so much affected by Round-off errors.
!!
!!          CALL PTLDEF(0.0,0.0, 0,0) ! init line search
!!          N=...
!!          X(.)=...
!!          D(.)=...
!!          ALPHA=1.0D0
!!     10   F(X)=...
!!          G(X)=...
!!          IF(.) S(X)=..
!!          CALL PTLINE(N,X,F,G,D,ALPHA,INFO)
!!          IF(INFO.LT.0) GOTO 10
!!

!> Line search data.
MODULE linesrch
    USE mpdef

    IMPLICIT NONE

    INTEGER(mpi), PARAMETER :: msfd=20
    INTEGER(mpi) :: nsfd    !< number of function calls
    INTEGER(mpi) :: idgl    !< index of smallest negative slope
    INTEGER(mpi):: idgr    !< index of smallest positive slope
    INTEGER(mpi) :: idgm    !< index of minimal slope
    INTEGER(mpi) :: minf=1  !< min. number of function calls
    INTEGER(mpi) :: maxf=5  !< max. number of function calls
    INTEGER(mpi) :: lsinfo  !< (status) information
    REAL(mpd), DIMENSION(4,msfd) :: sfd !< abscissa; function value; slope; predicted zero
    REAL(mpd) :: stmx=0.9 !< maximum slope ratio
    REAL(mpd) :: gtol     !< slope ratio

END MODULE linesrch

!> Perform linesearch.
!!
!! \param [in]      N     dimension of problem
!! \param [in,out]  X     current iterate
!! \param [in,out]  F     associated function value
!! \param [in,out]  G     associated gradient
!! \param [in,out]  S     search vector
!! \param [out]     STEP  step factor (initially = 1.0)
!! \param [out]     INFO  information
!!
!!      = -1  repeat function evaluation
!!      =  0  input error (e.g. gradient not negative)
!!      =  1  convergence reached
!!      =  2  convergence assumed, but round-off errors
!!      =  3  too many function calls
!!      =  4  step factor ALPHA to small (ALPHA <= TOL)
!!

SUBROUTINE ptline(n,x,f,g,s,step, info) ! - 2 arguments
    USE linesrch

    IMPLICIT NONE
    INTEGER(mpi), INTENT(IN)                      :: n
    REAL(mpd), INTENT(IN OUT)         :: x(n)
    REAL(mpd), INTENT(IN OUT)         :: f
    REAL(mpd), INTENT(IN OUT)         :: g(n)
    REAL(mpd), INTENT(IN OUT)         :: s(n)
    REAL(mpd), INTENT(OUT)            :: step
    INTEGER(mpi), INTENT(OUT)                     :: info

    INTEGER(mpi)::  i1
    INTEGER(mpi) :: i2
    INTEGER(mpi) :: i               ! internal
    INTEGER(mpi) :: im              ! internal
    REAL(mpd) :: alpha  ! internal
    REAL(mpd) :: dginit ! internal
    REAL(mpd) :: dg     ! internal
    REAL(mpd) :: fsaved ! internal
    REAL(mpd) :: tot    ! internal
    REAL(mpd) :: fp1    ! internal
    REAL(mpd) :: fp2    ! internal
    SAVE

    !     initialization ---------------------------------------------------

    info=0             ! reset INFO flag
    dg=0.0_mpd
    DO i=1,n           !
        dg=dg-g(i)*s(i)   ! DG = scalar product: grad x search
    END DO!

    IF(nsfd == 0) THEN    ! initial call
        dginit=dg          ! DG = initial directional gradient
        IF(dginit >= 0.0_mpd) GO TO 100 ! error: step not decreasing
        step=1.0_mpd         ! initial step factor is one
        alpha=step         ! get initial step factor
        tot=0.0_mpd          ! reset total step
        idgl=1             ! index of smallest negative slope
        idgr=0             ! index of smallest positive slope
        fsaved=f           ! initial Function value
        nsfd=1             ! starting point of iteration
        sfd(1,1)=0.0       ! abscissa
        sfd(2,1)=0.0       ! reference function value
        sfd(3,1)=dginit    ! slope
        sfd(4,1)=0.0       ! predicted zero
        im=1               ! optimum
    ELSE                  ! subsequent call
        nsfd=nsfd+1
        sfd(1,nsfd)=tot          ! abscissa
        sfd(2,nsfd)=f-fsaved     ! function value difference to reference
        sfd(3,nsfd)=dg           ! slope
        sfd(4,nsfd)=0.0          ! predicted zero (see below)
        IF(dg < sfd(3,im)) THEN
            im=nsfd
        END IF
  
        !        define interval indices IDGL and IDGR
        IF(dg <= 0.0_mpd) THEN
            IF(dg >= sfd(3,idgl)) idgl=nsfd
        END IF
        IF(dg >= 0.0_mpd) THEN     ! limit to the right
            IF(idgr == 0) idgr=nsfd
            IF(dg <= sfd(3,idgr)) idgr=nsfd
        END IF
  
        IF(idgr == 0) THEN
            i1=nsfd-1
            i2=nsfd
        ELSE
            i1=idgl
            i2=idgr
        END IF
        fp1=sfd(3,i1)
        fp2=sfd(3,i2)                       ! interpolation
        sfd(4,nsfd)=(sfd(1,i1)*fp2-sfd(1,i2)*fp1)/(fp2-fp1)
  
        !        convergence tests
        IF(nsfd >= minf.AND.ABS(dg) <= ABS(dginit)*gtol) THEN
            !           normal convergence return with INFO=1 ----------------------
            alpha=tot+alpha          ! total ALPHA is returned
            step =alpha
            idgm=idgl
            IF(idgr /= 0) THEN
                IF(sfd(3,idgr)+sfd(3,idgl) < 0.0_mpd) idgm=idgr
            END IF
            GO TO 101
        END IF
        IF(nsfd >= maxf) GO TO 102 ! max number of function calls
        alpha=MIN(sfd(4,nsfd),stmx)-tot     ! new step from previous
        IF(ABS(alpha) < 1.0E-3_mpd.AND.sfd(4,nsfd) > stmx) GO TO 103
        IF(ABS(alpha) < 1.0E-3_mpd) GO TO 104
    END IF

    !     prepare next function call ---------------------------------------

    DO i=1,n
        x(i)=x(i)+alpha*s(i)    ! step by ALPHA -> new X
    END DO
    tot=tot+alpha            !
    step=tot
    info=-1                  ! recalculate function and gradient
    lsinfo=info
    RETURN

    !     error exits ------------------------------------------------------
104 info=info+1              ! 4: step small
103 info=info+1              ! 3: maximum reached
102 info=info+1              ! 2: too many function calls
101 info=info+1              ! 1: normal convergence
    lsinfo=info
    im=1
    DO i=1,nsfd
        IF(ABS(sfd(3,i)) < ABS(sfd(3,im))) im=i
    END DO
    alpha=sfd(1,im)-sfd(1,nsfd)
    IF(im == nsfd) RETURN    ! already at minimum
    DO i=1,n
        x(i)=x(i)+alpha*s(i)    ! step by ALPHA to slope minimum
    END DO
    f=sfd(2,im)+fsaved       ! F at minimum
    step=sfd(1,im)           ! total step at convergence
    IF(im /= 1) RETURN       ! improvement
    info=5                   ! no improvement
100 step=0.0_mpd               ! 0: initial slope not negative
    lsinfo=info
    RETURN
END SUBROUTINE ptline

!> Initialize line search.
!!
!! \param[in]  gtole  slope ratio
!! \param[in]  stmax  total step limit
!! \param[in]  minfe  minimum number of evaluations
!! \param[in]  maxfe  maximum number of evaluations
!!
!!                        --- range ----       default
!!     slope ratio        1.0E-4 ... 0.9         0.9
!!     min. F-calls       1 ... 2                 1
!!     max. F-calls       2 ... 10                5
!!

SUBROUTINE ptldef(gtole,stmax,minfe,maxfe)
    USE linesrch

    IMPLICIT NONE
    INTEGER(mpi), INTENT(IN) :: minfe
    INTEGER(mpi), INTENT(IN) :: maxfe
    REAL(mps), INTENT(IN)    :: gtole
    REAL(mps), INTENT(IN)    :: stmax

    gtol=MAX(1.0E-4,MIN(gtole,0.9E0))  ! slope ratio
    IF(gtole == 0.0) gtol=0.9_mpd   ! default slope ratio
    stmx=stmax                    ! maximum total step
    IF(stmx == 0.0_mpd) stmx=10.0_mpd ! default limit
    minf=MAX(1,MIN(minfe,msfd-2)) ! minimum number of evaluations
    maxf=MAX(2,MIN(maxfe,msfd-1)) ! maximum number of evaluations
    IF(maxfe == 0) maxf=5         ! default max number of values
    nsfd=0                        ! reset
END SUBROUTINE ptldef

!> Get details.
!!
!! \param[out]   NF      number of function values
!! \param[out]   M       index of function value with smallest slope
!! \param[out]   SLOPES  initial, current, smallest slope
!! \param[out]   STEPS   initial position, current, smallest step

SUBROUTINE ptlopt(nf,m,slopes,steps)
    USE linesrch
    IMPLICIT NONE

    INTEGER(mpi), INTENT(OUT)                     :: nf
    INTEGER(mpi), INTENT(OUT)                     :: m
    REAL(mps), DIMENSION(3), INTENT(OUT)          :: slopes
    REAL(mps), DIMENSION(3), INTENT(OUT)          :: steps
    INTEGER(mpi) :: i

    !     ...
    nf=nsfd
    IF(nsfd == 0) THEN  ! no values
        m=0
        DO i=1,3
            slopes(i)=0.0
            steps(i) =0.0
        END DO
    ELSE                ! values exist
        m=1
        DO i=1,nsfd
            IF(ABS(sfd(3,i)) < ABS(sfd(3,m))) m=i
        END DO
        slopes(1)=REAL(sfd(3,1))
        slopes(2)=REAL(sfd(3,nsfd))
        slopes(3)=REAL(sfd(3,m))
        steps(1) =REAL(sfd(1,1))
        steps(2) =REAL(sfd(1,nsfd))
        steps(3) =REAL(sfd(1,m))
    END IF
END SUBROUTINE ptlopt

!> Print line search data.
!!
!! \param[in] lunp  unit number

SUBROUTINE ptlprt(lunp)
    USE linesrch

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: j
    INTEGER(mpi) :: im
    INTEGER(mpi) :: lun
    INTEGER(mpi), INTENT(IN) :: lunp
    REAL(mps) :: ratio
    CHARACTER (LEN=2) :: tlr
    !     ...
    lun=lunp
    IF(lun == 0) lun=6
    IF(nsfd <= 0) RETURN
    WRITE(lun,*) ' '
    WRITE(lun,*) 'PTLINE: line-search method based on slopes',  &
        ' with sufficient slope-decrease'
    WRITE(lun,*) 'PTLDEF: slope ratio limit=',gtol
    WRITE(lun,*) 'PTLDEF: maximum step =',stmx
    WRITE(lun,*) 'PTLDEF:',minf,' <= nr of calls <=',maxf
    WRITE(lun,101)
    im=1
    DO i=1,nsfd
        IF(ABS(sfd(3,i)) < ABS(sfd(3,im))) im=i
    END DO
    DO i=1,nsfd
        tlr='  '
        IF(i == im)   tlr='**'
        IF(i == idgl) tlr(1:1)='L'
        IF(i == idgr) tlr(2:2)='R'
        IF(i == 1) THEN
            WRITE(lun,102) i-1, sfd(1,i),tlr,(sfd(j,i),j=2,4)
        ELSE
            ratio=REAL(ABS(sfd(3,i)/sfd(3,1)))
            WRITE(lun,103) i-1, sfd(1,i),tlr,(sfd(j,i),j=2,4),ratio
        END IF

    END DO
    IF(lsinfo == 0) WRITE(lun,*)  &
        'PTLINE: INFO=0  input error (e.g. gradient not negative)'
    IF(lsinfo == 1) WRITE(lun,*) 'PTLINE: INFO=1  convergence reached'
    IF(lsinfo == 2) WRITE(lun,*) 'PTLINE: INFO=2  too many function calls'
    IF(lsinfo == 3) WRITE(lun,*) 'PTLINE: INFO=3  maximum step reached'
    IF(lsinfo == 4) WRITE(lun,*) 'PTLINE: INFO=4  step too small (< 0.001)'
    WRITE(lun,*) ' '

101 FORMAT('  i      x              F(x)         F''(X)',  &
        '          minimum     F''(X)')
102 FORMAT(i3,f12.6,1X,a2,g15.6,g14.6,f12.6,'     ratio')
103 FORMAT(i3,f12.6,1X,a2,g15.6,g14.6,f12.6,f10.3)

END SUBROUTINE ptlprt

