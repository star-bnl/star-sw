
! Code converted using TO_F90 by Alan Miller
! Date: 2012-03-03  Time: 17:00:12

!> \file
!! Millepede-II binary record.

!> Add data block to record.
!!
!!\verbatim
!!        CALL MILLE(...)       ! measured value, derivatives (one set)
!!        CALL ENDLE            ! complete, write record (many sets)
!!    (or CALL KILLE            ! stop record)
!!    The data transmitted by MILLE calls are collected in two arrays,
!!    a real array and an integer array, of same length. The collected
!!    data are written at the ENDLE call. The content of the arrays:
!!        real array              integer array
!!    1   0.0                     error count (this record)
!!    2   RMEAS, measured value   0                            JA
!!    3   local derivative        index of local derivative
!!    4   local derivative        index of local derivative
!!    5    ...
!!    6   SIGMA, error (>0)       0                            JB
!!        global derivative       label of global derivative
!!        global derivative       label of global derivative   IST
!!        RMEAS, measured value   0
!!        local derivative        index of local derivative
!!        local derivative        index of local derivative
!!        ...
!!        SIGMA, error            0
!!        global derivative       label of global derivative
!!        global derivative       label of global derivative
!!        ...
!!    NR  global derivative       label of global derivative
!!
!!    i.e. the 0's in the integer array allow to recognize the start
!!    of a new set, the measured value and the error. The local and
!!    the global derivatives are inbetween, with a positive value in
!!    the integer array, the index of the local derivative or the
!!    label of the global derivative.
!!    ------------------------------------------------------------------
!!    If more than one output unit is needed: duplicate this subroutine
!!    change the entry names to e.g. AMILLE, AENDLE, AKILLE and change
!!    the value of LUN and evtl. the dimension parameter in the
!!    parameter statements.
!!    -----------------------------------------------------------------
!!\endverbatim
!! \param [in] NLC    number of local derivatives
!! \param [in] DERLC  local derivatives
!! \param [in] NGL    number of global derivatives
!! \param [in] DERGL  global derivatives
!! \param [in] LABEL  labels for global derivatives
!! \param [in] RMEAS  measurement
!! \param [in] SIGMA  error of measurement

SUBROUTINE mille(nlc,derlc,ngl,dergl,label,rmeas,sigma) ! add data
    IMPLICIT NONE
    INTEGER :: i
    INTEGER :: icount
    INTEGER :: isp
    INTEGER :: nr
    INTEGER :: nsp
    !     -----------------------------------------------------------------

    INTEGER, INTENT(IN)                      :: nlc
    REAL, INTENT(IN)                         :: derlc(nlc)
    INTEGER, INTENT(IN)                      :: ngl
    REAL, INTENT(IN)                         :: dergl(ngl)
    INTEGER, INTENT(IN)                      :: label(ngl)
    REAL, INTENT(IN)                         :: rmeas
    REAL, INTENT(IN)                         :: sigma
    INTEGER, PARAMETER :: lun=51
    INTEGER, PARAMETER :: ndim=5000
    REAL :: glder(ndim)      ! real data record array
    INTEGER :: inder(ndim)      ! integer data record array
    !     -----------------------------------------------------------------

    SAVE
    DATA nr/0/                ! initial record length
    DATA icount/0/
    !     ...
    IF(sigma <= 0.0) RETURN   ! error zero - no measurement
    IF(nr == 0) THEN
        nr=1
        glder(1)=0.0
        inder(1)=0             ! error counter
        isp=0
    END IF
    IF(nr+nlc+ngl+2 > ndim) THEN
        icount=icount+1
        IF(icount <= 10) THEN
            WRITE(*,*) 'Mille warning: data can not be stored'
            IF(icount == 10) THEN
                WRITE(*,*) 'Mille warning: no further printout'
            END IF
        END IF
        inder(1)=inder(1)+1    ! count errors
        RETURN                 ! record dimension too small
    END IF
    nr=nr+1
    glder(nr)=rmeas           ! measured value
    inder(nr)=0
    DO i=1,nlc                ! local derivatives
        IF(derlc(i) /= 0.0) THEN
            nr=nr+1
            glder(nr)=derlc(i)    ! derivative of local parameter
            inder(nr)=i           ! index of local parameter
        END IF
    END DO

    nr=nr+1
    glder(nr)=sigma           ! error of measured value
    inder(nr)=0
    DO i=1,ngl                ! global derivatives
        IF(dergl(i) /= 0.0.AND.label(i) > 0) THEN
            nr=nr+1
            glder(nr)=dergl(i)    ! derivative of global parameter
            inder(nr)=label(i)    ! index of global parameter
        END IF
    END DO
    RETURN

    ENTRY millsp(nsp,dergl,label)
   !     add NSP special words (floating-point and integer)

   !     0.0            0
   !     -float(NSP)    0   ! indicates special data
   !     following NSP floating and NSP integer data

   IF(nsp <= 0.OR.isp /= 0) RETURN
   isp=nr
   IF(nr == 0) THEN
       nr=1
       glder(1)=0.0
       inder(1)=0             ! error counter
   END IF
   IF(nr+nsp+2 > ndim) THEN
       inder(1)=inder(1)+1    ! count errors
       RETURN                 ! record dimension too small
   END IF
   nr=nr+1                   ! zero pair
   glder(nr)=0.0
   inder(nr)=0
   nr=nr+1                   ! nsp and zero
   glder(nr)=-FLOAT(nsp)
   inder(nr)=0
   DO i=1,nsp
       nr=nr+1
       glder(nr)=dergl(i)       ! floating-point
       inder(nr)=label(i)       ! integer
   END DO
   RETURN

   ENTRY kille                             ! stop record
   nr=0    ! reset
   RETURN

   ENTRY endle                             ! end-of-record
   IF(nr > 1) THEN
       WRITE(lun) nr+nr,(glder(i),i=1,nr),(inder(i),i=1,nr)
   END IF
   nr=0    ! reset
   RETURN
   END SUBROUTINE mille
