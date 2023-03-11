
! Code converted using TO_F90 by Alan Miller
! Date: 2012-03-16  Time: 11:09:42

!> \file
!! Print vertical.
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

!> Print vertical.
!!
!! Print the array X of dimension N  (MAX  120) in 6 lines.
!!
!! \param[in]  n    number of numbers
!! \param[in]  x    array of numbers

SUBROUTINE pzvert(n,x)
    USE mpdef

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: i1
    INTEGER(mpi) :: i2
    INTEGER(mpi) :: ia
    INTEGER(mpi) :: ij
    INTEGER(mpi) :: im
    INTEGER(mpi) :: in
    INTEGER(mpi) :: iz
    INTEGER(mpi) :: j
    INTEGER(mpi) :: jl
    INTEGER(mpi) :: jm
    INTEGER(mpi) :: ke
    INTEGER(mpi) :: kl
    INTEGER(mpi) :: kn
    INTEGER(mpi) :: lc
    INTEGER(mpi) :: m
    INTEGER(mpi) :: mx
    REAL(mps) :: fac
    REAL(mps) :: xm
    !

    INTEGER(mpi), INTENT(IN)                      :: n
    REAL(mps), INTENT(IN)                         :: x(n)
    INTEGER(mpi), PARAMETER :: nn=6

    CHARACTER (LEN=66):: px(10)
    CHARACTER (LEN=66)::ch(10)*1
    SAVE
    DATA ch/'0','1','2','3','4','5','6','7','8','9'/
    !     ...
    IF(n <= 0) RETURN
    jm=0
    DO i=1,10
        px(i)=' '
    END DO

    m=MIN(60,n)
    jl=0
    xm=0.0
    DO j=1,m
        IF(ABS(x(j)) > xm) THEN
            xm=ABS(x(j))
            mx=j                             ! index of max
        END IF
        IF(x(j) < 0.0) px(1)(6+j:6+j)='-'  ! negative columns
        IF(x(j) /= 0.0) jl=j                ! last non-zero column
    END DO
    IF(xm == 0.0.OR.jl <= 0) RETURN      ! empty array
    jl=60

    kn=MIN(6,MAX(2,IABS(nn)))
    ke=INT(LOG10(xm*1.0001),mpi)
    IF(xm < 1.0) ke=ke-1
22  fac=10.0**(kn-1-ke)
    ij=NINT(fac*xm,mpi)
    IF(ij >= 10**kn) THEN
        ke=ke+1
        GO TO 22
    END IF
    ia=2+kn

    DO j=1,jl
        ij=NINT(fac*ABS(x(j)),mpi)   ! convert to integer
        im=0
        IF(ij /= 0) THEN
            DO i=1,kn
                IF(ij /= 0) THEN
                    in=MOD(ij,10)   ! last digit
                    ij=ij/10        ! reduce
                    IF(in /= 0.AND.im == 0) im=ia-i+1
                    px(ia-i)(6+j:6+j)=ch(in+1)
                END IF
            END DO
        END IF
        jm=MAX(im,jm)
    END DO

    kl=ke
50  IF(ke >= kn) THEN
        ke=ke-3
        GO TO 50
    END IF
55  IF(ke < 0) THEN
        ke=ke+3
        GO TO 55
    END IF

    in=ke+2                          ! exponent
    iz=kl-ke
    px(in)(6:6)='.'
    px(in)(1:1)='E'
    IF(iz < 0) THEN
        px(in)(2:2)='-'
        iz=-iz
    END IF
    i1=iz/10                         ! insert exponent
    i2=MOD(iz,10)
    px(in)(3:3)=ch(i1+1)
    px(in)(4:4)=ch(i2+1)
    jm=MIN(2+kn,jm)
    jm=MAX(in+1,jm)
    DO j=1,jl    ! '0' for small nonzero values
        IF(x(j) /= 0.0.AND.px(jm-1)(6+j:6+j) == ' ') px(jm-1)(6+j:6+j)='0'
    END DO
    DO i=jm,8
        px(i)=' '
    END DO

    DO j=1,((jl+9)/10)*10            ! index line below
        IF(px(jm-1)(6+j:6+j) == ' ') px(jm-1)(6+j:6+j)='_'
        IF(MOD(j,2) /= 1) THEN
            i=MOD(j,10)+1
            px(jm+1)(6+j:6+j)=ch(i)      ! last digit of even bin numbers
            IF(i == 1) THEN              ! ten'th column
                i=MOD(j/10,10)+1
                px(jm)(6+j:6+j)=ch(i)
      
            END IF
        END IF
    END DO

    DO j=1,jl
        IF(x(j) == x(mx)) THEN
            px(jm)(6+j:6+j)='*'          ! * in max bin
        END IF
    END DO

    jm=jm+1
    IF(nn < 0) jm=jm-2              ! no index line
    lc=((jl+9)/10)*10+6
    DO j=1,jm
        WRITE(*,*)  px(j)(1:lc)       ! print
    !       WRITE(*,101)  PX(J)(1:LC)       ! print
    END DO
    RETURN
!  101 FORMAT(A)
END SUBROUTINE pzvert

!> Vertical print of integer data.
!!
!! Print in up to 60 columns. Optionally average data.
!!
!! \param[in]  n    number of integers
!! \param[in]  list array of integers

SUBROUTINE pivert(n,list)                  !
    USE mpdef

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: l
    INTEGER(mpi) :: ll
    INTEGER(mpi) :: m
    INTEGER(mpi) :: nhist

    INTEGER(mpi), INTENT(IN)                      :: n
    INTEGER(mpi), INTENT(IN)                      :: list(n)

    
    REAL(mps) :: y(60)
    
    SAVE
    !     ...
    ll=(n+59)/60 ! compression factor
    m=0
    i=0
10  nhist=0
    DO l=1,ll
        IF(i+l <= n) nhist=nhist+list(i+l)
    END DO
    i=i+ll
    m=m+1
    y(m)=nhist
    IF(i < n) GO TO 10
    CALL pzvert(m,y)
    RETURN
END SUBROUTINE pivert

!> Vertical print of floating point data.
!!
!! Print in up to 60 columns. Optionally average data.
!!
!! \param[in]  n    number of floats
!! \param[in]  x    array of floats

SUBROUTINE pfvert(n,x)                       ! vert. print fltpt data
    USE mpdef

    IMPLICIT NONE
    REAL(mps) :: dsum
    INTEGER(mpi) :: i
    INTEGER(mpi) :: l
    INTEGER(mpi) :: ll
    INTEGER(mpi) :: m
    REAL(mps) :: y(60)

    INTEGER(mpi), INTENT(IN)                      :: n
    INTEGER(mpi), INTENT(IN)                      :: x(n)

    ll=(n+59)/60 ! compression factor
    m=0
    i=0
20  dsum=0.0
    DO l=1,ll
        IF(i+l <= n) dsum=dsum+x(i+l)
    END DO
    i=i+ll
    m=m+1
    y(m)=REAL(dsum,mps)
    IF(i < n) GO TO 20
    CALL pzvert(m,y)
    RETURN
END SUBROUTINE pfvert

!> Print scale.
!!
!! \param[in]   xa   lower bound of range
!! \param[in]   xb   upper bound of range

SUBROUTINE psvert(xa,xb)                     ! print scale
    USE mpdef

    IMPLICIT NONE
    INTEGER(mpi) :: i
    REAL(mps) :: xc
    !     print scale from XA ... XB


    REAL(mps), INTENT(IN)                         :: xa
    REAL(mps), INTENT(IN)                         :: xb
    REAL(mps) :: sc(7)
    xc=xb
    DO i=1,7
        sc(i)=(REAL(7-i,mps)*xa+REAL(i-1,mps)*xc)/6.0
    END DO
    WRITE(*,101) sc
101 FORMAT(3X,7G10.3)
    RETURN
END SUBROUTINE psvert

