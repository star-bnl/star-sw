!

! Code converted using TO_F90 by Alan Miller
! Date: 2012-03-03  Time: 17:00:00

! C. Kleinwort , DESY-FH1, www.terascale.de
! February 2011, Analysis Centre: Statistics Tools Group

!> \file
!! General linear algebra routines.
!!
!! \author Volker Blobel, University Hamburg, 2005-2009 (initial Fortran77 version)
!! \author Claus Kleinwort, DESY (maintenance and developement)
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
!!
!! ***** Collection of utility routines from V. Blobel *****

!======================================================================

!     V. Blobel, Univ. Hamburg
!     Numerical subprograms used in MP-II: matrix equations,
!        and matrix products, double precision
!
!     Solution by inversion
!        SQMINV
!
!     Solution by diagonalization
!        DEVROT
!
!     Solution by Cholesky decomposition of bordered band matrix
!CHK     SQMIBB
!
!     Matrix/vector products
!-        DBDOT     dot vector product
!-        DBAXPY    multiplication and addition
!-        DBSVX     symmetric matrix vector
!        DBGAX     general matrix vector
!        DBAVAT    AVAT product
!-        DBMPRV    print parameter and matrix
!CHK     DBPRV     print matrix

!----------------------------------------------------------------------
!> Matrix inversion.
!!
!! Obtain solution of a system of linear equations with symmetric
!! matrix (V * X = B) and the inverse.
!!
!! Method of solution is by elimination selecting the pivot on the
!! diagonal each stage. The rank of the matrix is returned in NRANK.
!! For NRANK ne N, all remaining rows and cols of the resulting
!! matrix V and the corresponding elements of B are set to zero.
!!
!! \param [in,out] V symmetric N-by-N matrix in symmetric storage mode
!!                   (V(1) = V11, V(2) = V12, V(3) = V22, V(4) = V13, ...),
!!                   replaced by inverse matrix
!! \param [in,out] B N-vector, replaced by solution vector
!! \param [in]     N size of V, B
!! \param [out]    NRANK rank of matrix V
!! \param [out]    DIAG  double precision scratch array
!! \param [out]    NEXT  integer aux array

SUBROUTINE sqminv(v,b,n,nrank,diag,next)
    IMPLICIT NONE
    INTEGER :: i
    INTEGER :: ij
    INTEGER :: j
    INTEGER :: jj
    INTEGER :: jk
    INTEGER :: jl
    INTEGER :: k
    INTEGER :: kk
    INTEGER :: l
    INTEGER :: last
    INTEGER :: lk
    INTEGER :: next0

    DOUBLE PRECISION, INTENT(IN OUT)         :: v(*)
    DOUBLE PRECISION, INTENT(OUT)            :: b(n)
    INTEGER, INTENT(IN)                      :: n
    INTEGER, INTENT(OUT)                     :: nrank
    DOUBLE PRECISION, INTENT(OUT)            :: diag(n)
    INTEGER, INTENT(OUT)                     :: next(n)

    DOUBLE PRECISION :: vkk
    DOUBLE PRECISION ::vjk

    DOUBLE PRECISION, PARAMETER :: eps=1.0D-10
    !     ..
    next0=1
    DO i=1,n
        next(i)=i+1                ! set "next" pointer
        diag(i)=ABS(v((i*i+i)/2))  ! save abs of diagonal elements
    END DO
    next(n)=-1                  ! end flag

    nrank=0
    loop: DO i=1,n                    ! start of loop
        k  =0
        vkk=0.0D0
  
        j=next0
        last=0
        DO WHILE (j > 0)
            jj=(j*j+j)/2
            IF(ABS(v(jj)) > MAX(ABS(vkk),eps*diag(j))) THEN
                vkk=v(jj)
                k=j
                l=last
            END IF
            last=j
            j=next(last)
        END DO
  
        IF(k /= 0) THEN            ! pivot found
            kk=(k*k+k)/2
            IF(l == 0) THEN
                next0=next(k)
            ELSE
                next(l)=next(k)
            END IF
            next(k)=0               ! index is used, reset
            nrank=nrank+1           ! increase rank and ...
            vkk    =1.0/vkk
            v(kk)  =-vkk
            b(k)   =b(k)*vkk
            jk     =kk-k
            jl     =0
            DO j=1,n                ! elimination
                IF(j == k) THEN
                    jk=kk
                    jl=jl+j
                ELSE
                    IF(j < k) THEN
                        jk=jk+1
                    ELSE
                        jk=jk+j-1
                    END IF
                    vjk  =v(jk)
                    v(jk)=vkk*vjk
                    b(j) =b(j)-b(k)*vjk
                    lk   =kk-k
                    DO l=1,j
                        jl=jl+1
                        IF(l == k) THEN
                            lk=kk
                        ELSE
                            IF(l < k) THEN
                                lk=lk+1
                            ELSE
                                lk=lk+l-1
                            END IF
                            v(jl)=v(jl)-v(lk)*vjk
                        END IF
                    END DO
                END IF
            END DO
        ELSE
            DO k=1,n
                IF(next(k) /= 0) THEN
                    b(k)=0.0D0       ! clear vector element
                    DO j=1,k
                        IF(next(j) /= 0) v((k*k-k)/2+j)=0.0D0  ! clear matrix row/col
                    END DO
                END IF
            END DO
            EXIT loop
        END IF
    END DO loop           ! end of loop
    DO ij=1,(n*n+n)/2
        v(ij)=-v(ij)      ! finally reverse sign of all matrix elements
    END DO
END SUBROUTINE sqminv

!> Diagonalization.
!!
!!  Determination  of  eigenvalues  and  eigenvectors of
!!  symmetric matrix V by  Householder method
!!
!! \param [in]  n      size of matrix
!! \param [out] diag   diagonal elements
!! \param [out] u      transformation matrix
!! \param [in]  v      symmetric matrix, unchanged
!! \param [out] work   work array
!! \param [out] iwork  work array

SUBROUTINE devrot(n,diag,u,v,work,iwork)
    IMPLICIT NONE

    INTEGER, INTENT(IN)                      :: n
    DOUBLE PRECISION, INTENT(OUT)            :: diag(n)
    DOUBLE PRECISION, INTENT(OUT)            :: u(n,n)
    DOUBLE PRECISION, INTENT(IN)             :: v(*)
    DOUBLE PRECISION, INTENT(OUT)            :: work(n)
    INTEGER, INTENT(OUT)                     :: iwork(n)

    INTEGER, PARAMETER :: itmax=30
    DOUBLE PRECISION, PARAMETER :: tol=1.0D-16
    DOUBLE PRECISION, PARAMETER :: eps=1.0D-16

    DOUBLE PRECISION :: f
    DOUBLE PRECISION :: g
    DOUBLE PRECISION :: h
    DOUBLE PRECISION :: sh
    DOUBLE PRECISION :: hh
    DOUBLE PRECISION :: b
    DOUBLE PRECISION :: p
    DOUBLE PRECISION :: r
    DOUBLE PRECISION :: s
    DOUBLE PRECISION :: c
    DOUBLE PRECISION :: workd

    INTEGER :: ij
    INTEGER :: i
    INTEGER :: j
    INTEGER :: k
    INTEGER :: l
    INTEGER :: m
    INTEGER :: ll
    !     ...
    !     1. part: symmetric matrix V reduced to tridiagonal from
    ij=0
    DO i=1,n
        DO j=1,i
            ij=ij+1
            u(i,j)=v(ij)  ! copy half of symmetric matirx
        END DO
    END DO

    DO i=n,2,-1
        l=i-2
        f=u(i,i-1)
        g=0.0D0
        IF(l /= 0) THEN
            DO k=1,l
                IF(ABS(u(i,k)) > tol) g=g+u(i,k)*u(i,k)
            END DO
            h=g+f*f
        END IF
        IF(g < tol) THEN   ! G too small
            work(i)=f           ! skip transformation
            h   =0.0D0
        ELSE
            l=l+1
            sh=SQRT(h)
            IF(f >= 0.0D0) sh=-sh
            g=sh
            work(i)=sh
            h=h-f*g
            u(i,i-1)=f-g
            f=0.0D0
            DO j=1,l
                u(j,i)=u(i,j)/h
                g=0.0D0
                !          form element of a u
                DO k=1,j
                    IF(ABS(u(j,k)) > tol.AND.ABS(u(i,k)) > tol) THEN
                        g=g+u(j,k)*u(i,k)
                    END IF
                END DO
                DO k=j+1,l
                    IF(ABS(u(k,j)) > tol.AND.ABS(u(i,k)) > tol) THEN
                        g=g+u(k,j)*u(i,k)
                    END IF
                END DO
                work(j)=g/h
                f=f+g*u(j,i)
            END DO
            !         form k
            hh=f/(h+h)
            !         form reduced a
            DO j=1,l
                f=u(i,j)
                work(j)=work(j)-hh*f
                g=work(j)
                DO k=1,j
                    u(j,k)=u(j,k)-f*work(k)-g*u(i,k)
                END DO
            END DO
        END IF
        diag(i)=h
    END DO

    diag(1)=0.0D0
    work(1)=0.0D0

    !     accumulation of transformation matrices
    DO i=1,n
        IF(diag(i) /= 0.0) THEN
            DO j=1,i-1
                g=0.0D0
                DO k=1,i-1
                    g=g+u(i,k)*u(k,j)
                END DO
                DO k=1,i-1
                    u(k,j)=u(k,j)-g*u(k,i)
                END DO
            END DO
        END IF
        diag(i)=u(i,i)
        u(i,i)=1.0D0
        DO j=1,i-1
            u(i,j)=0.0D0
            u(j,i)=0.0D0
        END DO
    END DO

    !     2. part: diagonalization of tridiagonal matrix
    DO i=2,n
        work(i-1)=work(i)
    END DO
    work(n)=0.0D0
    b=0.0D0
    f=0.0D0

    DO l=1,n
        j=0
        h=eps*(ABS(diag(l))+ABS(work(l)))
        IF(b < h) b=h
        DO m=l,n
            IF(ABS(work(m)) <= b) GO TO 10 ! look for small sub-diagonal element
        END DO
        m=l
10      IF(m == l) GO TO 30
        !      next iteration
20      IF(j == itmax) THEN
            WRITE(*,*) 'DEVROT: Iteration limit reached'
            STOP
        END IF
        j=j+1
        g=diag(l)
        p=(diag(l+1)-g)/(2.0D0*work(l))
        r=SQRT(1.0D0+p*p)
        diag(l)=work(l)
        IF(p < 0.0D0) diag(l)=diag(l)/(p-r)
        IF(p >= 0.0D0) diag(l)=diag(l)/(p+r)
        h=g-diag(l)
        DO i=l+1,n
            diag(i)=diag(i)-h
        END DO
        f=f+h
        !      QL transformation
        p=diag(m)
        c=1.0D0
        s=0.0D0
        DO i=m-1,l,-1          ! reverse loop
            g=c*work(i)
            h=c*p
            IF(ABS(p) >= ABS(work(i))) THEN
                c=work(i)/p
                r=SQRT(1.0D0+c*c)
                work(i+1)=s*p*r
                s=c/r
                c=1.0D0/r
            ELSE
                c=p/work(i)
                r=SQRT(1.0D0+c*c)
                work(i+1)=s*work(i)*r
                s=1.0D0/r
                c=c/r
            END IF
            p=c*diag(i)-s*g
            diag(i+1)=h+s*(c*g+s*diag(i))
            !       form vector
            DO k=1,n
                h=u(k,i+1)
                u(k,i+1)=s*u(k,i)+c*h
                u(k,i)=c*u(k,i)-s*h
            END DO
        END DO
        work(l)=s*p
        diag(l)=c*p
        IF(ABS(work(l)) > b) GO TO 20 ! next iteration
30      diag(l)=diag(l)+f
    END DO
    DO i=1,n
        iwork(i)=i
    END DO

    m=1
40  m=1+3*m    ! determine initial increment
    IF(m <= n) GO TO 40
50  m=m/3
    DO j=1,n-m ! sort with increment M
        l=j
60      IF(diag(iwork(l+m)) > diag(iwork(l))) THEN ! compare
            ll=iwork(l+m)      ! exchange the two index values
            iwork(l+m)=iwork(l)
            iwork(l)=ll
            l=l-m
            IF(l > 0) GO TO 60
        END IF
    END DO
    IF(m > 1) GO TO 50

    DO i=1,n
        IF(iwork(i) /= i) THEN
            !         move vector from position I to the work area
            workd=diag(i)
            DO l=1,n
                work(l)=u(l,i)
            END DO
            k=i
70          j=k
            k=iwork(j)
            iwork(j)=j
            IF(k /= i) THEN
                !            move vector from position K to the (free) position J
                diag(j)=diag(k)
                DO l=1,n
                    u(l,j)=u(l,k)
                END DO
                GO TO 70
            END IF
            !         move vector from the work area to position J
            diag(j)=workd
            DO l=1,n
                u(l,j)=work(l)
            END DO
        END IF
    END DO
END SUBROUTINE devrot

!> Multiply general M-by-N matrix A and N-vector X.
!!
!! \param [in]  A general M-by-N matrix (A11 A12 ... A1N  A21 A22 ...)
!! \param [in]  X N vector
!! \param [out] Y = M vector
!! \param [in]  M rows of A
!! \param [in]  N columns of A

SUBROUTINE dbgax(a,x,y,m,n)
    IMPLICIT NONE
    INTEGER :: i
    INTEGER :: ij
    INTEGER :: j

    DOUBLE PRECISION, INTENT(IN)             :: a(*)
    DOUBLE PRECISION, INTENT(IN)             :: x(*)
    DOUBLE PRECISION, INTENT(OUT)            :: y(*)
    INTEGER, INTENT(IN)                      :: m
    INTEGER, INTENT(IN)                      :: n

    !     ...
    ij=0
    DO i=1,m
        y(i)=0.0D0
        DO j=1,n
            ij=ij+1
            y(i)=y(i)+a(ij)*x(j)
        END DO
    END DO
END SUBROUTINE dbgax

!> A V AT product (similarity).
!!
!! Multiply symmetric N-by-N matrix from the left with general M-by-N
!! matrix and from the right with the transposed of the same general
!! matrix to form symmetric M-by-M matrix (used for error propagation).
!!
!! \param [in]  V symmetric N-by-N matrix
!! \param [in]  A general M-by-N matrix
!! \param [out] W symmetric M-by-M matrix
!! \param [in]  M rows of A
!! \param [in]  N columns of A
!!

SUBROUTINE dbavat(v,a,w,n,m)
    IMPLICIT NONE
    INTEGER :: i
    INTEGER :: ij
    INTEGER :: ijs
    INTEGER :: il
    INTEGER :: j
    INTEGER :: jk
    INTEGER :: k
    INTEGER :: l
    INTEGER :: lk
    INTEGER :: lkl

    DOUBLE PRECISION, INTENT(IN)             :: v(*)
    DOUBLE PRECISION, INTENT(IN)             :: a(*)
    DOUBLE PRECISION, INTENT(OUT)            :: w(*)
    INTEGER, INTENT(IN)                      :: n
    INTEGER, INTENT(IN)                      :: m

    DOUBLE PRECISION :: cik

    !     ...
    DO i=1,(m*m+m)/2
        w(i)=0.0                ! reset output matrix
    END DO
    il=-n
    ijs=0
    DO i=1,m                 ! do I
        ijs=ijs+i-1             !
        il=il+n                 !
        lkl=0                   !
        DO k=1,n                !   do K
            cik=0.0D0              !
            lkl=lkl+k-1            !
            lk=lkl                 !
            DO l=1,k               !     do L
                lk=lk+1               !     .
                cik=cik+a(il+l)*v(lk) !     .
            END DO                 !     end do L
            DO l=k+1,n             !     do L
                lk=lk+l-1             !     .
                cik=cik+a(il+l)*v(lk) !     .
            END DO                 !     end do L
            jk=k                   !
            ij=ijs                 !
            DO j=1,i               !     do J
                ij=ij+1               !     .
                w(ij)=w(ij)+cik*a(jk) !     .
                jk=jk+n               !     .
            END DO                 !     end do J
        END DO                  !   end do K
    END DO                   ! end do I
END SUBROUTINE dbavat

!                                                 090817 C. Kleinwort, DESY-FH1
!> Bordered band matrix.
!!
!! Obtain solution of a system of linear equations with symmetric
!! bordered band matrix (V * X = B), on request inverse is calculated.
!! For band part root-free Cholesky decomposition and forward/backward
!! substitution is used.
!!
!! Use decomposition in border and band part for block matrix algebra:
!!
!!     | A  Ct |   | x1 |   | b1 |        , A  is the border part
!!     |       | * |    | = |    |        , Ct is the mixed part
!!     | C  D  |   | x2 |   | b2 |        , D  is the band part
!!
!! Explicit inversion of D is avoided by using solution X of D*X=C (X=D^-1*C,
!! obtained from Cholesky decomposition and forward/backward substitution)
!!
!!     | x1 |   | E*b1 - E*Xt*b2 |        , E^-1 = A-Ct*D^-1*C = A-Ct*X
!!     |    | = |                |
!!     | x2 |   |  x   - X*x1    |        , x is solution of D*x=b2 (x=D^-1*b2)
!!
!! Inverse matrix is:
!!
!!     |  E   -E*Xt          |
!!     |                     |            , only band part of (D^-1 + X*E*Xt)
!!     | -X*E  D^-1 + X*E*Xt |              is calculated for inv=1
!!
!!
!! \param [in,out] V symmetric N-by-N matrix in symmetric storage mode
!!                   (V(1) = V11, V(2) = V12, V(3) = V22, V(4) = V13, ...),
!!                   replaced by inverse matrix
!! \param [in,out] B N-vector, replaced by solution vector
!! \param [in]     N size of V, B
!! \param [in]     NBDR  border size
!! \param [in]     NBND  band width
!! \param [in]     INV   =1 calculate band part of inverse (for pulls),
!!                       >1 calculate complete inverse
!! \param [out]    NRANK rank of matrix V
!!

SUBROUTINE sqmibb(v,b,n,nbdr,nbnd,inv,nrank)

    ! Double precision scratch arrays:
    !     VBND(N*(NBND+1)) = storage of band   part
    !     VBDR(N* NBDR)    = storage of border part
    !     AUX (N* NBDR)    = intermediate results

    ! cost[dot ops] ~= (N-NBDR)*(NBDR+NBND+1)**2 + NBDR**3/3 (leading term, solution only)

    USE gbltraj, ONLY: maxFitPar, maxBandWidth, maxBorderSize
    IMPLICIT NONE
    INTEGER :: i
    INTEGER :: ib
    INTEGER :: ij
    INTEGER :: ioff
    INTEGER :: ip
    INTEGER :: ip1
    INTEGER :: ip2
    INTEGER :: is
    INTEGER :: j
    INTEGER :: j0
    INTEGER :: jb
    INTEGER :: joff
    INTEGER :: mp1
    INTEGER :: nb1
    INTEGER :: nmb
    INTEGER :: npri
    INTEGER :: nrankb

    DOUBLE PRECISION, INTENT(IN OUT)         :: v(*)
    DOUBLE PRECISION, INTENT(OUT)            :: b(n)
    INTEGER, INTENT(IN)                      :: n
    INTEGER, INTENT(IN)                      :: nbdr
    INTEGER, INTENT(IN)                      :: nbnd
    INTEGER, INTENT(IN)                      :: inv
    INTEGER, INTENT(OUT)                     :: nrank

    DOUBLE PRECISION :: vbnd(maxFitPar*(maxBandWidth+1))
    DOUBLE PRECISION :: vbdr(maxFitPar*maxBorderSize)
    DOUBLE PRECISION :: aux(maxFitPar*maxBorderSize)
    DOUBLE PRECISION :: vbk((maxBorderSize*maxBorderSize+maxBorderSize)/2)
    DOUBLE PRECISION :: vzru(maxBorderSize)
    DOUBLE PRECISION ::scdiag(maxBorderSize)
    INTEGER :: scflag(maxBorderSize)

    SAVE npri
    DATA npri / 100 /
    !           ...
    nrank=0
    nb1=nbdr+1
    mp1=nbnd+1
    nmb=n-nbdr
    !     copy band part
    DO i=nb1,n
        ip=(i*(i+1))/2
        is=0
        DO j=i,MIN(n,i+nbnd)
            ip=ip+is
            is=j
            ib=j-i+1
            vbnd(ib+(i-nb1)*mp1)=v(ip)
        END DO
    END DO
    !     copy border part
    IF (nbdr > 0) THEN
        ioff=0
        DO i=1,nbdr
            ip=(i*(i+1))/2
            is=0
            DO j=i,n
                ip=ip+is
                is=j
                vbdr(ioff+j)=v(ip)
            END DO
            ioff=ioff+n
        END DO
    END IF

    CALL dbcdec(vbnd,mp1,nmb,aux)
    ! use? CALL DBFDEC(VBND,MP1,NMB) ! modified decomp., numerically more stable
    !      CALL DBCPRB(VBND,MP1,NMB)
    ip=1
    DO i=1, nmb
        IF (vbnd(ip) <= 0.0D0) THEN
            npri=npri-1
            IF (npri >= 0) THEN
                IF (vbnd(ip) == 0.0D0) THEN
                    PRINT *, ' SQMIBB matrix singular', n, nbdr, nbnd
                ELSE
                    PRINT *, ' SQMIBB matrix not positive definite', n, nbdr, nbnd
                END IF
            END IF
            !           return zeros
            DO ip=1,n
                b(ip)=0.0D0
            END DO
            DO ip=1,(n*n+n)/2
                v(ip)=0.0D0
            END DO
            RETURN
        END IF
        ip=ip+mp1
    END DO
    nrank=nmb

    IF (nbdr == 0) THEN ! special case NBDR=0
  
        CALL dbcslv(vbnd,mp1,nmb,b,b)
        IF (inv > 0) THEN
            IF (inv > 1) THEN
                CALL dbcinv(vbnd,mp1,nmb,v)
            ELSE
                CALL dbcinb(vbnd,mp1,nmb,v)
            END IF
        END IF
  
    ELSE ! general case NBDR>0
  
        ioff=nb1
        DO ib=1,nbdr
            !           solve for aux. vectors
            CALL dbcslv(vbnd,mp1,nmb,vbdr(ioff),aux(ioff))
            !           zT ru
            vzru(ib)=b(ib)
            DO i=0,nmb-1
                vzru(ib)=vzru(ib)-b(nb1+i)*aux(ioff+i)
            END DO
            ioff=ioff+n
        END DO
        !        solve for band part only
        CALL dbcslv(vbnd,mp1,nmb,b(nb1),b(nb1))
        !        Ck - cT z
        ip=0
        ioff=nb1
        DO ib=1,nbdr
            joff=nb1
            DO jb=1,ib
                ip=ip+1
                vbk(ip)=v(ip)
                DO i=0,nmb-1
                    vbk(ip)=vbk(ip)-vbdr(ioff+i)*aux(joff+i)
                END DO
                joff=joff+n
            END DO
            ioff=ioff+n
        END DO
        !        solve border part
        CALL sqminv(vbk,vzru,nbdr,nrankb,scdiag,scflag)
        IF (nrankb == nbdr) THEN
            nrank=nrank+nbdr
        ELSE
            npri=npri-1
            IF (npri >= 0) PRINT *, ' SQMIBB undef border ', n, nbdr, nbnd, nrankb
            DO ib=1,nbdr
                vzru(ib)=0.0D0
            END DO
            DO ip=(nbdr*nbdr+nbdr)/2,1,-1
                vbk(ip)=0.0D0
            END DO
        END IF
        !        smoothed data points
        ioff=nb1
        DO ib=1, nbdr
            b(ib) = vzru(ib)
            DO i=0,nmb-1
                b(nb1+i)=b(nb1+i)-b(ib)*aux(ioff+i)
            END DO
            ioff=ioff+n
        END DO
        !        inverse requested ?
        IF (inv > 0) THEN
            IF (inv > 1) THEN
                CALL dbcinv(vbnd,mp1,nmb,v)
            ELSE
                CALL dbcinb(vbnd,mp1,nmb,v)
            END IF
            !           expand/correct from NMB to N
            ip1=(nmb*nmb+nmb)/2
            ip2=(n*n+n)/2
            DO i=nmb-1,0,-1
                j0=0
                IF (inv == 1) j0=MAX(0,i-nbnd)
                DO j=i,j0,-1
                    v(ip2)=v(ip1)
                    ioff=nb1
                    DO ib=1,nbdr
                        joff=nb1
                        DO jb=1,nbdr
                            ij=MAX(ib,jb)
                            ij=(ij*ij-ij)/2+MIN(ib,jb)
                            v(ip2)=v(ip2)+vbk(ij)*aux(ioff+i)*aux(joff+j)
                            joff=joff+n
                        END DO
                        ioff=ioff+n
                    END DO
                    ip1=ip1-1
                    ip2=ip2-1
                END DO
                ip1=ip1-j0
                ip2=ip2-j0
      
                DO ib=nbdr,1,-1
                    v(ip2)=0.0D0
                    joff=nb1
                    DO jb=1,nbdr
                        ij=MAX(ib,jb)
                        ij=(ij*ij-ij)/2+MIN(ib,jb)
                        v(ip2)=v(ip2)-vbk(ij)*aux(i+joff)
                        joff=joff+n
                    END DO
                    ip2=ip2-1
                END DO
            END DO
    
            DO ip=(nbdr*nbdr+nbdr)/2,1,-1
                v(ip2)=vbk(ip)
                ip2=ip2-1
            END DO
    
        END IF
    END IF

END SUBROUTINE sqmibb

!> Prints the symmetric N-by-N matrix V
!!
!! \param [in] LUN output unit
!! \param [in] V   N-by-N matrix V (symmetric storage)
!! \param [in] N   size
!!

SUBROUTINE dbprv(lun,v,n)
    IMPLICIT NONE
    INTEGER :: i
    INTEGER :: ip
    INTEGER :: ipe
    INTEGER :: ipn
    INTEGER :: ips
    INTEGER :: k

    INTEGER, INTENT(IN OUT)                  :: lun
    DOUBLE PRECISION, INTENT(IN OUT)         :: v(*)
    INTEGER, INTENT(IN)                      :: n

    INTEGER, PARAMETER :: istp=6

    WRITE(lun,101)

    DO i=1,n
        ips=(i*i-i)/2
        ipe=ips+i
        ip =ips
100 CONTINUE
    ipn=ip+istp
    WRITE(lun,102), i, ip+1-ips, (v(k),k=ip+1,MIN(ipn,ipe))
    IF (ipn < ipe) THEN
        ip=ipn
        GO TO 100
    END IF
END DO
RETURN
101 FORMAT(1X,'--- DBPRV -----------------------------------')
102 FORMAT(1X,2I3,6G12.4)

END SUBROUTINE dbprv
