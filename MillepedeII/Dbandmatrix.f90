
! Code converted using TO_F90 by Alan Miller
! Date: 2012-04-18  Time: 19:56:08

!> \file
!! Symmetric (band) matrix routines.
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
!! For the original broken lines implementation by V. Blobel
!! (University Hamburg).
!!\verbatim
!!    *************************************************************
!!    *                                                           *
!!    *   Subroutines for symmetric and symmetric band matrices,  *
!!    *   based on the (square root free) Cholesky decomposition. *
!!    *                                                           *
!!    *************************************************************
!!
!!    All floating point arguments are in DOUBLE PRECISION (and all
!!    entry names start with a D).
!!
!!    The Cholesky decomposition transforms a symmetric matrix W
!!    e.g. the matrix from normal equation of least squares,
!!    according to
!!                          W = L D L^         (L^ means L transposed)
!!    where D is a diagonal matrix and L is a unit triangular matrix
!!    (diagonal elements all ones, all elements above diagonal zero).
!!
!!    The above decomposition allows to solve a matrix equation
!!                        W x = b
!!    in two steps, using an auxiliary vector y:
!!
!!                 solve  L y = b  for y, and
!!
!!               solve D L^ x = y  for x.
!!
!!    The inverse matrix of W can be calculated from the decomposition.
!!
!!    In least-squares normal equations the inverse matrix is equal to
!!    the covariance matrix of the fitted parameters. All diagonal elements
!!    of the inverse matrix, the parameter variances, are positive, and
!!    the matrix is positive-definite (all eigenvalues > 0).
!!
!!    The Cholesky algorithm is stable for a positive-definite matrix.
!!    The standard form of the Cholesky algorithm includes n square roots
!!    for a n-by-n matrix, and is possible only for positive-definite
!!    matrices. The version used here is squareroot-free; this algorithm
!!    does not necessarily break down in the indefinite case, although
!!    it is potentially unstable in this case. All decomposition routines
!!    include a check for singularity, and this check needs an auxiliary
!!    array AUX of dimension n.
!!
!!    Method: The Cholesky algorithm for symmetric matrix decomposition
!!    makes use of the symmetry. The operation count (leading term)
!!    is n**3/6 (compared to n**3/3 for normal Gaussian elimination).
!!    The solution of the two triangular systems involves operations
!!    proportional to n**2.
!!
!!    The real advantage of the Cholesky algorithm is for band matrices,
!!    where all matrix elements outside of a band with total width
!!    (2m+1) around the diagonal are zero. The band structure is kept
!!    in the decomposition, and allows a fast solution of matrix equations.
!!    The operation count (leading term) is proportional to m**2*n
!!    and thus (for fixed m) linear in n. Thus for n=100 and m=2 the
!!    Cholesky algorithm for the band matrix is 1000 times faster than
!!    the standard solution method.
!!
!!    The inverse of a band matrix is a full matrix. It is not necessary
!!    to calculate the inverse, if only the solution for a matrix equation
!!    is needed. However the inverse is often needed, because the elements
!!    of the inverse are the variances and covariances of parameters in
!!    a least-squares fit. The inverse can be calculated afterwards from
!!    the decomposition. Since the inverse matrix is a full matrix, this
!!    has of course an operation count proportional to n**3.
!!
!!    Usually only the elements of the inverse in and around the diagonal
!!    are really needed, and this subset of inverse elements, corresponding
!!    to the original band, can be calculated from the decomposition with
!!    an operation count, which is linear in n. Thus all variances (the
!!    diagonal elements) and covariances between neighbour parameters
!!    are calculated in a short time even for large matrices.
!!
!!    Matrix storage: the mathematical indexing of matrix elements follows
!!    the scheme:
!!
!!                      (  W11   W12   W13 ... W1n  )
!!                      (  W21   W22   W23 ... W2n  )
!!                  W = (  ...   ...   ...     ...  )
!!                      (  ...   ...   ...     ...  )
!!                      (  Wn1   Wn2   Wn3 ... Wnn  )
!!
!!    and a storage in an array would require n**2 words, although the
!!    symmetric matrix has only (n**2+n)/2 different elements, and a band
!!    matrix has less than (m+1)*n different elements. Therefore the
!!    following storage schemes are used.
!!
!!    Symmetric matrix: the elements are in the order
!!            W11   W12   W22   W13   W23   W33   W14 ... Wnn
!!    with total (n**2+n)/2 array elements.
!!
!!    Band matrix: a band matrix of bandwidth m is stored in an array
!!    of dimension W(m+1,n), according to
!!
!!                      W(1,.)    W(2,.)    W(3,.)
!!                     --------------------------------
!!                       W11       W12       W13
!!                       W22       W23       W24
!!                       W33       W34       W35
!!                       ...
!!                       Wnn        -         -
!!
!!    The example is for a bandwidth of m=2; three elements at the end
!!    are unused. The diagonal elements are in the array elements W(1,.).
!!
!!    This package includes subroutines for:
!!
!!       (1) Symmetric matrix W: decomposition, solution, inverse
!!
!!       (2) Symmetric band matrix: decomposition, solution, complete
!!           inverse and band part of the inverse
!!
!!       (3) Symmetric band matrix of band width m=1: decomposition,
!!           solution, complete, inverse and band part of the inverse
!!
!!       (4) Symmetric band matrix of band width m=2: decomposition,
!!           solution, complete, inverse and band part of the inverse
!!
!!       (5) Symmetric matrix with band structure, bordered by full row/col
!!           (not yet included)
!!
!!    The subroutines for a fixed band width of m=1 and of m=2 are
!!    faster than the general routine, because certain loops are avoided
!!    and replaced by the direct code.
!!
!!    Historical remark: the square-root algorithm was invented by the
!!    french Mathematician Andre-Louis Cholesky (1875 - 1918).
!!    Cholesky's method of computing solutions to the normal equations was
!!    published 1924, after the death of Cholesky, by Benoit.
!!    The method received little attention after its publication in 1924.
!!    In 1948 the method was analysed in a paper by Fox, Huskey and
!!    Wilkinson, and in the same year Turing published a paper on the
!!    stability of the method.
!!
!!    The fast method to calculate the band part of the inverse matrix
!!    is usually not mentioned in the literature. An exception is:
!!    I.S.Duff, A.M.Erisman and J.K.Reid, Direct Methods for Sparse
!!    Matrices, Oxford Science Publications, 1986.
!!    The following original work is quoted in this book:
!!    K.Takahashi, J.Fagan and M.Chin, Formation of a sparse bus
!!    impedance matrix and its application to short circuit study.
!!    Proceedings 8th PICA Conference, Minneapolis, Minnesota, 1973
!!    A.M.Erisman and W.F.Tinney, On computing certain elements of the
!!    inverse of a sparse matrix, CACM 18, 177-179, 1975
!!
!!
!!
!!    symmetric          decomposit. solution    inv-element    inverse
!!    ----------------  |-----------|-----------|--------------|-----------|
!!    n x n matrix        DCHDEC      DCHSLV      -              DCHINV
!!    band matrix m,n     DBCDEC      DBCSLV      DBCIEL/DBCINB  DBCINV
!!    bandwidth m=1       DB2DEC      DB2SLV      DB2IEL         -
!!    bandwidth m=2       DB3DEC      DB3SLV      DB3IEL         -
!!
!!    The DB2... and DB3... routines are special routines for a fixed bandwidth
!!    of 1 and 2, they are faster versions of the general DBG... routines.
!!    The complete inverse matrix can be obtained by DBGINV.
!!    The routine DBGPRI can be used to print all types of band matrices.
!!
!!    The decomposition in routines ...DEC replaces (overwrites) the
!!    original matrix (the number of elements is identical). All other
!!    routines require W to be the already decomposed matrix.
!!    The matrix L is a unit lower triangular matrix, with ones on the
!!    diagonal, which have not be stored. Instead the inverse of the
!!    diagonal elements of matrix D are stored in those places.
!!
!!    In the  solution routines ...SLV the array B is the right-hand matrix,
!!    the array is the resulting solution. The same array can be used
!!    for B and X.
!!
!!
!!    W(.) and V(.) are symmetric n-by-n matrices with (N*N+N)/2 elements
!!
!!    SUBROUTINE DCHDEC(W,N, AUX)      ! decomposition, symmetric matrix
!!         ENTRY DCHSLV(W,N,B, X)      ! solution B -> X
!!         ENTRY DCHINV(W,N, V)        ! inversion
!!
!!    SUBROUTINE DCFDEC(W,N)           ! modified composition, symmetric
!!                                     ! alternative to DCHDEC
!!
!!    W(.) and V(.) are band matrices, n rows, band width m (i.e. the total
!!         width of the band is (2m+1).
!!         With MP1 = m +1, the array has dimension W(MP1,N).
!!         The symmetric matrix VS has (N*N+N)/2 elements
!!
!!    SUBROUTINE DBCDEC(W,MP1,N, AUX)  ! decomposition, bandwidth M
!!         ENTRY DBCSLV(W,MP1,N,B, X)  ! solution B -> X
!!         ENTRY DBCIEL(W,MP1,N, V)    ! V = inverse band matrix elements
!!         ENTRY DBCINV(W,MP1,N, VS)   ! V = inverse symmetric matrix
!!
!!    SUBROUTINE DBFDEC(W,MP1,N)       ! modified decomposition, bandwidth M
!!                                     ! alternative to DBCDEC
!!
!!    SUBROUTINE DBCPRB(W,MP1,N)       ! print band matrix
!!    SUBROUTINE DBCPRV(W,MP1,N,B)     ! print corr band matrix and pars
!!
!!    SUBROUTINE DB2DEC(W,N, AUX)      ! decomposition (M=1)
!!         ENTRY DB2SLV(W,N,B, X)      ! solution B -> X
!!         ENTRY DB2IEL(W,N, V)        ! V = inverse band matrix elements
!!
!!    SUBROUTINE DB3DEC(W,N, AUX)      ! decomposition (M=2)
!!         ENTRY DB3SLV(W,N,B, X)      ! solution B -> X
!!         ENTRY DB3IEL(W,N, V)        ! V = inverse band matrix elements
!!\endverbatim


!     (1) Symmetric matrix W: decomposition, solution, inverse

!> Decomposition of symmetric matrix.
!!
!! ENTRY DCHSLV(W,N,B, X) for solution B -> X \n
!! ENTRY DCHINV(W,N,V) for inversion
!!
!! \param [in,out] W    symmetirc matrix
!! \param [in]     N    size
!! \param [in]     AUX  scratch array

SUBROUTINE dchdec(w,n, aux)
    USE mpdef

    implicit none
    INTEGER(mpi) :: i
    INTEGER(mpi) :: ii
    INTEGER(mpi) :: j
    INTEGER(mpi) :: jj
    INTEGER(mpi) :: k
    INTEGER(mpi) :: kk
    INTEGER(mpi) :: l
    INTEGER(mpi) :: m


    REAL(mpd), INTENT(IN OUT)         :: w(*)
    INTEGER(mpi), INTENT(IN)                      :: n
    REAL(mpd), INTENT(OUT)            :: aux(n)

    REAL(mpd) :: b(*),x(*),v(*),suma,ratio
    !     ...
    DO i=1,n
        aux(i)=16.0_mpd*w((i*i+i)/2) ! save diagonal elements
    END DO
    ii=0
    DO i=1,n
        ii=ii+i
        IF(w(ii)+aux(i) /= aux(i)) THEN     ! GT
            w(ii)=1.0_mpd/w(ii)                ! (I,I) div !
        ELSE
            w(ii)=0.0_mpd
        END IF
        jj=ii
        DO j=i+1,n
            ratio=w(i+jj)*w(ii)              ! (I,J) (I,I)
            kk=jj
            DO k=j,n
                w(kk+j)=w(kk+j)-w(kk+i)*ratio   ! (K,J) (K,I)
                kk=kk+k
            END DO ! K
            w(i+jj)=ratio                    ! (I,J)
            jj=jj+j
        END DO ! J
    END DO ! I


    RETURN

    ENTRY dchslv(w,n,b, x)       ! solution B -> X
    WRITE(*,*) 'before copy'
    DO i=1,n
        x(i)=b(i)
    END DO
    WRITE(*,*) 'after copy'
    ii=0
    DO i=1,n
        suma=x(i)
        DO k=1,i-1
            suma=suma-w(k+ii)*x(k)             ! (K,I)
        END DO
        x(i)=suma
        ii=ii+i
    END DO
    WRITE(*,*) 'after forward'
    DO i=n,1,-1
        suma=x(i)*w(ii)                    ! (I,I)
        kk=ii
        DO k=i+1,n
            suma=suma-w(kk+i)*x(k)             ! (K,I)
            kk=kk+k
        END DO
        x(i)=suma
        ii=ii-i
    END DO
    WRITE(*,*) 'after backward'
    RETURN

    ENTRY dchinv(w,n,v)         ! inversion
    ii=(n*n-n)/2
    DO i=n,1,-1
        suma=w(ii+i)                       ! (I,I)
        DO j=i,1,-1
            DO k=j+1,n
                l=MIN(i,k)
                m=MAX(i,k)
                suma=suma-w(j+(k*k-k)/2)*v(l+(m*m-m)/2) ! (J,K) (I,K)
            END DO
            v(ii+j)=suma                      ! (I,J)
            suma=0.0_mpd
        END DO
        ii=ii-i+1
    END DO
END SUBROUTINE dchdec

!> Etimate condition.
!!
!! \param [in] W    symmetric matrix
!! \param [in] N    size
!! \param [in] AUX  scratch array
!! \return condition

REAL(mps) FUNCTION condes(w,n,aux)
    USE mpdef

    implicit none
    REAL(mps) :: cond
    INTEGER(mpi) :: i
    INTEGER(mpi) :: ir
    INTEGER(mpi) :: is
    INTEGER(mpi) :: k
    INTEGER(mpi) :: kk
    REAL(mps) :: xla1
    REAL(mps) :: xlan


    REAL(mpd), INTENT(IN)             :: w(*)
    INTEGER(mpi), INTENT(IN)                      :: n
    REAL(mpd), INTENT(IN OUT)         :: aux(n)

    REAL(mpd) :: suma,sumu,sums

    ir=1
    is=1
    DO i=1,n
        IF(w((i*i+i)/2) < w((is*is+is)/2)) is=i ! largest Dii
        IF(w((i*i+i)/2) > w((ir*ir+ir)/2)) ir=i ! smallest Dii
    END DO

    sumu=0.0     ! find smallest eigenvalue
    sums=0.0
    DO i=n,1,-1  ! backward
        suma=0.0
        IF(i == ir) suma=1.0_mpd
        kk=(i*i+i)/2
        DO k=i+1,n
            suma=suma-w(kk+i)*aux(k)             ! (K,I)
            kk=kk+k
        END DO
        aux(i)=suma
        sumu=sumu+aux(i)*aux(i)
    END DO
    xlan=REAL(w((ir*ir+ir)/2)*SQRT(sumu),mps)
    IF(xlan /= 0.0) xlan=1.0/xlan

    DO i=1,n
        IF(i == is) THEN
            sums=1.0_mpd
        ELSE IF(i > is) THEN
            sums=sums+w(is+(i*i-i)/2)**2
        END IF
    END DO       ! is Ws
    xla1=0.0
    IF(w((is*is+is)/2) /= 0.0) xla1=REAL(SQRT(sums)/w((is*is+is)/2),mps)

    cond=0.0
    IF(xla1 > 0.0.AND.xlan > 0.0) cond=xla1/xlan
    !     estimated condition
    condes=cond
END FUNCTION condes


!     (2) Symmetric band matrix: decomposition, solution, complete
!                                inverse and band part of the inverse
!> Decomposition of symmetric band matrix.
!!
!! ENTRY DBCSLV(W,MP1,N,B, X) for solution B -> X \n
!! ENTRY DBCIEL(W,MP1,N, V), V = inverse band matrix elements \n
!! ENTRY DBCINB(W,MP1,N, VS), VS = band part of inverse symmetric matrix \n
!! ENTRY DBCINV(W,MP1,N, VS), V = inverse symmetric matrix
!!
!! \param [in,out] W    symmetric band matrix
!! \param [in]     MP1  band width (M) + 1
!! \param [in]     N    size
!! \param [in]     AUX  scratch array

SUBROUTINE dbcdec(w,mp1,n, aux)  ! decomposition, bandwidth M
    USE mpdef

    implicit none
    INTEGER(mpi) :: i
    INTEGER(mpi) :: j
    INTEGER(mpi) :: k
    !     M=MP1-1                                    N*M(M-1) dot operations

    REAL(mpd), INTENT(IN OUT)         :: w(mp1,n)
    INTEGER(mpi), INTENT(IN)                      :: mp1
    INTEGER(mpi), INTENT(IN)                      :: n
    REAL(mpd), INTENT(OUT)            :: aux(n)
    ! decompos
    REAL(mpd) :: v(mp1,n),b(n),x(n), vs(*),rxw
    !     ...
    DO i=1,n
        aux(i)=16.0_mpd*w(1,i) ! save diagonal elements
    END DO
    DO i=1,n
        IF(w(1,i)+aux(i) /= aux(i)) THEN
            w(1,i)=1.0/w(1,i)
        ELSE
            w(1,i)=0.0_mpd  ! singular
        END IF
        DO j=1,MIN(mp1-1,n-i)
            rxw=w(j+1,i)*w(1,i)
            DO k=1,MIN(mp1-1,n-i)+1-j
                w(k,i+j)=w(k,i+j)-w(k+j,i)*rxw
            END DO
            w(j+1,i)=rxw
        END DO
    END DO
    RETURN

    ENTRY dbcslv(w,mp1,n,b, x)  ! solution B -> X
    !                                                N*(2M-1) dot operations
    DO i=1,n
        x(i)=b(i)
    END DO
    DO i=1,n ! forward substitution
        DO j=1,MIN(mp1-1,n-i)
            x(j+i)=x(j+i)-w(j+1,i)*x(i)
        END DO
    END DO
    DO i=n,1,-1 ! backward substitution
        rxw=x(i)*w(1,i)
        DO j=1,MIN(mp1-1,n-i)
            rxw=rxw-w(j+1,i)*x(j+i)
        END DO
        x(i)=rxw
    END DO
    RETURN

    ENTRY dbciel(w,mp1,n, v)    ! V = inverse band matrix elements
    !                                               N*M*(M-1) dot operations
    DO i=n,1,-1
        rxw=w(1,i)
        DO j=i,MAX(1,i-mp1+1),-1
            DO k=j+1,MIN(n,j+mp1-1)
                rxw=rxw-v(1+ABS(i-k),MIN(i,k))*w(1+k-j,j)
            END DO
            v(1+i-j,j)=rxw
            rxw=0.0
        END DO
    END DO
    RETURN

    ENTRY dbcinb(w,mp1,n, vs)   ! VS = band part of inverse symmetric matrix
    !                                             N*M*(M-1) dot operations
    DO i=n,1,-1
        rxw=w(1,i)
        DO j=i,MAX(1,i-mp1+1),-1
            DO k=j+1,MIN(n,j+mp1-1)
                rxw=rxw-vs((MAX(i,k)*(MAX(i,k)-1))/2+MIN(i,k))*w(1+k-j,j)
            END DO
            vs((i*i-i)/2+j)=rxw
            rxw=0.0
        END DO
    !       DO J=MAX(1,I-MP1+1)-1,1,-1
    !        VS((I*I-I)/2+J)=0.0
    !       END DO
    END DO
    RETURN

    ENTRY dbcinv(w,mp1,n, vs)   ! V = inverse symmetric matrix
    !                                             N*N/2*(M-1) dot operations
    DO i=n,1,-1
        rxw=w(1,i)
        DO j=i,1,-1
            DO k=j+1,MIN(n,j+mp1-1)
                rxw=rxw-vs((MAX(i,k)*(MAX(i,k)-1))/2+MIN(i,k))*w(1+k-j,j)
            END DO
            vs((i*i-i)/2+j)=rxw
            rxw=0.0
        END DO
    END DO
    RETURN
END SUBROUTINE dbcdec

!> Print corr band matrix and pars.
!!
!! \param [in] W    symmetric band matrix
!! \param [in] MP1  band width (M) + 1
!! \param [in] N    size
!! \param [in] B    vector

SUBROUTINE dbcprv(w,mp1,n,b)
    USE mpdef

    implicit none
    INTEGER(mpi) :: i
    INTEGER(mpi) :: j
    INTEGER(mpi) :: nj
    REAL(mps) :: rho


    REAL(mpd), INTENT(IN OUT)         :: w(mp1,n)
    INTEGER(mpi), INTENT(IN)                      :: mp1
    INTEGER(mpi), INTENT(IN)                      :: n
    REAL(mpd), INTENT(OUT)            :: b(n)

    REAL(mpd) :: ERR
    INTEGER(mpi) :: irho(5)
    !     ...
    WRITE(*,*) ' '
    WRITE(*,101)

    DO i=1,n
        ERR=SQRT(w(1,i))
        nj=0
        DO j=2,mp1
            IF(i+1-j > 0.AND.nj < 5) THEN
                nj=nj+1
                rho=REAL(w(j,i+1-j)/(ERR*SQRT(w(1,i+1-j))),mps)
                irho(nj)=NINT(100.0*ABS(rho),mpi)
                IF(rho < 0.0) irho(nj)=-irho(nj)
            END IF
        END DO
        WRITE(*,102) i,b(i),ERR,(irho(j),j=1,nj)
    END DO
    WRITE(*,103)
101 FORMAT(5X,'i   Param',7X,'error',7X,'  c(i,i-1) c(i,i-2)'/)
102 FORMAT(1X,i5,2G12.4,1X,5I9)
103 FORMAT(33X,'(correlation coefficients in percent)')
END SUBROUTINE dbcprv

!> Print band matrix.
!!
!! \param [in] W    symmetric band matrix
!! \param [in] MP1  band width (M) + 1
!! \param [in] N    size

SUBROUTINE dbcprb(w,mp1,n)
    USE mpdef

    implicit none
    INTEGER(mpi) :: i
    INTEGER(mpi) :: j


    REAL(mpd), INTENT(IN OUT)         :: w(mp1,n)
    INTEGER(mpi), INTENT(IN)                      :: mp1
    INTEGER(mpi), INTENT(IN)                      :: n


    !     ...
    IF(mp1 > 6) RETURN
    WRITE(*,*) ' '
    WRITE(*,101)
    DO i=1,n
        WRITE(*,102) i,(w(j,i),j=1,mp1)
    END DO
    WRITE(*,*) ' '
101 FORMAT(5X,'i   Diag  ')
102 FORMAT(1X,i5,6G12.4)
END SUBROUTINE dbcprb


!     (3) Symmetric band matrix of band width m=1: decomposition,
!            solution, complete, inverse and band part of the inverse

!> Decomposition (M=1).
!!
!!    W is a symmetrix positive definite band matrix of bandwidth 1(+1).
!!    W(1,.) are the diagonal elements, W(2,.) is the next
!!    diagonals; W(2,N) is never referenced.
!!    AUX is an auxiliary array of length N.
!!    W is decomposed to L D Lt, where D = diagonal and L unit triangular.
!!    A row is set to zero, if the diagonal element is reduced in previous
!!    steps by a word length (i.e. global correlation coefficient large).
!!    The resulting L and D replace W: the diagonal elements W(1,...) will
!!    contain the inverse of the D-elements; the diagonal elements  of L are
!!    all 1 and not stored. The other elements of L are stored in the
!!    corresponding elements of W.
!!
!! ENTRY DB2SLV(W,N,B, X), solution B -> X \n
!! ENTRY DB2IEL(W,N, V), V = inverse band matrix elements
!!
!! \param [in,out] W    symmetric band matrix
!! \param [in]     N    size
!! \param [in]     AUX  scratch array

SUBROUTINE db2dec(w,n, aux)
    USE mpdef

    implicit none
    INTEGER(mpi) :: i


    REAL(mpd), INTENT(IN OUT)         :: w(2,n)
    INTEGER(mpi), INTENT(IN OUT)                  :: n
    REAL(mpd), INTENT(OUT)            :: aux(n)

    REAL(mpd) :: v(2,n),b(n),x(n), rxw

    DO i=1,n
        aux(i)=16.0_mpd*w(1,i) ! save diagonal elements
    END DO
    DO i=1,n-1
        IF(w(1,i)+aux(i) /= aux(i)) THEN
            w(1,i)=1.0_mpd/w(1,i)
            rxw=w(2,i)*w(1,i)
            w(1,i+1)=w(1,i+1)-w(2,i)*rxw
            w(2,i)=rxw
        ELSE ! singular
            w(1,i)=0.0_mpd
            w(2,i)=0.0_mpd
        END IF
    END DO
    IF(w(1,n)+aux(n) > aux(n)) THEN           ! N
        w(1,n)=1.0_mpd/w(1,n)
    ELSE ! singular
        w(1,n)=0.0_mpd
    END IF
    RETURN

    ENTRY db2slv(w,n,b, x)      ! solution B -> X
    !     The equation W(original)*X=B is solved for X; input is B in vector X.
    DO i=1,n
        x(i)=b(i)
    END DO
    DO i=1,n-1           ! by forward substitution
        x(i+1)=x(i+1)-w(2,i)*x(i)
    END DO
    x(n)=x(n)*w(1,n)     ! and backward substitution
    DO i=n-1,1,-1
        x(i)=x(i)*w(1,i)-w(2,i)*x(i+1)
    END DO
    RETURN

    ENTRY db2iel(w,n, v)        ! V = inverse band matrix elements
    !     The band elements of the inverse of W(original) are calculated,
    !     and stored in V in the same order as in W.
    !     Remaining elements of the inverse are not calculated.
    v(1,n  )= w(1,n)
    v(2,n-1)=-v(1,n  )*w(2,n-1)
    DO i=n-1,3,-1
        v(1,i  )= w(1,i  )-v(2,i  )*w(2,i  )
        v(2,i-1)=-v(1,i  )*w(2,i-1)
    END DO
    v(1,2)= w(1,2)-v(2,2)*w(2,2)
    v(2,1)=-v(1,2)*w(2,1)
    v(1,1)= w(1,1)-v(2,1)*w(2,1)
END SUBROUTINE db2dec


!     (4) Symmetric band matrix of band width m=2: decomposition,
!            solution, complete, inverse and band part of the inverse

!> Decomposition (M=2).
!!
!!    W is a symmetrix positive definite band matrix of bandwidth 2(+1).
!!    W(1,.) are the diagonal elements, W(2,.) and W(3,.) are the next
!!    diagonals; W(3,N-1), W(2,N) and W(3,N) are never referenced.
!!    AUX is an auxiliary array of length N.
!!    W is decomposed to L D Lt, where D = diagonal and L unit triangular.
!!    A row is set to zero, if the diagonal element is reduced in previous
!!    steps by a word length (i.e. global correlation coefficient large).
!!    The resulting L and D replace W: the diagonal elements W(1,...) will
!!    contain the inverse of the D-elements; the diagonal elements  of L are
!!    all 1 and not stored. The other elements of L are stored in the
!!    corresponding elements of W.
!!
!! ENTRY DB3SLV(W,N,B, X), solution B -> X \n
!! ENTRY DB3IEL(W,N, V), V = inverse band matrix elements
!!
!! \param [in,out] W    symmetric band matrix
!! \param [in]     N    size
!! \param [in]     AUX  scratch array

SUBROUTINE db3dec(w,n, aux)      ! decomposition (M=2)
    USE mpdef

    implicit none
    INTEGER(mpi) :: i


    REAL(mpd), INTENT(IN OUT)         :: w(3,n)
    INTEGER(mpi), INTENT(IN OUT)                  :: n
    REAL(mpd), INTENT(OUT)            :: aux(n)
    ! decompos

    REAL(mpd) :: v(3,n),b(n),x(n), rxw

    DO i=1,n
        aux(i)=16.0_mpd*w(1,i) ! save diagonal elements
    END DO
    DO i=1,n-2
        IF(w(1,i)+aux(i) /= aux(i)) THEN
            w(1,i)=1.0_mpd/w(1,i)
            rxw=w(2,i)*w(1,i)
            w(1,i+1)=w(1,i+1)-w(2,i)*rxw
            w(2,i+1)=w(2,i+1)-w(3,i)*rxw
            w(2,i)=rxw
            rxw=w(3,i)*w(1,i)
            w(1,i+2)=w(1,i+2)-w(3,i)*rxw
            w(3,i)=rxw
        ELSE ! singular
            w(1,i)=0.0_mpd
            w(2,i)=0.0_mpd
            w(3,i)=0.0_mpd
        END IF
    END DO
    IF(w(1,n-1)+aux(n-1) > aux(n-1)) THEN
        w(1,n-1)=1.0_mpd/w(1,n-1)
        rxw=w(2,n-1)*w(1,n-1)
        w(1,n)=w(1,n)-w(2,n-1)*rxw
        w(2,n-1)=rxw
    ELSE ! singular
        w(1,n-1)=0.0_mpd
        w(2,n-1)=0.0_mpd
    END IF
    IF(w(1,n)+aux(n) > aux(n)) THEN
        w(1,n)=1.0_mpd/w(1,n)
    ELSE ! singular
        w(1,n)=0.0_mpd
    END IF
    RETURN

    ENTRY db3slv(w,n,b, x)      ! solution B -> X
    DO i=1,n
        x(i)=b(i)
    END DO
    DO i=1,n-2           ! by forward substitution
        x(i+1)=x(i+1)-w(2,i)*x(i)
        x(i+2)=x(i+2)-w(3,i)*x(i)
    END DO
    x(n)=x(n)-w(2,n-1)*x(n-1)
    x(n)=x(n)*w(1,n)     ! and backward substitution
    x(n-1)=x(n-1)*w(1,n-1)-w(2,n-1)*x(n)
    DO i=n-2,1,-1
        x(i)=x(i)*w(1,i)-w(2,i)*x(i+1)-w(3,i)*x(i+2)
    END DO
    RETURN

    ENTRY db3iel(w,n, v)        ! V = inverse band matrix elements
    !     The band elements of the inverse of W(original) are calculated,
    !     and stored in V in the same order as in W.
    !     Remaining elements of the inverse are not calculated.
    v(1,n  )= w(1,n)
    v(2,n-1)=-v(1,n  )*w(2,n-1)
    v(3,n-2)=-v(2,n-1)*w(2,n-2)-v(1,n  )*w(3,n-2)
    v(1,n-1)= w(1,n-1) -v(2,n-1)*w(2,n-1)
    v(2,n-2)=-v(1,n-1)*w(2,n-2)-v(2,n-1)*w(3,n-2)
    v(3,n-3)=-v(2,n-2)*w(2,n-3)-v(1,n-1)*w(3,n-3)
    DO i=n-2,3,-1
        v(1,i  )= w(1,i  ) -v(2,i  )*w(2,i  )-v(3,i)*w(3,i  )
        v(2,i-1)=-v(1,i  )*w(2,i-1)-v(2,i)*w(3,i-1)
        v(3,i-2)=-v(2,i-1)*w(2,i-2)-v(1,i)*w(3,i-2)
    END DO
    v(1,2)= w(1,2) -v(2,2)*w(2,2)-v(3,2)*w(3,2)
    v(2,1)=-v(1,2)*w(2,1)-v(2,2)*w(3,1)
    v(1,1)= w(1,1) -v(2,1)*w(2,1)-v(3,1)*w(3,1)
END SUBROUTINE db3dec


!     (5) Symmetric matrix with band structure, bordered by full row/col
!          - is not yet included -

!      SUBROUTINE BSOLV1(N,CU,RU,CK,RK,CH,     BK,UH,   AU) ! 1
!     Input:  CU = 3*N array         replaced by decomposition
!             RU   N array rhs
!             CK   diagonal element
!             RK   rhs
!             CH   N-vector

!     Aux:    AU   N-vector auxliliary array

!     Result: FK   curvature
!             BK   variance
!             UH   smoothed data points


!      DOUBLE PRECISION CU(3,N),CI(3,N),CK,BK,AU(N),UH(N)
!     ...
!      CALL BDADEC(CU,3,N, AU)    ! decomposition
!      CALL DBASLV(CU,3,N, RU,UH)  ! solve for zero curvature
!      CALL DBASLV(CU,3,N, CH,AU)  ! solve for aux. vector
!      CTZ=0.0D0
!      ZRU=0.0D0
!      DO I=1,N
!       CTZ=CTZ+CH(I)*AU(I)        ! cT z
!       ZRU=ZRU+RY(I)*AU(I)        ! zT ru
!      END DO
!      BK=1.0D0/(CK-CTZ)           ! variance of curvature
!      FK=BK   *(RK-ZRU)           ! curvature
!      DO I=1,N
!       UH(I)=UH(I)-FK*AU(I)       ! smoothed data points
!      END DO
!      RETURN

!      ENTRY BINV1(N,CU,CI, FK,AU)
!      DOUBLE PRECISION CI(3,N)
!     ...
!      CALL DBAIBM(CU,3,N, CI)           ! block part of inverse
!      DO I=1,N
!       CI(1,I)=CI(1,I)+FK*AU(I)*AU(I)   ! diagonal elements
!       IF(I.LT.N) CI(2,I)=CI(2,I)+FK*AU(I)*AU(I+1) ! next diagonal
!       IF(I.LT.N-1) CI(3,I)=CI(3,I)+FK*AU(I)*AU(I+2) ! next diagonal
!      END DO

!      END

!> Decomposition of symmetric matrix.
!!
!!    Modified Cholesky decomposition,
!!    Philip E.Gill, Walter Murray and Margarete H.Wright:
!!      Practical Optimization, Academic Press, 1981
!!
!! \param [in,out] W    symmetirc matrix
!! \param [in]     N    size

SUBROUTINE dcfdec(w,n)
    USE mpdef

    IMPLICIT NONE
    REAL(mpd), INTENT(OUT)            :: w(*)
    INTEGER(mpi), INTENT(IN)                      :: n
    INTEGER(mpi) :: i,j,k
    REAL(mpd) :: epsm,gamm,xchi,beta,delta,theta

    epsm=EPSILON(epsm) ! machine precision
    gamm=0.0_mpd   ! max diagonal element
    xchi=0.0_mpd   ! max off-diagonal element
    DO k=1,n
        gamm=MAX(gamm,ABS(w((k*k+k)/2)))
        DO j=k+1,n
            xchi=MAX(xchi,ABS(w((j*j-j)/2+k)))
        END DO
    END DO
    beta=SQRT(MAX(gamm,xchi/MAX(1.0_mpd,SQRT(REAL(n*n-1,mpd))),epsm))
    delta=epsm*MAX(1.0_mpd,gamm+xchi)

    DO k=1,n
        DO i=1,k-1
            w((k*k-k)/2+i)=w((k*k-k)/2+i)*w((i*i+i)/2)
        END DO
        DO j=k+1,n
            DO i=1,k-1
                w((j*j-j)/2+k)=w((j*j-j)/2+k)-w((k*k-k)/2+i)*w((j*j-j)/2+i)
            END DO
        END DO
        theta=0.0_mpd
        DO j=k+1,n
            theta=MAX(theta,ABS(w((j*j-j)/2+k)))
        END DO
        w((k*k+k)/2)=1.0_mpd/MAX(ABS(w((k*k+k)/2)),(theta/beta)**2,delta)
        DO j=k+1,n
            w((j*j+j)/2)=w((j*j+j)/2)-w((j*j-j)/2+k)**2*w((k*k+k)/2)
        END DO
    END DO ! K

END SUBROUTINE dcfdec

!> Decomposition of symmetric band matrix.
!!
!!    Band matrix modified Cholesky decomposition,
!!    Philip E.Gill, Walter Murray and Margarete H.Wright:
!!      Practical Optimization, Academic Press, 1981
!!
!! \param [in,out] W    symmetric band matrix
!! \param [in]     MP1  band width (M) + 1
!! \param [in]     N    size

SUBROUTINE dbfdec(w,mp1,n)
    USE mpdef

    IMPLICIT NONE
    REAL(mpd), INTENT(OUT)            :: w(mp1,n)
    INTEGER(mpi), INTENT(IN OUT)                  :: mp1
    INTEGER(mpi), INTENT(IN)                      :: n
    INTEGER(mpi) :: i,j,k
    REAL(mpd) :: epsm,gamm,xchi,beta,delta,theta

    epsm=EPSILON(epsm) ! machine precision
    gamm=0.0_mpd   ! max diagonal element
    xchi=0.0_mpd   ! max off-diagonal element
    DO k=1,n
        gamm=MAX(gamm,ABS(w(1,k)))
        DO j=2,MIN(mp1,n-k+1)
            xchi=MAX(xchi,ABS(w(j,k)))
        END DO
    END DO
    beta=SQRT(MAX(gamm,xchi/MAX(1.0_mpd,SQRT(REAL(n*n-1,mpd))),epsm))
    delta=epsm*MAX(1.0_mpd,gamm+xchi)

    DO k=1,n
        DO i=2,MIN(mp1,k)
            w(i,k-i+1)=w(i,k-i+1)*w(1,k-i+1)
        END DO
        DO j=2,MIN(mp1,n-k+1)
            DO i=MAX(2,j+k+1-mp1),k
                w(j,k)=w(j,k)-w(k-i+2,i-1)*w(j-i+k+1,i-1)
            END DO
        END DO
        theta=0.0_mpd
        DO j=2,MIN(mp1,n-k+1)
            theta=MAX(theta,ABS(w(j,k)))
        END DO
        w(1,k)=1.0_mpd/MAX(ABS(w(1,k)),(theta/beta)**2,delta)
        DO j=2,MIN(mp1,n-k+1)
            w(1,k+j-1)=w(1,k+j-1)-w(1,k)*w(j,k)**2
        END DO
    END DO ! K

END SUBROUTINE dbfdec


