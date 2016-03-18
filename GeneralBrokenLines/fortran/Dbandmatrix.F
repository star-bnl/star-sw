!> \file
!! Symmetric (band) matrix routines.
!!
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
!!    french Mathematician André-Louis Cholesky (1875 - 1918).
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
 

*     (1) Symmetric matrix W: decomposition, solution, inverse

!> Decomposition of symmetric matrix.
!!
!! ENTRY DCHSLV(W,N,B, X) for solution B -> X \n
!! ENTRY DCHINV(W,N,V) for inversion
!!
!! \param [in,out] W    symmetirc matrix
!! \param [in]     N    size
!! \param [in]     AUX  scratch array
*
      SUBROUTINE DCHDEC(W,N, AUX)
*
      INTEGER N
      DOUBLE PRECISION W(*),AUX(N),B(*),X(*),V(*),SUM,RATIO
*     ...
      DO I=1,N
       AUX(I)=16.0D0*W((I*I+I)/2) ! save diagonal elements
      END DO
      II=0
      DO I=1,N
       II=II+I
       IF(W(II)+AUX(I).NE.AUX(I)) THEN     ! GT 
          W(II)=1.0D0/W(II)                ! (I,I) div !
       ELSE
          W(II)=0.0D0
       END IF  
       JJ=II
       DO J=I+1,N
        RATIO=W(I+JJ)*W(II)              ! (I,J) (I,I)
        KK=JJ
        DO K=J,N
         W(KK+J)=W(KK+J)-W(KK+I)*RATIO   ! (K,J) (K,I)
         KK=KK+K
        END DO ! K
        W(I+JJ)=RATIO                    ! (I,J)
        JJ=JJ+J
       END DO ! J
      END DO ! I


      RETURN

      ENTRY DCHSLV(W,N,B, X)       ! solution B -> X
      WRITE(*,*) 'before copy'
      DO I=1,N
       X(I)=B(I)
      END DO
      WRITE(*,*) 'after copy'   
      II=0
      DO I=1,N
       SUM=X(I)
       DO K=1,I-1
        SUM=SUM-W(K+II)*X(K)             ! (K,I)
       END DO
       X(I)=SUM
       II=II+I
      END DO
      WRITE(*,*) 'after forward'
      DO I=N,1,-1
       SUM=X(I)*W(II)                    ! (I,I)
       KK=II
       DO K=I+1,N
        SUM=SUM-W(KK+I)*X(K)             ! (K,I)
        KK=KK+K
       END DO
       X(I)=SUM
       II=II-I
      END DO
      WRITE(*,*) 'after backward'
      RETURN

      ENTRY DCHINV(W,N,V)         ! inversion
      II=(N*N-N)/2
      DO I=N,1,-1
       SUM=W(II+I)                       ! (I,I)
       DO J=I,1,-1
        DO K=J+1,N
         L=MIN(I,K)
         M=MAX(I,K)
         SUM=SUM-W(J+(K*K-K)/2)*V(L+(M*M-M)/2) ! (J,K) (I,K)
        END DO
        V(II+J)=SUM                      ! (I,J)
        SUM=0.0D0
       END DO
       II=II-I+1
      END DO
      END

!> Etimate condition.
!!
!! \param [in] W    symmetric matrix
!! \param [in] N    size
!! \param [in] AUX  scratch array
!! \return condition
*
      FUNCTION CONDES(W,N,AUX)
*
      INTEGER N
      DOUBLE PRECISION W(*),AUX(N),SUM,SUMU,SUMS
*
      IR=1
      IS=1
      DO I=1,N
       IF(W((I*I+I)/2).LT.W((IS*IS+IS)/2)) IS=I ! largest Dii
       IF(W((I*I+I)/2).GT.W((IR*IR+IR)/2)) IR=I ! smallest Dii
      END DO

      SUMU=0.0     ! find smallest eigenvalue        
      DO I=N,1,-1  ! backward 
       SUM=0.0
       IF(I.EQ.IR) SUM=1.0D0
       KK=(I*I+I)/2
       DO K=I+1,N
        SUM=SUM-W(KK+I)*AUX(K)             ! (K,I)
        KK=KK+K
       END DO
       AUX(I)=SUM
       SUMU=SUMU+AUX(I)*AUX(I)
      END DO
      XLAN=SNGL(W((IR*IR+IR)/2)*DSQRT(SUMU))
      IF(XLAN.NE.0.0) XLAN=1.0/XLAN
      
      DO I=1,N
       IF(I.EQ.IS) THEN
          SUMS=1.0D0
       ELSE IF(I.GT.IS) THEN
          SUMS=SUMS+W(IS+(I*I-I)/2)**2
       END IF
      END DO       ! is Ws
      XLA1=0.0 
      IF(W((IS*IS+IS)/2).NE.0.0) XLA1=SNGL(DSQRT(SUMS)/W((IS*IS+IS)/2))
      
      COND=0.0
      IF(XLA1.GT.0.0.AND.XLAN.GT.0.0) COND=XLA1/XLAN 
*     estimated condition
      CONDES=COND
      END 
  

*     (2) Symmetric band matrix: decomposition, solution, complete
*                                inverse and band part of the inverse
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
*
      SUBROUTINE DBCDEC(W,MP1,N, AUX)  ! decomposition, bandwidth M
*     M=MP1-1                                    N*M(M-1) dot operations
      INTEGER MP1,N
      DOUBLE PRECISION W(MP1,N),V(MP1,N),B(N),X(N),AUX(N),VS(*),RXW
*     ...
      DO I=1,N
       AUX(I)=16.0D0*W(1,I) ! save diagonal elements
      END DO
      DO I=1,N
       IF(W(1,I)+AUX(I).NE.AUX(I)) THEN
          W(1,I)=1.0/W(1,I)
       ELSE
          W(1,I)=0.0D0  ! singular
       END IF
       DO J=1,MIN(MP1-1,N-I)
        RXW=W(J+1,I)*W(1,I)            
        DO K=1,MIN(MP1-1,N-I)+1-J
         W(K,I+J)=W(K,I+J)-W(K+J,I)*RXW 
        END DO
        W(J+1,I)=RXW
       END DO
      END DO
      RETURN

      ENTRY DBCSLV(W,MP1,N,B, X)  ! solution B -> X
*                                                N*(2M-1) dot operations
      DO I=1,N
       X(I)=B(I)
      END DO 
      DO I=1,N ! forward substitution
       DO J=1,MIN(MP1-1,N-I)
        X(J+I)=X(J+I)-W(J+1,I)*X(I)   
       END DO
      END DO
      DO I=N,1,-1 ! backward substitution
       RXW=X(I)*W(1,I)                 
       DO J=1,MIN(MP1-1,N-I)
        RXW=RXW-W(J+1,I)*X(J+I)         
       END DO
       X(I)=RXW
      END DO
      RETURN

      ENTRY DBCIEL(W,MP1,N, V)    ! V = inverse band matrix elements
*                                               N*M*(M-1) dot operations
      DO I=N,1,-1
       RXW=W(1,I)
       DO J=I,MAX(1,I-MP1+1),-1
        DO K=J+1,MIN(N,J+MP1-1)
         RXW=RXW-V(1+ABS(I-K),MIN(I,K))*W(1+K-J,J)
        END DO
        V(1+I-J,J)=RXW
        RXW=0.0
       END DO  
      END DO
      RETURN

      ENTRY DBCINB(W,MP1,N, VS)   ! VS = band part of inverse symmetric matrix
*                                             N*M*(M-1) dot operations
      DO I=N,1,-1 
       RXW=W(1,I) 
       DO J=I,MAX(1,I-MP1+1),-1
        DO K=J+1,MIN(N,J+MP1-1)
         RXW=RXW-VS((MAX(I,K)*(MAX(I,K)-1))/2+MIN(I,K))*W(1+K-J,J)
        END DO 
        VS((I*I-I)/2+J)=RXW
        RXW=0.0
       END DO
C       DO J=MAX(1,I-MP1+1)-1,1,-1
C        VS((I*I-I)/2+J)=0.0
C       END DO
      END DO
      RETURN
      
      ENTRY DBCINV(W,MP1,N, VS)   ! V = inverse symmetric matrix
*                                             N*N/2*(M-1) dot operations
      DO I=N,1,-1 
       RXW=W(1,I) 
       DO J=I,1,-1
        DO K=J+1,MIN(N,J+MP1-1)
         RXW=RXW-VS((MAX(I,K)*(MAX(I,K)-1))/2+MIN(I,K))*W(1+K-J,J)
        END DO 
        VS((I*I-I)/2+J)=RXW
        RXW=0.0
       END DO
      END DO
      RETURN
      END

!> Print corr band matrix and pars.
!!
!! \param [in] W    symmetric band matrix
!! \param [in] MP1  band width (M) + 1
!! \param [in] N    size
!! \param [in] B    vector
*
      SUBROUTINE DBCPRV(W,MP1,N,B)
*
      INTEGER MP1,N
      DOUBLE PRECISION W(MP1,N),B(N),ERR
      INTEGER IRHO(5)
*     ...
      WRITE(*,*) ' '
      WRITE(*,101)
      DO I=1,N
       ERR=DSQRT(W(1,I))
       NJ=0
       DO J=2,MP1
        IF(I+1-J.GT.0.AND.NJ.LT.5) THEN
           NJ=NJ+1
           RHO=SNGL(W(J,I+1-J)/(ERR*DSQRT(W(1,I+1-J))))
           IRHO(NJ)=IFIX(100.0*ABS(RHO)+0.5)
           IF(RHO.LT.0.0) IRHO(NJ)=-IRHO(NJ)
        END IF
       END DO
       WRITE(*,102) I,B(I),ERR,(IRHO(J),J=1,NJ)
      END DO
      WRITE(*,103)
  101 FORMAT(5X,'i   Param',7X,'error',7X,'  c(i,i-1) c(i,i-2)'/)
  102 FORMAT(1X,I5,2G12.4,1X,5I9)
  103 FORMAT(33X,'(correlation coefficients in percent)')
      END

!> Print band matrix.
!!
!! \param [in] W    symmetric band matrix
!! \param [in] MP1  band width (M) + 1
!! \param [in] N    size
*
      SUBROUTINE DBCPRB(W,MP1,N)
*
      INTEGER MP1,N
      DOUBLE PRECISION W(MP1,N)
*     ...
      IF(MP1.GT.6) RETURN
      WRITE(*,*) ' '
      WRITE(*,101)
      DO I=1,N
       WRITE(*,102) I,(W(J,I),J=1,MP1)
      END DO
      WRITE(*,*) ' '
  101 FORMAT(5X,'i   Diag  ')
  102 FORMAT(1X,I5,6G12.4)
      END

   
*     (3) Symmetric band matrix of band width m=1: decomposition,
*            solution, complete, inverse and band part of the inverse

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
*
      SUBROUTINE DB2DEC(W,N, AUX)
*
      INTEGER N
      DOUBLE PRECISION W(2,N),V(2,N),B(N),X(N),AUX(N),RXW
*
      DO I=1,N
       AUX(I)=16.0D0*W(1,I) ! save diagonal elements
      END DO
      DO I=1,N-1
       IF(W(1,I)+AUX(I).NE.AUX(I)) THEN
          W(1,I)=1.0D0/W(1,I)
          RXW=W(2,I)*W(1,I)
          W(1,I+1)=W(1,I+1)-W(2,I)*RXW
          W(2,I)=RXW
       ELSE ! singular
          W(1,I)=0.0D0
          W(2,I)=0.0D0
       END IF
      END DO
      IF(W(1,N)+AUX(N).GT.AUX(N)) THEN           ! N
         W(1,N)=1.0D0/W(1,N)
      ELSE ! singular
         W(1,N)=0.0D0
      END IF
      RETURN

      ENTRY DB2SLV(W,N,B, X)      ! solution B -> X
*     The equation W(original)*X=B is solved for X; input is B in vector X.
      DO I=1,N
       X(I)=B(I)
      END DO
      DO I=1,N-1           ! by forward substitution
       X(I+1)=X(I+1)-W(2,I)*X(I)
      END DO
      X(N)=X(N)*W(1,N)     ! and backward substitution
      DO I=N-1,1,-1
       X(I)=X(I)*W(1,I)-W(2,I)*X(I+1)
      END DO
      RETURN

      ENTRY DB2IEL(W,N, V)        ! V = inverse band matrix elements 
*     The band elements of the inverse of W(original) are calculated,
*     and stored in V in the same order as in W.
*     Remaining elements of the inverse are not calculated.
      V(1,N  )= W(1,N)
      V(2,N-1)=-V(1,N  )*W(2,N-1)
      DO I=N-1,3,-1
       V(1,I  )= W(1,I  )-V(2,I  )*W(2,I  )
       V(2,I-1)=-V(1,I  )*W(2,I-1)
      END DO
      V(1,2)= W(1,2)-V(2,2)*W(2,2)
      V(2,1)=-V(1,2)*W(2,1)
      V(1,1)= W(1,1)-V(2,1)*W(2,1)
      END

   
*     (4) Symmetric band matrix of band width m=2: decomposition,
*            solution, complete, inverse and band part of the inverse

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
*
      SUBROUTINE DB3DEC(W,N, AUX)      ! decomposition (M=2) 
*
      INTEGER N
      DOUBLE PRECISION W(3,N),V(3,N),B(N),X(N),AUX(N),RXW
*
      DO I=1,N
       AUX(I)=16.0D0*W(1,I) ! save diagonal elements
      END DO
      DO I=1,N-2
       IF(W(1,I)+AUX(I).NE.AUX(I)) THEN
          W(1,I)=1.0D0/W(1,I)
          RXW=W(2,I)*W(1,I)
          W(1,I+1)=W(1,I+1)-W(2,I)*RXW
          W(2,I+1)=W(2,I+1)-W(3,I)*RXW
          W(2,I)=RXW
          RXW=W(3,I)*W(1,I)
          W(1,I+2)=W(1,I+2)-W(3,I)*RXW
          W(3,I)=RXW
       ELSE ! singular
          W(1,I)=0.0D0
          W(2,I)=0.0D0
          W(3,I)=0.0D0
       END IF
      END DO
      IF(W(1,N-1)+AUX(N-1).GT.AUX(N-1)) THEN
          W(1,N-1)=1.0D0/W(1,N-1)
          RXW=W(2,N-1)*W(1,N-1)
          W(1,N)=W(1,N)-W(2,N-1)*RXW
          W(2,N-1)=RXW
      ELSE ! singular
         W(1,N-1)=0.0D0
         W(2,N-1)=0.0D0
      END IF
      IF(W(1,N)+AUX(N).GT.AUX(N)) THEN
         W(1,N)=1.0D0/W(1,N)
      ELSE ! singular
         W(1,N)=0.0D0
      END IF
      RETURN

      ENTRY DB3SLV(W,N,B, X)      ! solution B -> X
      DO I=1,N
       X(I)=B(I)
      END DO  
      DO I=1,N-2           ! by forward substitution 
       X(I+1)=X(I+1)-W(2,I)*X(I)
       X(I+2)=X(I+2)-W(3,I)*X(I)
      END DO
      X(N)=X(N)-W(2,N-1)*X(N-1) 
      X(N)=X(N)*W(1,N)     ! and backward substitution
      X(N-1)=X(N-1)*W(1,N-1)-W(2,N-1)*X(N)
      DO I=N-2,1,-1 
       X(I)=X(I)*W(1,I)-W(2,I)*X(I+1)-W(3,I)*X(I+2)
      END DO   
      RETURN
      
      ENTRY DB3IEL(W,N, V)        ! V = inverse band matrix elements
*     The band elements of the inverse of W(original) are calculated,
*     and stored in V in the same order as in W.
*     Remaining elements of the inverse are not calculated. 
      V(1,N  )= W(1,N)
      V(2,N-1)=-V(1,N  )*W(2,N-1) 
      V(3,N-2)=-V(2,N-1)*W(2,N-2)-V(1,N  )*W(3,N-2) 
      V(1,N-1)= W(1,N-1)
     +         -V(2,N-1)*W(2,N-1) 
      V(2,N-2)=-V(1,N-1)*W(2,N-2)-V(2,N-1)*W(3,N-2)
      V(3,N-3)=-V(2,N-2)*W(2,N-3)-V(1,N-1)*W(3,N-3)
      DO I=N-2,3,-1
       V(1,I  )= W(1,I  )
     +          -V(2,I  )*W(2,I  )-V(3,I)*W(3,I  ) 
       V(2,I-1)=-V(1,I  )*W(2,I-1)-V(2,I)*W(3,I-1)
       V(3,I-2)=-V(2,I-1)*W(2,I-2)-V(1,I)*W(3,I-2)
      END DO
      V(1,2)= W(1,2)
     +       -V(2,2)*W(2,2)-V(3,2)*W(3,2)
      V(2,1)=-V(1,2)*W(2,1)-V(2,2)*W(3,1)
      V(1,1)= W(1,1)
     +       -V(2,1)*W(2,1)-V(3,1)*W(3,1)
      END


*     (5) Symmetric matrix with band structure, bordered by full row/col
*          - is not yet included -

c      SUBROUTINE BSOLV1(N,CU,RU,CK,RK,CH,     BK,UH,   AU) ! 1
*     Input:  CU = 3*N array         replaced by decomposition 
*             RU   N array rhs       
*             CK   diagonal element
*             RK   rhs
*             CH   N-vector
*
*     Aux:    AU   N-vector auxliliary array   
*
*     Result: FK   curvature
*             BK   variance 
*             UH   smoothed data points
*             
         
c      DOUBLE PRECISION CU(3,N),CI(3,N),CK,BK,AU(N),UH(N)
*     ...
c      CALL BDADEC(CU,3,N, AU)    ! decomposition
c      CALL DBASLV(CU,3,N, RU,UH)  ! solve for zero curvature
c      CALL DBASLV(CU,3,N, CH,AU)  ! solve for aux. vector
c      CTZ=0.0D0
c      ZRU=0.0D0
c      DO I=1,N
c       CTZ=CTZ+CH(I)*AU(I)        ! cT z
c       ZRU=ZRU+RY(I)*AU(I)        ! zT ru       
c      END DO 
c      BK=1.0D0/(CK-CTZ)           ! variance of curvature
c      FK=BK   *(RK-ZRU)           ! curvature
c      DO I=1,N
c       UH(I)=UH(I)-FK*AU(I)       ! smoothed data points
c      END DO
c      RETURN

c      ENTRY BINV1(N,CU,CI, FK,AU)
c      DOUBLE PRECISION CI(3,N)
*     ...
c      CALL DBAIBM(CU,3,N, CI)           ! block part of inverse
c      DO I=1,N
c       CI(1,I)=CI(1,I)+FK*AU(I)*AU(I)   ! diagonal elements
c       IF(I.LT.N) CI(2,I)=CI(2,I)+FK*AU(I)*AU(I+1) ! next diagonal
c       IF(I.LT.N-1) CI(3,I)=CI(3,I)+FK*AU(I)*AU(I+2) ! next diagonal 
c      END DO

c      END 

!> Decomposition of symmetric matrix.
!!
!!    Modified Cholesky decomposition,
!!    Philip E.Gill, Walter Murray and Margarete H.Wright:
!!      Practical Optimization, Academic Press, 1981
!!
!! \param [in,out] W    symmetirc matrix
!! \param [in]     N    size
*
      SUBROUTINE DCFDEC(W,N)

      IMPLICIT NONE
      INTEGER N,I,J,K
      DOUBLE PRECISION W(*),EPSM,GAMM,XCHI,BETA,DELTA,THETA
*
      EPSM=2.2D-16 ! machine precision
      GAMM=0.0D0   ! max diagonal element
      XCHI=0.0D0   ! max off-diagonal element
      DO K=1,N
       GAMM=MAX(GAMM,ABS(W((K*K+K)/2)))
       DO J=K+1,N
        XCHI=MAX(XCHI,ABS(W((J*J-J)/2+K)))
       END DO
      END DO
      BETA=SQRT(MAX(GAMM,XCHI/MAX(1.0D0,SQRT(DFLOAT(N*N-1))),EPSM))
      DELTA=EPSM*MAX(1.0D0,GAMM+XCHI)

      DO K=1,N
       DO I=1,K-1
         W((K*K-K)/2+I)=W((K*K-K)/2+I)*W((I*I+I)/2)   
       END DO
       DO J=K+1,N
        DO I=1,K-1
         W((J*J-J)/2+K)=W((J*J-J)/2+K)-W((K*K-K)/2+I)*W((J*J-J)/2+I)
        END DO
       END DO
       THETA=0.0D0
       DO J=K+1,N 
        THETA=MAX(THETA,ABS(W((J*J-J)/2+K)))
       END DO
       W((K*K+K)/2)=1.0D0/MAX(ABS(W((K*K+K)/2)),(THETA/BETA)**2,DELTA)
       DO J=K+1,N
        W((J*J+J)/2)=W((J*J+J)/2)-W((J*J-J)/2+K)**2*W((K*K+K)/2)
       END DO
      END DO ! K

      END

!> Decomposition of symmetric band matrix.
!!
!!    Band matrix modified Cholesky decomposition,
!!    Philip E.Gill, Walter Murray and Margarete H.Wright:
!!      Practical Optimization, Academic Press, 1981
!!
!! \param [in,out] W    symmetric band matrix
!! \param [in]     MP1  band width (M) + 1
!! \param [in]     N    size
*
      SUBROUTINE DBFDEC(W,MP1,N)
*
      IMPLICIT NONE
      INTEGER MP1,N,I,J,K  
      DOUBLE PRECISION W(MP1,N),EPSM,GAMM,XCHI,BETA,DELTA,THETA
*     
      EPSM=2.2D-16 ! machine precision
      GAMM=0.0D0   ! max diagonal element
      XCHI=0.0D0   ! max off-diagonal element
      DO K=1,N
       GAMM=MAX(GAMM,ABS(W(1,K)))
       DO J=2,MIN(MP1,N-K+1)
        XCHI=MAX(XCHI,ABS(W(J,K)))
       END DO
      END DO
      BETA=SQRT(MAX(GAMM,XCHI/MAX(1.0D0,SQRT(DFLOAT(N*N-1))),EPSM))
      DELTA=EPSM*MAX(1.0D0,GAMM+XCHI)

      DO K=1,N
       DO I=2,MIN(MP1,K)
        W(I,K-I+1)=W(I,K-I+1)*W(1,K-I+1)
       END DO
       DO J=2,MIN(MP1,N-K+1)
        DO I=MAX(2,J+K+1-MP1),K
         W(J,K)=W(J,K)-W(K-I+2,I-1)*W(J-I+K+1,I-1)
        END DO
       END DO
       THETA=0.0D0
       DO J=2,MIN(MP1,N-K+1)
        THETA=MAX(THETA,ABS(W(J,K))) 
       END DO
       W(1,K)=1.0D0/MAX(ABS(W(1,K)),(THETA/BETA)**2,DELTA) 
       DO J=2,MIN(MP1,N-K+1)
        W(1,K+J-1)=W(1,K+J-1)-W(1,K)*W(J,K)**2
       END DO
      END DO ! K

      END


