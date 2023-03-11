!> \file
!! QL decompostion.
!!
!! \author Claus Kleinwort, DESY, 2015 (Claus.Kleinwort@desy.de)
!!
!! \copyright
!! Copyright (c) 2015-2021 Deutsches Elektronen-Synchroton,
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
!! QL decomposition of constraints matrix by Householder transformations
!! for solution by elimination. Optionally split into disjoint blocks.
!!

!> QL data.
MODULE mpqldec
    USE mpdef
    IMPLICIT NONE

    INTEGER(mpi) :: npar   !< number of parameters
    INTEGER(mpi) :: ncon   !< number of constraints
    INTEGER(mpi) :: nblock !< number of blocks
    INTEGER(mpi) :: iblock !< active block
    INTEGER(mpi) :: monpg  !< flag for progress monitoring
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: sparseV !< sparsity structure matV
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: matV !< unit normals (v_i) of Householder reflectors
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: matL !< lower diagonal matrix L
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: vecN !< normal vector
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: nparBlock !< number of parameters in block
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: ioffBlock !< block offset (1. constraint -1)

END MODULE mpqldec

!> Initialize QL decomposition.
!!
!! \param [in]     n  number of rows (parameters)
!! \param [in]     m  number of columns (constraints)
!! \param [in]     l  number of disjoint blocks
!! \param [in]     k  flag for progress monitoring
!!
SUBROUTINE qlini(n,m,l,k)
    USE mpqldec
    USE mpdalc

    IMPLICIT NONE
    INTEGER(mpl) :: length

    INTEGER(mpi), INTENT(IN)          :: n
    INTEGER(mpi), INTENT(IN)          :: m
    INTEGER(mpi), INTENT(IN)          :: l
    INTEGER(mpi), INTENT(IN)          :: k

    npar=n
    ncon=m
    nblock=l
    iblock=1
    monpg=k
    ! allocate 
    length=5*ncon
    CALL mpalloc(sparseV,length,'QLDEC: sparsity structure of V')
    length=INT(npar,mpl)*INT(ncon,mpl)
    CALL mpalloc(matV,length,'QLDEC: V')
    matV=0.
    length=INT(ncon,mpl)*INT(ncon,mpl)
    CALL mpalloc(matL,length,'QLDEC: L')
    matL=0.
    length=npar
    CALL mpalloc(vecN,length,'QLDEC: v') 
    length=nblock   
    CALL mpalloc(nparBlock,length,'QLDEC: npar in block')
    nparBlock=0
    length=nblock+1   
    CALL mpalloc(ioffBlock,length,'QLDEC: ioff for block')
    ioffBlock=0   
END SUBROUTINE qlini

!                                                 141217 C. Kleinwort, DESY-FH1
!> QL decomposition (as single block).
!!
!! QL decomposition with Householder transformations.
!! Decompose N-By-M matrix A into orthogonal N-by-N matrix Q and a
!! N-by-M matrix containing zeros except for a lower triangular
!! M-by-M matrix L (at the bottom):
!!
!!              | 0 |
!!      A = Q * |   |
!!              | L |
!!
!! The decomposition is stored in a N-by-M matrix matV containing the unit
!! normal vectors v_i of the hyperplanes (Householder reflectors) defining Q.
!! The lower triangular matrix L is stored in the M-by-M matrix matL.
!!
!! \param [in]     a  Npar-by-Ncon matrix
!!
SUBROUTINE qldec(a)
    USE mpqldec
    USE mpdalc

    ! cost[dot ops] ~= Npar*Ncon*Ncon

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpl) :: ioff1
    INTEGER(mpl) :: ioff2
    INTEGER(mpl) :: ioff3
    INTEGER(mpi) :: k
    INTEGER(mpi) :: kn
    INTEGER(mpl) :: length
    REAL(mpd) :: nrm
    REAL(mpd) :: sp

    REAL(mpd), INTENT(IN)             :: a(*)

    ! prepare 
    length=INT(npar,mpl)*INT(ncon,mpl)
    matV=a(1:length)
    matL=0.0_mpd
    ! implemented as single block
    nblock=1
    nparBlock(1)=npar
    ioffBlock(2)=ncon   

    ! Householder procedure
    DO k=ncon,1,-1
        ! monitoring
        IF(monpg>0) CALL monpgs(ncon+1-k)
        kn=npar+k-ncon
        ! column offset
        ioff1=INT(k-1,mpl)*INT(npar,mpl)
        ! get column
        vecN(1:kn)=matV(ioff1+1:ioff1+kn)
        nrm = SQRT(dot_product(vecN(1:kn),vecN(1:kn)))
        IF (nrm == 0.0_mpd) CYCLE
        !
        IF (vecN(kn) >= 0.0_mpd) THEN
            vecN(kn)=vecN(kn)+nrm
        ELSE
            vecN(kn)=vecN(kn)-nrm
        END IF
        ! create normal vector
        nrm = SQRT(dot_product(vecN(1:kn),vecN(1:kn)))
        vecN(1:kn)=vecN(1:kn)/nrm
        ! transformation
        ioff2=0
        DO i=1,k
            sp=dot_product(vecN(1:kn),matV(ioff2+1:ioff2+kn))
            matV(ioff2+1:ioff2+kn)=matV(ioff2+1:ioff2+kn)-2.0_mpd*vecN(1:kn)*sp
            ioff2=ioff2+npar
        END DO
        ! store column of L
        ioff3=INT(k-1,mpl)*INT(ncon,mpl)
        matL(ioff3+k:ioff3+ncon)=matV(ioff1+kn:ioff1+npar)
        ! store normal vector
        matV(ioff1+1:ioff1+kn)=vecN(1:kn)
        matV(ioff1+kn+1:ioff1+npar)=0.0_mpd
        ! sparsity structure
        ioff3=(k-1)*5
        sparseV(ioff3+1)=1    ! number of non-zero regions
        sparseV(ioff3+2)=1    ! start
        sparseV(ioff3+3)=kn   ! end
    END DO

END SUBROUTINE qldec

!                                                 190312 C. Kleinwort, DESY-BELLE
!> QL decomposition (for disjoint block matrix).
!!
!! QL decomposition with Householder transformations.
!! Decompose N-By-M matrix A into orthogonal N-by-N matrix Q and a
!! N-by-M matrix containing zeros except for a lower triangular
!! M-by-M matrix L (at the bottom):
!!
!!              | 0 |
!!      A = Q * |   |
!!              | L |
!!
!! The decomposition is stored in a N-by-M matrix matV containing the unit
!! normal vectors v_i of the hyperplanes (Householder reflectors) defining Q.
!! The lower triangular matrix L is stored in the M-by-M matrix matL.
!!
!! \param [in]     a     block compressed Npar-by-Ncon matrix
!! \param [in]     bpar  2-by-NparBlock+1 matrix (with parameter block definition)
!! \param [in]     bcon  3-by-NconBlock+1 matrix (with constraint block definition)
!!
SUBROUTINE qldecb(a,bpar,bcon)
    USE mpqldec
    USE mpdalc

    ! cost[dot ops] ~= Npar*Ncon*Ncon

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: ibcon
    INTEGER(mpi) :: ibpar
    INTEGER(mpi) :: ifirst
    INTEGER(mpi) :: ilast
    INTEGER(mpl) :: ioff1
    INTEGER(mpl) :: ioff2
    INTEGER(mpl) :: ioff3
    INTEGER(mpi) :: iclast
    INTEGER(mpi) :: icoff
    INTEGER(mpi) :: iplast
    INTEGER(mpi) :: ipoff
    INTEGER(mpi) :: k
    INTEGER(mpi) :: k1
    INTEGER(mpi) :: kn
    INTEGER(mpi) :: ncb
    INTEGER(mpi) :: npb
    REAL(mpd) :: nrm
    REAL(mpd) :: sp

    REAL(mpd), INTENT(IN)             :: a(*)
    INTEGER(mpi), INTENT(IN)          :: bpar(2,*)
    INTEGER(mpi), INTENT(IN)          :: bcon(3,*)


    !$POMP INST BEGIN(qldecb)
    ! prepare 
    matV=0.0_mpd
    matL=0.0_mpd

    ioff1=0
    ioff2=0
    icoff=0
    DO ibpar=1,nblock ! parameter block
        DO ibcon=bpar(2,ibpar)+1, bpar(2,ibpar+1)! constraint block
            ncb=bcon(1,ibcon+1)-bcon(1,ibcon) ! number of constraints in constraint block
            npb=bcon(3,ibcon)+1-bcon(2,ibcon) ! number of parameters in constraint block
            ifirst=bcon(2,ibcon)
            ilast=bcon(3,ibcon)
            DO i=1,ncb
                matV(ioff1+ifirst:ioff1+ilast)=a(ioff2+1:ioff2+npb)
                ioff1=ioff1+npar
                ioff2=ioff2+npb
            END DO
            icoff=icoff+ncb
        END DO
        nparBlock(ibpar)=bpar(1,ibpar+1)-bpar(1,ibpar)
        ioffBlock(ibpar+1)=icoff
    END DO
 
    DO ibpar=1,nblock ! parameter block
        ipoff=bpar(1,ibpar) ! parameter offset in parameter block
        iplast=bpar(1,ibpar+1) ! last parameter in parameter block
        icoff=ioffBlock(ibpar) ! constraint offset in parameter block
        iclast=ioffBlock(ibpar+1) ! last constraint in parameter block
        ibcon=bpar(2,ibpar+1) ! start with last constraint block
        k1=bcon(1,ibcon) ! first constraint in block
        ! Householder procedure
        DO k=iclast,icoff+1,-1
            ! monitoring
            IF(monpg>0) CALL monpgs(ncon+1-k)
            kn=iplast+k-iclast
            ! different constraint block?
            IF (k < k1) THEN
                ibcon=ibcon-1
                k1=bcon(1,ibcon)
            END IF
            ! index if first non-zero element
            ifirst=bcon(2,ibcon)
            IF (ifirst > kn) CYCLE
            ! index of last element
            ilast=min(bcon(3,ibcon),kn)
            ! column offsets
            ioff1=INT(k-1,mpl)*INT(npar,mpl)
            ioff2=INT(k1-1,mpl)*INT(npar,mpl)
            ! get column
            vecN(kn)=0.0_mpd
            vecN(ifirst:ilast)=matV(ioff1+ifirst:ioff1+ilast)
            nrm = SQRT(dot_product(vecN(ifirst:ilast),vecN(ifirst:ilast)))
            IF (nrm == 0.0_mpd) CYCLE
            !
            IF (vecN(kn) >= 0.0_mpd) THEN
                vecN(kn)=vecN(kn)+nrm
            ELSE
                vecN(kn)=vecN(kn)-nrm
            END IF
        
            IF (ilast < kn) THEN
                ! create normal vector
                nrm = SQRT(dot_product(vecN(ifirst:ilast),vecN(ifirst:ilast))+vecN(kn)*vecN(kn))
                vecN(ifirst:ilast)=vecN(ifirst:ilast)/nrm
                vecN(kn)=vecN(kn)/nrm
                ! transformation
                DO i=k1,k
                    sp=dot_product(vecN(ifirst:ilast),matV(ioff2+ifirst:ioff2+ilast))
                    matV(ioff2+ifirst:ioff2+ilast)=matV(ioff2+ifirst:ioff2+ilast)-2.0_mpd*vecN(ifirst:ilast)*sp
                    matV(ioff2+kn)=-2.0_mpd*vecN(kn)*sp
                    ioff2=ioff2+npar
                END DO
            ELSE
                ! create normal vector
                nrm = SQRT(dot_product(vecN(ifirst:ilast),vecN(ifirst:ilast)))
                vecN(ifirst:ilast)=vecN(ifirst:ilast)/nrm
                ! transformation
                DO i=k1,k
                    sp=dot_product(vecN(ifirst:ilast),matV(ioff2+ifirst:ioff2+ilast))
                    matV(ioff2+ifirst:ioff2+ilast)=matV(ioff2+ifirst:ioff2+ilast)-2.0_mpd*vecN(ifirst:ilast)*sp
                    ioff2=ioff2+npar
                END DO
            END IF
            
            ! store column of L
            ioff3=INT(k-1,mpl)*INT(ncon,mpl)
            matL(ioff3+k-icoff:ioff3+iclast-icoff)=matV(ioff1+kn:ioff1+iplast)
            ! store normal vector
            matV(ioff1+1:ioff1+npar)=0.0_mpd
            matV(ioff1+ifirst-ipoff:ioff1+ilast-ipoff)=vecN(ifirst:ilast)
            matV(ioff1+kn-ipoff)=vecN(kn)
            ! sparsity structure
            ioff3=(k-1)*5
            sparseV(ioff3+1)=2               ! number of non-zero regions
            sparseV(ioff3+2)=ifirst-ipoff    ! start 1 
            sparseV(ioff3+3)=ilast-ipoff     ! end 1
            sparseV(ioff3+4)=kn-ipoff        ! start 2
            sparseV(ioff3+5)=kn-ipoff        ! end 2
        END DO
    END DO
    !$POMP INST END(qldecb)
    
    
END SUBROUTINE qldecb


!> Multiply left by Q(t) (per block).
!!
!! Multiply left by Q(t) from QL decomposition.
!!
!! \param [in,out] x    NparBlock-by-M matrix, overwritten with Q*X (t=false) or Q^t*X (t=true)
!! \param [in]     m    number of columns
!! \param [in]     t    use transposed of Q
!!
SUBROUTINE qlmlq(x,m,t)
    USE mpqldec

    ! cost[dot ops] ~= N*M*Nhr

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: icoff
    INTEGER(mpi) :: iclast
    INTEGER(mpl) :: ioff1
    INTEGER(mpl) :: ioff2
    INTEGER(mpi) :: j
    INTEGER(mpi) :: k
    INTEGER(mpi) :: kn
    INTEGER(mpi) :: nconb
    INTEGER(mpi) :: nparb
    REAL(mpd) :: sp

    REAL(mpd), INTENT(IN OUT)         :: x(*)
    INTEGER(mpi), INTENT(IN)          :: m
    LOGICAL, INTENT(IN)               :: t

    icoff=ioffBlock(iblock) ! constraint offset in parameter block
    iclast=ioffBlock(iblock+1) ! last constraint in parameter block
    nconb=iclast-icoff ! number of constraints in block
    nparb=nparBlock(iblock) ! number of parameters in block
    DO j=1,nconb
        k=j
        IF (t) k=nconb+1-j
        kn=nparb+k-nconb
        ! column offset
        ioff1=INT(k-1+icoff,mpl)*INT(npar,mpl)
        ! transformation
        ioff2=0
        DO i=1,m
            sp=dot_product(matV(ioff1+1:ioff1+kn),x(ioff2+1:ioff2+kn))
            x(ioff2+1:ioff2+kn)=x(ioff2+1:ioff2+kn)-2.0_mpd*matV(ioff1+1:ioff1+kn)*sp
            ioff2=ioff2+nparb
        END DO
    END DO

END SUBROUTINE qlmlq


!> Multiply right by Q(t).
!!
!! Multiply right by Q(t) from QL decomposition.
!!
!! \param [in,out] x    M-by-Npar matrix, overwritten with X*Q (t=false) or X*Q^t (t=true)
!! \param [in]     m    number of rows
!! \param [in]     t    use transposed of Q
!!
SUBROUTINE qlmrq(x,m,t)
    USE mpqldec

    ! cost[dot ops] ~= N*M*Nhr

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpl) :: ioff1
    INTEGER(mpl) :: iend
    INTEGER(mpi) :: j
    INTEGER(mpi) :: k
    INTEGER(mpi) :: kn
    REAL(mpd) :: sp

    REAL(mpd), INTENT(IN OUT)         :: x(*)
    INTEGER(mpi), INTENT(IN)          :: m
    LOGICAL, INTENT(IN)               :: t

    DO j=1,ncon
        k=j
        IF (.not.t) k=ncon+1-j
        kn=npar+k-ncon
        ! column offset
        ioff1=INT(k-1,mpl)*INT(npar,mpl)
        ! transformation
        iend=m*kn
        DO i=1,npar
            sp=dot_product(matV(ioff1+1:ioff1+kn),x(i:iend:m))
            x(i:iend:m)=x(i:iend:m)-2.0_mpd*matV(ioff1+1:ioff1+kn)*sp
        END DO
    END DO

END SUBROUTINE qlmrq


!> Similarity transformation by Q(t).
!!
!! Similarity transformation by Q from QL decomposition.
!!
!! \param [in,out] x    Npar-by-Npar matrix, overwritten with Q*X*Q^t (t=false) or Q^t*X*Q (t=true)
!! \param [in]     t    use transposed of Q
!!
SUBROUTINE qlsmq(x,t)
    USE mpqldec

    ! cost[dot ops] ~= N*N*Nhr

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpl) :: ioff1
    INTEGER(mpl) :: ioff2
    INTEGER(mpl) :: iend
    INTEGER(mpi) :: j
    INTEGER(mpi) :: k
    INTEGER(mpi) :: kn
    REAL(mpd) :: sp

    REAL(mpd), INTENT(IN OUT)         :: x(*)
    LOGICAL, INTENT(IN)               :: t

    DO j=1,ncon
        ! monitoring
        IF(monpg>0) CALL monpgs(j)
        k=j
        IF (t) k=ncon+1-j
        kn=npar+k-ncon
        ! column offset
        ioff1=INT(k-1,mpl)*INT(npar,mpl)
        ! transformation
        iend=INT(npar,mpl)*INT(kn,mpl)
        DO i=1,npar
            sp=dot_product(matV(ioff1+1:ioff1+kn),x(i:iend:npar))
            x(i:iend:npar)=x(i:iend:npar)-2.0_mpd*matV(ioff1+1:ioff1+kn)*sp
        END DO
        ioff2=0
        DO i=1,npar
            sp=dot_product(matV(ioff1+1:ioff1+kn),x(ioff2+1:ioff2+kn))
            x(ioff2+1:ioff2+kn)=x(ioff2+1:ioff2+kn)-2.0_mpd*matV(ioff1+1:ioff1+kn)*sp
            ioff2=ioff2+npar
        END DO
    END DO

END SUBROUTINE qlsmq


!> Similarity transformation by Q(t).
!!
!! Similarity transformation for symmetric matrix by Q from QL decomposition.
!!
!! \param [in]     aprod    external procedure to calculate A*v
!! \param [in,out] A        symmetric Npar-by-Npar matrix A in symmetric or unpacked storage mode
!!                          overwritten with Q*A*Q^t (t=false) or Q^t*A*Q (t=true)
!! \param [in]     roff     row offsets for A
!! \param [in]     t        use transposed of Q
!!
SUBROUTINE qlssq(aprod,A,roff,t)
    USE mpqldec
    USE mpdalc

    ! cost[dot ops] ~= N*N*Nhr

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: ibpar
    INTEGER(mpi) :: icoff
    INTEGER(mpi) :: iclast
    INTEGER(mpl) :: ioff1
    INTEGER(mpl) :: ioff2
    INTEGER(mpl) :: ioffp
    INTEGER(mpi) :: j
    INTEGER(mpi) :: k
    INTEGER(mpi) :: kn
    INTEGER(mpl) :: length
    INTEGER(mpi) :: nconb
    INTEGER(mpi) :: nparb
    REAL(mpd) :: vtAv
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: Av

    REAL(mpd), INTENT(IN OUT)         :: A(*)
    INTEGER(mpl), INTENT(IN)          :: roff(*)
    LOGICAL, INTENT(IN)               :: t

    INTERFACE
        SUBROUTINE aprod(n,l,x,y) ! y=A*x
            USE mpdef
            INTEGER(mpi), INTENT(in) :: n
            INTEGER(mpl), INTENT(in) :: l
            REAL(mpd), INTENT(IN)    :: x(n)
            REAL(mpd), INTENT(OUT)   :: y(n)
        END SUBROUTINE aprod
    END INTERFACE
    !$POMP INST BEGIN(qlssq)
    
    length=npar
    CALL mpalloc(Av,length,'qlssq: A*v')

    ioffp=0 ! parameter offset for block
    DO ibpar=1,nblock ! parameter block
        icoff=ioffBlock(ibpar) ! constraint offset in parameter block
        iclast=ioffBlock(ibpar+1) ! last constraint in parameter block
        nconb=iclast-icoff ! number of constraints in block
        nparb=nparBlock(ibpar) ! number of parameters in block
        DO j=1,nconb
            k=j
            ! monitoring
            IF(monpg>0) CALL monpgs(icoff+k)
            IF (t) k=nconb+1-j
            kn=nparb+k-nconb
            ! column offset
            ioff1=INT(k-1+icoff,mpl)*INT(npar,mpl)
            ! A*v
            CALL aprod(nparb,ioffp,matV(ioff1+1:ioff1+nparb),Av(1:nparb))
            ! transformation
            ! diagonal block
            ! v^t*A*v
            vtAv=dot_product(matV(ioff1+1:ioff1+kn),Av(1:kn))
            ! update
            ! parallelize row loop
            ! slot of 8 'I' for next idle thread 
            !$OMP PARALLEL DO &
            !$OMP PRIVATE(IOFF2) &
            !$OMP SCHEDULE(DYNAMIC,8)
            DO i=1,kn
                ioff2=roff(i+ioffp)+ioffp
                ! correct with  2*(2v*vtAv*v^t - Av*v^t - (Av*v^t)^t)
                A(ioff2+1:ioff2+i)=A(ioff2+1:ioff2+i)+2.0_mpd* &
                    ((2.0_mpd*matV(ioff1+i)*vtAv-Av(i))*matV(ioff1+1:ioff1+i)-Av(1:i)*matV(ioff1+i))
            END DO
            !$OMP END PARALLEL DO
            ! off diagonal block
            DO i=kn+1,nparb
                ioff2=roff(i+ioffp)+ioffp
                ! correct with -2Av*v^t
                A(ioff2+1:ioff2+kn)=A(ioff2+1:ioff2+kn)-2.0_mpd*matV(ioff1+1:ioff1+kn)*Av(i)
            END DO
        END DO
        ! update parameter offset
        ioffp=ioffp+nparb
    END DO

    CALL mpdealloc(Av)
    !$POMP INST END(qlssq)

END SUBROUTINE qlssq

!> Partial similarity transformation by Q(t).
!!
!! Partial similarity transformation for symmetric matrix by Q from QL decomposition.
!! Calculate corrections to band part of matrix.
!!
!! \param [in]     aprod    external procedure to calculate A*v
!! \param [in,out] B        band part of symmetric Npar-by-Npar matrix A in symmetric storage mode,
!!                          overwritten with band part of Q^t*A*Q (t=false) or Q^t*A*Q (t=true)
!! \param [in]     m        band width (including diagonal)
!! \param [in]     t        use transposed of Q
!!
SUBROUTINE qlpssq(aprod,B,m,t)
    USE mpqldec
    USE mpdalc

    ! cost[dot ops] ~= N*N*Nhr

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpl) :: ioff1
    INTEGER(mpl) :: ioff2
    INTEGER(mpi) :: ioffr
    INTEGER(mpi) :: ir
    INTEGER(mpi) :: j
    INTEGER(mpi) :: j2
    INTEGER(mpi) :: k
    INTEGER(mpi) :: k2
    INTEGER(mpi) :: kn
    INTEGER(mpi) :: l
    INTEGER(mpi) :: l1
    INTEGER(mpi) :: l2
    INTEGER(mpl) :: length
    INTEGER(mpi) :: mbnd
    REAL(mpd) :: vtAv
    REAL(mpd) :: vtAvp
    REAL(mpd) :: vtvp
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: Av   ! A*v 

    REAL(mpd), INTENT(IN OUT)         :: B(*)
    INTEGER(mpi), INTENT(IN)          :: m
    LOGICAL, INTENT(IN)               :: t

    INTERFACE
        SUBROUTINE aprod(n,x,y,is) ! y=A*x
            USE mpdef
            INTEGER(mpi), INTENT(in) :: n
            REAL(mpd), INTENT(IN)    :: x(n)
            REAL(mpd), INTENT(OUT)   :: y(n)
            INTEGER(mpi), INTENT(in) :: is(*)
        END SUBROUTINE aprod
    END INTERFACE
    !$POMP INST BEGIN(qlpssq)

    length=INT(npar,mpl)*INT(ncon,mpl)
    CALL mpalloc(Av,length,'qlpssq: Av')

    mbnd=max(0,m-1) ! band width without diagonal
    ! A*V
    ioff1=0
    DO i=1,ncon
        CALL aprod(npar,matV(ioff1+1:ioff1+npar),Av(ioff1+1:ioff1+npar),sparseV(i*5-4))
        ioff1=ioff1+npar
    END DO

    DO j=1,ncon
        k=j
        ! monitoring
        IF(monpg>0) CALL monpgs(k)
        IF (t) k=ncon+1-j
        kn=npar+k-ncon
        ! column offset
        ioff1=INT(k-1,mpl)*INT(npar,mpl)
        ! redion offset
        ioffr=(k-1)*5
        ! transformation (diagonal block)
        ! diagonal block
        ! v^t*A*v
        vtAv=dot_product(matV(ioff1+1:ioff1+kn),Av(ioff1+1:ioff1+kn))
        ! update
        ioff2=0
        DO i=1,kn
            ! correct with  2*(2v*vtAv*v^t - Av*v^t - (Av*v^t)^t)
            DO l=max(1,i-mbnd),i
                ioff2=ioff2+1
                B(ioff2)=B(ioff2)+2.0_mpd*((2.0_mpd*matV(ioff1+i)*vtAv-Av(ioff1+i))*matV(ioff1+l)-Av(ioff1+l)*matV(ioff1+i))
            END DO
        END DO
        ! off diagonal block
        DO i=kn+1,npar
            ! correct with -2Av*v^t
            DO l=max(1,i-mbnd),i
                ioff2=ioff2+1
                B(ioff2)=B(ioff2)-2.0_mpd*Av(ioff1+i)*matV(ioff1+l)
            END DO
        END DO
        ! correct A*v for the remainung v
        DO j2=j+1,ncon
            k2=j2
            IF (t) k2=ncon+1-j2
            ioff2=INT(k2-1,mpl)*INT(npar,mpl)
            ! loop over non-zero regions
            vtvp=0._mpd
            vtAvp=0._mpd
            DO ir=1, sparseV(ioffr+1)
                l1=sparseV(ioffr+2*ir)   ! first non-zero element in region
                l2=sparseV(ioffr+2*ir+1) ! last  non-zero element in region
                vtvp=vtvp+dot_product(matV(ioff1+l1:ioff1+l2),matV(ioff2+l1:ioff2+l2)) ! v^t*v'
                vtAvp=vtAvp+dot_product(matV(ioff1+l1:ioff1+l2),Av(ioff2+l1:ioff2+l2)) ! v^t*(A*v')
            END DO
            ! loop over non-zero regions
            DO ir=1, sparseV(ioffr+1)
                l1=sparseV(ioffr+2*ir)   ! first non-zero element in region
                IF (l1 > kn) EXIT
                l2=min(kn,sparseV(ioffr+2*ir+1)) ! last  non-zero element in region (<= kn)
                Av(ioff2+l1:ioff2+l2)=Av(ioff2+l1:ioff2+l2) &
                    +2.0_mpd*matV(ioff1+l1:ioff1+l2)*(2.0_mpd*vtAv*vtvp-vtAvp)
            END DO   
            ! some 'overlap'?
            IF (vtvp /= 0._mpd) THEN
                Av(ioff2+1:ioff2+npar)=Av(ioff2+1:ioff2+npar)-2.0_mpd*Av(ioff1+1:ioff1+npar)*vtvp
            END IF
        END DO

    END DO

    CALL mpdealloc(Av)
    !$POMP INST END(qlpssq)

END SUBROUTINE qlpssq


!> Get eigenvalues.
!!
!! Get smallest and largest |eigenvalue| of L.
!!
!! \param [out]    emin  eigenvalue with smallest absolute value
!! \param [out]    emax  eigenvalue with largest absolute value
!!
SUBROUTINE qlgete(emin,emax)
    USE mpqldec

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: ibpar
    INTEGER(mpi) :: icoff
    INTEGER(mpi) :: iclast
    INTEGER(mpl) :: idiag

    REAL(mpd), INTENT(OUT)         :: emin
    REAL(mpd), INTENT(OUT)         :: emax

    emax=matL(1)
    emin=emax
    DO ibpar=1,nblock ! parameter block
        icoff=ioffBlock(ibpar) ! constraint offset in parameter block
        iclast=ioffBlock(ibpar+1) ! last constraint in parameter block
        idiag=INT(ncon,mpl)*INT(icoff,mpl)+1
        DO i=icoff+1,iclast
            IF (ABS(emax) < ABS(matL(idiag))) emax=matL(idiag)
            IF (ABS(emin) > ABS(matL(idiag))) emin=matL(idiag)
            idiag=idiag+ncon+1
        END DO
    END DO

END SUBROUTINE qlgete


!> Backward substitution (per block).
!!
!! Get y from L^t*y=d.
!!
!! \param [in]     d  Ncon vector, resdiduals
!! \param [out]    y  Ncon vector, solution
!!
SUBROUTINE qlbsub(d,y)
    USE mpqldec

    IMPLICIT NONE
    INTEGER(mpi) :: icoff
    INTEGER(mpi) :: iclast
    INTEGER(mpl) :: idiag
    INTEGER(mpi) :: k
    INTEGER(mpi) :: nconb

    REAL(mpd), INTENT(IN)         :: d(*)
    REAL(mpd), INTENT(OUT)        :: y(*)

    ! solve L*y=d by forward substitution
    icoff=ioffBlock(iblock) ! constraint offset in parameter block
    iclast=ioffBlock(iblock+1) ! last constraint in parameter block
    nconb=iclast-icoff ! number of constraints in block
    idiag=INT(ncon,mpl)*INT(iclast-1,mpl)+nconb
    DO k=nconb,1,-1
        y(k)=(d(k)-dot_product(matL(idiag+1:idiag+nconb-k),y(k+1:nconb)))/matL(idiag)
        idiag=idiag-ncon-1
    END DO

END SUBROUTINE qlbsub

!> Set block
!!
SUBROUTINE qlsetb(ib)
    USE mpqldec

    IMPLICIT NONE
    INTEGER(mpi), INTENT(IN)      :: ib

    iblock=ib

END SUBROUTINE qlsetb

!> Print statistics
!!
SUBROUTINE qldump()
    USE mpqldec

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpl) :: ioff1
    INTEGER(mpl) :: ioff2
    INTEGER(mpi) :: istat(6)
    INTEGER(mpi) :: j
    
    print *
    ioff1=0
    ioff2=0
    DO i=1, ncon
        istat=0
        DO j=1,npar!+i-ncon
            IF (matV(ioff1+j) /= 0.0_mpd) THEN
                IF (istat(3) == 0) istat(1)=j
                istat(2)=j
                istat(3)=istat(3)+1
            END IF
        END DO
        ioff1=ioff1+npar
        DO j=1,ncon
            IF (matL(ioff2+j) /= 0.0_mpd) THEN
                IF (istat(6) == 0) istat(4)=j
                istat(5)=j
                istat(6)=istat(6)+1            
            END IF
        END DO
        ioff2=ioff2+ncon
        print 100, i, istat
    END DO
    print *
100 FORMAT(" qldump",7I8)   
    
END SUBROUTINE qldump

   
