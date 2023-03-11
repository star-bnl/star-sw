!> \file
!! QL decompostion.
!!
!! \author Claus Kleinwort, DESY, 2015 (Claus.Kleinwort@desy.de)
!!
!! \copyright
!! Copyright (c) 2015-2022 Deutsches Elektronen-Synchroton,
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
    INTEGER(mpl) :: matsize !< size of contraints matrix 
    INTEGER(mpi) :: iblock !< active block
    INTEGER(mpi) :: monpg  !< flag for progress monitoring
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: matV  !< unit normals (v_i) of Householder reflectors
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: vecVk !< secondary diagonal of matV (including last element)
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: matL  !< lower diagonal matrix L
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: vecN  !< normal vector
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: nparBlock !< number of parameters in block
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: ioffBlock !< block offset (1. constraint -1)
    INTEGER(mpl), DIMENSION(:), ALLOCATABLE :: ioffRow !< row offsets in matV (for constrint block)
    INTEGER(mpi), DIMENSION(:), ALLOCATABLE :: ioffPar !< parameter number offsets for matV ( " )
    INTEGER(mpi), DIMENSION(:,:), ALLOCATABLE :: irangeParNZ !< range for non zero part (except vecVk)

END MODULE mpqldec

!> Initialize QL decomposition.
!!
!! \param [in]     n  number of rows (parameters)
!! \param [in]     m  number of columns (constraints)
!! \param [in]     l  number of disjoint blocks
!! \param [in]     s  size of constraints matrix (compressed or n*m)
!! \param [in]     k  flag for progress monitoring
!!
SUBROUTINE qlini(n,m,l,s,k)
    USE mpqldec
    USE mpdalc

    IMPLICIT NONE
    INTEGER(mpl) :: length

    INTEGER(mpi), INTENT(IN)          :: n
    INTEGER(mpi), INTENT(IN)          :: m
    INTEGER(mpi), INTENT(IN)          :: l
    INTEGER(mpl), INTENT(IN)          :: s
    INTEGER(mpi), INTENT(IN)          :: k

    npar=n
    ncon=m
    nblock=l
    matsize=s
    iblock=1
    monpg=k
    ! allocate 
    length=matsize
    !print *, ' full length (V)', length
    CALL mpalloc(matV,length,'QLDEC: V')
    matV=0.
    length=INT(ncon,mpl)*INT(ncon,mpl)
    CALL mpalloc(matL,length,'QLDEC: L')
    matL=0.
    length=npar
    CALL mpalloc(vecN,length,'QLDEC: v') 
    length=ncon
    CALL mpalloc(vecVk,length,'QLDEC: sec. diag(V)')
    vecVk=0. 
    CALL mpalloc(ioffPar,length,'QLDEC: parameter offsets (V)')
    ioffPar=0
    CALL mpalloc(irangeParNZ,2_mpl,length,'QLDEC: parameter non zero range (V)')
    length=ncon+1
    CALL mpalloc(ioffRow,length,'QLDEC: row offsets (V)') 
    ioffRow=0   
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

    REAL(mpd), INTENT(IN)             :: a(matsize)

    ! prepare
    vecVk=0._mpd
    length=INT(npar,mpl)*INT(ncon,mpl)
    matV=a(1:length)
    matL=0.0_mpd
    ! implemented as single block
    nblock=1
    nparBlock(1)=npar
    ioffBlock(2)=ncon
    DO k=1,ncon
        ioffRow(k+1)=ioffRow(k)+npar
    END DO   

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
        matV(ioff1+1:ioff1+kn-1)=vecN(1:kn-1)
        matV(ioff1+kn:ioff1+npar)=0.0_mpd
        irangeParNZ(1,k)=1
        irangeParNZ(2,k)=kn-1
        ! store secondary diagonal 
        vecVk(k)=vecN(kn)
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
!! \param [in]     bpar  2-by-NparBlock+1    matrix (with parameter block definition)
!! \param [in]     bcon  3-by-Ncon(Block)s+1 matrix (with constraint block definition)
!! \param [in]     rcon  4-by-Ncons          matrix (with constraint ranges)
!!
SUBROUTINE qldecb(a,bpar,bcon,rcon)
    USE mpqldec
    USE mpdalc

    ! cost[dot ops] ~= Npar*Ncon*Ncon

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: ibcon
    INTEGER(mpi) :: iblast
    INTEGER(mpi) :: iboff
    INTEGER(mpi) :: ibpar
    INTEGER(mpi) :: ifirst
    INTEGER(mpi) :: ilast
    INTEGER(mpi) :: in
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
    INTEGER(mpi) :: ncol
    INTEGER(mpi) :: npb
    REAL(mpd) :: nrm
    REAL(mpd) :: sp

    REAL(mpd), INTENT(IN)             :: a(matsize)
    INTEGER(mpi), INTENT(IN)          :: bpar(2,nblock+1)
    INTEGER(mpi), INTENT(IN)          :: bcon(3,ncon+1)
    INTEGER(mpi), INTENT(IN)          :: rcon(4,ncon)

    !$POMP INST BEGIN(qldecb)
    ! prepare 
    vecVk=0.0_mpd
    matV=a(1:matsize)
    matL=0.0_mpd

    ! prepare offsets
    icoff=0
    DO ibpar=1,nblock ! parameter block
        iclast=icoff
        DO ibcon=bpar(2,ibpar)+1, bpar(2,ibpar+1)! constraint block
            ncb=bcon(1,ibcon+1)-bcon(1,ibcon) ! number of constraints in constraint block
            npb=bcon(3,ibcon)+1-bcon(2,ibcon) ! number of parameters in constraint block
            ifirst=bcon(2,ibcon)
            ilast=bcon(3,ibcon)
            DO i=bcon(1,ibcon),bcon(1,ibcon+1)-1
                ! non-zero range: first, last parameter
                irangeParNZ(1,i)=rcon(1,i)
                irangeParNZ(2,i)=rcon(2,i)
                ! storage: parameter, row offset
                ioffPar(i)=rcon(3,i)-1
                ioffRow(i+1)=ioffRow(i)+rcon(4,i)-ioffPar(i)
            END DO
            iclast=iclast+ncb
        END DO
        ! set up matL
        iblast=bpar(1,ibpar+1) ! last parameter in parameter block
        DO k=icoff+1,iclast
            kn=iblast+k-iclast
            ioff1=ioffRow(k)
            npb=INT(ioffRow(k+1)-ioff1,mpi)
            ! size of overlap
            ncol=ioffPar(k)+npb-kn
            IF (ncol >= 0) THEN
                ioff3=INT(k-1,mpl)*INT(ncon,mpl)
                matL(ioff3+iclast-ncol-icoff:ioff3+iclast-icoff)=matV(ioff1+npb-ncol:ioff1+npb)
            END IF
        END DO    
        icoff=iclast
        nparBlock(ibpar)=bpar(1,ibpar+1)-bpar(1,ibpar)
        ioffBlock(ibpar+1)=icoff
    END DO
 
    DO ibpar=1,nblock ! parameter block
        iboff=bpar(1,ibpar)    ! last offset in parameter block
        iblast=bpar(1,ibpar+1) ! last parameter in parameter block
        icoff=ioffBlock(ibpar) ! constraint offset in parameter block
        iclast=ioffBlock(ibpar+1) ! last constraint in parameter block
        IF(iclast <= icoff) CYCLE ! no constraints
        ibcon=bpar(2,ibpar+1) ! start with last constraint block
        k1=bcon(1,ibcon) ! first constraint in block
        ! Householder procedure
        DO k=iclast,icoff+1,-1
            ! monitoring
            IF(monpg>0) CALL monpgs(ncon+1-k)
            kn=iblast+k-iclast
            ! different constraint block?
            IF (k < k1) THEN
                ibcon=ibcon-1
                k1=bcon(1,ibcon)
            END IF
            ! parameter offset
            ipoff=ioffPar(k)
            ! index if first non-zero parameter
            ifirst=ipoff+1
            IF (ifirst > kn) CYCLE
            ! column offset
            ioff1=ioffRow(k)
            ! number of parameter
            npb=INT(ioffRow(k+1)-ioff1,mpi)
            ! index of last parameter
            iplast=ioffPar(k)+npb
            ! index of last used parameter
            ilast=min(iplast,kn)
            ! number of used columns
            ncol=ilast-ipoff
            ! get column
            in=iblast+k1-iclast
            vecN(in:kn)=0.0_mpd
            vecN(ifirst:ilast)=matV(ioff1+1:ioff1+ncol)
            nrm = SQRT(dot_product(vecN(ifirst:ilast),vecN(ifirst:ilast)))
            IF (nrm == 0.0_mpd) CYCLE
            !
            IF (vecN(kn) >= 0.0_mpd) THEN
                vecN(kn)=vecN(kn)+nrm
            ELSE
                vecN(kn)=vecN(kn)-nrm
            END IF
            ! create normal vector        
            IF (ilast < kn) THEN
                nrm = SQRT(dot_product(vecN(ifirst:ilast),vecN(ifirst:ilast))+vecN(kn)*vecN(kn))
                vecN(ifirst:ilast)=vecN(ifirst:ilast)/nrm
                vecN(kn)=vecN(kn)/nrm
            ELSE
                nrm = SQRT(dot_product(vecN(ifirst:ilast),vecN(ifirst:ilast)))
                vecN(ifirst:ilast)=vecN(ifirst:ilast)/nrm
            END IF
            ! update L too
            ioff3=INT(k1-2,mpl)*INT(ncon,mpl)
            ! transformation
            DO i=k1,k
                ioff3=ioff3+ncon
                IF (irangeParNZ(1,k) > irangeParNZ(2,i)) CYCLE ! no overlap
                ioff2=ioffRow(i)+ioffPar(k)-ioffPar(i)
                sp=dot_product(vecN(ifirst:ilast),matV(ioff2+1:ioff2+ncol))
                IF (sp /= 0.0_mpd) THEN
                    ! update matV
                    matV(ioff2+1:ioff2+ncol)=matV(ioff2+1:ioff2+ncol)-2.0_mpd*vecN(ifirst:ilast)*sp
                    ! update matL
                    in=iblast+i-iclast
                    matL(ioff3+i-icoff:ioff3+k-icoff)=matL(ioff3+i-icoff:ioff3+k-icoff)-2.0_mpd*vecN(in:kn)*sp
                    ! update non zero range
                    irangeParNZ(1,i)=min(irangeParNZ(1,i),irangeParNZ(1,k))
                    irangeParNZ(2,i)=max(irangeParNZ(2,i),irangeParNZ(2,k))
                END IF
            END DO  
            ! store secondary diagonal
            vecVk(icoff+k)=vecN(kn)         
            ! store normal vector (non zero part)
            ifirst=irangeParNZ(1,k)
            ilast=min(irangeParNZ(2,k),kn-1)
            ncol=ilast-ifirst+1
            matV(ioff1+1:ioff1+ncol)=vecN(ifirst:ilast)
            matV(ioff1+ncol+1:ioff1+npb)=0.0_mpd
            ! local to parameter block
            irangeParNZ(1,k)=ifirst-iboff
            irangeParNZ(2,k)=ilast-iboff
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
    INTEGER(mpi) :: ifirst
    INTEGER(mpi) :: ilast
    INTEGER(mpl) :: ioff2
    INTEGER(mpi) :: j
    INTEGER(mpi) :: k
    INTEGER(mpi) :: l
    INTEGER(mpi) :: kn
    INTEGER(mpi) :: nconb
    INTEGER(mpi) :: nparb
    REAL(mpd) :: sp

    INTEGER(mpi), INTENT(IN)          :: m
    REAL(mpd), INTENT(IN OUT)         :: x(INT(npar,mpl)*INT(m,mpl))
    LOGICAL, INTENT(IN)               :: t

    icoff=ioffBlock(iblock) ! constraint offset in parameter block
    iclast=ioffBlock(iblock+1) ! last constraint in parameter block
    nconb=iclast-icoff ! number of constraints in block
    nparb=nparBlock(iblock) ! number of parameters in block
    DO j=1,nconb
        k=j
        IF (t) k=nconb+1-j
        kn=nparb+k-nconb
        ! expand row 'l' of matV into vecN
        l=k+icoff
        ! non-zero range (excluding 'kn')
        ifirst=irangeParNZ(1,l) 
        ilast=irangeParNZ(2,l)
        vecN(1:nparb)=0._mpd
        vecN(ifirst:ilast)=matV(ioffRow(l)+1:ioffRow(l)+1+ilast-ifirst) 
        vecN(kn)=vecVk(l)
        ! transformation
        ioff2=0
        DO i=1,m
            sp=dot_product(vecN(ifirst:ilast),x(ioff2+ifirst:ioff2+ilast))+vecN(kn)*x(ioff2+kn)
            x(ioff2+ifirst:ioff2+ilast)=x(ioff2+ifirst:ioff2+ilast)-2.0_mpd*vecN(ifirst:ilast)*sp
            x(ioff2+kn)=x(ioff2+kn)-2.0_mpd*vecN(kn)*sp
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
    INTEGER(mpl) :: iend
    INTEGER(mpi) :: ifirst
    INTEGER(mpi) :: ilast
    INTEGER(mpi) :: j
    INTEGER(mpi) :: k
    INTEGER(mpi) :: kn
    REAL(mpd) :: sp

    INTEGER(mpi), INTENT(IN)          :: m
    REAL(mpd), INTENT(IN OUT)         :: x(INT(m,mpl)*INT(npar,mpl))
    LOGICAL, INTENT(IN)               :: t

    DO j=1,ncon
        k=j
        IF (.not.t) k=ncon+1-j
        kn=npar+k-ncon
        ! expand row 'k' of matV into vecN
        ! non-zero range (excluding 'kn')
        ifirst=irangeParNZ(1,k) 
        ilast=irangeParNZ(2,k)
        vecN=0._mpd
        vecN(ifirst:ilast)=matV(ioffRow(k)+1:ioffRow(k)+1+ilast-ifirst) 
        vecN(kn)=vecVk(k)
        ! transformation
        iend=m*kn
        DO i=1,npar
            sp=dot_product(vecN(1:kn),x(i:iend:m))
            x(i:iend:m)=x(i:iend:m)-2.0_mpd*vecN(1:kn)*sp
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
    INTEGER(mpl) :: ioff2
    INTEGER(mpl) :: iend
    INTEGER(mpi) :: ifirst
    INTEGER(mpi) :: ilast
    INTEGER(mpi) :: j
    INTEGER(mpi) :: k
    INTEGER(mpi) :: kn
    REAL(mpd) :: sp

    REAL(mpd), INTENT(IN OUT)         :: x(INT(npar,mpl)*INT(npar,mpl))
    LOGICAL, INTENT(IN)               :: t

    DO j=1,ncon
        ! monitoring
        IF(monpg>0) CALL monpgs(j)
        k=j
        IF (t) k=ncon+1-j
        kn=npar+k-ncon
        ! expand row 'k' of matV into vecN
        ! non-zero range (excluding 'kn')
        ifirst=irangeParNZ(1,k) 
        ilast=irangeParNZ(2,k)
        vecN=0._mpd
        vecN(ifirst:ilast)=matV(ioffRow(k)+1:ioffRow(k)+1+ilast-ifirst) 
        vecN(kn)=vecVk(k)
        ! transformation
        iend=INT(npar,mpl)*INT(kn,mpl)
        DO i=1,npar
            sp=dot_product(vecN(1:kn),x(i:iend:npar))
            x(i:iend:npar)=x(i:iend:npar)-2.0_mpd*vecN(1:kn)*sp
        END DO
        ioff2=0
        DO i=1,npar
            sp=dot_product(vecN(1:kn),x(ioff2+1:ioff2+kn))
            x(ioff2+1:ioff2+kn)=x(ioff2+1:ioff2+kn)-2.0_mpd*vecN(1:kn)*sp
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
!! \param [in]     s        size of A
!! \param [in]     roff     row offsets for A
!! \param [in]     t        use transposed of Q
!!
SUBROUTINE qlssq(aprod,A,s,roff,t)
    USE mpqldec
    USE mpdalc

    ! cost[dot ops] ~= N*N*Nhr

    IMPLICIT NONE
    INTEGER(mpi) :: i
    INTEGER(mpi) :: ibpar
    INTEGER(mpi) :: icoff
    INTEGER(mpi) :: iclast
    INTEGER(mpi) :: ifirst
    INTEGER(mpi) :: ilast
    INTEGER(mpi) :: ilasti
    INTEGER(mpl) :: ioff2
    INTEGER(mpi) :: ioffp
    INTEGER(mpi) :: j
    INTEGER(mpi) :: k
    INTEGER(mpi) :: l
    INTEGER(mpi) :: kn
    INTEGER(mpl) :: length
    INTEGER(mpi) :: nconb
    INTEGER(mpi) :: nparb
    REAL(mpd) :: vtAv
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: Av

    INTEGER(mpl), INTENT(IN)          :: s
    REAL(mpd), INTENT(IN OUT)         :: A(s)
    INTEGER(mpl), INTENT(IN)          :: roff(npar)
    LOGICAL, INTENT(IN)               :: t

    INTERFACE
        SUBROUTINE aprod(n,l,x,is,ie,y) ! y=A*x
            USE mpdef
            INTEGER(mpi), INTENT(in) :: n
            INTEGER(mpl), INTENT(in) :: l
            REAL(mpd), INTENT(IN)    :: x(n)
            INTEGER(mpi), INTENT(in) :: is
            INTEGER(mpi), INTENT(in) :: ie
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
            ! expand row 'l' of matV into vecN
            l=k+icoff
            ! non-zero range (excluding 'kn')
            ifirst=irangeParNZ(1,l) 
            ilast=irangeParNZ(2,l)
            vecN(1:nparb)=0._mpd
            vecN(ifirst:ilast)=matV(ioffRow(l)+1:ioffRow(l)+1+ilast-ifirst) 
            vecN(kn)=vecVk(k)
            ! A*v
            Av(1:nparb)=0._mpd
            CALL aprod(nparb,INT(ioffp,mpl),vecN(1:nparb),ifirst,ilast,Av(1:nparb))
            CALL aprod(nparb,INT(ioffp,mpl),vecN(1:nparb),kn,kn,Av(1:nparb))
            ! transformation
            ! diagonal block
            ! v^t*A*v
            vtAv=dot_product(vecN(ifirst:ilast),Av(ifirst:ilast))+vecN(kn)*Av(kn)
            ! update
            ! parallelize row loop
            ! slot of 8 'I' for next idle thread 
            !$OMP PARALLEL DO &
            !$OMP PRIVATE(IOFF2,ILASTI) &
            !$OMP SCHEDULE(DYNAMIC,8)
            DO i=1,kn
                ioff2=roff(i+ioffp)+ioffp
                ilasti=min(ilast,i)
                ! correct with  2*(2v*vtAv*v^t - Av*v^t)
                A(ioff2+ifirst:ioff2+ilasti)=A(ioff2+ifirst:ioff2+ilasti)+2.0_mpd* &
                    ((2.0_mpd*vecN(i)*vtAv-Av(i))*vecN(ifirst:ilasti))
            END DO
            !$OMP END PARALLEL DO
            
            ! parallelize row loop
            ! slot of 8 'I' for next idle thread 
            !$OMP PARALLEL DO &
            !$OMP PRIVATE(IOFF2) &
            !$OMP SCHEDULE(DYNAMIC,8)
            DO i=ifirst,ilast
                ioff2=roff(i+ioffp)+ioffp
                ! correct with  -2(Av*v^t)^t)
                A(ioff2+1:ioff2+i)=A(ioff2+1:ioff2+i)-2.0_mpd*Av(1:i)*vecN(i)
            END DO            
            !$OMP END PARALLEL DO
            ! i=kn, add secondary diagonal element
            ioff2=roff(kn+ioffp)+ioffp
            A(ioff2+kn)=A(ioff2+kn)+2.0_mpd*((2.0_mpd*vecVk(l)*vtAv-Av(kn))*vecVk(l)-Av(kn)*vecVk(l))
            ! off diagonal block
            DO i=kn+1,nparb
                ioff2=roff(i+ioffp)+ioffp
                ! correct with -2Av*v^t
                A(ioff2+ifirst:ioff2+ilast)=A(ioff2+ifirst:ioff2+ilast)-2.0_mpd*vecN(ifirst:ilast)*Av(i)
                A(ioff2+kn)=A(ioff2+kn)-2.0_mpd*vecVk(l)*Av(i)
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
    INTEGER(mpi) :: ifirst
    INTEGER(mpi) :: ilast
    INTEGER(mpi) :: ifirst2
    INTEGER(mpi) :: ilast2
    INTEGER(mpl) :: ioff1
    INTEGER(mpl) :: ioff2
    INTEGER(mpi) :: istat(3)
    INTEGER(mpi) :: j
    INTEGER(mpi) :: j2
    INTEGER(mpi) :: k
    INTEGER(mpi) :: k2
    INTEGER(mpi) :: kn
    INTEGER(mpi) :: kn2
    INTEGER(mpi) :: l
    INTEGER(mpi) :: l1
    INTEGER(mpi) :: l2
    INTEGER(mpl) :: length
    INTEGER(mpi) :: mbnd
    REAL(mpd) :: v2kn
    REAL(mpd) :: vtAv
    REAL(mpd) :: vtAvp
    REAL(mpd) :: vtvp
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: vecAv    ! A*v 
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: matvtvp  ! v^t*v' 
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: matvtAvp ! v^t*(A*v')
    REAL(mpd), DIMENSION(:), ALLOCATABLE :: matCoeff ! coefficients (d(A*v)=sum(c_i*v_i)) 
    INTEGER(mpi), DIMENSION(:,:), ALLOCATABLE :: irangeCoeff !< range for non zero part

    INTEGER(mpi), INTENT(IN)          :: m
    REAL(mpd), INTENT(IN OUT)         :: B(npar*m-(m*m-m)/2)
    LOGICAL, INTENT(IN)               :: t

    INTERFACE
        SUBROUTINE aprod(n,l,x,is,ie,y) ! y=A*x
            USE mpdef
            INTEGER(mpi), INTENT(in) :: n
            INTEGER(mpl), INTENT(in) :: l
            REAL(mpd), INTENT(IN)    :: x(n)
            INTEGER(mpi), INTENT(in) :: is
            INTEGER(mpi), INTENT(in) :: ie
            REAL(mpd), INTENT(OUT)   :: y(n)
        END SUBROUTINE aprod
    END INTERFACE
    !$POMP INST BEGIN(qlpssq)

    length=npar
    CALL mpalloc(vecAv,length,'qlpssq: A*v')    
    length=INT(ncon,mpl)*INT(ncon,mpl)
    CALL mpalloc(matvtvp,length,"qlpssq: v^t*v'")
    matvtvp=0._mpd
    CALL mpalloc(matvtAvp,length,"qlpssq: v^t*(A*v')")
    matvtAvp=0._mpd
    CALL mpalloc(matCoeff,length,'qlpssq: coefficients')
    matCoeff=0._mpd
    length=ncon
    CALL mpalloc(irangeCoeff,2_mpl,length,'qlpssq: non zero coefficient range')

    mbnd=max(0,m-1) ! band width without diagonal
 
    DO j=1,ncon
        k=j
        ! monitoring
        IF(monpg>0) CALL monpgs(k)
        IF (t) k=ncon+1-j
        kn=npar+k-ncon
        ioff1=INT(k-1,mpl)*INT(ncon,mpl)
        irangeCoeff(1,k)=ncon
        irangeCoeff(2,k)=1
        ! expand row 'k' of matV into vecN
        ! non-zero range (excluding 'kn')
        ifirst=irangeParNZ(1,k) 
        ilast=irangeParNZ(2,k)
        vecN=0._mpd
        vecN(ifirst:ilast)=matV(ioffRow(k)+1:ioffRow(k)+1+ilast-ifirst) 
        vecN(kn)=vecVk(k)  
        ! transformation A*v
        vecAv(1:npar)=0._mpd
        CALL aprod(npar,0_mpl,vecN(1:npar),ifirst,ilast,vecAv(1:npar))
        CALL aprod(npar,0_mpl,vecN(1:npar),kn,kn,vecAv(1:npar))
        ! products v^t*v'               
        DO j2=j+1,ncon
            k2=j2
            IF (t) k2=ncon+1-j2
            kn2=npar+k2-ncon
            ioff2=INT(k2-1,mpl)*INT(ncon,mpl)
            ! non-zero range (excluding 'kn')
            ifirst2=irangeParNZ(1,k2) 
            ilast2=irangeParNZ(2,k2)
            v2kn=0._mpd
            IF (kn >= ifirst2.AND.kn <= ilast2) v2kn=matV(ioffRow(k2)+1+kn-ifirst2)
            ! overlap regions
            l1=max(ifirst,ifirst2)
            l2=min(ilast,ilast2)
            vtvp=vecN(kn2)*vecVk(k2)+vecN(kn)*v2kn ! v^t*v'
            IF (l1 <= l2) vtvp=vtvp+dot_product(vecN(l1:l2), &
                matV(ioffRow(k2)+1+l1-ifirst2:ioffRow(k2)+1+l2-ifirst2))
            ! significant term?
            IF (abs(vtvp) > 16.0_mpd*epsilon(vtvp)) THEN
                matvtvp(ioff1+k2)=vtvp 
                matvtvp(ioff2+k)=vtvp 
            END IF    
        END DO
        matvtvp(ioff1+k)=1.0_mpd
        ! products v^t*(A*v')                 
        DO j2=1,j
            k2=j2
            IF (t) k2=ncon+1-j2
            kn2=npar+k2-ncon
            ! non-zero range (excluding 'kn')
            ifirst2=irangeParNZ(1,k2) 
            ilast2=irangeParNZ(2,k2)
            ! non-zero regions
            matvtAvp(ioff1+k2)=vecVk(k2)*vecAv(kn2)+dot_product(vecAv(ifirst2:ilast2), &
                matV(ioffRow(k2)+1:ioffRow(k2)+1+ilast2-ifirst2)) ! v'^t*(A*v)
        END DO
        ! update with (initial) A*v
        ioff2=0
        vtAv=matvtAvp(ioff1+k)
        DO i=1,kn
            ! correct with  2*(2v*vtAv*v^t - Av*v^t - (Av*v^t)^t)
            DO l=max(1,i-mbnd),i
                ioff2=ioff2+1
                B(ioff2)=B(ioff2)+2.0_mpd*((2.0_mpd*vecN(i)*vtAv-vecAv(i))*vecN(l)-vecAv(l)*vecN(i))
            END DO
        END DO
        ! off diagonal block
        DO i=kn+1,npar
            ! correct with -2Av*v^t
            DO l=max(1,i-mbnd),i
                ioff2=ioff2+1
                B(ioff2)=B(ioff2)-2.0_mpd*vecAv(i)*vecN(l)
            END DO
        END DO
    END DO

    ! corrections for A*v (as linear combination of v's)
    DO j=1,ncon
        k=j
        IF (t) k=ncon+1-j
        kn=npar+k-ncon
        ioff1=INT(k-1,mpl)*INT(ncon,mpl)
        ! expand row 'k' of matV into vecN
        ! non-zero range (excluding 'kn')
        ifirst=irangeParNZ(1,k) 
        ilast=irangeParNZ(2,k)
        vecN=0._mpd
        vecN(ifirst:ilast)=matV(ioffRow(k)+1:ioffRow(k)+1+ilast-ifirst) 
        vecN(kn)=vecVk(k)
        ! transformation (diagonal block)
        l1=irangeCoeff(1,k)
        l2=irangeCoeff(2,k)
        ! diagonal block
        ! v^t*A*v
        vtAv=matvtAvp(ioff1+k)+dot_product(matCoeff(ioff1+l1:ioff1+l2),matvtvp(ioff1+l1:ioff1+l2))
        ! expand correction to initial A*v
        vecAv(1:npar)=0._mpd
        istat=0
        DO k2=l1,l2
            IF (matCoeff(ioff1+k2) == 0._mpd) CYCLE
            if (istat(1)==0) istat(1)=k2
            istat(2)=k2
            istat(3)=istat(3)+1
            kn2=npar+k2-ncon
            ! expand row 'k2' of matV directly into vecAv
            ! non-zero range (excluding 'kn')
            ifirst2=irangeParNZ(1,k2) 
            ilast2=irangeParNZ(2,k2)          
            vecAv(ifirst2:ilast2)=vecAv(ifirst2:ilast2)+matCoeff(ioff1+k2)* &
                matV(ioffRow(k2)+1:ioffRow(k2)+1+ilast2-ifirst2)            
            vecAv(kn2)=vecAv(kn2)+matCoeff(ioff1+k2)*vecVk(k2)
        END DO
        ! update
        ioff2=0
        DO i=1,kn
            ! correct with  2*(2v*vtAv*v^t - Av*v^t - (Av*v^t)^t)
            DO l=max(1,i-mbnd),i
                ioff2=ioff2+1
                B(ioff2)=B(ioff2)+2.0_mpd*((2.0_mpd*vecN(i)*vtAv-vecAv(i))*vecN(l)-vecAv(l)*vecN(i))
            END DO
        END DO
        ! off diagonal block
        DO i=kn+1,npar
            ! correct with -2Av*v^t
            DO l=max(1,i-mbnd),i
                ioff2=ioff2+1
                B(ioff2)=B(ioff2)-2.0_mpd*vecAv(i)*vecN(l)
            END DO
        END DO
        ! correct A*v for the remainung v
        DO j2=j+1,ncon
            k2=j2
            IF (t) k2=ncon+1-j2
            kn2=npar+k2-ncon
            ioff2=INT(k2-1,mpl)*INT(ncon,mpl)
            vtvp=matvtvp(ioff1+k2) ! v^t*v'
            ! non-zero regions
            l1=irangeCoeff(1,k2)
            l2=irangeCoeff(2,k2)
            vtAvp=matvtAvp(ioff2+k)
            IF (l1 <= l2) vtAvp=vtAvp+dot_product(matCoeff(ioff2+l1:ioff2+l2),matvtvp(ioff1+l1:ioff1+l2)) ! v^t*(A*v')
            l1=min(l1,k)
            l2=max(l2,k)
            matCoeff(ioff2+k)=matCoeff(ioff2+k)+2.0_mpd*(2.0_mpd*vtAv*vtvp-vtAvp)
            IF (vtvp /= 0._mpd) THEN
                l1=min(l1,irangeCoeff(1,k))
                l2=max(l2,irangeCoeff(2,k))
                matCoeff(ioff2+l1:ioff2+l2)=matCoeff(ioff2+l1:ioff2+l2)-2.0_mpd*matCoeff(ioff1+l1:ioff1+l2)*vtvp
            END IF
            irangeCoeff(1,k2)=l1
            irangeCoeff(2,k2)=l2
        END DO
    END DO

    CALL mpdealloc(irangeCoeff)
    CALL mpdealloc(matCoeff)
    CALL mpdealloc(matvtAvp)
    CALL mpdealloc(matvtvp)
    CALL mpdealloc(vecAv)
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

    REAL(mpd), INTENT(IN)         :: d(ncon)
    REAL(mpd), INTENT(OUT)        :: y(ncon)

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
    INTEGER(mpi) :: ifirst
    INTEGER(mpi) :: ilast
    INTEGER(mpl) :: ioff1
    INTEGER(mpl) :: ioff2
    INTEGER(mpi) :: istat(6)
    INTEGER(mpi) :: j
    INTEGER(mpi) :: kn
    REAL(mpd) :: v1
    REAL(mpd) :: v2
    REAL(mpd) :: v3
    REAL(mpd) :: v4
    
    print *
    ioff1=0
    ioff2=0
    
    DO i=1, ncon
        kn=npar-ncon+i
        istat=0
        ! expand row 'i' of matV into vecN
        ! non-zero range (excluding 'kn')
        ifirst=irangeParNZ(1,i) 
        ilast=irangeParNZ(2,i)
        vecN=0._mpd
        vecN(ifirst:ilast)=matV(ioffRow(i)+1:ioffRow(i)+1+ilast-ifirst) 
        vecN(kn)=vecVk(i)
        DO j=1,npar+i-ncon
            IF (vecN(j) /= 0.0_mpd) THEN
                v2=vecN(j)
                IF (istat(3) == 0) THEN
                    istat(1)=j
                    v1=v2
                END IF     
                istat(2)=j
                istat(3)=istat(3)+1
            END IF
        END DO
        ioff1=ioff1+npar
        DO j=1,ncon
            IF (matL(ioff2+j) /= 0.0_mpd) THEN
                v4=matL(ioff2+j)
                IF (istat(6) == 0) THEN
                    istat(4)=j
                    v3=v4
                END IF    
                istat(5)=j
                istat(6)=istat(6)+1
            END IF
        END DO
        ioff2=ioff2+ncon
        print 100, i, istat, v1, v2, v3, v4, irangeParNZ(:,i)  
    END DO
    print *
100 FORMAT(" qldump",7I8,4G13.5,2I8)   
    
END SUBROUTINE qldump
