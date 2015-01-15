!> \file
!! Dynamic memory management.
!!
!! \author C. Kleinwort, DESY, 2012 (Claus.Kleinwort@desy.de)

!> (De)Allocate vectors and arrays.
MODULE mpdalc
    USE mpdef
    IMPLICIT NONE
    SAVE
    ! variables
    INTEGER(mpl) :: numwordsalloc = 0 !< current dynamic memory allocation (words)
    INTEGER(mpl) :: maxwordsalloc = 0 !< peak dynamic memory allocation (words)
    INTEGER(mpi) :: nummpalloc = 0     !< number of dynamic allocations
    INTEGER(mpi) :: nummpdealloc = 0   !< number of dynamic deallocations
    INTEGER(mpi) :: printflagalloc = 0 !< print flag for dynamic allocations

    !> allocate array
    INTERFACE mpalloc
        MODULE PROCEDURE mpallocdvec, mpallocfvec, mpallocivec, &
            mpallocfarr, mpallociarr, mpalloclarr, mpalloclist, mpalloccvec
    END INTERFACE mpalloc
    !> deallocate array
    INTERFACE mpdealloc
        MODULE PROCEDURE mpdeallocdvec, mpdeallocfvec, mpdeallocivec, &
            mpdeallocfarr, mpdeallociarr, mpdealloclarr, mpdealloclist, mpdealloccvec
    END INTERFACE mpdealloc

CONTAINS
    ! allocate dynamic vector or array
    !> allocate (1D) double precision array
    SUBROUTINE mpallocdvec(array,length,text)
        REAL(mpd), DIMENSION(:), INTENT(IN OUT), ALLOCATABLE :: array
        INTEGER(mpl), INTENT(IN) :: length
        CHARACTER (LEN=*), INTENT(IN) :: text

        INTEGER(mpi) :: ifail
        ALLOCATE (array(length),stat=ifail)
        CALL mpalloccheck(ifail,(mpd*length)/mpi,text)
    END SUBROUTINE mpallocdvec

    !> allocate (1D) single precision array
    SUBROUTINE mpallocfvec(array,length,text)
        REAL(mps), DIMENSION(:), INTENT(IN OUT), ALLOCATABLE :: array
        INTEGER(mpl), INTENT(IN) :: length
        CHARACTER (LEN=*), INTENT(IN) :: text

        INTEGER(mpi) :: ifail
        ALLOCATE (array(length),stat=ifail)
        CALL mpalloccheck(ifail,(mps*length)/mpi,text)
    END SUBROUTINE mpallocfvec

    !> allocate (1D) integer array
    SUBROUTINE mpallocivec(array,length,text)
        INTEGER(mpi), DIMENSION(:), INTENT(IN OUT), ALLOCATABLE :: array
        INTEGER(mpl), INTENT(IN) :: length
        CHARACTER (LEN=*), INTENT(IN) :: text

        INTEGER(mpi) :: ifail
        ALLOCATE (array(length),stat=ifail)
        CALL mpalloccheck(ifail,length,text)
    END SUBROUTINE mpallocivec

    !> allocate (2D) single precision array
    SUBROUTINE mpallocfarr(array,rows,cols,text)
        REAL(mps), DIMENSION(:,:), INTENT(IN OUT), ALLOCATABLE :: array
        INTEGER(mpl), INTENT(IN) :: rows
        INTEGER(mpl), INTENT(IN) :: cols
        CHARACTER (LEN=*), INTENT(IN)  :: text

        INTEGER(mpi) :: ifail
        ALLOCATE (array(rows,cols),stat=ifail)
        CALL mpalloccheck(ifail,(mps*rows*cols)/mpi,text)
    END SUBROUTINE mpallocfarr

    !> allocate (2D) INTEGER(mpi) array
    SUBROUTINE mpallociarr(array,rows,cols,text)
        INTEGER(mpi), DIMENSION(:,:), INTENT(IN OUT), ALLOCATABLE :: array
        INTEGER(mpl), INTENT(IN) :: rows
        INTEGER(mpl), INTENT(IN) :: cols
        CHARACTER (LEN=*), INTENT(IN)  :: text

        INTEGER(mpi) :: ifail
        ALLOCATE (array(rows,cols),stat=ifail)
        CALL mpalloccheck(ifail,rows*cols,text)
    END SUBROUTINE mpallociarr

    !> allocate (2D) large integer array
    SUBROUTINE mpalloclarr(array,rows,cols,text)
        INTEGER(mpl), DIMENSION(:,:), INTENT(IN OUT), ALLOCATABLE :: array
        INTEGER(mpl), INTENT(IN) :: rows
        INTEGER(mpl), INTENT(IN) :: cols
        CHARACTER (LEN=*), INTENT(IN)  :: text

        INTEGER(mpi) :: ifail
        ALLOCATE (array(rows,cols),stat=ifail)
        CALL mpalloccheck(ifail,(mpl*rows*cols)/mpi,text)
    END SUBROUTINE mpalloclarr

    !> allocate (1D) list item array
    SUBROUTINE mpalloclist(array,length,text)
        TYPE(listItem), DIMENSION(:), INTENT(IN OUT), ALLOCATABLE :: array
        INTEGER(mpl), INTENT(IN) :: length
        CHARACTER (LEN=*), INTENT(IN) :: text

        INTEGER(mpi) :: ifail
        ALLOCATE (array(length),stat=ifail)
        CALL mpalloccheck(ifail,((mps+mpi)*length)/mpi,text)
    END SUBROUTINE mpalloclist

    !> allocate (1D) character array
    SUBROUTINE mpalloccvec(array,length,text)
        CHARACTER, DIMENSION(:), INTENT(IN OUT), ALLOCATABLE :: array
        INTEGER(mpl), INTENT(IN) :: length
        CHARACTER (LEN=*), INTENT(IN) :: text

        INTEGER(mpi) :: ifail
        ALLOCATE (array(length),stat=ifail)
        CALL mpalloccheck(ifail,(length+mpi-1)/mpi,text)
    END SUBROUTINE mpalloccvec

    !> check allocation
    SUBROUTINE mpalloccheck(ifail,numwords,text)
        INTEGER(mpi), INTENT(IN) :: ifail
        INTEGER(mpl), INTENT(IN) :: numwords
        CHARACTER (LEN=*), INTENT(IN)  :: text
        IF (ifail == 0) THEN
            nummpalloc=nummpalloc+1
            numwordsalloc = numwordsalloc + numwords
            maxwordsalloc = MAX(maxwordsalloc, numwordsalloc)
            IF (printflagalloc /= 0) THEN
                print *, ' MPALLOC allocated ', numwords, ' words for : ', text
                print *, ' words used ', numwordsalloc, maxwordsalloc
            ENDIF
        ELSE
            print *, ' MPALLOC failed to allocate ', numwords, ' words for : ', text
            print *, ' MPALLOC words used ', numwordsalloc, maxwordsalloc
            print *, ' MPALLOC stat = ', ifail
            CALL peend(30,'Aborted, memory allocation failed')
            STOP
        ENDIF
    END SUBROUTINE mpalloccheck
    ! deallocate dynamic vector or array
    !> deallocate (1D) double precision array
    SUBROUTINE mpdeallocdvec(array)
        REAL(mpd), DIMENSION(:), INTENT(IN OUT), ALLOCATABLE :: array

        INTEGER(mpi) :: ifail
        INTEGER(mpl) :: isize
        isize = (mpd*size(array,kind=mpl))/mpi
        DEALLOCATE (array,stat=ifail)
        CALL mpdealloccheck(ifail,isize)
    END SUBROUTINE mpdeallocdvec

    !> deallocate (1D) single precision array
    SUBROUTINE mpdeallocfvec(array)
        REAL(mps), DIMENSION(:), INTENT(IN OUT), ALLOCATABLE :: array

        INTEGER(mpi) :: ifail
        INTEGER(mpl) :: isize
        isize = (mps*size(array,kind=mpl))/mpi
        DEALLOCATE (array,stat=ifail)
        CALL mpdealloccheck(ifail,isize)
    END SUBROUTINE mpdeallocfvec

    !> deallocate (1D) integer array
    SUBROUTINE mpdeallocivec(array)
        INTEGER(mpi), DIMENSION(:), INTENT(IN OUT), ALLOCATABLE :: array

        INTEGER(mpi) :: ifail
        INTEGER(mpl) :: isize
        isize = size(array,kind=mpl)
        DEALLOCATE (array,stat=ifail)
        CALL mpdealloccheck(ifail,isize)
    END SUBROUTINE mpdeallocivec

    !> allocate (2D) single precision array
    SUBROUTINE mpdeallocfarr(array)
        REAL(mps), DIMENSION(:,:), INTENT(IN OUT), ALLOCATABLE :: array

        INTEGER(mpi) :: ifail
        INTEGER(mpl) :: isize
        isize = (mps*size(array,kind=mpl))/mpi
        DEALLOCATE (array,stat=ifail)
        CALL mpdealloccheck(ifail,isize)
    END SUBROUTINE mpdeallocfarr

    !> allocate (2D) integer array
    SUBROUTINE mpdeallociarr(array)
        INTEGER(mpi), DIMENSION(:,:), INTENT(IN OUT), ALLOCATABLE :: array

        INTEGER(mpi) :: ifail
        INTEGER(mpl) :: isize
        isize = size(array,kind=mpl)
        DEALLOCATE (array,stat=ifail)
        CALL mpdealloccheck(ifail,isize)
    END SUBROUTINE mpdeallociarr

     !> deallocate (2D) large integer array
    SUBROUTINE mpdealloclarr(array)
        INTEGER(mpl), DIMENSION(:,:), INTENT(IN OUT), ALLOCATABLE :: array

        INTEGER(mpi) :: ifail
        INTEGER(mpl) :: isize
        isize = (mpl*size(array,kind=mpl))/mpi
        DEALLOCATE (array,stat=ifail)
        CALL mpdealloccheck(ifail,isize)
    END SUBROUTINE mpdealloclarr

    !> deallocate (1D) list item array
    SUBROUTINE mpdealloclist(array)
        TYPE(listItem), DIMENSION(:), INTENT(IN OUT), ALLOCATABLE :: array

        INTEGER(mpi) :: ifail
        INTEGER(mpl) :: isize
        isize = ((mpi+mps)*size(array,kind=mpl))/mpi
        DEALLOCATE (array,stat=ifail)
        CALL mpdealloccheck(ifail,isize)
    END SUBROUTINE mpdealloclist

    !> deallocate (1D) character array
    SUBROUTINE mpdealloccvec(array)
        CHARACTER, DIMENSION(:), INTENT(IN OUT), ALLOCATABLE :: array

        INTEGER(mpi) :: ifail
        INTEGER(mpl) :: isize
        isize = (size(array,kind=mpl)+mpi-1)/mpi
        DEALLOCATE (array,stat=ifail)
        CALL mpdealloccheck(ifail,isize)
    END SUBROUTINE mpdealloccvec

    !> check deallocation
    SUBROUTINE mpdealloccheck(ifail,numwords)
        INTEGER(mpi), INTENT(IN) :: ifail
        INTEGER(mpl), INTENT(IN) :: numwords
        IF (ifail == 0) THEN
            numwordsalloc = numwordsalloc - numwords
            nummpdealloc=nummpdealloc+1
            IF (printflagalloc /= 0) THEN
                print *, ' MPDEALLOC deallocated ', numwords, ' words '
                print *, ' words used ', numwordsalloc, maxwordsalloc
            ENDIF
        ELSE
            print *, ' MPDEALLOC failed to deallocate ', numwords, ' words'
            print *, ' MPDEALLOC words used ', numwordsalloc, maxwordsalloc
            print *, ' MPDEALLOC stat = ', ifail
            CALL peend(31,'Aborted, memory deallocation failed')
            STOP
        ENDIF
    END SUBROUTINE mpdealloccheck

END MODULE mpdalc
