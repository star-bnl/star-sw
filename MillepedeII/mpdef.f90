!> \file
!! Definitions.

!> Definition of constants.
MODULE mpdef
    IMPLICIT NONE
    SAVE
    ! precision constants
    INTRINSIC :: selected_real_kind
    INTRINSIC :: selected_int_kind
    INTEGER, PARAMETER :: mpi = selected_int_kind(9)         !>  4 byte integer
    INTEGER, PARAMETER :: mpl = selected_int_kind(18)        !>  8 byte integer
    INTEGER, PARAMETER :: mps = selected_real_kind(6, 37)    !>  4 byte float
    INTEGER, PARAMETER :: mpd = selected_real_kind(15, 307)  !>  8 byte float
    INTEGER, PARAMETER :: mpq = selected_real_kind(33, 4931) !> 16 byte float, gcc needs libquadmath
    !> list items from steering file
    TYPE listItem
        INTEGER(mpi) :: label
        REAL(mps) :: value
    END TYPE listItem
END MODULE mpdef
