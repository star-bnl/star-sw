!> \file
!! Definitions.
!!
!! \author Claus Kleinwort, DESY, 2012 (Claus.Kleinwort@desy.de)
!!
!! \copyright
!! Copyright (c) 2012 - 2015 Deutsches Elektronen-Synchroton,
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

!> Definition of constants.
MODULE mpdef
    IMPLICIT NONE
    SAVE
    ! precision constants
    INTRINSIC :: selected_real_kind
    INTRINSIC :: selected_int_kind
    INTEGER, PARAMETER :: mpi4  = selected_int_kind(9)         !>  4 byte integer
    INTEGER, PARAMETER :: mpi8  = selected_int_kind(18)        !>  8 byte integer
    INTEGER, PARAMETER :: mpr4  = selected_real_kind(6, 37)    !>  4 byte float
    INTEGER, PARAMETER :: mpr8  = selected_real_kind(15, 307)  !>  8 byte float
    INTEGER, PARAMETER :: mpr16 = selected_real_kind(33, 4931) !> 16 byte float, gcc needs libquadmath    INTEGER, PARAMETER :: mpi = selected_int_kind(9)         !>  4 byte integer
    INTEGER, PARAMETER :: mpi  = mpi4                          !>  integer
    INTEGER, PARAMETER :: mpl  = mpi8                          !>  long integer
    INTEGER, PARAMETER :: mps  = mpr4                          !>  single precision
    INTEGER, PARAMETER :: mpd  = mpr8                          !>  double precision
    !> list items from steering file
    TYPE listItem
        INTEGER(mpi) :: label
        REAL(mpd) :: value
    END TYPE listItem
END MODULE mpdef
