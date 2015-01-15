
!> \file
!! MINRES (data) definitions.

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! File minresDataModule.f90
!
! Defines real(kind=dp) and a few constants for use in other modules.
!
! 14 Oct 2007: First version implemented after realizing -r8 is not
!              a standard compiler option.
! 15 Oct 2007: Temporarily used real(8) everywhere.
! 16 Oct 2007: Found that we need
!                 use minresDataModule
!              at the beginning of modules AND inside interfaces.
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!> Defines real(kind=dp) and a few constants for use in other modules.
module minresDataModule
  use mpdef, only: mpd

  implicit none

  intrinsic                        ::      selected_real_kind
  integer,       parameter, public :: dp = mpd !selected_real_kind(15)
  real(kind=dp), parameter, public :: zero = 0.0_dp, one = 1.0_dp

end module minresDataModule
