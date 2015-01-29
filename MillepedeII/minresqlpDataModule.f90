!> \file
!! MINRES-QLP (data) definitions.

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! File minresqlpDataModule.f90
!
!> Defines precision and range in real(kind=dp) and integer(kind=ip) for
!! portability and a few constants for use in other modules.
!
!
! Authors:
!     Sou-Cheng Choi <sctchoi@uchicago.edu>
!     Computation Institute (CI)
!     University of Chicago
!     Chicago, IL 60637, USA
!
!     Michael Saunders <saunders@stanford.edu>
!     Systems Optimization Laboratory (SOL)
!     Stanford University
!     Stanford, CA 94305-4026, USA
!
! History:
! 14 Oct 2007: First version implemented after realizing -r8 is not
!              a standard compiler option.
! 15 Oct 2007: Temporarily used real(8) everywhere.
! 16 Oct 2007: Found that we need
!                 use minresqlpDataModule
!              at the beginning of modules AND inside interfaces.
! 20 Aug 2012: (1) Added single real kind 'sp' and integer kind 'ip'.
!              (2) Added smallest and largest real positive 'realmin'
!                  and 'realmax'.
!              (3) Added single precision kind 'sp'.
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

module minresqlpDataModule
  use mpdef, only: mpi, mps, mpd

  implicit none

  intrinsic                   :: selected_real_kind, selected_int_kind, tiny, huge

  ! The following reals are provided for portability. Do not use 'DOUBLE PRECISION'.
  integer,  parameter, public :: dp      = mpd !selected_real_kind(15,307)    ! 64-bit real, default
  integer,  parameter, public :: sp      = mps !selected_real_kind(6,37)      ! 32-bit real
 !integer,  parameter, public :: qp      = selected_real_kind(33,4931)   !128-bit real

  integer,  parameter, public :: ip      = mpi !selected_int_kind(9)          ! R: (-10^R, 10^R)

  real(dp), parameter, public :: zero    = 0.0_dp, one =  1.0_dp, eps = epsilon(zero)
  real(dp), parameter, public :: realmin = tiny(one), realmax = huge(one)

  integer,  parameter, public :: prcsn   = precision(zero) ! first argument of selected_real_kind

  ! WARN: turning on debug could significantly slow down the program due to file output
  logical,             public :: debug        = .false.
  logical,             public :: testSymortho = .true., testMtx = .true.
end module minresqlpDataModule
