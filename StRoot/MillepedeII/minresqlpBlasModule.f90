!> \file
!! MINRES-QLP BLAS subroutines.

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     File minresqlpBlasModule.f90
!
!     This file contains the following BLAS subroutines
!        ddot, dnrm2
!     required by subroutine MINRESQLP.
!
! Contributors:
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
! 24 Sep 2007: All parameters declared with correct intent
!              to avoid compiler warnings.
! 24 Oct 2007: Use real(8) instead of double precision or -r8.
! 24 May 2011: Use a module to package the BLAS subroutines. Use real(dp)
!              instead of real(8), where dp is a constant defined in
!              minresqlpDataModule and used in other program units.
! 12 Jul 2011: Created complex version zminresqlpBlasModule.f90
!              from real version minresqlpBlasModule.f90.
! 03 Aug 2013: dp constants 0.d0 and 1.d0 defined with _dp.
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

module minresqlpBlasModule

  use  minresqlpDataModule,    only : dp, ip, zero, one
  implicit none

  public   :: ddot, dnrm2

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!> DDOT forms the dot product of two vectors.
!
!  Discussion:
!    This routine uses double precision real arithmetic.
!    This routine uses unrolled loops for increments equal to one.
!
!  Modified:
!    16 May 2005
!
!  Author:
!    Jack Dongarra
!    Fortran90 translation by John Burkardt.
!
!  Reference:
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Algorithm 539,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer N, the number of entries in the vectors.
!
!    Input, real ( kind = dp ) DX(*), the first vector.
!
!    Input, integer INCX, the increment between successive entries in DX.
!
!    Input, real ( kind = dp ) DY(*), the second vector.
!
!    Input, integer INCY, the increment between successive entries in DY.
!
!    Output, real ( kind = dp ) DDOT, the sum of the product of the
!    corresponding entries of DX and DY.

      real(dp) function ddot(n,dx,incx,dy,incy)

      implicit none
      integer(ip), intent(in) :: n,incx,incy
      real(dp),    intent(in) :: dx(*),dy(*)

      real(dp)    :: dtemp
      integer(ip) :: i,ix,iy,m

      ddot  = zero
      dtemp = zero
      if ( n <= 0 ) then
         return
      end if

!  Code for unequal increments or equal increments
!  not equal to 1.

      if ( incx /= 1 .or. incy /= 1 ) then

         if ( 0 <= incx ) then
            ix = 1
         else
            ix = ( - n + 1 ) * incx + 1
         end if

         if ( 0 <= incy ) then
            iy = 1
         else
            iy = ( - n + 1 ) * incy + 1
         end if

         do i = 1, n
            dtemp = dtemp + dx(ix) * dy(iy)
            ix = ix + incx
            iy = iy + incy
         end do

!  Code for both increments equal to 1.

        else

           m = mod ( n, 5 )

           do i = 1, m
              dtemp = dtemp + dx(i) * dy(i)
           end do

           do i = m+1, n, 5
              dtemp = dtemp + dx(i)*dy(i) + dx(i+1)*dy(i+1) + dx(i+2)*dy(i+2) &
                                          + dx(i+3)*dy(i+3) + dx(i+4)*dy(i+4)
           end do

        end if

        ddot = dtemp
        return
end function ddot

!*****************************************************************************
!
!> DNRM2 returns the euclidean norm of a vector.
!
!  Discussion:
!    This routine uses real(dp) real arithmetic.
!     DNRM2 ( X ) = sqrt ( X' * X )
!
!  Modified:
!    16 May 2005
!
!  Author:
!    Sven Hammarling
!    Fortran90 translation by John Burkardt.
!
!  Reference:
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Algorithm 539,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer N, the number of entries in the vector.
!
!    Input, real ( kind = dp ) X(*), the vector whose norm is to be computed.
!
!    Input, integer INCX, the increment between successive entries of X.
!
!    Output, real ( kind = dp ) DNRM2, the Euclidean norm of X.

      real(dp) function dnrm2 ( n, x, incx )

      implicit none
      integer(ip), intent(in) :: n,incx
      real(dp),    intent(in) :: x(*)

      integer(ip) :: ix
      real(dp)    :: ssq,absxi,norm,scale

      if ( n < 1 .or. incx < 1 ) then
         norm  = zero
      else if ( n == 1 ) then
         norm  = abs ( x(1) )
      else
         scale = zero
         ssq   = one

         do ix = 1, 1 + ( n - 1 )*incx, incx
            if ( x(ix) /= zero ) then
               absxi = abs ( x(ix) )
               if ( scale < absxi ) then
                  ssq = 1_dp + ssq * ( scale / absxi )**2
                  scale = absxi
               else
                  ssq = ssq + ( absxi / scale )**2
               end if
            end if
         end do
         norm  = scale * sqrt ( ssq )
      end if

      dnrm2 = norm
      return
end function dnrm2

end module minresqlpBlasModule
