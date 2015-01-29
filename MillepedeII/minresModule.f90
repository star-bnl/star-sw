!> \file
!! MINRES algorithm.

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! File minresModule.f90
!
!> MINRES solves symmetric systems Ax = b or min ||Ax - b||_2,
!! where the matrix A may be indefinite and/or singular.
!! \verbatim
!!
!! The software for MINRES (f90 version) is provided by SOL, Stanford University
!! under the terms of the OSI Common Public License (CPL):
!! http://www.opensource.org/licenses/cpl1.0.php
!!
!! Contributors:
!!     Chris Paige <chris@cs.mcgill.ca>
!!     Sou-Cheng Choi <scchoi@stanford.edu>
!!
!!     Michael Saunders <saunders@stanford.edu>
!!     Systems Optimization Laboratory (SOL)
!!     Stanford University
!!     Stanford, CA 94305-4026, USA
!!     (650)723-1875
!!
!! 09 Oct 2007: F90 version constructed from the F77 version.
!!              Initially used compiler option -r8, but this is nonstandard.
!! 15 Oct 2007: Test on Arnorm = ||Ar|| added to recognize singular systems.
!! 15 Oct 2007: Temporarily used real(8) everywhere.
!! 16 Oct 2007: Use minresDataModule to define dp = selected_real_kind(15).
!!              We need "use minresDataModule"
!!              at the beginning of modules AND inside interfaces.
!!
!!              g95 compiles successfully with the following options:
!!   g95 -c -g -O0 -pedantic -Wall -Wextra -fbounds-check -ftrace=full minresModule.f90
!!
!! \endverbatim
!!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

module minresModule

  use  minresDataModule, only : dp

  implicit none
  public   :: MINRES

contains

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !> Solution of linear equation system.
    !!
    !! \verbatim
    !!-------------------------------------------------------------------
    !!
    !! MINRES  is designed to solve the system of linear equations
    !!
    !!    Ax = b
    !!
    !! or the least-squares problem
    !!
    !!    min ||Ax - b||_2,
    !!
    !! where A is an n by n symmetric matrix and b is a given vector.
    !! The matrix A may be indefinite and/or singular.
    !!
    !! 1. If A is known to be positive definite, the Conjugate Gradient
    !! Method might be preferred, since it requires the same number
    !! of iterations as MINRES but less work per iteration.
    !!
    !! 2. If A is indefinite but Ax = b is known to have a solution
    !! (e.g. if A is nonsingular), SYMMLQ might be preferred,
    !! since it requires the same number of iterations as MINRES
    !! but slightly less work per iteration.
    !!
    !! The matrix A is intended to be large and sparse.  It is accessed
    !! by means of a subroutine call of the form
    !! SYMMLQ development:
    !!
    !!    call Aprod ( n, x, y )
    !!
    !! which must return the product y = Ax for any given vector x.
    !!
    !!
    !! More generally, MINRES is designed to solve the system
    !!
    !!    (A - shift*I) x = b
    !! or
    !!    min ||(A - shift*I) x - b||_2,
    !!
    !! where  shift  is a specified scalar value.  Again, the matrix
    !! (A - shift*I) may be indefinite and/or singular.
    !! The work per iteration is very slightly less if  shift = 0.
    !!
    !! Note: If  shift  is an approximate eigenvalue of  A
    !! and  b  is an approximate eigenvector,  x  might prove to be
    !! a better approximate eigenvector, as in the methods of
    !! inverse iteration and/or Rayleigh-quotient iteration.
    !! However, we're not yet sure on that -- it may be better to use SYMMLQ.
    !!
    !! A further option is that of preconditioning, which may reduce
    !! the number of iterations required.  If M = C C' is a positive
    !! definite matrix that is known to approximate  (A - shift*I)
    !! in some sense, and if systems of the form  My = x  can be
    !! solved efficiently, the parameters precon and Msolve may be
    !! used (see below).  When  precon = .true., MINRES will
    !! implicitly solve the system of equations
    !!
    !!    P (A - shift*I) P' xbar  =  P b,
    !!
    !! i.e.             Abar xbar  =  bbar
    !! where                    P  =  C**(-1),
    !!                       Abar  =  P (A - shift*I) P',
    !!                       bbar  =  P b,
    !!
    !! and return the solution       x  =  P' xbar.
    !! The associated residual is rbar  =  bbar - Abar xbar
    !!                                  =  P (b - (A - shift*I)x)
    !!                                  =  P r.
    !!
    !! In the discussion below, eps refers to the machine precision.
    !!
    !! Parameters
    !! ----------
    !!
    !! n       input      The dimension of the matrix A.
    !! b(n)    input      The rhs vector b.
    !! x(n)    output     Returns the computed solution x.
    !!
    !! Aprod   external   A subroutine defining the matrix A.
    !!                       call Aprod ( n, x, y )
    !!                    must return the product y = Ax
    !!                    without altering the vector x.
    !!
    !! Msolve  external   An optional subroutine defining a
    !!                    preconditioning matrix M, which should
    !!                    approximate (A - shift*I) in some sense.
    !!                    M must be positive definite.
    !!
    !!                       call Msolve( n, x, y )
    !!
    !!                    must solve the linear system My = x
    !!                    without altering the vector x.
    !!
    !!                    In general, M should be chosen so that Abar has
    !!                    clustered eigenvalues.  For example,
    !!                    if A is positive definite, Abar would ideally
    !!                    be close to a multiple of I.
    !!                    If A or A - shift*I is indefinite, Abar might
    !!                    be close to a multiple of diag( I  -I ).
    !!
    !! checkA  input      If checkA = .true., an extra call of Aprod will
    !!                    be used to check if A is symmetric.  Also,
    !!                    if precon = .true., an extra call of Msolve
    !!                    will be used to check if M is symmetric.
    !!
    !! precon  input      If precon = .true., preconditioning will
    !!                    be invoked.  Otherwise, subroutine Msolve
    !!                    will not be referenced; in this case the
    !!                    actual parameter corresponding to Msolve may
    !!                    be the same as that corresponding to Aprod.
    !!
    !! shift   input      Should be zero if the system Ax = b is to be
    !!                    solved.  Otherwise, it could be an
    !!                    approximation to an eigenvalue of A, such as
    !!                    the Rayleigh quotient b'Ab / (b'b)
    !!                    corresponding to the vector b.
    !!                    If b is sufficiently like an eigenvector
    !!                    corresponding to an eigenvalue near shift,
    !!                    then the computed x may have very large
    !!                    components.  When normalized, x may be
    !!                    closer to an eigenvector than b.
    !!
    !! nout    input      A file number.
    !!                    If nout > 0, a summary of the iterations
    !!                    will be printed on unit nout.
    !!
    !! itnlim  input      An upper limit on the number of iterations.
    !!
    !! rtol    input      A user-specified tolerance.  MINRES terminates
    !!                    if it appears that norm(rbar) is smaller than
    !!                       rtol * norm(Abar) * norm(xbar),
    !!                    where rbar is the transformed residual vector,
    !!                       rbar = bbar - Abar xbar.
    !!
    !!                    If shift = 0 and precon = .false., MINRES
    !!                    terminates if norm(b - A*x) is smaller than
    !!                       rtol * norm(A) * norm(x).
    !!
    !! istop   output     An integer giving the reason for termination...
    !!
    !!          -1        beta2 = 0 in the Lanczos iteration; i.e. the
    !!                    second Lanczos vector is zero.  This means the
    !!                    rhs is very special.
    !!                    If there is no preconditioner, b is an
    !!                    eigenvector of A.
    !!                    Otherwise (if precon is true), let My = b.
    !!                    If shift is zero, y is a solution of the
    !!                    generalized eigenvalue problem Ay = lambda My,
    !!                    with lambda = alpha1 from the Lanczos vectors.
    !!
    !!                    In general, (A - shift*I)x = b
    !!                    has the solution         x = (1/alpha1) y
    !!                    where My = b.
    !!
    !!           0        b = 0, so the exact solution is x = 0.
    !!                    No iterations were performed.
    !!
    !!           1        Norm(rbar) appears to be less than
    !!                    the value  rtol * norm(Abar) * norm(xbar).
    !!                    The solution in  x  should be acceptable.
    !!
    !!           2        Norm(rbar) appears to be less than
    !!                    the value  eps * norm(Abar) * norm(xbar).
    !!                    This means that the residual is as small as
    !!                    seems reasonable on this machine.
    !!
    !!           3        Norm(Abar) * norm(xbar) exceeds norm(b)/eps,
    !!                    which should indicate that x has essentially
    !!                    converged to an eigenvector of A
    !!                    corresponding to the eigenvalue shift.
    !!
    !!           4        Acond (see below) has exceeded 0.1/eps, so
    !!                    the matrix Abar must be very ill-conditioned.
    !!                    x may not contain an acceptable solution.
    !!
    !!           5        The iteration limit was reached before any of
    !!                    the previous criteria were satisfied.
    !!
    !!           6        The matrix defined by Aprod does not appear
    !!                    to be symmetric.
    !!                    For certain vectors y = Av and r = Ay, the
    !!                    products y'y and r'v differ significantly.
    !!
    !!           7        The matrix defined by Msolve does not appear
    !!                    to be symmetric.
    !!                    For vectors satisfying My = v and Mr = y, the
    !!                    products y'y and r'v differ significantly.
    !!
    !!           8        An inner product of the form  x' M**(-1) x
    !!                    was not positive, so the preconditioning matrix
    !!                    M does not appear to be positive definite.
    !!
    !!                    If istop >= 5, the final x may not be an
    !!                    acceptable solution.
    !!
    !! itn     output     The number of iterations performed.
    !!
    !! Anorm   output     An estimate of the norm of the matrix operator
    !!                    Abar = P (A - shift*I) P',   where P = C**(-1).
    !!
    !! Acond   output     An estimate of the condition of Abar above.
    !!                    This will usually be a substantial
    !!                    under-estimate of the true condition.
    !!
    !! rnorm   output     An estimate of the norm of the final
    !!                    transformed residual vector,
    !!                       P (b  -  (A - shift*I) x).
    !!
    !! ynorm   output     An estimate of the norm of xbar.
    !!                    This is sqrt( x'Mx ).  If precon is false,
    !!                    ynorm is an estimate of norm(x).
    !!-------------------------------------------------------------------
    !! MINRES is an implementation of the algorithm described in
    !! the following reference:
    !!
    !! C. C. Paige and M. A. Saunders (1975),
    !! Solution of sparse indefinite systems of linear equations,
    !! SIAM J. Numer. Anal. 12(4), pp. 617-629.
    !!-------------------------------------------------------------------
    !!
    !!
    !! MINRES development:
    !!    1972: First version, similar to original SYMMLQ.
    !!          Later lost @#%*!!
    !!    Oct 1995: Tried to reconstruct MINRES from
    !!              1995 version of SYMMLQ.
    !! 30 May 1999: Need to make it more like LSQR.
    !!              In middle of major overhaul.
    !! 19 Jul 2003: Next attempt to reconstruct MINRES.
    !!              Seems to need two vectors more than SYMMLQ.  (w1, w2)
    !!              Lanczos is now at the top of the loop,
    !!              so the operator Aprod is called in just one place
    !!              (not counting the initial check for symmetry).
    !! 22 Jul 2003: Success at last.  Preconditioning also works.
    !!              minres.f added to http://www.stanford.edu/group/SOL/.
    !!
    !! 16 Oct 2007: Added a stopping rule for singular systems,
    !!              as derived in Sou-Cheng Choi's PhD thesis.
    !!              Note that ||Ar|| small => r is a null vector for A.
    !!              Subroutine minrestest2 in minresTestModule.f90
    !!              tests this option.  (NB: Not yet working.)
    !!-------------------------------------------------------------------
    !! \endverbatim
  subroutine MINRES( n, Aprod, Msolve, b, shift, checkA, precon, &
                     x, itnlim, nout, rtol,                      &
                     istop, itn, Anorm, Acond, rnorm, Arnorm, ynorm )

    integer,  intent(in)    :: n, itnlim, nout
    logical,  intent(in)    :: checkA, precon
    real(dp), intent(in)    :: b(n)
    real(dp), intent(in)    :: shift, rtol
    real(dp), intent(out)   :: x(n)
    integer,  intent(out)   :: istop, itn
    real(dp), intent(out)   :: Anorm, Acond, rnorm, Arnorm, ynorm

    interface
       subroutine Aprod (n,x,y)                   ! y := A*x
         use       minresDataModule
         integer,  intent(in)    :: n
         real(dp), intent(in)    :: x(n)
         real(dp), intent(out)   :: y(n)
       end subroutine Aprod

       subroutine Msolve(n,x,y)                   ! Solve M*y = x
         use       minresDataModule
         integer,  intent(in)    :: n
         real(dp), intent(in)    :: x(n)
         real(dp), intent(out)   :: y(n)
       end subroutine Msolve
    end interface

!     Local arrays and variables
      real(dp)  :: r1(n), r2(n), v(n), w(n), w1(n), w2(n), y(n)
      real(dp)  :: alfa  , beta  , beta1 , cs    ,          &
                   dbar  , delta , denom , diag  ,          &
                   eps   , epsa  , epsln , epsr  , epsx  ,  &
                   gamma , gbar  , gmax  , gmin  ,          &
                   oldb  , oldeps, qrnorm, phi   , phibar,  &
                   rhs1  , rhs2  , rnorml, rootl ,          &
                   Arnorml,        relArnorml,              &
                   s     , sn    , t     , tnorm2, ynorm2, z
      integer   :: i
      logical   :: debug, prnt

    ! Local constants
      real(dp),         parameter :: zero =  0.0,  one = 1.0
      real(dp),         parameter :: ten  = 10.0
      character(len=*), parameter :: enter = ' Enter MINRES.  '
      character(len=*), parameter :: exitt = ' Exit  MINRES.  '
      character(len=*), parameter :: msg(-1:8) =                  &
        (/ 'beta2 = 0.  If M = I, b and x are eigenvectors of A', & ! -1
           'beta1 = 0.  The exact solution is  x = 0           ', & !  0
           'Requested accuracy achieved, as determined by rtol ', & !  1
           'Reasonable accuracy achieved, given eps            ', & !  2
           'x has converged to an eigenvector                  ', & !  3
           'Acond has exceeded 0.1/eps                         ', & !  4
           'The iteration limit was reached                    ', & !  5
           'Aprod  does not define a symmetric matrix          ', & !  6
           'Msolve does not define a symmetric matrix          ', & !  7
           'Msolve does not define a pos-def preconditioner    ' /) !  8
    !-------------------------------------------------------------------

    intrinsic       :: abs, dot_product, epsilon, min, max, sqrt

    ! Print heading and initialize.

    debug = .false.
    eps   = epsilon(eps)
    if (nout > 0) then
       write(nout, 1000) enter, n, checkA, precon, itnlim, rtol, shift
    end if
    istop  = 0
    itn    = 0
    Anorm  = zero
    Acond  = zero
    rnorm  = zero
    ynorm  = zero
    x(1:n) = zero
    Arnorml = zero
    gmin = zero
    gmax = zero

    !-------------------------------------------------------------------
    ! Set up y and v for the first Lanczos vector v1.
    ! y = beta1 P' v1, where P = C**(-1).
    ! v is really P' v1.
    !-------------------------------------------------------------------
    y      = b
    r1     = b
    if ( precon ) call Msolve( n, b, y )
    beta1  = dot_product(b,y)

    if (beta1 < zero) then     ! M must be indefinite.
       istop = 8
       go to 900
    end if

    if (beta1 == zero) then    ! b = 0 exactly.  Stop with x = 0.
       istop = 0
       go to 900
    end if

    beta1  = sqrt( beta1 )     ! Normalize y to get v1 later.

    !-------------------------------------------------------------------
    ! See if Msolve is symmetric.
    !-------------------------------------------------------------------
    if (checkA  .and.  precon) then
       call Msolve( n, y, r2 )
       s      = dot_product(y ,y )
       t      = dot_product(r1,r2)
       z      = abs(s - t)
       epsa   = (s + eps) * eps**0.33333
       if (z > epsa) then
          istop = 7
          go to 900
       end if
    end if

    !-------------------------------------------------------------------
    ! See if Aprod  is symmetric.  Initialize Arnorm.
    !-------------------------------------------------------------------
    if (checkA) then
       call Aprod ( n, y, w )
       call Aprod ( n, w, r2 )
       s      = dot_product(w,w )
       t      = dot_product(y,r2)
       z      = abs(s - t)
       epsa   = (s + eps) * eps**0.33333
       if (z > epsa) then
          istop = 6
          go to 900
       end if
       Arnorml = sqrt(s);
    else
       call Aprod ( n, y, w )
       Arnorml = sqrt( dot_product(w,w) )
    end if

    !-------------------------------------------------------------------
    ! Initialize other quantities.
    !-------------------------------------------------------------------
    oldb   = zero
    beta   = beta1
    dbar   = zero
    epsln  = zero
    qrnorm = beta1
    phibar = beta1
    rhs1   = beta1
    rhs2   = zero
    tnorm2 = zero
    ynorm2 = zero
    cs     = - one
    sn     = zero
    w(1:n) = zero
    w2(1:n)= zero
    r2(1:n)= r1

    if (debug) then
       write(*,*) ' '
       write(*,*) 'b    ', b
       write(*,*) 'beta ', beta
       write(*,*) ' '
    end if

    !===================================================================
    ! Main iteration loop.
    !===================================================================
    do
       itn = itn + 1               ! k = itn = 1 first time through

       !----------------------------------------------------------------
       ! Obtain quantities for the next Lanczos vector vk+1, k = 1, 2,...
       ! The general iteration is similar to the case k = 1 with v0 = 0:
       !
       !   p1      = Operator * v1  -  beta1 * v0,
       !   alpha1  = v1'p1,
       !   q2      = p2  -  alpha1 * v1,
       !   beta2^2 = q2'q2,
       !   v2      = (1/beta2) q2.
       !
       ! Again, y = betak P vk,  where  P = C**(-1).
       ! .... more description needed.
       !----------------------------------------------------------------
       s      = one / beta            ! Normalize previous vector (in y).
       v      = s*y(1:n)              ! v = vk if P = I

       call Aprod ( n, v, y )
       y      = y - shift*v           ! call daxpy ( n, (- shift), v, 1, y, 1 )
       if (itn >= 2) then
          y   = y - (beta/oldb)*r1    ! call daxpy ( n, (- beta/oldb), r1, 1, y, 1 )
       end if

       alfa   = dot_product(v,y)      ! alphak
       y      = y - (alfa/beta)*r2    ! call daxpy ( n, (- alfa/beta), r2, 1, y, 1 )
       r1     = r2
       r2     = y
       if ( precon ) call Msolve( n, r2, y )

       oldb   = beta                  ! oldb = betak
       beta   = dot_product(r2,y)     ! beta = betak+1^2
       if (beta < zero) then
          istop = 6
          go to 900
       end if

       beta   = sqrt( beta )          ! beta = betak+1
       tnorm2 = tnorm2 + alfa**2 + oldb**2 + beta**2

       if (itn == 1) then                   ! Initialize a few things.
          if (beta/beta1 <= ten*eps) then   ! beta2 = 0 or ~ 0.
             istop = -1                     ! Terminate later.
          end if
         !tnorm2 = alfa**2
          gmax   = abs( alfa )              ! alpha1
          gmin   = gmax                     ! alpha1
       end if

       ! Apply previous rotation Qk-1 to get
       !   [deltak epslnk+1] = [cs  sn][dbark    0   ]
       !   [gbar k dbar k+1]   [sn -cs][alfak betak+1].

       oldeps = epsln
       delta  = cs * dbar  +  sn * alfa ! delta1 = 0         deltak
       gbar   = sn * dbar  -  cs * alfa ! gbar 1 = alfa1     gbar k
       epsln  =               sn * beta ! epsln2 = 0         epslnk+1
       dbar   =            -  cs * beta ! dbar 2 = beta2     dbar k+1

       ! Compute the next plane rotation Qk

       gamma  = sqrt( gbar**2 + beta**2 )   ! gammak
       cs     = gbar / gamma                ! ck
       sn     = beta / gamma                ! sk
       phi    = cs * phibar                 ! phik
       phibar = sn * phibar                 ! phibark+1

       if (debug) then
          write(*,*) ' '
          write(*,*) 'v    ', v
          write(*,*) 'alfa ', alfa
          write(*,*) 'beta ', beta
          write(*,*) 'gamma', gamma
          write(*,*) 'delta', delta
          write(*,*) 'gbar ', gbar
          write(*,*) 'epsln', epsln
          write(*,*) 'dbar ', dbar
          write(*,*) 'phi  ', phi
          write(*,*) 'phiba', phibar
          write(*,*) ' '
       end if

       ! Update  x.

       denom = one/gamma

       do i = 1, n
          w1(i) = w2(i)
          w2(i) = w(i)
          w(i)  = ( v(i) - oldeps*w1(i) - delta*w2(i) ) * denom
          x(i)  =   x(i) +   phi * w(i)
       end do

       ! Go round again.

       gmax   = max( gmax, gamma )
       gmin   = min( gmin, gamma )
       z      = rhs1 / gamma
       ynorm2 = z**2  +  ynorm2
       rhs1   = rhs2  -  delta * z
       rhs2   =       -  epsln * z

       ! Estimate various norms and test for convergence.

       Anorm  = sqrt( tnorm2 )
       ynorm  = sqrt( ynorm2 )
       epsa   = Anorm * eps
       epsx   = Anorm * ynorm * eps
       epsr   = Anorm * ynorm * rtol
       diag   = gbar
       if (diag == zero) diag = epsa

       qrnorm = phibar
       rnorml = rnorm
       rnorm  = qrnorm
       rootl       = sqrt( gbar**2 +dbar**2  )  ! norm([gbar; dbar]);
       Arnorml     = rnorml*rootl               ! ||A r_{k-1} ||
       relArnorml  = rootl  /  Anorm;           ! ||Ar|| / (||A|| ||r||)     
       !relArnorml = Arnorml / Anorm;           ! ||Ar|| / ||A|| 

       ! Estimate  cond(A).
       ! In this version we look at the diagonals of  R  in the
       ! factorization of the lower Hessenberg matrix,  Q * H = R,
       ! where H is the tridiagonal matrix from Lanczos with one
       ! extra row, beta(k+1) e_k^T.

       Acond  = gmax / gmin

       ! See if any of the stopping criteria are satisfied.
       ! In rare cases, istop is already -1 from above (Abar = const*I).

       if (istop == 0) then
          if (itn    >= itnlim    ) istop = 5
          if (Acond  >= 0.1d+0/eps) istop = 4
          if (epsx   >= beta1     ) istop = 3
          ! original
          !if (qrnorm <= epsx  .or.  relArnorml <= epsx) istop = 2
          !if (qrnorm <= epsr  .or.  relArnorml <= epsr) istop = 1
          ! C. Kleinwort, DESY, 131002
          if (qrnorm <= epsx  .or.  relArnorml <= eps) istop = 2
          if (qrnorm <= epsr  .or.  relArnorml <= rtol) istop = 1
       end if


       ! See if it is time to print something.

       if (nout > 0) then
          prnt   = .false.
          if (n      <= 40         ) prnt = .true.
          if (itn    <= 10         ) prnt = .true.
          if (itn    >= itnlim - 10) prnt = .true.
          if (mod(itn,10)  ==     0) prnt = .true.
          if (qrnorm <=  ten * epsx) prnt = .true.
          if (qrnorm <=  ten * epsr) prnt = .true.
          if (relArnorml<= ten*epsx) prnt = .true.
          if (relArnorml<= ten*epsr) prnt = .true.
          if (Acond  >= 1.0d-2/eps ) prnt = .true.
          if (istop  /=  0         ) prnt = .true.

          if ( prnt ) then
             if (    itn     == 1) write(nout, 1200)
             write(nout, 1300) itn, x(1), qrnorm, Anorm, Acond
             if (mod(itn,10) == 0) write(nout, 1500)
          end if
       end if
       if (istop /= 0) exit

    end do
    !===================================================================
    ! End of iteration loop.
    !===================================================================

    ! Display final status.

900 Arnorm = Arnorml
    if (nout  > 0) then
       write(nout, 2000) exitt, istop, itn,   &
                         exitt, Anorm, Acond, &
                         exitt, rnorm, ynorm, Arnorm
       write(nout, 3000) exitt, msg(istop)
    end if

    return

 1000 format(// 1p,    a, 5x, 'Solution of symmetric   Ax = b'    &
              / ' n      =', i7, 5x, 'checkA =', l4, 12x,         &
                 'precon =', l4                                   &
              / ' itnlim =', i7, 5x, 'rtol   =', e11.2, 5x,       &
                 'shift  =', e23.14)
 1200 format(// 5x, 'itn', 8x, 'x(1)', 10x,                       &
                'norm(r)', 3x, 'norm(A)', 3X, 'cond(A)')
 1300 format(1p, i8, e19.10, 3e10.2)
 1500 format(1x)
 2000 format(/ 1p, a, 5x, 'istop =', i3,   14x, 'itn   =', i8     &
             /     a, 5x, 'Anorm =', e12.4, 5x, 'Acond =', e12.4  &
             /     a, 5x, 'rnorm =', e12.4, 5x, 'ynorm =', e12.4, 5x, 'Arnorml =', e12.4)
 3000 format(      a, 5x, a )

  end subroutine MINRES

end module minresModule
