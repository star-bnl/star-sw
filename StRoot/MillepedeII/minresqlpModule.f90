!> \file
!! MINRES-QLP algorithm.


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! File minresqlpModule.f90
!
!> MINRESQLP solves symmetric systems Ax = b or min ||Ax - b||_2,
!! where the matrix A may be indefinite and/or singular.
!! "A" is really (A - shift*I), where shift is an input real scalar.
!!
!! \verbatim
!! 09 Sep 2013: Version 27
!!-------------------------------------------------------------------
!!
!! The software for MINRES-QLP is provided by SOL, Stanford University
!! under the terms of the OSI Common Public License (CPL)
!!    http://www.opensource.org/licenses/cpl1.0.php
!! or the BSD License
!!    http://www.opensource.org/licenses/bsd-license.php
!!
!!-------------------------------------------------------------------
!!
!! Authors:
!!     Sou-Cheng Choi <sctchoi@uchicago.edu>
!!     Computation Institute (CI)
!!     University of Chicago
!!     Chicago, IL 60637, USA
!!
!!     Michael Saunders <saunders@stanford.edu>
!!     Systems Optimization Laboratory (SOL)
!!     Stanford University
!!     Stanford, CA 94305-4026, USA
!!
!! Contributor:
!!     Christopher Paige <paige@cs.mcgill.ca>
!!
!!   See also: Makefile
!! \endverbatim
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

module minresqlpModule

  use  minresqlpDataModule, only : dp, ip, one, zero, eps, realmin, prcsn, debug
  use  minresqlpBlasModule, only : dnrm2

  implicit none

  private                                       ! sets default for module
  public   :: MINRESQLP, SYMORTHO

contains

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !> Solution of linear equation system or least squares problem.
    !!
    !! \verbatim
    !!------------------------------------------------------------------
    !!
    !! MINRESQLP  is designed to solve the system of linear equations
    !!
    !!                Ax = b
    !!
    !!     or the least-squares problem
    !!
    !!         min || Ax - b ||_2,
    !!
    !!     where A is an n by n symmetric matrix and b is a given vector.
    !!     The matrix A may be indefinite and/or singular.
    !!
    !!     1. If A is known to be positive definite, the Conjugate Gradient
    !!        Method might be preferred, since it requires roughly the same
    !!        number of iterations as MINRESQLP but less work per iteration.
    !!        But if a low-accuracy solution is adequate, MINRESQLP will
    !!        terminate sooner.
    !!
    !!     2. If A is indefinite but Ax = b is known to have a solution
    !!        (e.g. if A is nonsingular), SYMMLQ might be preferred,
    !!        since it requires roughly the same number of iterations as
    !!        MINRESQLP but slightly less work per iteration.
    !!
    !!     3. If A is indefinite and well-conditioned, and Ax = b has a
    !!        solution, i.e., it is not a least-squares problem, MINRES might
    !!        be preferred as it requires the same number of iterations as
    !!        MINRESQLP but slightly less work per iteration.
    !!
    !!     The matrix A is intended to be large and sparse.  It is accessed
    !!     by means of a subroutine call of the form
    !!
    !!                call Aprod ( n, x, y )
    !!
    !!     which must return the product y = Ax for any given vector x.
    !!
    !!
    !!     More generally, MINRESQLP is designed to solve the system
    !!
    !!                (A - shift*I) x = b
    !!     or
    !!         min || (A - shift*I) x - b ||_2,
    !!
    !!     where shift is a specified real scalar.  Again, the matrix
    !!     (A - shift*I) may be indefinite and/or singular.
    !!     The work per iteration is very slightly less if shift = 0.
    !!
    !!     Note: If  shift  is an approximate eigenvalue of  A
    !!     and  b  is an approximate eigenvector,  x  might prove to be
    !!     a better approximate eigenvector, as in the methods of
    !!     inverse iteration and/or Rayleigh-quotient iteration.
    !!     However, we are not yet sure on that -- it may be better
    !!     to use SYMMLQ.
    !!
    !!     In this documentation, ' denotes the transpose of
    !!     a vector or a matrix.
    !!
    !!     A further option is that of preconditioning, which may reduce
    !!     the number of iterations required.  If M = C C' is a positive
    !!     definite matrix that is known to approximate  (A - shift*I)
    !!     in some sense, and if systems of the form  My = x  can be
    !!     solved efficiently, the parameter Msolve may be used (see below).
    !!     When an external procedure Msolve is supplied, MINRESQLP will
    !!     implicitly solve the system of equations
    !!
    !!             P (A - shift*I) P' xbar  =  P b,
    !!
    !!     i.e.                  Abar xbar  =  bbar
    !!     where                         P  =  C**(-1),
    !!                                Abar  =  P (A - shift*I) P',
    !!                                bbar  =  P b,
    !!
    !!     and return the solution       x  =  P' xbar.
    !!     The associated residual is rbar  =  bbar - Abar xbar
    !!                                      =  P (b - (A - shift*I)x)
    !!                                      =  P r.
    !!
    !!     In the discussion below, eps refers to the machine precision.
    !!     eps is computed by MINRESQLP.  A typical value is eps = 2.22d-16
    !!     for IEEE double-precision arithmetic.
    !!
    !!     Parameters
    !!     ----------
    !!     Some inputs are optional, with default values described below.
    !!     Mandatory inputs are n, Aprod, and b.
    !!     All outputs other than x are optional.
    !!
    !!     n       input      The dimension of the matrix or operator A.
    !!
    !!     b(n)    input      The rhs vector b.
    !!
    !!     x(n)    output     Returns the computed solution  x.
    !!
    !!     Aprod   external   A subroutine defining the matrix A.
    !!                        For a given vector x, the statement
    !!
    !!                              call Aprod ( n, x, y )
    !!
    !!                        must return the product y = Ax
    !!                        without altering the vector x.
    !!                        An extra call of Aprod is
    !!                        used to check if A is symmetric.
    !!
    !!     Msolve  external   An optional subroutine defining a
    !!                        preconditioning matrix M, which should
    !!                        approximate (A - shift*I) in some sense.
    !!                        M must be positive definite.
    !!                        For a given vector x, the statement
    !!
    !!                              call Msolve( n, x, y )
    !!
    !!                        must solve the linear system My = x
    !!                        without altering the vector x.
    !!
    !!                        In general, M should be chosen so that Abar has
    !!                        clustered eigenvalues.  For example,
    !!                        if A is positive definite, Abar would ideally
    !!                        be close to a multiple of I.
    !!                        If A or A - shift*I is indefinite, Abar might
    !!                        be close to a multiple of diag( I  -I ).
    !!
    !!     shift   input      Should be zero if the system Ax = b is to be
    !!                        solved.  Otherwise, it could be an
    !!                        approximation to an eigenvalue of A, such as
    !!                        the Rayleigh quotient b'Ab / (b'b)
    !!                        corresponding to the vector b.
    !!                        If b is sufficiently like an eigenvector
    !!                        corresponding to an eigenvalue near shift,
    !!                        then the computed x may have very large
    !!                        components.  When normalized, x may be
    !!                        closer to an eigenvector than b. Default to 0.
    !!
    !!     nout    input      A file number. The calling program must open a file
    !!                        for output using for example:
    !!                          open(nout, file='MINRESQLP.txt', status='unknown')
    !!                        If nout > 0, a summary of the iterations
    !!                        will be printed on unit nout. If nout is absent or
    !!                        the file associated with nout is not opened properly,
    !!                        results will be written to 'MINRESQLP_tmp.txt'.
    !!                        (Avoid 0, 5, 6 because by convention stderr=0,
    !!                        stdin=5, stdout=6.)
    !!
    !!     itnlim  input      An upper limit on the number of iterations. Default to 4n.
    !!
    !!     rtol    input      A user-specified tolerance.  MINRESQLP terminates
    !!                        if it appears that norm(rbar) is smaller than
    !!                              rtol*[norm(Abar)*norm(xbar) + norm(b)],
    !!                        where rbar = bbar - Abar xbar,
    !!                        or that norm(Abar*rbar) is smaller than
    !!                              rtol*norm(Abar)*norm(rbar).
    !!
    !!                        If shift = 0 and Msolve is absent, MINRESQLP
    !!                        terminates if norm(r) is smaller than
    !!                              rtol*[norm(A)*norm(x) + norm(b)],
    !!                        where r = b - Ax,
    !!                        or if norm(A*r) is smaller than
    !!                              rtol*norm(A)*norm(r).
    !!
    !!                        Default to machine precision.
    !!
    !!     istop   output     An integer giving the reason for termination...
    !!               0        Initial value of istop.
    !!
    !!               1        beta_{k+1} < eps.
    !!                        Iteration k is the final Lanczos step.
    !!
    !!               2        beta2 = 0 in the Lanczos iteration; i.e. the
    !!                        second Lanczos vector is zero.  This means the
    !!                        rhs is very special.
    !!
    !!                        If there is no preconditioner, b is an
    !!                        eigenvector of Abar. Also, x = (1/alpha1) b
    !!                        is a solution of Abar x = b.
    !!
    !!                        Otherwise (if Msolve is present), let My = b.
    !!                        If shift is zero, y is a solution of the
    !!                        generalized eigenvalue problem Ay = lambda My,
    !!                        with lambda = alpha1 from the Lanczos vectors.
    !!
    !!                        In general, (A - shift*I)x = b
    !!                        has the solution x = (1/alpha1) y
    !!                        where My = b.
    !!
    !!               3        b = 0, so the exact solution is x = 0.
    !!                        No iterations were performed.
    !!
    !!               4        Norm(rbar) appears to be less than
    !!                        the value  rtol * [ norm(Abar) * norm(xbar) + norm(b) ].
    !!                        The solution in  x  should be an acceptable
    !!                        solution of Abar x = b.
    !!
    !!               5        Norm(rbar) appears to be less than
    !!                        the value  eps * norm(Abar) * norm(xbar).
    !!                        This means that the solution is as accurate as
    !!                        seems reasonable on this machine.
    !!
    !!               6        Norm(Abar rbar) appears to be less than
    !!                        the value  rtol * norm(Abar) * norm(rbar).
    !!                        The solution in x should be an acceptable
    !!                        least-squares solution.
    !!
    !!               7        Norm(Abar rbar) appears to be less than
    !!                        the value  eps * norm(Abar) * norm(rbar).
    !!                        This means that the least-squares solution is as
    !!                        accurate as seems reasonable on this machine.
    !!
    !!               8        The iteration limit was reached before convergence.
    !!
    !!               9        The matrix defined by Aprod does not appear
    !!                        to be symmetric.
    !!                        For certain vectors y = Av and r = Ay, the
    !!                        products y'y and r'v differ significantly.
    !!
    !!               10       The matrix defined by Msolve does not appear
    !!                        to be symmetric.
    !!                        For vectors satisfying My = v and Mr = y, the
    !!                        products y'y and r'v differ significantly.
    !!
    !!               11       An inner product of the form  x' M**(-1) x
    !!                        was not positive, so the preconditioning matrix
    !!                        M does not appear to be positive definite.
    !!
    !!               12       xnorm has exceeded maxxnorm or will exceed it
    !!                        next iteration.
    !!
    !!               13       Acond (see below) has exceeded Acondlim or 0.1/eps,
    !!                        so the matrix Abar must be very ill-conditioned.
    !!
    !!               14       | gamma_k | < eps.
    !!                        This is very likely a least-squares problem but
    !!                        x may not contain an acceptable solution yet.
    !!
    !!               15       norm(Abar x) < rtol * norm(Abar) * norm(x).
    !!                        If disable = .true., then a null vector will be
    !!                        obtained, given rtol.
    !!
    !!                        If istop >= 7, the final x may not be an
    !!                        acceptable solution.
    !!
    !!     itn     output     The number of iterations performed.
    !!
    !!     Anorm   output     An estimate of the norm of the matrix operator
    !!                        Abar = P (A - shift*I) P',   where P = C**(-1).
    !!
    !!     Acond   output     An estimate of the condition of Abar above.
    !!                        This will usually be a substantial
    !!                        under-estimate of the true condition.
    !!
    !!     rnorm   output     An estimate of the norm of the final
    !!                        transformed residual vector,
    !!                           P (b  -  (A - shift*I) x).
    !!
    !!     xnorm   output     An estimate of the norm of xbar.
    !!                        This is sqrt( x'Mx ).  If Msolve is absent,
    !!                        xnorm is an estimate of norm(x).
    !!
    !!   maxxnorm  input      An upper bound on norm(x). Default value is 1e7.
    !!
    !!   trancond  input      If trancond > 1, a switch is made from MINRES
    !!                        iterations to MINRES-QLP iterations when
    !!                        Acond > trancond.
    !!                        If trancond = 1, all iterations are MINRES-QLP
    !!                        iterations.
    !!                        If trancond = Acondlim, all iterations are
    !!                        conventional MINRES iterations (which are
    !!                        slightly cheaper).
    !!                        Default to 1e7.
    !!
    !!   Acondlim  input      An upper bound on Acond. Default value is 1e15.
    !!
    !!   disable   input      All stopping conditions are disabled except
    !!                        norm(Ax) / norm(x) < tol. Default to .false..
    !!
    !!------------------------------------------------------------------
    !!
    !!     MINRESQLP is an implementation of the algorithm described in
    !!     the following references:
    !!
    !!     Sou-Cheng Choi,
    !!     Iterative Methods for Singular Linear Equations and Least-
    !!     Squares Problems, PhD dissertation, ICME, Stanford University,
    !!     2006.
    !!
    !!     Sou-Cheng Choi, Christopher Paige, and Michael Saunders,
    !!     MINRES-QLP: A Krylov subspace method for indefinite or
    !!     singular symmetric systems, SIAM Journal of Scientific
    !!     Computing 33:4 (2011) 1810-1836.
    !!
    !!     Sou-Cheng Choi and Michael Saunders,
    !!     ALGORITHM & DOCUMENTATION: MINRES-QLP for singular Symmetric and Hermitian
    !!     linear equations and least-squares problems, Technical Report,
    !!     ANL/MCS-P3027-0812, Computation Institute,
    !!     University of Chicago/Argonne National Laboratory, 2012.
    !!
    !!     Sou-Cheng Choi and Michael Saunders,
    !!     ALGORITHM xxx: MINRES-QLP for singular Symmetric and Hermitian
    !!     linear equations and least-squares problems,
    !!     ACM Transactions on Mathematical Software, to appear, 2013.
    !!
    !!     FORTRAN 90 and MATLAB implementations are
    !!     downloadable from
    !!            http://www.stanford.edu/group/SOL/software.html
    !!            http://home.uchicago.edu/sctchoi/
    !!
    !!------------------------------------------------------------------
    !!
    !!     MINRESQLP development:
    !!     14 Dec 2006: Sou-Cheng's thesis completed.
    !!                  MINRESQLP includes a stopping rule for singular
    !!                  systems (using an estimate of ||Ar||) and very many
    !!                  other things(!).
    !!                  Note that ||Ar|| small => r is a null vector for A.
    !!     09 Oct 2007: F90 version constructed from the F77 version.
    !!                  Initially used compiler option -r8, but this is
    !!                  nonstandard.
    !!     15 Oct 2007: Test on Arnorm = ||Ar|| added to recognize
    !!                  singular systems.
    !!     15 Oct 2007: Temporarily used real(8) everywhere.
    !!     16 Oct 2007: Use minresqlpDataModule to define
    !!                  dp = selected_real_kind(15).
    !!                  We need "use minresqlpDataModule" at the
    !!                  beginning of modules AND inside interfaces.
    !!     06 Jun 2010: Added comments.
    !!     12 Jul 2011: Created complex version zminresqlpModule.f90
    !!                  from real version minresqlpModule.f90.
    !!     23 Aug 2011: (1) Tim Hopkins ran version 17 on the NAG Fortran compiler
    !!                  We removed half a dozen unused variables in MINRESQLP
    !!                  and also local var sgn_a and sgn_b in SMMORTHO,
    !!                  as they result in division by zero for inputs a=b=0.
    !!                  (2) Version 18 was submitted to ACM TOMS for review.
    !!     20 Aug 2012: Version 19:
    !!                  (1) Added optional inputs and outputs, and
    !!                  default values for optional inputs.
    !!                  (2) Removed inputs 'checkA' and 'precon'.
    !!                  (3) Changed slightly the order of parameters in the
    !!                  MINRESQLP API.
    !!                  (4) Updated documentation.
    !!                  (5) Fixed a minor bug in printing x(1) in iteration
    !!                      log during MINRES mode.
    !!                  (6) Made sure MINRESQLP is portable in both single
    !!                      and double precison.
    !!                  (7) Fixed a bug to ensure the 2x2 Hermitian reflectors
    !!                      are orthonormal. Make output c real.
    !!     24 Apr 2013: istop = 12 now means xnorm just exceeded maxxnorm.
    !!     28 Jun 2013: likeLS introduced to terminate with big xnorm
    !!                  only if the problem seems to be singular and inconsistent.
    !!     08 Jul 2013: (1) dot_product replaces ddotc.
    !!     04 Aug 2013: If present(maxxnorm), use maxxnorm_ = min(maxxnorm, one/eps).
    !!     09 Sep 2013: Initialize relresl and relAresl to zero.
    !!------------------------------------------------------------------
    !! \endverbatim

  subroutine MINRESQLP( n, Aprod, b, shift, Msolve, disable, nout,        &
                        itnlim, rtol, maxxnorm, trancond, Acondlim,       &
                        x, istop, itn, rnorm, Arnorm, xnorm, Anorm, Acond )
    ! Inputs
    integer(ip), intent(in)             :: n
    real(dp),    intent(in)             :: b(n)
    integer(ip), intent(in), optional   :: itnlim, nout
    logical,     intent(in), optional   :: disable
    real(dp),    intent(in), optional   :: shift
    real(dp),    intent(in), optional   :: rtol, maxxnorm, trancond, Acondlim

    ! Outputs
    real(dp),    intent(out)            :: x(n)
    integer(ip), intent(out), optional  :: istop, itn
    real(dp),    intent(out), optional  :: rnorm, Arnorm, xnorm, Anorm, Acond

    optional :: Msolve

    interface
       subroutine Aprod(n,x,y)                    ! y := A*x
         use minresqlpDataModule
         integer(ip), intent(in)    :: n
         real(dp),    intent(in)    :: x(n)
         real(dp),    intent(out)   :: y(n)
       end subroutine Aprod

       subroutine Msolve(n,x,y)                   ! Solve M*y = x
         use minresqlpDataModule
         integer(ip), intent(in)    :: n
         real(dp),    intent(in)    :: x(n)
         real(dp),    intent(out)   :: y(n)
       end subroutine Msolve
    end interface

    intrinsic :: abs, sqrt, present, floor, log10, dot_product

    ! Local arrays and variables
    real(dp)     :: shift_
    real(dp)     :: rtol_, maxxnorm_, trancond_, Acondlim_
    real(dp)     :: rnorm_, Arnorm_, xnorm_, Anorm_, Acond_
    logical      :: checkA_, precon_, disable_
    integer(ip)  :: itnlim_, nout_, istop_, itn_

    real(dp)     :: r1(n), r2(n), v(n), w(n), wl(n), wl2(n),&
                    xl2(n), y(n), vec2(2), vec3(3)
    real(dp)     :: abs_gama, Acondl, alfa, Anorml, Arnorml,&
                    Axnorm, beta, beta1, betal, betan,      &
                    epsa, epsx, gminl2, ieps, pnorm,        &
                    relAres, relAresl, relres, relresl,     &
                    rnorml, rootl, t1, t2, xl2norm,         &
                    xnorm_tmp, xnorml, z,                   &
                    cr1, cr2, cs, dbar, dlta, dlta_QLP,     &
                    dlta_tmp, dltan, epln, eplnn, eta, etal,&
                    etal2, gama, gama_QLP, gama_tmp, gamal, &
                    gamal_QLP, gamal_tmp, gamal2, gamal3,   &
                    gbar, gmin, gminl, phi, s, sn, sr1, sr2,&
                    t, tau, taul, taul2, u, u_QLP,          &
                    ul, ul_QLP, ul2, ul2_QLP, ul3, ul4,     &
                    vepln, vepln_QLP, veplnl, veplnl2, x1last

    integer(ip)  :: j, QLPiter, headlines, lines, nprint, flag0, ios
    logical      :: prnt, done, lastiter, connected, named_file, likeLS
    character(len=20) :: filename
    character(len=2)  :: QLPstr = ' '

    ! Local constants
    real(dp), parameter :: EPSINV  = 10.0_dp**floor(log10(one/eps))
    real(dp)            :: NORMMAX = 10.0_dp**floor(log10(one/eps)/2)
    character(len=*), parameter :: enter = ' Enter MINRES-QLP.  '
    character(len=*), parameter :: exitt = ' Exit  MINRES-QLP.  '
    character(len=*), parameter :: msg(1:15) =                                &
      (/ 'beta_{k+1} < eps.                                                ', & ! 1
         'beta2 = 0.  If M = I, b and x are eigenvectors of A.             ', & ! 2
         'beta1 = 0.  The exact solution is  x = 0.                        ', & ! 3
         'A solution to (poss. singular) Ax = b found, given rtol.         ', & ! 4
         'A solution to (poss. singular) Ax = b found, given eps.          ', & ! 5
         'Pseudoinverse solution for singular LS problem, given rtol.      ', & ! 6
         'Pseudoinverse solution for singular LS problem, given eps.       ', & ! 7
         'The iteration limit was reached.                                 ', & ! 8
         'The operator defined by Aprod appears to be unsymmetric.         ', & ! 9
         'The operator defined by Msolve appears to be unsymmetric.        ', & ! 10
         'The operator defined by Msolve appears to be indefinite.         ', & ! 11
         'xnorm has exceeded maxxnorm or will exceed it next iteration.    ', & ! 12
         'Acond has exceeded Acondlim or 0.1/eps.                          ', & ! 13
         'Least-squares problem but no converged solution yet.             ', & ! 14
         'A null vector obtained, given rtol.                              ' /) ! 15

     character(len=*), parameter :: ddebugStr1 = "(a, T5, i0, a, 5(e12.3))"
     character(len=*), parameter :: ddebugStr2 = "(5(a, i0, a, e12.3, a))"

     character(len=*), parameter :: headerStr =                    &
        "(// 1p,    a, 4x, 'Solution of symmetric   Ax = b'"    // &
        "  / ' n        =', i7, 6x,  '||b||    =', e11.2, 3x,"  // &
        "     'precon   =', l4                           "      // &
        "  / ' itnlim   =', i7, 6x, 'rtol     =', e11.2, 3x,"   // &
        "     'shift    =', e23.14                       "      // &
        "  / ' maxxnorm =', e11.2, 2x, 'Acondlim =', e11.2, 3x,"// &
        "     'trancond =', e11.2)"
     character(len=*), parameter :: tableHeaderStr =                         &
      "(// '    iter   x(1)              xnorm     rnorm     Arnorm   '," // &
      " 'Compatible   LS      norm(A)   cond(A)')"
     character(len=*), parameter :: itnStr = "(1p, i8, e19.10, 7e10.2, a)"
     character(len=*), parameter :: finalStr1 =                    &
        "(/ 1p, a, 5x, a, i3,   14x, a, i8 "                    // &
        " /     a, 5x, a, e12.4, 5x, a, e12.4 "                 // &
        " /     a, 5x, a, e12.4, 5x, a, e12.4 "                 // &
        " /     a, 5x, a, e12.4 )"
     character(len=*), parameter :: finalStr2 = "(      a, 5x, a )"

    !------------------------------------------------------------------
    prnt = .FALSE.
    t1 = zero
    t2 = zero
    gminl = zero
    ! Optional inputs
    if (present(shift)) then
       shift_ = shift
    else
       shift_ = zero
    end if

    checkA_ = .true.

    if (present(disable)) then
       disable_ = disable
    else
       disable_ = .false.
    end if

    if (present(itnlim)) then
       itnlim_ = itnlim
    else
       itnlim_ = 4 * n
    end if

    connected = .false.
    filename = "MINRESQLP_tmp.txt"
    nout_ = 10

    if (present(nout)) then
       nout_ = nout
       inquire(unit=nout, opened=connected, named=named_file, name=filename)
       !write(*,*) connected, named_file, filename
       if (.not. connected) then
          write(*,*) "File unit 'nout' is not open."
          if (nout==5  .or.  nout == 6) then
             nout_ = 10
          end if
       end if
    end if

    if (.not. connected) then
        write(*,*) 'nout_ = ', nout_
        open(nout_, file=filename, status='unknown', iostat=ios)
        write(*,*) 'ios = ', ios
        if (ios /= 0) then
           write(*,*) "Error opening file '", filename, "'."
           STOP
        end if
    end if

    if (present(rtol)) then
       rtol_ = rtol
    else
       rtol_ = eps
    end if

    if (prcsn == 6) then
       NORMMAX = 1.0e4_dp
    end if
    if (present(maxxnorm)) then
       maxxnorm_ = min(maxxnorm, one/eps)
    else
       maxxnorm_ = NORMMAX
    end if

    if (present(trancond)) then
       trancond_ = min(trancond, NORMMAX)
    else
       trancond_ = NORMMAX
    end if

    if (present(Acondlim)) then
       Acondlim_ = min(Acondlim, EPSINV)
    else
       Acondlim_ = EPSINV
    end if

    if (present(Msolve)) then
       precon_ = .true.
    else
       precon_ = .false.
    end if

    !------------------------------------------------------------------
    ! Print heading and initialize.
    !------------------------------------------------------------------
    nprint   = min(n,20)
    !debug   = .true.
    lastiter = .false.
    flag0    = 0
    istop_   = flag0
    beta1    = dnrm2(n, b, 1)
    ieps     = 0.1_dp/eps
    itn_     = 0
    QLPiter  = 0
    xnorm_   = zero
    xl2norm  = zero
    Axnorm   = zero
    Anorm_   = zero
    Acond_   = one
    pnorm    = zero
    relresl  = zero
    relAresl = zero
    x        = zero
    xl2      = zero
    x1last   = x(1)

    if (nout_ > 0) then
       write(nout_, headerStr) enter, n, beta1, precon_, itnlim_, rtol_, &
       shift_, maxxnorm_, Acondlim_, trancond_
    end if

    !------------------------------------------------------------------
    ! Set up y and v for the first Lanczos vector v1.
    ! y  =  beta1 P'v1,  where  P = C**(-1).
    ! v is really P'v1.
    !------------------------------------------------------------------
    y  = b
    r1 = b
    if ( precon_ ) then
       call Msolve( n, b, y )
    end if

    beta1  = dot_product(b, y)

    if (beta1 < zero .and. dnrm2(n, y, 1) > eps) then     ! M must be indefinite.
       istop_ = 11
    end if

    if (beta1 == zero) then    ! b = 0 exactly.  Stop with x = 0.
       istop_ = 3
    end if

    beta1  = sqrt( beta1 )     ! Normalize y to get v1 later.

    if (debug) then
       write(*,ddebugStr1) ' y_', itn_, ' = ', (y(j), j=1,nprint)
       write(*,*) 'beta1 ', beta1
    end if

    !------------------------------------------------------------------
    ! See if Msolve is symmetric.
    !------------------------------------------------------------------
    if (checkA_  .and.  precon_) then
       call Msolve( n, y, r2 )
       s      = dot_product( y, y )
       t      = dot_product(r1, r2)
       z      = abs( s - t )
       epsa   = (abs(s) + eps) * eps**0.33333_dp
       if (z > epsa) then
          istop_ = 10
       end if
    end if

    !------------------------------------------------------------------
    ! See if Aprod  is symmetric.
    !------------------------------------------------------------------
    if (checkA_) then
       call Aprod ( n, y, w  )  ! w  = A*y
       call Aprod ( n, w, r2 )  ! r2 = A*w
       s      = dot_product( w, w )
       t      = dot_product( y, r2)
       z      = abs( s - t )
       epsa   = (abs(s) + eps) * eps**0.33333_dp
       if (z > epsa) then
          istop_ = 9
       end if
    end if

    !------------------------------------------------------------------
    ! Initialize other quantities.
    !------------------------------------------------------------------
    tau    = zero
    taul   = zero
    gmin   = zero
    beta   = zero
    betan  = beta1
    dbar   = zero
    dltan  = zero
    eta    = zero
    etal   = zero
    etal2  = zero
    vepln  = zero
    veplnl = zero
    veplnl2= zero
    eplnn  = zero
    gama   = zero
    gamal  = zero
    gamal2 = zero
    phi    = beta1
    cr1    = -one
    sr1    = zero
    cr2    = -one
    sr2    = zero
    cs     = -one
    sn     = zero
    ul3    = zero
    ul2    = zero
    ul     = zero
    u      = zero
    lines  = 1
    headlines = 30 * lines
    rnorm_ = betan
    relres = rnorm_ / (Anorm_*xnorm_ + beta1)
    relAres= zero
    r2     = b
    w      = zero    ! vector of zeros
    wl     = zero
    done   = .false.

    if (debug) then
       write(*,*)
       write(*,*) 'Checking variable values before main loop'
       write(*,*) 'istop ', istop_, ' done ', done, ' itn ', itn_, ' QLPiter' , QLPiter
       write(*,*) 'lastiter', lastiter, ' lines ', lines, ' headlines ', headlines
       write(*,*) 'beta ', beta, ' tau ', tau, ' taul ', taul, ' phi ', phi
       write(*,*) 'betan ', betan, ' gmin ', gmin, ' cs ', cs, ' sn ', sn
       write(*,*) 'cr1 ', cr1, ' sr1 ', sr1, ' cr2 ', cr2, ' sr2 ', sr2
       write(*,*) 'dltan ', dltan, ' eplnn ', eplnn, ' gama ', gama, ' gamal ', gamal
       write(*,*) 'gamal2 ', gamal2, ' eta ', eta, ' etal ', etal, 'etal2', etal2
       write(*,*) 'vepln ', vepln, ' veplnl', veplnl, ' veplnl2 ', veplnl2, ' ul3 ', ul3
       write(*,*) 'ul2 ', ul2, ' ul ', ul, ' u ', u, ' rnorm ', rnorm_
       write(*,*) 'xnorm ', xnorm_, ' xl2norm ', xl2norm, ' pnorm ', pnorm, ' Axnorm ', Axnorm
       write(*,*) 'Anorm ', Anorm_, ' Acond ', Acond_, ' relres ', relres
       write(*,ddebugStr1) 'w_', itn_-1, ' = ', (wl(j), j=1,nprint)
       write(*,ddebugStr1) 'w_', itn_,   ' = ', (w(j),  j=1,nprint)
       write(*,ddebugStr1) 'x_', itn_,   ' = ', (x(j),  j=1,nprint)
    end if


    !------------------------------------------------------------------
    ! Main iteration loop.
    !------------------------------------------------------------------
    do while (istop_ <= flag0)
       itn_ = itn_ + 1               ! k = itn = 1 first time through

       !-----------------------------------------------------------------
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
       !-----------------------------------------------------------------

       betal  = beta;               ! betal = betak
       beta   = betan;
       s      = one / beta          ! Normalize previous vector (in y).
       v      = s*y;                ! v = vk if P = I.
       call Aprod ( n, v, y )
       if (shift_ /= zero) then
          y = y - shift_ * v
       end if
       if (itn_ >= 2) then
          y = y + (- beta/betal) * r1
       end if
       alfa = dot_product(v, y)                  ! alphak
       y    = y + (- alfa/beta) * r2
       r1   = r2
       r2   = y

       if ( .not. precon_ ) then
          betan = dnrm2(n, y, 1)                 ! betan = ||y||_2
       else
          call Msolve( n, r2, y )
          betan = dot_product(r2, y)             ! betan = betak+1^2
          if (betan > zero) then
             betan = sqrt(betan)
          elseif ( dnrm2(n, y, 1) > eps ) then   ! M must be indefinite.
             istop_ = 11
             exit
          end if
       end if

       if (itn_ == 1) then
          vec2(1) = alfa
          vec2(2) = betan
          pnorm   = dnrm2(2, vec2, 1)
       else
          vec3(1) = beta
          vec3(2) = alfa
          vec3(3) = betan
          pnorm   = dnrm2(3, vec3, 1)
       end if


       if (debug) then
          write(*,*)
          write(*,*) 'Lanczos iteration ', itn_
          write(*,ddebugStr1) 'v_',  itn_, ' = ', (v(j),  j=1,nprint)
          write(*,ddebugStr1) 'r1_', itn_, ' = ', (r1(j), j=1,nprint)
          write(*,ddebugStr1) 'r2_', itn_, ' = ', (r2(j), j=1,nprint)
          write(*,ddebugStr1) 'y_',  itn_, ' = ', (y(j),  j=1,nprint)
          write(*,ddebugStr2) 'alpha_', itn_,   ' = ', alfa,  ', ', &
                              'beta_',  itn_,   ' = ', beta,  ', ', &
                              'beta_',  itn_+1, ' = ', betan, ', ', &
                              'pnorm_', itn_,   ' = ', pnorm
       end if

       ! Apply previous left reflection Qk-1 to get
       !   [deltak epslnk+1] = [cs  sn][dbark    0   ]
       !   [gbar k dbar k+1]   [sn -cs][alfak betak+1].

       dbar   = dltan
       dlta   = cs * dbar  +  sn * alfa  ! dlta1  = 0         deltak
       epln   = eplnn;
       gbar   = sn * dbar  -  cs * alfa  ! gbar 1 = alfa1     gbar k
       eplnn  =               sn * betan ! eplnn2 = 0         epslnk+1
       dltan  =            -  cs * betan ! dbar 2 = beta2     dbar k+1
       dlta_QLP = dlta;

       if (debug) then
          write(*,*)
          write(*,*) 'Apply previous left reflection Q_{', itn_-1, ',', itn_, '}'
          write(*,ddebugStr2) 'c_',    itn_-1, ' = ', cs,    ', ', &
                              's_',    itn_-1, ' = ', sn
          write(*,ddebugStr2) 'dlta_', itn_,   ' = ', dlta,  ', ', &
                              'gbar_', itn_,   ' = ', gbar,  ', ', &
                              'epln_', itn_+1, ' = ', eplnn, ', ', &
                              'dbar_', itn_+1, ' = ', dltan
       end if

       ! Compute the current left reflection Qk
       gamal3 = gamal2
       gamal2 = gamal
       gamal  = gama
       call symortho(gbar, betan, cs, sn, gama)
       gama_tmp = gama;
       taul2  = taul
       taul   = tau
       tau    = cs * phi
       phi    = sn * phi                     ! phik
       Axnorm = sqrt( Axnorm**2 + tau**2 );

       if (debug) then
          write(*,*)
          write(*,*) 'Compute the current left reflection Q_{', itn_, ',', itn_+1, '}'
          write(*,ddebugStr2) 'c_',    itn_, ' = ', cs,   ', ', &
                              's_',    itn_, ' = ', sn
          write(*,ddebugStr2) 'tau_',  itn_, ' = ', tau,  ', ', &
                              'phi_',  itn_, ' = ', phi,  ', ', &
                              'gama_', itn_, ' = ', gama
       end if

       ! Apply the previous right reflection P{k-2,k}

       if (itn_ > 2) then
          veplnl2  = veplnl
          etal2    = etal
          etal     = eta
          dlta_tmp = sr2 * vepln - cr2 * dlta
          veplnl   = cr2 * vepln + sr2 * dlta
          dlta     = dlta_tmp
          eta      = sr2 * gama
          gama     = -cr2 * gama
          if (debug) then
             write(*,*)
             write(*,*) 'Apply the previous right reflection P_{', itn_-2, ',', itn_, '}'
             write(*,ddebugStr2) 'cr2_',    itn_,   ' = ', cr2,    ', ', &
                                 'sr2_',    itn_,   ' = ', sr2
             write(*,ddebugStr2) 'gamal2_', itn_,   ' = ', gamal2, ', ', &
                                 'gamal_',  itn_,   ' = ', gamal,  ', ', &
                                 'gama_',   itn_,   ' = ', gama
             write(*,ddebugStr2) 'dlta_',   itn_,   ' = ', dlta,   ', ', &
                                 'vepln_',  itn_-1, ' = ', veplnl
          end if
       end if


       ! Compute the current right reflection P{k-1,k}, P_12, P_23,...
       if (itn_ > 1) then
          call symortho(gamal, dlta, cr1, sr1, gamal_tmp)
          gamal     = gamal_tmp
          vepln     = sr1 * gama
          gama      = -cr1 * gama

          if (debug) then
             write(*,*)
             write(*,*) 'Apply the current right reflection  P_{', itn_-1, ',', itn_, '}'
             write(*,ddebugStr2) 'cr1_ ',  itn_,   ' = ', cr1,    ', ', &
                                 'sr1_' ,  itn_,   ' = ', sr1
             write(*,ddebugStr2) 'gama_',  itn_-2, ' = ', gamal2, ', ', &
                                 'gama_',  itn_-1, ' = ', gamal,  ', ', &
                                 'gama_',  itn_,   ' = ', gama
             write(*,ddebugStr2) 'dlta_',  itn_,   ' = ', dlta,   ', ', &
                                 'vepln_', itn_-1, ' = ', veplnl, ', ', &
                                 'eta_',   itn_,   ' = ', eta
          end if
       end if

       ! Update xnorm

       xnorml  = xnorm_
       ul4     = ul3
       ul3     = ul2

       if (itn_ > 2) then
          ul2 = ( taul2 - etal2 * ul4 - veplnl2 * ul3 ) / gamal2
          if (debug) then
             write(*,ddebugStr2) 'tau_',   itn_-2, ' = ', taul2,   ', ', &
                                 'eta_',   itn_-2, ' = ', etal2,   ', ', &
                                 'vepln_', itn_-2, ' = ', veplnl2, ', ', &
                                 'gama_',  itn_-2, ' = ', gamal2
          end if
       end if
       if (itn_ > 1) then
          ul  = ( taul  - etal  * ul3 - veplnl  * ul2) / gamal
          if (debug) then
             write(*,ddebugStr2) 'tau_',   itn_-1, ' = ', taul,   ', ', &
                                 'eta_',   itn_-1, ' = ', etal,   ', ', &
                                 'vepln_', itn_-1, ' = ', veplnl, ', ', &
                                 'gamal_', itn_-1, ' = ', gamal
          end if
       end if

       vec3(1) = xl2norm
       vec3(2) = ul2
       vec3(3) = ul
       xnorm_tmp = dnrm2(3, vec3, 1)     ! norm([xl2norm ul2 ul]);
       if (abs(gama) > eps) then         ! .and.  xnorm_tmp < maxxnorm_) then
          if (debug) then
             write(*,ddebugStr2) 'tau_',   itn_, ' = ', tau,   ', ', &
                                 'eta_',   itn_, ' = ', eta,   ', ', &
                                 'vepln_', itn_, ' = ', vepln, ', ', &
                                 'gama_',  itn_, ' = ', gama
          end if
          u       = (tau - eta*ul2 - vepln*ul) / gama
          likeLS  = relAresl < relresl
          vec2(1) = xnorm_tmp
          vec2(2) = u
          if (likeLS  .and.  dnrm2(2, vec2, 1) > maxxnorm_) then
             u      = zero
             istop_ = 12
          end if
       else
          u      = zero
          istop_ = 14
       end if
       vec2(1)   = xl2norm
       vec2(2)   = ul2
       xl2norm   = dnrm2(2, vec2, 1)
       vec3(1)   = xl2norm
       vec3(2)   = ul
       vec3(3)   = u
       xnorm_    = dnrm2(3, vec3, 1)

       if (Acond_ < trancond_  .and.  istop_ == flag0  .and.  QLPiter == 0) then !! MINRES updates
          wl2  = wl
          wl   = w
          if (gama_tmp > eps) then
             s = one / gama_tmp
             w = (v - epln*wl2 - dlta_QLP*wl) * s
          end if

          if (xnorm_ < maxxnorm_) then
             x1last = x(1)
             x = x + tau*w
          else
             istop_   = 12
             lastiter = .true.
          end if

          if (debug) then
             write(*,*)
             write(*,*) 'MINRES updates'
             write(*,ddebugStr2) 'gama_tmp_', itn_, ' = ', gama_tmp, ', ', &
                                 'tau_',      itn_, ' = ', tau,      ', ', &
                                 'epln_',     itn_, ' = ', epln,     ', ', &
                                 'dlta_QLP_', itn_, ' = ', dlta_QLP
             write(*,ddebugStr1) 'v_', itn_ , ' = ', (v(j), j=1,nprint)
             write(*,ddebugStr1) 'w_', itn_ , ' = ', (w(j), j=1,nprint)
          end if
       else                                         !! MINRES-QLP updates
          QLPiter = QLPiter + 1;
          if (QLPiter == 1) then
             xl2 = zero     ! vector
             if (itn_ > 1) then                     ! construct w_{k-3}, w_{k-2}, w_{k-1}
                if (itn_ > 3) then
                   wl2 = gamal3*wl2 + veplnl2*wl + etal*w
                end if                              ! w_{k-3}
                if (itn_ > 2) then
                   wl = gamal_QLP*wl + vepln_QLP*w
                end if                              ! w_{k-2}
                w = gama_QLP*w
                xl2 =  x - ul_QLP*wl - u_QLP*w
             end if
          end if

          if (itn_ == 1) then
             wl2 =  wl
             wl  =  sr1*v
             w   = -cr1*v
          else if (itn_ == 2) then
             wl2 = wl
             wl  = cr1*w + sr1*v
             w   = sr1*w - cr1*v
          else
             wl2 = wl
             wl  = w
             w   = sr2*wl2 - cr2*v
             wl2 = cr2*wl2 + sr2*v
             v   = cr1*wl  + sr1*w
             w   = sr1*wl  - cr1*w
             wl  = v
          end if
          x1last = x(1)
          xl2 = xl2 + ul2*wl2
          x   = xl2 + ul *wl + u*w

          if (debug) then
             write(*,*)
             write(*,*) 'MINRESQLP updates'
          end if
       end if

       if (debug) then
          write(*,*)
          write(*,*) 'Update u, w, x and xnorm'
          write(*,ddebugStr2) 'u_', itn_-2, ' = ', ul2, ', ', &
                              'u_', itn_-1, ' = ', ul,  ', ', &
                              'u_', itn_,   ' = ', u
          write(*,ddebugStr1) 'w_',   itn_-2, ' = ',  (wl2(j), j=1,nprint)
          write(*,ddebugStr1) 'w_',   itn_-1, ' = ',  (wl(j),  j=1,nprint)
          write(*,ddebugStr1) 'w_',   itn_,   ' = ',  (w(j),   j=1,nprint)
          write(*,ddebugStr1) 'x_',   itn_,   ' = ',  (x(j),   j=1,nprint)
          write(*,ddebugStr2) 'xnorm_', itn_-2, ' = ', xl2norm, ', ', &
                              'xnorm_', itn_-1, ' = ', xnorml,  ', ', &
                              'xnorm_', itn_,   ' = ', xnorm_
       end if

       ! Compute the next right reflection P{k-1,k+1}

       if (debug) then
          write(*,*)
          write(*,*) 'Compute the next right reflection P{', itn_-1, itn_+1,'}'
          write(*,ddebugStr2) 'gama_', itn_-1, ' = ', gamal, ', ', &
                              'epln_', itn_+1, ' = ', eplnn
       end if

       gamal_tmp = gamal
       call symortho(gamal_tmp, eplnn, cr2, sr2, gamal)

       if (debug) then
          write(*,ddebugStr2) 'cr2_',  itn_+1, ' = ', cr2, ', ', &
                              'sr2_',  itn_+1, ' = ', sr2, ', ', &
                              'gama_', itn_-1, ' = ', gamal
       end if

       ! Store quantities for transfering from MINRES to MINRES-QLP

       gamal_QLP = gamal_tmp
       vepln_QLP = vepln
       gama_QLP  = gama
       ul2_QLP   = ul2
       ul_QLP    = ul
       u_QLP     = u

       if (debug) then
          write(*,*)
          write(*,*) 'Store quantities for transfering from MINRES to MINRES-QLP '
          write(*,ddebugStr2) 'gama_QLP_', itn_-1, ' = ', gamal_QLP, ', ', &
                              'vepln_QLP_',itn_,   ' = ', vepln_QLP, ', ', &
                              'gama_QLP_', itn_,   ' = ', gama_QLP
          write(*,ddebugStr2) 'u_QLP_', itn_-2,    ' = ', ul2_QLP,   ', ', &
                              'u_QLP_', itn_-1,    ' = ', ul_QLP,    ', ', &
                              'u_QLP_', itn_,      ' = ', u_QLP
       end if

       ! Estimate various norms

       abs_gama = abs(gama)
       Anorml   = Anorm_
       Anorm_   = max(Anorm_, pnorm, gamal, abs_gama)
       if (itn_ == 1) then
          gmin  = gama
          gminl = gmin
       else if (itn_ > 1) then
          gminl2  = gminl
          gminl   = gmin
          vec3(1) = gminl2
          vec3(2) = gamal
          vec3(3) = abs_gama
          gmin    = min(gminl2, gamal, abs_gama)
       end if
       Acondl   = Acond_
       Acond_   = Anorm_ / gmin
       rnorml   = rnorm_
       relresl  = relres
       if (istop_ /= 14) rnorm_ = phi
       relres   = rnorm_ / (Anorm_ * xnorm_ + beta1)
       vec2(1)  = gbar
       vec2(2)  = dltan
       rootl    = dnrm2(2, vec2, 1)
       Arnorml  = rnorml * rootl
       relAresl = rootl / Anorm_

       if (debug) then
          write(*,*)
          write(*,*) 'Estimate various norms '
          write(*,ddebugStr2) 'gmin_',   itn_,   ' = ', gmin,     ', ', &
                              'pnorm_',  itn_,   ' = ', pnorm,    ', ', &
                              'rnorm_',  itn_,   ' = ', rnorm_,   ', ', &
                              'Arnorm_', itn_-1, ' = ', Arnorml
          write(*,ddebugStr2) 'Axnorm_', itn_,   ' = ', Axnorm,   ', ', &
                              'Anorm_',  itn_,   ' = ', Anorm_,   ', ', &
                              'Acond_',  itn_,   ' = ', Acond_
       end if

       ! See if any of the stopping criteria are satisfied.

       epsx = Anorm_*xnorm_*eps
       if (istop_ == flag0  .or.  istop_ == 14) then
          t1   = one + relres
          t2   = one + relAresl
       end if
       if (t1 <= one                  ) then
          istop_ = 5                           ! Accurate Ax=b solution
       else if (t2 <= one             ) then
          istop_ = 7                           ! Accurate LS solution
       else if (relres   <= rtol_     ) then
          istop_ = 4                           ! Good enough Ax=b solution
       else if (relAresl <= rtol_     ) then
          istop_ = 6                           ! Good enough LS solution
       else if (epsx     >= beta1     ) then
          istop_ = 2                           ! x is an eigenvector
       else if (xnorm_    >= maxxnorm_) then
          istop_ = 12                          ! xnorm exceeded its limit
       else if (Acond_    >= Acondlim_ .or. Acond_ >= ieps) then
          istop_ = 13                          ! Huge Acond
       else if (itn_      >= itnlim_  ) then
          istop_ = 8                           ! Too many itns
       else if (betan     <  eps      ) then
          istop_ = 1                           ! Last iteration of Lanczos, rarely happens
       end if

       if (disable_  .and.  itn_ < itnlim_) then
          istop_ = flag0
          done   = .false.
          if (Axnorm < rtol_*Anorm_*xnorm_) then
             istop_   = 15
             lastiter = .false.
          end if
       end if

       if (istop_ /= flag0) then
          done = .true.
          if (istop_ == 6  .or.  istop_ == 7  .or.  istop_ == 12  .or.  istop_ == 13) then
             lastiter = .true.
          end if
          if (lastiter) then
             itn_   = itn_ - 1
             Acond_ = Acondl
             rnorm_ = rnorml
             relres = relresl
          end if

          call Aprod ( n, x, r1 )
          r1  = b - r1 + shift_*x       ! r1 to temporarily store residual vector
          call Aprod ( n, r1, wl2 )     ! wl2 to temporarily store A*r1
          wl2 = wl2 - shift_*r1
          Arnorm_ = dnrm2(n, wl2, 1)
          if (rnorm_ > zero  .and.  Anorm_ > zero) then
             relAres = Arnorm_ / (Anorm_*rnorm_)
          end if
       end if

       if (nout_ > 0  .and.  .not. lastiter  .and.  mod(itn_-1,lines) == 0) then
          if (itn_ == 101) then
             lines     = 10
             headlines = 30*lines
          else if (itn_ == 1001) then
             lines = 100
             headlines = 30*lines
          end if

          if (QLPiter == 1) then
              QLPstr = ' P'
          else
              QLPstr = ' '
          end if
       end if


       ! See if it is time to print something.

       if (nout_ > 0) then
          prnt   = .false.
          if (n              <= 40          ) prnt = .true.
          if (itn_           <= 10          ) prnt = .true.
          if (itn_           >= itnlim_ - 10) prnt = .true.
          if (mod(itn_-1,10) == 0           ) prnt = .true.
          if (QLPiter        == 1           ) prnt = .true.
          if (Acond_         >= 0.01_dp/eps ) prnt = .true.
          if (istop_         /= flag0       ) prnt = .true.

          if ( prnt ) then
             if (itn_ == 1) write(nout_, tableHeaderStr)
             write(nout_, itnStr) itn_-1, x1last, xnorml, &
             rnorml, Arnorml, relresl, relAresl, Anorml, Acondl, QLPstr
             if (mod(itn_,10) == 0) write(nout_, "(1x)")
          end if
       end if

       if (debug) then
          write(*,*) 'istop = ', istop_
       end if
       if (istop_ /= flag0) exit
    enddo
    !===================================================================
    ! End of iteration loop.
    !===================================================================

    ! Optional outputs

    if (present(istop)) then
       istop = istop_
    end if

    if (present(itn)) then
       itn = itn_
    end if

    if (present(rnorm)) then
       rnorm = rnorm_
    end if

    if (present(Arnorm)) then
       Arnorm = Arnorm_
    end if

    if (present(xnorm)) then
       xnorm = xnorm_
    end if

    if (present(Anorm)) then
       Anorm = Anorm_
    end if

    if (present(Acond)) then
       Acond = Acond_
    end if

    if ( prnt ) then
       write(nout_, itnStr) itn_, x(1), xnorm_, &
             rnorm_, Arnorm_, relres, relAres, Anorm_, Acond_, QLPstr
    end if

    ! Display final status.

    if (nout_ > 0) then
       write(nout_, finalStr1) exitt, 'istop =', istop_, 'itn    =', itn_,    &
                               exitt, 'Anorm =', Anorm_, 'Acond  =', Acond_,  &
                               exitt, 'rnorm =', rnorm_, 'Arnorm =', Arnorm_, &
                               exitt, 'xnorm =', xnorm_
       write(nout_, finalStr2) exitt, msg(istop_)
    end if

    return
end subroutine MINRESQLP

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !> SymOrtho: Stable Householder reflection
    !!
    !!\verbatim
    !!  USAGE:
    !!     SymOrtho(a, b, c, s, r)
    !!
    !!  INPUTS:
    !!    a      first element of a two-vector  [a; b]
    !!    b      second element of a two-vector [a; b]
    !!
    !!  OUTPUTS:
    !!    c      cosine(theta), where theta is the implicit angle of reflection
    !!    s      sine(theta)
    !!    r      two-norm of [a; b]
    !!
    !!  DESCRIPTION:
    !!     Stable Householder reflection that gives c and s such that
    !!        [ c  s ][a] = [r],
    !!        [ s -c ][b]   [0]
    !!     where r = two norm of vector [a, b],
    !!        c = a / sqrt(a**2 + b**2) = a / r,
    !!        s = b / sqrt(a**2 + b**2) = b / r.
    !!     The implementation guards against overlow in computing sqrt (a**2 + b**2).
    !!
    !!
    !!  REFERENCES:
    !!    Algorithm 4.9, stable unsymmetric Givens rotations in
    !!     Golub and van Loan's book Matrix Computations, 3rd edition.
    !!
    !!  MODIFICATION HISTORY:
    !!    20/08/2012: Fixed a bug to ensure the 2x2 Hermitian reflectors
    !!                are orthonormal.
    !!    05/27/2011: Created this file from Matlab SymGivens2.m
    !!
    !!  KNOWN BUGS:
    !!     MM/DD/2004: description
    !!
    !!  AUTHORS: Sou-Cheng Choi, CI, University of Chicago
    !!           Michael Saunders, MS&E, Stanford University
    !!
    !!  CREATION DATE: 05/27/2011
    !!\endverbatim
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine SYMORTHO(a, b, c, s, r)

    real(dp), intent(in)  :: a, b
    real(dp), intent(out) :: c, s, r

    intrinsic :: abs, sqrt

    ! Local variables
    logical, parameter :: debug = .false.
    real(dp)           :: t
    real(dp)           :: abs_a, abs_b

    abs_a = abs(a)
    abs_b = abs(b)

    if (abs_b <= realmin) then
       s = zero
       r = abs_a
       if (a == zero) then
          c = one
       else
          c = a / abs_a
       end if

    else if (abs_a <= realmin) then
       c = zero;
       r = abs_b
       s = b / abs_b

    else if (abs_b > abs_a) then
       t = a / b
       s = (b / abs_b) / sqrt(one + t**2)
       c = s * t
       r = b / s  ! computationally better than r = a / c since |c| <= |s|

    else
       t = b / a
       c = (a / abs_a) / sqrt(one + t**2)
       s = c * t;
       r = a / c  ! computationally better than r = b / s since |s| <= |c|
    end if

    if (debug) then
       write(*,*)  'c = ',  c, ', s = ',  s, ', r = ',  r
    end if

  end subroutine SYMORTHO

end module minresqlpModule
