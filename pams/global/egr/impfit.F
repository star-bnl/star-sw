      INTEGER*4 FUNCTION impfit(Iter,rxy,pvect,cov,
     +                            bfield,mv,chisq,err)

***********************************************************************
*                                                                     *
* Interface routine to impact parameter fitting package               *
*                                                                     *
* 7 April 2000  AUTHOR: A. C. Saulys , BNL                         *
*                                                                     *
* Iter is maximum number of iterations                                *
* rxy  is the radius from 0,0 for track                               *
* pvect is the helix parameter vector phi*r,z,tanl,psi,q/pt at rxy    *
* cov is the covariance matrix indexed as above                       *
* bfield is the z component of the magnetic field                     *
* mv is the x,y,z of the mv                                           *
* chisq is the change of chisq of the constrained fit                 *
* err is the error matrix of the fit                                  *
*                                                                     *
***********************************************************************
      IMPLICIT NONE

      EXTERNAL userimpfit

      REAL           CONSB,Xb
      COMMON/FITV0C/ CONSB,Xb(30)

#include "math_constants.inc"
#include "phys_constants.inc"


      INTEGER Mode, N1g, N1gf,N1c, Ng, Nc, Nx, Ierr

      PARAMETER (Nx = 6)
      PARAMETER (N1g = 5)
      PARAMETER (N1c = Nx+1)
      PARAMETER (Ng = 5)
      PARAMETER (Nc = 3)
      PARAMETER (N1gf = Nx)


      REAL mv(3),pvect(*),rcov(5,5),chisq
      REAL rxy,bfield
      DOUBLE PRECISION COV(*)

      REAL err(Nx,Nx), r1
      REAL Lastchisq, Xi2
      REAL Good, Goodsc(Nc)

      REAL G(N1g,Ng), Gf(N1gf,Nx), Xf(Nx)
      REAL*8 W((Nx+Nc)*(Nx+Nc+3)+Ng*Ng),C(N1c,Nc)

      INTEGER ISW(Nx+Nc), Iter, indx, i, j, it

      impfit = 0

      CONSB = C_D_CURVATURE*bfield

      Good = .01 ! convergence criterion
      Do i = 1,Nc
       Goodsc(i) = 1.0
      enddo

      indx = 0
      do i = 1,5
       do j = i,5
        rcov(i,j) = cov(indx+j)
        rcov(j,i) = cov(indx+j)
       enddo
       indx=indx+(5-i)
      enddo

      do i = 1,5
       do j = 1,5
        G(i,j) = rcov(i,j)
       enddo
      enddo

      CALL UCOPY(pvect(1), Xb(1), 5)
      r1 = 1./(Xb(5)*CONSB) ! signed radius of curvature

CCC      Xb(6) = r1*(pvecv(4)-pvect(4)) 
      Xb(6) = rxy ! approximate initial guess for arclength 

      CALL UCOPY(mv(1), Xb(7), 3)

      Xb(10) = rxy

      CALL VFILL(ISW(1),Nc+Nx,1) ! set these as variables

      CALL VZERO(XF(1),Nx)

      Mode = 2 + 10*Iter ! using covariance matrix
      Lastchisq = 100000.0

        CALL CFIT (Good, Goodsc, Mode, Isw, G, N1g, C, N1c, Ng, Nc, Xf, Nx
     &             , Gf, N1gf, Xi2, W, USERIMPFIT, Ierr)
        chisq = Xi2
        if(Ierr.ne.0)return
        CALL VADD (Xb(1), Xf(1), Xf(1), Nx)

        do i = 1,Nx
         do j = 1,Nx
          err(i,j) = Gf(i,j)
         enddo
        enddo

 999  Continue
      if(Ierr.eq.0)impfit = 1

      Return
      END

 






