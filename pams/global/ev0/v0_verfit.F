      INTEGER FUNCTION v0_verfit(Iter,Chidif,rxy1,pvec1,cov1,
     +                                 rxy2,pvec2,cov2,
     +                            bfield,verpar,verchisq,vererr)

***********************************************************************
*                                                                     *
* Interface routine to V0 vertex fitting package                      *
* The assumption here is that track1 is +tive and track2 is -tive     *
*                                                                     *
* 11 October 1999  AUTHOR: A. C. Saulys , BNL                         *
*                                                                     *
* Iter is maximum number of iterations                                *
* Chidif is the change in chisq                                       *
* rxy1 is the radius from 0,0 for track 1                             *
* rxy2 is the radius from 0,0 for track 2                             *
* pvec is the helix parameter vector phi*r,z,tanl,psi,q/pt            *
* cov is the covariance matrix indexed as above                       *
* bfield is the z component of the magnetic field                     *
* verpar is the vertex x,y,z vector                                   *
* verchisq is the vertex chisq                                        *
*                                                                     *
***********************************************************************
      IMPLICIT NONE

#include "math_constants.inc"
#include "phys_constants.inc"

      EXTERNAL v0userfit

      INTEGER Mode, N1g, N1gf,N1c, Ng, Nc, Nx, Ierr

      PARAMETER (Nx = 15)
      PARAMETER (N1g = 10)
      PARAMETER (N1c = Nx+1)
      PARAMETER (Ng = 10)
      PARAMETER (Nc = 7)
      PARAMETER (N1gf = Nx)

      REAL           CONSB,Xb
      COMMON/FITV0C/ CONSB,Xb(30)

      REAL Chidif, rxy1, rxy2, verchisq
      REAL pvec1(*), pvec2(*), verpar(*), bfield(3)
      REAL RCOV1(5,5), RCOV2(5,5)
      REAL vererr(Nx,Nx)
      REAL Good, Goodsc(Nc)

      DOUBLE PRECISION COV1(*), COV2(*)
      REAL Lastchisq, Xi2

      REAL G(N1g,Ng), Gf(N1gf,Nx), Xf(Nx)
      REAL*8 W((Nx+Nc)*(Nx+Nc+3)+Ng*Ng),C(N1c,Nc)
      REAL r1,r2

      INTEGER ISW(Nx+Nc), Iter, indx, i, j, it

      CONSB = C_D_CURVATURE*bfield(3)*10.0 ! bfield is in Tor !!!!

      v0_verfit = 0

      Good = 0.25 ! convergence criterion
      do i = 1,Nc
        Goodsc(i) = 1.0
      enddo
      Goodsc(7) = 36.0 ! Loosen up the 7th constraint

      indx = 0
      do i = 1,5
       do j = i,5
        rcov1(i,j) = cov1(indx+j)
        rcov1(j,i) = cov1(indx+j)
        rcov2(i,j) = cov2(indx+j)
        rcov2(j,i) = cov2(indx+j)
       enddo
       indx=indx+(5-i)
      enddo

      do i = 1,5
       do j = 1,5
        G(j,i) = rcov1(j,i)
        G(j+5,i+5) = rcov2(j,i)
       enddo
      enddo
      CALL UCOPY(pvec1(1), Xb(1), 5)
      CALL UCOPY(pvec2(1), Xb(6), 5)

      Xb(Ng+1) = verpar(3)*cos(verpar(1)) ! x 
      Xb(Ng+2) = verpar(3)*sin(verpar(1)) ! y
      Xb(Ng+3) = verpar(2) ! z

      Xb(16) = rxy1
      Xb(17) = rxy2
      Xb(18) = verpar(5) ! psi1 at vertex
      Xb(28) = verpar(8) ! psi2 at verte
      Xb(20) = verpar(10) ! x at primary vertex
      Xb(21) = verpar(11) ! y at primary vertex
      Xb(22) = verpar(12) ! z at primary vertex
CCC      Xb(20) = 0.0 ! x at primary vertex (test)
CCC      Xb(21) = 0.0 ! y at primary vertex (test)
CCC      Xb(22) = 0.0 ! z at primary vertex (test)

      r1 = 1./(Xb(5)*CONSB) ! signed radius of curvature
      r2 = 1./(Xb(10)*CONSB) ! signed radius of curvature

      Xb(Ng+4) = r1*(verpar(5) - Xb(4)) ! alen1
      Xb(Ng+5) = r2*(verpar(8) - Xb(9)) ! alen2

      CALL VFILL(ISW(1),Nc+Nx,1) ! set these as variables

      CALL VZERO(XF(1),Nx)

      Mode = 2 + 10*Iter ! using covariance matrix
      Lastchisq = 100000.0

CCC      DO it = 1,Iter
        CALL CFIT (Good, Goodsc, Mode, Isw, G, N1g, C, N1c, Ng, Nc, Xf, Nx
     &             , Gf, N1gf, Xi2, W, V0USERFIT, Ierr)
        verchisq = Xi2
CCC KLUDGE
CCC        if(Ierr.eq.3)Ierr = 0
CCC ENDKLUDGE
        if(Ierr.ne.0)return
        CALL VADD (Xb(1), Xf(1), Xf(1), Nx)

        verpar(3)= sqrt(Xf(Ng+1)**2+Xf(Ng+2)**2) ! rxy
        verpar(1)= atan2(Xf(Ng+2),Xf(Ng+1)) ! phi
        verpar(2)= Xf(Ng+3) ! z
        CALL UCOPY(Xf(1), pvec1(1), 5)
        CALL UCOPY(Xf(6), pvec2(1), 5)

        do i = 1,nx
         do j = 1,nx
          vererr(i,j) = Gf(i,j)
         enddo
        enddo

      r1 = 1./(Xf(5)*CONSB) ! signed radius of curvature
      r2 = 1./(Xf(10)*CONSB) ! signed radius of curvature

      verpar(5) = Xf(Ng+4)/r1 + Xf(4) ! psi1 at vertex
      verpar(8) = Xf(Ng+5)/r2 + Xf(9) ! psi2 at vertex

CCC        verpar(13) = xf(14) ! impact parameter squared
CCC        verpar(14) = sqrt(vererr(14,14)) ! error on verpar(13)

        if((lastchisq-verchisq).lt.Chidif)go to 999 ! quit if no signifcant change


CCC      ENDDO
      
 999  Continue
      if(Ierr.eq.0)v0_verfit = 1

      Return
      END

 






