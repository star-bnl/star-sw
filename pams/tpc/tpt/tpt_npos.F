
      SUBROUTINE n_max(trk,rr,nmax,bf)
      IMPLICIT NONE
C
#include "phys_constants.inc"
#include "math_constants.inc"

C
      REAL    trk(6),rr,xc(2)
      INTEGER nmax,i,l,ipad1(8),ipad2(5),ipad3(31)
      REAL    dia,zpl,bf,sav2
      REAL    ypad(45),xwidth(45),x(2),y(2),x1,x2,f2,y0
C     
      DATA ipad1/0,8,8,8,6,8,8,8/
      DATA ipad2/8,6,10,8,10/
      DATA ipad3/4*2,0,3*2,0,5*2,0,3*2,0,4*2,0,4*2,3*0/
      
C//  first job is to initialize ypad, xwidth ( lengths in cm )
      ypad(1) = 60.0            !radial position of innermost padrow
      xwidth(1) = 29.48         !full width 88pads x .335 cm spacing 
      DO i =2,8
         ypad(i) = ypad(i -1) + 4.8
         xwidth(i) = xwidth(i -1) +  float(ipad1(i))*.335
      ENDDO
      DO i =9,13
         ypad(i) = ypad(i-1) + 5.20
	 xwidth(i) = xwidth(i-1) +  float(ipad2(i-8))*.335
      ENDDO
C     // 14th row is first padrow in the outer sector
      ypad(14) = ypad(13) + 7.6 !radial position of first outer sector padrow
      xwidth(14) = 65.77        !98 pads x .67 cm spacing
      DO i = 15,45
         ypad(i) = ypad(i-1) +2.00
         xwidth(i) = xwidth(i-1) + float(ipad3(i-14))*.67
      ENDDO
      
C***  z position of padplane
      zpl = 210.
      
C***  Rotate track so its sector pads are normal to y-axis
C***  Watch out. This is not as trivial as it seems.
      sav2 = trk(2)
      IF(amod(trk(2),0.5236).gt.0.2618) THEN !0.2618 = 15 degrees
         trk(2)=amod(trk(2),0.5236)+1.0472 !only 60 degrees
      ELSE
         trk(2)=amod(trk(2),0.5236)+1.5708 ! 0.5236 = 30 degrees
      ENDIF
      
      trk(4)=trk(4) +  trk(2) - sav2 ! psi has const. relation to phi
      
C***  Loop over three adjacent sectors
      
      nmax = 0
      trk(2) = trk(2) - 2.*0.5236
      trk(4) = trk(4) - 2.*0.5236
      DO 75 l=1,3
         
C***  Clear variables
         DO i=1,2
            x(i) = 0.
            y(i) = 0.
         ENDDO
         
         trk(2) = trk(2) + 0.5236
         trk(4) = trk(4) + 0.5236
         call nmax_circle_param(trk,xc,rr,bf)         
         
         
C***  Intersections of track with first-last padrow
         DO 30 i=1,45
            IF( (trk(5).ne.0.).and.
     +           (ypad(i).le.(abs(trk(1)*sin(trk(2)))+
     +           (zpl-abs(trk(3)))/abs(trk(5))))
     +           ) THEN
               f2 = (ypad(i)-xc(2))*(ypad(i)-xc(2))
               dia = rr*rr - f2
               IF(dia.lt.0.) goto 30 ! No solution
               
               x1 =  sqrt(dia)
               x2 = -sqrt(dia)
C***  First pair of possible solutions
               x(1) = x1 + xc(1)
C***  Second pair
               x(2) = x2 + xc(1)
               
               IF( abs(x(1)).lt.0.5*xwidth(i) ) nmax = nmax + 1
               IF( abs(x(2)).lt.0.5*xwidth(i) ) nmax = nmax + 1
            ENDIF
 30      CONTINUE
         
         
 75   CONTINUE                  ! over three sectors
      
 90   IF(nmax.eq.0) nmax = 2    ! protect divisions
      
      RETURN
      END
C=======================================================================
      
      SUBROUTINE nmax_circle_param(trk,xc,r,beta)
      IMPLICIT NONE
C   Input arguments  :	trk(6)	!Helix parameters of track
C                               !radius of circle
C                       beta    !MAg field in tesla
C   Output arguments :	xc(2)	!coordinates of the centre of circle
C   Functional Description : Returns the x-y coordinates and the radius
C			     of the circle (projected-helix) on the
C			     bending plane.
C   Created  DEC-1991 S.Margetis   
C   Error conditions	: None
C   Status		: Tested
C----------------------------------------------------------------------
C-SM      INTEGER iflag
      REAL    trk(6),xc(2),r,beta
      REAL    x,y,p(3),a,b,ab,rxp,test
C--------find the radius and the (hit)x-y coordinates------------------
      IF(trk(1).eq.0.) trk(1)=0.01
      x=trk(1)*cos( trk(2) )
      y=trk(1)*sin( trk(2))
C--------get the momentum components-----------------------------------
C-SM      iflag=0
C-SM      CALL nmax_track_mom(trk,r,iflag,p,beta)
      CALL nmax_track_mom(trk,r,p,beta)
C--------find the two possible solutions------------------------------
      IF(abs(p(1)).lt.0.001) p(1)=0.001
      a=p(2)/p(1)
      b = sqrt( r*r/(a*a+1) )
C     yt1= b + y
C     yt2=-b + y
C     xt1=-a*b + x
C     xt2= a*b + x
      ab = a*b
C-------decide which one is the right one------------------------------
C     rxp=(xt1-x)*p(2) - b*p(1)
      rxp=-(ab*p(2) + b*p(1))
      test=rxp*trk(6)/r
      IF(test.gt.0.0) THEN
C	xc(1)=xt1
C	xc(2)=yt1
	xc(1)=-ab + x
	xc(2)= b + y
      ELSE
C	xc(1)=xt2
C	xc(2)=yt2
	xc(1)=ab + x
	xc(2)=-b + y
      ENDIF 	!test.lt.0.0
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END

C******************************************************************************


C-SM      SUBROUTINE nmax_track_mom(trk,rc,iflag,p,beta)
      SUBROUTINE nmax_track_mom(trk,rc,p,beta)
      IMPLICIT NONE
C   Input arguments  :	trk(6)	!helix parameters of the track
C			iflag	!if =5 it is a neutral particle and
C				!trk(6)=1./momentum of particle
C                        beta    !field in Tesla
C   Output arguments : 	p(3)	!momentum vector of track
C   Functional Description :  	returns the momentum components
C				of a track
C   Created  DEC-1991  S.Margetis
C   Error conditions	:	None
C   Status 		:	Tested
C----------------------------------------------------------------------

C-SM      INTEGER iflag
      REAL    trk(6),p(3),rc
      REAL    const,beta
      REAL    sph,cph,pt
      PARAMETER (const=0.299792458)

C-----Extract momentum components from helix --------------------------

	cph  =cos(trk(4))
	sph  =sin(trk(4))

C-------check if it is a neutral particle------------------------------

C-SM       IF(iflag.eq.5) THEN
       IF(abs(beta).le.0.001) THEN
	pt   = 100.        !just any big number is good (GeV)
       ELSE
	pt   = (beta*const*rc)/100.
       ENDIF	!iflag.eq.5

        p(3) = pt*trk(5)
	p(1) = pt*cph
	p(2) = pt*sph

  999 CONTINUE
      RETURN
      END







