
      SUBROUTINE n_max(trk,rr,nmax,bf)
      IMPLICIT NONE
C
#include "phys_constants.inc"
#include "math_constants.inc"

C
      REAL    trk(6),rr,xc(2)               !TPC parametrization (tpt_track.idl)
      INTEGER nmax,i,k,l
      REAL    ath(2),dia,b,zpl,hit(8),bf,sav2
      REAL    ypad(2),x(8),y(8),x1,x2,f2,y0

C***Angles of sector-1 sides and y0 in y=y0+ath(i)*x
      ath(1) = tan(1.31)
      ath(2) = tan(1.83)     
      y0     = 5.25   

C***y positions of first/last padrow
      ypad(1) = 59.9
      ypad(2) = 189.5

C*** z position of padplane
      zpl = 210.
      
C***  Rotate track so its sector pads are normal to y-axis
C***  Watch out. This is not as trivial as it seems.
      sav2 = trk(2)
      IF(amod(trk(2),0.5236).gt.15) THEN
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
         DO i=1,8
            x(i) = 0.
            y(i) = 0.
         ENDDO

         trk(2) = trk(2) + 0.5236
         trk(4) = trk(4) + 0.5236
         call nmax_circle_param(trk,xc,rr,bf)         
         
C***  Intersections of track circle with sector sides (line)
         k = 0
         DO 15 i=1,2
            b = ath(i)*xc(1)+y0-xc(2)
            dia=(ath(i)*b)*(ath(i)*b)-(1+ath(i)*ath(i))*(b*b-rr*rr)
            IF(dia.lt.0.) goto 15 ! No solution
            
            x1 = (-ath(i)*b + sqrt(dia))/(1 + ath(i)*ath(i))
            x2 = (-ath(i)*b - sqrt(dia))/(1 + ath(i)*ath(i))
C***  First pair of possible solutions
            x(k+1) = x1 + xc(1)
            y(k+1) = y0 + ath(i)*x(k+1)
C***  Second pair
            x(k+2) = x2 + xc(1)
            y(k+2) = y0 + ath(i)*x(k+2)
            
            IF(y(k+1).le.60.) y(k+1)=0.
            IF(y(k+2).le.60.) y(k+2)=0.
            IF(y(k+1).ge.189.2) y(k+1)=0.
            IF(y(k+2).ge.189.2) y(k+2)=0.
            
            k = k + 2
            
 15      CONTINUE   
         
C***  Intersections of track with first-last padrow
         DO 30 i=1,2
            f2 = (ypad(i)-xc(2))*(ypad(i)-xc(2))
            dia = rr*rr - f2
            IF(dia.lt.0.) goto 30 ! No solution
            
            x1 =  sqrt(dia)
            x2 = -sqrt(dia)
C***  First pair of possible solutions
            x(k+1) = x1 + xc(1)
            y(k+1) = ypad(i)
C***  Second pair
            x(k+2) = x2 + xc(1)
            y(k+2) = ypad(i)

            IF(i.eq.1) THEN     !inner padrow limits left-right
               IF( abs(x(k+1)).gt.14.74 ) y(k+1)=0.
               IF( abs(x(k+2)).gt.14.74 ) y(k+2)=0.
            ELSE                !outer padrow limits left-right
               IF( abs(x(k+1)).gt.48.24 ) y(k+1)=0.
               IF( abs(x(k+2)).gt.48.24 ) y(k+2)=0.
            ENDIF
            
            k = k + 2

 30      CONTINUE
         
C***  Arrange possible solutions in increasing y-order
         
         DO 45 i=1,8
            hit(i)=0.
            IF( (trk(5).ne.0.).and.
     +(y(i).gt.(abs(trk(1)*sin(trk(2)))+(zpl-abs(trk(3)))/abs(trk(5))))
     +        ) THEN
      hit(i) = abs(trk(1)*sin(trk(2)))+(zpl-abs(trk(3)))/abs(trk(5))
            ELSE
               hit(i) = y(i)
            ENDIF
            IF(y(i).le.59.) hit(i) = 200.
 45      CONTINUE
         k = 8
C***  short the "hit" array (name 'hit' is misleading)
         CALL FLPSOR(hit,k)
c         CALL MY_SORT(hit) !my slow sorting routine

         
C***  Calculate padrow crossings
         DO 60 i=1,8,2
            IF(hit(i).gt.190.) goto 75 ! jump out, no solutions
            IF(hit(i).le.123) THEN ! inner sector
               IF(hit(i+1).le.123) THEN
                  nmax = nmax + int(abs(hit(i+1)-hit(i))/5.2)
               ELSE             ! some inner, some outer
                  nmax = nmax + int(abs(123.-hit(i))/5.2) + 1
                  nmax = nmax + int(abs(127.2-hit(i+1))/2.) + 1
               ENDIF
            ELSE                ! outer sector
               nmax = nmax + int(abs(hit(i+1)-hit(i))/2.)
            ENDIF   
 60      CONTINUE
         
         
 75   CONTINUE                  ! over three sectors
      RETURN
      END
C=======================================================================

       SUBROUTINE nmax_circle_param(trk,xc,r,beta)
      IMPLICIT NONE
C   Input arguments  :	trk(6)	!Helix parameters of track
C			r	!radius of circle
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

C***********************************************************************

      SUBROUTINE MY_SORT(a)
      IMPLICIT NONE
      REAL     a(8), temp
      INTEGER  i, j

      DO  i=1,7
         DO  j=(i+1),8
         IF(a(i).gt.a(j)) THEN
            temp = a(i)
            a(i) = a(j)
            a(j) = temp
         ENDIF
         ENDDO
      ENDDO


      RETURN
      END









