CC:>--------------------------------------------------------------------
CC: FILE:       evr_am.F
CC: HISTORY:    
CC:             no history only future !!!
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION EVR_AM(
     1            evrpar_h,            evrpar ,
     2           tptrack_h,           tptrack ,
     3         svt_track_h,         svt_track ,
     4           globtrk_h,           globtrk ,
     5         evt_match_h,         evt_match ,
     6           privert_h,           privert ) 
      IMPLICIT NONE
#include "evr_am.inc"
CC:>----------------------------------------------------------------------
CC: ROUTINE:    EVR
CC: DESCRIPTION: Vertex fit, least square method with outlier removal
CC: AUTHOR:     S. Margetis  Created  MARCH-1992
CC: ARGUMENTS:
CC:          IN:
CC:	       	evrpar   = Parameters controlling vertex finding
CC:     	tptrack    = Track information from tpt package
CC:       	svt_track    = Track information from stk package
CC:       	globtrk  = Track information from egr package
CC:       	evt_match    = Matching information from svm package
CC:         OUT:
CC:	       	privert  = Primary Vertex Information
CC: RETURNS:    STAF Condition Value
CC: ERROR CONDITIONS	: None-Yet
CC: STATUS		: Tested
CC: MODIFIED TO WORK IN TAS : E. G. Judd, November 1993
CC:   Added switches so the vertex finder can use TPC, SVT or global tracks.
CC:         Also added cuts on pt, and whether or not the track has been
CC:         matched, to improve track selection : E. G. Judd, April 1994
CC: MODIFIED TO WORK IN STAF : S. Margetis, November 1996
CC:
CC:  METHOD:
CC:  Minimization of the perpendicular distances from the vectors to a point.
CC:
CC:  Consider an arbitrary vector Vi, with a unit vector in the direction
CC:  The distance from a point P to the vector Vi is the crossproduct of the 
CC:  unit vector e(Vi)^ with any line connecting P to any point Q on the line.
CC:  Line PQ = (x0-xi)x^+(y0-yi)y^+(z0-zi)z^
CC:	P=(x0,y0,z0)    Q=(xi,yi,zi)
CC:  The distance is: |d|=|PQ X e(Vi)^|
CC:
CC:  A least square minimization employs a merit function (chi square) to 
CC:  assess quality of fit.  The optimum values of the variables are 
CC:  determined by where the partial derivatives of the merit function are
CC:  equal to zero.  For this case, the merit function is the summation of
CC:  the squares of the perpendicular distances from the vectors (Vi) to a 
CC:  point (Xo, Yo, Zo) [divided by the squares of the uncertanty
CC:  errors?] (sumation over vector number).  The partial derivatives are taken
CC:  with respect to x0, y0, and Z0.  We are left with three equations 
CC:  for three unknowns (Xo,Yo,Zo).  The evaluation is done through matrix 
CC:  manipulation.
CC:
CC:>----------------------------------------------------------------------

      INTEGER 	maxtr,i,j,k,ipnt
      PARAMETER	(maxtr=5000)
      REAL 	vtrv(6,maxtr)
      COMMON 	/FITVERT/ vtrv
      REAL 	XV,YV,ZV
      COMMON 	/VERTEX/ XV,YV,ZV
      INTEGER   ntrk,index
      LOGICAL   match_flag
      REAL	dr
      REAL      x0(maxtr),y0(maxtr),z0(maxtr)
      REAL	phi0(maxtr),tanl(maxtr)
      REAL      psi,p,r0,phi,b,z,tanla
      INTEGER   icharge
      REAL	xp(2),x(4)
      REAL	zsum(100),seed,zmax
*
        CALL VZERO(zsum,100)
	ntrk = 0
C=== decide which tracks to use===
        if (evrpar(1).use.eq.0.or.evrpar(1).use.eq.1) then
           if (evrpar(1).match.eq.1) then
              match_flag = .true.
              ntrk = evt_match_h.nok
           elseif (evrpar(1).match.eq.0) then
              if (evrpar(1).use.eq.0) ntrk = tptrack_h.nok
              if (evrpar(1).use.eq.1) ntrk = svt_track_h.nok
           else
              write(6,'('' Not a valid value for EVRPAR.MATCH'')')
              evr_am = STAFCV_BAD
              return
           endif
        elseif (evrpar(1).use.eq.2) then
           ntrk = globtrk_h.nok
        else
           write(6,'('' Not a valid value for EVRPAR.USE'')')
           evr_am = STAFCV_BAD
           return
        endif

        k = 0
        do 20 i = 1,ntrk
           if (evrpar(1).use.eq.0) then
              if (match_flag) then
                 ipnt = evt_match(i).idtpc
              else
                 ipnt = i
              endif
              psi = tptrack(ipnt).psi
             icharge = tptrack(ipnt).q
              p = 1/tptrack(ipnt).invp
              tanla = tptrack(ipnt).tanl
              r0 = tptrack(ipnt).r0
              phi = tptrack(ipnt).phi0
              z = tptrack(ipnt).z0
           elseif (evrpar(1).use.eq.1) then
              if (match_flag) then
                 ipnt = evt_match(i).idsvt
              else
                 ipnt = i
              endif
              psi = svt_track(ipnt).psi
             icharge = sign(1.0,svt_track(ipnt).invpt)
              p = 1/abs(svt_track(ipnt).invpt)
              tanla = svt_track(ipnt).tanl
              r0 = svt_track(ipnt).r0
              phi = svt_track(ipnt).phi0
              z = svt_track(ipnt).z0
           elseif (evrpar(1).use.eq.2) then
              psi = globtrk(i).psi
             icharge = globtrk(i).icharge
              p = 1/globtrk(i).invpt
              tanla = globtrk(i).tanl
              r0 = sqrt(globtrk(i).x0**2+globtrk(i).y0**2)
              phi = atan2(globtrk(i).x0,globtrk(i).y0)*57.29
              z = globtrk(i).z0
           endif
           if (p.ge.evrpar(1).ptmin) then
              b = 5.0
              xp(1) = 0.0
              xp(2) = 0.0
              call evr_project_track(psi,icharge,p,tanla,r0,phi,b,z,xp,x)
              k = k + 1
	      x0(k) = x(1)
	      y0(k) = x(2)
	      z0(k) = x(3)
              phi0(k)= x(4)
              tanl(k) = tanla
              if(abs(z0(k)).lt.15.) then
                 zsum(int(2*z0(k))+30)=zsum(int(2*z0(k))+30)+1.
              endif
           endif
20      continue

        ntrk = k
        if (ntrk.lt.3) then
           write(6,'('' Not enough tracks to find the vertex'')')
           evr_am = STAFCV_BAD
           return
        endif

        seed=0.
        zmax=0.
        do i=2,60
         if(zsum(i).gt.zmax) then
           zmax=zsum(i)
           seed=float(i-30)/2.
         endif
        enddo   

	k=0
	do 10 i=1,ntrk
	   dr=sqrt( x0(i)*x0(i)+y0(i)*y0(i) )
	   if(dr.lt.evrpar(1).cutxy) then
	      if( abs(z0(i)-seed).lt.evrpar(1).cutz) then
	         k=k+1
	         vtrv(1,k)=phi0(i)
	         vtrv(2,k)=tanl(i)
	         vtrv(4,k)=x0(i)
	         vtrv(5,k)=y0(i)
	         vtrv(6,k)=z0(i)
	      endif
	   endif
10	continue

	CALL EVR_VERTEX_FIT(k)

	k=0
	do 105 i=1,ntrk
	dr=sqrt( (x0(i)-xv)*(x0(i)-xv)+(y0(i)-yv)*(y0(i)-yv)
     +          +(z0(i)-zv)*(z0(i)-zv) )
	if(dr.lt.evrpar(1).cut2) then
	k=k+1
	vtrv(1,k)=phi0(i)
	vtrv(2,k)=tanl(i)
	vtrv(4,k)=x0(i)
	vtrv(5,k)=y0(i)
	vtrv(6,k)=z0(i)
	endif
105	continue

	CALL EVR_VERTEX_FIT(k)

	k=0
	do 205 i=1,ntrk
	dr=sqrt( (x0(i)-xv)*(x0(i)-xv)+(y0(i)-yv)*(y0(i)-yv)
     +          +(z0(i)-zv)*(z0(i)-zv) )
	if(dr.lt.evrpar(1).cut3) then
	k=k+1
	vtrv(1,k)=phi0(i)
	vtrv(2,k)=tanl(i)
	vtrv(4,k)=x0(i)
	vtrv(5,k)=y0(i)
	vtrv(6,k)=z0(i)
	endif
205	continue

	CALL EVR_VERTEX_FIT(k)
C
C save results
C
        index=privert_h.nok + 1
        privert_h.nok = index

        if (evrpar(1).use.eq.0) then
           privert(index).det_id = 1
           privert(index).sigma(1)  = 0.0150
           privert(index).sigma(2)  = 0.0150
           privert(index).sigma(3)  = 0.0200
        elseif (evrpar(1).use.eq.1) then
           privert(index).det_id = 2
           privert(index).sigma(1)  = 0.0050
           privert(index).sigma(2)  = 0.0050
           privert(index).sigma(3)  = 0.0050
        elseif (evrpar(1).use.eq.2) then
           privert(index).det_id = 3
           privert(index).sigma(1)   = 0.0150
           privert(index).sigma(2)  = 0.0150
           privert(index).sigma(3)  = 0.0200
        endif
CC        privert(index).match = evrpar(1).match
CC        privert(index).ptmin = evrpar(1).ptmin
        privert(index).id = 1
cc        privert(index).ndegf = 2*k
cc        privert(index).ntch = k
        privert(index).x = xv
        privert(index).y = yv
        privert(index).z = zv
        privert(index).iflag  = 1     ! found
        privert(index).vtx_id  = 1    ! primary
        privert(index).n_daughters  = 0
        privert(index).pchi2  = 0
        privert(index).id_aux_ent  = 0


        evr_am = STAFCV_OK

      RETURN
      END
C-------------------------------------------------------------------

	SUBROUTINE EVR_VERTEX_FIT(NT)
	IMPLICIT NONE
CC:>--------------------------------------------------------------------
CC:Passed parameters:  NT = number of tracks invloved in the fit
CC:returned param:    
CC:		      Xv,Yv,Zv = the vertex co-ordinants
CC:		      NT = number of tracks used for the vertex fitting
CC:			   (note this variable is passed and returned.)
CC:		       the pointer list is reordered to reflect which
CC:		       tracks were used in the fit
CC:
CC:Description: It performs the least square fit to find a common vertex
CC:for NT tracks whose parameters are stored in the /fitvert/ common.
CC:The vertex information is returned as coordinants.
CC:
CC:Written by Daniel Cebra
CC:Modified for STAR by S. Margetis
CC:
CC:>--------------------------------------------------------------------

	integer ngmx
	PARAMETER (NGMX=5000)
	integer ITACP(NGMX),NEX,NTRK
	real Trk(15,ngmx)
        COMMON /VTRAK/ ITACP,NEX,NTRK,TRK
	REAL vtrv(6,ngmx)
	common /fitvert/ vtrv
	real Xv,Yv,Zv
	COMMON /VERTEX/XV,YV,ZV
c  passed parameters:
	integer FT,NT,LT
	real optimize

c  Local variables:
	integer i,j,k,l,nstart,maxIndex
	real x,y,z,x0,y0,z0
	real dxdx,dxdy,dxdz,dxdL,dxdV,dydy,dydz,dydV,dydL,dzdz,dzdV,dzdL,
     >       dVdV,dVdL,dLdL
	real sinV,cosV,sinL,cosL,U,rho
	real sigma(3,3),H(3),G(3,3),Txx,Txy,Txz,Tyy,Tyz,Tzz,G_inv(3,3),det
	real Sxx,Sxy,Sxz,Syy,Syz,Szz,s2
	real Dmcs,d0,d2,d3,Dtlt,Dtot,Dsig,Dmax,Dave,Dst,Dmat(ngmx)
	real SigX,SigY,SigZ,Cx(ngmx),Cy(ngmx),Cz(ngmx)
	real Mass,Q,Ptot,multiple_Coulomb_scattering
	real cut


	nstart=nt
c  Convention: L=angle lambda, V=angle phi
*
200	do i=1,3			! Clear the transformation matrices
	  H(i)=0
	  do j=1,3
	    G(i,j)=0
	  end do
	end do
*
	do k=1,nt-1			! Master loop over tracks
	  sinV=sind(vtrv(1,k))
	  cosV=cosd(vtrv(1,k))
	  cosL=1/(sqrt(1 + vtrv(2,k)**2))
	  sinL=cosL*vtrv(2,k)
*
c	  Dmcs=Multiple_Coulomb_Scattering(Ptot,Mass,Q,cosL)
c	  Dmcs=0.005
c	  Sxx=(Dmcs+0.0025)*(Dmcs+0.0025)
c	  Sxy=(Dmcs+0.0025)*(Dmcs+0.0025)
c	  Sxz=(Dmcs+0.0025)*(Dmcs+0.0025)
c	  Syy=(Dmcs+0.0025)*(Dmcs+0.0025)
c	  Syz=(Dmcs+0.0025)*(Dmcs+0.0025)
c	  Szz=(Dmcs+0.0025)*(Dmcs+0.0025)
*
	  X0= vtrv(4,k)
	  Y0= vtrv(5,k)
	  Z0= vtrv(6,k)
c  Calculate the matrix elements of Ti (individual transformation matrices)
c  in the document Mi is the same as Ti
	  Txx= sinL**2+(sinV*cosL)**2
	  Txy=-cosL*cosL*cosV*sinV
	  Txz=-cosV*cosL*sinL
	  Tyy= sinL**2+(cosL*cosV)**2
	  Tyz=-cosL*sinL*sinV
	  Tzz= cosL*cosL
c  The G matrix is the summations of the individual transformation matrices 
	  G(1,1)=G(1,1)+Txx
	  G(1,2)=G(1,2)+Txy
	  G(1,3)=G(1,3)+Txz
	  G(2,2)=G(2,2)+Tyy
	  G(2,3)=G(2,3)+Tyz
	  G(3,3)=G(3,3)+Tzz
c  The H vector is the summation of the Qi times Ti
	  H(1)=H(1)+X0*Txx+Y0*Txy+Z0*Txz
	  H(2)=H(2)+X0*Txy+Y0*Tyy+Z0*Tyz
	  H(3)=H(3)+X0*Txz+Y0*Tyz+Z0*Tzz
	end do
c  assign the reflexive elements
	G(2,1)=G(1,2)
	G(3,1)=G(1,3)
	G(3,2)=G(2,3)
c  Invert the G matrix and calculate the determinant
c  Calcualate the determinant
	det=G(1,1)*(G(2,2)*G(3,3)-G(2,3)*G(3,2))-
     >      G(2,1)*(G(1,2)*G(3,3)-G(1,3)*G(3,2))+
     >      G(3,1)*(G(1,2)*G(2,3)-G(1,3)*G(2,2))
	IF(abs(DET).LT.1.0E-25) THEN
	    write(6,*) ' V_F:  determinant too small   det=',det
	    write(6,*) (g(1,k),k=1,3)
	    write(6,*) (g(2,k),k=1,3)
	    write(6,*) (g(3,k),k=1,3)
	    GO TO 999
	END IF
	G_inv(1,1)=+(G(2,2)*G(3,3)-G(3,2)*G(2,3))/det
	G_inv(1,2)=-(G(2,1)*G(3,3)-G(3,1)*G(2,3))/det
	G_inv(1,3)=+(G(2,1)*G(3,2)-G(3,1)*G(2,2))/det
	G_inv(2,1)=-(G(1,2)*G(3,3)-G(3,2)*G(1,3))/det
	G_inv(2,2)=+(G(1,1)*G(3,3)-G(3,1)*G(1,3))/det
	G_inv(2,3)=-(G(1,1)*G(3,2)-G(3,1)*G(1,2))/det
	G_inv(3,1)=+(G(1,2)*G(2,3)-G(2,2)*G(1,3))/det
	G_inv(3,2)=-(G(1,1)*G(2,3)-G(2,1)*G(1,3))/det
	G_inv(3,3)=+(G(1,1)*G(2,2)-G(2,1)*G(1,2))/det
*
*  The vertex vector = The inverted G matrix times the H vector
*
	Xv=G_inv(1,1)*H(1)+G_inv(1,2)*H(2)+G_inv(1,3)*H(3)
	Yv=G_inv(2,1)*H(1)+G_inv(2,2)*H(2)+G_inv(2,3)*H(3)	
	Zv=G_inv(3,1)*H(1)+G_inv(3,2)*H(2)+G_inv(3,3)*H(3)	
	print *,' Estimated Vertex:',Xv,Yv,Zv

999	return
	end
C
      SUBROUTINE EVR_PROJECT_TRACK(psi,q,p,tanl,r0,phi,bfld,z0,xp,x)
      IMPLICIT NONE
CC:>----------------------------------------------------------------------
CC: INPUT ARGUMENTS  :	
CC:                       psi     !azimuthal angle for pt at first point
CC:                               !on track
CC:                       q       !charge
CC:                       p       !momentum of track
CC:                       tanl    !tangent of dip angle at first point on track
CC:                       r0      !radial coordinate of first point
CC:                       phi     !azimuthal angle of first point
CC:                       bfld    !magnetic field in kG
CC:                       z0      !z coord of first point on track
CC:                       xp      !x & y coord. of new point
CC:
CC: OUTPUT ARGUMENTS : 	x(4)	!xyz coord. of point of closest approach
CC:                               !and phi at that point
CC:
CC: DESCRIPTION :	Extrapolates a helix 
CC:				and calculates the 
CC:				coord. of the point of the closest approach
CC:				from a given point in the bending plane.
CC:
CC: AUTHOR:  JAN-1992 S.Margetis 
CC: MODIFIED NOV-1993 E. G. Judd  
CC: ERROR CONDITIONS : None
CC: STATUS	     : Test
CC:
CC:>---------------------------------------------------------------------

      REAL psi,p,tanl,r0,phi,bfld,z0
      INTEGER q,i
      REAL rh,phih,psin
      REAL xp(2),xc(2),x(4)
      REAL x0,y0,axb,arg,dphih,ds,dz
      REAL x1(2),x2(2),yy1,yy2,zz1,zz2
      REAL a,b,c,ka
      DATA ka / 0.0003 /
C
      i = i + 1
C     
C first find phih, azimuthal angle of starting point wrt helix axis
C
      phih = psi + float(q)*90.0
      if (phih.lt.-180.0) then
         phih = 360.0 - phih
      elseif (phih.gt.180.0) then
         phih = phih - 360.0
      endif
C
C now find rh, the radius of the helix
C
      rh = p/abs(ka*float(q)*bfld)
C
C next calculate x0 and y0, the first points on the track
C
      if( phi.ge.-90.0.and.phi.le.90.0 ) then
         x0 = r0/sqrt(1.0 + tand(phi)**2)
         y0 = x0*tand(phi)
      else
         x0 = -r0/sqrt(1.0 + tand(phi)**2)
         y0 = x0*tand(phi)
      endif
C
C now calculate the coords of the centre of the helix
C
      xc(1) = x0 - rh*cosd(phih)
      xc(2) = y0 - rh*sind(phih)
C
C-----first find the two possible solutions for closest approach-----
C
      a=xc(1)-xp(1)
      b=xc(2)-xp(2)
      IF(b.eq.0.0) THEN
	x1(2)=xc(2)
	x2(2)=xc(2)
	x1(1)=xc(1)+rh
	x2(1)=xc(1)-rh
      ELSE
      	c=a/b
	yy1= rh/sqrt(c*c+1.)
	yy2=-rh/sqrt(c*c+1.)
	zz1=c*yy1
	zz2=c*yy2
	x1(1)=zz1+xc(1)
	x2(1)=zz2+xc(1)
	x1(2)=yy1+xc(2)
	x2(2)=yy2+xc(2)
      ENDIF	!b.eq.0
C-------choose the right one----------------------------------------
      a=(xp(1)-xc(1))*(x1(1)-xc(1)) + (xp(2)-xc(2))*(x1(2)-xc(2))
      IF(a.gt.0.) THEN
	x(1)=x1(1)
	x(2)=x1(2)
      ELSE
	x(1)=x2(1)
	x(2)=x2(2)
      ENDIF	
C
C     calculate the distance ds and the angle between the old and new position
C
      axb = (x0-xc(1))*(x(2)-xc(2)) - (y0-xc(2))*(x(1)-xc(1))
      arg = axb/(rh*rh)
      if( arg.ge.1.0) arg = 1.0
      if( arg.le.-1.0) arg = -1.0
      dphih = asin(arg)
      ds = dphih*rh
      dz = ds*tanl
C
C     calculate z0 and psin
C
      x(3) = z0 - float(q)*dz
      dphih = asind(arg)
      psin = phih + dphih
      if( psin.lt.-180.0 ) then
         psin = 360.0 - psin
      elseif (psin.gt.180.0) then
         psin = psin - 360.0
      endif
      x(4) = psin - float(q)*90.0

  999 CONTINUE
      RETURN
      END
