      SUBROUTINE eut_extra_rad ( p,  rd,     zp,   ang
     +,                          td, cosphi, zint, iercod, dirvec )
c:>------------------------------------------------------------------
C:FILE:         ems_ev0_routines.F
C:DESCRIPTION:  Service routines for EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       Spiros Margetis?
C:BUGS:         None known.
C:HISTORY:      05dec95-v1.0-wjl- Creation.
c:>------------------------------------------------------------------
c:>------------------------------------------------------------------
C:ROUTINE:      eut_extra_rad
C:DESCRIPTION:  use helix parameters to compute incercept w/ given cylinder...
C:ARGUMENTS:    see code
C:ARGUMENTS:    see code
C:RETURN VALUE: 
c:>------------------------------------------------------------------
      IMPLICIT NONE
CCC
CCC   NEW SUBR: eut_extra_rad 
CCC
CCC        Uses the wayne state helix parameters xctr, yctr, etc. passed
CCC        as arguments.
CCC
CCC        This takes the input helix parameters and computes the intercept
CCC        point with an imaginary cylinder at radius rd (cm).
CCC
CCC    p(I=1,6) : Array of input parameters
CCC    rd       : Radius for point of intercept.
CCC    zp       : Charge
CCC    ang      : Intercept azimuthal angle
CCC    td       : Phase angle of intercept point in helix equation.
CCC    cosphi   : Cosine of angle between particle momentum vector at intercept
CCC               and normal vector to cylinder at intercept point.
CCC    zint     : Z value of intercept point.
CCC    iercod   : 0 , track is intercepted ok.
CCC             : 1 , helix does not intercept the cylinder
CCC    dirvec(I=1,3) : (X,Y,Z) components of unit vector in direction of
CCC                    particle momentum at the intercept point.
CCC
CCC  Variable and array type declarations
CCC
      REAL    pi
      PARAMETER (pi=3.14159265)
      REAL    ang,cosphi,ddd,dsx,dsy,dsz,eq
      REAL    fac1,sfac, fac2,r,rd,sl,t0,td,deltat
      REAL    xc,yc,z0,zint,zp
      INTEGER iercod
      REAL    p(6), dirvec(3)
CCC
CCC   Set to zero
CCC
      iercod = 0
CCC
CCC   Copy input array
CCC
      eq = zp/abs(zp)
      xc = p(1)
      yc = p(2)
      z0 = p(3)
      r  = p(4)
      sl = p(5)
      t0 = p(6)
CCC
CCC    Check helix and cylinder intersect
CCC
      fac1 = xc*xc + yc*yc
      sfac = sqrt( fac1 )
CCC
CCC    If they don't intersect return
CCC
      if ( abs(sfac-r).gt.rd .or. abs(sfac+r).lt.rd ) then
         iercod = 1
         return
      endif
CCC
CCC    Find intersection
CCC
      fac2   = ( rd*rd + fac1 - R*R) / (2.00 * rd * sfac )
      ang    = atan2(yc,xc) + eq*acos(fac2)
      td     = atan2(rd*sin(ang) - yc,rd*cos(ang) - xc)
      cosphi = eq*sin(td-ang) / sqrt(1.00 + sl*sl)
CCC
CCC   Intersection in z
CCC
*     zint = z0 + r*sl*(-eq*td + eq*t0)
      if ( td .lt. 0 ) td = td + 2. * pi
      deltat = mod((-eq*td + eq*t0),2*pi)
      if ( deltat .lt. 0. ) deltat = deltat + 2. * pi
      zint = z0 + r * sl * deltat
CCC
CCC   Momentum trajectory at intersection
CCC
      dsx       =  eq*r*sin(td)
      dsy       = -eq*r*cos(td)
      dsz       = r*sl
      ddd       = sqrt(dsx*dsx + dsy*dsy + dsz*dsz)
CCC
      dirvec(1) = dsx/ddd
      dirvec(2) = dsy/ddd
      dirvec(3) = dsz/ddd
CCC
CCC   That's it
CCC
      END
