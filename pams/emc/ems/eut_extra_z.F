      SUBROUTINE eut_extra_z( p,     z,    zp,
     +           phi_int, td, r_int, iercod, dirvec)
c:>------------------------------------------------------------------
C:ROUTINE:      eut_extra_rad
C:DESCRIPTION:  use helix parameters to compute incercept w/ given cylinder...
C:ARGUMENTS:    see code
C:ARGUMENTS:    see code
C:RETURN VALUE: 
c:>------------------------------------------------------------------
      IMPLICIT NONE
CCC
CCC   NEW SUBR: eut_extra_z 
CCC
CCC        Uses the wayne state helix parameters xctr, yctr, etc. passed
CCC        as arguments.
CCC
CCC        This takes the input helix parameters and computes the intercept
CCC        point with an imaginary 2 plane at z (cm).
CCC
CCC    p(I=1,6) : Array of input parameters
CCC    rd       : z for point of intercept.
CCC    zp       : Charge
CCC    phi_int  : Intercept azimuthal angle
CCC    td       : Phase angle of intercept point in helix equation.
CCC    r_int    : radius value of intercept point.
CCC    iercod   : 0 , track is intercepted ok.
CCC             : 1 , helix does not intercept the cylinder
CCC    dirvec(I=1,3) : (X,Y,Z) components of unit vector in direction of
CCC                    particle momentum at the intercept point.
CCC
CCC  Variable and array type declarations
CCC
      REAL    pi
      REAL    ddd,dsx,dsy,dsz,eq
      REAL    r,rd,sl,t0,td,deltat,xc,yc,z0,zp
      REAL    z,phi_int,x_int,y_int,r_int,dz
      INTEGER iercod
      REAL    p(6), dirvec(3)
CCC
CCC   Set to zero
CCC
      iercod = 0
      pi = acos(-1.0)
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
CCC    Check helix and z=const plane intersect
CCC
      dz      = z - z0
      deltat  = eq*dz/sl/r
      td      = t0-deltat
 100  continue
      if(td.lt.0.0) then
         td = td + 2. * pi
         goto 100
      endif
      td = mod(td, 2.0*pi)
      x_int   = xc + r*cos(td)
      y_int   = yc + r*sin(td)
      phi_int = atan2(y_int,x_int)
      r_int   = sqrt(x_int*x_int+y_int*y_int)

      dsx       =  eq*r*sin(td)
      dsy       = -eq*r*cos(td)
      dsz       = r*sl
      ddd       = sqrt(dsx*dsx + dsy*dsy + dsz*dsz)
      dirvec(1) = dsx/ddd
      dirvec(2) = dsy/ddd
      dirvec(3) = dsz/ddd
CCC
CCC   That's it
CCC
      END
