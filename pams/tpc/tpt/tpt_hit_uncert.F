      subroutine tpt_hit_uncert(z,alpha,lambda,ipadrow,sigX,sigZ)
      implicit none
      real tpt_resolution_parameterization
*
* Based on a global fit of residual widths as function of
* alpha, lambda, and z (200-drift), but NOT on Q, this returns
* the uncertainty in hit position along padrow, LOCAL-X and

* in time direction, z
*
* Input:
*  alpha - crossing angle with respect to padrow          [degrees]
*  lambda - crossing angle with respect to time direction [degrees]
*  z     - z position  (0.0 is central membrane)          [cm]
*  ipadrow - 1..45
*
* Output:
*  sigX  - uncertainty along padrow direction [cm]
*  sigZ  - uncertainty along time direction   [cm]
*
* Mike Lisa, July 2000
*
      integer ipadrow,jj
      real z,alpha,lambda,sigX,sigZ,dlen
      real resolution_parameterization

      dlen = abs(200.0 - abs(z))

      if (ipadrow.lt.14) then
         jj=1
      else
         jj=2
      endif

      sigX = tpt_resolution_parameterization(jj,1,dlen,alpha)
      sigZ = tpt_resolution_parameterization(jj,2,dlen,lambda)


      return
      end


c--------------------------------------------------------------------------
      real function 
     +     tpt_resolution_parameterization(io,ixz,dlen,angle)
      implicit none
*
* returns local-X or Z resolution [cm], based on parameterization
*
* Input:
*  io = 1 for inner sector, 2 for outer sector
*  ixz         = 1 for localX, for for Z
*  dlen        = drift length
*  angle       = alpha (for localX resolution) or lambda (for Z resolution)
*
*
      integer ixz,io
      real dlen,angle

      real xval(2,2,7)          ! index1 = 1,2 for inner,outer sector
                                ! index2 = 1,2 for x,z resolution
                                ! index3 = parameter#


      real sig2_intrinsic,sig2_drift,sig2_tan

      logical first/.true./
      save


c===================initialize...=====================
      if (first) then
         first=.false.
c$$$c----------  Inner padrow X-resolution:------------
c$$$         xval(1,1,1) =   0.27297E-03
c$$$         xval(1,1,2) =   0.72269E-04
c$$$         xval(1,1,3) =   0.21579E-01
c$$$         xval(1,1,4) =   0.0
c$$$         xval(1,1,5) =   0.0
c$$$         xval(1,1,6) =   0.0
c$$$         xval(1,1,7) =   200.0
c$$$c----------  Outer padrow X-resolution:------------
c$$$         xval(2,1,1) =   0.12063E-02
c$$$         xval(2,1,2) =   0.42314E-04
c$$$         xval(2,1,3) =   0.37258E-01
c$$$         xval(2,1,4) =   0.11146E-01
c$$$         xval(2,1,5) =   0.47545E-04
c$$$         xval(2,1,6) =   0.15743E-01
c$$$         xval(2,1,7) =   35.0
c$$$c----------  Inner padrow Z-resolution:------------
c$$$         xval(1,2,1) =   0.35521E-02
c$$$         xval(1,2,2) =   0.15138E-03
c$$$
c$$$         xval(1,2,3) =   0.20967E-01
c$$$         xval(1,2,4) =   0.0
c$$$         xval(1,2,5) =   0.0
c$$$         xval(1,2,6) =   0.0
c$$$         xval(1,2,7) =   200.0
c$$$c----------  Outer padrow Z-resolution:------------
c$$$         xval(2,2,1) =   0.54256E-02
c$$$         xval(2,2,2) =   0.80906E-04
c$$$         xval(2,2,3) =   0.72093E-01
c$$$         xval(2,2,4) =   0.11630E-01
c$$$         xval(2,2,5) =   0.97988E-04
c$$$         xval(2,2,6) =   0.31120E-01
c$$$         xval(2,2,7) =   20.0
c$$$      endif
c----------  Inner sector X-resolution:------------
         xval(1,1,1) =   1.68243E-03
         xval(1,1,2) =   5.23272E-05
         xval(1,1,3) =   5.75341E-02
         xval(1,1,4) =   0.0
         xval(1,1,5) =   0.0
         xval(1,1,6) =   0.0
         xval(1,1,7) =   0.0
c----------  Outer sector X-resolution:------------
         xval(2,1,1) =   2.02775E-04
         xval(2,1,2) =   3.55219E-05
         xval(2,1,3) =   6.45610E-02
         xval(2,1,4) =   0.0
         xval(2,1,5) =   0.0
         xval(2,1,6) =   0.0
         xval(2,1,7) =   0.0
c----------  Inner sector Z-resolution:------------
         xval(1,2,1) =   3.12735E-03
         xval(1,2,2) =   1.51055E-04
         xval(1,2,3) =   2.43806E-02
         xval(1,2,4) =   0.0
         xval(1,2,5) =   0.0
         xval(1,2,6) =   0.0
         xval(1,2,7) =   0.0
c----------  Outer sector Z-resolution:------------
         xval(2,2,1) =   8.15800E-03
         xval(2,2,2) =   5.69582E-05
         xval(2,2,3) =   4.48440E-02
         xval(2,2,4) =   0.0
         xval(2,2,5) =   0.0
         xval(2,2,6) =   0.0
         xval(2,2,7) =   0.0
      endif
c=====================================================


c calculate resolution:
c$$$      if (abs(angle).lt.xval(io,ixz,7)) then
c$$$         sig2_intrinsic = xval(io,ixz,1)
c$$$         sig2_drift     = xval(io,ixz,2)*dlen/(cos(angle/57.2958)**2)
c$$$         sig2_tan       = xval(io,ixz,3)*(tan(angle/57.2958))**2
c$$$      else
c$$$         sig2_intrinsic = xval(io,ixz,4)
c$$$         sig2_drift     = xval(io,ixz,5)*dlen/(cos(angle/57.2958)**2)
c$$$         sig2_tan       = xval(io,ixz,6)*(tan(angle/57.2958))**2
c$$$      endif

      sig2_intrinsic = xval(io,ixz,1)
      sig2_drift     = xval(io,ixz,2)*dlen/(cos(angle/57.2958)**2)
      sig2_tan       = xval(io,ixz,3)*(tan(angle/57.2958))**2

      tpt_resolution_parameterization = sqrt(sig2_intrinsic+
     +     sig2_drift+sig2_tan)

      end
