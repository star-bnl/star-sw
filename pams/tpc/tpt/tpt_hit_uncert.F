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
      integer tpc_hit_error_table
      integer iret,i,j,k
      integer ixz,io
      real dlen,angle
      real errorval 

      real xval(2,2,7)          ! index1 = 1,2 for inner,outer sector
                                ! index2 = 1,2 for x,z resolution
                                ! index3 = parameter#


      real sig2_intrinsic,sig2_drift,sig2_tan

      logical first/.true./
      save


c===================initialize...=====================
      if (first) then
         first=.false.

       do i=1,2
        do j=1,2
         do k=1,3
          iret = tpc_hit_error_table(i,j,k,errorval)
          xval(i,j,k) = errorval
         enddo
        enddo
       enddo 

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
