
*  SMD strip definitions


      integer nxudmax          ! now defined in fpd_com.inc
      parameter(nxudmax=30)
      integer nyudmax
      parameter(nyudmax=50)
      integer mstrp

      integer*2 smd_x1,smd_x2  ! up-(down-) strips (local)
      integer*2 smd_y1,smd_y2  ! up-(down-) strips (local)

      real smxlo,smxhi         ! fiducial volume cuts on SMX plane
      real smylo,smyhi         ! fiducial volume cuts on SMY plane

      common/fpdsmd/
     $     smxlo,smxhi,
     $     smylo,smyhi,
     $     smd_x1(nxudmax),
     $     smd_x2(nxudmax),
     $     smd_y1(nyudmax),
     $     smd_y2(nyudmax)
