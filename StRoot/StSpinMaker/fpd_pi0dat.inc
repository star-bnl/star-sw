      real savpar
      common/svsmd1/savpar(2,2,10)
      real savpar2
      integer npeak
      common/svsmd2/savpar2(2,10),npeak(2)
*
      integer ntowx,ntowy
      parameter (ntowx=4)
      parameter (ntowy=3)
*
      real esum
      real towe,tmaxe
      integer nrmax,ncmax
      real p1,p2
      real finv2
      real SMXp1sum,SMXp2sum
      real SMXp1cnt,SMXp2cnt
      real SMXp1wid,SMXp2wid
      real SMYp1sum,SMYp2sum
      real SMYp1cnt,SMYp2cnt
      real SMYp1wid,SMYp2wid
      real SMXqfit,SMYqfit

      real timdif
      real pmagpi,thpi,etapi,phipi
*
      common/pi0dat/
     $     esum,                   ! tower summed pulse height scaled to GeV
     $     towe(ntowx,ntowy),      ! tower pulse height scaled to GeV
     $     tmaxe,nrmax,ncmax,      ! HT energy, row and column number
     $     p1(4),p2(4),            ! photon four momenta
     $     finv2,                  ! invariant mass squared
     $     pmagpi,thpi,etapi,phipi,! reconstruction kinematics
     $     SMXp1sum,SMXp2sum,      ! integral of SMX peaks
     $     SMXp1cnt,SMXp2cnt,      ! centroid of SMX peaks
     $     SMXp1wid,SMXp2wid,      ! FWHM of SMX peaks
     $     SMYp1sum,SMYp2sum,      ! integral of SMY peaks
     $     SMYp1cnt,SMYp2cnt,      ! centroid of SMY peaks
     $     SMYp1wid,SMYp2wid,      ! FWHM of SMY peaks
     $     SMXqfit,SMYqfit,        ! fit qualities to SMX,SMY
     $     timdif                  ! BBC time difference east-west

