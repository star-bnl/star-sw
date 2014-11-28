c
c
c     File:- evt_ctrl.inc
c     ===================
c
      integer hijet,venus,frtiof,hijng,frtiof17
      parameter (hijet=1)
      parameter (venus=2)
      parameter (frtiof=3)
      parameter (hijng=4)
      parameter (frtiof17=5)

      integer   evtcod,iextra
      real      rextra,evtver
      common    /egctrl/ evtcod,evtver,rextra(50),iextra(50)
