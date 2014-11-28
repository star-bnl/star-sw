      integer nt
      integer ntdis,ntpho
      integer ntx,nty
      parameter(ntdis=35)
      parameter(ntpho=44)
      parameter(nt=ntpho)

      real*4 denstk,width
      real*4 densdis,widdis
      real*4 denspho,widpho
* make these arrays large enough for all cases
      common/density/ntx,nty,denstk(nt,nt,245,3),width(nt,nt,7,3)
     &              ,densdis(ntdis,ntdis,245,3),widdis(ntdis,ntdis,7,3)
     &              ,denspho(ntpho,ntpho,245,3),widpho(ntpho,ntpho,7,3)
