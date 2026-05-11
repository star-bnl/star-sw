c $Id: getspin.f,v 1.3 1999/01/18 09:57:03 ernst Exp $
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      integer function getspin(iityp,itag)
c
c     Revision : 1.0
c
cinput ityp   : ID of particle
cinput itag   : flag for return value
c
c output: $2*J_{tot}$ of particle
c
c     This subroutine converts global ityp to maximum spin and optionally
c     chooses a random projection ({\tt itag=-1}).
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
      implicit none
      include 'comres.f'
c
      integer ityp,iityp,jtot,itag
      real*8 ranf
c
      ityp=abs(iityp)

      jtot=0
      if(ityp.ge.nucleon.and.ityp.le.maxbar) then
         jtot=Jres(ityp)
      elseif(ityp.ge.offmeson.and.ityp.le.maxmeson) then
         jtot=Jmes(ityp)
      else
         write(6,*)'undefined total isospin in getspin:'
         write(6,*)'ityp: ',ityp
         stop
      endif
c
      getspin=0
      if(itag.eq.1) then
         getspin=jtot
      elseif(itag.eq.-1) then
         getspin=jtot-2*int(ranf(0)*(jtot+1))
      else
         write(6,*)'itag-error in getspin.f'
         stop
      endif

      return
      end
