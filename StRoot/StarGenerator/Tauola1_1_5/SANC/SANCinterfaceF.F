      subroutine 
     $  flagset(iqedx,iewx,ibornx,gfschemex,ifggx,ncx,fcx,tlmu2x)
      implicit none!
#include "s2n_declare.h"
      integer iqedx,iewx,ibornx,gfschemex,ifggx
      real*8 ncx,fcx,tlmu2x
*-- Flag settings 
      iqed     = iqedx
      iew      = iewx
      iborn    = ibornx
      gfscheme = gfschemex
      ifgg     = ifggx
* Input constants (masses, couplings)
#include "leshw_input.h"
      call s2n_init()
      call ffini()
      nc=ncx
      fc=fcx
      tlmu2=tlmu2x
      end

      subroutine paraget(mtax,conhcx,pix)
      implicit none!
      real*8 mtax,conhcx,pix
#include "s2n_declare.h"
      mtax=mta
      conhcx=conhc
      pix= pi
      end

      subroutine PrintConsts (pmg)
      implicit none!
      integer pmg,NOUT
#include "s2n_declare.h"

      if ((pmg.eq.0).or.(pmg.eq.1)) then
         print *
         print *, "flag iqed     = ",iqed
         print *, "flag iew      = ",iew
         print *, "flag qcd      = ",iqcd
         print *, "flag gfscheme = ",gfscheme
         print *, "thmu2(GeV^2)  = ",thmu2
         print *, "tlmu2(GeV^2)  = ",tlmu2
         print *, "mup(GeV)      = ",mup
         print *, "mta(GeV)      = ",mta
       endif
      if ((pmg.eq.0).or.(pmg.eq.2)) then
         print *
         print *, "alphai    = ",alphai
         print *, "alpha     = ",alpha
         print *, "GF(GeV-^2)= ",gf
         print *, "g         = ",g
         print *, "e         = ",e
         print *, "alphas    = ",alpha
         print *, "gs        = ",gs
         print *, "cwm       = ",ctw
         print *, "swm2      = ",stw2
         print *, "mh(GeV)   = ",mh
         print *, "mw(GeV)   = ",mw
         print *, "mz(GeV)   = ",mz
         print *, "wz(GeV)   = ",wz
         print *, "ww(GeV)   = ",ww
         print *, "wh(GeV)   = ",wh
         print *, "wtp(GeV)  = ",wtp
      endif
      
      NOUT=11
      OPEN(NOUT   ,file='var.dump')
         write(NOUT,*) "TAUOLA interace: SANC input parameters:"
      if ((pmg.eq.0).or.(pmg.eq.1)) then
         write(NOUT,*) " "
         write(NOUT,*) "flag iqed     = ",iqed
         write(NOUT,*) "flag iew      = ",iew
         write(NOUT,*) "flag qcd      = ",iqcd
         write(NOUT,*) "flag gfscheme = ",gfscheme
         write(NOUT,*) "thmu2(GeV^2)  = ",thmu2
         write(NOUT,*) "tlmu2(GeV^2)  = ",tlmu2
         write(NOUT,*) "mup(GeV)      = ",mup
         write(NOUT,*) "mta(GeV)      = ",mta
       endif
      if ((pmg.eq.0).or.(pmg.eq.2)) then
         write(NOUT,*) " "
         write(NOUT,*) "alphai    = ",alphai
         write(NOUT,*) "alpha     = ",alpha
         write(NOUT,*) "GF(GeV-^2)= ",gf
         write(NOUT,*) "g         = ",g
         write(NOUT,*) "e         = ",e
         write(NOUT,*) "alphas    = ",alpha
         write(NOUT,*) "gs        = ",gs
         write(NOUT,*) "cwm       = ",ctw
         write(NOUT,*) "swm2      = ",stw2
         write(NOUT,*) "mh(GeV)   = ",mh
         write(NOUT,*) "mw(GeV)   = ",mw
         write(NOUT,*) "mz(GeV)   = ",mz
         write(NOUT,*) "wz(GeV)   = ",wz
         write(NOUT,*) "ww(GeV)   = ",ww
         write(NOUT,*) "wh(GeV)   = ",wh
         write(NOUT,*) "wtp(GeV)  = ",wtp
      endif
 
      CLOSE(NOUT)

      return
      end
