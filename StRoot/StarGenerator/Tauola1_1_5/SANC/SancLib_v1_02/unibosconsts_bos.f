      subroutine UniBosConsts_Bos ()
      implicit none!
      include "s2n_declare.h"

      real*8 a0fw,a0fz,a0fh
      real*8 b0fzww,b0fzzh,b0fwwz,b0fwwh,b0fhww,b0fhzz,b0fhhh

*                       Za1Bos ZmzZzBos ZmwZwBos ZmhZhBos DeltarhoBos 
      common/unibos_bos/za1bos,zmzzzbos,zmwzwbos,zmhzhbos,deltarhobos

      a0fw = log(rwm2/thmu2) - 1d0
      a0fz = log(rzm2/thmu2) - 1d0
      a0fh = log(rhm2/thmu2) - 1d0

      b0fzww = dreal(b0f(-rzm2,thmu2,wm2,wm2))
      b0fzzh = dreal(b0f(-rzm2,thmu2,zm2,hm2))
      b0fwwz = dreal(b0f(-rwm2,thmu2,wm2,zm2))
      b0fwwh = dreal(b0f(-rwm2,thmu2,wm2,hm2))
      b0fhww = dreal(b0f(-rhm2,thmu2,wm2,wm2))
      b0fhzz = dreal(b0f(-rhm2,thmu2,zm2,zm2))
      b0fhhh = dreal(b0f(-rhm2,thmu2,hm2,hm2))

      za1bos =
     & -7d0/3*stw2-3*a0fw*stw2

      zmzzzbos =
     &     -65d0/9+32d0/3*stw2-4*stw4-1d0/18/ctw2-1d0/6*rhm2/rwm2
     &     +2*rwm2/rhm2+1/ctw2*rzm2/rhm2
     &     +b0fzww*(-25d0/3+41d0/3*stw2-4*stw4+1d0/12/ctw2)
     &     +b0fzzh*(1/ctw2+1d0/12*rhm2**2/rwm2/rzm2-1d0/3*rhm2/rwm2)
     &     +a0fw*(-13d0/2+32d0/3*stw2-4*stw4+3*rwm2/rhm2)
     &     +a0fz*(+3d0/2/ctw2*rzm2/rhm2+1d0/6/ctw2-1d0/12*rhm2/rwm2)
     &     +a0fh*(+1d0/2*rhm2/rwm2+1d0/12*rhm2**2/rwm2/rzm2)

      zmwzwbos =
     &     -64d0/9-4*stw2-1d0/6/ctw2-1d0/6*rhm2/rwm2+2*rwm2/rhm2+
     &     1/ctw2*rzm2/rhm2
     &     +b0fwwz*(-29d0/3+4*stw2+4d0/3/ctw2+1d0/12/ctw4)
     &     +b0fwwh*(1-1d0/3*rhm2/rwm2+1d0/12*rhm2**2/rwm2**2)
     &     +a0fw*(-5+4*stw2-1d0/12/ctw2-1d0/12*rhm2/rwm2+3*rwm2/rhm2)
     &     +a0fz*(-2+2d0/3/ctw2+1d0/12/ctw4+3d0/2/ctw2*rzm2/rhm2)
     &     +a0fh*(+1d0/2*rhm2/rwm2+1d0/12*rhm2**2/rwm2**2)

      zmhzhbos = 
     &     +b0fhww*(-1+1d0/4*rhm2/rwm2+3*rwm2/rhm2)
     &     +b0fhzz*(-1d0/2/ctw2+1d0/8*rhm2/rwm2+3d0/2/ctw2*rzm2/rhm2)
     &     +9d0/8*b0fhhh*rhm2/rwm2
     &     +a0fw*(1d0/2+3*rwm2/rhm2)
     &     +a0fz*(1d0/4/ctw2+3d0/2/ctw2*rzm2/rhm2)
     &     +a0fh*(3d0/4*rhm2/rwm2)

      deltarhobos = zmwzwbos-zmzzzbos

      return
      end
