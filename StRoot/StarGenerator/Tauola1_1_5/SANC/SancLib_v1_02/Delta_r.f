*      subroutine delr(rwm2,rzm2,rhm2,thmu2,deltar,dr_bos,dr_fer)
      subroutine delr(deltar,dr_bos,dr_fer)
*---------------------------------------------------------------
      implicit none!
      include "s2n_declare.h"
*-------------------
*      real*8 thmu2,rwm2,rzm2,rhm2
*      real*8 ctw2,stw2,ctw4,lnctw
*      real*8 rhz,rhw,rhz2,rhw2
      real*8 rhz2,rhw2
      real*8 lnmuwm,lnmuzm,lnmuhm,lnctw
      real*8 pg0fer,drfer,drbos,drwfer,drwbos,deltar,dr_bos,dr_fer,dreal
*-
*      complex*16 cz2
      complex*16 b0fwwz,b0fzww,b0fwwh,b0fzzh,b0fww0
      complex*16 swwfer,szzfer,pggfer
*-
*-    constants
*-
      rhz2 = rhz**2
      rhw2 = rhw**2
*-
*-    scales
*-
      lnmuwm = log(rwm2/thmu2) 
      lnmuzm = log(rzm2/thmu2)
      lnmuhm = log(rhm2/thmu2)
      lnctw  = lnmuwm-lnmuzm

*- calculation of \delta r in one-to-one with (6.390)-(6.395) of thebook
*-
      b0fwwz = dreal(b0f(-rwm2,thmu2,wm2,zm2))
      b0fwwh = dreal(b0f(-rwm2,thmu2,wm2,hm2))
      b0fzww = dreal(b0f(-rzm2,thmu2,wm2,wm2))
      b0fzzh = dreal(b0f(-rzm2,thmu2,zm2,hm2))
      b0fww0 = dreal(b0f(-rwm2,thmu2,wm2,cm2))
*-
      pg0fer = dreal(pggfer(0d0))
      drfer  = dreal(swwfer(rwm2)-szzfer(rzm2))/rwm2 
      drwfer = dreal(swwfer(0d0) -swwfer(rwm2))/rwm2
*- formula (6.393)
*     drbos =(1d0/12d0/ctw4+4d0/3d0/ctw2-17d0/3d0-4d0*ctw2)
*    &      *(b0fwwz-ctw2*b0fzww)
*    &      +(1d0-1d0/3d0*rhw+1d0/12d0*rhw2)*b0fwwh
*    &      -(1d0-1d0/3d0*rhz+1d0/12d0*rhz2)/ctw2*b0fzzh       
*    &      +1d0/12d0*stw2*rhw2*(lnrhw-1d0)
*    &      -1d0/12d0*(1d0/ctw4+6d0/ctw2-24d0+rhw)*lnctw
*    &      -1d0/12d0/ctw4-19d0/36d0/ctw2-133d0/18d0+8d0*ctw2
*- representation coded in ff_lib, both are numerically equal (gauge invariance)
      drbos = (1d0/12d0/ctw4+4d0/3d0/ctw2-17d0/3d0-4d0*ctw2)
     &     *(b0fwwz-ctw2*b0fzww)
     &     +(1d0-1d0/3d0*rhw+1d0/12d0*rhw2)*b0fwwh
     &     -(1d0-1d0/3d0*rhz+1d0/12d0*rhz2)/ctw2*b0fzzh-4d0*stw2*b0fww0
     &     -1d0/12d0*(-(1d0/ctw4+6d0/ctw2-24d0+rhw)*lnmuzm
     &     -rhw2*stw2*lnmuhm
     &     +(14d0+1d0/ctw2+16d0*ctw2-48d0*ctw4+rhw)*lnmuwm
     &     +1d0/ctw4+19d0/3d0/ctw2-22d0/3d0+stw2*rhw2)
*-
      drwbos = -(1d0/12d0/ctw4+4d0/3d0/ctw2-17d0/3d0-4d0*ctw2)*b0fwwz
     &     -(1d0-1d0/3d0*rhw+1d0/12d0*rhw2)*b0fwwh
     &     +3d0/4d0*rhw/(1d0-rhw)*(lnmuhm-lnmuwm)
     &     +1d0/4d0*rhw*(1d0-1d0/3d0*rhw)*lnmuhm
     &     +(1d0/4d0-3d0/stw2)*(lnmuwm-lnmuzm)
     &     -(1d0/12d0/ctw4+17d0/12d0/ctw2)*lnmuzm
     &     +(1d0/12d0/ctw2+11d0/2d0+1d0/12d0*rhw-4d0*stw2)*lnmuwm 
     &     +1d0/12d0/ctw4+11d0/8d0/ctw2+139d0/36d0-177d0/24d0*ctw2
     &     +5d0/8d0*ctw4-1d0/12d0*rhw*(7d0/2d0-rhw)
*-
      deltar = (stw2*(-2d0/3-pg0fer)
     &     +ctw2/stw2*(drfer+drbos)+drwfer+drwbos
     &     +11d0/2-5d0/8*ctw2*(1d0+ctw2)+9d0/4d0*ctw2/stw2*lnctw
     &     +(3d0-7d0*ctw2)*lnmuwm)
      dr_bos = (stw2*(-2d0/3)
     &     +ctw2/stw2*drbos+drwbos
     &     +11d0/2-5d0/8*ctw2*(1d0+ctw2)+9d0/4d0*ctw2/stw2*lnctw
     &     +(3d0-7d0*ctw2)*lnmuwm)
      dr_fer = (stw2*(-pg0fer)+ctw2/stw2*drfer+drwfer)
*-
      return
      end
