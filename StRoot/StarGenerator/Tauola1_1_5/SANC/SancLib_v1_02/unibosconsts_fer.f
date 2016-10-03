      subroutine UniBosConsts_Fer ()
      implicit none!
      include "s2n_declare.h"

      integer i
      real*8 nm,em,um,dm,hm,zm,wm,qn,qe,qu,qd
      real*8 vn,ve,vu,vd,an,ae,au,ad !,cfl,cfq
      real*8 za1fera,zz1fera,zh1fera,zw1fera,zmsqrfera
      real*8 zmzzzfera,zmwzwfera,zmhzhfera,deltarhofera,tadfera

      real*8 lnmunm,lnmuem,lnmuum,lnmudm,lnmuwm,lnmuzm,lnmuhm
      real*8 vpan,vman,vpae,vmae,vpau,vmau,vpad,vmad
      real*8 rcm2,rnm2,rem2,rum2,rdm2
      real*8 a0fn,a0fe,a0fu,a0fd
      real*8 b0pzee,b0pzuu,b0pzdd,b0phee,b0phuu,b0phdd,b0pwud
      real*8 b0pznn,b0phnn,b0pwne
      real*8 b0fhee,b0fhuu,b0fhdd,b0fzee
      real*8 b0fzuu,b0fzdd,b0fwud
      real*8 b0fznn,b0fhnn,b0fwne
      complex*16 nm2,em2,um2,dm2

*                       Za1Fer Zz1Fer Zh1Fer Zw1Fer ZmsqrFer 
      common/unibos_fer/za1fer,zz1fer,zh1fer,zw1fer,zmsqrfer,
*      ZmzZzFer ZmwZwFer ZmhZhFer DeltarhoFer TadFer
     & zmzzzfer,zmwzwfer,zmhzhfer,deltarhofer,tadfer

      za1fera = 0d0
      zz1fera = 0d0
      zh1fera = 0d0
      zw1fera = 0d0
      zmsqrfera = 0d0
      zmzzzfera = 0d0
      zmwzwfera = 0d0
      zmhzhfera = 0d0
      deltarhofera = 0d0
      tadfera = 0d0

      do i=1,3
*-- 1,3,5
      nm = rmf1(1+2*(i-1))
*-- 2,4,6
      em = rmf1(2+2*(i-1))
*-- 7,9,11
      um = rmf1(7+2*(i-1))
*-- 8,10,12
      dm = rmf1(8+2*(i-1))
*
*-- 1,3,5
      qn = qf(1+2*(i-1))
*-- 2,4,6
      qe = qf(2+2*(i-1))
*-- 7,9,11
      qu = qf(7+2*(i-1))
*-- 8,10,12
      qd = qf(8+2*(i-1))
*
*-- 1,3,5
      an = i3f(1+2*(i-1))
*-- 2,4,6
      ae = i3f(2+2*(i-1))
*-- 7,9,11
      au = i3f(7+2*(i-1))
*-- 8,10,12
      ad = i3f(8+2*(i-1))

      vn = 1d0/2
      ve =-1d0/2-2d0*qe*stw2
      vu = 1d0/2-2d0*qu*stw2
      vd =-1d0/2-2d0*qd*stw2

      vpan=vn+an
      vman=vn-an
      vpae=ve+ae
      vmae=ve-ae
      vpau=vu+au
      vmau=vu-au
      vpad=vd+ad
      vmad=vd-ad
      
      rnm2=nm**2
      rem2=em**2
      rum2=um**2
      rdm2=dm**2 

      nm2 = dcmplx(rnm2,-1d-30)
      em2 = dcmplx(rem2,-1d-30)
      um2 = dcmplx(rum2,-1d-30)
      dm2 = dcmplx(rdm2,-1d-30)
 
      lnmunm = log(rnm2/thmu2)
      lnmuem = log(rem2/thmu2)
      lnmuum = log(rum2/thmu2)      
      lnmudm = log(rdm2/thmu2)
      lnmuwm = log(rwm2/thmu2)
      lnmuzm = log(rzm2/thmu2)
      lnmuhm = log(rhm2/thmu2)

      a0fe = lnmuem - 1d0
      a0fu = lnmuum - 1d0
      a0fd = lnmudm - 1d0
      a0fn = lnmunm - 1d0

      b0pznn = dreal(b0p(-rzm2,nm2,nm2))
*      print *, "b0pznn",b0pznn
      b0pzee = dreal(b0p(-rzm2,em2,em2))
*      print *, "b0pzee",b0pzee
      b0pzuu = dreal(b0p(-rzm2,um2,um2))
*      print *, "b0pzuu",b0pzuu
      b0pzdd = dreal(b0p(-rzm2,dm2,dm2))
*      print *, "b0pzdd",b0pzdd
      b0phnn = dreal(b0p(-rhm2,nm2,nm2))
*      print *, "b0phnn",b0phnn
      b0phee = dreal(b0p(-rhm2,em2,em2))
*      print *, "b0phee",b0phee
      b0phuu = dreal(b0p(-rhm2,um2,um2))
*      print *, "b0phuu",b0phuu
      b0phdd = dreal(b0p(-rhm2,dm2,dm2))
*      print *, "b0phdd",b0phdd
      b0pwne = dreal(b0p(-rwm2,nm2,em2))
*      print *, "b0pwne",b0pwne
      b0pwud = dreal(b0p(-rwm2,um2,dm2))
*      print *, "b0pwud",b0pwud

      b0fznn = dreal(b0f(-rzm2,thmu2,nm2,nm2))
      b0fzee = dreal(b0f(-rzm2,thmu2,em2,em2))
      b0fzuu = dreal(b0f(-rzm2,thmu2,um2,um2))
      b0fzdd = dreal(b0f(-rzm2,thmu2,dm2,dm2))
      b0fhnn = dreal(b0f(-rhm2,thmu2,nm2,nm2))
      b0fhee = dreal(b0f(-rhm2,thmu2,em2,em2))
      b0fhuu = dreal(b0f(-rhm2,thmu2,um2,um2))
      b0fhdd = dreal(b0f(-rhm2,thmu2,dm2,dm2))
      b0fwne = dreal(b0f(-rwm2,thmu2,nm2,em2))
      b0fwud = dreal(b0f(-rwm2,thmu2,um2,dm2))

      za1fer =
     &     +cfl*4d0/3d0*stw2*qe**2*(1d0+a0fe)
     &     +cfq*4d0/3d0*stw2*qu**2*(1d0+a0fu)
     &     +cfq*4d0/3d0*stw2*qd**2*(1d0+a0fd)

      zz1fer =
     &     +cfl/ctw2/6d0*((vpan**2+vman**2+vpae**2+vmae**2)/3
     &     +b0pznn*(6*vpan*vman*rnm2+(vpan**2+vman**2)*(rzm2-rnm2))
     &     +b0pzee*(6*vpae*vmae*rem2+(vpae**2+vmae**2)*(rzm2-rem2))
     &     -b0fznn*(vpan**2+vman**2)
     &     -b0fzee*(vpae**2+vmae**2))
     &     +cfq/ctw2/6d0*((vpau**2+vmau**2+vpad**2+vmad**2)/3
     &     +b0pzuu*(6*vpau*vmau*rum2+(vpau**2+vmau**2)*(rzm2-rum2))
     &     +b0pzdd*(6*vpad*vmad*rdm2+(vpad**2+vmad**2)*(rzm2-rdm2))
     &     -b0fzuu*(vpau**2+vmau**2)
     &     -b0fzdd*(vpad**2+vmad**2))

      zh1fer =
     &     +cfl/2d0*rnm2/rwm2*(+b0phnn*(rhm2-4d0*rnm2)-b0fhnn)
     &     +cfl/2d0*rem2/rwm2*(+b0phee*(rhm2-4d0*rem2)-b0fhee)
     &     +cfq/2d0*rum2/rwm2*(+b0phuu*(rhm2-4d0*rum2)-b0fhuu)
     &     +cfq/2d0*rdm2/rwm2*(+b0phdd*(rhm2-4d0*rdm2)-b0fhdd)

      zw1fer =
     &     +cfl/6d0*(
     &     +2d0/3
     &     +b0pwne*rwm2*(2d0-(rnm2+rem2)/rwm2-(rnm2-rem2)**2/rwm2**2)
     &     -b0fwne*(2d0+(rnm2-rem2)**2/rwm2**2)
     &     -a0fn*rnm2*(rnm2-rem2)/rwm2**2
     &     -a0fe*rem2*(rem2-rnm2)/rwm2**2)
     &     +cfq/6d0*(
     &     +2d0/3
     &     +b0pwud*rwm2*(2d0-(rum2+rdm2)/rwm2-(rum2-rdm2)**2/rwm2**2)
     &     -b0fwud*(2d0+(rum2-rdm2)**2/rwm2**2)
     &     -a0fu*rum2*(rum2-rdm2)/rwm2**2-a0fd*rdm2*(rdm2-rum2)/rwm2**2)

      zmsqrfer =
     &     +cfl*stw/ctw*2d0/3d0*qe*ve*(-1d0/3d0+2d0*rem2/rzm2
     &     +b0fzee*(1d0+2d0*rem2/rzm2)+2d0*a0fe*rem2/rzm2)
     &     +cfq*stw/ctw*2d0/3d0*qu*vu*(-1d0/3d0+2d0*rum2/rzm2
     &     +b0fzuu*(1d0+2d0*rum2/rzm2)+2d0*a0fu*rum2/rzm2)
     &     +cfq*stw/ctw*2d0/3d0*qd*vd*(-1d0/3d0+2d0*rdm2/rzm2
     &     +b0fzdd*(1d0+2d0*rdm2/rzm2)+2d0*a0fd*rdm2/rzm2)

      zmzzzfer = 
     &     +cfl/ctw2/6d0*(
     &     +(vpan**2+vman**2)*(-1d0/3d0+2d0*rnm2/rzm2)
     &     +(vpae**2+vmae**2)*(-1d0/3d0+2d0*rem2/rzm2)
     &     +b0fznn*((vpan**2+vman**2)*(1d0-rnm2/rzm2)
     &     +6d0*vpan*vman*rnm2/rzm2)
     &     +b0fzee*((vpae**2+vmae**2)*(1d0-rem2/rzm2)
     &     +6d0*vpae*vmae*rem2/rzm2)
     &     +2d0*a0fn*(vpan**2+vman**2)*rnm2/rzm2
     &     +2d0*a0fe*(vpae**2+vmae**2)*rem2/rzm2)
     &     +cfq/ctw2/6d0*
     &     (
     &     +(vpau**2+vmau**2)*(-1d0/3d0+2d0*rum2/rzm2)
     &     +(vpad**2+vmad**2)*(-1d0/3d0+2d0*rdm2/rzm2)
     &     +b0fzuu*((vpau**2+vmau**2)*(1d0-rum2/rzm2)
     &     +6d0*vpau*vmau*rum2/rzm2)
     &     +b0fzdd*((vpad**2+vmad**2)*(1d0-rdm2/rzm2)
     &     +6d0*vpad*vmad*rdm2/rzm2)
     &     +2d0*a0fu*(vpau**2+vmau**2)*rum2/rzm2
     &     +2d0*a0fd*(vpad**2+vmad**2)*rdm2/rzm2
     &     )

      zmwzwfer =
     &     +cfl/6d0*(-2d0/3d0+2d0*rnm2/rwm2+2d0*rem2/rwm2
     &     +b0fwne*(2d0-(rnm2+rem2)/rwm2-(rnm2-rem2)**2/rwm2**2)
     &     +a0fn*(2d0-(rnm2-rem2)/rwm2)*rnm2/rwm2
     &     +a0fe*(2d0-(rem2-rnm2)/rwm2)*rem2/rwm2)
     &     +cfq/6d0*(-2d0/3d0+2d0*(rum2+rdm2)/rwm2
     &     +b0fwud*(2d0-(rum2+rdm2)/rwm2-(rum2-rdm2)**2/rwm2**2)
     &     +a0fu*(2d0-(rum2-rdm2)/rwm2)*rum2/rwm2
     &     +a0fd*(2d0-(rdm2-rum2)/rwm2)*rdm2/rwm2)

      zmhzhfer =
     &     +cfl/2d0*(
     &     +b0fhnn*(1d0-4d0*rnm2/rhm2)*rnm2/rwm2
     &     +b0fhee*(1d0-4d0*rem2/rhm2)*rem2/rwm2)
     &     +cfq/2d0*(
     &     +b0fhuu*(1d0-4d0*rum2/rhm2)*rum2/rwm2
     &     +b0fhdd*(1d0-4d0*rdm2/rhm2)*rdm2/rwm2)
      
      deltarhofer = zmwzwfer-zmzzzfer

      tadfer =
     &     -2d0*(
     &     +cfl*(a0fn*nm**4+a0fe*em**4)
     &     +cfq*(a0fu*um**4+a0fd*dm**4)
     &     )/rwm2/rhm2

      za1fera = za1fera + za1fer
      zz1fera = zz1fera + zz1fer
      zh1fera = zh1fera + zh1fer
      zw1fera = zw1fera + zw1fer
      zmsqrfera = zmsqrfera + zmsqrfer
      zmzzzfera = zmzzzfera + zmzzzfer
      zmwzwfera = zmwzwfera + zmwzwfer
      zmhzhfera = zmhzhfera + zmhzhfer
      deltarhofera = deltarhofera + deltarhofer
      tadfera = tadfera + tadfer

      enddo

      za1fer = za1fera
      zz1fer = zz1fera
      zh1fer = zh1fera
      zw1fer = zw1fera
      zmsqrfer = zmsqrfera
      zmzzzfer = zmzzzfera
      zmwzwfer = zmwzwfera
      zmhzhfer = zmhzhfera
      deltarhofer = deltarhofera
      tadfer = tadfera

      return
      end
