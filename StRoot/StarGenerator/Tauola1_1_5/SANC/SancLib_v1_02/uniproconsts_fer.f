      subroutine UniProConsts_Fer (qs)
      implicit none !
      include "s2n_declare.h"

      integer i
      real*8 nm,em,um,dm,hm,zm,wm,qn,qe,qu,qd
      real*8 vn,ve,vu,vd,an,ae,au,ad
      real*8 lnmunm,lnmuem,lnmuum,lnmudm,lnmuwm,lnmuzm,lnmuhm
      real*8 vpan,vman,vpae,vmae,vpau,vmau,vpad,vmad
      real*8 rcm2,rnm2,rem2,rum2,rdm2
      real*8 a0fn,a0fe,a0fu,a0fd,a0fh,a0fz,a0fw
      real*8 caldzfera,caldwfera
      complex*16 nm2,em2,um2,dm2
      complex*16 b0fqsee,b0fqsuu,b0fqsdd,b0fqscc,b0fqsce,b0fqsud
      complex*16 b0fqsee1,b0fqsuu1,b0fqsdd1
      complex*16 pizgfera,piggfera

      common/unibos_fer/za1fer,zz1fer,zh1fer,zw1fer,zmsqrfer,
     & zmzzzfer,zmwzwfer,zmhzhfer,deltarhofer,tadfer
*                       CalDzFer calDwFer PIZgFer PIggFer
      common/unipro_fer/caldzfer,caldwfer,pizgfer,piggfer

      caldzfer  = 0d0
      caldwfer  = 0d0
      pizgfer   = dcmplx(0d0,0d0)
      piggfer   = dcmplx(0d0,0d0)
      caldzfera = 0d0
      caldwfera = 0d0
      pizgfera  = dcmplx(0d0,0d0) 
      piggfera  = dcmplx(0d0,0d0)

      do i=1,3
*-- 1,3,5
      nm = rmf1(1+2*(i-1))
*-- 2,4,6
      em = rmf1(2+2*(i-1))
*-- 7,9,11
      um = rmf1(7+2*(i-1))
*-- 8,10,12
      dm = rmf1(8+2*(i-1))

*-- 1,3,5
      qn = qf(1+2*(i-1))
*-- 2,4,6
      qe = qf(2+2*(i-1))
*-- 7,9,11
      qu = qf(7+2*(i-1))
*-- 8,10,12
      qd = qf(8+2*(i-1))

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

      a0fn = lnmunm - 1d0
      a0fe = lnmuem - 1d0
      a0fu = lnmuum - 1d0
      a0fd = lnmudm - 1d0
      a0fw = lnmuwm - 1d0
      a0fz = lnmuzm - 1d0
      a0fh = lnmuhm - 1d0

      b0fqsee = dreal(b0f(qs,thmu2,em2,em2))
      b0fqscc = dreal(b0f(qs,thmu2,cm2,cm2))
      b0fqsuu = dreal(b0f(qs,thmu2,um2,um2))
      b0fqsdd = dreal(b0f(qs,thmu2,dm2,dm2))
      b0fqsce = dreal(b0f(qs,thmu2,cm2,em2))
      b0fqsud = dreal(b0f(qs,thmu2,um2,dm2))

      b0fqsee1 = b0f(qs,thmu2,em2,em2)
      b0fqsuu1 = b0f(qs,thmu2,um2,um2)
      b0fqsdd1 = b0f(qs,thmu2,dm2,dm2)

      caldzfer =
     & +1d0/ctw**2/(rzm2+qs)*cfl*(
     &  +1d0/9*vn**2*qs
     &  +2d0/3*ve**2*rem2
     &  +1d0/9*ve**2*qs
     &  +1d0/9*an**2*qs
     &  +2d0/3*ae**2*rem2
     &  +1d0/9*ae**2*qs
     &  +2d0/3*a0fe*ve**2*rem2
     &  +2d0/3*a0fe*ae**2*rem2
     &  -2*a0fe/rhm2*rem2**2
     &  +2d0/3*b0fqsee*ve**2*rem2
     &  -1d0/3*b0fqsee*ve**2*qs
     &  -4d0/3*b0fqsee*ae**2*rem2
     &  -1d0/3*b0fqsee*ae**2*qs
     &  -1d0/3*b0fqscc*vn**2*qs
     &  -1d0/3*b0fqscc*an**2*qs)
     & +1d0/ctw**2/(rzm2+qs)*cfq*(
     &   2d0/3*vu**2*rum2
     &  +1d0/9*vu**2*qs
     &  +2d0/3*vd**2*rdm2
     &  +1d0/9*vd**2*qs
     &  +2d0/3*au**2*rum2
     &  +1d0/9*au**2*qs
     &  +2d0/3*ad**2*rdm2
     &  +1d0/9*ad**2*qs
     &  +2d0/3*a0fu*vu**2*rum2
     &  +2d0/3*a0fu*au**2*rum2
     &  -2*a0fu/rhm2*rum2**2
     &  +2d0/3*a0fd*vd**2*rdm2
     &  +2d0/3*a0fd*ad**2*rdm2
     &  -2*a0fd/rhm2*rdm2**2
     &  +2d0/3*b0fqsuu*vu**2*rum2
     &  -1d0/3*b0fqsuu*vu**2*qs
     &  -4d0/3*b0fqsuu*au**2*rum2
     &  -1d0/3*b0fqsuu*au**2*qs
     &  +2d0/3*b0fqsdd*vd**2*rdm2
     &  -1d0/3*b0fqsdd*vd**2*qs
     &  -4d0/3*b0fqsdd*ad**2*rdm2
     &  -1d0/3*b0fqsdd*ad**2*qs)

      caldwfer =
     & +1d0/(qs+rwm2)*cfl*(
     &  -1d0/9*rwm2
     &  +1d0/3*rem2
     &  +1d0/6*a0fe/qs*rem2**2
     &  -2*a0fe/rhm2*rem2**2
     &  +1d0/3*a0fe*rem2
     &  +1d0/6*b0fqsce/qs*rem2**2
     &  +1d0/3*b0fqsce*rwm2
     &  -1d0/6*b0fqsce*rem2)
     & +1d0/(qs+rwm2)*cfq*(
     &  -1d0/9*rwm2
     &  +1d0/3*rum2
     &  +1d0/3*rdm2
     &  -1d0/6*a0fu/qs*rum2*rdm2
     &  +1d0/6*a0fu/qs*rum2**2
     &  -2*a0fu/rhm2*rum2**2
     &  +1d0/3*a0fu*rum2
     &  -1d0/6*a0fd/qs*rum2*rdm2
     &  +1d0/6*a0fd/qs*rdm2**2
     &  -2*a0fd/rhm2*rdm2**2
     &  +1d0/3*a0fd*rdm2
     &  -1d0/3*b0fqsud/qs*rum2*rdm2
     &  +1d0/6*b0fqsud/qs*rum2**2
     &  +1d0/6*b0fqsud/qs*rdm2**2
     &  +1d0/3*b0fqsud*rwm2
     &  -1d0/6*b0fqsud*rum2
     &  -1d0/6*b0fqsud*rdm2)
     & +cfl*(1d0/9-1d0/3*b0fqsce)
     & +cfq*(1d0/9-1d0/3*b0fqsud)

      pizgfer =
     & +cfl*(
     &  +4d0/3*qe*ve*rem2/qs
     &  +2d0/9*qe*ve
     &  +4d0/3*a0fe*qe*ve*rem2/qs
     &  +4d0/3*b0fqsee1*qe*ve*rem2/qs
     &  -2d0/3*b0fqsee1*qe*ve)
     & +cfq*(
     &  +4d0/3*qu*vu*rum2/qs
     &  +2d0/9*qu*vu
     &  +4d0/3*qd*vd*rdm2/qs
     &  +2d0/9*qd*vd
     &  +4d0/3*a0fu*qu*vu*rum2/qs
     &  +4d0/3*a0fd*qd*vd*rdm2/qs
     &  +4d0/3*b0fqsuu1*qu*vu*rum2/qs
     &  -2d0/3*b0fqsuu1*qu*vu
     &  +4d0/3*b0fqsdd1*qd*vd*rdm2/qs
     &  -2d0/3*b0fqsdd1*qd*vd)

      piggfer =
     &  +cfl*(
     &        +8d0/3d0*qe**2*rem2/qs
     &        +4d0/9d0*qe**2
     &        +8d0/3d0*a0fe*qe**2*rem2/qs
     &        +8d0/3d0*b0fqsee1*qe**2*rem2/qs
     &        -4d0/3d0*b0fqsee1*qe**2)
     &  +cfq*(
     &        +8d0/3d0*qu**2*rum2/qs
     &        +4d0/9d0*qu**2
     &        +8d0/3d0*qd**2*rdm2/qs
     &        +4d0/9d0*qd**2
     &        +8d0/3d0*a0fu*qu**2*rum2/qs
     &        +8d0/3d0*a0fd*qd**2*rdm2/qs
     &        +8d0/3d0*b0fqsuu1*qu**2*rum2/qs
     &        -4d0/3d0*b0fqsuu1*qu**2
     &        +8d0/3d0*b0fqsdd1*qd**2*rdm2/qs
     &        -4d0/3d0*b0fqsdd1*qd**2)

      caldzfera = caldzfera + caldzfer
      caldwfera = caldwfera + caldwfer
      pizgfera = pizgfera + pizgfer
      piggfera = piggfera + piggfer

      enddo

      caldzfer = caldzfera-rzm2*(zmzzzfer+tadfer)/(qs+rzm2)
      caldwfer = caldwfera-1d0/(qs+rwm2)*rwm2*(zmwzwfer+tadfer)
      pizgfer = pizgfera
      piggfer = piggfera

      return
      end
