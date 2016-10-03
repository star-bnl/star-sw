      subroutine QedPhotRenProp(qs)
      implicit none !
      include 's2n_declare.h'
*
      real*8 a0fel,a0fmo,a0fta
      complex*16 b0fqselel,b0fqsmomo,b0fqstata
*      complex*16 pigglep,za1lep,piggren
*
      common /qedphotren/pigglep,za1lep,piggren
*
      a0fel = dlog(relm2/thmu2) - 1d0
      a0fmo = dlog(rmom2/thmu2) - 1d0
      a0fta = dlog(rtam2/thmu2) - 1d0
*
      b0fqselel = b0f(qs,thmu2,elm2,elm2)
      b0fqsmomo = b0f(qs,thmu2,mom2,mom2)
      b0fqstata = b0f(qs,thmu2,tam2,tam2)
*
      pigglep=
     & +qel**2*(4d0/9+8d0/3/qs*relm2+8d0/3*a0fel/qs*relm2-4d0/3*b0fqsele
     & l+8d0/3*b0fqselel/qs*relm2)
     & +qmo**2*(4d0/9+8d0/3/qs*rmom2+8d0/3*a0fmo/qs*rmom2-4d0/3*b0fqsmom
     & o+8d0/3*b0fqsmomo/qs*rmom2)
     & +qta**2*(4d0/9+8d0/3/qs*rtam2+8d0/3*a0fta/qs*rtam2-4d0/3*b0fqstat
     & a+8d0/3*b0fqstata/qs*rtam2)
*
      za1lep=
     & +qel**2*(4d0/3+4d0/3*a0fel)
     & +qmo**2*(4d0/3+4d0/3*a0fmo)
     & +qta**2*(4d0/3+4d0/3*a0fta)
      piggren=
     & -za1lep+pigglep
*
      return
      end
