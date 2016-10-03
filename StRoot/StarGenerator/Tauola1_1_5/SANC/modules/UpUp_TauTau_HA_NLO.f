************************************************************************
* File created on Thu Dec  4 19:14:00 MSK 2008.
************************************************************************
* This is the FORTRAN module which calculates EW NLO helicity Amplitudes
* for the up + up -> ta + ta process.
*
* It is produced by s2n package of SANC (v1.10) Project.
* Copyright (C), SANC Project Team, 2002-2008.
*
* http://sanc.jinr.ru/
* http://pcphsanc.cern.ch/
* E-mail: <sanc@jinr.ru>
************************************************************************
      subroutine UpUp(l1,l2,l3,l4,s,t,u,zoro,har,hai)
      implicit none!
      include 's2n_declare.h'

      integer is,jj,l1,l2,l3,l4,zoro
      real*8 sum,tw(2),sig(2),har,hai
      real*8 deltar,dr_bos,dr_fer
      real*8 kappa,cof,nsig,twcoeff,twl,coeff,slams
      complex*16 czoro, UpUp_TauTau_HA_NLO
      complex*16 ff_qed_ll,ff_qed_lq,ff_qed_ql,ff_qed_qq,ff_qed_ld
      complex*16 ff_qed_qd,ff_ew_ll,ff_ew_lq,ff_ew_ql,ff_ew_qq
      complex*16 ff_ew_ld,ff_qed_gg
      complex*16 ff_ll,ff_lq,ff_ql,ff_qq,ff_ld,ff_qd,ff_gg
      complex*16 ffll,fflq,ffql,ffqq,ffld,ffqd,ffgg

      real*8 sqrtpxstata,isqrtpxstata,pxstata
      real*8 costhta,sinthta
      real*8 kappaz
      complex*16 chizs,chizsc

      cmz2 = mz2
      spr = s
      qs = -s
      ts = -t
      us = -u

      if (dabs(s-mz**2).le.1d-4) s = (mz**2-1d-4)

      betaf = sqrt(1d0-4d0*mta**2/s)
      cosf = (t-u)/s/betaf
      sinf = dsqrt(1d0-cosf**2)
       
      sqrtpxstata = sqrt(abs(s-(mta+mta)**2))
      isqrtpxstata = 1d0/sqrtpxstata
      pxstata = (sqrtpxstata)**2
      costhta = cosf
      sinthta = sinf
      kappaz = gf*rzm2/(sqrt(2d0)*8d0*pi)/alpha
      chizs = 4d0*kappaz*s/(s-cmz2)
      chizsc = dconjg(chizs)
      nsig = pi*alpha**2*conhc/s*betaf*nc/fc

      if (iborn.eq.0) then
         coeff = alpha/4d0/pi/stw2
 
         igzm2 = 1
         call delr(deltar,dr_bos,dr_fer)
         call UniBosConsts_Bos ()
         call UniBosConsts_Fer ()
         call UniProConsts_fer (-s)
         call nc_ff_1313_2020 (-s,-t,-u)

         if ((iew.eq.0).and.(iqed.ge.1)) then
            ff_ll = coeff*ffarray(1,1)
            ff_lq = coeff*ffarray(1,3)
            ff_ql = coeff*ffarray(1,5)
            ff_qq = coeff*ffarray(1,7)
            ff_ld = coeff*ffarray(1,9)
            ff_qd = coeff*ffarray(1,11)
            ff_gg = coeff*ffarray(1,13)
            deltar = 0d0
         elseif ((iew.eq.1).and.(iqed.eq.0)) then
            ff_ll = coeff*ffarray(1,2)
            ff_lq = coeff*ffarray(1,4)
            ff_ql = coeff*ffarray(1,6)
            ff_qq = coeff*ffarray(1,8)
            ff_ld = coeff*ffarray(1,10)
            ff_qd = coeff*ffarray(1,12)
            ff_gg = dcmplx(0d0,0d0)
         elseif ((iew.eq.1).and.(iqed.ge.1)) then
            ff_ll = coeff*(ffarray(1,1)  + ffarray(1,2))
            ff_lq = coeff*(ffarray(1,3)  + ffarray(1,4))
            ff_ql = coeff*(ffarray(1,5)  + ffarray(1,6))
            ff_qq = coeff*(ffarray(1,7)  + ffarray(1,8))
            ff_ld = coeff*(ffarray(1,9)  + ffarray(1,10))
            ff_qd = coeff*(ffarray(1,11) + ffarray(1,12))
            ff_gg = coeff*ffarray(1,13)
         elseif ((iew.eq.0).and.(iqed.eq.0)) then
            ff_ll = dcmplx(0d0,0d0)
            ff_lq = dcmplx(0d0,0d0)
            ff_ql = dcmplx(0d0,0d0)
            ff_qq = dcmplx(0d0,0d0)
            ff_ld = dcmplx(0d0,0d0)
            ff_qd = dcmplx(0d0,0d0)
            ff_gg = dcmplx(0d0,0d0)
            deltar = 0d0
         endif

         if ((gfscheme.eq.1).or.(gfscheme.eq.2)) then
            ff_ll = ff_ll-coeff*deltar
            ff_ql = ff_ql-coeff*deltar
            ff_lq = ff_lq-coeff*deltar
            ff_qq = ff_qq-coeff*deltar
         endif
         if (gfscheme.eq.2) then
            ff_gg = ff_gg-coeff*deltar
         endif
      elseif (iborn.eq.1) then
         ff_ll = dcmplx(0d0,0d0)
         ff_ql = dcmplx(0d0,0d0)
         ff_lq = dcmplx(0d0,0d0)
         ff_qq = dcmplx(0d0,0d0)
         ff_ld = dcmplx(0d0,0d0)
         ff_qd = dcmplx(0d0,0d0)
         ff_gg = dcmplx(0d0,0d0)
      endif      
         
      czoro = dcmplx(zoro,0d0)

      ffll = czoro + ff_ll
      ffql = czoro + ff_ql
      fflq = czoro + ff_lq
      ffqq = czoro + ff_qq
      ffld =         ff_ld
      ffqd =         ff_qd
   
      if (ifgg.eq.-1) then
         ffgg = dcmplx(0d0,0d0)
      elseif (ifgg.eq.0) then
         ffgg = czoro
      elseif (ifgg.eq.1) then
         ffgg = czoro + ff_gg
      elseif ((ifgg.eq.2).and.(gfscheme.ne.2)) then
         ffgg = czoro/(1d0 - ff_gg)
      endif
      
      if (l1.eq.1.and.l2.eq.1.and.l3.eq.1.and.l4.eq.1) then
          UpUp_TauTau_HA_NLO = 0d0
      elseif (l1.eq.1.and.l2.eq.1.and.l3.eq.1.and.l4.eq.2) then
          UpUp_TauTau_HA_NLO = 0d0
      elseif (l1.eq.1.and.l2.eq.1.and.l3.eq.2.and.l4.eq.1) then
          UpUp_TauTau_HA_NLO = 0d0
      elseif (l1.eq.1.and.l2.eq.1.and.l3.eq.2.and.l4.eq.2) then
          UpUp_TauTau_HA_NLO = 0d0
      elseif (l1.eq.1.and.l2.eq.2.and.l3.eq.1.and.l4.eq.1) then
          UpUp_TauTau_HA_NLO = 
     &      +ffgg*(-2*sinthta*1d0/sqrt(s)*qup*qta*mta)
     &      +ffqq*(-2*chizs*sinthta*1d0/sqrt(s)*vmaup*vmata*mta)
     &      +ffll*(-4*chizs*sinthta*1d0/sqrt(s)*aup*ata*mta)
     &      +fflq*(-4*chizs*sinthta*1d0/sqrt(s)*aup*vmata*mta)
     &      +ffql*(-2*chizs*sinthta*1d0/sqrt(s)*vmaup*ata*mta)
     &      +ffld*(-2*chizs*sinthta*1d0/sqrt(s)*pxstata*aup*ata*mta)
     &      +ffqd*(-chizs*sinthta*1d0/sqrt(s)*pxstata*vmaup*ata*mta)
      elseif (l1.eq.1.and.l2.eq.2.and.l3.eq.1.and.l4.eq.2) then
          UpUp_TauTau_HA_NLO = 
     &      +ffgg*(-qup*qta+costhta*qup*qta)
     &      +ffqq*(-chizs*vmaup*vmata+chizs*costhta*vmaup*vmata)
     &      +ffll*(-2*chizs*aup*ata+2*chizs*costhta*aup*ata-2*chizs*costhta
     &       *1d0/sqrt(s)*sqrtpxstata*aup*ata+2*chizs*1d0/sqrt(s)*sqrtpxstata*aup*ata)
     &      +fflq*(-2*chizs*aup*vmata+2*chizs*costhta*aup*vmata)
     &      +ffql*(-chizs*vmaup*ata+chizs*costhta*vmaup*ata-chizs*costhta
     &       *1d0/sqrt(s)*sqrtpxstata*vmaup*ata+chizs*1d0/sqrt(s)*sqrtpxstata*vmaup*ata)
      elseif (l1.eq.1.and.l2.eq.2.and.l3.eq.2.and.l4.eq.1) then
          UpUp_TauTau_HA_NLO = 
     &      +ffgg*(-qup*qta-costhta*qup*qta)
     &      +ffqq*(-chizs*vmaup*vmata-chizs*costhta*vmaup*vmata)
     &      +ffll*(-2*chizs*aup*ata-2*chizs*costhta*aup*ata-2*chizs*costhta
     &       *1d0/sqrt(s)*sqrtpxstata*aup*ata-2*chizs*1d0/sqrt(s)*sqrtpxstata*aup*ata)
     &      +fflq*(-2*chizs*aup*vmata-2*chizs*costhta*aup*vmata)
     &      +ffql*(-chizs*vmaup*ata-chizs*costhta*vmaup*ata-chizs*costhta
     &       *1d0/sqrt(s)*sqrtpxstata*vmaup*ata-chizs*1d0/sqrt(s)*sqrtpxstata*vmaup*ata)
      elseif (l1.eq.1.and.l2.eq.2.and.l3.eq.2.and.l4.eq.2) then
          UpUp_TauTau_HA_NLO = 
     &      +ffgg*(-2*sinthta*1d0/sqrt(s)*qup*qta*mta)
     &      +ffqq*(-2*chizs*sinthta*1d0/sqrt(s)*vmaup*vmata*mta)
     &      +ffll*(-4*chizs*sinthta*1d0/sqrt(s)*aup*ata*mta)
     &      +fflq*(-4*chizs*sinthta*1d0/sqrt(s)*aup*vmata*mta)
     &      +ffql*(-2*chizs*sinthta*1d0/sqrt(s)*vmaup*ata*mta)
     &      +ffld*(-2*chizs*sinthta*1d0/sqrt(s)*pxstata*aup*ata*mta)
     &      +ffqd*(-chizs*sinthta*1d0/sqrt(s)*pxstata*vmaup*ata*mta)
       elseif (l1.eq.2.and.l2.eq.1.and.l3.eq.1.and.l4.eq.1) then
          UpUp_TauTau_HA_NLO = 
     &      +ffgg*(+2*sinthta*1d0/sqrt(s)*qup*qta*mta)
     &      +ffqq*(+2*chizs*sinthta*1d0/sqrt(s)*vmaup*vmata*mta)
     &      +ffql*(+2*chizs*sinthta*1d0/sqrt(s)*vmaup*ata*mta)
     &      +ffqd*(+chizs*sinthta*1d0/sqrt(s)*pxstata*vmaup*ata*mta)
      elseif (l1.eq.2.and.l2.eq.1.and.l3.eq.1.and.l4.eq.2) then
          UpUp_TauTau_HA_NLO = 
     &      +ffgg*(-qup*qta-costhta*qup*qta)
     &      +ffqq*(-chizs*vmaup*vmata-chizs*costhta*vmaup*vmata)
     &      +ffql*(-chizs*vmaup*ata-chizs*costhta*vmaup*ata+chizs*costhta
     &       *1d0/sqrt(s)*sqrtpxstata*vmaup*ata+chizs*1d0/sqrt(s)*sqrtpxstata*vmaup*ata)
      elseif (l1.eq.2.and.l2.eq.1.and.l3.eq.2.and.l4.eq.1) then
          UpUp_TauTau_HA_NLO = 
     &      +ffgg*(-qup*qta+costhta*qup*qta)
     &      +ffqq*(-chizs*vmaup*vmata+chizs*costhta*vmaup*vmata)
     &      +ffql*(-chizs*vmaup*ata+chizs*costhta*vmaup*ata+chizs*costhta
     &       *1d0/sqrt(s)*sqrtpxstata*vmaup*ata-chizs*1d0/sqrt(s)*sqrtpxstata*vmaup*ata)
      elseif (l1.eq.2.and.l2.eq.1.and.l3.eq.2.and.l4.eq.2) then
          UpUp_TauTau_HA_NLO = 
     &      +ffgg*(+2*sinthta*1d0/sqrt(s)*qup*qta*mta)
     &      +ffqq*(+2*chizs*sinthta*1d0/sqrt(s)*vmaup*vmata*mta)
     &      +ffql*(+2*chizs*sinthta*1d0/sqrt(s)*vmaup*ata*mta)
     &      +ffqd*(+chizs*sinthta*1d0/sqrt(s)*pxstata*vmaup*ata*mta)
      elseif (l1.eq.2.and.l2.eq.2.and.l3.eq.1.and.l4.eq.1) then
          UpUp_TauTau_HA_NLO = 0d0
      elseif (l1.eq.2.and.l2.eq.2.and.l3.eq.1.and.l4.eq.2) then
          UpUp_TauTau_HA_NLO = 0d0
      elseif (l1.eq.2.and.l2.eq.2.and.l3.eq.2.and.l4.eq.1) then
          UpUp_TauTau_HA_NLO = 0d0
      elseif (l1.eq.2.and.l2.eq.2.and.l3.eq.2.and.l4.eq.2) then
          UpUp_TauTau_HA_NLO = 0d0
      else
          print*,""
          print*,"UpUp_TauTau_HA_NLO:: wrong value of helicity indices:"
          print*,"  l1=",l1," l2=",l2," l3=",l3," l4=",l4
          print*,""
          stop
      endif
 
      UpUp_TauTau_HA_NLO = 4*pi*alpha*UpUp_TauTau_HA_NLO
      har=DREAL(UpUp_TauTau_HA_NLO+DCONJG(UpUp_TauTau_HA_NLO))/2
      hai=DIMAG(UpUp_TauTau_HA_NLO-DCONJG(UpUp_TauTau_HA_NLO))/2
      return
      end
