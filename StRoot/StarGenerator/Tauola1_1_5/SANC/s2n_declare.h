
      integer iborn,iqed,iew,iqcd,gfscheme,ifgg,its,igzm2,ilin,ichep
      real*8 ma,mv,mh,mz,mw,men,mel,mup,mdn,mmn,mmo,mch,mst,mtn,mta,
     & mtp,mbt
      real*8 ram2,rvm2,rhm2,rzm2,rwm2,renm2,relm2,rupm2,rdnm2,rmnm2,
     & rmom2,rchm2,rstm2,rtnm2,rtam2,rtpm2,rbtm2
      real*8 ctw,stw,ctw2,stw2,ctw4,stw4,ctw6,stw6,stw2i
      real*8 rwz,rhz,rhw,renh,relh,ruph,rdnh,rmnh,rmoh,rchh,rsth,
     & rtnh,rtah,rtph,rbth,renz,relz,rupz,rdnz,rmnz,rmoz,rchz,rstz,rtnz,
     & rtaz,rtpz,rbtz,renw,relw,rupw,rdnw,rmnw,rmow,rchw,rstw,rtnw,rtaw,
     & rtpw,rbtw
      real*8 qen,qel,qup,qdn,qmn,qmo,qch,qst,qtn,qta,qtp,qbt,aen,
     & ael,aup,adn,amn,amo,ach,ast,atn,ata,atp,abt,ven,vel,vup,vdn,vmn,
     & vmo,vch,vst,vtn,vta,vtp,vbt,vpaen,vmaen,vpael,vmael,vpaup,vmaup,
     & vpadn,vmadn,vpamn,vmamn,vpamo,vmamo,vpach,vmach,vpast,vmast,
     & vpatn,vmatn,vpata,vmata,vpatp,vmatp,vpabt,vmabt,dfen,dfel,dfup,
     & dfdn,dfmn,dfmo,dfch,dfst,dftn,dfta,dftp,dfbt
      real*8 thmu,thmu2,tlmu,tlmu2,omega
      real*8 s,t,u,qs,ts,us,mf,mf1,spr
      real*8 nc,fc,cfl,cfq,cfscheme
      real*8 betaf,cosf,sinf
      real*8 alpha,alphai,gf,g,e,alphas,gs,cf,conhc,pi,sr2
      real*8 wz,ww,wh,wtp,gz,gw,gh,gtp
      real*8 rhm4,rzm4,rwm4
      real*8 qf(12),i3f(12),rmf1(12)
      real*8 born,width,sigma,soft,hard,virt,softvirt,totalvirt,volum
      real*8 hardisr,hardifi,hardfsr,hardifw
      complex*16 chardisr,chardifi,chardfsr,chardifw
      complex*16 cm2,vm2,hm2,zm2,wm2,enm2,elm2,upm2,dnm2,mnm2,mom2,
     & chm2,stm2,tnm2,tam2,tpm2,btm2
      complex*16 ffarray(3,100)
      complex*16 mz2,mz2c,mw2,mw2c,mh2,mh2c,mtp2,mtp2c
      complex*16 cmz2,cmw2,cmh2,ctpm2,i_

      real*8 za1bos,zmzzzbos,zmwzwbos,zmhzhbos,deltarhobos
      real*8 za1fer,zz1fer,zh1fer,zw1fer,zmsqrfer,
     & zmzzzfer,zmwzwfer,zmhzhfer,deltarhofer,tadfer
      real*8 caldzfer,caldwfer
      complex*16 pizgfer,piggfer
      complex*16 pigglep,za1lep,piggren

      real*8 ddilog
      complex*16 jaa,jaz,jav,javsub,javsubt0,javsubu0,javsubtn,javsubun
      complex*16 d040,d040wz,tdd0
      complex*16 b0p,b0f,b0d1,b0d2,b0dw,bff,c0_sp,c01,c0ir,c0irf
      complex*16 xspenz,jinteg

      common/masses/ma,mv,mh,mz,mw,men,mel,mup,mdn,mmn,mmo,mch,mst,mtn,
     & mta,mtp,mbt
      common/r2masses/ram2,rvm2,rhm2,rzm2,rwm2,renm2,relm2,rupm2,rdnm2,
     & rmnm2,rmom2,rchm2,rstm2,rtnm2,rtam2,rtpm2,rbtm2
      common/c2masses/cm2,vm2,hm2,zm2,wm2,enm2,elm2,upm2,dnm2,mnm2,mom2,
     & chm2,stm2,tnm2,tam2,tpm2,btm2
      common/cstw/ctw,stw,ctw2,stw2,ctw4,stw4,ctw6,stw6
      common/rmhwz/rwz,rhz,rhw,renh,relh,ruph,rdnh,rmnh,rmoh,rchh,rsth,
     & rtnh,rtah,rtph,rbth,renz,relz,rupz,rdnz,rmnz,rmoz,rchz,rstz,rtnz,
     & rtaz,rtpz,rbtz,renw,relw,rupw,rdnw,rmnw,rmow,rchw,rstw,rtnw,rtaw,
     & rtpw,rbtw
      common/qavpm/qen,qel,qup,qdn,qmn,qmo,qch,qst,qtn,qta,qtp,qbt,aen,
     & ael,aup,adn,amn,amo,ach,ast,atn,ata,atp,abt,ven,vel,vup,vdn,vmn,
     & vmo,vch,vst,vtn,vta,vtp,vbt,vpaen,vmaen,vpael,vmael,vpaup,vmaup,
     & vpadn,vmadn,vpamn,vmamn,vpamo,vmamo,vpach,vmach,vpast,vmast,
     & vpatn,vmatn,vpata,vmata,vpatp,vmatp,vpabt,vmabt,dfen,dfel,dfup,
     & dfdn,dfmn,dfmo,dfch,dfst,dftn,dfta,dftp,dfbt
      common/flags/iborn,iqed,iew,iqcd,gfscheme,ilin,ifgg,its
      common/comphep/ichep
      common/ff/ffarray
      common/qfi3f/qf,i3f,rmf1
      common/nc/nc,fc,cfl,cfq,cfscheme
      common/kinematics/betaf,mf,mf1
      common/width/wz,ww,wh,wtp,gz,gw,gh,gtp
      common/widthm/mz2,mw2,mh2,mtp2,mz2c,mw2c,mh2c,mtp2c
      common/consts/alpha,alphai,gf,g,e,alphas,gs,cf,conhc,pi,sr2,i_
      common/scales/thmu2,tlmu2,omega

