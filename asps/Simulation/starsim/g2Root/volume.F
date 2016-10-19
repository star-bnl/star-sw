*_______________________________________________________________________
      subroutine volume(cname,qjv,iposp,ifirst,gname)
#if 0
*KEEP,HCBOOK.
      parameter (nwpaw=4000000)
      common/GCBANK/paw(nwpaw)

      INTEGER   IQ(2), LQ(8000)
      REAL            Q(2)
      EQUIVALENCE (LQ(1),paw(11)),(IQ(1),paw(19)),(Q(1),paw(19))

      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
#else
#include "geant321/gcbank.inc"
#include "geant321/gcnum.inc"
#endif
      parameter (MAXPOS=250000)
      common/cnpos/nodepos(MAXPOS),nodediv(MAXPOS),nvflags(MAXPOS),
     +npflags(MAXPOS),nppflags(MAXPOS)  

      character *(*) cname, gname
      character*128 cmater
      character*20 astring, pname, rname
      character*128 creals
      character*256 line
      real qjv(100)
      double precision RADDEG
      dimension dummypars(100)
      DIMENSION NPARS(30)
      CHARACTER*6 KSHAP(30)
      data dummypars/100*0./

      DATA KSHAP/'Box','Trd1','Trd2','Trap','Tube','Tubs','Cone',
     +           'Cons','Sphere','Para','Pgon','Pcon','Eltu','Hype',
     + 13*'      ','Gtra','Ctub','    '/
      DATA NPARS/3,4,5,11,3,5,5,7,6,6,10,9,3,4,13*0,12,11,0/
*________________________________________________________________________
*

      RADDEG = 57.2957795130823209
      n1=lenocc(cname)

**      print *, 'VOLUME n1=',n1,' cname=',cname(1:n1)
      ishape = qjv(2)
      numed  = qjv(4)
      jtm    = lq(jtmed-numed)
*yf      call toint(numed,astring,nc)
*yf
      call uhtoc(iq(jtm+1),4,astring,20)
      nc=lenocc(astring)
      cmater='GetMed("'//astring(1:nc)//'")'
      ncmat = lenocc(cmater)
      nord  = qjv(1)
      nin   = qjv(3)
      npar  = qjv(5)
      npar0 = npars(ishape)
**TRAP
      jpar = 6
      if (ishape.eq.4) then
         ph = 0.
         if (qjv(jpar+2).ne.0.)ph=atan2(qjv(jpar+3),qjv(jpar+2))*RADDEG
         tt = sqrt(qjv(jpar+2)**2+qjv(jpar+3)**2)
         qjv(jpar+2) = atan(tt)*RADDEG
         if (ph.lt.0.0) ph = ph+360.0
         qjv(jpar+3) = ph
         qjv(jpar+7) = atan(qjv(jpar+7))*RADDEG
         if (qjv(jpar+7).gt.90.0) qjv(jpar+7) = qjv(jpar+7)-180.0
         qjv(jpar+11)= atan(qjv(jpar+11))*RADDEG
         if (qjv(jpar+11).gt.90.0) qjv(jpar+11) = qjv(jpar+11)-180.0
      endif
**PARA
      if (ishape.eq.10) then
         ph = 0.
         if (qjv(jpar+5).ne.0.)ph=atan2(qjv(jpar+6),qjv(jpar+5))*RADDEG
         tt = sqrt(qjv(jpar+5)**2+qjv(jpar+6)**2)
         qjv(jpar+4) = atan(qjv(jpar+4))*RADDEG
         if (qjv(jpar+4).gt.90.0) qjv(jpar+4) = qjv(jpar+4)-180.0
         qjv(jpar+5) = atan(tt)*RADDEG
         if (ph.lt.0.0) ph = ph+360.0
         qjv(jpar+6) = ph
      endif
      if(ishape.eq.11)npar0=4
      if(ishape.eq.12)npar0=3
**HYPE
      if (ishape.eq.14) then
         hyrmin = qjv(jpar+1)
         hyrmax = qjv(jpar+2)
         hydz = qjv(jpar+3)
         hyst = qjv(jpar+4)
         dummypars(1) = hyrmin
         dummypars(2) = hyst
         dummypars(3) = hyrmax
         dummypars(4) = hyst
         dummypars(5) = hydz
         npar0 = -5
       endif  
         
*      print 2351, cname(1:n1),kshap(ishape)
* 2351      format('Volume:',a, ' shape=',a)
      if (npar.le.0) then
**       print 2352, cname(1:n1),kshap(ishape)
** 2352      format('Warning, volume with 0 parameters:',a, ' shape=',a)
       return
      endif
      if(npar0.le.0)then
         call toreals(-npar0,dummypars(1),creals,ncr)
      else
         call toreals(npar0,qjv(7),creals,ncr)
      endif
      line=' '
      nshape = lenocc(kshap(ishape))
      call ptname(cname, pname)
      np = lenocc(pname)
      call realname(cname, rname)
      nrr = lenocc(rname)
      ng  = lenocc(gname)
      if (iposp.eq.0) then
#if 1
         write(line,2000)pname(1:np),kshap(ishape)(1:nshape)
     +         ,gname(1:ng),cmater(1:ncmat),creals(1:ncr)
     +         ,pname(1:np),rname(1:nrr)
#else
         write(line,2000)pname(1:np),kshap(ishape)(1:nshape)
     +         ,rname(1:nrr),cmater(1:ncmat),creals(1:ncr)
     +         ,pname(1:np), gname(1:ng)
#endif
      else
         if (ifirst.eq.1) then
#if 1
            write(line,2001)pname(1:np),gname(1:ng),cmater(1:ncmat)
     +         ,pname(1:np),rname(1:nrr)
#else
            write(line,2001)pname(1:np),rname(1:nrr),cmater(1:ncmat)
     +         ,gname(1:ng), pname(1:np)
#endif
            nch=lenocc(line)
            write(51,'(a)')line(1:nch)
         endif
         line=' '
#if 1
         write(line,2002)pname(1:np),kshap(ishape)(1:nshape)
     +         ,gname(1:ng),cmater(1:ncmat),creals(1:ncr)
#else
         write(line,2002)pname(1:np),kshap(ishape)(1:nshape)
     +         ,rname(1:nrr),cmater(1:ncmat),creals(1:ncr)
*     +         ,pname(1:np), gname
#endif
         nch=lenocc(line)
         write(51,'(a)')line(1:nch) 
      endif           
2000  format(
     + 'TGeoVolume',' *',a,' = gGeoManager->Make',a,'("',a,'",'
     +,a,a,'); ',a,'->SetTitle("',a,'");')
2001  format('TGeoVolumeMulti *',a,' = gGeoManager->MakeVolumeMulti("'
     +,a,'", ',a,'); ',a,'->SetTitle("',a,'");')
2002  format(' ',a,'->AddVolume(gGeoManager->Make',a,'("',a,'",',
     +a,a,'));')          
*     +a,a,')); ',a,'->SetTitle("',a4,'");')          
      nch = lenocc(line)
      if (iposp.eq.0) write(51,'(a)')line(1:nch)
      if(ishape.eq.11)then
         ndz=qjv(10)
         do iz=1,ndz
            call toreals(3,qjv(11+(iz-1)*3),creals,ncr)
            line=' '
            call toint(iz-1,astring,nci)
            if (iposp.eq.0) then
            write(line,2010)pname(1:np),astring(1:nci),creals(1:ncr)
            else
            write(line,2011)pname(1:np),astring(1:nci),creals(1:ncr)
            endif
            nch = lenocc(line)
            write(51,'(a)')line(1:nch)
         enddo
      endif
      if(ishape.eq.12)then
         ndz=qjv(9)
         do iz=1,ndz
            call toreals(3,qjv(10+(iz-1)*3),creals,ncr)
            line=' '
            call toint(iz-1,astring,nci)
            if (iposp.eq.0) then
            write(line,2010)pname(1:np),astring(1:nci),creals(1:ncr)
            else
            write(line,2011)pname(1:np),astring(1:nci),creals(1:ncr)
            endif
            nch = lenocc(line)
            write(51,'(a)')line(1:nch)
         enddo
      endif
2010  format(2x,'((TGeoPcon*)',a,'->GetShape())->DefineSection(',
     + a,a,');')
2011  format(2x,'((TGeoPcon*)',a,'->GetLastShape())->DefineSection(',
     + a,a,');')
*   Any attributes set ?
      lseen  = qjv(npar+8)
      lstyle = qjv(npar+9)
      lwidth = qjv(npar+10)
      lcolor = qjv(npar+11)
      lfill  = qjv(npar+12)
      if (lstyle.le.0) lstyle = 1
      if (lwidth.le.0) lwidth = 1
      if (lcolor.lt.0) lcolor = 1
      if (lfill.lt.0)  lfill  = 0
*      if(ivo.eq.1)lseen=0
*      if(nord.lt.0)then
*         print *,'ordering : ',-nord
*         call toint(-nord,creals,ncr)
*      endif
      if ((iposp.eq.0).or.(ifirst.eq.1)) then
      if(lseen.ne.1)then
         call toint(lseen,creals,ncr)
         write(51,195)pname(1:np),creals(1:ncr)
195        format(2x,a,'->SetVisibility(',a,');')
      endif
      if(lstyle.ne.1)then
         call toint(lstyle,creals,ncr)
         write(51,196)pname(1:np),creals(1:ncr)
196        format(2x,a,'->SetLineStyle(',a,');')
      endif
      if(lwidth.ne.1)then
         call toint(lwidth,creals,ncr)
         write(51,197)pname(1:np),creals(1:ncr)
197        format(2x,a,'->SetLineWidth(',a,');')
      endif
      if(lcolor.ne.1)then
         call toint(lcolor,creals,ncr)
         write(51,198)pname(1:np),creals(1:ncr)
198        format(2x,a,'->SetLineColor(',a,');')
      endif
      if(lfill.ne.0)then
         call toint(lfill,creals,ncr)
         write(51,199)pname(1:np),creals(1:ncr)
199        format(2x,a,'->SetFillStyle(',a,');')
      endif
      endif
#ifdef __TPCREFSYS__
      if (pname .eq. 'CAVE') then
         write(51,201) 
 201     format(2x,'TGeoVolumeAssembly *TpcRefSys ',
     &        ' = new TGeoVolumeAssembly("TpcRefSys");')
      endif
#endif /*  __TPCREFSYS__ */
      end

