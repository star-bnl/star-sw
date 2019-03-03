      Subroutine toroot(fname)
*
**********************************************************************
*
*      Rotation matrices (structure JROTM) are saved into a linked
*      list of class objects TRotMatrix.
*      The JVOLUM structure generates the following lists:
*        - the TMaterial list (material definition only).
*        - the TRotmatrix list (Rotation matrices definition only).
*        - the TShape list (volume definition only).
*        - the TNode list assembling all detector elements
*          the TNode list is a real tree.
*          The Node list contains two variants TNode and TNodeDiv
*          corresponding to the GSPOS and GSDIV mechanisms.
*
*  Author: Rene Brun
**********************************************************************
*
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
*KEEP,GCNUM.
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
#else
#include "geant321/gcbank.inc"
#include "geant321/gcnum.inc"
#include "geant321/gccuts.inc"
#include "geant321/gcphys.inc"
      DIMENSION CUTS(10),MECA(5,13)
      EQUIVALENCE (CUTS(1),CUTGAM),(MECA(1,1),IPAIR)
#endif
*

      common/cnodes/nnodes

      parameter (MAXPOS=250000)
*      parameter (MAXPOS=50000)
      common/cnpos/nodepos(MAXPOS),nodediv(MAXPOS),nvflags(MAXPOS),
     +npflags(MAXPOS),nppflags(MAXPOS)  

*     external itorotm
      CHARACTER*4 KSHAP(30),klshap(30)
      character*20 matname,medname
      character*20 cname,gname,mname,pname, rname
      character*(*) fname
      character*256 line
      character*128 creals
      character*20 astring,astring2,ptrname
      dimension pmixt(3)
      logical map_found

      DATA KSHAP/'BRIK','TRD1','TRD2','TRAP','TUBE','TUBS','CONE',
     +           'CONS','SPHE','PARA','PGON','PCON','ELTU','HYPE',
     + 13*'    ','GTRA','CTUB','    '/
      character*8 KPar(33)
      data Kpar /
     +     'CUTGAM','CUTELE','CUTNEU','CUTHAD','CUTMUO',
     +     'BCUTE' ,'BCUTM' ,'DCUTE' ,'DCUTM' ,'PPCUTM',
     +     'PAIR'  ,'COMP'  ,'PHOT'  ,'PFIS'  ,'DRAY'  ,
     +     'ANNI'  ,'BREM'  ,'HADR'  ,'MUNU'  ,'DCAY'  ,
     +     'LOSS'  ,'MULS'  ,'RAYL'  ,''      ,''      ,
     +     'GHCOR1','BIRK1' ,'BIRK2' ,'BIRK3' ,''      ,
     +     'LABS' , 'SYNC' , 'STRA' /

*________________________________________________________________________
*

      open(unit=51,file=fname,status='unknown')
*
* Create new Geometry object
* ==========================
      nch=lenocc(fname)
      idot=index(fname,'.')
      nct=idot-1
      write(51,1111)fname(1:nct)
1111  format('void ',a,'()',/,'{',/,
     +'//',/,
     +'//  This file has been generated automatically via the root',/,
     +'//  utility g2root from an interactive version of GEANT',/,
     +'//   (see ROOT class TGeoManager for an example of use)',/,
     +'//',/,
     +'gSystem->Load("libGeom");',/,
     +'TGeoRotation *rot;',/,
     +'TGeoNode *Node, *Node1;')

      do 1 i=1,30
         klshap(i) = kshap(i)
         call cutol(klshap(i))
   1  continue
      do 2 i=1,MAXPOS
         nodepos(i) = 0
         nodediv(i) = 0
   2  continue
      nodepos(1) = 1
      nodediv(1) = 1

      write(51,490)fname(1:nct),fname(1:nct),fname(1:nch)
 490  format(/,'TGeoManager *',a,' = new TGeoManager("',a,'","',a,'");'
     +,/)
      IF(JVOLUM.NE.0 ) NVOLUM = IQ(JVOLUM-2)
      IF(JMATE.NE.0 )  NMATE  = IQ(JMATE-2)
      IF(JTMED.NE.0 )  NTMED  = IQ(JTMED-2)
      IF(JROTM.NE.0 )  NROTM  = IQ(JROTM-2)
* Print Materials
* =======================
      write(51,3019)
 3019 format(/,
     +'//-----------List of Materials and Mixtures--------------',/)
      write(51,4001) 
 4001 format('TGeoMixture *mix = 0;')
      write(51,4002) 
 4002 format('Int_t imat = 0;')
      write(51,4003) 
 4003 format('Int_t imed = 0;')
      do 300 imat=1,nmate
         jma=lq(jmate-imat)
         if(jma.eq.0)go to 300
         nmixt=q(jma+11)
         call uhtoc(iq(jma+1),4,matname,20)
         ncn=lenocc(matname)
         call toint(imat,astring,nc)
         nm=abs(nmixt)
*-*             Case of a simple material
         if (nm.le.1)then
            call toreals(3,q(jma+6),creals,ncr)
            if(q(jma+6).lt.1.and.q(jma+7).lt.1)then
               creals=' '
               ncr=lenocc(creals)
            endif
            line=' '
*yf            write(line,3000)astring(1:nc),matname(1:ncn),creals(1:ncr)
*            write(line,3000)matname(1:ncn),matname(1:ncn),creals(1:ncr)
* 3000       format('TGeoMaterial *mat',a,' = new TGeoMaterial("',a,
*yf     +       '"',a,');')
*yf            write(line,3000) matname(1:ncn),creals(1:ncr)
*yf 3000       format('new TGeoMaterial("',a,'"',a,');')
            write(line,3000) matname(1:ncn),creals(1:ncr)
 3000       format('(new TGeoMaterial("',a,'"',a,
     +'))->SetUniqueID(++imat);')
*yf            write(line,3005) astring(1:nc),imat
*yf            write(line,3005) matname(1:ncn),imat
* 3005       format(4x,'mat',a,'->SetUniqueID(',i4,');')
            nch = lenocc(line)
            write(51,'(a)')line(1:nch)
*-*             Case of a mixture
         else
            call toint(nm,creals,ncm)
            jmixt=lq(jma-5)
            if(nmixt.gt.0)then
               mname=creals(1:ncm)
            else
               mname='-'//creals(1:ncm)
               ncm=ncm+1
            endif
            line=' '
*yf            write(line,3010)astring(1:nc),matname(1:ncn),mname(1:ncm),
*            write(line,3010)matname(1:ncn),matname(1:ncn),mname(1:ncm),
*     +      q(jma+8)
* 3010       format('mix',a,' = new TGeoMixture("',a,'",',a,
*     +              ',',g14.6,');')
*yf            write(line,3010) matname(1:ncn),mname(1:ncm),q(jma+8)
*yf 3010       format('mix = new TGeoMixture("',a,'",',a, ',',g14.6,');')
            write(line,3010) matname(1:ncn),mname(1:ncm),q(jma+8)
 3010       format('mix = new TGeoMixture("',a,'",',a, ',',g14.6,');',
     +  'mix->SetUniqueID(++imat);')
*yf            write(line,3011) astring(1:nc),imat
*yf            write(line,3011) matname(1:ncn),imat
*yf 3011       format(4x,'mat',a,'->SetUniqueID(',i4,');')
            nch = lenocc(line)
            write(51,'(a)')line(1:nch)
            do 292 im=1,nm
               call toint(im-1,astring2,nc2)
               pmixt(1) = q(jmixt+im)
               pmixt(2) = q(jmixt+nm+im)
               pmixt(3) = q(jmixt+2*nm+im)
               call toreals(3,pmixt,creals,ncr)
               line=' '
*yf               write(line,3020)astring(1:nc),astring2(1:nc2),
*               write(line,3020)matname(1:ncn),astring2(1:nc2),
*     +         creals(1:ncr)
* 3020          format(4x,'mix',a,'->DefineElement(',a,a,');')
               write(line,3020) astring2(1:nc2), creals(1:ncr)
 3020          format(4x,'mix->DefineElement(',a,a,');')
               nch = lenocc(line)
               write(51,'(a)')line(1:nch)
 292        continue
         endif
 300  continue
* Print Tracking Media
* ====================
      write(51,3069)
 3069 format(/,
     +'//-----------List of Tracking Media--------------',/)
      write(51,3051) 
 3051 format('TGeoMedium *med = 0;')
      do 350 itmed=1,ntmed
         jtm=lq(jtmed-itmed)
         if(jtm.eq.0)go to 350
         imat=q(jtm+6)
         jma=lq(jmate-imat)
         if(jma.eq.0)go to 350
*yf         call toint(imat,astring2,ncm)
         call uhtoc(iq(jma+1),4,astring2,20)
         ncmk=lenocc(astring2)
         call uhtoc(iq(jtm+1),4,medname,20)
         ncm=lenocc(medname)
         call toint(itmed,astring,nc)
         call toreals(8,q(jtm+7),creals,ncr)
         line=' '
*yf         write(line,3050)astring(1:nc),medname(1:ncn),astring(1:nc),
*         write(line,3050)medname(1:ncm),medname(1:ncm),astring(1:nc),
*     +     astring2(1:ncmk),creals(1:ncr)
* 3050    format('TGeoMedium *med',a,' = new TGeoMedium("',a,'",',a,
*yf     +     ',mat',a,'->GetUniqueID()',a,');')
         write(line,3050) medname(1:ncm),astring2(1:ncmk),creals(1:ncr)
 3050    format('med = new TGeoMedium("',a,'",0',
     +',GetMat("',a,'")->GetUniqueID()',a,
     +'); med->SetUniqueID(++imed);')
         nch = lenocc(line)
         write(51,'(a)')line(1:nch)
         jtmn = lq(jtm)
         if (jtmn .ne. 0) then
            do  349 itpar = 1, 33
               par = q(jtmn+itpar)
               if (itpar .le. 10 .and. par .eq. cuts(itpar)) go to  349
               ipar = par
               if (itpar .gt. 10 .and. itpar .le. 23 .and. 
     +              ipar .eq. meca(1,itpar-10)) go to  349
               if (par .ne. 0) then
                  line=' '
                  nch = lenocc(Kpar(itpar));
                  write (line,3052) Kpar(itpar)(1:nch), par 
 3052             format('med->SetParam("',a,'",',g14.6,');') 
                  nch = lenocc(line)
                  write(51,'(a)')line(1:nch)
               endif
 349        continue
         endif
*yf ================================================================================           
 350  continue
* Print Rotation matrices
* =======================
      write(51,3021)
 3021 format(/,'//-----------List of Rotation matrices--------------',/)
      do 100 irot=1,nrotm
         jr=lq(jrotm-irot)
         if(jr.eq.0)go to 100
*yf
*         call toint(irot,astring,nc)
*         call toreals(6,q(jr+11),creals,ncr)
*         line=' '
*         ptrname = 'rot'//astring(1:nc)
*         nch = nc+3
*         write(line,1000)ptrname(1:nch),ptrname(1:nch),
*     +     creals(1:ncr)
* 1000    format('TGeoRotation *',a,
*     +        ' = new TGeoRotation("',a,'"',a,');')
*yf
         iok = itorotm(q(jr+11),astring)
         nc = 4
         if (iok .eq. 0) go to 100
         call toreals(6,q(jr+11),creals,ncr)
         line=' '
*yf         ptrname = 'rot'//astring(1:nc)
*         nch = nc+3
*         write(line,1000)ptrname(1:nch),ptrname(1:nch),
*     +     creals(1:ncr)
* 1000    format('TGeoRotation *',a,
*     +        ' = new TGeoRotation("',a,'"',a,');')
         write(line,1000) astring(1:nc),creals(1:ncr)
 1000    format('(new TGeoRotation("',a,'"',a,'))->RegisterYourself();')
         nch = lenocc(line)
         write(51,'(a)')line(1:nch)
 100  continue

*  Print volume definition (ROOT shapes)
*  =======================
      write(51,3022)
 3022 format(/,'//-----------List of Volumes--------------',/)
      Print *,' nvolum= ',nvolum, ' jvolum=',jvolum
C
C  ???? Convert GEANT3 keys to legal C++ names ?????
C
      do 50 ivo = 1,nvolum
         if (lq(jvolum-ivo).eq.0)go to 50        ! <--  That's
         cname=' '                                   !    'real'
         gname=' '
*         write(cname,'(a4)')iq(jvolum+ivo)          !     trick !!!
         call uhtoc(iq(jvolum+ivo),4,cname,4)    ! <--
         call uhtoc(iq(lq(jvolum-ivo)-4),4,gname,4)    ! <--
         do 29 i=1,4
            if (ichar(cname(i:i)).eq.0)cname(i:i) = ' '
            if (ichar(gname(i:i)).eq.0)gname(i:i) = ' '
  29     continue
         n1=lenocc(cname)
         if(n1.lt.4)then
            do 30 i=n1+1,4
               cname(i:i)='_'
  30        continue
         endif
         n1=lenocc(gname)
         if(n1.lt.4)then
            do i=n1+1,4
               gname(i:i)='_'
            end do 
         endif
  50  continue
C----------------------------------------------
      do 77 ivo = 1,nvolum
 77   nvflags(ivo) = 0
      nlevel = 0
      call markdiv(1,1)
 
      do 200 ivo = 1,nvolum
         if (nvflags(ivo).eq.2) goto 200
         jv=lq(jvolum-ivo)
         if (jv.eq.0)go to 200
         cname=' '
         if(.not.map_found(iq(jvolum+ivo),cname)) then
            write(cname,'(a4)')iq(jvolum+ivo)
         endif
         gname=' '
         if(.not.map_found(iq(lq(jvolum-ivo)-4),gname)) then
            write(gname,'(a4)') iq(lq(jvolum-ivo)-4)
         endif
         call volume(cname,q(jv+1),0,0,gname)
 200  continue

      do 88 ivo = 1,nvolum
         nvflags(ivo) = 0
         npflags(ivo) = 0
         nppflags(ivo) = 0
 88   continue
* Print volume positioning (ROOT nodes)
* ========================

      nnodes = 1
      nlevel = 0
      if (nvolum.gt.0) then
         call node(1,1,1)
         write(51,3023)
 3023    format(/,'//-----------List of Nodes--------------',/)
         do 89 ivo = 1,nvolum
 89      nvflags(ivo) = 0
         call node(1,1,0)
      endif   

*      write(51,2223)
      write(51,2222)
2222  format('}')
2223  format(' gGeoManager->CloseGeometry();')
      close(51)
      end

