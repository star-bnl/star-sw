*_______________________________________________________________________
      Subroutine node2(ivo,nuserm,iposp)
      call node(ivo,nuserm,iposp)
      End

*_______________________________________________________________________
       Subroutine node(ivo,nuserm,iposp)
*
*             Process one node (volume with contents)
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
#endif
      common/cnodes/nnodes
      common/clevel/nodeold(20),nlevel

      parameter (MAXPOS=250000)
      common/cnpos/nodepos(MAXPOS),nodediv(MAXPOS),nvflags(MAXPOS),
     +npflags(MAXPOS),nppflags(MAXPOS)  

      dimension qjv(1000)
      character*20 cnode,cname,mname,anode,mother,pname, rname,ptrname
      character*20 motherS
      character*20 gname
      integer nmother, nmotherS
      character*256 line, rline, sline
      character*128 creals, rreals
      character*20 astring,astring1
      character*226 matrix
      character*256 matrixs
      character *20 cblank
      Logical map_found
      data cblank/' '/
*---------------------------------------------------------------------
      cblank = ' '
      rline  = ' '
      sline  = ' '
      nlevel = nlevel + 1
      nodeold(nlevel) = nnodes
      jv=lq(jvolum-ivo)
      ishape = q(jv+2)
      nin = q(jv+3)
      mname=' '
      if(.not.map_found(iq(jvolum+ivo),mname)) then
         write(mname,'(a4)')iq(jvolum+ivo)
      endif
      n2=lenocc(mname)
      call toint(nuserm,astring,nci)
*- If top volume, create the top node
      call ptname(mname, mother)
      nmother = lenocc(mother)
      if((ivo.eq.1).and.(iposp.eq.0))then
         write(51,510)mother(1:nmother)
*         write(51,510)
510      format('gGeoManager->SetTopVolume(',a,');')
         
      endif
*-  Generate subnodes of this node (if any)
**      print 2346, iq(jvolum+ivo), nin
2346  format('Processing node:',a4,' nin=',i4)
      if(nin.eq.0)then
         nlevel=nlevel-1
         return
      endif
      call cdnode(nodeold(nlevel))
*      print 520, mother(1:nmother), ivo
520   format('mother ',a,' index ',i9)
      if(nin.gt.0)then
            if (nvflags(ivo).ne.0) then
               goto 996
            endif
            nvflags(ivo)=1
*                    Volume has positioned contents
         do 300 in=1,nin
            ifirst = 0
            icurrent = 0
            imulti = 0
            nci1 = 0
            jin=lq(jv-in)
            ivom=q(jin+2)
            nuser  = q(jin+3)
            imany = q(jin+8)
*            print *,'in=',in,' nuser=',nuser
            jinvom = lq(jvolum-ivom)
            npar   = q(jinvom+5)
            ninvom = q(jinvom+3)
*           print 1234, iq(jinvom-4), iq(jvolum+ivom)
 1234       format('bank name ',A4,' volume name ',A4)
            cname=' '
            if(.not.map_found(iq(jvolum+ivom),cname)) then
               write(cname,'(a4)')iq(jvolum+ivom)
            endif
            gname=' '
            if(.not.map_found(iq(lq(jvolum-ivom)-4),gname)) then
               write(gname,'(a4)') iq(lq(jvolum-ivom)-4)
            endif
            n1=lenocc(cname)
            if (npar.gt.0) then
               jpar = jinvom+6
            else
               jpar = jin+9
               if (iposp.eq.1) then
                  if (npflags(ivom).eq.0) then
                     ifirst = 1
                     npflags(ivom) = 1
                  else
                     npflags(ivom) = npflags(ivom)+1   
                  endif   
               else
                  icurrent = nppflags(ivom)
                  call toint(icurrent,astring1,nci1)
                  imulti = 1
                  nppflags(ivom) = nppflags(ivom)+1   
               endif   
               npar = q(jin+9)
               call ucopy(q(jinvom+1),qjv(1),6)
               qjv(5) = npar
               call ucopy(q(jin+10),qjv(7),npar)
               call ucopy(q(jinvom+7),qjv(7+npar),6)
               call toint(in,astring,nci)
               mname=cname(1:n1)//astring(1:nci)
               if (iposp.eq.1) call volume(cname,qjv,iposp,ifirst,gname)
            endif
            if(ninvom.ge.0)then
               nnodes = nnodes+1
               if (nnodes.gt.MAXPOS) then
                  print *,'Too many nodes =',nnodes
                  go to 300
               endif
               call toint(nnodes,anode,ncd)
               cnode = 'Node'//anode(1:ncd)
               if (nodepos(nnodes).eq.0) then
*                  write(51,4444)cblank(1:nlevel),anode(1:ncd)
 4444             format(a,'TNode *Node',a,';')
                  nodepos(nnodes) = 1
               endif
            else
               cnode = 'Node'
            endif
            nd=lenocc(cnode)
            call toreals(3,q(jin+5),creals,ncr)
            itrans = 1
            if ((abs(q(jin+5)).lt.1E-30).and.
     +          (abs(q(jin+6)).lt.1E-30).and.
     +          (abs(q(jin+7)).lt.1E-30)) then 
               itrans = 0
            endif
            irot=q(jin+4)
            matrixs=' '
            if(irot.eq.0)then
               matrix='0'
               ncmatrix=1
               if (itrans.eq.0) then
                  matrixs='gGeoIdentity'
               else
                  matrixs='new TGeoTranslation('//creals(2:ncr)//')'
               endif
            else
*yf 
               ncmatrix = 3
               jr=lq(jrotm-irot)
               if(jr.eq.0) stop 'Bad rotation matrix'
               iok = itorotm(q(jr+11),astring)
               if (iok .ne. 0) then
                  matrix='GetRot("'//astring(1:4)//'")'
                  ncmatrix = lenocc(matrix)
               else
                  call toint(irot,astring,nc)
                  call toreals(6,q(jr+11),rreals,ncrr)
                  sline=' '
                  matrix = 'rot'
*yf                  ptrname = 'rot'//astring(1:nc)
*yf                  nch = 3 + nc
                  ptrname = 'next'
                  nch = 4
                  write(sline,1000) ptrname(1:nch),rreals(1:ncrr)
 1000             format('rot = new TGeoRotation("',a,'"',a,');')
                  nch = lenocc(sline)
                  do i=1,nch
                     if (rline(i:i) .ne. sline(i:i)) go to 1317
                  end do
                  goto 1318
 1317             if (iposp.eq.0) write(51,'(a,a)') 
     +                 cblank(1:nlevel), sline(1:nch)
                  rline = sline
 1318             continue
               endif
               if (itrans.eq.0) then
                  matrixs=matrix(1:ncmatrix)
               else
                  matrixs='new TGeoCombiTrans('//creals(2:ncr)//','//
     +               matrix(1:ncmatrix)//')'
               endif
            endif
            call toint(nuser,astring,nci)
**            print *,' cname=',cname(1:n1), ' astring=',astring(1:nci)
            mname=cname(1:n1)//astring(1:nci)
            n2=lenocc(mname)
            ncmats=lenocc(matrixs)
            line=' '

            call ptname(cname, pname)
            np = lenocc(pname)
#ifdef __TPCREFSYS__
            if (mother(1:nmother) .eq. 'CAVE' .and. (
     +           pname(1:np) .eq. 'IDSM' .or.
     +           pname(1:np) .eq. 'SVTT' .or.
     +           pname(1:np) .eq. 'SCON' .or.
     +           pname(1:np) .eq. 'SFMO' .or.
     +           pname(1:np) .eq. 'TPCE' .or.
     +           pname(1:np) .eq. 'FTPC' .or.
     +           pname(1:np) .eq. 'FTMO' .or.
     +           pname(1:np) .eq. 'FSMO' .or.
     +           pname(1:np) .eq. 'BTOF' .or.
     +           pname(1:np) .eq. 'PXMO' .or.
     +           pname(1:np) .eq. 'IGMO' .or.
     +           pname(1:np) .eq. 'FGMO' .or.
     +           pname(1:np) .eq. 'BTOF' )) then
               nmotherS = 9
               motherS(1:nmothers) = 'TpcRefSys'
            else 
               nmotherS = nmother
               motherS(1:nmothers) = mother(1:nmother)
            endif
            if (imany.eq.1 .or. motherS(1:nmothers) .eq. 'TpcRefSys') 
     + then
#else /*!  __TPCREFSYS__ */ 
               nmotherS = nmother
               motherS(1:nmothers) = mother(1:nmother)
            if (imany.eq.1) then
#endif /*  __TPCREFSYS__ */
               if (imulti.eq.0) then
               write(line,3000)cblank(1:nlevel),motherS(1:nmotherS),
     +               pname(1:np), astring(1:nci), matrixs(1:ncmats)
 3000          format(a,a,'->AddNode(',a,',',a,',',a,');')
               else
               write(line,3002)cblank(1:nlevel),motherS(1:nmotherS),
     +               pname(1:np), astring1(1:nci1),astring(1:nci), 
     +               matrixs(1:ncmats)
 3002          format(a,a,'->AddNode(',a,'->GetVolume(',a,'),',a
     +                ,',',a,');')
               endif
            else
               if (imulti.eq.0) then
               write(line,3001)cblank(1:nlevel),motherS(1:nmotherS),
     +               pname(1:np), astring(1:nci), matrixs(1:ncmats)
 3001          format(a,a,'->AddNodeOverlap(',a,',',a,',',a,');')
               else
               write(line,3003)cblank(1:nlevel),motherS(1:nmotherS),
     +               pname(1:np), astring1(1:nci1), astring(1:nci), 
     +               matrixs(1:ncmats)
 3003          format(a,a,'->AddNodeOverlap(',a,'->GetVolume(',a,'),',a
     +                ,',',a,');')
               endif
            endif 
            
            nch = lenocc(line)
            if (iposp.eq.0) write(51,'(a)')line(1:nch)
#ifdef __TPCREFSYS__
            if (iposp .eq. 0 .and. mothers(1:nmothers) .eq. 'HALL' .and. 
     +           pname(1:np) .eq. 'CAVE') then
               write(51,3013)
 3013          format(2x,'CAVE->AddNode(TpcRefSys,1);')
            endif
#endif /*  __TPCREFSYS__ */
            npar=q(jv+5)
            if(ninvom.ne.0) then
               call node2(ivom,nuser,iposp)
            endif
 300     continue
      else
*         Print *,'===== DIVISION ====='
*         Print 4567,mother(1:nmother)
         nnodes = nnodes+1
         call toint(nnodes,anode,ncd)
         cnode = 'Nodiv'//anode(1:ncd)
         nd=lenocc(cnode)
         if (nodediv(nnodes).eq.0) then
             nodediv(nnodes) = 1
          endif
         jin=lq(jv-1)
         ivod=q(jin+2)
         if (nvflags(ivod).eq.1) goto 996
         cname=' '
*         if(.not.map_found(iq(jvolum+ivod),cname)) then
            write(cname,'(a4)')iq(jvolum+ivod)
*         endif
            gname = ' '
            write(gname,'(a4)')iq(lq(jvolum-ivod)-4)
*         Print 4445,iq(jvolum+ivod)
 4445    format('daughter division', a4)
         n1=lenocc(cname)
*         if(cname(n1:n1).eq.'+')cname(n1:)='plus'
*         if(cname(n1:n1).eq.'-')cname(n1:)='minus'
*         n1=lenocc(cname)
*         cname=cname(1:n1)//'_0'
*         n2=lenocc(cname)
         iaxis=q(jin+1)
         call toint(iaxis,astring,nci)
         call toreals(3,q(jin+3),creals,ncr)
         line = ' '
         call ptname(cname, pname)
         np = lenocc(pname)
         call realname(cname, rname)
         nrr = lenocc(rname)
#if 1
         nr3 = index(rname,':')-1
         nr4 = nrr
         if (nr3 > 0) nr4 = nr3
         write(line,995)cblank(1:nlevel),pname(1:np),mother(1:nmother),
     +      gname(1:nr4),astring(1:nci), creals(1:ncr), 
     +      pname(1:np), rname(1:nrr)
 995    format(a,'TGeoVolume *',a,' = ',a,'->Divide("',a,'",',a,a,'); '
     + ,a,'->SetTitle("',a,'");')
#else
         write(line,995)cblank(1:nlevel),pname(1:np),mother(1:nmother),
     +      rname(1:nrr),astring(1:nci), creals(1:ncr)
 995    format(a,'TGeoVolume *',a,' = ',a,'->Divide("',a,'",',a,a,');')
#endif
         nch = lenocc(line)
         if (iposp.eq.0) write(51,'(a)')line(1:nch)

         call node2(ivod,0,iposp)
         nvflags(ivod) = 1
      endif

996   continue
      nlevel = nlevel - 1
      if (nlevel.gt.0)call cdnode(nodeold(nlevel))
      end
