*_______________________________________________________________________
      Subroutine markdiv2(ivo,nuserm)
      call markdiv(ivo,nuserm)
      End

*_______________________________________________________________________
       Subroutine markdiv(ivo,nuserm)
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
#include "geant321/gcnum.inc"
#endif
      common/cnodes/nnodes
      common/clevel/nodeold(20),nlevel

      parameter (MAXPOS=250000)
      common/cnpos/nodepos(MAXPOS),nodediv(MAXPOS),nvflags(MAXPOS),
     +npflags(MAXPOS),nppflags(MAXPOS)  

*      dimension qjv(1000)
      character*20 cname
      character*20 gname
*---------------------------------------------------------------------
      nlevel = nlevel + 1
      nodeold(nlevel) = nnodes
      jv=lq(jvolum-ivo)
      ishape = q(jv+2)
      nin = q(jv+3)
*-  Loop subnodes
      if(nin.eq.0)then
         nlevel=nlevel-1
         return
      endif
      call cdnode(nodeold(nlevel))
      if(nin.gt.0)then
            if (nvflags(ivo).ne.0) then
               goto 996
            endif
            nvflags(ivo)=1
*                    Volume has positioned contents
         do 300 in=1,nin
            jin=lq(jv-in)
*yf             if (jin .eq. 0) goto 300
            ivom=q(jin+2)
            nuser  = q(jin+3)
            jinvom = lq(jvolum-ivom)
            ninvom = q(jinvom+3)
            cname=' '
            gname=' '
            write(cname,'(a4)')iq(jvolum+ivom)
            write(gname,'(a4)')iq(lq(jvolum-ivom)-4)
            n1=lenocc(cname)
            if(ninvom.ge.0)then
               nnodes = nnodes+1
               if (nnodes.gt.MAXPOS) then
                  print *,'Too many nodes =',nnodes
                  go to 300
               endif
               if (nodepos(nnodes).eq.0) then
                  nodepos(nnodes) = 1
               endif
            endif
            if(ninvom.ne.0) then
               call markdiv2(ivom,nuser)
            endif
 300     continue
      else
         nnodes = nnodes+1
         if (nodediv(nnodes).eq.0) then
            nodediv(nnodes) = 1
         endif
         jin=lq(jv-1)
         ivod=q(jin+2)
         if (nvflags(ivod).gt.0) goto 996
         cname=' '
         gname=' '
         write(cname,'(a4)')iq(jvolum+ivod)
         write(gname,'(a4)')iq(lq(jvolum-ivod)-4)
         n1=lenocc(cname)
         Print 200, cname(1:n1)
 200     format('Division volume ', a4)
         call markdiv2(ivod,0)
         nvflags(ivod) = 2
      endif

996   continue
      nlevel = nlevel - 1
      if (nlevel.gt.0)call cdnode(nodeold(nlevel))
      end
