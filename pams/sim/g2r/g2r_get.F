* ----------------------------------------------
      subroutine  G2R_GET_SYS (Cset,Cdet,iprin,idigi)
      IMPLICIT    NONE
*
      character*4     Cset, Cdet
*    - - - - - - - - - 
      Integer         Lnmax,Lnam0,Nmh
      Parameter       (Lnmax=50,Lnam0=35,Nmh=15)
*    
      real            xxx(3),ccc(3),QQ(Lnmax)
      Integer         id,trac,next,volume,IQQ(Lnmax)
      Real            x,   xx,   c,   p   ,r,rr,phi,the,eta,
     >                tdr,tof,Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn
      common /genhit/ id,trac,next,volume,
     >                x(3),xx(3),c(3),p(4),r,rr,phi,the,eta,
     >                tdr,tof,Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn(3)
      Equivalence     (QQ(1),IQQ(1),id)
*
      integer         Lnam,jX,jP,iX,iC,iPT,iLP,iET,iE,xD2M,pD2M,iflg,iadr
      common /gentit/ Lnam,jX,jP,iX,iC,iPT,iLP,iET,iE,xD2M,pD2M,
     >                iflg(Lnmax), iadr(Nmh)
*    - - - - - - - - -
*
      character*4 Gname(Lnmax),Tname(Lnmax)
      data        Lnam/Lnam0/
      data        (Gname(i),i=1,Lnam0) /'id','trac','next','volume',
     >  'X','Y','Z','XX','YY','ZZ','CX','CY','CZ','PX','PY','PZ',
     >  'PTOT','R','RR','PHI','THE','ETA','TDR','TOF','SLEN','STEP',
     >  'ETOT','LGAM','LPTO','ELOS','USER','BIRK','DE','NONE','UNKN'/
*
      Real            Hits(Nmh),FHmin(Nmh),FHmax(Nmh),FHbin(Nmh)
      Character*4     Csys*4/' '/,chit(Nmh)*4
      Integer         AgFHIT0,ISLFLAG,Iprin,Idigi,i,j,k,nh
      External        AgFHIT0,ISLFLAG
*   
      Iprin = -1
      if (AgFHIT0(Cset,Cdet) .ne. 0) return
      Iprin = ISLFLAG(Cset,'PRIN')
      Idigi = ISLFLAG(Cset,'DIGI')
      if (ISLFLAG('G2TM','PRIN').gt.0) 
     >    print *,' for idet= ',Cset,' iprin,idigi= ',Iprin,Idigi

      call vzero (QQ,   Lnmax)
      call vzero (iflg, Lnmax)
      call vzero (iadr, Nmh)
      call vzero (hits, Nmh)
*
*     1. flag  hardwired information
      iX  = 0
      iC  = 0
      jX  = 0
      jP  = 0
      iET = 0
      iPT = 0
      iLP = 0
      iE  = 0
      do j=1,Lnam
         If (Gname(j).eq.Tname(j))  iflg(j)=99
         If (Gname(j).eq. 'X'   )   iX=j
         If (Gname(j).eq. 'CX'  )   iC=j
         If (Gname(j).eq. 'XX'  )   jX=j
         If (Gname(j).eq. 'PX'  )   jP=j
         If (Gname(j).eq. 'ETOT')   iET=j
         If (Gname(j).eq. 'PTOT')   iPT=j
         If (Gname(j).eq. 'LPTO')   iLP=j
      enddo
      if (iX*iC*jX*jP.le.0) print *,' errenious G1 table ',iX,iC,jX,jP
      if (iET*iPT*iLP.le.0) print *,' errenious G2 table ',iET,iPT,iLP
*
*     2. prepare map to unfold an input hit into the full hit
      Call AgFDPAR (hits,Chit,FHmin,FHmax,FHbin)

      nh=0
      do k=1,Nmh
         if (chit(k).eq.' ') go to 101
         nh=nh+1
         do j=1,Lnam
            if (chit(k).eq.Gname(j)) go to 100
         enddo
         if (Iprin.ge.0) print *,'G2T: introducing new hit element ',chit(k)
         j=min(Lnam+1,Lnmax)
         Gname(j) = chit(k)
         Tname(j) = chit(k)
         call CUTOL(Tname(j))
 100     iadr(k)  = j
         iflg(j)  = k
         If (chit(k).eq.'ELOS')   iE=j
         If (chit(k).eq.'USER')   iE=j
         If (chit(k).eq.'BIRK')   iE=j

      enddo         
 101  continue
*
*     3. transformations
      xD2M=iflg(iX)+iflg(iX+1)+iflg(iX+2)
      pD2M=iflg(iC)+iflg(iC+1)+iflg(iC+2)

*     transformation rules: Ptot<->LPtot, Etot<->Ptot
      if (iflg(iPT).eq.0) iflg(iPT)=-iflg(iLP)
      if (iflg(iLP).eq.0) iflg(iLP)=-iflg(iPT)

*     transformation rules: cos<->Pi, Xloc<->Xglob
      do i=0,2
         if (iflg(jP+i).eq.0) iflg(jP+i)=-iflg(iC+i)
         if (iflg(iC+i).eq.0) iflg(iC+i)=-iflg(jP+i)
         if (iflg(jX+i).eq.0) iflg(jX+i)=-iflg(iX+i)
         if (iflg(iX+i).eq.0) iflg(iX+i)=-iflg(jX+i)
      enddo
      
      If (Iprin.ge.3) then
         print *,'hits provide the following elements'     
         do j=1,Nh
            print *,Chit(j),iadr(j)
         enddo
         print *,'filling the following standard items'     
         do j=1,Lnam
            if (iflg(j).ne.0) print *,Gname(j),iflg(j)
         enddo
      endif
      id = 0
*
      END
*
* --------------------------------------------------------------------
*
      Function   G2R_GET_HIT (Csys)
      IMPLICIT   NONE
*
*    - - - - - - - - - 
      Integer         Lnmax,Lnam0,Nmh
      Parameter       (Lnmax=50,Lnam0=35,Nmh=15)
*    
      real            xxx(3),ccc(3),QQ(Lnmax)
      Integer         id,trac,next,volume,IQQ(Lnmax)
      Real            x,   xx,   c,   p   ,r,rr,phi,the,eta,
     >                tdr,tof,Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn
      common /genhit/ id,trac,next,volume,
     >                x(3),xx(3),c(3),p(4),r,rr,phi,the,eta,
     >                tdr,tof,Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn(3)
      Equivalence     (QQ(1),IQQ(1),id)
*
      integer         Lnam,jX,jP,iX,iC,iPT,iLP,iET,iE,xD2M,pD2M,iflg,iadr
      common /gentit/ Lnam,jX,jP,iX,iC,iPT,iLP,iET,iE,xD2M,pD2M,
     >                iflg(Lnmax), iadr(Nmh)
*    - - - - - - - - -
      Integer         G2R_GET_HIT,G2T_VOLUME_ID,AgFHIT1,ih,jtra,i,k
      real            hits(nmh)
      integer         NUMBV(Nmh)/Nmh*0/
      character*3     Csys

         i=AgFHIT1(ih,jtra,numbv,hits)
         G2R_GET_HIT = i
         if (i.ne.0) return
*
         id     = id + 1
         trac   = abs(jtra)
         do k=1,Nmh
            if (iadr(k).gt.0) QQ(iadr(k))=(hits(k))
         enddo

*        overwrite global coordinates with local if they are not measured
         if (abs(ih).eq.1)  call Agfpath(numbv)
         volume  =   g2t_volume_id (Csys,NUMBV)
         if ( xD2M .ne. 0)  call GDTOM(x,xxx,1)
         if ( pD2M .ne. 0)  call GDTOM(c,ccc,2)
         if ( iflg(iPT).le.0.and.iflg(iLP).gt.0) p(4)=10.**Lpto
         if ( iflg(iLP).le.0.and.iflg(iPT).gt.0) Lpto=alog10(p(4))
         do i=1,3
            if (iflg(jX+i-1).le.0.and.xD2M.gt.0) xx(i)=(xxx(i))
            if (iflg(jP+i-1).le.0.and.pD2M.gt.0) p(i)=(ccc(i)*p(4))
         enddo
         if (iE.gt.0) then
            Elos=QQ(iE)
         endif
      end




