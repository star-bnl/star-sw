C :>--------------------------------------------------------------------
C : DESCRIPTION: general hit table filling  using it specification
C :              4 bytes per computer word are assumed
C : AUTHOR:      narod
C : ARGUMENTS:   p_vert,  p_vert_h     - vertex table
C :              p_track, p_track_h    - track table
C :              p_hits,  p_hits_h     - hit table
C : HISTORY:     31dec97-v000a-PLN
C :>--------------------------------------------------------------------
      function g2t_get_hits (   p_vert_h,      p_vert ,
     >                          p_track_h,     p_track,
     >                          p_hits_h,      p_hits)
      IMPLICIT   NONE
#include "PAM.inc"
#include "geant321/gcnum.inc"
      RECORD          /TABLE_HEAD_ST/    p_vert_h
      RECORD          /TABLE_HEAD_ST/    p_track_h
      RECORD          /TABLE_HEAD_ST/    p_hits_h
      Integer         Lnmax,Lnam0,Lnam,Nbytpw,Nmh
      Parameter       (Lnmax=50,Lnam0=35,Nbytpw=4,Nmh=15)
      INTEGER         G2T_GET_HITS,p_hits(*),p_track(*),p_vert(*)
      integer         nh/0/,i,j,k,l,e,f,m,n,ip,in,jX,jP,iX,iC,
     >                LenT,LenH,xD2M,pD2M,IH,JTRA,Iprin
      Real            Hits(Nmh),FHmin(Nmh),FHmax(Nmh),FHbin(Nmh)
      integer         IQQ(100),iadr(Nmh),jadr(Nmh),NUMBV(Nmh),phits(Nmh)
      Character*4     Csys*4/' '/,chit(Nmh)*4,col*80,typ*80
      real            xxx(3),ccc(3),QQ(100)
      Equivalence     (QQ(1),IQQ(1),id)

      character*4     Gname(Lnmax),Tname(Lnmax)
      integer         iflg(Lnmax), jflg(Lnmax)
      Integer         id,trac,next,volume
      Real            x,   xx,   c,   p   ,r,rr,phi,the,eta,
     >                tdr,tof,Slen,Step,Etot,Lgam,Shtn,Elos,User,Unkn
      common /genhit/ id,trac,next,volume,
     >                x(3),xx(3),c(3),p(4),r,rr,phi,the,eta,
     >                tdr,tof,Slen,Step,Etot,Lgam,Shtn,Elos,User,Unkn(3)
      data        (Gname(i),i=1,Lnam0) /'id','trac','next','volume',
     >  'X','Y','Z','XX','YY','ZZ','CX','CY','CZ','PX','PY','PZ',
     >  'PTOT','R','RR','PHI','THE','ETA','TDR','TOF','SLEN','STEP',
     >  'ETOT','LGAM','SHTN','ELOS','USER','BIRK','DE','NONE','UNKN'/
      data        (Tname(i),i=1,Lnam0) /'id','trac','next','volume',
     >  '_', '_', '_', 'x', '_', '_', '_', '_', '_', 'p', '_','_',
     >  '_', '_', '_', '_', '_', '_', '_','tof','s_tr','ds',  
     >  'e', '_', '_','de','de','de','de','comp','res' /
      data            Lnam/Lnam0/
      Integer         TDM_GET_SPEC, TDM_GET_COLUMN, TDM_GET_CCOUNT
      External        TDM_GET_SPEC, TDM_GET_COLUMN, TDM_GET_CCOUNT
      Integer         AgFHIT1, ISLFLAG, G2T_VOLUME_ID
      External        AgFHIT1, ISLFLAG, G2T_VOLUME_ID
*   
      G2T_GET_HITS = STAFCV_BAD
      Iprin = ISLFLAG('G2TM','PRIN')
      LenH  = p_hits_h.rbytes/NBytPW
      LenT  = p_track_h.rbytes/NBytPW
      call vzero (QQ,   100)
      call vzero (iflg, Lnmax)
      call vzero (iadr, Nmh)
      call vzero (phits,Nmh)
      if (Iprin.ge.4) then
         print *,'   name  = ',p_hits_h.name
         print *,'   type  = ',p_hits_h.type
         print *,'   nok   = ',p_hits_h.nok
         print *,'   maxl  = ',p_hits_h.maxlen
         print *,'   dsl_p = ',p_hits_h.dsl_pointer
         print *,'   rbyte = ',p_hits_h.rbytes
      endif
*
*     1. flag information which is hardwired:
*     - what comes directly (not measurments) should be in both tables - (T&G)
*     - set D2M conversion flags if any of xyz or Cxyz are measured.
*      
      iX=0
      iC=0
      jX=0
      jP=0
      do j=1,Lnam
         If (Gname(j).eq.Tname(j)) iflg(j)=99
         If (Gname(j).eq. 'X'  )   iX=j
         If (Gname(j).eq. 'CX' )   iC=j
         If (Gname(j).eq. 'XX' )   jX=j
         If (Gname(j).eq. 'PX' )   jP=j
      enddo
      if (iX*iC*jX*jP.le.0) print *,' errenious G table ',iX,iP,jX,jP

*     STAR system TLA code is extracted from the table header:
      if   (  p_hits_h.name(1:4) .eq.'g2t_' 
     >  .and. p_hits_h.name(8:11).eq.'_hit') then
        if (Csys.ne.p_hits_h.name(5:7)) then
            Csys =  p_hits_h.name(5:7)
            nh   =  0
        endif
      endif
*
*     2. locate track descriptors in output hit table:
*     use star subsystem TLA to map ip - hit pointer and in - hit counter
*   
      n  = 0
      ip = 0
      in = 0
      do k=1,TDM_GET_CCOUNT (p_track_h)
         i = TDM_GET_COLUMN (p_track_h,k-1,col,typ,l,e,m)
         if (col(1:i-1) .eq. 'hit_'//Csys(1:3)//'_p' ) ip=n+1
         if (col(1:i-1) .eq.  'n_'//Csys(1:3)//'_hit') in=n+1
         n=n+e
      enddo
      if (ip.eq.0) print *,'hit_'//Csys(1:3)//'_p not found in g2t_track'
      if (in.eq.0) print *,'n_'//Csys(1:3)//'_hit not found in g2t_track'
*
*     3. prepare map to unfold an input hit into full hit
*     Each filled variable in the extended hit is flagged by iflg - 
*     its position in measurements. Iadr is measurement reference.
*
      Call AgFDPAR (hits,Chit,FHmin,FHmax,FHbin)
      if  (Iprin.ge.4)  print *,' chit=',chit
      do k=1,Nmh
         if (chit(k).eq.' ') go to 101
         do j=1,Lnam
            if (chit(k).eq.Gname(j)) go to 100
         enddo
         if (Iprin.ge.2) print *,'G2T: introducing new hit element ',chit(k)
         j=min(Lnam+1,Lnmax)
         Gname(j) = chit(k)
         Tname(j) = chit(k)
         call CUTOL(Tname(j))
 100     iadr(k)  = j
         iflg(j)  = k
      enddo         
 101  continue
*
*     4. prepare map to fold full hit into table description
*
      n=0
      do k=1,TDM_GET_CCOUNT (p_hits_h)
         i = TDM_GET_COLUMN (p_hits_h,k-1,col,typ,l,e,m)
         if (Iprin.ge.3) print *,'    k,col=',
     >                   k,i,l,e,m,' ',col(1:i),' t=',typ(1:10)
         do j=1,Lnam
            if (col(1:min(i-1,4)).eq.Tname(j).and.iflg(j).gt.0) then
               Do f=1,e
                  jadr(n+f)=j+f-1
                  jflg(j)=n+f
               enddo
            endif
         enddo
         if (jadr(n+1).le.0) print *,' G2T warning: table element ',
     >                                 col(1:i),' not found '
         n=n+e
      enddo
      if (LenH.ne.n) then
         print *,' G2T: unexpected table structure',p_hits_h.rbytes,n
         return
      endif

      xD2M=iflg(iX)+iflg(iX+1)+iflg(iX+2)
      pD2M=iflg(iC)+iflg(iC+1)+iflg(iC+2)
      if (Iprin.ge.3) then
         print *,' iX,iC,jX,jP iflg=',iX,iC,jX,jP
         print *,' D2M  flags  are = ',xD2M,pD2M
         print *,' pointer/counter position =',ip,in
      endif
      call VZERO(numbv,Nmh)
*
* - - -     
      DO WHILE (agfhit1(ih,jtra,numbv,hits).eq.0)
*
         nh     = nh + 1
         id     = nh
         trac   = abs(jtra)
         volume = g2t_volume_id (Csys,NUMBV)
         if (csys(1:3).eq.'tpc' .and. numbv(1).le.1 .and. hits(3).le.0)
     >      print *,' tpc hit ',(numbv(i),i=1,3),(hits(i),i=1,3)
         do k=1,Nmh
            if (iadr(k).gt.0) QQ(iadr(k))=hits(k)
         enddo

C        overwrite global coordinates with local if they are not measured
         if (abs(ih).eq.1)  call Agfpath(numbv)
         if ( xD2M .ne. 0)  call GDTOM(x,xxx,1)
         if ( pD2M .ne. 0)  call GDTOM(c,ccc,2)
         do i=1,3
            if (iflg(jX+i-1).eq.0.and.xD2M.gt.0) xx(i)=xxx(i)
            if (iflg(jP+i-1).eq.0.and.pD2M.gt.0) p(i)=ccc(i)*p(4)
         enddo
*
*                  add to track linked list 
*        Track(p_hits.track_p).n_tpc_hit += 1
*        p_hits(nh).next_tr_hit_p = Track(p_hits.track_p).hit_tpc_p
*        Track(p_hits.track_p).hit_tpc_p  = p_hits(nh).id
*
         if (in.gt.0) 
     >      p_track(LenT*(trac-1)+in) = p_track(LenT*(trac-1)+in)+1
         if (ip.gt.0) then
            next                      = p_track(LenT*(trac-1)+ip)
            p_track(LenT*(trac-1)+ip) = id
         endif

         do k=1,n
            if (jadr(k).gt.0) phits(k)=IQQ(jadr(k))
         enddo
         call ucopy (phits,p_hits(LenH*(nh-1)+1),LenH)
         p_hits_h.nok = nh
      enddo

      G2T_GET_HITS = STAFCV_OK

      RETURN
      end



