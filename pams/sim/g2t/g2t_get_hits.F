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
      Integer         Lnmax,Lnam0,Lnam,Nbytpw,Nmh,Npar,Ish
      Parameter       (Lnmax=50,Lnam0=35,Nbytpw=4,Nmh=25)
      INTEGER         G2T_GET_HITS,p_hits(*),p_track(*),p_vert(*)
      Integer         nh/0/,i,j,k,l,e,f,m,n,ip,in,jX,jP,iX,iC,iPT,iLP,
     >                iET,LenT,LenH,xD2M,pD2M,IH,JTRA,Iprin
      Real            VMOD,Hits(Nmh),FHmin(Nmh),FHmax(Nmh),FHbin(Nmh),Par(50)
      Integer         IQQ(Lnmax),iadr(Nmh),jadr(Nmh),NUMBV(Nmh),phits(Nmh)
      Character*4     Csys*4/' '/,chit(Nmh)*4,cnam
      Character*80    col,typ,cp
      real            xxx(3),ccc(3),QQ(Lnmax)
      Equivalence     (QQ(1),IQQ(1),id)
      character*4     Gname(Lnmax),Tname(Lnmax)
      Integer         iflg(Lnmax), jflg(Lnmax)
      Integer         id,trac,next,volume
      Real            x,   xx,   c,   p   ,r,rr,phi,the,eta,
     >                tdr,tof,Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn
      common /genhit/ id,trac,next,volume,
     >                x(3),xx(3),c(3),p(4),r,rr,phi,the,eta,
     >                tdr,tof,Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn(3)
      data        (Gname(i),i=1,Lnam0) /'id','trac','next','volume',
     >  'X','Y','Z','XX','YY','ZZ','CX','CY','CZ','PX','PY','PZ',
     >  'PTOT','R','RR','PHI','THE','ETA','TDR','TOF','SLEN','STEP',
     >  'ETOT','LGAM','LPTO','ELOS','USER','BIRK','DE','NONE','UNKN'/
      data        (Tname(i),i=1,Lnam0) /'id','trac','next','volume',
     >  '_', '_', '_', 'x', '_', '_', '_', '_', '_', 'p', '_','_',
     >  '_', '_', '_', '_', '_', '_', '_','tof','s_tr','ds',  
     >  'e', '_', '_','de','de','de','de','comp','res' /
      data            Lnam/Lnam0/
      Integer         TDM_GET_SPEC, TDM_GET_COLUMN, TDM_GET_CCOUNT
      External        TDM_GET_SPEC, TDM_GET_COLUMN, TDM_GET_CCOUNT
      Integer         AgFHIT1, ISLFLAG, G2T_VOLUME_ID, IG2TPOS
      External        AgFHIT1, ISLFLAG, G2T_VOLUME_ID, IG2TPOS
      Real            G2T_FIELD, vert(4),pvert(4),Ub(10),
     >                Pt,Vr,mass,charge,tlf,dphi,dtr,Vc,FieldZ
      Integer         IPOS,Iprt,Ip0/0/,Ivert,It0/0/,Nu,tp
      IPOS(cnam) =    IG2TPOS(cnam,Gname,Lnam0)
* - -    
      G2T_GET_HITS = STAFCV_BAD
      Iprin  = ISLFLAG('G2TM','PRIN')

      FieldZ = G2T_FIELD (0.)
      If (Iprin.gt.0)  print *,' G2T Mag. Field  =',FieldZ

      LenH   = p_hits_h.rbytes/NBytPW
      LenT   = p_track_h.rbytes/NBytPW
      call vzero (QQ,   Lnmax)
      call vzero (iflg, Lnmax)
      call vzero (iadr, Nmh)
      call vzero (jadr, Nmh)
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
      iX  = 0
      iC  = 0
      jX  = 0
      jP  = 0
      iET = 0
      iPT = 0
      iLP = 0
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
      nh = p_hits_h.nok
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
      if (Iprin.gt.0) then
         if (ip.eq.0) print *,'hit_'//Csys(1:3)//'_p not found in g2t_track'
         if (in.eq.0) print *,'n_'//Csys(1:3)//'_hit not found in g2t_track'
      endif
*
*     3. prepare map to unfold an input hit into the full hit
*     Each filled variable in the extended hit is flagged by iflg - 
*     its position in measurements. Iadr is measurement reference.
*     Positive Iflg means direct filling (unpacking),
*     negative Iflg means this variable should be calculated
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
      xD2M=iflg(iX)+iflg(iX+1)+iflg(iX+2)
      pD2M=iflg(iC)+iflg(iC+1)+iflg(iC+2)
      if (Iprin.ge.3) then
         print *,' iX,iC,jX,jP iflg=',iX,iC,jX,jP
         print *,' D2M  flags  are = ',xD2M,pD2M
         print *,' pointer/counter position =',ip,in
      endif

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
         print *,'hits are filling the following standard elements'     
         print *,Gname
         print *,iflg
      endif
*
*     4. prepare map to fold full hit into table description
*
      n=0
      do k=1,TDM_GET_CCOUNT (p_hits_h)
         i = TDM_GET_COLUMN (p_hits_h,k-1,col,typ,l,e,m)
         if (Iprin.ge.3) print *,'    k,col=',
     >                   k,i,l,e,m,' ',col(1:i),' t=',typ(1:10)
         do j=1,Lnam
            if (col(1:min(i-1,4)).eq.Tname(j).and.iflg(j).ne.0) then
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

      call VZERO(numbv,Nmh)
      it0 = 0
      ip0 = 0
* - - -     
      DO WHILE (agfhit1(ih,jtra,numbv,hits).eq.0)
*
         trac   = abs(jtra)
         if (abs(ih).eq.1) then
            call Agfpath(numbv)
            call Agfpara(ish,npar,par)
         endif 
         do k=1,Nmh
            if (iadr(k).gt.0) QQ(iadr(k))=(hits(k))
         enddo

C        overwrite global coordinates with local if they are not measured

         volume  =   g2t_volume_id (Csys,NUMBV)
         if ( iflg(iPT).le.0.and.iflg(iLP).gt.0) p(4)=10.**Lpto
         if ( iflg(iLP).le.0.and.iflg(iPT).gt.0) Lpto=alog10(p(4))
         Vc  = VMOD(c,3)
         do i=1,3
            c(i)=c(i)/Vc
         enddo

C TPC correction: - - - - - - - - - - - - - - - - - - - - - - - - - -
         if (Csys.eq.'tpc') then
            if (user.lt.0) then

               If (Ish.ne.1)    print *,' Awful error in TPC, shape=',Ish,Npar
               if (trac.ne.it0) call gfkine(trac,vert,pvert,iprt,ivert,Ub,nu)
               if (iprt.ne.ip0) call gfpart(iprt,cp,tp,mass,charge,tlf,Ub,nu)
               if (charge.ne.0.and.step.lt.0.01) go to 190
               Vr  = 1.e9 
               Pt  = p(4)*sqrt(c(1)**2+c(2)**2)
               if (Pt.gt.0) Vr = 0.0003*FieldZ/Pt
               dphi = min (Step*Vr,1.0)
               dtr  = dphi/8*charge*(3-2*NumBV(1))*Step

               x(1) = min(max(-par(1),x(1)+c(2)*dtr),par(1))
               x(2) = min(max(-par(2),x(2)-c(1)*dtr),par(2))              
               it0 = trac
               ip0 = iprt
            endif
            if (abs(Step*c(1))<2*par(1)-0.1) User=-abs(User)
         endif
C                 - - - - - - - - - - - - - - - - - - - - - - - - - -
         if ( xD2M .ne. 0)  call GDTOM(x,xxx,1)
         if ( pD2M .ne. 0)  call GDTOM(c,ccc,2)

         do i=1,3
            if (iflg(jX+i-1).le.0.and.xD2M.gt.0) xx(i)=(xxx(i))
            if (iflg(jP+i-1).le.0.and.pD2M.gt.0) p(i)=(ccc(i)*p(4))
         enddo

* hit is selected:
         nh     = nh + 1
         id     = nh
*         print *,nh,' X, c =',x,c
*         print *,nh,' XX,cc=',xxx,ccc
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
            if (jadr(k).gt.0)  phits(k)=IQQ(jadr(k))
         enddo
         call ucopy (phits,p_hits(LenH*(nh-1)+1),LenH)
         p_hits_h.nok = nh
 190  enddo

 200  G2T_GET_HITS = STAFCV_OK
      end

*****************************************************************************

      Function IG2TPOS(name,table,L)
      character*4  name,table(L)
      do i=1,L
         Ig2tpos=i
         if (name.eq.table(i)) return
      enddo
         Ig2tpos=0
      end

