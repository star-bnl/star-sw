C :>--------------------------------------------------------------------
C : DESCRIPTION: general hit table filling  using it specification
C :              4 bytes per computer word are assumed
C : AUTHOR:      narod
C : ARGUMENTS:   p_vert,  p_vert_h     - vertex table
C :              p_track, p_track_h    - track table
C :              p_hits,  p_hits_h     - hit table
C : HISTORY:     31dec97-v000a-PLN
C : v010         universal hit interface - 08-Jan-00 PN
C :>--------------------------------------------------------------------
      Function g2t_get_hits (   p_vert_h,      p_vert ,
     >                          p_track_h,     p_track,
     >                          p_hits_h,      p_hits)
      IMPLICIT   NONE
#include "PAM.inc"
#include "geant321/gcnum.inc"
#include "geant321/gckine.inc"
      RECORD          /TABLE_HEAD_ST/    p_vert_h
      RECORD          /TABLE_HEAD_ST/    p_track_h
      RECORD          /TABLE_HEAD_ST/    p_hits_h
      INTEGER         p_hits(*),p_track(*),p_vert(*)

*KEEP,genhit.
      Integer         Lnmax,Lnam0,Lnams,Nmh
      Parameter       (Lnmax=50,Lnam0=35,Lnams=Lnmax-Lnam0,Nmh=15)
      Integer         IQHit(Lnmax),id,trac,next,volume
      Real            QQHit(Lnmax),xhloc,xhglb,chloc,pmomg,
     >                radi,rrad,phi,theta,eta,tdr,tof,Slen,
     >                Step,Etot,Lgam,Lpto,Elos,User,Unkn
      common /genhit/ id,trac,next,volume,xhloc(3),xhglb(3),chloc(3),
     >                pmomg(4),radi,rrad,phi,theta,eta,tdr,tof,
     >                Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn(3)
      Equivalence     (id,QQHit,IQHit)
*KEEP,gentit.
      character*4     Gname(Lnmax)  /'id','trac','next','volume',
     >  'X','Y','Z','XX','YY','ZZ','CX','CY','CZ','PX','PY','PZ',
     >  'PTOT','R','RR','PHI','THE','ETA','TDR','TOF','SLEN','STEP',
     >  'ETOT','LGAM','LPTO','ELOS','USER','BIRK','DE','NONE','UNKN',
     >   Lnams*'    '/
*     aliases
      character*4     Tname(Lnmax) /'id','trac','next','volume',
     >  '_', '_', '_', 'x', '_', '_', '_', '_', '_', 'p', '_','_',
     >  '_', '_', '_', '_', '_', '_', '_','tof','s_tr','ds',
     >  'e', '_', '_','de','de','de','de','comp','res',
     >   Lnams*'    '/
*     - - - - - - - - -
      integer         Lnam,jX,jP,iX,iC,iPT,iLP,iET,iE,xD2M,pD2M,
     >                iflg,iadr
      common /gentit/ Lnam,jX,jP,iX,iC,iPT,iLP,iET,iE,xD2M,pD2M,
     >                iflg(Lnmax), iadr(Nmh)
      character*4     Cset,Cdet,Chit
      Real            FHmin,FHmax,FHbin,Hits
      Integer         NVL,NumBV
      common /gensit/ Cset,Cdet, NVL(Nmh),NumBV(Nmh),hits(Nmh),
     >                FHmin(Nmh),FHmax(Nmh),FHbin(Nmh),chit(Nmh)
*KEND

* External functions:
      Integer         G2T_GET_HITS, AgHITSET, AgHITGET, G2T_VOLUME_ID, 
     >                TDM_GET_COLUMN, TDM_GET_CCOUNT, ISLFLAG, IG2TPOS
* Local variables
      Integer         jflg(Lnmax),jadr(Nmh),phits(Nmh),IPOS,Nbytpw,Iprin,
     >                LenT,LenH,Npar,Ish,nh,i,j,k,l,e,f,m,n,ip,in,Ip0,Nu,n0
      Real            G2T_FIELD,Par(50),xxx(10),U(50),Pt,Vr,dphi,dtr,FieldZ
      Character*4     Csys*4/' '/,cnam*4,col*80,typ*80,cp*20
      Parameter       (Nbytpw=4)
      Equivalence     (NAPART,cp)
      IPOS(cnam) =    IG2TPOS(cnam,Gname,Lnam0)
* - -    
      G2T_GET_HITS = STAFCV_BAD
      Iprin  = ISLFLAG('G2TM','PRIN')

      FieldZ = G2T_FIELD (0.)
      If (Iprin.gt.0)  print *,' G2T Mag. Field  =',FieldZ

      LenH   = p_hits_h.rbytes/NBytPW
      LenT   = p_track_h.rbytes/NBytPW

      Call vzero (jadr, Nmh)
      Call vzero (phits,Nmh)
*
      nh = p_hits_h.nok
*     STAR system TLA code is extracted from the table header:
      if   (  p_hits_h.name(1:4) .eq.'g2t_' 
     >  .and. p_hits_h.name(8:11).eq.'_hit') then
        if (Csys.ne.p_hits_h.name(5:7)) then
            Csys =  p_hits_h.name(5:7)
            nh   =  0
        endif
      else 
        print *,' bad input table name :',p_hits_h.name
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
* - - -     
      Itra = 0
      ip0  = 0
      n0   = 0

      DO WHILE (AgHITGET(0,xxx).eq.0)
*
         volume  =   g2t_volume_id (Csys,NUMBV)

C TPC correction: - - - - - - - - - - - - - - - - - - - - - - - - - -
         if (Csys.eq.'tpc') then
            if (NUMBV(3).ne.n0) Call Agfpara(ish,npar,par)
            If (Ish.ne.1) print *,' Awful error in TPC, shape=',Ish,Npar
            if(trac.ne.Itra) Call GfKINE(trac,vert,pvert,ipart,ivert,U,nu)
            if(ipart.ne.ip0) Call GfPART(ipart,cp,ItrTyp,amass,charge,TLife,U,nu)
            n0   = NUMBV(3)
            Itra = trac
            ip0  = ipart
            if (charge.ne.0.and.step.lt.0.01) go to 190

            if (user.lt.0) then
               Vr       = 1.e9 
               Pt       = pmomg(4)*sqrt(Chloc(1)**2+Chloc(2)**2)
               if (Pt.gt.0) Vr = 0.0003*FieldZ/Pt
               dphi     = min (Step*Vr,1.0)
               dtr      = dphi/8*charge*(3-2*NumBV(1))*Step
               xhloc(1) = min(max(-par(1),xhloc(1)+Chloc(2)*dtr),par(1))
               xhloc(2) = min(max(-par(2),xhloc(2)-Chloc(1)*dtr),par(2))
            endif
            if (abs(Step*Chloc(1)).lt.2*par(1)-0.1) User=-abs(User)
            Call GDTOM(xhloc,xhglb,1)
         endif

* hit is selected, add it to track linked list as
*        Track(p_hits.track_p).n_tpc_hit += 1
*        p_hits(nh).next_tr_hit_p = Track(p_hits.track_p).hit_tpc_p
*        Track(p_hits.track_p).hit_tpc_p  = p_hits(nh).id
*
         nh     = nh + 1
         id     = nh
         if (in.gt.0) 
     >      p_track(LenT*(trac-1)+in) = p_track(LenT*(trac-1)+in)+1
         if (ip.gt.0) then
            next                      = p_track(LenT*(trac-1)+ip)
            p_track(LenT*(trac-1)+ip) = id
         endif

         do k=1,n
            if (jadr(k).gt.0)  phits(k)=IQHit(jadr(k))
         enddo
         Call ucopy (phits,p_hits(LenH*(nh-1)+1),LenH)
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

