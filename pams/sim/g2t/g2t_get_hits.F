* $Id: g2t_get_hits.F,v 1.26 2000/03/03 21:54:19 nevski Exp $
* $Log: g2t_get_hits.F,v $
* Revision 1.26  2000/03/03 21:54:19  nevski
* protection against bad track number
*
* Revision 1.25  2000/03/03 20:51:24  nevski
* add protection against corrupted Itrac
*
* Revision 1.24  2000/01/23 19:04:42  nevski
* unused variables deleted
*
* Revision 1.23  2000/01/12 00:10:45  nevski
* add cvs headers
*
*
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
#include "geant321/gcflag.inc"
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
      Integer         G2T_GET_HITS, AgHITGET, G2T_VOLUME_ID, 
     >                TDM_GET_COLUMN, TDM_GET_CCOUNT, ISLFLAG
* Local variables
      Integer         jflg(Lnmax),jadr(Nmh),phits(Nmh),Nbytpw,Iprin,Jtra,Nb,
     >                LenT,LenH,Npar,Ish,nh,i,j,k,l,e,f,m,n,ip,in,Ip0,Nu,n0
      Real            G2T_FIELD,Par(50),xxx(10),U(50),Pt,Vr,dphi,dtr,FieldZ
      Character*4     Csys*4/' '/,col*80,typ*80,cp*20
      Parameter       (Nbytpw=4)
      Equivalence     (NAPART,cp)
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
      Jtra = 0
      ip0  = 0
      n0   = 0
      nb   = 0

      DO WHILE (AgHITGET(0,xxx).eq.0)
         if (abs(trac)>Ntrack) then
            nb = nb + 1
            go to 190
         endif
*
         volume  =   g2t_volume_id (Csys,NUMBV)

C TPC correction: - - - - - - - - - - - - - - - - - - - - - - - - - -
         if (Csys.eq.'tpc') then
*           skip short after-steps
            if (NUMBV(3).eq.n0 .and. trac.eq.jtra .and.step.le.0.01) go to 190

            if (NUMBV(3).ne.n0) Call Agfpara(ish,npar,par)
            If (Ish.ne.1) print *,' Awful error in TPC, shape=',Ish,Npar
            n0   = NUMBV(3)
            Jtra = trac

            if (user.lt.0) then
               if(trac.ne.Itra) Call GfKINE(trac,vert,pvert,ipart,ivert,U,nu)
               if(ipart.ne.ip0) Call GfPART(ipart,cp,ItrTyp,amass,charge,TLife,
     >                                                                  U,nu)
               Itra     = trac
               ip0      = ipart
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
      if (nb.eq.0) return

      print *,' G2R_get_hits ERROR in ',Cset,' ',Cdet,': ',nb,' bad hits'
      IEOTRI = 1

      G2T_GET_HITS = STAFCV_BAD    

      end






