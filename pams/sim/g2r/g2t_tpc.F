      FUNCTION  G2T_TPC (g2t_track_h,   g2t_track , 
     >                   g2t_tpc_hit_h, g2t_tpc_hit ) 
      IMPLICIT  NONE
#include "g2t_tpc.inc"
*    - - - - - - - - - 
      Integer         Lnmax
      Parameter       (Lnmax=50)
      Integer         id,trac,next,volume,IQQ(Lnmax)
      Real            x,   xx,   c,   p   ,r,rr,phi,the,eta,tdr,tof,
     >                Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn,QQ(Lnmax)
      common /genhit/ id,trac,next,volume,
     >                x(3),xx(3),c(3),p(4),r,rr,phi,the,eta,tdr,tof,
     >                Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn(3)
      Equivalence     (QQ(1),IQQ(1),id)
*    - - - - - - - - -
      INTEGER   G2T_TPC,G2R_GET_HIT,ISLFLAG,Iprin/0/,Idigi,i
      REAL      G2T_FIELD,par(50),vert(4),pvert(4),Ub(10),
     >          Pt,Vr,mass,charge,tlf,dphi,dtr,Vc,FieldZ
      Integer   Iprt,Ip0/0/,Ivert,It0/0/,Nu,tp,ish,npar
      logical   Local/.true./
      Character Cp*80
*
      G2T_TPC=STAFCV_OK
      if (g2t_tpc_hit_h.maxlen.le.0) return
*
      call G2R_GET_SYS ('TPCE','TPAD',Iprin,Idigi)
      if (Iprin.lt.0) return
      FieldZ = G2T_FIELD (0.)
      Local  = Idigi.ge.2
*
      If (Iprin.ge.3) then
         print *,'   G2T_TPC: Iprin, Idigi =',Iprin, Idigi
         print *,'   name  = ',g2t_tpc_hit_h.name
         print *,'   type  = ',g2t_tpc_hit_h.type
         print *,'   nok   = ',g2t_tpc_hit_h.nok
         print *,'   maxl  = ',g2t_tpc_hit_h.maxlen
         print *,'   dsl_p = ',g2t_tpc_hit_h.dsl_pointer
         print *,'   rbyte = ',g2t_tpc_hit_h.rbytes
      endif
*
      i   = 0
      it0 = 0
      ip0 = 0
      g2t_tpc_hit_h.nok = 0
      Do While (G2R_GET_HIT('tpc') .eq. 0)
*
         if (Elos.lt.0) then
            call Agfpara(ish,npar,par)
            If (Ish.ne.1)    print *,' Awful error in TPC, shape=',Ish,Npar
            if (trac.ne.it0) call gfkine(trac,vert,pvert,iprt,ivert,Ub,nu)
            if (iprt.ne.ip0) call gfpart(iprt,cp,tp,mass,charge,tlf,Ub,nu)
            if (charge.ne.0.and.step.lt.0.01) go to 190
            Pt  = p(4)*sqrt(c(1)**2+c(2)**2)

            if (Pt.gt.0) then
               Vr = 0.0003*FieldZ/Pt
               dphi = min (Step*Vr,1.0)
               dtr  = dphi/8*charge*Sign(Step,XX(3))
               x(1) = min(max(-par(1),x(1)+c(2)*dtr),par(1))
               x(2) = min(max(-par(2),x(2)-c(1)*dtr),par(2))              
            endif
            it0 = trac
            ip0 = iprt
            if (.not.local) call GDTOM(x,xx,1)
         endif
         if (abs(Step*c(1)) .lt. 2*par(1)-0.1) Elos=-abs(Elos)

         i=i+1
         g2t_tpc_hit_h.nok            = i
         g2t_tpc_hit(i).id            = i

         If (local) then
           g2t_tpc_hit(i).x(1)        = x(1)
           g2t_tpc_hit(i).x(2)        = x(2)
           g2t_tpc_hit(i).x(3)        = x(3)
           g2t_tpc_hit(i).p(1)        = c(1)*p(4)
           g2t_tpc_hit(i).p(2)        = c(2)*p(4)
           g2t_tpc_hit(i).p(3)        = c(3)*p(4)
         else
           g2t_tpc_hit(i).x(1)        = xx(1)
           g2t_tpc_hit(i).x(2)        = xx(2)
           g2t_tpc_hit(i).x(3)        = xx(3)
           g2t_tpc_hit(i).p(1)        = p(1)
           g2t_tpc_hit(i).p(2)        = p(2)
           g2t_tpc_hit(i).p(3)        = p(3)
         endif
         g2t_tpc_hit(i).tof           = tof
         g2t_tpc_hit(i).de            = Elos
         g2t_tpc_hit(i).ds            = Step
         g2t_tpc_hit(i).track_p       = trac
         g2t_tpc_hit(i).volume_id     = volume
*
*                  add to track linked list 
*
         g2t_tpc_hit(i).next_tr_hit_p = g2t_track(trac).hit_tpc_p
         g2t_track(trac).hit_tpc_p    = i
         g2t_track(trac).n_tpc_hit    = g2t_track(trac).n_tpc_hit + 1

 190  enddo    

      RETURN
      END



