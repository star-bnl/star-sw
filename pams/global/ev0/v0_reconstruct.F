      INTEGER FUNCTION V0_RECONSTRUCT(trak_h,trak,
     +     ev0par_h,ev0par,
     +     vertex_h,vertex,
     +     ev0out_h,ev0out,
     +     bmag_h, bmag)
 

      IMPLICIT NONE
C     Functional Description : Search for secondary vertices. 
C     K_0_short and Lambda particles
C     and anti-particles are 
C     considered. Only charged decay modes.
C     Created  MARCH-1994	  S. Margetis
C     Error conditions	: None-Yet
C     Status		: Tested with svt_hits
C                       :  JUNE-1997  H. Caines 
C                          Order in which calcs/tests were made was altered to
C                          improve speed of search
C
C     Modifications:
C         1.  Removed use of obsolete table evr_privert; L.Ray, 8/19/98
C----------------------------------------------------------------------
#include "PAM.inc"
#include "dst_vertex.inc"
#include "ev0_aux.inc"
#include "ev0_ev0par.inc"
#include "ev0_track.inc"
#include "mft_control.inc"
C---------------
  
      RECORD/ table_head_st/ ev0par_h
      RECORD/ ev0_ev0par_st/ ev0par(*)
      RECORD/ table_head_st/ vertex_h
      RECORD/ dst_vertex_st/ vertex(*)
      RECORD/ table_head_st/ ev0out_h
      RECORD/ ev0_aux_st/ ev0out(*)
      RECORD/ table_head_st/ trak_h
      RECORD/ ev0_track_st/ trak(*)
      RECORD/ table_head_st/ bmag_h
      RECORD/ mft_control_st/ bmag(*)
    
      INTEGER 	ok
      INTEGER  	jk
      INTEGER   id,id1,id2
      INTEGER   ll,iflag,iflag1
      INTEGER   i,j,index
      
      REAL	rd_cut,rv_cut,rmin_cut,mass_id(3)
      REAL	mass1(3),mass2(3),pi,pst1(3)
      REAL	rv,pst2(3)
      REAL	pdotp,ndotp,pv0,pv0sq,ppar,npar,pper
      REAL	trk1(6),trk2(6)
      REAL	xcom(2),ycom(2),inv_mass,rmin
      REAL	xp(2),xn1(2),xn2(2),rd,xpp(3),massl(2),pp(3)
      REAL      env0,bv0,gv0,ep,ppar_cm,mainv(3),rap
      REAL      MASST(50)
      real      rdcheck(2),open,dot ! wkw
      LOGICAL	first, updated
      DATA MASS1/0.139567,.93828,.139567/
      DATA MASS2/0.139567,.139567,.93828/
      DATA MASST/7*0.0005,.139567,.139567
     +,            3*0.493667,2*.93828,.93828
     +,            35*0./

*
*
*-----executable statements--------------------------------------------
*
      pi=acos(-1.)
*
      id=ev0par(1).id
      index=1
      IF(id.eq.16) THEN
	mass_id(1)=0.49767
        id1=8 ! positive
        id2=9 ! negative
        index=1
      ELSEIF(id.eq.18) THEN
	mass_id(2)=1.11563
        id1=14
        id2=9
        index=2
      ELSEIF(id.eq.26) THEN
	mass_id(3)=1.11563
        id1=8
        id2=15
        index=3
      ENDIF
	rd_cut=ev0par(index).dca**2
	rv_cut=ev0par(index).dlen
	rmin_cut=ev0par(index).dcav0**2
*
CC:  Load Primary vertex position coordinates:

      mainv(1) = 0.0
      mainv(2) = 0.0
      mainv(3) = 0.0
      if(vertex_h.nok .gt. 0) then
         do i = 1,vertex_h.nok
            if(vertex(i).vtx_id .eq. 1) then
               mainv(1) = vertex(i).x
               mainv(2) = vertex(i).y
               mainv(3) = vertex(i).z
               go to 100
            end if
         end do
100      continue
      end if

*-----steering routine------------------------------------------------
*     
        
        ok=0
        first = .true.
        DO 10 i=1,trak_h.nok
*     First track is a positive
           
           IF(trak(i).q .lt. 0) GOTO 10
           
*     
*decide here if dE/dx,TOF can say anything about PID
*if yes .and. it is a pion/proton pair proceed if not
*take GEANT ID or a (hypothetical) mass will be assigned
*     
           IF(id.ne.0) THEN
              IF(trak(i).pid.ne.id1) GOTO 10
           ENDIF
           
*           trki(1)=trak(i).r0
*           trki(2)=trak(i).phi
*           trki(3)=trak(i).z0
*           trki(4)=trak(i).psi
*           trki(5)=trak(i).tanl
*           trki(6)=trak(i).q
           
           
C--   calculate impact parameter at main vertex--- wkw
           
           call ev0_project_track(trak(i).xc,trak(i).rc,mainv,xn1)
           trak(i).dca=sqrt(xn1(1)**2.+xn1(2)**2.)
           call ev0_track_mom(trak(i).r0,trak(i).rc,0,pst1,bmag(1).B)
           trak(i).p=sqrt(pst1(1)*pst1(1)+pst1(2)*pst1(2)
     +          +pst1(3)*pst1(3))
*     
*-----reject all tracks that are compatible with vertex within dr-----
*     
           
           DO 20 j=1,trak_h.nok

*     second track is a negative
              IF(trak(j).q .gt.0) GOTO 20
              
              IF(id.ne.0) THEN
                 IF(trak(j).pid.ne.id2) GOTO 20
              ENDIF
              
               
*              trkj(1)=trak(j).r0
*              trkj(2)=trak(j).phi
*              trkj(3)=trak(j).z0
*              trkj(4)=trak(j).psi
*              trkj(5)=trak(j).tanl
*              trkj(6)=trak(j).q
              

              if( first ) then

C--   I only have to calculate impact parameter ---
C--    and total mom for trak(j) first time I see it---
              

                 call ev0_project_track(trak(j).xc,trak(j).rc,mainv,xn2)
                 trak(j).dca=sqrt(xn2(1)**2.+xn2(2)**2.)
                 call ev0_track_mom(trak(j).r0,trak(j).rc,0,
     +                pst2,bmag(1).B)
                 trak(j).p=sqrt(pst2(1)*pst2(1)+pst2(2)*pst2(2)
     +                +pst2(3)*pst2(3))
              endif


              CALL VZERO(xcom,2)
              CALL VZERO(ycom,2)
              iflag1=0
              call ev0_vzero_geom(rd_cut,trak(i).xc,trak(j).xc,
     +             trak(i).rc,trak(j).rc,xcom,ycom,iflag1)
              IF(iflag1.eq.5) GOTO 20
               
C---- two possible intersection points ---------------------------
c     Modified by wkw to pick the one which minimizes rd
c     
              rdcheck(1) = 9999999.0
              rdcheck(2) = 9999990.0
              updated = .false.
              DO jk=1,2
                 xp(1)=xcom(jk)
                 xp(2)=ycom(jk)
                
                 call ev0_project_track(trak(i).xc,trak(i).rc,xp,xn1)
                 call ev0_update_track_param(trak(i).xc,trak(i).rc,
     +                xn1,trak(i).r0,trk1)
                 call ev0_project_track(trak(j).xc,trak(j).rc,xp,xn2)
                 
                 call ev0_update_track_param(trak(j).xc,trak(j).rc,
     +                xn2,trak(j).r0,trk2)
                 
                 if( trk1(3) .lt. 99990.0 .and. 
     +                trk2(3) .lt. 99990.0) then
                    rdcheck(jk)=(xn1(1)-xn2(1))**2+(xn1(2)-xn2(2))**2
     +                   +(trk1(3)-trk2(3))**2
                    updated = .true.
                 endif
             
                 if( iflag1 .eq. 3) goto 12
              enddo
 12           continue
              if (rdcheck(1).lt.rdcheck(2) ) then ! go back to it
                 xp(1)=xcom(1)
                 xp(2)=ycom(1)
                 call ev0_project_track(trak(i).xc,trak(i).rc,xp,xn1)
                 call ev0_update_track_param(trak(i).xc,trak(i).rc,
     +                xn1,trak(i).r0,trk1)
                 call ev0_project_track(trak(j).xc,trak(j).rc,xp,xn2)
                 call ev0_update_track_param(trak(j).xc,trak(j).rc,
     +                xn2,trak(j).r0,trk2)
              endif

              if( updated) then 
                 rd=(xn1(1)-xn2(1))**2+(xn1(2)-xn2(2))**2
     +                +(trk1(3)-trk2(3))**2
              else
                 rd = rd_cut + 1.0
              endif
              IF( (rd.le.rd_cut)) then 
                 xpp(1)=(xn1(1)+xn2(1))/2.
                 xpp(2)=(xn1(2)+xn2(2))/2.            
                 xpp(3)=(trk1(3)+trk2(3))/2.
                 rv=sqrt(xpp(1)**2+xpp(2)**2+xpp(3)**2)
*     
*---  decide here if it is a good candidate
*     
                 if(  (rv.gt.rv_cut) ) THEN
                    massl(1)=mass1(1)
                    massl(2)=mass2(1)
                    call ev0_track_mom(trk1,trak(i).rc,0,pst1,bmag(1).B)
                    call ev0_track_mom(trk2,trak(j).rc,0,pst2,bmag(1).B)
C---- calculate the momentum components of the primary-----------
                    pp(1)  =pst1(1)+pst2(1)
                    pp(2)  =pst1(2)+pst2(2)
                    pp(3)  =pst1(3)+pst2(3)
                    pv0sq = pp(1)**2+pp(2)**2+pp(3)**2
C---- find square of impact param of reconstructed particle------
                    call ev0_v0_impact_param(mainv,xpp,pp,pv0sq,rmin
     +                   ,iflag)
                    IF((rmin.le.rmin_cut).and.(iflag.ne.2)) THEN
                       ok=ok+1
                       ev0out_h.nok= ev0out_h.nok+1
                       vertex_h.nok = vertex_h.nok +1
                       if (ok.gt.ev0out_h.maxlen .or.
     +                      vertex_h.nok .eq.  vertex_h.maxlen) then
                          write(6,*) 'Too many v0s'
                          
                          return ! wkw
                       endif
                       vertex(vertex_h.nok).x=xpp(1)
                       vertex(vertex_h.nok).y=xpp(2)
                       vertex(vertex_h.nok).z=xpp(3)
                       vertex(vertex_h.nok).iflag = 0
                       vertex(vertex_h.nok).det_id = 0
                       vertex(vertex_h.nok).vtx_id= 2
                       vertex(vertex_h.nok).sigma(1) = 0
                       vertex(vertex_h.nok).sigma(2) = 0
                       vertex(vertex_h.nok).sigma(3) = 0
                       vertex(vertex_h.nok).pchi2 = 0
                       vertex(vertex_h.nok).id_aux_ent= ok
                       vertex(vertex_h.nok).id = vertex_h.nok
                       vertex(vertex_h.nok).n_daughters = 2
                       ev0out(ok).px=pp(1)
                       ev0out(ok).py=pp(2)
                       ev0out(ok).pz=pp(3)
                       ev0out(ok).id=ok
                       ev0out(ok).id_vertex= vertex_h.nok                    
                       ev0out(ok).dcap = trak(i).dca
                       ev0out(ok).dcan = trak(j).dca
                       ev0out(ok).dcapn = sqrt(rd)
                       ev0out(ok).dcav0=sqrt(rmin)
                       ev0out(ok).idpos=trak(i).id
                       ev0out(ok).idneg=trak(j).id
                       ev0out(ok).pidpos = trak(i).pid
                       ev0out(ok).pidneg = trak(j).pid
C--   calculate Armenteros variables---
                       ev0out(ok).pos_px = pst1(1)
                       ev0out(ok).pos_py = pst1(2)
                       ev0out(ok).pos_pz = pst1(3)
                       ev0out(ok).neg_px = pst2(1)
                       ev0out(ok).neg_py = pst2(2)
                       ev0out(ok).neg_pz = pst2(3)
                       
                       pdotp=pst1(1)*pp(1)+pst1(2)*pp(2)+pst1(3)*pp(3) !dot product of p+ and p(v0)
                       ndotp=pst2(1)*pp(1)+pst2(2)*pp(2)+pst2(3)*pp(3) !dot product of p- and p(v0)
                       pv0  = sqrt(pv0sq) 
                       ppar= pdotp/pv0
                       npar= ndotp/pv0
                       pper= sqrt(trak(i).p*trak(i).p-ppar*ppar)
                       ev0out(ok).pt = pper
                       ev0out(ok).alpha = (ppar-npar)/(ppar+npar)
                       ev0out(ok).sagitta_pos = trak(i).sagitta
                       ev0out(ok).sagitta_neg = trak(j).sagitta
                       
                       
c     Opening angles: wkw
                       dot=pst1(1)*pst2(1)+pst1(2)*pst2(2)
     +                      +pst1(3)*pst2(3)
                       open=acosd(dot/(trak(i).p*trak(j).p))
                       ev0out(ok).theta = open
*     
*---  if blind run test all mass hypothesis
*     
                       
                       DO ll=1,3
                          IF(id.eq.0) THEN
                             massl(1)=mass1(ll)
                             massl(2)=mass2(ll)
                             index=ll
                          ELSE
                             massl(1)=masst(id1)
                             massl(2)=masst(id2)
                          ENDIF
                          
                          call ev0_inv_mass(trak(i).p,trak(j).p,massl,
     +                         pv0sq,pp(3),inv_mass,env0,ep,rap)
                          
                          if( index .eq. 1) then
                             ev0out(ok).inv_mass_k0=inv_mass
                             ev0out(ok).rapidity_k0 = rap
C-- calculate proper lifetime assuming that particle originated at main vertex
                             ev0out(ok).tau_k0=inv_mass*rv/pv0
                          elseif( index .eq. 2) then
                             ev0out(ok).inv_mass_la=inv_mass
                             ev0out(ok).rapidity_la = rap
C-- calculate proper lifetime assuming that particle originated at main vertex
                             ev0out(ok).tau_la=inv_mass*rv/pv0
                          elseif( index .eq. 3) then
                             ev0out(ok).inv_mass_lb=inv_mass
                             ev0out(ok).rapidity_lb = rap
C-- calculate proper lifetime assuming that particle originated at main vertex
                             ev0out(ok).tau_lb=inv_mass*rv/pv0
                          endif
                             
     
C                          ev0out(ok).tmin(index)=inv_mass*rv_cut/pv0 
c--   calculate cos(theta_c.m.)
C                          bv0  =pv0/env0
C                          gv0  =env0/inv_mass
C                          ppar_cm=gv0*(ppar-bv0*ep)
C                          ev0out(ok).cos_th_cm(index)=
C     +                         cos(atan2(pper,ppar_cm))
c     
c     WRITE(7) rd,rv,rmin,inv_mass
                          
                          IF(id.ne.0) GOTO 30 !blind or not?
                       ENDDO
                       GOTO 30  !next j
*                    else
*                       if( j.eq. i+1 ) then
*                        write(6,*) 'rmin =',rmin,i,j,trak(i).id,
*     >                         trak(j).id,trak(i).r0,trak(j).r0
*                       endif
                    ENDIF       !rmin and iflag check
*                 else
*                    if( j.eq. i+1 ) then
*                       write(6,*) 'rv =',rv,i,j,trak(i).p,trak(j).p
*                    endif
                 ENDIF          !rv check      
*              else
*                 if( j.eq. i+1 ) then
*                    write(6,*) 'rd =',rd,i,j,trak(i).id,trak(j).id
*     >                   ,trak(i).z0,trak(j).z0,xn1(1),xn1(2),
*     >                   xn2(1),xn2(2),trk1(3),trk2(3)
*                    trak(i).q = 100.
*                    trak(j).q = 100.0
*                 endif  

              endif ! rd check
              
              IF(iflag1.eq.3) GOTO 30 !touching circles-loop once
              
 30           CONTINUE
              
 20        CONTINUE

           first = .false.

 10     CONTINUE	
        
          
 999    RETURN
        END
      

