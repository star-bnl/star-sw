      INTEGER FUNCTION V0_RECONSTRUCT3(trak_h,trak,
     +     ev0par_h,ev0par,
     +     vertex_h,vertex,
     +     ev0out_h,ev0out,
     +     bfield)
 

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
C         1.  Adapted to vertex fitting techniques: A.C. Saulys 4/2000
C----------------------------------------------------------------------
#include "PAM.inc"
#include "dst_vertex.inc"
#include "dst_v0_vertex.inc"
#include "ev0_ev0par2.inc"
#include "ev0_track3.inc"

C---------------
#include "phys_constants.inc"
#include "math_constants.inc"
  
      RECORD/ table_head_st/ ev0par_h
      RECORD/ ev0_ev0par2_st/ ev0par(*)
      RECORD/ table_head_st/ vertex_h
      RECORD/ dst_vertex_st/ vertex(*)
      RECORD/ table_head_st/ ev0out_h
      RECORD/ dst_v0_vertex_st/ ev0out(*)
      RECORD/ table_head_st/ trak_h
      RECORD/ ev0_track3_st/ trak(*)
    
      INTEGER 	ok
      INTEGER  	jk,det_id_i,det_id
      INTEGER   jkk
      INTEGER   ll,iflag,iflag1
      INTEGER   i,j,index
      INTEGER   iret
      INTEGER   ii
      INTEGER   ndegf      

      REAL	mass1(3),mass2(3),pst1(3)
      REAL      inv_mass_la
      REAL	rv,pst2(3)
      REAL	pdotp,ndotp,pv0,pv0sq,ppar,npar,pper
      REAL	trk1(6),trk2(6)
      REAL      trk1sav(6),trk2sav(6)
      REAL      xn1sav(2),xn2sav(2),pvecisav(5),pvecjsav(5)
      REAL	xcom(2),ycom(2),inv_mass,rmin
      REAL	xp(2),xn1(2),xn2(2),rd,xpp(3),massl(2),pp(3)
      REAL      env0,bv0,gv0,ep,ppar_cm,mainv(3),rap
      real      rdcheck(2),open,dot, bfield(3)! wkw
      real      temp ! gvb
      real      rver,rver1,rver2,pveci(5),pvecj(5)
      real      rxy1s,rxy2s,pvecis(5),pvecjs(5)
      real      xc(2),rciri,rcirj
      real      verpar(15),vererr(16,16),rminchis,verchisq,phi,psi
      real      x1,y1,z1,x2,y2,z2,check
      real      xn13d(2), xn23d(2)
      real      bimp(3),bimps,bchisp,bchisn
      real      xyzver(3)
      REAL      Prob_cern,chitot
      REAL      rci,rcj,p1,p2

      logical v0_verfit

      double precision covi(15),covj(15),covisav(15),covjsav(15)

C*** TEST
      real rvert, pvecit(5),trk1t(6),xn1t(2) ! TEST
      double precision covit(15) ! TEST
C*** TEST

      LOGICAL	first, updated
      DATA MASS1/0.139567,.93828,.139567/
      DATA MASS2/0.139567,.139567,.93828/
*
*
*-----executable statements--------------------------------------------
*
*      pi=acos(-1.)
*
*
CC:  Load Primary vertex position coordinates:


      mainv(1) = 0.0
      mainv(2) = 0.0
      mainv(3) = 0.0
         do i = 1,vertex_h.nok
            if(vertex(i).vtx_id .eq. 1) then
               mainv(1) = vertex(i).x
               mainv(2) = vertex(i).y
               mainv(3) = vertex(i).z
               go to 100
            end if
         end do
100      continue

*-----steering routine------------------------------------------------
*     
        
        ok=0
        first = .true.
        DO 10 i=1,trak_h.nok
*     First track is a positive
           
           IF(trak(i).q .lt. 0) GOTO 10
CCC           IF(trak(i).pid .ne. 14)go to 10 !only proton hypo for +           
*     Require tracks to have at leas 10 hits
CCC           IF(trak(i).ndef.lt.20) go to 10
           IF(trak(i).dcachis.gt.0.01) go to 10 ! require secondary tracks
*    
           
C--   calculate impact parameter at main vertex--- wkw
           
           call ev0_project_track(trak(i).xc,trak(i).rc,mainv,xn1)
           trak(i).dca=sqrt(xn1(1)**2.+xn1(2)**2.)

C--   skip all tracks with impact parameter .lt. dcapnmin
           if( trak(i).det_id .eq. 1 )then
                 det_id_i = 1
           elseif( trak(i).det_id .eq. 2 ) then
                 det_id_i = 2
           else
                 det_id_i = 3
           endif
           IF( (trak(i).dca.le.ev0par(det_id_i).dcapnmin)) GO TO 10 
C******************
C-- skip all SVT only tracks
c           if(det_id_i.eq.2)go to 10
C******************
           call ev0_track_mom(trak(i).r0,trak(i).rc,0,pst1,bfield(3))
           trak(i).p=sqrt(pst1(1)*pst1(1)+pst1(2)*pst1(2)
     +          +pst1(3)*pst1(3))
*     
*-----reject all tracks that are compatible with vertex within dr-----
*     
           
           DO 20 j=1,trak_h.nok

*     second track is a negative
              IF(trak(j).q .gt.0) GOTO 20
CCC              IF(trak(j).pid .ne. 8)GO TO 20 ! pion hypo for -
*     Require tracks to have at least 10 hits
CCC           IF(trak(j).ndef.lt.20) go to 20
           IF(trak(j).dcachis.gt.0.01) go to 20

                 if( trak(j).det_id .eq. 1 )then
                     det_id = 1
                 elseif( trak(j).det_id .eq. 2 ) then
                     det_id = 2
                 else
                     det_id = 3
                 endif
C******************
C-- skip all SVT only tracks
c           if(det_id.eq.2)go to 20
C******************

              if( first ) then

C--   I only have to calculate impact parameter ---
C--    and total mom for trak(j) first time I see it---
              

                 call ev0_project_track(trak(j).xc,trak(j).rc,mainv,xn2)
                 trak(j).dca=sqrt(xn2(1)**2.+xn2(2)**2.)

C--   skip all tracks with impact parameter .lt. dcapnmin
                IF((trak(j).dca.gt.ev0par(det_id).dcapnmin)) then 

                 call ev0_track_mom(trak(j).r0,trak(j).rc,0,
     +                pst2,bfield(3))
                 trak(j).p=sqrt(pst2(1)*pst2(1)+pst2(2)*pst2(2)
     +                +pst2(3)*pst2(3))

                endif
              endif
                 
                 
C--   skip all tracks with impact parameter .lt. dcapnmin
                IF((trak(j).dca.le.ev0par(det_id).dcapnmin)) GO TO 20 


              if( det_id_i .eq. 1 .or. det_id .eq. 1)then
                 det_id = 1
              elseif( det_id_i .eq. 2 .or. det_id .eq. 2)then
                 det_id = 2
              endif
              xcom(1)=0
              xcom(2)=0
              ycom(1)=0
              ycom(2)=0
              iflag1=0
              call ev0_vzero_geom(ev0par(det_id).dca**2,
     +             trak(i).xc,trak(j).xc,
     +             trak(i).rc,trak(j).rc,xcom,ycom,iflag1)
              IF(iflag1.eq.5) GOTO 20
               
C---- two possible intersection points ---------------------------
c     Modified by wkw to pick the one which minimizes rd
c     
              rdcheck(1) = 9999990.0
              rdcheck(2) = 9999990.0
              updated = .false.

              DO jk=1,2
                 xp(1)=xcom(jk)
                 xp(2)=ycom(jk)
                 trk1(3) = 99990.0
                 trk2(3) = 99990.0
             
                 call ev0_project_track(trak(i).xc,trak(i).rc,xp,xn1)
CDEBUG
                 call ev0_update_track_param(trak(i).xc,trak(i).rc,
     +                xn1,trak(i).r0,trk1,bfield(3))
CDEBUG
C Test for "fake" vertex -> tracks heading away from vertex
                 call ev0_track_mom(trk1,trak(i).rc,0,
     +                pst1,bfield(3))
                 x1 = xn1(1)-mainv(1)
                 y1 = xn1(2)-mainv(2)
                 z1 = trk1(3)-mainv(3)
                 check=(x1*pst1(1))+(y1*pst1(2))+(z1*pst1(3))
                 IF(check.lt.0.0)then
                   trk1(3) = 99990.0
                   go to 11
                 endif

                 call ev0_project_track(trak(j).xc,trak(j).rc,xp,xn2)
CDEBUG
                 
                 call ev0_update_track_param(trak(j).xc,trak(j).rc,
     +                xn2,trak(j).r0,trk2,bfield(3))
CDEBUG
C Test for "fake" vertex -> tracks heading away from vertex
                 call ev0_track_mom(trk2,trak(j).rc,0,
     +                pst2,bfield(3))
                 x2 = xn2(1)-mainv(1)
                 y2 = xn2(2)-mainv(2)
                 z2 = trk2(3)-mainv(3)
                 check=(x2*pst2(1))+(y2*pst2(2))+(z2*pst2(3))
                 IF(check.lt.0.0)then
                   trk2(3) = 99990.0
                   go to 11
                 endif



CCC 11              continue
                 
                 if( trk1(3) .lt. 99990.0 .and. 
     +                trk2(3) .lt. 99990.0) then
                    rdcheck(jk)=(xn1(1)-xn2(1))**2+(xn1(2)-xn2(2))**2
     +                   +(trk1(3)-trk2(3))**2
                    updated = .true.
                 endif

              if(jk.eq.1)then
C               save current point so that no swimming back is necessary
                do ii =1,5
                  trk1sav(ii)=trk1(ii)
                  trk2sav(ii)=trk2(ii)
                  pvecisav(ii)= pveci(ii)
                  pvecjsav(ii)= pvecj(ii)
                enddo
                  trk1sav(6)=trk1(6)
                  trk2sav(6)=trk2(6)
                do ii =1,15
                  covisav(ii) = covi(ii)
                  covjsav(ii) = covj(ii)
                enddo
                xn1sav(1)=xn1(1)
                xn2sav(1)=xn2(1)
                xn1sav(2)=xn1(2)
                xn2sav(2)=xn2(2)
              endif 
                 if( iflag1 .eq. 3) goto 12
 11              continue
              enddo
 12           continue
              if (rdcheck(1).lt.rdcheck(2) ) then ! go back to it
                 xp(1)=xcom(1)
                 xp(2)=ycom(1)
C restore first point so that no swimming back is necessary
              do ii = 1,5
                trk1(ii)=trk1sav(ii)
                trk2(ii)=trk2sav(ii)
                pveci(ii)=pvecisav(ii)
                pvecj(ii)=pvecjsav(ii)
              enddo
                trk1(6)=trk1sav(6)
                trk2(6)=trk2sav(6)
              do ii = 1,15
                covi(ii) = covisav(ii)
                covj(ii) = covjsav(ii)
              enddo
              xn1(1)=xn1sav(1)
              xn2(1)=xn2sav(1)
              xn1(2)=xn1sav(2)
              xn2(2)=xn2sav(2)
 
              endif

              if( updated) then 

CC---------This part is added by Hui Long in order to include
CC-------  the 3D calculation of the v0_dca
                 call ev0_track_mom(trk1,trak(i).rc,0,
     +                pst1,bfield(3))
                 call ev0_track_mom(trk2,trak(j).rc,0,
     +                pst2,bfield(3))
                 temp = 1/trak(i).p
                 pst1(1)=pst1(1)*temp
                 pst1(2)=pst1(2)*temp
                 pst1(3)=pst1(3)*temp
                 temp = 1/trak(j).p
CCC                 temp = 1./sqrt(pst2(1)**2+pst2(2)**2+pst2(3)**2)
                 pst2(1)=pst2(1)*temp
                 pst2(2)=pst2(2)*temp
                 pst2(3)=pst2(3)*temp
                 rd=(trk1(3)-trk2(3))**2

                 call ev0_3D_dca(rd,pst1,pst2,trk1,trk2,xn1,
     +                xn2,trak(i).rc,trak(j).rc)

                 rver1 = sqrt(xn1(1)**2+xn1(2)**2)
                 rver2 = sqrt(xn2(1)**2+xn2(2)**2)
                 call ev0_trak2pvec(trak(i).rc,trak(i).r0,pvecis,bfield(3)*10.)
                 rxy1s = trak(i).r0

                 call ev0_trak2pvec(trak(j).rc,trak(j).r0,pvecjs,bfield(3)*10.)
                 rxy2s = trak(j).r0

CC  ----------------------------------------------------------------------
CC  -----------------------------------------------------------------------

              else
                 rd = ev0par(det_id).dca**2 + 1.0
              endif
              IF( (rd.le.ev0par(det_id).dca**2)) then 
                 xpp(1)=(xn1(1)+xn2(1))/2.
                 xpp(2)=(xn1(2)+xn2(2))/2.            
                 xpp(3)=(trk1(3)+trk2(3))/2.

                 rv=sqrt((xpp(1)-mainv(1))**2+
     +                (xpp(2)-mainv(2))**2+(xpp(3)-mainv(3))**2)
*     
*---  decide here if it is a good candidate
*  
                 if(  (rv .gt. ev0par(det_id).dlen) ) THEN
                    massl(1)=mass1(1)
                    massl(2)=mass2(1)
                    call ev0_track_mom(trk1,trak(i).rc,0,pst1,bfield(3))
                    call ev0_track_mom(trk2,trak(j).rc,0,pst2,bfield(3))
C---- calculate the momentum components of the primary-----------
                    pp(1)  =pst1(1)+pst2(1)
                    pp(2)  =pst1(2)+pst2(2)
                    pp(3)  =pst1(3)+pst2(3)
                    pv0sq = pp(1)**2+pp(2)**2+pp(3)**2
C---- find square of impact param of reconstructed particle------
                    call ev0_v0_impact_param(mainv,xpp,pp,pv0sq,rmin
     +                   ,iflag)
                   
                    IF((rmin.le.ev0par(det_id).dcav0**2).and.
     +                   (iflag.ne.2)) THEN

C --- update pveci and pvecj
C                    convert to 0-2pi
                     phi = amod(trk1(2),real(C_2PI))
                     if(phi.lt.0)phi=phi+C_2PI

                     pveci(1) = trk1(1)*phi !r*phi
                     pveci(2) = trk1(3) !z
                     pveci(3) = trk1(5) !tanl

C                    convert to 0-2pi
                     psi = amod(trk1(4),real(C_2PI))
                     if(psi.lt.0)psi=psi+C_2PI
                     pveci(4) = psi !psi
                     pveci(5) = 1./trak(i).p ! q/pt

C                    convert to 0-2pi
                     phi = amod(trk2(2),real(C_2PI))
                     if(phi.lt.0)phi=phi+C_2PI

                     pvecj(1) = trk2(1)*phi !r*phi
                     pvecj(2) = trk2(3) !z
                     pvecj(3) = trk2(5) !tanl

C                    convert to 0-2pi
                     psi = amod(trk2(4),real(C_2PI))
                     if(psi.lt.0)psi=psi+C_2PI
                     pvecj(4) = psi !psi
                     pvecj(5) = -1./trak(j).p ! q/pt


C --- fit vertex
                     phi = atan2(xpp(2),xpp(1)) ! phi

C                    convert to 0-2pi
                     phi = amod(phi,real(C_2PI))
                     if(phi.lt.0)phi=phi+C_2PI
                     verpar(1) = phi ! phi
                     verpar(2) = xpp(3) ! z
                     verpar(3) = sqrt(xpp(1)**2+xpp(2)**2) ! rxy
                     verpar(4) = abs(pveci(5)*C_D_CURVATURE*10.*bfield(3)) ! curvi

C                    convert to 0-2pi
                     psi = amod(trk1(4),real(C_2PI))
                     if(psi.lt.0)psi=psi+C_2PI

                     verpar(5) = psi ! psi
                     verpar(6) = trk1(5) ! tanl
                     verpar(7) = abs(pvecj(5)*C_D_CURVATURE*10.*bfield(3)) ! curvi

C                    convert to 0-2pi
                     psi = amod(trk2(4),real(C_2PI)) 
                     if(psi.lt.0)psi=psi+C_2PI

                     verpar(8) = psi ! psi
                     verpar(9) = trk2(5) ! tanl

                     verpar(10) = mainv(1) ! x at primary vertex
                     verpar(11) = mainv(2) ! y at primary vertex
                     verpar(12) = mainv(3) ! z at primary vertex

                     do ii = 1,15
                      covisav(ii) = trak(i).cov(ii)
                      covjsav(ii) = trak(j).cov(ii)
                     enddo

                     IF(v0_verfit(100,0.1,rxy1s,pvecis,covisav,
     +                                  rxy2s,pvecjs,covjsav,
     +                            bfield,verpar,verchisq,vererr))then

C use new vertex position
                 xyzver(1) = verpar(3)*cos(verpar(1)) ! x
                 xyzver(2) = verpar(3)*sin(verpar(1)) ! y
                 xyzver(3) = verpar(2)                ! z

                     rmin = (xyzver(1)-mainv(1))**2
     +                  +(xyzver(2)-mainv(2))**2
     +                  +(xyzver(3)-mainv(3))**2
 
                     rd = verpar(13)


                       ok=ok+1
                       
                       ev0out_h.nok= ev0out_h.nok+1
                       vertex_h.nok = vertex_h.nok +1
                       if (ok.gt.ev0out_h.maxlen .or.
     +                      vertex_h.nok .eq.  vertex_h.maxlen) then
                          write(6,*) 'Too many v0s'
                          
                          return ! wkw
                       endif
CCC                       vertex(vertex_h.nok).x=xpp(1)
CCC                       vertex(vertex_h.nok).y=xpp(2)
CCC                       vertex(vertex_h.nok).z=xpp(3)
                       vertex(vertex_h.nok).x=verpar(3)*cos(verpar(1))
                       vertex(vertex_h.nok).y=verpar(3)*sin(verpar(1))
                       vertex(vertex_h.nok).z=verpar(2)

                       vertex(vertex_h.nok).iflag = 0
                       vertex(vertex_h.nok).det_id = 
     +                      trak(i).det_id+(10.*trak(j).det_id)
                       vertex(vertex_h.nok).vtx_id= 2
                       vertex(vertex_h.nok).covar(1) = vererr(11,11)
                       vertex(vertex_h.nok).covar(3) = vererr(12,12)
                       vertex(vertex_h.nok).covar(6) = vererr(13,13)
                       vertex(vertex_h.nok).covar(2) = vererr(11,12)
                       vertex(vertex_h.nok).covar(4) = vererr(11,13)
                       vertex(vertex_h.nok).covar(5) = vererr(12,13)

                       ndegf = trak(i).ndef+trak(j).ndef+2
                       chitot = verchisq + trak(i).chisq*trak(i).ndef
     +                           + trak(j).chisq*trak(j).ndef
                       vertex(vertex_h.nok).chisq(1) = chitot/ndegf
                       vertex(vertex_h.nok).chisq(2) = 
     +                 Prob_cern(verchisq + trak(i).chisq*trak(i).ndef
     +                           + trak(j).chisq*trak(j).ndef,ndegf)


                       vertex(vertex_h.nok).id_aux_ent= ok
                       vertex(vertex_h.nok).id = vertex_h.nok
                       vertex(vertex_h.nok).n_daughters = 2
C                       ev0out(ok).px=pp(1)
C                       ev0out(ok).py=pp(2)
C                       ev0out(ok).pz=pp(3)
                       ev0out(ok).id=ok
                       ev0out(ok).id_vertex= vertex_h.nok                    
CCC TEMPORARY                       ev0out(ok).dcap = trak(i).dca
CCC TEMPORARY                      ev0out(ok).dcan = trak(j).dca

                       ev0out(ok).dcap = trak(i).dcachis
                       ev0out(ok).dcan = trak(j).dcachis

                       ev0out(ok).dcapn = sqrt(rd)
                       ev0out(ok).dcav0=sqrt(rmin)
                       ev0out(ok).idpos=trak(i).id
                       ev0out(ok).idneg=trak(j).id

C use the vertex fit parameters

                       trk1(1) = verpar(3) ! rxy
                       trk2(1) = verpar(3)
                       trk1(2) = verpar(1) ! phi
                       trk2(2) = verpar(1)
                       trk1(3) = verpar(2) ! z1
                       trk2(3) = verpar(2) ! z2
                       trk1(4) = verpar(5) ! psi1
                       trk2(4) = verpar(8) ! psi2
                       trk1(5) = pvecis(3) ! tanl1
                       trk2(5) = pvecjs(3) ! tanl2
                       trk1(6) =  1.0      ! charge1
                       trk2(6) = -1.0      ! charge2
                       rci = 1./abs(pvecis(5)*C_D_CURVATURE*10.*bfield(3)) ! rcurv
                       rcj = 1./abs(pvecjs(5)*C_D_CURVATURE*10.*bfield(3)) ! rcurv

                       call ev0_track_mom(trk1,rci,0,pst1,bfield(3))
                       call ev0_track_mom(trk2,rcj,0,pst2,bfield(3))

                       ev0out(ok).pos_px = pst1(1)
                       ev0out(ok).pos_py = pst1(2)
                       ev0out(ok).pos_pz = pst1(3)
                       ev0out(ok).neg_px = pst2(1)
                       ev0out(ok).neg_py = pst2(2)
                       ev0out(ok).neg_pz = pst2(3)
                       pp(1)  =pst1(1)+pst2(1)
                       pp(2)  =pst1(2)+pst2(2)
                       pp(3)  =pst1(3)+pst2(3)
                       pv0sq = pp(1)**2+pp(2)**2+pp(3)**2
                       p1 = sqrt(pst1(1)**2+pst1(2)**2+pst1(3)**2)
                       p2 = sqrt(pst2(1)**2+pst2(2)**2+pst2(3)**2)
*     
*---  if blind run test all mass hypothesis
*     
                       
                       DO ll=1,3
                          massl(1)=mass1(ll)
                          massl(2)=mass2(ll)
                          index=ll
                          
CCC                          call ev0_inv_mass(trak(i).p,trak(j).p,massl,
                          call ev0_inv_mass(p1,p2,massl,
     +                         pv0sq,pp(3),inv_mass,env0,ep,rap)
                          
C                          if( index .eq. 1) then
C                             ev0out(ok).inv_mass_k0=inv_mass
C                             ev0out(ok).rapidity_k0 = rap
C-- calculate proper lifetime assuming that particle originated at main vertex
C                             ev0out(ok).tau_k0=inv_mass*rv/pv0
C                          elseif( index .eq. 2) then
                           if(index.eq.2)then
C                             ev0out(ok).inv_mass_la=inv_mass
                              inv_mass_la=inv_mass
C                             ev0out(ok).rapidity_la = rap
C-- calculate proper lifetime assuming that particle originated at main vertex
C                             ev0out(ok).tau_la=inv_mass*rv/pv0
C                          elseif( index .eq. 3) then
C                             ev0out(ok).inv_mass_lb=inv_mass
C                             ev0out(ok).rapidity_lb = rap
C-- calculate proper lifetime assuming that particle originated at main vertex
C                             ev0out(ok).tau_lb=inv_mass*rv/pv0
                          endif
                                 
C                    ev0out(ok).tmin(index)=inv_mass*ev0par(det_id).dlen/pv0 
c--   calculate cos(theta_c.m.)
C                          bv0  =pv0/env0
C                          gv0  =env0/inv_mass
C                          ppar_cm=gv0*(ppar-bv0*ep)
C                          ev0out(ok).cos_th_cm(index)=
C     +                         cos(atan2(pper,ppar_cm))
c     
c                          WRITE(6,*) rd,rv,rmin,inv_mass
C                          
                       ENDDO
                       GOTO 30  !next j
*                    else
*                       if( j.eq. i+1 ) then
c                        write(6,*) 'rmin =',rmin,i,j,trak(i).id,
c     >                         trak(j).id,trak(i).r0,trak(j).r0
*                       endif
                    ENDIF       !rmin and iflag check
*                 else
*                    if( j.eq. i+1 ) then
c                      write(6,*) 'rv =',rv,i,j,trak(i).p,trak(j).p
*                    endif
                 ENDIF          !rv check      
*              else
*                 if( j.eq. i+1 ) then
c                    write(6,*) 'rd =',rd,i,j,trak(i).id,trak(j).id
c     >                   ,trak(i).z0,trak(j).z0,xn1(1),xn1(2),
c     >                   xn2(1),xn2(2),trk1(3),trk2(3)
*                    trak(i).q = 100.
*                    trak(j).q = 100.0
*                 endif  

                endif ! if(v0_verfit
              endif ! rd check
              
              IF(iflag1.eq.3) GOTO 30 !touching circles-loop once
              
 30           CONTINUE
              
 20        CONTINUE

           first = .false.

 10     CONTINUE	
        
          
 999    RETURN
        END
      

