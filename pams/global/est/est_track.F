      integer FUNCTION est_track(
     >     geom_h, geom, 
     >     shape_h, shape,
     >     activea_h, activea,
     >     srspar_h, srspar,
     >     config_h, config, 
     >     scs_spt_h,scs_spt,
     >     eval_h, eval,
     >     est_ctrl_h, est_ctrl,
     >     est_match_h, est_match)
      implicit none

CCC   Author: PL,WP,LM,CR Sun Nov 23 14:00:37 MET 1997
CCC   Function matches projection from tpc and hits in layers of svt, 
CCC   (tpc -> SSD -> 3rd -> 2nd -> 1st layer of SVT)
CCC   and put data into est_match table.

#include "est_track.inc"
#include "est_param_def.inc" 
#include "math_constants.inc"
#include "phys_constants.inc"

      integer waf_min,waf_max,pas
      integer i,ii,j,curlo,numb,ini_pts,max_pts,used_pts,found,wafer,tot_found
      integer jj, jjj,waf,clayer
      integer k, gf, corr_found
      integer layer,id_active,active,active_area, id_shape
      integer order,found_h,pom
      integer hit_f,track_f,wafer_f,ht,tr
      real r_f,phier_f,zer_f
      real drift_length,trans_length, l_preint(3)
      real rsvt, zsvt, phisvt
      real x0, y0, z0, phi0, b_magn
      real r0,h,q_ch,passdim(5)
      real a_par, b_par
      real x000, y000, phi000, psi
      real fact, rh, lambda, cosla
      real es, asine, xf, yf, zf,z_tpc, phi_tpc
      real x0w,y0w,xfw,angle
      real phidif, phidif2, err_z, err_phi, err_z_i, err_phi_i
      real afacmax(14),aphi(14),az(14)

      dimension zsvt(HIT_MAX), phisvt(HIT_MAX)
      dimension rsvt(HIT_MAX), numb(HIT_MAX), waf(HIT_MAX)
      dimension order(IN_DET), found_h(IN_DET)
      dimension phier_f(IN_DET),zer_f(IN_DET),r_f(IN_DET)
      dimension hit_f(IN_DET), track_f(IN_DET), wafer_f(IN_DET)
      

C.....GETTING THE CURRENT PT LOOP, LAYER AND SCANING PARAMETERS
      curlo=est_ctrl(1).current_loop
      clayer =est_ctrl(1).layer 
      pas=est_ctrl(1).pass
      passdim(1)=sqrt(3.)
      passdim(2)=sqrt(1.)
      passdim(3)=sqrt(1.)
      passdim(4)=sqrt(1.5)
      passdim(5)=sqrt(2.)
      if (clayer.eq.1) then
         waf_min = 1000
         waf_max = 2999       
         afacmax(curlo)=est_ctrl(1).afacmax1(curlo)
         aphi(curlo)=est_ctrl(1).aphi1(curlo)/passdim(pas)
         az(curlo)=est_ctrl(1).az1(curlo)/passdim(pas)        
      endif

      if (clayer.eq.2) then
         waf_min = 3000
         waf_max = 4999        
         afacmax(curlo)=est_ctrl(1).afacmax2(curlo)
         aphi(curlo)=est_ctrl(1).aphi2(curlo)/passdim(pas)
         az(curlo)=est_ctrl(1).az2(curlo)/passdim(pas)
      endif

      if (clayer.eq.3) then
         waf_min = 5000
         waf_max = 6999
         afacmax(curlo)=est_ctrl(1).afacmax3(curlo)
         aphi(curlo)=est_ctrl(1).aphi3(curlo)/passdim(pas)
         az(curlo)=est_ctrl(1).az3(curlo)/passdim(pas)
      endif
      if (clayer.eq.4) then
         waf_min = 7000
         waf_max = 8999
         afacmax(curlo)=est_ctrl(1).afacmax4(curlo)
         aphi(curlo)=est_ctrl(1).aphi4(curlo)/passdim(pas)
         az(curlo)=est_ctrl(1).az4(curlo)/passdim(pas)
      endif

      if (est_ctrl(1).ktrl1.eq.1) write(6,*)'Matching.....'


C.....FILLING TABLES WITH INTERESTING HITS ONLY AND 
C.....RECALCULATING LAYER HITS FORM (X,Y,Z) TO (R,PHI,Z) SYSTEM
      j=0
      k=0
      jj=0
      do i=1,scs_spt_h.nok
         if (scs_spt(i).id_wafer.ge.waf_min .and.
     >        scs_spt(i).id_wafer.le.waf_max) jj=jj+1
         if (scs_spt(i).id_wafer.ge.waf_min .and.
     >        scs_spt(i).id_wafer.le.waf_max.and.
     >        scs_spt(i).flag.eq.est_ctrl(1).maxass) k=k+1
         if (scs_spt(i).id_wafer.ge.waf_min .and.
     >        scs_spt(i).id_wafer.le.waf_max.and.scs_spt(i).flag.lt.est_ctrl(1).maxass) then
            j = j+1
            rsvt(j) = sqrt(scs_spt(i).x(1)**2 + scs_spt(i).x(2)**2) 
            zsvt(j) = scs_spt(i).x(3)
            phisvt(j) = 0.
            phisvt(j) = atan2(scs_spt(i).x(2),scs_spt(i).x(1))
            if (phisvt(j).lt.0) phisvt(j)=phisvt(j)+C_2PI
            numb(j) = i
            waf(j) = scs_spt(i).id_wafer
         end if
      end do
      ini_pts = jj
      max_pts = j
      used_pts = k


C.....HERE WE START WITH A LOOP OVER THE WAFERS

      if (est_ctrl(1).ktrl1.eq.1) write(6,*)'Starting to loop over the wafers.....'
      do j=geom_h.nok,1,-1
         if (geom(j).id.ge.waf_min .and. 
     >        geom(j).id.le.waf_max) then
            a_par = geom(j).n(1)
            b_par = geom(j).n(2)
            angle = atan2(b_par,a_par)-C_PI_2
            if (angle.lt.-C_PI) angle = angle + C_2PI
            
            do i=1,IN_DET
               order(i) = i
               phier_f(i) = 0.
               zer_f(i) = 0.
               r_f(i) = 0.
               hit_f(i) = 0
               track_f(i) = 0
               wafer_f(i) = 0
            enddo

            gf=0

C...........HERE WE START THE LOOP OVER THE TRACKS.

            do i=1,est_match_h.nok        
C..............THE TRACK AS TO BE IN THE PT RANGE AND NOT MORE THAN ONE MISSING HIT IN THE 
C..............PREVIOUS LAYERS
               if (1./est_match(i).invp_tpc.gt.est_ctrl(1).apmin(curlo).and.est_match(i).flag.eq.0
     >              .and.clayer.ge.est_match(i).layer-(est_ctrl(1).nhole(pas))) then

C.................WE TRANSFORM THE TRACK PARAMETER IN THE WAFER LOCAL FRAME
C.................AND THEN WE CALCULATE THE INTERCEPT OF THE TRACK AND THE WAFER
                  r0 = est_match(i).r0
                  x0 = r0*cos(est_match(i).phi0/C_DEG_PER_RAD)
                  y0 = r0*sin(est_match(i).phi0/C_DEG_PER_RAD)
                  z0 = est_match(i).z0
                  b_magn = est_ctrl(1).bmag
                  q_ch = est_match(i).q
                  h = 0.
                  if (q_ch*b_magn.lt.0.) h = 1.
                  if (q_ch*b_magn.gt.0.) h = -1.

                  lambda = atan(est_match(i).tanl)
                  cosla = sqrt(1./((est_match(i).tanl)**2+1.))
                  rh = abs(1./(10.*C_D_CURVATURE*q_ch*b_magn*est_match(i).invp))
                  fact = h*cosla/rh
                  psi = est_match(i).psi/C_DEG_PER_RAD
                  x0w = (x0-geom(j).x(1))*cos(angle)+
     >                 (y0-geom(j).x(2))*sin(angle)
                  y0w = -(x0-geom(j).x(1))*sin(angle)+
     >                 (y0-geom(j).x(2))*cos(angle)
            
                  phi0 = psi - h*C_PI_2-angle
                  if (abs(sin(phi0)-y0w/rh).le.1.) then
                     asine = asin(sin(phi0) - y0w/rh)
                     if (cos(phi0).gt.0) then
                        es = asine
                     else
                        es = C_PI-asine
                     end if
                     if (es.gt.phi0+C_PI) es=es-C_2PI
                     if (es.lt.phi0-C_PI) es=es+C_2PI
                     es = es-phi0
                  else
                     es = 0.
                  end if
C.................WE SHOULD FOUND A PROJECTION CLOSE TO THE TRACK
C.................AND NOT BE ON THE OTHER SIDE OF THE BARREL
                  x000 = x0*cos(angle)+y0*sin(angle)
                  y000 = -x0*sin(angle)+y0*cos(angle)
                  phi000 = atan2(y000,x000)

                  xfw = x0w +rh*(cos(phi0+es) - cos(phi0))
                  es = es/fact
                  zf = z0 + es*sin(lambda)

C.................POSITION OF THE TRACK PROJECTION IN THE LOCAL FRAME OF THE CURRENT WAFER
                  l_preint(1) = xfw
                  l_preint(2) = zf-geom(j).x(3)
                  l_preint(3) = 0.
C.................CHECKING IF THE PROJECTION IS WITHIN THE ACTIVE AREA OF THE WAFER
C.................WITH SOME EXTEND OF THE REAL ACTIVE AREA SIZE                  
                  layer=geom(j).layer
                  id_shape=config(1).layer_shape(layer)
                  drift_length=shape(id_shape).shape(1)
                  trans_length=shape(id_shape).shape(2)  
                  id_active=srspar(1).id_active(layer)
                  active=active_area(id_active,l_preint,
     >                 drift_length*est_ctrl(1).ext(curlo),
     >                 trans_length*est_ctrl(1).ext(curlo),
     >                 activea_h,activea)
                  if (active.eq.1 .and. phi000.gt.0.) then
C.................PROJECTION IS IN THE J DETECTOR
                     wafer = geom(j).id
                     xf = geom(j).x(1) + xfw*cos(-angle)
                     yf = geom(j).x(2) - xfw*sin(-angle)
                     z_tpc = zf
                     phi_tpc = atan2(yf,xf)                  
                     if (phi_tpc.lt.0.) phi_tpc = phi_tpc + C_2PI


C....................FOR THE GOOD HITS THAT SHLOUD BE MATCHED WE SAVE THE PROJECTION POSITION
                     do jj=1,NUM_SVT_HIT
                        if (est_match(i).perf_svt_waf(jj).eq.geom(j).id) then
                           est_match(i).perf_proj_x(jj) = geom(j).x(1) + xfw*cos(-angle) 
                           est_match(i).perf_proj_y(jj) = geom(j).x(2) - xfw*sin(-angle)
                           est_match(i).perf_proj_z(jj) = zf
                           
                        endif
                     enddo
                     


           
C....................WE START LOOKING FOR ASSOCIATIONS
                     found=0
                     err_phi_i = aphi(curlo)/afacmax(curlo)
                     err_z_i = az(curlo)/afacmax(curlo)
                     err_phi = err_phi_i
                     err_z = err_z_i
                 
                     do jj=1,max_pts
                        found_h(jj)=0
                     enddo

C....................WE SEARCH UNTIL WE FOUND MXFND ASSOCIATIONS OR THE 
C....................SCANING AREA REACHED ITS MAMIMUM SIZE              
                     do while(found.lt.est_ctrl(1).mxfnd.and. 
     >                    err_z.le.afacmax(curlo)*err_z_i.and.
     >                    err_phi.le.afacmax(curlo)*err_phi_i)

C.......................LOOPING OVER THE HITS IN THE LAYER                        
                        do jj=1,max_pts

C..........................THEY SHOULD BE IN THE WAFER AND STILL BE SHARED
                           if (waf(jj).eq.wafer .and. found_h(jj).eq.0
     >                          .and.scs_spt(numb(jj)).flag.lt.est_ctrl(1).maxass) then
                              phidif = phi_tpc-phisvt(jj)
                              if (phidif.lt.0) phidif=phidif+C_2PI
                              phidif2 = C_2PI- phidif
                              if (phidif2.lt.0) phidif2=phidif2+C_2PI
                              phidif = min(phidif,phidif2)

C.............................THE DISTANCE IN Z SHOULD BE SMALLER THAN THE SEARCHING SCALE
c                              if ((abs(z_tpc - zsvt(jj)).le.
c     >                             err_z.and.est_ctrl(1).stupid.eq.0).or.
c     >                             (scs_spt(numb(jj)).id_mctrack.eq.est_match(i).id_mctrtpc.and.
c     >                               est_ctrl(1).stupid.eq.1.and.abs(z_tpc - zsvt(jj)).le.
c     >                             err_z)) then
                                 
                              if ((abs(z_tpc - zsvt(jj)).le.
     >                             err_z.and.est_ctrl(1).stupid.eq.0).or.
     >                             (scs_spt(numb(jj)).id_mctrack.eq.est_match(i).id_mctrtpc.and.
     >                               est_ctrl(1).stupid.eq.1)) then
                                 

C................................THE DISTANCE IN PHI SHOULD BE SMALLER THAN THE SEARCHING SCALE
c                                 if ((phidif.le.err_phi.and.est_ctrl(1).stupid.eq.0).or.
c     >                                est_ctrl(1).stupid.eq.1.and.phidif.le.err_phi) then

                                 if ((phidif.le.err_phi.and.est_ctrl(1).stupid.eq.0).or.
     >                                est_ctrl(1).stupid.eq.1) then

C...................................OK THE HIT IS IN THE SEARCHING AREA
                                    found = found+1
                                    gf = gf+1
c                                    print *,'found!',gf,found
                                    track_f(gf)=i
                                    hit_f(gf)=jj
                                    r_f(gf)=sqrt((xf-scs_spt(numb(jj)).x(1))**2+
     >                                   (yf-scs_spt(numb(jj)).x(2))**2+
     >                                   (zf-scs_spt(numb(jj)).x(3))**2)
                                    
                                    zer_f(gf) = err_z
                                    phier_f(gf) = err_phi
                                    wafer_f(gf) = wafer
                                    found_h(jj) = 1

                                 endif ! END OF RELATIVE DISTANCE IN Z
                              endif ! END OF RELATIVE DISTANCE IN Z
                           endif ! END OF HITS IN THE WAFER
                        enddo ! END OF HITS IN THE LAYER
C.......................IF WE DONT FIND ENOUGHT HITS WE INCREASE THE SEARCHING AREA
                        if (found.lt.est_ctrl(1).mxfnd) then
                           err_z= err_z+err_z_i
                           err_phi = err_phi+err_phi_i
                        else
                           write(6,*)pas,curlo,clayer
                        endif
                     enddo ! END OF SEARCHING THE ASSOCIATIONS
                  endif ! END OF THE TRACK IN ACTIVE AREA
               endif ! END OF TRACK IN GOOD PT RANGE
            enddo  ! END OF LOOP OVER TRACKS


C..........SORTING THE HIT-TRACK ASSOCIATIONS STARTING BY THE SMALLEST DISTANCE
           

            do jj=1,gf
              do jjj=1,jj-1
                 if (r_f(order(jj)).lt.r_f(order(jjj))) then
                    pom = order(jj)
                    order(jj)=order(jjj)
                    order(jjj)=pom
                 endif
              enddo
           enddo

C..........NOW WE UPDATE THE TRACKS WITH THE NEW ASSOCIATED HIT
           do i=1,gf
              jj=order(i)
              if (track_f(jj).ne.0 .and. hit_f(jj).ne.0) then
                 tr = track_f(jj)
                 ht = hit_f(jj)

C................ADDING THE INFORMATION FROM THE HIT
                 k = est_match(tr).n_svt_hits+1
                 est_match(tr).id_hit(k) = numb(ht)
                 est_match(tr).z_4th(k) = zsvt(ht)
                 est_match(tr).phi_4th(k) = phisvt(ht)
C................temporarely reused to now the loop number
                 est_match(tr).phi_err(k) = phier_f(jj)
                 est_match(tr).z_err(k) = zer_f(jj)
                 est_match(tr).r_4th(k) = rsvt(ht)
                 est_match(tr).id_mctrsvt(k) =
     >                scs_spt(numb(ht)).id_mctrack 
                 est_match(tr).id_det(k) = wafer_f(jj)
                 est_match(tr).ang_det(k) = angle

                 est_match(tr).dist(k) = r_f(jj)
                 est_match(tr).n_svt_hits = est_match(tr).n_svt_hits+1
                 est_match(tr).layer = clayer
C................FLAG THE HIT AS USED ONE MORE TIME TO ALLOW HIT SHARING (NOT MORE THAN MAXASS)
                 scs_spt(numb(ht)).flag = scs_spt(numb(ht)).flag + 1

                 if (clayer.lt.6) then
C...................HIT SHARING AND TRACK DUPLICATION ARE NOT ALLOWED IN 1ST SVT LAYERS
                    scs_spt(numb(ht)).flag = est_ctrl(1).maxass
                    do jjj=1,gf
                       if (track_f(jjj).eq.tr) track_f(jjj)=0
                       if (hit_f(jjj).eq.ht) hit_f(jjj)=0
                    enddo
                 endif
              endif
           enddo                ! END OF MATCHES FOUND
        endif                   ! END OF DETECTORS IN THE CURRENT LAYER
      enddo                     ! END OF LOOP OVER THE DETECTORS 

      
C.....CALCULATING THE NUMBER OF MATCHES AND CORRECT ONE DONE AT THIS STAGE
      tot_found = 0
      corr_found = 0
      do ii=1,est_match_h.nok
         if (est_match(ii).flag.eq.0) then
            do j=1,est_match(ii).n_svt_hits
               if (est_match(ii).id_det(j).le.waf_max .and.
     >              est_match(ii).id_det(j).ge.waf_min) then
                  tot_found=tot_found+1
                  if (est_match(ii).id_mctrsvt(j).eq.est_match(ii).id_mctrtpc)
     >                 corr_found = corr_found+1
               endif
            end do
         endif
      end do

      if (est_ctrl(1).ktrl1.eq.1) then
         write(6,*)'Matches (found/correct) : ',tot_found,corr_found
      endif

      return
      end




