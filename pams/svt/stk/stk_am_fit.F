      INTEGER*4 FUNCTION STK_AM_FIT(
     1            stkpar_h,            stkpar ,
     2               spt_h,               spt ,
     3               vtx_h,               vtx ,
     4            groups_h,            groups ,
     5             track_h,             track ) 
      IMPLICIT NONE
C     DESCRIPTION:
C     
C     Perform a fit of track (candidates) in the SVT.  A pure helicoidal 
c     movement is assumed so the movement along the z-axis is decoupled 
c     from the movement along a circle. The circle fit is performed using 
c     a conformal mapping technique which assumed that the cricle goes
c     through the origin allowing a reduction of the problem to a linear fit.
C     
C     This version superseeds previous versions. Here the fits are divided
c     into two categories. Tracks considered primary tracks are fitted 
c     including the primary vertex. Tracks known or assumed to be secondaries 
c     are fitted excluding the primary vertex.
c
c     Fitting of primary tracks: 
c     A linear translation is performed on all SVT points to force the circle 
c     the main (primary) vertex to be on the circle.  The space points are in 
c     the table spt.  The table groups is a list, for each track 
c     (candidate) of the associated space points.  The vertex is therefore not
c     included explicitely in the fit as it would result in a division by zero
c     in the conformal mapping. It is however included in the straight line
c     fit.
C
c     Fitting of secondary tracks:
c     A calculation of the helix/circle radius is done on the basis of 3
c     space points and is used to get the transverse momentum. Tanl is 
c     calculated with a fit between the circle phase and the z coordinate.
c
C
C     Currently the error calculated and output by the code are the square of
c     the sigma.
C
c     wkw: changed sign of tanl,stkpar.method is used to decide if the primary
c     vertex is to be used in the fit, now using 1 space point for angle ref.
c     
C     AUTHOR:
C     C. Pruneau,  Wayne State University
C     
C     DATE:
C     18-OCT-1993  c.a.p. written
C     30-march-1994 c.a.p added different fit methods for secondaries
C     
C     ARGUMENTS:
C     spt_h    header to table spt
C     spt      rows of table   spt
C     vtx_h    header to table vtx
C     vtx      rows of table   vtx
C     track_h  header to table track
C     track    rows of table   track
C     groups_h header to table groups
C     groups   rows of table groups
C==============================================================================
    
C     argument declaration
C     ===================
#include "stk_am_fit.inc"
      
C     Local Declarations:
C     ===================
c     positions
      integer      max_spt
      parameter (max_spt=30)
      real     x(max_spt),  y(max_spt), z(max_spt)
      real     u(max_spt),  v(max_spt), sq(max_spt), phase(max_spt)   
c     error on positions
      real    dx(max_spt), dy(max_spt), dz(max_spt)
      real    du(max_spt), dv(max_spt), dphase(max_spt)    
      real pha
c     wkw: add in residues:
      real    cir_res(max_spt),  lin_res(max_spt)
      integer nspt, kspt(max_spt), the_spt, layer(3)
      
      real      tanl,  psi,  invpt      ! momentum info
      real     dtanl, dpsi, dinvpt      ! error on momentum info
      real       z0,    r0,  phi0       ! 1st point position 
                                        !(reference cylinder)
      real      dz0,   dr0, dphi0       ! error on 1st point position 
                                        ! (reference cylinder)
      real     dtanphi, dphi, dphi0invpt, dr0invpt, dr0phi0
      real    dtanlinvpt, dtanlphi0, dtanlr0
      real    dz0invpt, dz0phi0, dz0r0, dz0tanl
      real      xh,  yh,  rh
      real     dxh, dyh, drh, dxhyh, dcosl, dx0, dy0, dx0y0
      
      real       a(2), b(2), da(3), db(3)
      real       cchi2, zchi2, r2
      real     v1x, v1y, v2x, v2y, ch, phi, cosl
      real     konst, bfield, x0, y0, yd, xd
      real     infinity,xpart,ypart,sumsq
      parameter (infinity=-1.e12)

c     some temporary variables for dedx consistancy checks
      real dedx_diff,new_dedx_diff,lowest_dedx,ave_dedx
      integer other_spt,j

      integer  ispt, i, itrack
      integer  status
      
      integer ok,tls_Search_Near_i,nrows,table_off(max_spt)
      logical SCV_STATE
c     external declarations
c     =====================
      integer   svt_fit_circle3pts

c     error/message
c     =============
      character*80 mess
      character msg*132
      integer   msg_id(10)/10*0./
      logical   first
      data      first/.true./
      save      first
      
C     RETURN:
C     =======
C     stafcv_ok    : normal completion
C     stafcv_bad : abort event upon abnormal input
C     
C     FORMAT:
C     =======
 1001 format(1x,a)
 1002 format(1x,'<W>-stk_am_fit_nespt : not enough space points',
     &     'to fit track')

C     EXECUTABLE CODE:
C     ================

      WRITE(*,*) 'STK_AM_FIT: Starting ################################'

      if (stkpar(1).c1norm(1).eq.0) then
         print*,'Setting chi1 normalization to 1.0'
         stkpar(1).c1norm(1)=1.0
         stkpar(1).c1norm(2)=0.0
         stkpar(1).c1norm(3)=1.0
      endif
      if (stkpar(1).c2norm(1).eq.0) then
         print*,'Setting chi2 normalization to 1.0'
         stkpar(1).c2norm(1)=1.0
         stkpar(1).c2norm(2)=0.0
         stkpar(1).c2norm(3)=1.0
      endif

      stk_am_fit = stafcv_ok

c     set error id 
      
c     check if inputs are valid, if not return with abort event status
      
      if (vtx_h.nok.le.0) then
         stk_am_fit = stafcv_bad
         msg = 'stk_am_fit_npvi-e: no primary vertex information'
         call message(msg,1,msg_id(1))
         return
      else if (spt_h.nok.le.0) then
         stk_am_fit = stafcv_bad
         msg = '<e>_stk_am_fit_nspp: no svt space point'
         call message(msg,1,msg_id(2))
         return
      else if (track_h.nok.le.0) then
         stk_am_fit = stafcv_bad
         msg = '<e>_stk_am_fit_ntc: no track candicate'
         call message(msg,1,msg_id(3))
         return
      else if (groups_h.nok.le.0) then
         stk_am_fit = stafcv_bad
         msg = '<e>_stk_am_fit_ntc: no groups'
         call message(msg,1,msg_id(3))
         return
      end if
      
c     loop on all track candidates pointed to by track


      do itrack = stkpar(1).ngood+1, track_h.nok
         
c     distinguish primaries from secondaries
c     track(itrack).sec = >1 secondaries
c                             = 1  primaries
c                             = 0  unknown - will be taken as primaries

c     wkw: (I changed the order of the sec/nspt tests)

         if (track(itrack).nspt .gt. 2) then ! enough hits

            if (stkpar(1).method .le. 1) then ! fit as primary (wkw)


c     Unknown and Primary Tracks
c     load space points into work arrays
c     at this level, assume the primary vertex is part of the track.
c     a negative space point index refers by convention to the vertex
c     table.  Note the primary vertex was not inserted in the track
c     table, so one has go fetch explicitly  here.
         
            
c     using conformal mapping technique
c     vertex first - the vertex will be assumed to be exactly on the circle...
c     the vertex is therefore not in the fitted points.

               ispt       = 1
               kspt(ispt)  = -1  ! -1 indicates this is a vertex
               x(ispt)    = vtx(1).x(1)
               y(ispt)    = vtx(1).x(2)
               z(ispt)    = vtx(1).x(3)
               sq(ispt)   = x(ispt)**2+y(ispt)**2
               u(ispt)    = x(ispt)/sq(ispt)
               v(ispt)    = y(ispt)/sq(ispt)
               dx(ispt)   = vtx(1).cov(1)
               dy(ispt)   = vtx(1).cov(4)
               dz(ispt)   = vtx(1).cov(6)
c              wkw: du and dv calc added 
               sumsq       =x(ispt)**2.0+y(ispt)**2.0
               xpart       =(sumsq-2.0*x(ispt)**2.0)/sumsq**2.0
               ypart       =2.0*x(ispt)*y(ispt)/sumsq**2.
               du(ispt)    =sqrt((xpart*dx(ispt))**2.0+
     &                              (ypart*dy(ispt))**2.0)
               xpart       =2.0*y(ispt)*x(ispt)/sumsq **2.
               ypart       =(sumsq-2.0*y(ispt)**2.0)/sumsq**2.0
               dv(ispt)    =sqrt((xpart*dx(ispt))**2.0+
     &                              (ypart*dy(ispt))**2.0)

c               write(6,*) ' ====================='
c	write(6,*) 'ispt,x,y,z,sq,u,v,dx,dy,dz,du,dv:',ispt,
c     &              x(ispt),y(ispt),z(ispt),sq(ispt),
c     &              u(ispt),v(ispt),
c     &              dx(ispt),dy(ispt),dz(ispt),du(ispt),dv(ispt)              

c     space points
               
               nspt = track(itrack).nspt
c               if (nspt.gt.6) nspt=6 ! wkw

c               write(6,*) ' nspt=',nspt

               dedx_diff=0
               lowest_dedx=1e9

               nrows=max_spt
               table_off(1) = 3*itrack
               ok = tls_Search_Near_i(groups_h.nok,groups(1).id1,
     +              groups(2).id1,itrack,table_off,nrows)
       
               if( .not. SCV_STATE( ok, 'i')) then
                  call SCV_MESSAGES( ok, mess )
                  write(6,*)' in stk_fit:',mess
               endif
               if( nrows .ne. track(itrack).nspt) then
                  write(6,*) 'nrows=',nrows, 'for track',itrack
               endif

               do i = 1, nrows

                  ispt        = ispt + 1
                  the_spt     = groups(table_off(i)).id2
                  kspt(ispt)   = the_spt
                  
c     This is a new section to calculate the largest difference between
c     any of the space points de/dx and the average of the other space
c     points
                  ave_dedx=0
                  do j=1, nrows
                     if (j .ne. i) then
                        other_spt=groups(table_off(j)).id2
                        ave_dedx=ave_dedx+
     1                       spt(other_spt).de(1)/(float(nspt)-1.0)
                     endif
                  enddo
                  new_dedx_diff=(ave_dedx-spt(the_spt).de(1))/
     &                 ave_dedx
                  if (abs(new_dedx_diff).gt.abs(dedx_diff)) then
                     dedx_diff=new_dedx_diff
                  endif
                  if (spt(the_spt).de(1).lt.lowest_dedx) then
                     lowest_dedx=spt(the_spt).de(1)
                  endif
                  
                  
                  
c     the space points - include translation to force primary vertex
c     to be at origin
                  
                  x(ispt)     = spt(the_spt).x(1) - vtx(1).x(1)
                  y(ispt)     = spt(the_spt).x(2) - vtx(1).x(2)
                  z(ispt)     = spt(the_spt).x(3) - vtx(1).x(3)
                  sq(ispt)    = x(ispt)**2+y(ispt)**2
                  u(ispt)     = x(ispt)/sq(ispt)
                  v(ispt)     = y(ispt)/sq(ispt)
                  
                  dx(ispt)    = spt(the_spt).cov(1)
                  dy(ispt)    = spt(the_spt).cov(2)
                  dz(ispt)    = spt(the_spt).cov(3)
                  
c     wkw: set best resolution to be 10 microns
                  if (dx(ispt).lt.0.001) dx(ispt) = 0.001
                  if (dy(ispt).lt.0.001) dy(ispt) = 0.001
                  if (dz(ispt).lt.0.001) dz(ispt) = 0.001
                  
c     wkw: du and dv calc added
                  sumsq       =x(ispt)**2.0+y(ispt)**2.0
                  xpart       =(sumsq-2.0*x(ispt)**2.0)/sumsq**2.0
                  ypart       =2.0*x(ispt)*y(ispt)/sumsq**2.
                  du(ispt)    =sqrt((xpart*dx(ispt))**2.0+
     &                 (ypart*dy(ispt))**2.0)
                  xpart       =2.0*y(ispt)*x(ispt)/sumsq **2.
                  ypart       =(sumsq-2.0*y(ispt)**2.0)/sumsq**2.0
                  dv(ispt)    =sqrt((xpart*dx(ispt))**2.0+
     &                 (ypart*dy(ispt))**2.0)
                  
c     write(6,*) 'ispt,x,y,z,sq,u,v,dx,dy,dz,du,dv:',ispt,
c     &              x(ispt),y(ispt),z(ispt),sq(ispt),
c     &              u(ispt),v(ispt),
c     &              dx(ispt),dy(ispt),dz(ispt),du(ispt),dv(ispt)              
                  
                  if ( nspt .eq. ispt) goto 11
               end do
 11            continue
               nspt = nspt + 1
            
c     2001       format('i,x,y,z,u,v,s, nspt=',i10)
c     2002       format(i6,6f16.6)
c     write(6,2001) nspt
c     do i = 1, nspt
c     write(6,2002) i,x(i),y(i),z(i),u(i),v(i)
c     end do
               
c     fit current track candidate including vertex
c     decouple the z-motion from the circular motion
               
c     fit circular motion - exclude vertex
               
               call linfitr(u(2),v(2),du(2),dv(2),nspt-1,b,db,
     &              cchi2,cir_res)
               
 1000          format(1x,a,5f14.4)
               
c     write(6,1000) ' b,db:', b,db,cchi2
               
c     circle center coordiantes, radius, azimuthal angle
c     (xh,yh) : coordinates of the circle center
c     rh      : radius of the circle
c     phi     : angle between vector pointing from (xh,yh) to (x(1),y(1)) 
c     and  the x-axis
c     psi     : phi+90 : direction of the particle in transverse plane
c     tanl    : slope of the particle direction in the z,s plane
c     bfield  : magnetic filed strenght, bfield assume positive-z direction
c     konst   : conversion factor to get the momentum from the radius of 
c               curvature
c     units are GeV c-1 kgauss-1 cm-1
c     
c     Track Parameters Reconstruction
               
c     coordinates of center, radius and assoicated errors
               
               xh   = -0.5*b(2)/b(1)
               yh   = 0.5/b(1)
               rh   = sqrt(xh**2+yh**2)
c     epsi = -b(3)/(1+b(2)**2)**1.5
c               phi  = atan2d(y(2)-yh,x(2)-xh)
C               phi  = atan2d(y(1)-yh,x(1)-xh) ! wkw
               
               dxh    = (db(1)*b(2)**2 + db(2)*b(1)**2-2.*b(1)*
     &              b(2)*db(3))/(4.*b(1)**4)
               dyh    = 0.25*db(1)/b(1)**4
               dxhyh  = (b(1)*db(3)-b(2)*db(1))/(4.*b(1)**4)
               drh    = (dxh*xh**2 + dyh*yh**2 + 2.*xh*yh*dxhyh)/rh**2
               
c     Track sign: the sign of the track is determined from the vector product
c     of two vectors pointing from the center of the circle to the 
c     first two points on the svt - this of course assumes the particle
c     is moving away from the beam axis - not coming back
               
               v1x  = x(2) - xh
               v1y  = y(2) - yh
               v2x  = x(3) - xh
               v2y  = y(3) - yh
               if (v1x*v2y-v2x*v1y .gt. 0.) then
                  ch = -1.
               else
                  ch = 1.
               end if
               
c     vector direction

c              take second point (first svt point) as reference
               phi  = atan2d(y(2)-yh,x(2)-xh)
               dtanphi = (dyh*v1x**2 + dxh*v1y**2 + 
     &              2.*v1x*v1y*dxhyh)/v1x**4
               dphi    = dtanphi*v1x**4/(v1x**2+v1y**2)**2
               
c     z-motion fit from phase
               
               nspt = track(itrack).nspt
	       ispt = 0 ! fix from lm 10/16/1996
c              if (nspt.gt.6) nspt=6 ! wkw
c     do i = 1, nspt
               do i = 1, nspt+1 ! wkw (10/3/1996)
                  
                  ispt        = ispt + 1
                  phase(ispt) = atan2((y(ispt)-yh),(x(ispt)-xh))
                  xd=(x(ispt))-xh
                  yd=(y(ispt))-yh
c                 wkw: dphase calc added 
                  dphase(ispt) = (yd*dx(ispt)/(yd**2.+xd**2.))**2.
                  dphase(ispt) =  dphase(ispt) +(xd*dy(ispt)/
     &                 (yd**2.+xd**2.))**2. 
                  dphase(ispt) = sqrt(dphase(ispt))
               end do
c               call linfitr(phase,z,dphase,dz,nspt,a,da,zchi2,
c     &              lin_res)
               call linfitr(phase,z,dphase,dz,nspt+1,a,da,zchi2,
     &              lin_res) ! wkw (10/3/1996)
               
c               write(6,1000) ' a,da:', a,da,zchi2

               tanl = -a(2)*ch/rh
               cosl = 1./sqrt(1.+tanl**2)
               
               dtanl = da(2)
               dcosl = dtanl*tanl**2/(1.+tanl**2)**3
               
c     direction of the particle in the transverse plane
               
               if (ch .gt. 0.) then
                  psi  = phi - 90.
                  dpsi = dphi
               else
                  psi  = phi + 90.
                  dpsi = dphi
               end if
               if (psi.gt.360) psi=psi-360
               if (psi.lt.0) psi=psi+360
               
c     assume uniform field of 0.5 tesla = 5 kgauss
c     calculate an inverse momentum
               
               bfield  = 5.     ! kgauss
               konst   = 2.99792458e-4 ! GeV c-1 kgauss-1 cm-1
               
               invpt  = 1./(rh*konst*ch*bfield)
               dinvpt = (dcosl*rh**2 + drh*cosl**2)/
     &              (rh**4*(konst*bfield)**2)
               
c     write(6,*) 'fit: invpt,tanl:',invpt,tanl
               
c     calculate track parameters
c     1st point on svt
c     use spt on 1st layer to get the reference cylinder
c     phi will be the parameter with no error
               
               x0   = xh  + rh*cosd(phi) + vtx(1).x(1)
               y0   = yh  + rh*sind(phi) + vtx(1).x(2)
c               z0   = z(2) ! new z0 calc follows wkw
               pha=phi*3.1415927/180.0 ! wkw
               z0   = a(1)+a(2)*pha+vtx(1).x(3) ! wkw
               r0   = sqrt(x0**2+y0**2)
               phi0 = atan2d(y0,x0)
               
               dx0   = dxh + drh*cosd(phi)**2
               dy0   = dyh + drh*sind(phi)**2
c               dz0   = dz(2) ! new dz0 calc follows wkw
               dz0   = sqrt(da(1)**2.0+(pha*da(2))**2.0)
               dr0   = (dx0*x0**2 + dy0*y0**2)/r0**2
               dx0y0 = 0.       ! this is wrong but we will assume it 
                                ! for the time being.
               dphi0 = 0.       ! by convention
               
               dphi0invpt   = 0.
               dr0invpt     = 0.
               dr0phi0     = 0.
               dtanlinvpt   = 0.
               dtanlphi0   = 0.
               dtanlr0     = 0.
               dz0invpt     = 0.
               dz0phi0     = 0.
               dz0r0       = 0.
               dz0tanl     = 0.

               
c              wkw: renormalize chi
c               track(itrack).chisq(1) = cchi2 ! circle fit chi-square
c               track(itrack).chisq(1) = cchi2/(3.47+280*exp(-13.7/abs(invpt)))
               track(itrack).chisq(1) = cchi2/
     1          (stkpar(1).c1norm(1)+
     2          stkpar(1).c1norm(2)*exp(stkpar(1).c1norm(3)/abs(invpt)))
c               track(itrack).chisq(2) = zchi2 ! linear fit chi-square
c               track(itrack).chisq(2) = zchi2/(45.5+14200*exp(-17.5/
c             abs(invpt)))
               track(itrack).chisq(2) = zchi2/
     1          (stkpar(1).c2norm(1)+
     2          stkpar(1).c2norm(2)*exp(stkpar(1).c2norm(3)/abs(invpt)))
c              wkw: store residuals
               do ispt=1,nspt
                  track(itrack).cres(ispt) = cir_res(ispt) 
                  track(itrack).lres(ispt) = lin_res(ispt)
               enddo                  
               track(itrack).dedx(1)  = 0.    ! energy loss
               track(itrack).dedx(2)  = 0.    ! error on above
               track(itrack).invpt    = invpt ! inverse radius of 
                                                    ! curvature
               track(itrack).psi      = psi   ! azimuthal angle of 
                                                    !momentum at first point
               track(itrack).tanl     = tanl  ! pith angle at first point
               track(itrack).phi0     = phi0  ! 1st point coordinate
               track(itrack).r0       = r0
               track(itrack).z0       = z0
               track(itrack).flag     = 1     ! primary fit ok
               track(itrack).cov(1)   = dinvpt
               track(itrack).cov(2)   = dphi0invpt
               track(itrack).cov(3)   = dphi0
               track(itrack).cov(4)   = dr0invpt
               track(itrack).cov(5)   = dr0phi0
               track(itrack).cov(6)   = dedx_diff
               track(itrack).cov(7)   = lowest_dedx
               track(itrack).cov(8)   = dtanlphi0
               track(itrack).cov(9)   = dtanlr0
               track(itrack).cov(10)  = dtanl
               track(itrack).cov(11)  = dz0invpt
               track(itrack).cov(12)  = dz0phi0
               track(itrack).cov(13)  = dz0r0
               track(itrack).cov(14)  = dz0tanl
               track(itrack).cov(15)  = dz0
               
            else

c     the particle is (or assumed to be) a secondary
c     the main vertex is therefore not included in the fit.

c     load space points in work arrays
               
               ispt = 4
               layer(1) = 0
               layer(2) = 0
               layer(3) = 0
               nspt = track(itrack).nspt
c              if (nspt.gt.6) nspt=6 ! wkw
 
               nrows=max_spt
               table_off(1) = 3*itrack
               ok = tls_Search_Near_i(groups_h.nok,groups(1).id1,
     +              groups(2).id1,itrack,table_off,nrows)
               if( .not. SCV_STATE( ok, 'i')) then
                  call SCV_MESSAGES( ok, mess )
                  write(6,*)' in stk_fit:',mess
               endif
               if( nrows .ne. track(itrack).nspt) then
                  write(6,*) 'nrows=',nrows, 'for track',itrack
               endif
               
               do i = 1, nrows
                  the_spt     = groups(table_off(i)).id2
                  kspt(ispt)   = the_spt
c     In order to make code similar to case where vertex
c     is used in fit, transform to remove vertex offset 
c     here too (wkw)
c     Find out which layer hits are on and ensure that spt(1-3)
c     are on different layers H.Caines 4.97      
                  r2 = spt(the_spt).x(1)**2 + spt(the_spt).x(2)**2
                  if( (30.0 .lt. r2) .and. (r2 .lt. 70.0)) then
                     layer(1) = layer(1)+1
                     if( layer(1) .gt. 1) then
                        x(ispt)     = spt(the_spt).x(1) - vtx(1).x(1) ! wkw
                        y(ispt)     = spt(the_spt).x(2) - vtx(1).x(2) ! wkw
                        z(ispt)     = spt(the_spt).x(3) - vtx(1).x(3) ! wkw
                        dx(ispt)    = spt(the_spt).cov(1)
                        dy(ispt)    = spt(the_spt).cov(2)
                        dz(ispt)    = spt(the_spt).cov(3)
c     wkw: set best resolution to be 10 microns
                        if (dx(ispt).lt.0.001) dx(ispt) = 0.001
                        if (dy(ispt).lt.0.001) dy(ispt) = 0.001
                        if (dz(ispt).lt.0.001) dz(ispt) = 0.001
                        ispt = ispt + 1
                     else
                        x(1)   = spt(the_spt).x(1) - vtx(1).x(1) ! wkw
                        y(1)   = spt(the_spt).x(2) - vtx(1).x(2) ! wkw
                        z(1)   = spt(the_spt).x(3) - vtx(1).x(3) ! wkw
                        dx(1)  = spt(the_spt).cov(1)
                        dy(1)  = spt(the_spt).cov(2)
                        dz(1)  = spt(the_spt).cov(3)
c     wkw: set best resolution to be 10 microns
                        if (dx(1).lt.0.001) dx(1) = 0.001
                        if (dy(1).lt.0.001) dy(1) = 0.001
                        if (dz(1).lt.0.001) dz(1) = 0.001
                     endif
                  elseif( (90.0 .lt. r2) .and. (r2 .lt. 150.0)) then
                     layer(2) = layer(2)+1
                     if( layer(2) .gt. 1) then
                        x(ispt)     = spt(the_spt).x(1) - vtx(1).x(1) ! wkw
                        y(ispt)     = spt(the_spt).x(2) - vtx(1).x(2) ! wkw
                        z(ispt)     = spt(the_spt).x(3) - vtx(1).x(3) ! wkw
                        dx(ispt)    = spt(the_spt).cov(1)
                        dy(ispt)    = spt(the_spt).cov(2)
                        dz(ispt)    = spt(the_spt).cov(3)
c     wkw: set best resolution to be 10 microns
                        if (dx(ispt).lt.0.001) dx(ispt) = 0.001
                        if (dy(ispt).lt.0.001) dy(ispt) = 0.001
                        if (dz(ispt).lt.0.001) dz(ispt) = 0.001
                        ispt = ispt + 1
                     else
                        x(2)   = spt(the_spt).x(1) - vtx(1).x(1) ! wkw
                        y(2)   = spt(the_spt).x(2) - vtx(1).x(2) ! wkw
                        z(2)   = spt(the_spt).x(3) - vtx(1).x(3) ! wkw
                        dx(2)  = spt(the_spt).cov(1)
                        dy(2)  = spt(the_spt).cov(2)
                        dz(2)  = spt(the_spt).cov(3)
c     wkw: set best resolution to be 10 microns
                        if (dx(2).lt.0.001) dx(2) = 0.001
                        if (dy(2).lt.0.001) dy(2) = 0.001
                        if (dz(2).lt.0.001) dz(2) = 0.001
                     endif
                  elseif( (175.0 .lt. r2) .and. (r2 .lt. 250.0)) then
                     layer(3) = layer(3)+1
                     if( layer(3) .gt. 1) then
                        x(ispt)     = spt(the_spt).x(1) - vtx(1).x(1) ! wkw
                        y(ispt)     = spt(the_spt).x(2) - vtx(1).x(2) ! wkw
                        z(ispt)     = spt(the_spt).x(3) - vtx(1).x(3) ! wkw
                        dx(ispt)    = spt(the_spt).cov(1)
                        dy(ispt)    = spt(the_spt).cov(2)
                        dz(ispt)    = spt(the_spt).cov(3)
c     wkw: set best resolution to be 10 microns
                        if (dx(ispt).lt.0.001) dx(ispt) = 0.001
                        if (dy(ispt).lt.0.001) dy(ispt) = 0.001
                        if (dz(ispt).lt.0.001) dz(ispt) = 0.001
                        ispt = ispt + 1
                     else
                        x(3)   = spt(the_spt).x(1) - vtx(1).x(1) ! wkw
                        y(3)   = spt(the_spt).x(2) - vtx(1).x(2) ! wkw
                        z(3)   = spt(the_spt).x(3) - vtx(1).x(3) ! wkw
                        dx(3)  = spt(the_spt).cov(1)
                        dy(3)  = spt(the_spt).cov(2)
                        dz(3)   = spt(the_spt).cov(3)
c     wkw: set best resolution to be 10 microns
                        if (dx(3).lt.0.001) dx(3) = 0.001
                        if (dy(3).lt.0.001) dy(3) = 0.001
                        if (dz(3).lt.0.001) dz(3) = 0.001
                     endif
                  endif
               end do
            
c     calculate circle parameters
               
               status = svt_fit_circle3pts(x,y,xh,yh,rh)
               
               if (status.eq.0) then ! fit is OK
               
c     circle center coordiantes, radius, azimuthal angle
c     (xh,yh) : coordinates of the circle center
c     rh      : radius of the circle
c     phi     : angle between vector pointing from (xh,yh) to (x(2),y(2)) 
c               and  the x-axis
c     psi     : phi+90 : direction of the particle in transverse plane
c     tanl    : slope of the particle direction in the z,s plane
c     bfield  : magnetic filed strenght, bfield assume positive-z direction
c     konst   : conversion factor to get the momentum from the radius of 
c               curvature
c     units are GeV c-1 kgauss-1 cm-1
c     
c     Track Parameters Reconstruction
               
c     coordinates of center, radius and assoicated errors
               
c                  phi  = atan2d(y(2)-yh,x(2)-xh)
c                 take first point (first svt point) as reference
                  phi  = atan2d(y(1)-yh,x(1)-xh) ! wkw
               
                  dxh    = 0.
                  dyh    = 0.
                  dxhyh  = 0.
                  drh    = 0.
               
c     Track sign: the sign of the track is determined from the vector product
c     of two vectors pointing from the center of the circle to the 
c     first two points on the svt - this of course assumes the particle
c     is moving away from the beam axis - not coming back
               
                  v1x  = x(2) - xh
                  v1y  = y(2) - yh
                  v2x  = x(3) - xh
                  v2y  = y(3) - yh
                  if (v1x*v2y-v2x*v1y .gt. 0.) then
                     ch = -1.
                  else
                     ch = 1.
                  end if
               
c     vector direction
               
c                  phi  = atan2d(y(2)-yh,x(2)-xh)
                  phi  = atan2d(y(1)-yh,x(1)-xh) ! wkw
                  dtanphi = (dyh*v1x**2 + dxh*v1y**2 + 
     &                 2.*v1x*v1y*dxhyh)/v1x**4
                  dphi = dtanphi*v1x**4/(v1x**2+v1y**2)**2
               
c     z-motion fit from phase
               
                  nspt = track(itrack).nspt
                  if (nspt.gt.6) nspt=6 ! wkw
                  ispt = 0
                  do i = 1, nspt
                     ispt        = ispt + 1
                     phase(ispt) = atan2((y(ispt)-yh),
     &                    (x(ispt)-xh))
                     xd=(x(ispt))-xh
                     yd=(y(ispt))-yh
                     dphase(ispt) = (yd*dx(ispt)/
     &                    (yd**2.+xd**2.))**2.
                     dphase(ispt) =  dphase(ispt) +
     &                    (xd*dy(ispt)/(yd**2.+xd**2.))**2. 
                     dphase(ispt) = sqrt(dphase(ispt))
                  end do
                  call linfitr(phase,z,dphase,dz,nspt,a,da,zchi2,
     &                 lin_res)
               
                  tanl = -a(2)*ch/rh
                  cosl = 1./sqrt(1.+tanl**2)
               
                  dtanl = da(2)
                  dcosl = dtanl*tanl**2/(1.+tanl**2)**3
               
c     direction of the particle in the transverse plane
               
                  if (ch .gt. 0.) then
                     psi  = phi - 90.
                     dpsi = dphi
                  else
                     psi  = phi + 90.
                     dpsi = dphi
                  end if
                  if (psi.gt.360) psi=psi-360
                  if (psi.lt.0) psi=psi+360          

c     assume uniform field of 0.5 tesla = 5 kgauss
c     calculate an inverse momentum
               
                  bfield  = 5.     ! kgauss
                  konst   = 2.99792458e-4 ! GeV c-1 kgauss-1 cm-1
               
                  invpt  = 1./(rh*konst*ch*bfield)

                  dinvpt = (dcosl*rh**2 + drh*cosl**2)/
     &                 (rh**4*(konst*bfield)**2)
               
c     write(6,*) 'fit: invpt,tanl:',invpt,tanl
               
c     calculate track parameters
c     1st point on svt
c     use spt on 1st layer to get the reference cylinder
c     phi will be the parameter with no error
               
                  x0   = xh  + rh*cosd(phi) + vtx(1).x(1)
                  y0   = yh  + rh*sind(phi) + vtx(1).x(2)
c                  z0   = z(2) ! new z0 calc follows wkw
                  pha=phi*3.1415927/180.0 ! wkw
                  z0   = a(1)+a(2)*pha+vtx(1).x(3) ! wkw
                  r0   = sqrt(x0**2+y0**2)
                  phi0 = atan2d(y0,x0)
               
                  dx0   = dxh + drh*cosd(phi)**2
                  dy0   = dyh + drh*sind(phi)**2
c                  dz0   = dz(2) ! new dz0 calc follows wkw
                  dz0   = sqrt(da(1)**2.0+(pha*da(2))**2.0) ! wkw
                  dr0   = (dx0*x0**2 + dy0*y0**2)/r0**2
                  dx0y0 = 0.       ! this is wrong but we will assume it 
                                   ! for the time being.
                  dphi0 = 0.       ! by convention
               
                  dphi0invpt   = 0.
                  dr0invpt     = 0.
                  dr0phi0     = 0.
                  dtanlinvpt   = 0.
                  dtanlphi0   = 0.
                  dtanlr0     = 0.
                  dz0invpt     = 0.
                  dz0phi0     = 0.
                  dz0r0       = 0.
                  dz0tanl     = 0.
               
c     store fit results
               
                  track(itrack).chisq(1) = 0 ! circle fit has no chi-sqr
                                                   ! for 3 points (wkw)
c                  track(itrack).chisq(2) = zchi2 ! linear fit chi-square
c                  track(itrack).chisq(2) = zchi2/(45.5+14200*exp(-17.5/
C                             abs(invpt)))
                  track(itrack).chisq(2) = zchi2/
     &                 (stkpar(1).c2norm(1)+
     &                 stkpar(1).c2norm(2)*
     &                 exp(stkpar(1).c2norm(3)/abs(invpt)))
c                 wkw: store residuals
                  do ispt=1,nspt
                     track(itrack).cres(ispt) = 0 ! 3 points
                     track(itrack).lres(ispt) = lin_res(ispt)
                  enddo                  
                  track(itrack).dedx(1)  = 0. ! energy loss
                  track(itrack).dedx(2)  = 0. ! error on above
                  track(itrack).invpt    = invpt ! inverse radius of 
                                                       ! curvat ure
                  track(itrack).psi      = psi ! azimuthal angle of 
                                                     !momentum at first point
                  track(itrack).tanl     = tanl ! pith angle at first point
                  track(itrack).phi0     = phi0 ! 1st point coordinate
                  track(itrack).r0       = r0
                  track(itrack).z0       = z0
                  track(itrack).flag     = 2 ! wkw: well fit sec track 
c    more than 2 points
                  track(itrack).cov(1)   = dinvpt
                  track(itrack).cov(2)   = dphi0invpt
                  track(itrack).cov(3)   = dphi0
                  track(itrack).cov(4)   = dr0invpt
                  track(itrack).cov(5)   = dr0phi0
                  track(itrack).cov(6)   = dr0
                  track(itrack).cov(7)   = dtanlinvpt
                  track(itrack).cov(8)   = dtanlphi0
                  track(itrack).cov(9)   = dtanlr0
                  track(itrack).cov(10)  = dtanl
                  track(itrack).cov(11)  = dz0invpt
                  track(itrack).cov(12)  = dz0phi0
                  track(itrack).cov(13)  = dz0r0
                  track(itrack).cov(14)  = dz0tanl
                  track(itrack).cov(15)  = dz0
               
               else ! check of fit status (wkw)

                  track(itrack).chisq(1) = infinity ! circle fit chi-square
                  track(itrack).chisq(2) = infinity ! linear fit chi-square
c                 wkw: store residuals
                  do ispt=1,nspt
                     track(itrack).cres(ispt) = infinity
                     track(itrack).lres(ispt) = infinity
                  enddo                  
                  track(itrack).dedx(1)  = infinity ! energy loss
                  track(itrack).dedx(2)  = infinity ! error on above
                  track(itrack).pid      = infinity
                  track(itrack).invpt     = infinity ! inverse radius of curvature
                  track(itrack).psi      = infinity ! azimuthal angle of momentum at first point
                  track(itrack).tanl     = infinity ! pith angle at first point
                  track(itrack).phi0     = infinity ! 1st point coordinate
                  track(itrack).r0       = infinity
                  track(itrack).z0       = infinity
                  track(itrack).flag     = 0 ! flag=0 ! wkw: bad fit
                  track(itrack).cov(1)   = infinity
                  track(itrack).cov(2)   = infinity
                  track(itrack).cov(3)   = infinity
                  track(itrack).cov(4)   = infinity
                  track(itrack).cov(5)   = infinity
                  track(itrack).cov(6)   = infinity 
                  track(itrack).cov(7)   = infinity 
                  track(itrack).cov(8)   = infinity 
                  track(itrack).cov(9)   = infinity 
                  track(itrack).cov(10)  = infinity 
                  track(itrack).cov(11)  = infinity 
                  track(itrack).cov(12)  = infinity 
                  track(itrack).cov(13)  = infinity 
                  track(itrack).cov(14)  = infinity 
               
               end if ! check of secondary fit status

            end if ! pri vs sec

         else ! not enough points to fit (wkw)
c     write(6,1002)

            track(itrack).chisq(1) = infinity ! circle fit chi-square
            track(itrack).chisq(2) = infinity ! linear fit chi-square
c           wkw: store residuals
            do ispt=1,nspt
               track(itrack).cres(ispt) = infinity
               track(itrack).lres(ispt) = infinity
            enddo                  
            track(itrack).dedx(1)  = infinity ! energy loss
            track(itrack).dedx(2)  = infinity ! error on above
            track(itrack).pid      = 0        ! no pid at this time
            track(itrack).invpt    = infinity ! inverse radius of curvature
            track(itrack).psi      = infinity ! azimuthal angle of momentum at
c      first point
            track(itrack).tanl     = infinity ! pith angle at first point
            track(itrack).phi0     = infinity ! 1st point coordinate
            track(itrack).r0       = infinity
            track(itrack).z0       = infinity
            track(itrack).flag     = 0 ! flag=0 ! wkw: not enough hits
            track(itrack).cov(1)   = infinity
            track(itrack).cov(2)   = infinity
            track(itrack).cov(3)   = infinity
            track(itrack).cov(4)   = infinity
            track(itrack).cov(5)   = infinity
            track(itrack).cov(6)   = infinity 
            track(itrack).cov(7)   = infinity 
            track(itrack).cov(8)   = infinity 
            track(itrack).cov(9)   = infinity 
            track(itrack).cov(10)  = infinity 
            track(itrack).cov(11)  = infinity 
            track(itrack).cov(12)  = infinity 
            track(itrack).cov(13)  = infinity 
            track(itrack).cov(14)  = infinity 
            track(itrack).cov(15)  = infinity 

         end if ! not enough points to fit


      end do ! loop over tracks

      track_h.nok       = itrack-1

      WRITE(*,*) 'STK_AM_FIT: Stopping ###############################'
      STK_AM_FIT=STAFCV_OK
      RETURN
      END
