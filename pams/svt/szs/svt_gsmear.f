      program test

      character file_name*60
      integer   file_lun

      include 'svt_inc.f'

      record /svt_asics_data_st/svt_asics_data(svt_asics_data_nmax)
      record /svt_asics_para_st/svt_asics_para(svt_asics_para_nmax)
      record /svt_gsmear_para_st/svt_gsmear_para(svt_gsmear_para_nmax)
      record /svt_hit_st/svt_hit(svt_hit_nmax)

      integer svt_load_gsmear_para_st
      integer svt_print_gsmear_para_st

      integer status

      file_name = 'svt_gsmear_para_st.par'
      file_lun  = 1
      status = svt_load_gsmear_para_st(file_name,file_lun,svt_gsmear_para,svt_gsmear_para_n)
      if (status.ne.0) then
         write(6,*) 'does not work - abort'
         stop
      end if
      status = svt_print_gsmear_para_st(svt_gsmear_para,svt_gsmear_para_n)

      file_name = 'svt_hit_st.par '
      file_lun  = 2
      status = svt_load_hit_st(svt_hit,svt_hit_n,file_name,file_lun)
      if (status.ne.0) then
         write(6,*) 'does not work - abort'
         stop
      end if
      status = svt_print_hit_st(svt_hit,svt_hit_n)

      status = svt_gsmear(svt_gsmear_para,svt_gsmear_para_n,
     1     svt_hit,svt_hit_n,
     1     svt_asics_data,svt_asics_data_n,
     1     svt_asics_para,svt_asics_para_n)

      stop
      end


c==========================================================================================
      integer function svt_load_gsmear_para_st(file_name,file_lun,svt_gsmear_para,svt_gsmear_para_n)
      implicit none

c     description: load gaussian smearing parameters for svt detector into svt_gsmear_para_st

c     author: C. Pruneau, march 93, WSU

c     argument declaration
c     ====================
      character file_name*60
      integer   file_lun

      include 'svt_inc.f'

      record /svt_gsmear_para_st/svt_gsmear_para(svt_gsmear_para_nmax)

c     local declarations
c     ==================
      integer  i
      logical  done
      integer nx, ny, nxmax, nymax, iw
      real    mx, my, sx, sy, size_x, size_y, speed
      real    low_x, low_y, high_x, high_y, step_x, step_y, x0, y0, ww, wwsum, cte
      real    apitch, tpitch, xoff, yoff

      namelist /gsmeardef/iw,sx,sy,nx,ny,mx,my,nxmax,nymax,speed,xoff,yoff,apitch,tpitch

c     format
c     ======
 1010 format(1x,'-f-svt_load_gsmear_para_st: could not open hit definition file:',a)
 1011 format(1x,'-f-svt_load_gsmear_para_st: error reading hit definition file:',a)
 1012 format(1x,'-i-svt_load_gsmear_para_st: read ',i3,' definitions from',
     1             ' file:',a)
 1013 format(1x,'-f-eg_load_part_st: too many hit definitions in file:',a)

c     executable code
c     ===============

      svt_load_gsmear_para_st = 0

      open (file=file_name,unit=file_lun,form='formatted',
     1       status='unknown',err=100)

      done = .false.
      i    = 0

      iw     = 1
      sx     = 0.0100     ! 100 microns default value
      sy     = 0.0100   
      apitch = 0.0250     ! 250 microns anode width/pitch
      tpitch = 25.        ! 25 ns
      mx     = 3.
      my     = 3.
      nx     = 5
      ny     = 5
      nxmax  = 224        ! 224 anodes
      nymax  = 256        ! 256 time samples
      speed  = 0.001      ! 10 micron/ns = 1 cm/micro-s
      xoff   = -2.        ! 2. cm half transverse size
      yoff   = 2.         ! 2 cm half drift

      do while (.not.done)
         read(file_lun,nml=gsmeardef,err=200,end=300)

         svt_gsmear_para(iw).sx          = sx
         svt_gsmear_para(iw).sy          = sy
         svt_gsmear_para(iw).apitch      = apitch
         svt_gsmear_para(iw).tpitch      = tpitch
         svt_gsmear_para(iw).nx          = nx
         svt_gsmear_para(iw).ny          = ny
         svt_gsmear_para(iw).mx          = mx
         svt_gsmear_para(iw).my          = my
         svt_gsmear_para(iw).nxmax       = nxmax
         svt_gsmear_para(iw).nymax       = nymax
         svt_gsmear_para(iw).speed       = speed
         svt_gsmear_para(iw).xoff        = xoff
         svt_gsmear_para(iw).yoff        = yoff

         i = i + 1
         if (i.gt.svt_gsmear_para_nmax) then
            goto 400
         end if
         if (i.eq.200) done = .true.
      end do

c     status is ok, normal completion

      svt_gsmear_para_n = i

      write(6,1012) svt_gsmear_para_n, file_name

      svt_load_gsmear_para_st = 0

      close (file_lun)
      return

 100  continue
c     status is fatal error, could not open file
      write(6,1010) file_name
      svt_load_gsmear_para_st = -1
      return

 200  continue
c     status is fatal error, error reading file
      write(6,1011) file_name
      close (file_lun)
      svt_load_gsmear_para_st = -3
      return

 300  continue
c     status is OK, found EOF
      svt_gsmear_para_n = i
      close (file_lun)
      write(6,1012) svt_gsmear_para_n, file_name
      svt_load_gsmear_para_st = 0
      return

 400  continue
c     status is fatal error, error reading file
      write(6,1013) file_name
      close (file_lun)
      svt_load_gsmear_para_st = -5

      return
      end

c=================================================================================
      integer function svt_gsmear(svt_gsmear_para,svt_gsmear_para_n,
     1                            svt_hit,svt_hit_n,
     1                            svt_asics_data,svt_asics_data_n,
     1                            svt_asics_para,svt_asics_para_n)
      implicit none

c     description produce a gaussian smearing of space points and charge

c     author: C. Pruneau, march 93, WSU

c     argument declaration
c     ====================

      include 'svt_inc.f'

      record /svt_gsmear_para_st/svt_gsmear_para(svt_gsmear_para_nmax)
      record /svt_hit_st/svt_hit(svt_hit_nmax)
      record /svt_asics_data_st/svt_asics_data(svt_asics_data_nmax)
      record /svt_asics_para_st/svt_asics_para(svt_asics_para_nmax)

c     local declarations
c     ==================
      integer i_sample, i_anode
      integer iw, ihit
      integer nx, ny, nxmax, nymax 
      real    sx, sy, size_x, size_y, speed, mx, my
      real    low_x, low_y, high_x, high_y, step_x, step_y, x0, y0, ww, wwsum, cte
      real    xoff, yoff
      real    x, y
      real    work(256,224)

c     executable code
c     ===============

      svt_gsmear = 0

      iw = 0

      do ihit = 1, svt_hit_n

         if (iw.ne.svt_hit(ihit).wid) then

c     new wafer, reset work area to zero


            iw = svt_hit(ihit).wid

            sy = svt_gsmear_para(iw).sy ! sigma of gaussian (cm) - drift direction
            sx = svt_gsmear_para(iw).sx ! sigma of gaussian (cm) - transverse direction
            
            size_x = svt_gsmear_para(iw).apitch ! pitch anode (cm)
            size_y = svt_gsmear_para(iw).tpitch ! pitch time  (micro-seconds)
            
            ny = svt_gsmear_para(iw).ny ! number of sample for computation - drift direction
            nx = svt_gsmear_para(iw).nx ! number of sample for computation - transverse direction

            my = svt_gsmear_para(iw).my ! number of times sigma for computation - drift direction
            mx = svt_gsmear_para(iw).mx ! number of times sigma for computation - transverse direction
            
            nxmax = svt_gsmear_para(iw).nxmax
            nymax = svt_gsmear_para(iw).nymax
            
            speed = svt_gsmear_para(iw).speed
            
            low_y  = svt_hit(ihit).ly - my*sy
            high_y = svt_hit(ihit).ly + my*sy
            step_y = sy/ny
            
            low_x  = svt_hit(ihit).lx - mx*sx
            high_x = svt_hit(ihit).lx + mx*sx
            step_x = sx/nx
            
            yoff = svt_gsmear_para(iw).yoff
            xoff = svt_gsmear_para(iw).xoff

            cte = step_x*step_y/(sx*sy*3.1415927)
            
            x0 = svt_hit(ihit).lx
            y0 = svt_hit(ihit).ly

            if (iw.gt.0) then

c     store the data to wafer structure and reset work area to zero

               do i_anode  = 1, svt_asics_para(iw).n_anode
                  do i_sample = 1, svt_asics_para(iw).n_sample

                     svt_asics_data(iw).q(i_sample,i_anode) = work(i_sample,i_anode) 

c     reset for next wafer

                     work(i_sample,i_anode) = 0.

                  end do
               end do
               
            else

c     this is the first wafer data, set work area to zero

               do i_anode  = 1, svt_asics_para(iw).n_anode
                  do i_sample = 1, svt_asics_para(iw).n_sample
                     work(i_sample,i_anode) = 0.
                  end do
               end do
            end if

         end if

         wwsum = 0.

         do y = low_y, high_y, step_y

            ny = 1 + (yoff - y)/speed/size_y

            do x = low_x, high_x, step_x
               nx = 1 + (xoff + x)/size_x

               ww = cte*exp(-(((x-x0)/sx)**2+((y-y0)/sy)**2))
c               write(6,*) 'cte,x0,y0,x,y,ww,nx,ny',cte,x0,y0,x,y,ww,nx,ny

               wwsum = wwsum + ww
               if (nx.gt.0.and.nx.le.nxmax.and.ny.gt.0.and.ny.le.nymax) then
                  work(nx,ny) = work(nx,ny) + ww
               end if
            end do

         end do
      end do

c     store the data to wafer structure

      do i_anode  = 1, svt_asics_para(iw).n_anode
         do i_sample = 1, svt_asics_para(iw).n_sample
            
            svt_asics_data(iw).q(i_sample,i_anode) = work(i_sample,i_anode) 
            
         end do
      end do

      return
      end

c==========================================================================================
      integer function svt_print_gsmear_para_st(svt_gsmear_para,svt_gsmear_para_n)
      implicit none

c     description: print gaussian smearing parameters for svt detector into svt_gsmear_para_st

c     author: C. Pruneau, march 93, WSU

c     argument declaration
c     ====================
      include 'svt_inc.f'

      record /svt_gsmear_para_st/svt_gsmear_para(svt_gsmear_para_nmax)

c     local declarations
c     ==================
      integer i

c     format
c     ======
 1001 format(1x,'-f-svt_print_gsmear_para_st: svt_gsmear_para_n argument out of bounds:',i8)
 1002 format(1x,' wafer read:',i5/1x,'wafer# nx/mx/sx/xoff/apitch ny/my/sy/yoff/tpitch speed')
 1003 format(1x,i3,2(i3,'/',f5.1,'/',f6.2,'/',f6.2,'/',f8.2,3x),f12.6)

c     executable code
c     ===============

      if (svt_gsmear_para_n.lt.1.or.svt_gsmear_para_n.gt.svt_gsmear_para_nmax) then
c     status is error, too many wafer requested
         svt_print_gsmear_para_st = -1
         write(6,1001) svt_gsmear_para_n
         return
      end if

      write(6,1002) svt_gsmear_para_n

      do i = 1, svt_gsmear_para_n

         write(6,1003) i,
     1                   svt_gsmear_para(i).nx,
     1                   svt_gsmear_para(i).mx,
     1                   svt_gsmear_para(i).sx,
     1                   svt_gsmear_para(i).xoff,
     1                   svt_gsmear_para(i).apitch,
     1                   svt_gsmear_para(i).ny,
     1                   svt_gsmear_para(i).my,
     1                   svt_gsmear_para(i).sy,
     1                   svt_gsmear_para(i).yoff,
     1                   svt_gsmear_para(i).tpitch,
     1                   svt_gsmear_para(i).speed

      end do

c     status is ok - normal completion

      write(6,*) 'ths job is done'

      svt_print_gsmear_para_st = 0

      return
      end


c==========================================================================================
      integer function svt_load_hit_st(svt_hit,svt_hit_n,file_name,file_lun)
      implicit none

c     description: load gaussian smearing parameters for svt detector into svt_hit_st

c     author: C. Pruneau, march 93, WSU

c     argument declaration
c     ====================
      character file_name*60
      integer   file_lun

      include 'svt_inc.f'

      record /svt_hit_st/svt_hit(svt_hit_nmax)

c     local declarations
c     ==================
      integer  i,ll
      logical  done
      integer   htid             ! track id
      real      hx,hy,hz           ! hit, space point
      real      hth, hph          ! polar, azimuthal angle of track at space point
      integer   hwid             ! wafer id
      real      hlx              ! (local) x-coordinate wafer frame, transverse to drift direction
      real      hly              ! (local) y-coordinate wafer frame, drift direction
      real      hdedx            ! energy loss returned by GEANT
      real      ht               ! TOF
      real      hthn, hthd        ! angles of track relative to normal, and drift direction

      namelist /hitdef/htid,hx,hy,hz,hth,hph,hwid,hlx,hly,hdedx,ht,hthn,hthd

c     format
c     ======
 1010 format(1x,'-f-svt_load_hit_st: could not open hit definition file:',a)
 1011 format(1x,'-f-svt_load_hit_st: error reading hit definition file:',a)
 1012 format(1x,'-i-svt_load_hit_st: read ',i3,' definitions from',
     1             ' file:',a)
 1013 format(1x,'-f-eg_load_part_st: too many hit definitions in file:',a)

c     executable code
c     ===============

      svt_load_hit_st = 0

      ll = index(file_name,' ') -1

      write(6,*) file_name, ll, file_lun
      open (file=file_name(1:ll),unit=file_lun,form='formatted',
     1       status='unknown',err=100)

      done = .false.
      i    = 0

      do while (.not.done)

      write(6,*) 'a ',file_name, ll, file_lun
            
         read(file_lun,nml=hitdef,err=200,end=300)

         i = i + 1
         if (i.gt.svt_hit_nmax) then
            goto 400
         end if
         if (i.eq.200) done = .true.

         svt_hit(i).tid  = htid
         svt_hit(i).x    = hx
         svt_hit(i).y    = hy
         svt_hit(i).z    = hz
         svt_hit(i).th   = hth
         svt_hit(i).ph   = hph
         svt_hit(i).wid  = hwid
         svt_hit(i).lx   = hlx
         svt_hit(i).ly   = hly
         svt_hit(i).dedx = hdedx
         svt_hit(i).t    = ht
         svt_hit(i).thn  = hthn
         svt_hit(i).thd  = hthd

      end do

c     status is ok, normal completion

      svt_hit_n = i

      write(6,1012) svt_hit_n, file_name(1:ll)

      svt_load_hit_st = 0
      close (file_lun)

      return

 100  continue
c     status is fatal error, could not open file
      write(6,1010) file_name(1:ll)
      svt_load_hit_st = -1
      return

 200  continue
c     status is fatal error, error reading file
      write(6,1011) file_name(1:ll)
      close (file_lun)
      svt_load_hit_st = -3
      return

 300  continue
c     status is OK, found EOF
      svt_hit_n = i
      write(6,1012) svt_hit_n, file_name(1:ll)
      close (file_lun)
      svt_load_hit_st = 0
      return

 400  continue
c     status is fatal error, error reading file
      write(6,1013) file_name(1:ll)
      close (file_lun)
      svt_load_hit_st = -5

      return
      end

c==========================================================================================
      integer function svt_print_hit_st(svt_hit,svt_hit_n)
      implicit none
c
c     description: print gaussian smearing parameters for svt detector into svt_hit_st
c
c     author: C. Pruneau, march 93, WSU

c     argument declaration
c     ====================
      include 'svt_inc.f'

      record /svt_hit_st/svt_hit(svt_hit_nmax)

c     local declarations
c     ==================
      integer i

c     format
c     ======
 1001 format(1x,'-f-svt_print_hit_st: svt_hit_n argument out of bounds:',i8)
 1002 format(1x,'hit# tid, x, y, z, th, ph, wid, lx, ly, dedx, t, thn, thd')
 1003 format(1x,i3,1x,i3,3f6.2,2f7.2,i3,2f8.2,f6.2,f6.2,2f7.2)

c     executable code
c     ===============

      if (svt_hit_n.lt.1.or.svt_hit_n.gt.svt_hit_nmax) then
c     status is error, too many wafer requested
         svt_print_hit_st = -1
         write(6,1001) svt_hit_n
         return
      end if

      write(6,1002) 

      do i = 1, svt_hit_n

         write(6,1003) i,
     1        svt_hit(i).tid,
     1        svt_hit(i).x,svt_hit(i).y,svt_hit(i).z,
     1        svt_hit(i).th,svt_hit(i).ph,
     1        svt_hit(i).wid,
     1        svt_hit(i).lx,svt_hit(i).ly,
     1        svt_hit(i).dedx,
     1        svt_hit(i).t,
     1        svt_hit(i).thn,svt_hit(i).thd
 
      end do

      
c     status is ok - normal completion

      svt_print_hit_st = 0

      return
      end

c=====================================================================================
      integer function svt_asics(svt_asics_data,svt_asics_data_n,
     1     svt_asics_calib,svt_asics_calib_n,
     1     svt_asics_para,svt_asics_para_n)
      implicit none
c
c     perform pedestal subtraction and
c     finds cluster on a given wafer and output zero suppresed info. 
c     HERE I ASSUME A CLUSTER IS A SEQUENCE OF SAMPLES ABOVE THRESHOLD
C     IN ONE ANODE. I.E. AN ACTUAL HIT WILL BE COMPOSED OF POSSIBLY
C     MANY OF THIS "CLUSTERS"
c     this is meant to be the code ran by the ASICs
c
c     returns 0 if no error occured
c            -1 if some fatal error occured
c             1 if some non fatal error occured
c
c     svt_asics_data is a struc containing the svt_asics_data information relative to the current
c     wafer being analysed.
c
c     svt_asics_para().n_anode     : number of active anodes on current wafer
c     svt_asics_para().n_sample    : number of samples read out 
c     svt_asics_para().high        : high threshold
c     svt_asics_para().n_high      : number of contiguous pixels required to be above high threshold
c     svt_asics_para().low         : low threshold
c     svt_asics_para().n_before    : number of pixels recorded on the leading edge before low threshold
c     svt_asics_para().n_after     : number of pixels recorded on the trailing edge after low threshold

c     svt_asics_data().q           : charge read out, it is an array of dimension: n_sample x n_anode
c     svt_asics_data().select      : on output, array of pixels selected for readout
c

c     global declarations
c     ===================

      include 'svt_inc.f'

      record  /svt_asics_data_st/svt_asics_data(svt_asics_data_nmax)
      record  /svt_asics_calib_st/svt_asics_calib(svt_asics_calib_nmax)
      record  /svt_asics_para_st/svt_asics_para(svt_asics_para_nmax)

c     local declarations
c     ==================

      integer anode, i, i1, i2, n, i2p, i3
      integer imin, imax, ilast
      integer iwafer, iw

      logical done1, done2

c     executable
c     ==========
c     assume success

      svt_asics = 0
      
c     loop on all wafers present
       
      do iwafer = 1, svt_asics_data_n
         
         iw = svt_asics_data(iwafer).wafer
         
c     reset select array
         
         do anode = 1, svt_asics_para(iw).n_anode
            do i = 1, svt_asics_para(iw).n_sample
               svt_asics_data(iw).select(i,anode) = 0
            end do
         end do
         
c     start analysis here
         
 
         do anode = 1, svt_asics_para(iw).n_anode
            
            imin  = 1
            imax  = svt_asics_para(iw).n_sample + 1
            done1 = .false.
            i1    = imin
            
            do while (.not.done1)
               i1 = i1 + 1
               if (svt_asics_data(iw).q(i1,anode).gt.svt_asics_para(iw).high) then
                  
c     check if enough samples above high threshold
                   
                  done2  = .false.
                  i2     = i1
                  n      = 1
                  do while (.not.done2)
                     i2 = i2 + 1
                     if (i2.lt.imax) then
                        if (svt_asics_data(iw).q(i2,anode).gt.svt_asics_para(iw).high) then
                           n = n + 1
                        else
                           done2 = .true.
                        end if
                     else
                        done2 = .true.
                     end if 
                  end do

                  if (n.ge.svt_asics_para(iw).nhigh) then
                     
c     have a cluster - proceed to selection of samples above high threshold
                     
    
                     do i2 = i1, i1+n-1
                        svt_asics_data(iw).select(i2,anode) = 1
                     end do
                     
c     look backward to see how many samples above low threshold

                     done2 = .false.
                     i2 = i1
                     do while (.not.done2)
                        i2  = i2 - 1
                        if (i2.gt.imin) then
                           if (svt_asics_data(iw).q(i2,anode).gt.svt_asics_para(iw).low) then

                             svt_asics_data(iw).select(i2,anode) = 1

                         else
                              done2 = .true.
                           end if
                        else
                           done2 = .true.
                        end if
                     end do

c     select leading samples backward
                     
                     done2 = .false.
                     i2p = 0
                     i2  = i2 + 1
                     do while (.not.done2)
                        i2  = i2 - 1
                        i2p = i2p + 1
                        if (i2p.le.svt_asics_para(iw).nlow.and.i2.gt.imin) then
                           svt_asics_data(iw).select(i2,anode) = 1
                        else
                           done2 = .true.
                        end if
                     end do

c     look forward to see how many samples above low threshold
                     
                     done2 = .false.
                     i2 = i1 + n
                     do while (.not.done2)
                        i2  = i2 + 1
                        if (i2.le.imax) then
                           if (svt_asics_data(iw).q(i2,anode).gt.svt_asics_para(iw).low) then
                              svt_asics_data(iw).select(i2,anode) = 1
                           else
                              done2 = .true.
                           end if
                        else
                           done2 = .true.
                        end if
                     end do
                     
c     select trailing samples
                     
                     done2 = .false.
                     i2p = 0
                     i2 = i2 - 1
                     do while (.not.done2)
                        i2  = i2  + 1
                        i2p = i2p + 1
                        if (i2p.le.svt_asics_para(iw).nhigh.and.i2.gt.imax) then
                           svt_asics_data(iw).select(i2,anode) = 1
                        else
                           done2 = .true.
                        end if
                     end do
                     
                  else
                     
c     not a good cluster, proceed forward...
                     
                     i1 = i1 + n
                  end if
                  
               end if
            end do 
            
         end do 
         
         do anode = 1, svt_asics_para(iw).n_anode
            do i1 = 1, svt_asics_para(iw).n_sample
               i2 = i1 + svt_asics_para(iw).n_sample*(anode-1) - 1
               i3 = mod(i2,32)
               i2 = 1 + (i2-i3)/32
               if (svt_asics_data(iw).select(i1,anode).gt.0) then
                  svt_asics_data(iw).mask(i2) = ibset(svt_asics_data(iw).mask(i2),i3)
                  write(6,*) ' anode, sample, i2, i3, v:',anode,i1,i2,i3,svt_asics_data(iw).mask(i2)
               end if
            end do
         end do
         
      end do
      
      return
      end
