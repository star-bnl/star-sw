      integer function svt_fcluster(svt_adc_h,   svt_adc,
     1                              svt_time_h,  svt_time,
     1                              svt_anode_h, svt_anode,
     1                              svt_wafer_h, svt_wafer,
     1                              svt_geom_h,  svt_geom,
     1                              svt_drift_h, svt_drift,
     1                              svt_spt_h,   svt_spt)
      implicit none

c     created: March 17, 93, C. Pruneau, WSU

c     description: this routine performs cluster search on every wafer and output
c     the results into structure svt_spt
c     Input Arguments: 
c
c     svt_adc_h     : header to table svt_adc
c     svt_adc       : rows of table   svt_adc
c     svt_time_h    : header to table svt_time
c     svt_time      : rows of table   svt_time
c     svt_anode_h   : header to table svt_anode
c     svt_anode     : rows of table   svt_anode
c     svt_wafer_h   : header to table svt_wafer
c     svt_wafer     : rows of table   svt_wafer
c     svt_geom_h    : header to table svt_geom
c     svt_geom      : rows of table   svt_geom
c     svt_drift_h   : header to table svt_drift
c     svt_drift     : rows of table   svt_drift

c     Output Arguments:
c
c     svt_spt_h     : header to table svt_spt
c     svt_spt       : rows of table   svt_spt

c     Argument declarations:

      INCLUDE 'tas_structures_inc'
      INCLUDE 'tas_user_codes_inc'

      INCLUDE 'szs_svt_adc_pars_inc'
      INCLUDE 'szs_svt_adc_st_inc'

      INCLUDE 'szs_svt_time_pars_inc'
      INCLUDE 'szs_svt_time_st_inc'

      INCLUDE 'szs_svt_anode_pars_inc'
      INCLUDE 'szs_svt_anode_st_inc'

      INCLUDE 'szs_svt_wafer_pars_inc'
      INCLUDE 'szs_svt_wafer_st_inc'

      INCLUDE 'svg_svt_geom_pars_inc'
      INCLUDE 'svg_svt_geom_st_inc'

      INCLUDE 'svg_svt_drift_pars_inc'
      INCLUDE 'svg_svt_drift_st_inc'

      INCLUDE 'szs_svt_spt_pars_inc'
      INCLUDE 'szs_svt_spt_st_inc'

      RECORD/ table_head_st/ svt_geom_h
      RECORD/ svt_geom_row_st / svt_geom(*)

      RECORD/ table_head_st/ svt_adc_h
      RECORD/ svt_adc_row_st / svt_adc(*)

      RECORD/ table_head_st/ svt_time_h
      RECORD/ svt_time_row_st / svt_time(*)

      RECORD/ table_head_st/ svt_anode_h
      RECORD/ svt_anode_row_st / svt_anode(*)

      RECORD/ table_head_st/ svt_wafer_h
      RECORD/ svt_wafer_row_st / svt_wafer(*)

      RECORD/ table_head_st/ svt_drift_h
      RECORD/ svt_drift_row_st / svt_drift(*)

      RECORD/ table_head_st/ svt_spt_h
      RECORD/ svt_spt_row_st / svt_spt(*)

c     Local Declarations
c     ==================
      integer adc_nok, time_nok, anode_nok, wafer_nok, drift_nok, spt_nok
      integer iadc, itime, ianode, iwafer, idrift, ispt
      integer i_wafer, i_anode, i_time, isample
      logical done, error
      integer nd, na
      parameter (nd=512)
      parameter (na=228)
      real    svt_work(nd,na)

c     executable code
c     ===============
      adc_nok   = svt_adc_h.nok
      time_nok  = svt_time_h.nok
      anode_nok = svt_anode_h.nok
      wafer_nok = svt_wafer_h.nok
      drift_nok = svt_drift_h.nok
      spt_nok   = 0

c     check if any data

      if (adc_nok.eq.0) goto 900

c     check if all data...

      if (time_nok.eq.0.or.anode_nok.eq.0.or.wafer.eq.0.or.drift_nok.eq.0) goto 800

      done = .false.

      i_wafer = 0

      do while (iwafer.lt.wafer_nok.and..not.done)

         i_wafer = i_wafer + 1

c     reset wafer work area to zero

         do ianode = 1, na
            do isample = 1, nd
               svt_work(isample,ianode) = 0
            end do
         end do

c     copy current wafer to work array

         iwafer     = svt_wafer(i_wafer).wafer
         nanode     = svt_wafer(i_wafer).nanode
         frst_anode = svt_wafer(i_wafer).ptr

         do i_anode = 0, nanode-1
            ianode    = svt_anode(frst_anode+i_anode).anode
            nclus     = svt_anode(frst_anode+i_anode).nclus
            frst_clus = svt_anode(frst_anode+i_anode).ptr
            
            do i_time = 0, nclus-1
               itime    = svt_time(frst_clus+i_time).time
               nadc     = svt_time(frst_clus+i_time).nadc
               frst_adc = svt_time(frst_clus+i_time).ptr
               
               do i_adc = 0, nadc-1
                  svt_work(itime+i_adc,ianode) = svt_adc(frst_adc+i_adc).adc
               end do

            end do
         end do

c     search for cluster candidates on current wafer

         svt_high  = svt_drift(iwafer).high
         svt_low   = svt_drift(iwafer).low
         svt_nhigh = svt_drift(iwafer).nhigh
         svt_nlow  = svt_drift(iwafer).nlow
         svt_nbef  = svt_drift(iwafer).n_before
         svt_naft  = svt_drift(iwafer).n_after
         n_anode   = svt_drift(iwafer).n_anode
         n_sampmax  = svt_drift(iwafer).n_sampmax
         n_clusmax = svt_drift(iwafer).n_clusmax


c     rough estimate of cluster position and size

         status = cluster(svt_work,n_sample,n_anode,svt_clist,n_sampmax,n_clusmax,nclus,svt_high,svt_low)

c     get fitting parameter first guest

         do iclus = 1, nclus

            xx(1) = 0.
            xx(2) = 0.
            xx2(1) = 0.
            xx2(2) = 0.
            do isamp = 1, nsamp(iclus)
               xx(1)  = xx(1) + svt_clist(iclus0+isamp).q * x(1)
               xx(2)  = xx(2) + svt_clist(iclus0+isamp).q * x(2)
               xx2(1) = xx2(1) + svt_clist(iclus0+isamp).q * x(1)**2
               xx2(2) = xx2(2) + svt_clist(iclus0+isamp).q * x(2)**2
            end do

         end do

c     perform fit of current cluster

c     output cluster to structure

      end do

      return
      end

c     =======================

      integer function cluster(a,ncmax,nrmax,b,nsmax,nlmax,nl,high,low)
      implicit none

      integer  ncmax,nrmax,nsmax,nlmax
      integer  nc,nr,ns,nl
      real     a(ncmax,nrmax)
      integer  b(nsmax,nlmax)
      real     high, low
      logical  done, more 
      integer  cid, rid
      integer  r, c, r0, c0, r1, c1, rg, cg

c     a(nc,nr)   input matrix
c     b(l,k)   output list


      r0 = 0
      c0 = 0

      rg  = 0
      cg  = 0

      done = .false.

      do while (rg.lt.nrmax.and..not.done)
         rg = rg + 1
         cg = 0
         do while (cg.lt.ncmax)
            cg = cg + 1

            if (a(cg,rg).gt.high) then

c     this is a new cluster; increment list counter, reset sample counter 

               nl = nl + 1
               if (nl.gt.nlmax) then
                  write(6,*) ' number of clusters exceeded maximum'
                  goto 900
               end if

               ns = 0

               r0  = rg
               c0  = cg
               r1  = rg
               c1  = cg
               rid = 1
               cid = 0

               do while (rid.ne.0.and.r.le.nrmax)

c     record current sample

                  ns = ns + 1
                  b(ns,nl) = c1 + nrmax*(r1-1)

                  a(c1,r1) = 0.

                  if (cid.ge.0) then

c     get samples for increasing "c"

                     r   = r1
                     c   = c1

                     more = .true.
                     do while (c.lt.ncmax.and.more)
                        c = c + 1
                        if (a(c,r).gt.low) then

                           ns = ns + 1
                           if (ns.gt.nsmax) then
                              write(6,*) ' number of samples exceeded maximum'
                              goto 900
                           end if
                           b(ns,nl) = c + nrmax*(r-1)

                           a(c,r) = 0.
                           write(6,*) 'recorded ',c,r

                        else

                           more = .false.

                        end if
                     end do
                  end if
                  
                  if (cid.le.0) then

c     get samples for decreasing "c"

                     r   = r1
                     c   = c1

                     more = .true.
                     do while (c.lt.ncmax.and.more)
                        c = c - 1
                        if (a(c,r).gt.low) then

                           ns = ns + 1
                           if (ns.gt.nsmax) then
                              write(6,*) ' number of samples exceeded maximum'
                              goto 900
                           end if
                           b(ns,nl) = c + nrmax*(r-1)

                           a(c,r) = 0.

                           write(6,*) 'recorded ',c,r

                        else

                           more = .false.

                        end if
                     end do

                  end if

                  write(6,*) 'rid,cid,r1,c1,r,c:',rid,cid,r1,c1,r,c

                  if (rid.lt.0.and.r1.gt.1) then
                     r1  = r1 - 1
                     if (a(c1,r1).gt.low) then
                        cid = 0
                     else if (a(c1+1,r1).gt.low) then
                        cid = 1
                     else if (a(c1-1,r1).gt.low) then
                        cid = -1
                     else
                        rid = 0
                     end if
                  else if (rid.gt.0.and.r1.lt.nrmax) then
                     r1 = r1 + 1
                     if (a(c1,r1).gt.low) then
                        cid = 0
                     else if (a(c+1,r).gt.low) then
                        cid = 1
                     else if (a(c-1,r).gt.low) then
                        cid = -1
                     else
                        rid = -1
                        if (r0.gt.1) then
                           r1  = r0 - 1
                           c1  = c0
                           if (a(c1,r1).gt.low) then
                              cid = 0
                           else if (a(c1+1,r1).gt.low) then
                              cid = 1
                           else if (a(c1-1,r1).gt.low) then
                              cid = -1
                           else
                              rid = 0
                           end if
                        end if
                     end if
                  end if

               end do        ! in a cluster since rid.ne.0
            end if           ! in a cluster
         end do              ! loop on c
      end do                 ! loop on r
      
 900  continue
      
      cluster = -1
      return
      end 
      
