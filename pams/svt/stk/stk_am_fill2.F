      INTEGER*4 FUNCTION STK_AM_FILL2(
     1               spt_h,               spt ,
     2               vtx_h,               vtx ,
     3             track_h,             track ,
     4            filler_h,            filler ,
     5            config_h,            config ,
     6              geom_h,              geom ) 
      IMPLICIT NONE
C     SYNOPSIS:
C     fill track table with all relevant spacepoints combinations
C     
C     DESCRIPTION:
c     This module fills the table track with all possible 
c     combinations of svt space points falling into a cone - 
c     It has to be followed by a call to stk_am_fit 
c     to perform a layer to layer helix fit assuming a 3 layer design 
c     with the appropriate MCS. The space points will be used only if 
c     they  have not been assigned previously to a track by another module
c     (like the grouper).
C     
C     AUTHOR:
C     C. Pruneau,  Wayne State University
C
C     DATE:
C     1-march-1994,c.p. Modified to have the search driven on by the wafer#
C                       rather than the layer#
C     23-jan-1994,c.p.  Changed cone test to cos(th) test
C     29-nov-1993,c.p.  Added key to space point table instead of sorter
C     26-nov-1993,c.p.  Debugging, added table config as input, added 
c                       default values for filler
C     21-oct-1993,c.p.  Written, Integration into tas
C     Aug 20 1997 hlc   Adapted to Sl97a removed ctrack def.
c     
C     ARGUMENTS:
C     spt_h    header to table spt     svt space points
C     spt      rows of table   spt
C     vtx_h    header to table vtx     svt determined vertex
C     vtx      rows of table   vtx
C     track_h header to table track  svt candidate tracks
C     track   rows of table   track
c     filler_h header to table filler  svt fill parameters
c     filler   rows of table   filler  
c
c     argument declarations
C     ---------------------
#include "stk_am_fill2.inc"
      
C     Local Declarations:
C     ===================
      real       v01(3),v12(3),v23(3),v01m, v12m, v23m
      real       x0(3), x1(3), x2(3), x3(3)
      real       costh12, costh23
      real       cth12, cth23, cth
      logical    done1, done2, done3, first/.true./
      integer    iwafer1, iwafer2, iwafer3, iwafer, ilist, iladder
      integer    jwafer1, jwafer2, jwafer3
      integer    ispt, ispt1, ispt2, ispt3, ncone12, ncone23
      integer    jspt1, jspt2, jspt3
      integer    jspt(6)         ! index of current space point in layer j=1,6
      integer    nspt(6)        ! number of space point in layer j=1,6
      integer    key(2000,250)    ! key to space points table organized 
C per wafer
      integer    keyn(250)
      integer    waferkey1(100) 
      integer    waferkey1n, wafern
      integer    waferkey(50,300) ! key to wafers to be look at
      integer    waferkeyn(300),wafercnt(6)
      integer    itrack
      integer    ilayer, layer, layer1, layer2 
      integer    flag, id, id_cluster, id_wafer
      real       cov(3), de(2), x(3)
      real       list(2,300)
      save       first


C     RETURN:
C     =======
C     FORMAT:
C     =======
 1000 format(1x,'f-stk_am_fill: no rows in spt - '//
     &     'abort event.')
 1001 format(1x,'f-stk_am_fill: no rows in vtx -'//
     &     'abort event.')
c 1002 format(1x,'f-stk_am_fill: track already `//
c     defined - return.')
 1003 format(1x,'f-stk_am_fill: no rows in filler'//
     &     '- will use defaults parameters.')
 1004 format(1x,'f-stk_am_fill: no rows in config'//
     &     '- abort event.')
 1005 format(1x,'f-stk_am_fill: layer number out of'//
     &     'bounds at label:',i5)
 1006 format(1x,'f-stk_am_fill: no space points on `//
     &     `layer :',i5)
 1007 format(1x,'f-stk_am_fill: too many track'//
     &     'candidates: ',i8)

C     EXECUTABLE CODE:
C     ================

      WRITE(*,*) 'STK_AM_FILL2: Starting ############################'

      stk_am_fill2 = stafcv_ok

      first = .true.

      if (first) then
         first = .false.

         waferkey1n = 2.*config(1).n_ladder(1)*
     &        config(1).n_wafer(1)
         wafern     = 0
         do ilayer = 1, config(1).n_layer
            wafern = wafern+
     &           config(1).n_ladder(ilayer)*
     &           config(1).n_wafer(ilayer)
         end do

         do iwafer1 = 1, wafern ! build wafer lists
c            write(6,*) ' building list for iwafer1=',iwafer1
            waferkeyn(iwafer1) = 0
            v01(1) = geom(iwafer1).x(1)
            v01(2) = geom(iwafer1).x(2)
            v01(3) = geom(iwafer1).x(3)
            v01m   = sqrt(v01(1)**2+v01(2)**2+v01(3)**2)
            if (v01m.le.0) then
               write(6,*) ' stk_am_fill2: v01m=<0'
               stop
            end if
            if (geom(iwafer1).layer.lt.5) then
               ilist = 0
               do iwafer2 = 1, wafern
                  layer1 = geom(iwafer1).layer
                  layer2 = geom(iwafer2).layer
                  if (((layer2.eq.3.or.layer2.eq.4).and.
     &                 (layer1.eq.1.or.layer1.eq.2)).or.
     &                 ((layer2.eq.5.or.layer2.eq.6).and.
     &                 (layer1.eq.3.or.layer1.eq.4))) then

                     v12(1) = geom(iwafer2).x(1)-
     &                    geom(iwafer1).x(1)
                     v12(2) = geom(iwafer2).x(2)-
     &                    geom(iwafer1).x(2)
                     v12(3) = geom(iwafer2).x(3)-
     &                    geom(iwafer1).x(3)
                     v12m   = sqrt(v12(1)**2+v12(2)**2+
     &                    v12(3)**2)
                     if (v01m.le.0) then
c     write(6,*) ' stk_am_fill2:`//
c     &                       'v12m=<0'
                        stop
                     end if
                     cth = (v01(1)*v12(1)+v01(2)*v12(2)+
     &                    v01(3)*v12(3))/v01m/v12m
                     ilist = ilist + 1
                     list(1,ilist)   = iwafer2
                     list(2,ilist)   = cth
                  end if
               end do
               call sortrq(list,2,ilist,-2)
               iwafer2 = 0
               done1 = .false.
               do while (.not.done1)
                  iwafer2 = iwafer2 + 1
                  if (list(2,iwafer2).gt.cosd(60.).and.
     &                 iwafer2.le.ilist) then
                     waferkeyn(iwafer1) = iwafer2
                     waferkey(iwafer2,iwafer1) = list(1,iwafer2)
                     write(66,*) ' storing ',iwafer1,
     &                    iwafer2,list(1,iwafer2)
                  else
                     done1 = .true.
                  end if
               end do
            end if
         end do
      end if
         
      if (spt_h.nok.eq.0) then
         write(6,1000)
         stk_am_fill2 = stafcv_bad ! no svt space point
         return
      else if (vtx_h.nok.eq.0) then
         write(6,1001)
         stk_am_fill2 = stafcv_bad ! no svt vertex 
         return
      else if (config_h.nok.eq.0) then
         write(6,1004)
         return
      else if (filler_h.nok.eq.0) then 
         write(6,1003)
         filler(1).th12_step = 0.5       ! degrees assumed
         filler(1).th23_step = 0.5  
         filler(1).th12_min = 5.
         filler(1).th23_min = 5.  
      end if

c     write(6,*) 'nspt:',spt_h.nok
c     write(26,*) 'angles:',filler(1).th12_min,
c     &        filler(1).th23_min
      costh12 = cosd(filler(1).th12_min)
      costh23 = cosd(filler(1).th23_min)

c     reset counters
c     sort the space points by wafer and count them

      wafercnt(1)  = 0
      wafercnt(2)  = config(1).n_ladder(1)*
     &     config(1).n_wafer(1)
      wafercnt(3)  = wafercnt(2) +
     &     config(1).n_ladder(2)*config(1).n_wafer(2)
      wafercnt(4)  = wafercnt(3) +
     &     config(1).n_ladder(3)*config(1).n_wafer(3)
      wafercnt(5)  = wafercnt(4) + config(1).n_ladder(4)*
     &     config(1).n_wafer(4)
      wafercnt(6)  = wafercnt(5) + config(1).n_ladder(5)*
     &     config(1).n_wafer(5)

c      write(6,*) 'wafercnt:',wafercnt
      do iwafer = 1, 300       ! zero counters
         keyn(iwafer) = 0
      end do
      ispt1 = 0
      do ispt = 1, spt_h.nok   ! count
         iwafer       = spt(ispt).id_wafer
         ilayer       = iwafer/1000
         iwafer1      = (iwafer-ilayer*1000)/100
         iladder      =  iwafer-ilayer*1000-iwafer1*100
         iwafer2  = wafercnt(ilayer) + (iladder-1)*
     &        config(1).n_wafer(ilayer) + iwafer1
         if (iwafer.ne.geom(iwafer2).id) then
            write(6,*) ' fatal mis-match'
            write(6,*) 'idw,ly,w,ld,w2,id:', iwafer,ilayer,
     &           iwafer1,iladder,iwafer2,
     &           geom(iwafer2).id
            return
         end if

         keyn(iwafer2) = keyn(iwafer2) + 1
         key(keyn(iwafer2),iwafer2) = ispt
      end do ! spt = 1, spt_h.nok
c      write(77,*) 'wafeykey'
c      write(77,*) waferkey
c      write(77,*) 'keyn is next'
c      write(77,*) keyn
c      write(77,*) 'key is next'
c      write(77,*) key

c     wkw: start where we left off on the last loop
      itrack = track_h.nok
      done1   = .false.
      jwafer1 = 0
      do while (jwafer1.lt.waferkey1n.and..not.done1) 
         jwafer1 = jwafer1 + 1
c        iwafer1 = waferkey1(jwafer1)
         iwafer1   = jwafer1
         jspt1   = 0
         done1   = .false.
c     write(6,*) ' jwafer1,iwafer1,keyn(iwafer1):',
c     &        jwafer1,iwafer1,keyn(iwafer1)
         do while (jspt1.lt.keyn(iwafer1).and..not.done1)  
            jspt1   = jspt1 + 1
            ispt1   = key(jspt1,iwafer1)
c     write(6,*) ' jspt1,key(jspt1,iwafer1):',jspt1,key(jspt1,iwafer1)
c     wkw: Only track from this point if this point is free.
            if (spt(ispt).flag.eq.0) then
               x0(1)  = vtx(1).x(1)
               x0(2)  = vtx(1).x(2)
               x0(3)  = vtx(1).x(3)
               x1(1)  = spt(ispt1).x(1)
               x1(2)  = spt(ispt1).x(2)
               x1(3)  = spt(ispt1).x(3)
               v01(1) = x1(1) - x0(1)
               v01(2) = x1(2) - x0(2)
               v01(3) = x1(3) - x0(3)
               v01m   = sqrt(v01(1)**2+v01(2)**2+v01(3)**2)
               v01(1) = v01(1)/v01m
               v01(2) = v01(2)/v01m
               v01(3) = v01(3)/v01m
               jwafer2= 0
               done2  = .false.
               do while (jwafer2.lt.waferkeyn(iwafer1).and.
     &              .not.done2) 
                  jwafer2 = jwafer2 + 1
                  iwafer2 = waferkey(jwafer2,iwafer1)
                  jspt2   = 0
                  do while (jspt2.lt.keyn(iwafer2).and..not.done2) 
                     jspt2  = jspt2 + 1
                     ispt2  = key(jspt2,iwafer2)
c                     write(6,*) 'iwafer1,jwafer2,iwafer2,jspt2,ispt2',
c     &   	       iwafer1,jwafer2,iwafer2,jspt2,ispt2
                     x2(1)  = spt(ispt2).x(1)
                     x2(2)  = spt(ispt2).x(2)
                     x2(3)  = spt(ispt2).x(3)
                     v12(1) = x2(1) - x1(1)
                     v12(2) = x2(2) - x1(2)
                     v12(3) = x2(3) - x1(3)
                     v12m   = sqrt(v12(1)**2+v12(2)**2+
     &                    v12(3)**2)+0.00001
                     v12(1) = v12(1)/v12m
                     v12(2) = v12(2)/v12m
                     v12(3) = v12(3)/v12m
                     cth12 = v01(1)*v12(1)+v01(2)*v12(2)+
     &                    v01(3)*v12(3)
                     if ((cth12 .gt. costh12).and.
     &                    (spt(ispt2).flag.eq.0)) then 
                        if (waferkeyn(iwafer2).le.0) then 
c     write(6,*) ' bug: iwafer1,jwafer2,iwafer2:',
c     &iwafer1,jwafer2,iwafer2
c     write(6,*) ' layer2:',geom(iwafer2).layer
c     write(6,*) ' ladde2:',geom(iwafer2).ladder
c     write(6,*) ' id   2:',geom(iwafer2).id
                        end if ! (waferkeyn(iwafer2).le.0)
                        jwafer3= 0
                        done3  = .false.
                        do while (jwafer3.lt.
     &                       waferkeyn(iwafer2).and..not.done3) 
                           jwafer3 = jwafer3 + 1
                           iwafer3 = waferkey(jwafer3,iwafer2)
                           jspt3   = 0
c     write(6,*) '>>>jwafer3,iwafer3,keyn(iwafer3):',jwafer3,iwafer3,
c     keyn(iwafer3)
                           do while (jspt3.lt.keyn(iwafer3)
     &                          .and..not.done3) 
                              jspt3  = jspt3 + 1
                              ispt3  = key(jspt3,iwafer3)
c     write(6,*) 'iwafer3,jspt3,key(jspt3,iwafer3):',iwafer3,jspt3,ispt3
                              x3(1)  =  spt(ispt3).x(1)
                              x3(2)  =  spt(ispt3).x(2)
                              x3(3)  =  spt(ispt3).x(3)
                              v23(1) = x3(1) - x2(1)
                              v23(2) = x3(2) - x2(2)
                              v23(3) = x3(3) - x2(3)
                              v23m   = sqrt(v23(1)**2+v23(2)**2+
     &                             v23(3)**2)+0.00001
                              v23(1) = v23(1)/v23m
                              v23(2) = v23(2)/v23m
                              v23(3) = v23(3)/v23m
                              cth23  = v12(1)*v23(1)+
     &                             v12(2)*v23(2)+
     &                             v12(3)*v23(3)
c     wkw: Second test, flag.ne.0 if spt already assigned to a track
                   if ((cth23 .gt. costh23).and.
     &                             (spt(ispt3).flag.eq.0)) then 
                      itrack = itrack + 1
                      if (itrack.gt.track_h.maxlen) then 
                         write(6,1007) itrack
                         done1   = .true.
                         done2   = .true.
                         done3   = .true.
                         itrack = track_h.maxlen
                      else      ! (itrack.le.track_h.maxlen)
                         track(itrack).spt(1)  = ispt1 
                         track(itrack).spt(2)  = ispt2
                         track(itrack).spt(3)  = ispt3
                         track(itrack).nspt    = 3
                         track(itrack).id      = itrack
C                         spt(ispt1).id_track     = itrack
C                         spt(ispt2).id_track     = itrack
C                         spt(ispt3).id_track     = itrack
                         track(itrack).id_mctrack = 
     &                        spt(ispt1).id_mctrack
                      end if    ! spt storage ! (itrack.gt.ctra...
                   endif        ! ((cth23 .gt. costh23).and...
                enddo           ! (jspt3.lt.keyn(iwafer3).and..not.done3)
             end do             ! (jwafer3.lt.waferkeyn(iwafer2).and....
          endif                 ! ((cth12 .gt. costh12).and.(spt(ispt2)...
       enddo                    ! (jspt2.lt.keyn(iwafer2).and..not.done2)
      end do                    ! (jwafer2.lt.waferkeyn(iwafer1).and.....
      endif                     ! (spt(ispt).flag.eq.0)
      end do                    ! (jspt1.lt.keyn(iwafer1).and..not.done1)
      end do                   ! (jwafer1.lt.waferkey1n.and..not.done1)

c     record the number of candidate tracks
c     wkw: itrack here is the sum of the previous loop and the old loops

      track_h.nok       = itrack
      
      WRITE(*,*) 'STK_AM_FILL2: Stopping #############################'
      STK_AM_FILL2=STAFCV_OK
      RETURN
      END
