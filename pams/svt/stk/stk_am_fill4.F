      INTEGER*4 FUNCTION STK_AM_FILL4(
     1               spt_h,               spt ,
     2               vtx_h,               vtx ,
     3            groups_h,            groups ,
     4             track_h,             track ,
     5            filler_h,            filler ,
     6            config_h,            config ,
     7              geom_h,              geom ) 
      IMPLICIT NONE
C     SYNOPSIS:
C     fill track table with all relevant spacepoints combinations
C     
C     DESCRIPTION:
c     This module fills the table track with all possible 
c     combinations of svt space points falling into a cone - 
c     It has to be followed by a call to tracking_fit 
c     to perform a layer to layer helix fit assuming a 3 layer design with the
c     appropriate MCS. The space points will be used only if they have 
C     not been 
c     assigned previously to a track by another module (like the grouper).
C     
C     AUTHOR:
C     C. Pruneau,  Wayne State University
C
C     DATE:
c
c     19-march-1995 wkw Modified to take 8 layers (4 super)
c
c     29-mai-1994,c.p.  Modified to have a full 6 layers search allowing
c                       an arbitrary and selectable hit pattern
C     1-march-1994,c.p. Modified to have the search driven on by the wafer#
C                       rather than the layer#
C     23-jan-1994,c.p.  Changed cone test to cos(th) test
C     29-nov-1993,c.p.  Added key to space point table instead of sorter
C     26-nov-1993,c.p.  Debugging, added table config as input, added 
c                       default values for filler
C     21-oct-1993,c.p.  Written, Integration into tas
C     Aug 20 1997 hlc   Adapted to SL97a, removed ctrack table
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
c     groups_h header for table groups
C     groups   rows of table groups give space point - track id links 
C
C     argument declarations
C     =====================
#include "stk_am_fill4.inc"

C     Local Declarations:
C     ===================
      real       x(3,0:8), v(3,8,0:8), vm(8,0:8), cth, cthij(8,0:8)
      integer    extra(8), ispts(0:8), ispt, jspt(0:8), iwafer(0:8)
      integer    jwafer(0:8), islayer_hits
      integer    iwafer1, iwafer2, n, kwafer
      integer    iladder, iladder1, iladder2
      integer    layer, layerp, layerpp, k, n_groups
      logical    layers(8), loop(8), done(8), first

      integer    waferkey(400,8,0:400), waferkeyn(8,0:400)
      integer    sptkey(200,400), sptkeyn(400)
      real       list(2,400)

      integer    waferi(0:8),wafern(0:8), waferntot
      integer    itrack
      integer    ilayer0, ilayer1, ilayer2, ilayer
      integer    flag, id
      integer    ilist
      save       first


C     RETURN:
C     =======
C     FORMAT:
C     =======
 1000 format(1x,'f-stk_am_fill: no rows in spt - abort event.')
 1001 format(1x,'f-stk_am_fill: no rows in vtx - abort event.')
 1003 format(1x,'f-stk_am_fill: no rows in filler -',
     &     'will use defaults parameters.')
 1004 format(1x,'f-stk_am_fill: no rows in config - abort event.')
 1005 format(1x,'f-stk_am_fill: layer number out of bounds at label:'
     &      ,i5)
 1006 format(1x,'f-stk_am_fill: no space points on layer :',i5)
 1007 format(1x,'f-stk_am_fill: too many track candidates: ',i8)

C     EXECUTABLE CODE:
C     ================

      WRITE(*,*) 'STK_AM_FILL4: Starting ###########################'

      stk_am_fill4 = stafcv_ok

c     wkw: don't do setup on subsequent loops:
c      if (track_h.nok.eq.0) then
         first = .true.
c      else
c         first = .false.
c      endif
c     Note: had to comment out the preceeding because pretreatment
c     with sgr fills track

      if (first) then

         do ilayer1 = 1, 8
            do ilayer2 = 1, 8
               cthij(ilayer2,ilayer1) = 0.
            end do
         end do

c     wkw: make cthij parameters
         cthij(2,1) = cosd(filler(1).wth21)
         cthij(3,1) = cosd(filler(1).wth31)
         cthij(4,1) = cosd(filler(1).wth41)
         cthij(5,1) = cosd(filler(1).wth51)
         cthij(6,1) = cosd(filler(1).wth61)
         cthij(7,1) = cosd(filler(1).wth71)
         cthij(8,1) = cosd(filler(1).wth81)
         cthij(3,2) = cosd(filler(1).wth32)
         cthij(4,2) = cosd(filler(1).wth42)
         cthij(5,2) = cosd(filler(1).wth52)
         cthij(6,2) = cosd(filler(1).wth62)
         cthij(7,2) = cosd(filler(1).wth72)
         cthij(8,2) = cosd(filler(1).wth82)
         cthij(4,3) = cosd(filler(1).wth43)
         cthij(5,3) = cosd(filler(1).wth53)
         cthij(6,3) = cosd(filler(1).wth63)
         cthij(7,3) = cosd(filler(1).wth73)
         cthij(8,3) = cosd(filler(1).wth83)
         cthij(5,4) = cosd(filler(1).wth54)
         cthij(6,4) = cosd(filler(1).wth64)
         cthij(7,4) = cosd(filler(1).wth74)
         cthij(8,4) = cosd(filler(1).wth84)
         cthij(6,5) = cosd(filler(1).wth65)
         cthij(7,5) = cosd(filler(1).wth75)
         cthij(8,5) = cosd(filler(1).wth85)
         cthij(7,6) = cosd(filler(1).wth76)
         cthij(8,6) = cosd(filler(1).wth86)
         cthij(8,7) = cosd(filler(1).wth87)
         waferntot     = 0
         waferi(0)     = 0
         wafern(0)     = 0
         do ilayer = 1, config(1).n_layer
            wafern(ilayer) = 
     &           config(1).n_ladder(ilayer)*
     &           config(1).n_wafer(ilayer)
            waferi(ilayer) = waferi(ilayer-1) + 
     &           wafern(ilayer)
            waferntot = waferntot + wafern(ilayer)
         end do

         do iwafer1 = 0, waferntot
            do ilayer2 = 1, 8
               waferkeyn(ilayer2,iwafer1) = 0
            end do
         end do

         do iwafer2 = 1, waferntot
            ilayer2 = geom(iwafer2).layer
            if (ilayer2.lt.1.or.ilayer2.gt.8) then
               write(6,*) 'ilayer2 error:',ilayer2
               stop
            end if
            waferkeyn(ilayer2,0) = waferkeyn(ilayer2,0) + 1
            n = waferkeyn(ilayer2,0)
            waferkey(n,ilayer2,0) = iwafer2
         end do

         ilayer0 = 0
         x(1,ilayer0) = 0.
         x(2,ilayer0) = 0.
         x(3,ilayer0) = 0.

         do iwafer1 = 1, waferntot
            ilayer1 = geom(iwafer1).layer
            x(1,ilayer1) = geom(iwafer1).x(1)
            x(2,ilayer1) = geom(iwafer1).x(2)
            x(3,ilayer1) = geom(iwafer1).x(3)
            v(1,ilayer1,ilayer0) = x(1,ilayer1) - x(1,ilayer0)
            v(2,ilayer1,ilayer0) = x(2,ilayer1) - x(2,ilayer0)
            v(3,ilayer1,ilayer0) = x(3,ilayer1) - x(3,ilayer0)
            vm(ilayer1,ilayer0) = 
     &           sqrt(v(1,ilayer1,ilayer0)**2+
     &           v(2,ilayer1,ilayer0)**2+
     &           v(3,ilayer1,ilayer0)**2)
            v(1,ilayer1,ilayer0) = v(1,ilayer1,ilayer0)/
     &           vm(ilayer1,ilayer0)
            v(2,ilayer1,ilayer0) = v(2,ilayer1,ilayer0)/
     &           vm(ilayer1,ilayer0)
            v(3,ilayer1,ilayer0) = v(3,ilayer1,ilayer0)/
     &           vm(ilayer1,ilayer0)
            
            do ilayer2 = ilayer1+1, 8
               ilist = 0
               do iwafer2 = waferi(ilayer2-1)+1, waferi(ilayer2)
                  if (ilayer2.ne.geom(iwafer2).layer) then
                     write(6,*) ' une erreur est survenu - crash...'
                     stop
                  end if
                  x(1,ilayer2) = geom(iwafer2).x(1)
                  x(2,ilayer2) = geom(iwafer2).x(2)
                  x(3,ilayer2) = geom(iwafer2).x(3)
                  v(1,ilayer2,ilayer1) = x(1,ilayer2) - x(1,ilayer1)
                  v(2,ilayer2,ilayer1) = x(2,ilayer2) - x(2,ilayer1)
                  v(3,ilayer2,ilayer1) = x(3,ilayer2) - x(3,ilayer1)
                  vm(ilayer2,ilayer1) = 
     &                 sqrt(v(1,ilayer2,ilayer1)**2+
     &                 v(2,ilayer2,ilayer1)**2+
     &                 v(3,ilayer2,ilayer1)**2)
                  v(1,ilayer2,ilayer1) = v(1,ilayer2,ilayer1)/
     &                 vm(ilayer2,ilayer1)
                  v(2,ilayer2,ilayer1) = v(2,ilayer2,ilayer1)/
     &                 vm(ilayer2,ilayer1)
                  v(3,ilayer2,ilayer1) = v(3,ilayer2,ilayer1)/
     &                 vm(ilayer2,ilayer1)
                  
                  cth = v(1,ilayer2,ilayer1)*v(1,ilayer1,ilayer0)+
     &                 v(2,ilayer2,ilayer1)*v(2,ilayer1,ilayer0)+
     &                 v(3,ilayer2,ilayer1)*v(3,ilayer1,ilayer0)
                  ilist = ilist + 1
                  list(1,ilist)   = iwafer2
                  list(2,ilist)   = cth

               end do
 
               call sortrq(list,2,ilist,-2)

               k = 0
               do while (k.lt.ilist)
                  k = k + 1
                  if (list(2,k).gt.cthij(ilayer2,ilayer1)) then
                     waferkeyn(ilayer2,iwafer1)  = k
                     waferkey(k,ilayer2,iwafer1) = list(1,k)
                  end if
               end do
            end do
         end do

c         do iwafer1 = 0, waferntot
c            do ilayer1 = 1, 8
c               write(884,*) 'iwafer, ilayer, n:',iwafer1,ilayer1,
c     &              waferkeyn(ilayer1,iwafer1) 
c            end do
c         end do

      end if  ! first call
         
      if (spt_h.nok.eq.0) then
         write(6,1000)
         stk_am_fill4 = stafcv_bad ! no svt space point
         return
      else if (vtx_h.nok.eq.0) then
         write(6,1001)
         stk_am_fill4 = stafcv_bad ! no svt vertex 
         return
      else if (config_h.nok.eq.0) then
         write(6,1004)
         return
      else if (filler_h.nok.eq.0) then 
      end if

c      write(6,*) 'nspt:',spt_h.nok

      cthij(2,1) = cosd(filler(1).th21)
      cthij(3,1) = cosd(filler(1).th31)
      cthij(4,1) = cosd(filler(1).th41)
      cthij(5,1) = cosd(filler(1).th51)
      cthij(6,1) = cosd(filler(1).th61)
      cthij(7,1) = cosd(filler(1).th71)
      cthij(8,1) = cosd(filler(1).th81)
      cthij(3,2) = cosd(filler(1).th32)
      cthij(4,2) = cosd(filler(1).th42)
      cthij(5,2) = cosd(filler(1).th52)
      cthij(6,2) = cosd(filler(1).th62)
      cthij(7,2) = cosd(filler(1).th72)
      cthij(8,2) = cosd(filler(1).th82)
      cthij(4,3) = cosd(filler(1).th43)
      cthij(5,3) = cosd(filler(1).th53)
      cthij(6,3) = cosd(filler(1).th63)
      cthij(7,3) = cosd(filler(1).th73)
      cthij(8,3) = cosd(filler(1).th83)
      cthij(5,4) = cosd(filler(1).th54)
      cthij(6,4) = cosd(filler(1).th64)
      cthij(7,4) = cosd(filler(1).th74)
      cthij(8,4) = cosd(filler(1).th84)
      cthij(6,5) = cosd(filler(1).th65)
      cthij(7,5) = cosd(filler(1).th75)
      cthij(8,5) = cosd(filler(1).th85)
      cthij(7,6) = cosd(filler(1).th76)
      cthij(8,6) = cosd(filler(1).th86)
      cthij(8,7) = cosd(filler(1).th87)

c     reset counters
c     sort the space points by wafer and count them

      waferi(1)  = 0
      waferi(2)  = config(1).n_ladder(1)*config(1).n_wafer(1)
      waferi(3)  = waferi(2) + config(1).n_ladder(2)*
     &     config(1).n_wafer(2)
      waferi(4)  = waferi(3) + config(1).n_ladder(3)*
     &     config(1).n_wafer(3)
      waferi(5)  = waferi(4) + config(1).n_ladder(4)*
     &     config(1).n_wafer(4)
      waferi(6)  = waferi(5) + config(1).n_ladder(5)*
     &     config(1).n_wafer(5)
      waferi(7)  = waferi(6) + config(1).n_ladder(6)*
     &     config(1).n_wafer(6)
      waferi(8)  = waferi(7) + config(1).n_ladder(7)*
     &     config(1).n_wafer(7)

      do iwafer1 = 1, 400       ! zero counters
        sptkeyn(iwafer1) = 0
      end do


      do ispt = 1, spt_h.nok   ! count
         kwafer       = spt(ispt).id_wafer
         ilayer       = kwafer/1000
         iwafer1      = (kwafer-ilayer*1000)/100
         iladder      =  kwafer-ilayer*1000-iwafer1*100
         iwafer2  = waferi(ilayer) + 
     &        (iladder-1)*config(1).n_wafer(ilayer) + iwafer1
         if (kwafer.ne.geom(iwafer2).id) then
            write(6,*) ' fatal mis-match'
            write(6,*) 'idw,ly,w,ld,w2,id:', 
     &           kwafer,ilayer,iwafer1,iladder,
     &           iwafer2, geom(iwafer2).id
            return
         end if

         sptkeyn(iwafer2) = sptkeyn(iwafer2) + 1
         sptkey(sptkeyn(iwafer2),iwafer2) = ispt

      end do ! spt = 1, spt_h.nok

c      do iwafer1 = 1, waferntot
c         write(886,*) 'wafer, nspt:',iwafer1,sptkeyn(iwafer1)
c      end do

c     wkw: start where we left off on the last loop
      itrack = track_h.nok
      n_groups = groups_h.nok + 1

c     main vertex 

      x(1,0)  = vtx(1).x(1)
      x(2,0)  = vtx(1).x(2)
      x(3,0)  = vtx(1).x(3)

      jwafer(0) = 0
      iwafer(0) = 0
c     -------------------------------------------------
c     layer 1
c     -------------------------------------------------
      jspt(1)   = 0
      jwafer(1) = 1
      iwafer(1) = waferkey(jwafer(1),1,0)
c      write(889,*) '1:jwafer,iwafer,nspt:',jwafer(1),iwafer(1),
c     &     sptkeyn(iwafer(1))
      done(1)   = .false.
      extra(1)  = 0

      do while (.not.done(1))
 1       continue

         if (extra(1).lt.1) then

            jspt(1) = jspt(1) + 1

c            write(889,*) 'jspt(1),jwafer(1),iwafer(1),n:',
c     &           jspt(1),jwafer(1),iwafer(1),sptkeyn(iwafer(1))

            if (jspt(1).gt.sptkeyn(iwafer(1))) then

               if (jwafer(1).lt.waferkeyn(1,0)) then
                  jwafer(1) = jwafer(1) + 1
                  iwafer(1) = waferkey(jwafer(1),1,0)
                  jspt(1)   = 0
                  goto 1
               else
                  extra(1) = extra(1) + 1
                  goto 1
               end if
               
            else 
               ispts(1) = sptkey(jspt(1),iwafer(1))
c               write(889,*) 'spt(1):',ispts(1)
               if (spt(ispts(1)).flag.eq.0) then
                  layer   = 1
                  layerp  = 0
                  layerpp = -1
                  x(1,layer)   =  spt(ispts(layer)).x(1)
                  x(2,layer)   =  spt(ispts(layer)).x(2)
                  x(3,layer)   =  spt(ispts(layer)).x(3)
                  v(1,layer,layerp) = x(1,layer) - x(1,layerp)
                  v(2,layer,layerp) = x(2,layer) - x(2,layerp)
                  v(3,layer,layerp) = x(3,layer) - x(3,layerp)
                  vm(layer,layerp)   = sqrt(v(1,layer,layerp)**2+
     &                 v(2,layer,layerp)**2+
     &                 v(3,layer,layerp)**2)
                  v(1,layer,layerp) = v(1,layer,layerp)/vm(layer,layerp)
                  v(2,layer,layerp) = v(2,layer,layerp)/vm(layer,layerp)
                  v(3,layer,layerp) = v(3,layer,layerp)/vm(layer,layerp)
                  layers(1) = .true.
                  loop(1)  = .true.
               else 
                  layers(1) = .false.
                  loop(1)  = .false.
               end if
            end if
         else if (extra(1).eq.1) then
c            write(889,*) 'extra(1).eq.1'
            done(1)   = .false.
            extra(1)  = 2
            loop(1)   = .true.
            layers(1) = .false.
            ispts(1)  = 0
         else
            done(1)   = .true.
            loop(1)   = .false.
            layers(1) = .false.
            ispts(1)  = 0
         end if
         
         if (loop(1)) then 
c     ------------------------------------------------ 
c     layer 2 
c     -------------------------------------------------
      jspt(2)   = 0
      jwafer(2) = 1
      done(2)   = .false.
      extra(2)  = 0


      do while (.not.done(2))
 2       continue
         
         layer = 2
         if (layers(1)) then
            layerp  = 1
            layerpp = 0
         else 
            layerp  =  0
            layerpp =  -1
         end if
         
         if (extra(2).lt.1) then
            
            jspt(2) = jspt(2) + 1
            iwafer(2) = waferkey(jwafer(2),2,iwafer(layerp))

c      write(889,*) '2:jwafer,iwafer,nspt:',jwafer(2),iwafer(2),
c     &     sptkeyn(iwafer(2))

            if (jspt(2).gt.sptkeyn(iwafer(2))) then
               
               if (jwafer(2).lt.waferkeyn(2,iwafer(layerp))) then
                  jwafer(2) = jwafer(2) + 1
                  iwafer(2) = waferkey(jwafer(2),2,iwafer(layerp))
                  jspt(2)   = 0
                  goto 2
               else
                  extra(2) = extra(2) + 1
                  goto 2
               end if
               
            else 
               ispts(2) = sptkey(jspt(2),iwafer(2))
               if (spt(ispts(2)).flag.eq.0) then
                  layer   =  2
                  if (layers(1)) then
                     layerp  = 1
                     layerpp = 0
                  else 
                     layerp  =  0
                     layerpp =  -1
                  end if
                  x(1,layer)   =  spt(ispts(layer)).x(1)
                  x(2,layer)   =  spt(ispts(layer)).x(2)
                  x(3,layer)   =  spt(ispts(layer)).x(3)
                  v(1,layer,layerp) = x(1,layer) - x(1,layerp)
                  v(2,layer,layerp) = x(2,layer) - x(2,layerp)
                  v(3,layer,layerp) = x(3,layer) - x(3,layerp)
                  vm(layer,layerp)   = sqrt(v(1,layer,layerp)**2+
     &                 v(2,layer,layerp)**2+
     &                 v(3,layer,layerp)**2)
                  v(1,layer,layerp) = v(1,layer,layerp)/vm(layer,layerp)
                  v(2,layer,layerp) = v(2,layer,layerp)/vm(layer,layerp)
                  v(3,layer,layerp) = v(3,layer,layerp)/vm(layer,layerp)
                  if (layerpp.lt.0) then
                     layers(2) = .true.
                     loop(2)   = .true.
                  else
                     cth= v(1,layer,layerp)*v(1,layerp,layerpp) +
     &                    v(2,layer,layerp)*v(2,layerp,layerpp) +
     &                    v(3,layer,layerp)*v(3,layerp,layerpp) 
                     if (cth .gt. cthij(layer,layerp).and.
     &                    spt(ispts(layer)).flag.eq.0) then 
                        layers(2) = .true.
                        loop(2)  = .true.
                     else
                        layers(2) = .false.
                        loop(2)  = .false.
                     end if
                  end if
               else 
                  layers(2) = .false.
                  loop(2)  = .false.
               end if
            end if
         else if (extra(2).eq.1) then
            done(2)  = .false.
            extra(2) = 2
            loop(2)  = .true.
            layers(2) = .false.
            ispts(2)  = 0
         else
            done(2)  = .true.
            loop(2)  = .false.
            layers(2) = .false.
            ispts(2)  = 0
         end if
         
         if (loop(2)) then
c     -------------------------------------------------
c     layer 3
c     -------------------------------------------------
      jspt(3)   = 0
      jwafer(3) = 1
      done(3)   = .false.
      extra(3)  = 0

      
      do while (.not.done(3))
 3       continue
         
         layer = 3
         if (layers(2)) then
            layerp = 2
            if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(1)) then
            layerp  = 1
            layerpp = 0
         else 
            layerp  =  0
            layerpp =  -1
         end if
         
c         write(889,*) 'layer, p, pp:',3,layerp, layerpp

        if (extra(3).lt.1) then
            
            jspt(3)   = jspt(3) + 1
            iwafer(3) = waferkey(jwafer(3),3,iwafer(layerp))

c            write(889,*) '3:jwafer,iwafer,nspt:',jwafer(3),iwafer(3),
c     &           sptkeyn(iwafer(3))

            if (jspt(3).gt.sptkeyn(iwafer(3))) then
               
               if (jwafer(3).lt.waferkeyn(3,iwafer(layerp))) then
                  jwafer(3) = jwafer(3) + 1
                  iwafer(3) = waferkey(jwafer(3),3,iwafer(layerp))
                  jspt(3)   = 0
                  goto 3
               else
                  extra(3) = extra(3) + 1
                  goto 3
               end if
               
            else 
               ispts(3) = sptkey(jspt(3),iwafer(3))
               if (spt(ispts(3)).flag.eq.0) then
                  layer   =  3
                  if (layers(2)) then
                     layerp = 2
                     if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(1)) then
                     layerp  = 1
                     layerpp = 0
                  else 
                     layerp  =  0
                     layerpp =  -1
                  end if
                  x(1,layer)   =  spt(ispts(layer)).x(1)
                  x(2,layer)   =  spt(ispts(layer)).x(2)
                  x(3,layer)   =  spt(ispts(layer)).x(3)
                  v(1,layer,layerp) = x(1,layer) - x(1,layerp)
                  v(2,layer,layerp) = x(2,layer) - x(2,layerp)
                  v(3,layer,layerp) = x(3,layer) - x(3,layerp)
                  vm(layer,layerp)   = sqrt(v(1,layer,layerp)**2+
     &                 v(2,layer,layerp)**2+
     &                 v(3,layer,layerp)**2)
                  v(1,layer,layerp) = v(1,layer,layerp)/vm(layer,layerp)
                  v(2,layer,layerp) = v(2,layer,layerp)/vm(layer,layerp)
                  v(3,layer,layerp) = v(3,layer,layerp)/vm(layer,layerp)
                  if (layerpp.lt.0) then
                     layers(3) = .true.
                     loop(3)   = .true.
                  else
                     cth= v(1,layer,layerp)*v(1,layerp,layerpp) +
     &                    v(2,layer,layerp)*v(2,layerp,layerpp) +
     &                    v(3,layer,layerp)*v(3,layerp,layerpp) 
                     if (cth .gt. cthij(layer,layerp).and.
     &                    spt(ispts(layer)).flag.eq.0) then 
                        layers(3) = .true.
                        loop(3)  = .true.
                     else
                        layers(3) = .false.
                        loop(3)  = .false.
                     end if
                  end if
               else 
                  layers(3) = .false.
                  loop(3)  = .false.
               end if
            end if
         else if (extra(3).eq.1) then
            done(3)  = .false.
            extra(3) = 2
            loop(3)  = .true.
            layers(3) = .false.
            ispts(3)  = 0
         else
            done(3)  = .true.
            loop(3)  = .false.
            layers(3) = .false.
            ispts(3)  = 0
         end if
         
         if (loop(3)) then
c     -------------------------------------------------
c     layer 4
c     -------------------------------------------------
      jspt(4)   = 0
      jwafer(4) = 1
      done(4)   = .false.
      extra(4)  = 0
      
      
      do while (.not.done(4))
 4       continue
                 
         if (layers(3)) then
            layerp = 3
            if (layers(2)) then
               layerpp = 2
            else if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(2)) then
            layerp = 2
            if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(1)) then
            layerp  = 1
            layerpp = 0
         else 
            layerp  =  0
            layerpp =  -1
         end if               
         
c         write(889,*) 'layer, p, pp:',4,layerp, layerpp

         if (extra(4).lt.1) then
            
            jspt(4) = jspt(4) + 1
            iwafer(4) = waferkey(jwafer(4),4,iwafer(layerp))

c            write(889,*) '4:jwafer,iwafer,nspt:',jwafer(4),iwafer(4),
c     &     sptkeyn(iwafer(4))

            if (jspt(4).gt.sptkeyn(iwafer(4))) then
               
               if (jwafer(4).lt.waferkeyn(4,iwafer(layerp))) then
                  jwafer(4) = jwafer(4) + 1
                  iwafer(4) = waferkey(jwafer(4),4,iwafer(layerp))
                  jspt(4)   = 0
                  goto 4
               else
                  extra(4) = extra(4) + 1
                  goto 4
               end if
               
            else 
               ispts(4) = sptkey(jspt(4),iwafer(4))
               if (spt(ispts(4)).flag.eq.0) then
                  layer   =  4
                  if (layers(3)) then
                     layerp = 3
                     if (layers(2)) then
                        layerpp = 2
                     else if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(2)) then
                     layerp = 2
                     if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(1)) then
                     layerp  = 1
                     layerpp = 0
                  else 
                     layerp  =  0
                     layerpp =  -1
                  end if               

                  x(1,layer)   =  spt(ispts(layer)).x(1)
                  x(2,layer)   =  spt(ispts(layer)).x(2)
                  x(3,layer)   =  spt(ispts(layer)).x(3)
                  v(1,layer,layerp) = x(1,layer) - x(1,layerp)
                  v(2,layer,layerp) = x(2,layer) - x(2,layerp)
                  v(3,layer,layerp) = x(3,layer) - x(3,layerp)
                  vm(layer,layerp)  = sqrt(v(1,layer,layerp)**2+
     &                 v(2,layer,layerp)**2+
     &                 v(3,layer,layerp)**2)
                  v(1,layer,layerp) = v(1,layer,layerp)/vm(layer,layerp)
                  v(2,layer,layerp) = v(2,layer,layerp)/vm(layer,layerp)
                  v(3,layer,layerp) = v(3,layer,layerp)/vm(layer,layerp)
                  if (layerpp.lt.0) then
                     layers(4) = .true.
                     loop(4)   = .true.
                  else
                     cth= v(1,layer,layerp)*v(1,layerp,layerpp) +
     &                    v(2,layer,layerp)*v(2,layerp,layerpp) +
     &                    v(3,layer,layerp)*v(3,layerp,layerpp) 
                     if (cth .gt. cthij(layer,layerp).and.
     &                    spt(ispts(layer)).flag.eq.0) then 
                        layers(4) = .true.
                        loop(4)  = .true.
                     else
                        layers(4) = .false.
                        loop(4)  = .false.
                     end if
                  end if
               else
                  layers(4) = .false.
                  loop(4)  = .false.
               end if
            end if
         else if (extra(4).eq.1) then
            done(4)  = .false.
            extra(4) = 2
            loop(4)  = .true.
            layers(4) = .false.
            ispts(4)  = 0
         else
            done(4)  = .true.
            loop(4)  = .false.
            layers(4) = .false.
            ispts(4)  = 0
         end if
         
         if (loop(4)) then
c     -------------------------------------------------
c     layer 5
c     -------------------------------------------------
      jspt(5)   = 0
      jwafer(5) = 1
      done(5)   = .false.
      extra(5)  = 0
 

      do while (.not.done(5))
 5       continue
         
         layer = 5
         if (layers(4)) then
            layerp = 4
            if (layers(3)) then
               layerpp = 3
            else if (layers(2)) then
               layerpp = 2
            else if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(3)) then
            layerp = 3
            if (layers(2)) then
               layerpp = 2
            else if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(2)) then
            layerp = 2
            if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(1)) then
            layerp  = 1
            layerpp = 0
         else 
            layerp  =  0
            layerpp =  -1
         end if
         
c         write(889,*) 'layer, p, pp:',5,layerp, layerpp

         if (extra(5).lt.1) then
            
            jspt(5) = jspt(5) + 1
            iwafer(5) = waferkey(jwafer(5),5,iwafer(layerp))

c            write(889,*) '5:jwafer,iwafer,nspt:',jwafer(5),iwafer(5),
c     &           sptkeyn(iwafer(5))
      
            if (jspt(5).gt.sptkeyn(iwafer(5))) then
               
               if (jwafer(5).lt.waferkeyn(5,iwafer(layerp))) then
                  jwafer(5) = jwafer(5) + 1
                  iwafer(5) = waferkey(jwafer(5),5,iwafer(layerp))
                  jspt(5)   = 0
                  goto 5
               else
                  extra(5) = extra(5) + 1
                  goto 5
               end if
               
            else 
               ispts(5) = sptkey(jspt(5),iwafer(5))
               if (spt(ispts(5)).flag.eq.0) then
                  ilayer = 5
                  if (layers(4)) then
                     layerp = 4
                     if (layers(3)) then
                        layerpp = 3
                     else if (layers(2)) then
                        layerpp = 2
                     else if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(3)) then
                     layerp = 3
                     if (layers(2)) then
                        layerpp = 2
                     else if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(2)) then
                     layerp = 2
                     if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(1)) then
                     layerp  = 1
                     layerpp = 0
                  else 
                     layerp  =  0
                     layerpp =  -1
                  end if

                  x(1,layer)   =  spt(ispts(layer)).x(1)
                  x(2,layer)   =  spt(ispts(layer)).x(2)
                  x(3,layer)   =  spt(ispts(layer)).x(3)
                  v(1,layer,layerp) = x(1,layer) - x(1,layerp)
                  v(2,layer,layerp) = x(2,layer) - x(2,layerp)
                  v(3,layer,layerp) = x(3,layer) - x(3,layerp)
                  vm(layer,layerp)   = sqrt(v(1,layer,layerp)**2+
     &                 v(2,layer,layerp)**2+
     &                 v(3,layer,layerp)**2)
                  v(1,layer,layerp) = v(1,layer,layerp)/vm(layer,layerp)
                  v(2,layer,layerp) = v(2,layer,layerp)/vm(layer,layerp)
                  v(3,layer,layerp) = v(3,layer,layerp)/vm(layer,layerp)
                  if (layerpp.lt.0) then
                     layers(5) = .true.
                     loop(5)   = .true.
                  else
                     cth= v(1,layer,layerp)*v(1,layerp,layerpp) +
     &                    v(2,layer,layerp)*v(2,layerp,layerpp) +
     &                    v(3,layer,layerp)*v(3,layerp,layerpp) 
                     if (cth .gt. cthij(layer,layerp).and.
     &                    spt(ispts(layer)).flag.eq.0) then 
                        layers(5) = .true.
                        loop(5)  = .true.
                     else
                        layers(5) = .false.
                        loop(5)  = .false.
                     end if
                  end if
               else 
                  layers(5) = .false.
                  loop(5)  = .false.
               end if
            end if
         else if (extra(5).eq.1) then
            done(5)  = .false.
            extra(5) = 2
            loop(5)  = .true.
            layers(5) = .false.
            ispts(5)  = 0
         else
            done(5)  = .true.
            loop(5)  = .false.
            layers(5) = .false.
            ispts(5)  = 0
         end if
         
         if (loop(5)) then         
c     -------------------------------------------------
c     layer 6
c     -------------------------------------------------
      jspt(6)   = 0
      jwafer(6) = 1
      done(6)   = .false.
      extra(6)  = 0


      do while (.not.done(6))
 6       continue
         
         layer   =  6
         if (layers(5)) then
            layerp = 5
            if (layers(4)) then
               layerpp = 4
            else if (layers(3)) then
               layerpp = 3
            else if (layers(2)) then
               layerpp = 2
            else if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(4)) then
            layerp = 4
            if (layers(3)) then
               layerpp = 3
            else if (layers(2)) then
               layerpp = 2
            else if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(3)) then
            layerp = 3
            if (layers(2)) then
               layerpp = 2
            else if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(2)) then
            layerp = 2
            if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(1)) then
            layerp  = 1
            layerpp = 0
         else 
            layerp  =  0
            layerpp =  -1
         end if
      
c         write(889,*) 'layer, p, pp:',6,layerp, layerpp

         if (extra(6).lt.1) then

            jspt(6) = jspt(6) + 1
            iwafer(6) = waferkey(jwafer(6),6,iwafer(layerp))

c            write(889,*) 'jspt(6),jwafer(6),iwafer(6),n:',
c     &           jspt(6),jwafer(6),iwafer(6),sptkeyn(iwafer(6))

            if (jspt(6).gt.sptkeyn(iwafer(6))) then
               
               if (jwafer(6).lt.waferkeyn(6,iwafer(layerp))) then
                  jwafer(6) = jwafer(6) + 1
                  iwafer(6) = waferkey(jwafer(6),6,iwafer(layerp))
                  jspt(6)   = 0
                  ispts(6)  = 0
                  goto 6
               else
                  extra(6)  = extra(6) + 1
                  ispts(6)  = 0
                  goto 6
               end if
               
            else 
               ispts(6) = sptkey(jspt(6),iwafer(6))
               if (spt(ispts(6)).flag.eq.0) then
                  layer   =  6
                  if (layers(5)) then
                     layerp = 5
                     if (layers(4)) then
                        layerpp = 4
                     else if (layers(3)) then
                        layerpp = 3
                     else if (layers(2)) then
                        layerpp = 2
                     else if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(4)) then
                     layerp = 4
                     if (layers(3)) then
                        layerpp = 3
                     else if (layers(2)) then
                        layerpp = 2
                     else if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(3)) then
                     layerp = 3
                     if (layers(2)) then
                        layerpp = 2
                     else if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(2)) then
                     layerp = 2
                     if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(1)) then
                     layerp  = 1
                     layerpp = 0
                  else 
                     layerp  =  0
                     layerpp =  -1
                  end if
                  
                  x(1,layer)   =  spt(ispts(layer)).x(1)
                  x(2,layer)   =  spt(ispts(layer)).x(2)
                  x(3,layer)   =  spt(ispts(layer)).x(3)
                  v(1,layer,layerp) = x(1,layer) - x(1,layerp)
                  v(2,layer,layerp) = x(2,layer) - x(2,layerp)
                  v(3,layer,layerp) = x(3,layer) - x(3,layerp)
                  vm(layer,layerp)   = sqrt(v(1,layer,layerp)**2+
     &                 v(2,layer,layerp)**2+
     &                 v(3,layer,layerp)**2)
                  v(1,layer,layerp) = v(1,layer,layerp)/vm(layer,layerp)
                  v(2,layer,layerp) = v(2,layer,layerp)/vm(layer,layerp)
                  v(3,layer,layerp) = v(3,layer,layerp)/vm(layer,layerp)
                  if (layerpp.lt.0) then
                     layers(6) = .true.
                     loop(6)   = .true.
                  else
                     cth= v(1,layer,layerp)*v(1,layerp,layerpp) +
     &                    v(2,layer,layerp)*v(2,layerp,layerpp) +
     &                    v(3,layer,layerp)*v(3,layerp,layerpp) 
                     if (cth .gt. cthij(layer,layerp).and.
     &                    spt(ispts(layer)).flag.eq.0) then 
                        layers(6) = .true.
                        loop(6)   = .true.
                     else
                        layers(6) = .false.
                        loop(6)  = .false.
                     end if
                  end if
               else
                  layers(6) = .false.
                  loop(6)  = .false.
               end if
            end if
         else if (extra(6).eq.1) then

c            write(889,*) 'extra(1).eq.6'
            
            done(6)   = .false.
            extra(6)  = 2
            loop(6)   = .true.
            layers(6) = .false.
            ispts(6)  = 0
         else
            done(6)   = .true.
            loop(6)   = .false.
            layers(6) = .false.
            ispts(6)  = 0
         end if
         
         if (loop(6)) then
c     -------------------------------------------------
c     layer 7
c     -------------------------------------------------
      jspt(7)   = 0
      jwafer(7) = 1
      done(7)   = .false.
      extra(7)  = 0
 

      do while (.not.done(7))
 7       continue
         layer   =  7
         if (layers(6)) then
            layerp = 6
            if (layers(5)) then
               layerpp = 5
            else if (layers(4)) then
               layerpp = 4
            else if (layers(3)) then
               layerpp = 3
            else if (layers(2)) then
               layerpp = 2
            else if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(5)) then
            layerp = 5
            if (layers(4)) then
               layerpp = 4
            else if (layers(3)) then
               layerpp = 3
            else if (layers(2)) then
               layerpp = 2
            else if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(4)) then
            layerp = 4
            if (layers(3)) then
               layerpp = 3
            else if (layers(2)) then
               layerpp = 2
            else if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(3)) then
            layerp = 3
            if (layers(2)) then
               layerpp = 2
            else if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(2)) then
            layerp = 2
            if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(1)) then
            layerp  = 1
            layerpp = 0
         else 
            layerp  =  0
            layerpp =  -1
         end if
      
         
c         write(889,*) 'layer, p, pp:',7,layerp, layerpp

         if (extra(7).lt.1) then
            
            jspt(7) = jspt(7) + 1
            iwafer(7) = waferkey(jwafer(7),7,iwafer(layerp))

c            write(889,*) '7:jwafer,iwafer,nspt:',jwafer(7),iwafer(7),
c     &           sptkeyn(iwafer(7))
      
            if (jspt(7).gt.sptkeyn(iwafer(7))) then
               
               if (jwafer(7).lt.waferkeyn(7,iwafer(layerp))) then
                  jwafer(7) = jwafer(7) + 1
                  iwafer(7) = waferkey(jwafer(7),7,iwafer(layerp))
                  jspt(7)   = 0
                  goto 7
               else
                  extra(7) = extra(7) + 1
                  goto 7
               end if
               
            else 
               ispts(7) = sptkey(jspt(7),iwafer(7))
               if (spt(ispts(7)).flag.eq.0) then
                  layer   =  7
                  if (layers(6)) then
                     layerp = 6
                     if (layers(5)) then
                        layerpp=5
                     else if (layers(4)) then
                        layerpp=4
                     else if (layers(3)) then
                        layerpp=3
                     else if (layers(2)) then
                        layerpp=2
                     else if (layers(1)) then
                        layerpp=1
                     else 
                        layerpp=0
                     end if
                  else if (layers(5)) then
                     layerp = 5
                     if (layers(4)) then
                        layerpp = 4
                     else if (layers(3)) then
                        layerpp = 3
                     else if (layers(2)) then
                        layerpp = 2
                     else if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(4)) then
                     layerp = 4
                     if (layers(3)) then
                        layerpp = 3
                     else if (layers(2)) then
                        layerpp = 2
                     else if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(3)) then
                     layerp = 3
                     if (layers(2)) then
                        layerpp = 2
                     else if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(2)) then
                     layerp = 2
                     if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(1)) then
                     layerp  = 1
                     layerpp = 0
                  else 
                     layerp  =  0
                     layerpp =  -1
                  end if
                  
                  x(1,layer)   =  spt(ispts(layer)).x(1)
                  x(2,layer)   =  spt(ispts(layer)).x(2)
                  x(3,layer)   =  spt(ispts(layer)).x(3)
                  v(1,layer,layerp) = x(1,layer) - x(1,layerp)
                  v(2,layer,layerp) = x(2,layer) - x(2,layerp)
                  v(3,layer,layerp) = x(3,layer) - x(3,layerp)
                  vm(layer,layerp)   = sqrt(v(1,layer,layerp)**2+
     &                 v(2,layer,layerp)**2+
     &                 v(3,layer,layerp)**2)
                  v(1,layer,layerp) = v(1,layer,layerp)/vm(layer,layerp)
                  v(2,layer,layerp) = v(2,layer,layerp)/vm(layer,layerp)
                  v(3,layer,layerp) = v(3,layer,layerp)/vm(layer,layerp)
                  if (layerpp.lt.0) then
                     layers(7) = .true.
                     loop(7)   = .true.
                  else
                     cth= v(1,layer,layerp)*v(1,layerp,layerpp) +
     &                    v(2,layer,layerp)*v(2,layerp,layerpp) +
     &                    v(3,layer,layerp)*v(3,layerp,layerpp) 
                     if (cth .gt. cthij(layer,layerp).and.
     &                    spt(ispts(layer)).flag.eq.0) then 
                        layers(7) = .true.
                        loop(7)  = .true.
                     else
                        layers(7) = .false.
                        loop(7)  = .false.
                     end if
                  end if
               else 
                  layers(7) = .false.
                  loop(7)  = .false.
               end if
            end if
         else if (extra(7).eq.1) then
            done(7)  = .false.
            extra(7) = 2
            loop(7)  = .true.
            layers(7) = .false.
            ispts(7)  = 0
         else
            done(7)  = .true.
            loop(7)  = .false.
            layers(7) = .false.
            ispts(7)  = 0
         end if

         if (loop(7)) then
c     -------------------------------------------------
c     layer 8
c     -------------------------------------------------
      jspt(8)   = 0
      jwafer(8) = 1
      done(8)   = .false.
      extra(8)  = 0
 

      do while (.not.done(8))
 8     continue
         layer   =  8
         if (layers(7)) then
            layerp = 7
            if (layers(6)) then
               layerpp = 6
            else if (layers(5)) then
               layerpp = 5
            else if (layers(4)) then
               layerpp = 4
            else if (layers(3)) then
               layerpp = 3
            else if (layers(2)) then
               layerpp = 2
            else if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(6)) then
            layerp = 6
            if (layers(5)) then
               layerpp = 5
            else if (layers(4)) then
               layerpp = 4
            else if (layers(3)) then
               layerpp = 3
            else if (layers(2)) then
               layerpp = 2
            else if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(5)) then
            layerp = 5
            if (layers(4)) then
               layerpp = 4
            else if (layers(3)) then
               layerpp = 3
            else if (layers(2)) then
               layerpp = 2
            else if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(4)) then
            layerp = 4
            if (layers(3)) then
               layerpp = 3
            else if (layers(2)) then
               layerpp = 2
            else if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(3)) then
            layerp = 3
            if (layers(2)) then
               layerpp = 2
            else if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(2)) then
            layerp = 2
            if (layers(1)) then
               layerpp = 1
            else
               layerpp = 0
            end if
         else if (layers(1)) then
            layerp  = 1
            layerpp = 0
         else 
            layerp  =  0
            layerpp =  -1
         end if
      
         
c         write(889,*) 'layer, p, pp:',8,layerp, layerpp

         if (extra(8).lt.1) then
            
            jspt(8) = jspt(8) + 1
            iwafer(8) = waferkey(jwafer(8),8,iwafer(layerp))

c            write(889,*) '8:jwafer,iwafer,nspt:',jwafer(8),iwafer(8),
c     &           sptkeyn(iwafer(8))
      
            if (jspt(8).gt.sptkeyn(iwafer(8))) then
               
               if (jwafer(8).lt.waferkeyn(8,iwafer(layerp))) then
                  jwafer(8) = jwafer(8) + 1
                  iwafer(8) = waferkey(jwafer(8),8,iwafer(layerp))
                  jspt(8)   = 0
                  goto 8
               else
                  extra(8) = extra(8) + 1
                  goto 8
               end if
               
            else 
               ispts(8) = sptkey(jspt(8),iwafer(8))
               if (spt(ispts(8)).flag.eq.0) then
                  layer   =  8
                  if (layers(7)) then
                     layerp = 7
                     if (layers(6)) then
                        layerpp=6
                     else if (layers(5)) then
                        layerpp=5
                     else if (layers(4)) then
                        layerpp=4
                     else if (layers(3)) then
                        layerpp=3
                     else if (layers(2)) then
                        layerpp=2
                     else if (layers(1)) then
                        layerpp=1
                     else 
                        layerpp=0
                     end if
                  else if (layers(6)) then
                     layerp = 6
                     if (layers(5)) then
                        layerpp=5
                     else if (layers(4)) then
                        layerpp=4
                     else if (layers(3)) then
                        layerpp=3
                     else if (layers(2)) then
                        layerpp=2
                     else if (layers(1)) then
                        layerpp=1
                     else 
                        layerpp=0
                     end if
                  else if (layers(5)) then
                     layerp = 5
                     if (layers(4)) then
                        layerpp = 4
                     else if (layers(3)) then
                        layerpp = 3
                     else if (layers(2)) then
                        layerpp = 2
                     else if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(4)) then
                     layerp = 4
                     if (layers(3)) then
                        layerpp = 3
                     else if (layers(2)) then
                        layerpp = 2
                     else if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(3)) then
                     layerp = 3
                     if (layers(2)) then
                        layerpp = 2
                     else if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(2)) then
                     layerp = 2
                     if (layers(1)) then
                        layerpp = 1
                     else
                        layerpp = 0
                     end if
                  else if (layers(1)) then
                     layerp  = 1
                     layerpp = 0
                  else 
                     layerp  =  0
                     layerpp =  -1
                  end if
                  
                  x(1,layer)   =  spt(ispts(layer)).x(1)
                  x(2,layer)   =  spt(ispts(layer)).x(2)
                  x(3,layer)   =  spt(ispts(layer)).x(3)
                  v(1,layer,layerp) = x(1,layer) - x(1,layerp)
                  v(2,layer,layerp) = x(2,layer) - x(2,layerp)
                  v(3,layer,layerp) = x(3,layer) - x(3,layerp)
                  vm(layer,layerp)   = sqrt(v(1,layer,layerp)**2+
     &                 v(2,layer,layerp)**2+
     &                 v(3,layer,layerp)**2)
                  v(1,layer,layerp) = v(1,layer,layerp)/vm(layer,layerp)
                  v(2,layer,layerp) = v(2,layer,layerp)/vm(layer,layerp)
                  v(3,layer,layerp) = v(3,layer,layerp)/vm(layer,layerp)
                  if (layerpp.lt.0) then
                     layers(8) = .true.
                     loop(8)   = .true.
                  else
                     cth= v(1,layer,layerp)*v(1,layerp,layerpp) +
     &                    v(2,layer,layerp)*v(2,layerp,layerpp) +
     &                    v(3,layer,layerp)*v(3,layerp,layerpp) 
                     if (cth .gt. cthij(layer,layerp).and.
     &                    spt(ispts(layer)).flag.eq.0) then 
                        layers(8) = .true.
                        loop(8)  = .true.
                     else
                        layers(8) = .false.
                        loop(8)  = .false.
                     end if
                  end if
               else 
                  layers(8) = .false.
                  loop(8)  = .false.
               end if
            end if
         else if (extra(8).eq.1) then
            done(8)  = .false.
            extra(8) = 2
            loop(8)  = .true.
            layers(8) = .false.
            ispts(8)  = 0
         else
            done(8)  = .true.
            loop(8)  = .false.
            layers(8) = .false.
            ispts(8)  = 0
         end if
         
         
         if (loop(8)) then
c     -----------------------------------------
c     consider track candidate for storage
c            write(889,*) ' considering track for storage:',itrack+1

         if (itrack.ge.track_h.maxlen) then 
            write(6,1007) itrack
            done(1)   = .true.
            done(2)   = .true.
            done(3)   = .true.
            done(4)   = .true.
            done(5)   = .true.
            done(6)   = .true.
            done(7)   = .true.
            done(8)   = .true.
            itrack   = track_h.maxlen
         else                   ! (itrack.le.track_h.maxlen)
            ispt = 0
            do k = 1, 8
               if (ispts(k).gt.0) ispt = ispt + 1
            end do

            islayer_hits=0 ! wkw section
            if (ispts(1).gt.0.or.ispts(2).gt.0) then ! hit in slayer 1
               islayer_hits=islayer_hits+1
            endif
            if (ispts(3).gt.0.or.ispts(4).gt.0) then ! hit in slayer 2
               islayer_hits=islayer_hits+1
            endif
            if (ispts(5).gt.0.or.ispts(6).gt.0) then ! hit in slayer 3
               islayer_hits=islayer_hits+1
            endif
            if (ispts(7).gt.0.or.ispts(8).gt.0) then ! hit in slayer 4
               islayer_hits=islayer_hits+1
            endif

            if ((islayer_hits.ge.3)) then ! least 3 super layer hits
c            if ((islayer_hits.ge.4)) then ! least 3 super layer hits
               itrack = itrack + 1
               track(itrack).nspt       = ispt
               track(itrack).id         = itrack
               ispt = 0
               do k = 1, 6
                  if (ispts(k).gt.0) then
                     ispt = ispt + 1
C     spt(ispts(k)).id_track     = itrack
                     groups(n_groups).ident = 2
                     groups(n_groups).id1 = itrack
                     groups(n_groups).id2 = ispts(k)
                     n_groups = n_groups + 1
                     if (ispt.eq.1) then
C                        track(itrack).id_mctrack = 
C     &                       spt(ispts(k)).id_mctrack
                     end if
                  end if
               end do
            end if
         end if 
         
      end if                    ! 8
      end do

      end if                    ! 7
      end do

      end if                    ! 6
      end do
      
      end if                    ! 5
      end do
      
      end if                    ! 4
      end do
      
      end if                    ! 3
      end do
      
      end if                    ! 2
      end do
      
      end if                    ! 1
      end do
      
      
c     record the number of candidate tracks
c     wkw: itrack here is the sum of the previous loop and the old loops
      
      track_h.nok       = itrack
      groups_h.nok      = n_groups -1
      
      WRITE(*,*) 'STK_AM_FILL4: Stopping ############################'
      STK_AM_FILL4=STAFCV_OK
      RETURN
      END
