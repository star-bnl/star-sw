       INTEGER*4 FUNCTION STK_AM_DIRECT(
     1         g2t_track_h,         g2t_track ,
     2               spt_h,               spt ,
     3          mcgroups_h,          mcgroups ,
     4           mctrack_h,           mctrack ,
     5              geom_h,              geom ) 
      IMPLICIT NONE
C     SYNOPSIS:
C     fill ctrack table with all space points belonging to MC tracks
C     
C     DESCRIPTION:
c     This module fills the table track with svt space points 
C     belonging to MC tracks.
c     It has to be followed by a call to
c     stk_am_fit to perform
c     a layer to layer helix fit assuming a 3 layer design with the
c     appropriate MCS.
C     
C     A search is performed in 2 nested loops. It assumed that all tracks
C     have a hit on the first layer. The first loop is on first layer space
C     space points. The second loop is for 2nd and third layer space points.
C     
C     ARGUMENTS:
C     geom_h   header to table geom
C     geom     rows of table   geom
C     spt_h    header to table spt
C     spt      rows of table   spt
c     g2t_track_h      header to table g2t_track
c     g2t_track        rows of table   g2t_track
c     mcgroups_h header to table mcgroups
c     mcgroups  list of spt matched to individual groups  
C     mctrack_h header to table track
C     mctrack   rows of table   track
C
C     AUTHOR:
C     Claude Pruneau, Wayne State University, 313-577-1813                  
C     
C     CREATION DATE:
C     22-jul-1993,cap written
C     Copyright 1993 WSU
C     
C<-----------------------------------------------------------------------------
      
C     Argument Declarations
C     =====================
#include "stk_am_direct.inc"
      
C     Local Declarations: 
C     ===================

      integer    ispt           ! index to space points
      integer    imctrk, mctrk, n_groups
      integer    idtrack, jdtrack
      integer    sec, pid
c     keys

      integer    nkinemax, ig2t_track
      parameter (nkinemax=100000)
      integer   g2t_trackkey(nkinemax)
      integer    nmctrackmax
      parameter (nmctrackmax=100000)
      integer   mctrackkey(nmctrackmax)


      integer  tls_quick_sort_i
      integer  ok,i,nspt

c     error/message
      
      character msg*132
      integer   msg_id(30)/30*0./

C     Returned Values:
C     ================
C     Format:
C     =======
 1001 format(1x,'stk_am_direct_nrspt-e: no rows in spt.')
 1002 format(1x,'stk_am_direct_nrgeom-e: no rows in geom.')
 1004 format(1x,'stk_am_direct_nrmkin-e: no rows in g2t_track.')
 1013 format(1x,'stk_am_direct_g2t_trackoob-e : ',
     & ' g2t_track(ig2t_track).id out of boundaries:',2i6)
 1024 format(1x,'stk_am_direct_nmctrk-e : mctrk has no key',
     &     'for idtrack=',i10)
 1025 format(1x,'stk_am_direct_bmctrk-e : bad mctrk key =',
     &     2i10)
 1026 format(1x,'stk_am_direct_tmsot-e : too many space',
     &     'points on track',3i10)
 1027 format(1x,'stk_am_direct_tmst-e : too many SVT tracks')
 1028 format(1x,'stk_am_direct_snm_e :',
     & ' spt has not a valid id_mctrack, ispt=',i8)
 1029 format(1x,'stk_am_direct_snm_f : jdtrack out of bound',i8)

C     Executable Code:
C     ================
      WRITE(*,*) 'STK_AM_DIRECT: Starting ##########################'
      stk_am_direct = stafcv_ok
      
C     check if the table of space points is empty
      
      if (spt_h.nok.eq.0) then
         write(msg,1001) 
         call message(msg,1,msg_id(1))
         stk_am_direct = stafcv_bad ! no svt space point
         return
      else if (geom_h.nok.eq.0) then
         write(msg,1002) 
         call message(msg,1,msg_id(2))
         stk_am_direct = stafcv_bad ! no svt geometry
         return
      else if (g2t_track_h.nok.eq.0) then
         write(msg,1004)
         call message(msg,1,msg_id(4))
         stk_am_direct = stafcv_bad ! no svt vertex
         return
      end if

c     sort svt hits by wafer number

      ok = tls_quick_sort_i(spt_h.nok, spt(1).id_wafer,
     >     spt(2).id_wafer, spt)
c     reset/establish a kine key
     
      do ig2t_track = 1, nkinemax
         g2t_trackkey(ig2t_track) = 0
      end do
      do ig2t_track = 1, g2t_track_h.nok
         if (g2t_track(ig2t_track).id.gt.0.and.
     &        g2t_track(ig2t_track).id.lt.nkinemax) then
            g2t_trackkey( g2t_track(ig2t_track).id ) = ig2t_track
         else
            write(msg,1013) ig2t_track, g2t_track(ig2t_track).id
            call message(msg,1,msg_id(5))
         end if
      end do
      
c     reset the mctrack key

      do idtrack = 1, nmctrackmax
         mctrackkey(idtrack) = 0
      end do

c     scan spt table to find all g2t_track tracks
      
      idtrack = 0    ! track index for tracks just found
      ispt    = 0
      n_groups = mcgroups_h.nok + 1

c     loop on all space points to find current track

      do while (ispt.lt.spt_h.nok)
         ispt = ispt + 1
            
         if (spt(ispt).id_mctrack.lt.1) then
            write(msg,1028) ispt
            call message(msg,1,msg_id(8))
          else 

            mctrk = spt(ispt).id_mctrack ! monte carlo track index
            if (mctrackkey(mctrk).le.0) then
               if (idtrack.ge.mctrack_h.maxlen) then
                  write(msg,1027) 
                  call message(msg,1,msg_id(7))
                  mctrack_h.nok       = idtrack
                  stk_am_direct    = stafcv_bad
                  return
               else
                  idtrack                     = idtrack + 1
                  jdtrack                     = idtrack
                  mctrackkey(mctrk)           = idtrack
                  mctrack(jdtrack).id         = idtrack
                  mctrack(jdtrack).id_mctrack = mctrk
                  mctrack(jdtrack).nspt       = 0
                  if (mctrk.lt.1 .or. mctrk.gt.nkinemax) then
                     write(msg,1024) idtrack
                     call message(msg,1,msg_id(6))
                     sec = 0
                     pid = 0
                  else
                     imctrk = g2t_trackkey(mctrk)
                     if (imctrk.lt.1 .or. mctrk.gt.
     >                    g2t_track_h.nok) then
                        write(msg,1025) mctrk,imctrk
                        call message(msg,1,msg_id(9))
                        sec = 0
                        pid = 0
                     else
                        sec = g2t_track(imctrk).start_vertex_p
                        pid = g2t_track(imctrk).ge_pid
                     end if
                  end if
                  mctrack(jdtrack).sec  = sec   
C     sec=1 primary, sec=2 secondary, sec=0 unknown
                  mctrack(jdtrack).pid  = pid   ! geant particle id
               end if
            else
               jdtrack = mctrackkey(mctrk)
               if (jdtrack.gt.mctrack_h.maxlen) then
                  write(msg,1029) jdtrack
                  call message(msg,1,msg_id(10))
                  mctrack_h.nok       = idtrack
                  stk_am_direct    = stafcv_bad
                  return
               end if
            end if
            mctrack(jdtrack).nspt  = mctrack(jdtrack).nspt + 1

c           Temporarily fill ident with layer of space point
            mcgroups(n_groups).ident = spt(ispt).id_wafer/1000
            mcgroups(n_groups).id1 = jdtrack ! id of track
            mcgroups(n_groups).id2 = spt(ispt).id ! id of spt on track
            n_groups = n_groups + 1
         end if
      end do

c     update the mctrack table header

      mctrack_h.nok       = idtrack
      mcgroups_h.nok      = n_groups - 1
c     sort groups table by track id

      ok = tls_quick_sort_i(mcgroups_h.nok, mcgroups(1).id1,
     >         mcgroups(2).id1, mcgroups)

c     put spt table back to sorted by id
      
      ok = tls_quick_sort_i(spt_h.nok, spt(1).id, spt(2).id, spt)

c     now sort groups table by hit position within track
      

      i = 1
      do while ( i .le. mcgroups_h.nok)
         jdtrack = mcgroups(i).id1
         nspt = mctrack(jdtrack).nspt
         ok = tls_quick_sort_i(nspt, mcgroups(i).ident,
     >        mcgroups(i+1).ident,mcgroups(i))
         i = i+nspt
      enddo
      
      do i=1,mcgroups_h.nok
         mcgroups(i).ident = 1  ! groups id of a mctrack 
      enddo
      
      
      WRITE(*,*) 'STK_AM_DIRECT: Stopping ###########################'
      STK_AM_DIRECT=STAFCV_OK
      RETURN
      END










