C============================================================================
      INTEGER*4 FUNCTION sgr_am( v_h, v,
     >                        svt_geom_h, svt_geom,
     >			      svt_hits_h, svt_hits,
     >                        groups_h, groups,
     >			      pix_info_h, pix_info,
     >                        candidate_groups_h, candidate_groups,
     >                        stk_track_h, stk_track)
      implicit none
C
#include "sgr_am.inc"

C PURPOSE OF FUNCTiON: Make SVT groups that are topologically consistent
C                      with tracks.
C
C INPUT
C                      No. of passes required set by pix_info_h.nok
C                      Main Vertex position.
C                      SVT hits
C                      SVT Geom
C OUTPUT
C                      SVT tracks
C                      groups table.
C INTERNAL 
C                     candidate_groups
C                     pix_info
C
C    11-4-93 Modify for super-layers.
C            Assume layers in svt_hits are numbered 1 to 6.
C            Within groups we number layers from 1 to 3.
C            groups.Layer = (svt_hits.Layer + 1)/2
C
C We try a four pass approach.
C  Pass 1 is for 400 MeV/c and higher.
C  Pass 2 is for 200 MeV/c and higher.
C  Pass 3 is for 100 MeV/c and higher.
C  Pass 4 is to push down Pt as low as possible, perhaps 50 MeV/c
C
C
C ROUTINES THIS FUNCTION CALLS:
      integer pix_reset
      integer GRP_Reset
      integer pix_Add_Point
      integer pix_Next_pixel
      integer GRP_Add_pixel
      integer GRP_Check_Neighbors
      integer sgr_to_ctrack
C
C FUNCTiON WRiTTEN BY:
C     Duncan Prindle,    8-8-92
C
C
C
C Converted to STAF HLC 4/12/96
C iNTERNAL VARIABLES:
      integer MAXHITS
      parameter (MAXHITS=30000)
      integer n_candidate
      integer iTag(3,100)
      real    RTag(100)
      integer indTag(100)
      integer ih, jh, kh, i, j, k, ok, iphi, iz, g_last
      integer ipass
      real    Z0, ZS(MAXHITS), RS, phi(MAXHITS)
      save    ZS, phi
      integer Layer(MAXHITS), Merge(MAXHITS), ind(MAXHITS)
      integer ind_SVT(MAXHITS)
      save    Layer, Merge, ind_SVT
      logical Used(MAXHITS)
      save    Used
      integer pix_Map(576,1600,10)
      save    pix_Map
      real    ZCent, phi1, phi2, phi3, phiCent
      integer iS1, iS2, iS3, nnodes
      real    SCALEPHI2           ! Turns degrees into cm (sort of).
      data    SCALEPHI2/ 0.01 /
      real Dist13, Dist2C, Dist2
      real MergeSize2            ! Hits in same layer within 100microns 
c are essentially the same.
      data MERGESIZE2/ 0.0025 /
      real R2, delphi
      real ScattSize2(4)
      data ScattSize2/  0.00025, 0.0009, 0.0036, 0.014 /
      save SCALEPHI2, MERGESIZE2, ScattSize2
      integer ier
C
      integer tls_index_Sort_r, tls_Quick_Sort_r, tls_id_Offsets
      logical SCV_STATE
C
      character*80 mess
      logical First, Vert_Ok
      data    First/ .TRUE. /
      save    First
C
C-------------------------------------------------------------------------
C How many passes is there to be done?. Check all setup info there

      write(6,*) 'Doing', pix_info_h.nok, ' passes'
      if( pix_info_h.nok .le. 0) then
         write(6,*) ' Dont know how many passes to make.'
         write(6,*) 'Not going to run sgr_am.'
         sgr_am = STAFCV_BAD
         return
      endif

      vert_ok = .FALSE.
      do i = 1,v_h.nok
         if( v(i).flag .eq. 1) then
            z0 = v(i).x(3)
            vert_ok = .TRUE.
          endif
       enddo
       if( .not. vert_ok) then
          write(6,*)' No valid vertex. Not going to run sgr_am. '
          sgr_am = STAFCV_BAD
          return
       endif
       if( svt_hits_h.nok .le. 2) then
          write(6,*) ' Not enough SVT hits. Not going to run sgr_am.'
          sgr_am = STAFCV_BAD
          return
       endif

C Loop up to number of passes requested.

       do 100 ipass = 1, pix_info_h.nok

C Use pix_info grouping.

      if (ipass.eq.1) then               ! Tuned to 400 MeV/c

C
C Creat index into svt_hits.
C  ind_SVT(j) is the row in svt_hits with id=j
          ok = tls_id_offsets( svt_hits_h.nok, svt_hits(1).id,
     +                         svt_hits(2).id, ind_svt, MAXHITS )
          if (.not. SCV_STATE( ok, 'i')) then
              call SCV_MESSAGES( ok, mess )
              write(6,*)' in sgr_am:',mess
           endif
C Reset groups on first pass.
          ok = grp_reset( groups, groups_h.maxlen )
          groups_h.nok = 0
          g_last = 0
C Calculate phi, zs arrays on first pass.
          do i = 1,svt_hits_h.nok
             r2 = svt_hits(i).x(1)**2 + svt_hits(i).x(2)**2
c             if(( 30.0 .lt. r2) .and. (r2 .lt. 45.0)) then
c                Layer(i) = 1
c             elseif(( 46.0 .lt. r2) .and. (r2 .lt. 70.0)) then
c                Layer(i) = 1
c             elseif(( 90.0 .lt. r2) .and. (r2 .lt. 115.0)) then
c                Layer(i) = 2
c             elseif( (116.0 .lt. r2) .and. (r2 .lt. 150.0)) then
c                Layer(i) = 2
c             elseif( (175.0 .lt. r2) .and. (r2 .lt. 210.0)) then
c                Layer(i) = 3
c             elseif( (211.0 .lt. r2) .and. (r2 .lt. 250.0)) then
c                Layer(i) = 3
c            endif
             Layer(i) = ((svt_hits(i).id_wafer/1000)+1)/2
             if( Layer(i) .gt. 3 .or. Layer(i) .lt. 1) then
               write(6,*) ' Hit',i,'recorded as on layer',Layer(i)
            endif
             rs       = 10.0 / sqrt(r2)
             zs(i)    = rs * (svt_hits(i).x(3) - z0) + z0
             phi(i)   = 180. +
     +            atan2d( svt_hits(i).x(2), svt_hits(i).x(1))
             Merge(i) = 0
             Used(i)  = .FALSE.
          enddo
C Now we try merging close hits together.
C if we merge hits, say i and j, we set Merge(j)=i and average ZS and Phi
C storing in ZS(i) and phi(i).
C if more than two hits get merged this particular algorithm will give
C the last one the greatest weight in the average, but the hits should be
C pretty close together so i hope this won't matter.

          ok = tls_index_Sort_r( svt_hits_h.nok, zs(1),
     >         zs(2), ind, MAXHITS )
          if (.not. SCV_STATE( ok, 'i')) then
             call SCV_MESSAGES( ok, mess )
             write(6,*)' in sgr_am:',mess
          endif
          do i = 1,svt_hits_h.nok-1
             j = i+1
             dist2 = (zs(ind(i))- zs(ind(j)))**2
             do while( dist2 .lt. MergeSize2)
                if( Layer(ind(i)) .eq. Layer(ind(j))) then
                   delphi = phi(ind(i))-phi(ind(j))
                   if (delphi .lt.-180.0) then
                      delphi = delphi + 360.0
                   elseif( delphi .gt. 180.0) then
                      delphi = delphi - 180.0
                   endif
                   dist2 = dist2 + SCALEPHI2*delphi**2
                   if( dist2 .lt. MergeSize2) then
                        Merge(ind(j)) = ind(i)
                        zs(ind(i))    = (zs(ind(i))  + zs(ind(j)))/2.0
                        phi(ind(i))   = (phi(ind(i)) + phi(ind(j)))/2.0
                        Layer(ind(j)) = 10
                        used(ind(j))  = .TRUE.
                        endif
                    endif
                j = j + 1
                if( j .gt. svt_hits_h.nok) goto 10
                dist2 = (zs(ind(i))- zs(ind(j)))**2
                enddo
 10             continue
             enddo
C Now we have Merge set up with backward indices.
C  We can have many Merge(j) = i for one i (with Merg(i) = 0).
C  Since i is the hit we will be using, it will be more convenient to be
C  able to follow a list starting at Merge(i) and going to the end.
          do i = 1,svt_hits_h.nok
             if( Merge(i) .ne. 0) then
                j = Merge(i)
                do while (Merge(j) .ne.0)
                   j = Merge(j)
                enddo
                Merge(j) = i
                Merge(i) = 0
             endif
          enddo
C  
        elseif( ipass .gt. 1) then
C ipass = 2 Tuned to 200 MeV/c
C ipass = 3 Tuned to 100 MeV/c
C ipass = 4 Tuned to 50 MeV/c

          g_last = groups(groups_h.nok).id1
        endif
C
C Reset pix_Map.
      if( .not. First) then
          ok = pix_Reset( pix_map, pix_info(ipass).nhitsmax,
     +        pix_info(ipass).nphi, pix_info(ipass).nz,
     +        pix_info(ipass).nhitsmax )
       endif
       First = .FALSE.
C
C Mark SVThits that have been used so we ignore them on this pass.
       do i = 1,groups_h.nok
          ih = groups(i).id2
          ih = ind_SVT(ih)
          Used(ih) = .TRUE.
       enddo
C
C Enter all SVT hits in pixel map.
C Note: i is simply an index into svt_hits.
C       For Candidate_groups (and hence in pix_Map) we simply
C       use the current row number.
C       When we copy to groups we set id_spt = svt_hits(i).id.
C       Thus when going from groups to svt_hit row number we need to
C       use the index (ind_svt), but from Candidate_groups or pix_Map
C       we don't. (Clear?)
       do i = 1,svt_hits_h.nok
          if( .not. used(i)) then
             ok = pix_add_point( i, phi(i), zs(i), pix_map,
     +            pix_info(ipass).nhitsmax, pix_info(ipass).nphi, 
     +            pix_info(ipass).nz,
     +            pix_info(ipass).nhitsmax)
          endif
       enddo
C
C Form groups.
      iphi     = 0
      iz       = 1
    1    continue
         ok = pix_next_pixel( iphi, iz, pix_map,
     +        pix_info(ipass).nhitsmax,pix_info(ipass).nphi,
     +        pix_info(ipass).nz, pix_info(ipass).nhitsmax)
         if( ok .ne. 1) then    ! No more hits left.
            goto 100
         endif
         n_candidate = 0
         ok = grp_add_pixel( iphi, iz ,pix_map,
     +        pix_info(ipass).nhitsmax, pix_info(ipass).nphi, 
     +        pix_info(ipass).nz, pix_info(ipass).nhitsmax, 1, 
     +        candidate_groups, n_candidate, 100)
         ok = grp_check_neighbors(iphi, iz, pix_map,
     +        pix_info(ipass).nhitsmax, pix_info(ipass).nphi,
     +        pix_info(ipass).nz, pix_info(ipass).nhitsmax, 1,
     +        candidate_groups,n_candidate,100)
C     Need at least three hits to keep group.
         if( n_candidate .lt. 3) then
            goto 1
         endif
C     Need at least one hit in every layer.
         do i = 1,n_candidate
            ih = candidate_groups(i).id2
            candidate_groups(i).ident = Layer(ih)
         enddo
C     Sort candidate_groups by layer.
C     iS1 will be index to first hit in layer 1.
C     iS2 will be index to first hit in layer 2.
C     iS3 will be index to first hit in layer 3.
         ok = tls_Quick_Sort_r(n_candidate, candidate_groups(1).ident,
     +        candidate_groups(2).ident, candidate_groups )
         if (.not.SCV_STATE( ok, 'i')) then
            call SCV_MESSAGES( ok, mess )
            write(6,*)' in sgr_am:',mess
         endif
         is1 = 0
         is2 = 0
         is3 = 0
         do i = 1,n_candidate
            if ((is1.eq.0) .and. (candidate_groups(i).ident.eq.1)) then
               is1 = i
            elseif ((is2.eq.0) .and.
     +              (candidate_groups(i).ident.eq.2)) then
               is2 = i
            else if ((iS3.eq.0) .AND.
     +              (candidate_groups(i).ident.eq.3)) then
               iS3 = i
            endif
         enddo
         if ((iS1.eq.0) .OR. (iS2.eq.0) .OR. (iS3.eq.0)) then
            GO TO 1
         endif
C     
C     Now require a hit in layer 2 to be near the midpoint of the
C     layer 1 and 3 hits. (in phi-ZS space.)
C     Find distance from center of 1-3 pair to 2 for all combinations.
         nnodes = 0
         do i = iS1,iS2-1
            ih = candidate_groups(i).id2
            phi1 = phi(ih)
            do j = iS3,n_candidate
               jh = candidate_groups(j).id2
               phi3 = phi(jh)
               if ((phi1-phi3).LT.-180.0) phi3 = phi3 - 360.0
               if ((phi1-phi3).GT. 180.0) phi3 = phi3 + 360.0
               phiCent = (phi1 + phi3) / 2.0
               zcent   = (zs(ih) + zs(jh)) / 2.0
               Dist13  = SCALEPHI2*(phi1-phi3)**2 + (zs(ih)-zs(jh))**2
               do k = iS2,iS3-1
                  kh = candidate_groups(k).id2
                  phi2 = phi(kh)
                  if ((phi1-phi2).LT.-180.0) phi2 = phi2 - 360.0
                  if ((phi1-phi2).GT. 180.0) phi2 = phi2 + 360.0
                  Dist2C = SCALEPHI2*(phiCent - phi2)**2
     +                 + (ZCent - zs(kh))**2
                  if (Dist2C.lt.(ScattSize2(ipass)+Dist13/16.0)) then
                     if (nnodes.eq.100) then ! Group is really too big
                        GO TO 13 ! for this analysis.
                     endif
                     nnodes = nnodes + 1
                     iTag(1,nnodes) = i ! Notice we use index to 
                     iTag(2,nnodes) = k !  candidate Group, not SVT hit.
                     iTag(3,nnodes) = j !
                     RTag(nnodes)   = Dist2C
                  endif
               enddo
            enddo
         enddo
 13      continue
C     
C     Now we copy candidate_groups to groups for Triplets.
C if a hit is shared among triplets we choose only the best triplet.
C     Loop over groups, best grouping criteria first (rtag).
C     Copy from candidate_groups to groups if not already rejected.
C     Check all other groups to see if they share a hit.
C     if they do we reject them (set rtag<0)
C     
C     For every hit we need to check if it has been merged with others.
C     (When we have only one triplet we can save some time.)
         if (nnodes.eq.0) then
            GO TO 1
         else if (nnodes.eq.1) then
            ih = candidate_groups(iTag(1,1)).id2
            jh = candidate_groups(iTag(2,1)).id2
            kh = candidate_groups(iTag(3,1)).id2
            if ((.NOT.Used(ih)) .AND. (.NOT.Used(jh))
     +           .AND. (.NOT.Used(kh))) then
               Used(ih) = .TRUE.
               Used(jh) = .TRUE.
               Used(kh) = .TRUE.
               g_last = g_last + 1
               do j = 1,3
                  groups_h.nok = groups_h.nok + 1
                  groups(groups_h.nok)        =
     +                 candidate_groups(iTag(j,1))
                  groups(groups_h.nok).id1  = g_last
                  groups(groups_h.nok).ident = ipass*10+3
                  ih = groups(groups_h.nok).id2
                  groups(groups_h.nok).id2 = svt_hits(ih).id
C     
C     Check if other hits have been merged with this one.
                  do while (Merge(ih).NE.0)
                     ih = Merge(ih)
                     groups_h.nok = groups_h.nok + 1
                     groups(groups_h.nok)        =
     +                    candidate_groups(iTag(j,1))
                     groups(groups_h.nok).id1     = g_last
                     groups(groups_h.nok).ident = 10*ipass+3
                     groups(groups_h.nok).id2 = svt_hits(ih).id
                  enddo
               enddo
            else
C     
C     How can we get here?
               ier = ier + 1
            endif
         else
            ok = tls_index_Sort_r( nnodes, RTag(1), RTag(2),
     +           indTag, 100 )
            if (.not. SCV_STATE( ok, 'i')) then
               call SCV_MESSAGES( ok, mess )
               write(6,*)' in sgr_am:',mess
            endif
            do i = 1,nnodes
C     Copy candidate_Group(indTag(i)) to groups if not rejected.
C               if (RTag(indTag(i)).GT.0) then
C
C I changed ih = candidate_groups(iTag(1,1).id_spt to
C           ih = candidate_groups(iTag(1,indTag(i))).id_spt
C  same for jh and kh other wise always get same points! (I think)
C   Helen Caines 12.11.96
C
               ih = candidate_groups(iTag(1,indTag(i))).id2
               jh = candidate_groups(iTag(2,indTag(i))).id2
               kh = candidate_groups(iTag(3,indTag(i))).id2
C
C                  ih = candidate_groups(iTag(1,1)).id_spt
C                  jh = candidate_groups(iTag(2,1)).id_spt
C                  kh = candidate_groups(iTag(3,1)).id_spt
               if ((.not. Used(ih)) .and. ( .not. Used(jh))
     +              .and. (.not. Used(kh))) then
                  Used(ih) = .TRUE.
                  Used(jh) = .TRUE.
                  Used(kh) = .TRUE.
                  g_last = g_last + 1
                  do j = 1,3
                     groups_h.nok = groups_h.nok + 1
                     groups(groups_h.nok)        =
     +                    candidate_groups(iTag(j,indTag(i)))
                     groups(groups_h.nok).id1     = g_last
                     groups(groups_h.nok).ident = 10*ipass+3
                     ih = groups(groups_h.nok).id2
                     groups(groups_h.nok).id2 = svt_hits(ih).id
C     
C     Check if other hits have been merged with this one.
                     do while (Merge(ih).ne.0)
                        ih = Merge(ih)
                        groups_h.nok = groups_h.nok + 1
                        groups(groups_h.nok)        =
     +                       candidate_groups(iTag(j,indTag(i)))
                        groups(groups_h.nok).id1     = g_last
                        groups(groups_h.nok).ident = 10*ipass+3
                        groups(groups_h.nok).id2 = svt_hits(ih).id
                     enddo
                  enddo
C     Now discard all groups containing a hit that is also
C     in candidate_Group(indTag(i))
C     do j = i+1,nnodes
C     do k = 1,3
C     ih = iTag(k,indTag(i))
C     jh = iTag(k,indTag(j))
C     if (candidate_groups(ih).id_spt .eq.
C     +                          candidate_groups(jh).id_spt) then
C     RTag(indtag(j)) = -abs(RTag(indtag(j)))
C     endif
C     enddo
C     enddo
               else
               endif ! Candidate Group had a point that has already been used
C     endif
            enddo
         endif
         goto 1
 100  continue
C     
C     is that all?

      ok = sgr_to_ctrack(svt_hits_h.nok, svt_hits,
     +     groups_h.nok, groups, stk_track_h.nok, stk_track)

      if (ok .ne. 1) write(6,*) 'Error completing sgr_to_ctrack'
      
      if( (ier) .gt. 0) then 
         write(6,*)' Ier = ' ,ier
         write(6,*) ' This is bad!!!! Shouldnt be able to get iers'
         sgr_am = STAFCV_BAD
      else
         sgr_am = STAFCV_OK
         write(6,'(A,I5,A,I5,A)')' Found',stk_track_h.nok,' trks from',
     >      svt_hits_h.nok,' SVT hits.'
      endif
      return
      end
      



