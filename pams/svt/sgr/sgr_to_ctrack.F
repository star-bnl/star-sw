        INTEGER FUNCTION sgr_to_ctrack(no_hits, svt_hits,
     +                                 no_groups, groups,
     +                                 no_track, svt_track)
      IMPLICIT NONE					
!...Title      : 

!...Author     : Stathes Paganis (U.T. Austin) 

!...Description: This module links sgr (SVT-grouper-tracking) and 
!      stk (SVT-slow tracking)
!                svt_track table is filled with data from svt_groups 
!      and svt_hits tables

!...Input      : svt_hits , groups

!...Output     : svt_track

!...Status     : Tested May/14/94

!...Adapted to STAF DEC 96 Helen Caines

#include "scs_spt.inc"
#include "sgr_groups.inc"
#include "stk_track.inc"

      record /stk_track_st/ svt_track(1)
      record /sgr_groups_st/ groups(1)
      record /scs_spt_st/ svt_hits(1)
      
      INTEGER k,i,j,ii
      integer no_hits, no_groups, no_track
      
      REAL cx(8),cy(8),cz(8)
      
 10   format(1x,'sgr_to_track: no rows in svt_hits.')
 11   format(1x,'sgr_to_track: no rows in svt_groups.')

      sgr_to_ctrack = 1
      
      if (no_hits .le. 0) then
         write(6,10)
         sgr_to_ctrack = 0
      elseif (no_groups .le. 0) then
         write(6,11) 
         sgr_to_ctrack = 0
      endif
      
      ii=0
      i=0
      j=0
      
      do i=1,no_groups
         k=0
         
C------------IGNORE  THE DOUBLE TRACKS  ----------------------------------------

         if(i.gt.1)then
            do ii=i-1,1,-1
               if(groups(i).id1 .eq. groups(ii).id1)then
                  goto 1001
               endif
            enddo
         endif
         
C--------------------------------------------------------------------------------
         
         no_track = no_track + 1
         do j=i,no_groups
            if( groups(i).id1 .eq. groups(j).id1) then
               
               if( k .eq. 0) then
                  goto 1002
               elseif(abs(cx(k)-svt_hits(groups(j).id2).x(1))
     +                 .lt.0.000000001.and.
     +                 abs(cy(k)-svt_hits(groups(j).id2).x(2))
     +                 .lt.0.000000001.and.
     +                 abs(cz(k)-svt_hits(groups(j).id2).x(3))
     +                 .lt.0.000000001)then
                  print *,'same points in one group'
                  goto 1003
               endif
 1002          k=k+1
               if( k .gt. 8) then
                  write(6,*) ' Too many space points in this group',i
                  k = k-1
                  goto 1005
               endif
               cx(k)        = svt_hits(groups(j).id2).x(1)
               cy(k)        = svt_hits(groups(j).id2).x(2)
               cz(k)        = svt_hits(groups(j).id2).x(3)      
            endif
 1003    enddo
 1005     continue
          svt_track(no_track).id      = no_track
          svt_track(no_track).id_mctrack
     +         = svt_hits(groups(i).id2).id_mctrack
          svt_track(no_track).nspt    = k
          svt_track(no_track).pid     = 0
          svt_track(no_track).sec     = 0

 1001 enddo
 1004 RETURN
      END




