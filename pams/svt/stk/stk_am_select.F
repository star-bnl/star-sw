      INTEGER*4 FUNCTION STK_AM_SELECT(
     1            stkpar_h,            stkpar ,
     2               spt_h,               spt ,
     3            groups_h,            groups ,
     4             track_h,             track ) 
      IMPLICIT NONE
C     SYNOPSIS:
C     selects the tracks from common first layer with best chi2
C     
C     DESCRIPTION:
C     select and keep track candidates with the lowest chi-squares
C
C     AUTHOR:
C     K.Wilson and C. Pruneau,  Wayne State University
C     
C     DATE:
C     2-nov-1993 c.a.p.
C     12-FEB-1997 Helen Caines added in asignment of spt.id_track
C     20-AUG-1997 Helen Caines removed ctrack table
C     20-AUG-1997 Helen Caines selects track with best chi2 if any hits in
C     29-AUG-1997 Helen Caines added groups table & removed spts from trk table
C                  common
C     
C     ARGUMENTS:
C     track_h   header to table track
C     track     rows of table   track
C      
C     argument declaration
C     --------------------
#include "stk_am_select.inc"
      
C     Local Declarations:
C     ===================
      integer itrack1, itrack2,n_count
      integer  j, i, k, ispt, kspt, jspt, other_track, bad_track
      integer nsame,start  ! wkw
      integer ok,krows,table_off(20)
      integer tls_Search_Near_i
      real    chisqnew,chisqother
      logical more_hits,SCV_STATE
      character*80 mess


C     RETURN:
C     =======
C     FORMAT:
C     =======
 1001 format(1x,'<w>-stk_am_select :'//
     &     'no rows in track')

C     EXECUTABLE CODE:
C     ================
c     check if the svt track table is empty

      WRITE(*,*) 'STK_AM_SELECT: Starting #############################'
      
      if (track_h.nok.eq.0) then
         stk_am_select = stafcv_ok
         write(6,1001)
         return
      end if

c     wkw:starting from previous loop
      start=stkpar(1).ngood+1
      
      table_off(1) = start*3
      krows = 20
      ok = tls_Search_Near_i(groups_h.nok,groups(1).id1,groups(2).id1,
     +    start,table_off,krows)
      if (.not. SCV_STATE( ok, 'i')) then
         call SCV_MESSAGES( ok, mess )
         write(6,*)' in stk_select:',mess
      endif


c     WKW: Section to test chi:
      do i=start,track_h.nok
C         track(i).flag=1        ! start by marking track as good
c     for mode three, put a .075 lower limit on pt
         if (stkpar(1).mode.eq.3) then
            if (abs(track(i).invpt).gt.13.3333) then
               track(i).flag=0  ! dump this track
            endif
         endif
         if (stkpar(1).chi_filter.eq.1) then
            if (track(i).chisq(1).gt.stkpar(1).chicut(1)) then
               track(i).flag=0  ! dump this track
            endif
            if (track(i).chisq(2).gt.stkpar(1).chicut(2)) then
               track(i).flag=0  ! dump this track
            endif
         endif
      enddo

      do j=table_off(1),groups_h.nok
         if( groups(j).ident .gt. 1 .and. 
     +        track(groups(j).id1).flag .lt. 1) then
            groups(j).ident = 0
         endif
      enddo

c     wkw: This section removes duplicate tracks 
c     It checks
c     the latest tracks to make sure they were not found before
c     This section is only important if spts are not being removed.
      if ( stkpar(1).spt_mark .ne. 1) then 
         do itrack1=start,track_h.nok ! new tracks
            if ( track(itrack1).flag .ge. 1) then
               do itrack2=1,start-1 ! old tracks
                  if (track(itrack1).nspt.eq.track(itrack2).nspt) then
                     nsame=0    ! initialize number of matches
                     do jspt=table_off(1),groups_h.nok 
                        if( groups(jspt).ident .gt. 1 .and. 
     +                       groups(jspt).id1 .eq. itrack1) then 
                           do ispt=1,table_off(1)-1
                              if( groups(ispt).ident .gt. 1 .and. 
     +                             groups(ispt).id1 .eq. itrack2) then 
                                 if( groups(jspt).id2.eq.
     1                       groups(ispt).id2) nsame=nsame+1 ! match
                              endif
                           enddo
                        endif
                     enddo                 
                     if (nsame.eq.track(itrack1).nspt) then ! perfect match
                        track(itrack1).flag = 0 ! get rid of duplicate track
                     endif 
                  endif         ! same track numbers
               enddo            ! loop over old tracks
            endif
         enddo                  ! loop over new tracks
         do j=table_off(1),groups_h.nok
            if( groups(j).ident .gt. 1 .and. 
     +           track(groups(j).id1).flag .lt. 1) then
               groups(j).ident = 0
            endif
         enddo
 
c This section ensures that hits only get placed on one track
c  if stkpar(1).mark =1

      else
         do i=start,track_h.nok
            more_hits = .true.
            ispt = 1
            j = table_off(1)
            if( track(i).flag .ge. 1) then
               do while (more_hits .and. j .le. groups_h.nok)
                  if( groups(j).ident .gt. 1 .and. 
     +                 groups(j).id1 .eq. i) then
                     jspt = groups(j).id2
                     if( spt(jspt).id_track .eq. 0 ) then
                        spt(jspt).id_track = i
                        ispt = ispt +1
                     else
                        other_track = spt(jspt).id_track
                        chisqnew = track(i).chisq(1) + track(i).chisq(2)
                        chisqother = track(other_track).chisq(1) + 
     +                       track(other_track).chisq(2)
                        if( chisqnew .lt. chisqother) then
                           bad_track = other_track
                           ispt = ispt +1
                           spt(jspt).id_track = i
                        else
                           bad_track = i
                        endif

C Delete space point from bad_track if bad_track has only 3 points
C then delete track otherwise remove space point and shuffle remaining
C points down list

                        if ( track(bad_track).nspt .le. 3) then
                           track(bad_track).flag = 0
                           k = table_off(1)
                           n_count = 0
                           do while (k .le. groups_h.nok)
                              if (groups(k).ident .gt. 1 .and.  
     +                             groups(k).id1 .eq. bad_track) then
                                 kspt = groups(k).id2
                                 groups(k).ident = 0
                                 n_count = n_count + 1
                                 if ( spt(kspt).id_track .eq. bad_track)
     +                                then
                                    spt(kspt).id_track = 0
                                 endif
                                 if( n_count .ge. track(bad_track).nspt)
     +                                then 
                                    k = groups_h.nok + 1
                                 endif
                              endif
                              k = k+1
                           enddo
                        else    ! nspt .le. 3
                           k = table_off(1)
                           do while ( k .le. groups_h.nok)
                              
                              if( groups(k).ident .gt. 1 .and. 
     +                             groups(k).id1 .eq. bad_track
     +                             .and. groups(k).id2 .eq. jspt) then
                                 groups(k).ident = 0
                                 k = groups_h.nok +1
                              endif
                              k = k+1
                           enddo
                           track(bad_track).nspt=track(bad_track).nspt-1
                        endif   ! nspt .le. 3
                     endif      ! spt.id = 0
                     if ( ispt .gt. track(i).nspt .or. 
     +                    track(i).flag .eq. 0) more_hits = .false.
                  endif         ! if( groups(j).id1 = i
                  j = j + 1
               enddo            ! do( groups_h.nok .and. more_hits
            endif               ! if flag = 1
         enddo                  ! do new_tracks
      endif                     ! if mark_spts
      

c     WKW: Section to filter out bad tracks: (reassign id to get rid of 
C     hole in sequence)
      
      j=start-1
      do i=start,track_h.nok
         if (track(i).flag.ge.1) then ! keep 
            j=j+1
          
            track(j).id=j
            track(j).id_mctrack=track(i).id_mctrack
            track(j).nspt=track(i).nspt
            track(j).sec=track(i).sec
            track(j).pid=track(i).pid
            
c     update track table:
            track(j).flag=track(i).flag
            track(j).id_globtrk = 0
            track(j).chisq(1)=track(i).chisq(1)
            track(j).chisq(2)=track(i).chisq(2)
            do k=1,2
               track(j).dedx(k)=track(i).dedx(k)
            enddo
            track(j).invpt=track(i).invpt
            track(j).phi0=track(i).phi0
            track(j).psi=track(i).psi
            track(j).r0=track(i).r0
            track(j).tanl=track(i).tanl
            track(j).z0=track(i).z0
            track(j).impact=track(i).impact
            do k=1,15
               track(j).cov(k)=track(i).cov(k)
            enddo
            do k=1,10
               track(j).cres(k)=track(i).cres(k)
               track(j).lres(k)=track(i).lres(k)
            enddo
C HLC reasign spt.id_track to new value
            do k=table_off(1),groups_h.nok 
               if ( groups(k).ident .gt. 1 .and. 
     +              groups(k).id1 .eq. i) then
                  ispt = groups(k).id2
                  spt(ispt).id_track = j
                  groups(k).id1 = j
                  if (stkpar(1).spt_mark.eq.1) spt(ispt).flag=1.0 ! mark 
C     space point as used
               endif
            enddo
         endif
      enddo

      track_h.nok=j
      stkpar(1).ngood = j
      
      k=table_off(1)
      do j=table_off(1),groups_h.nok
         if( groups(j).ident .ne. 0 ) then
            groups(k).ident = groups(j).ident
            groups(k).id1 = groups(j).id1
            groups(k).id2 = groups(j).id2
            k = k+1
         endif
      enddo
         
      groups_h.nok = k-1

      WRITE(*,*) 'STK_AM_SELECT: Stopping #############################'
      STK_AM_SELECT=STAFCV_OK
      RETURN
      END
