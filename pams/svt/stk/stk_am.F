      INTEGER*4 FUNCTION STK_AM(
     1            stkpar_h,            stkpar ,
     2         g2t_track_h,         g2t_track ,
     3         g2t_event_h,         g2t_event ,
     4        g2t_vertex_h,        g2t_vertex ,
     5               spt_h,               spt ,
     6               vtx_h,               vtx ,
     7        vtx_direct_h,        vtx_direct ,
     8            groups_h,            groups ,
     9             track_h,             track ,
     9              kine_h,              kine ,
     8          mcgroups_h,          mcgroups ,
     9           mctrack_h,           mctrack ,
     9            mckine_h,            mckine ,
     9            filler_h,            filler ,
     9            config_h,            config ,
     9              geom_h,              geom ) 
      IMPLICIT NONE
C     SYNOPSIS:
C     stk steering routine
C     
C     DESCRIPTION:
C     
C     AUTHOR:
C     C. Pruneau,  Wayne State University
C
C     DATE:
c     27-july-1994 c.a.p. written
c     Wed Aug 20 11:21:43 EDT 1997 H.Caines adapted to SL97a and removed
c                                  ctrack table
c
C     argument declarations
C     =====================
#include "stk_am.inc"
      
C     Local Declarations:
C     ===================
      integer status,isecond

      integer iteration, oldcandidate, iternull, nitermax
      real    th_step, th_init, th_max, th, factor, sfactor(4)
      logical done

      integer mess_id(20)
      character mess*132
      common /stk_mess_com/mess_id

c     EXTERNALS
c     ---------
      integer stk_am_vertex_direct
      integer stk_am_init
      integer stk_am_direct
      integer stk_am_fill3
      integer stk_am_fill4
      integer stk_am_fit
      integer stk_am_select
      integer stk_am_kine

      data sfactor/1.0,1.5,2.0,4.0/

C     RETURN:
C     =======
C     FORMAT:
C     =======
 1000 format(1x,'stk_bs_i : beginning stk')
 1001 format(1x,'stk_',a,'_w : ',a,' returned with abnormal status err')
 1002 format(1x,'stk_fin_i : iteration#',i3,' th_max=',f12.4)

C     EXECUTABLE CODE:
C     ================
      WRITE(*,*) 'STK_AM: Starting ##################################'
      stk_am = stafcv_ok

      write(mess,1000) 
      call message(mess,1,mess_id(1))

c     initialization phase
      if( track_h.nok .eq. 0) then
         status = STK_AM_INIT(
     1            stkpar_h,            stkpar ,
     2            groups_h,            groups ,
     3             track_h,             track ,
     4              kine_h,              kine ,
     5          mcgroups_h,          mcgroups ,
     6           mctrack_h,           mctrack ,
     7            mckine_h,            mckine )
      endif

      if (stkpar(1).vertex.eq.1) then
         status = STK_AM_VERTEX_DIRECT(
     1         g2t_event_h,         g2t_event ,
     2        g2t_vertex_h,        g2t_vertex ,
     3               vtx_h,               vtx ,
     4        vtx_direct_h,        vtx_direct )
         if (status.ne.stafcv_ok) then
c            stk_am = status
            write(mess,1001) 'svd','stk_am_vertex_direct'
            call message(mess,1,mess_id(2))
            return
         end if
      end if
            
      if (stkpar(1).direct.eq.1) then
         status = STK_AM_DIRECT(
     1         g2t_track_h,         g2t_track ,
     2               spt_h,               spt ,
     3          mcgroups_h,           mcgroups,
     4           mctrack_h,            mctrack,
     5              geom_h,              geom )
         if (status.ne.stafcv_ok) then
c            stk_am = status
            write(mess,1001) 'std','stk_am_direct'
            call message(mess,1,mess_id(3))
            return
         end if

         status = STK_AM_FIT(
     1            stkpar_h,            stkpar ,
     2               spt_h,               spt ,
     3               vtx_h,               vtx ,
     4          mcgroups_h,          mcgroups ,
     5           mctrack_h,           mctrack )
         if (status.ne.stafcv_ok) then
            stk_am = status
            write(mess,1001) 'stdf','stk_am_fit'
            call message(mess,1,mess_id(4))
            return
         end if

         status = STK_AM_KINE(
     1               vtx_h,               vtx ,
     2          mcgroups_h,          mcgroups ,
     3           mctrack_h,           mctrack ,
     4            mckine_h,            mckine )
         if (status.ne.stafcv_ok) then
c            stk_am = status
            write(mess,1001) 'stdk','stk_am_kine'
            call message(mess,1,mess_id(5))
            return
         end if

      end if

c     If the grouper has been called, track_h.nok will be non-zero.
c     Then we should call  fit, select, kine, etc before going into the
c     normal reconstruction loop. (wkw)
      if (track_h.nok.gt.0) then
               status = STK_AM_FIT(
     1            stkpar_h,            stkpar ,
     2               spt_h,               spt ,
     3               vtx_h,               vtx ,
     4            groups_h,            groups ,
     5             track_h,             track )
               if (status.ne.stafcv_ok) then
                  write(mess,1001) 'stfit','stk_am_fit'
                  call message(mess,1,mess_id(7))
               end if
               status = STK_AM_SELECT(
     1            stkpar_h,            stkpar ,
     2               spt_h,               spt ,
     3            groups_h,            groups ,
     4             track_h,             track )
               if (status.ne.stafcv_ok) then
                  write(mess,1001) 'sts','stk_am_select'
                  call message(mess,1,mess_id(8))
               end if

         endif

            
c     normal reconstruction

      if (stkpar(1).fill.eq.1) then

c         if (stkpar(1).nitermax.lt.0) stkpar(1).nitermax = 1 ! wkw
         if (stkpar(1).nitermax.lt.0) stkpar(1).nitermax = 0 ! wkw
         if (stkpar(1).niternull.le.0) stkpar(1).niternull = 4

         iteration    = 0
         oldcandidate = track_h.nok

c         th_max  = stkpar(1).th_init ! wkw
c         th_step = stkpar(1).th_step ! wkw
         th_init=stkpar(1).th_init ! wkw
         th_max=stkpar(1).th_max ! wkw
         nitermax=stkpar(1).nitermax ! wkw
c         th_step=(th_max-th_init)/(float(nitermax)-1.0) ! wkw

         if (stkpar(1).mode.lt.3) then

            done = .false.
            do while (.not.done)

               iteration = iteration + 1
               print 401,iteration
 401           format(' Doing STK iteration # ',i2,' ...')
               factor=(float(iteration)-1.0)/(float(nitermax)-1.0) 
c wkw
c              factor=1 when iteration=nitermax 
               th=th_init+(factor**2.0)*(th_max-th_init) ! wkw
c               th_max = th_max + float(iteration)*th_step ! wkw
               write(mess,1002) iteration, th

c               filler(1).th21  =    th
c               filler(1).th31  =    th
c               filler(1).th41  =  1.2 * th
c               filler(1).th51  =  1.2 * th
c               filler(1).th61  =  1.2 * th
c               filler(1).th71  =  1.2 * th
c               filler(1).th81  =  1.2 * th
c               filler(1).th32  =    th
c               filler(1).th42  =    th
c               filler(1).th52  =  1.2 * th
c               filler(1).th62  =  1.2 * th
c               filler(1).th72  =  1.2 * th
c               filler(1).th82  =  1.2 * th
c               filler(1).th43  =    th
c               filler(1).th53  =    th
c               filler(1).th63  =  1.2 * th
c               filler(1).th73  =  1.2 * th
c               filler(1).th83  =  1.2 * th
c               filler(1).th54  =    th
c               filler(1).th64  =    th
c               filler(1).th74  =  1.2 * th 
c               filler(1).th84  =  1.2 * th
c               filler(1).th65  =    th
c               filler(1).th75  =    th
c               filler(1).th85  =  1.2 * th
c               filler(1).th76  =    th
c               filler(1).th86  =    th
c               filler(1).th87  =    th

               if (stkpar(1).mode.eq.1) then
                  filler(1).th21  =    th
                  filler(1).th31  =    th
                  filler(1).th41  =    th
                  filler(1).th51  =    th
                  filler(1).th61  =    th
                  filler(1).th71  =    th
                  filler(1).th81  =    th
                  filler(1).th32  =    th
                  filler(1).th42  =    th
                  filler(1).th52  =    th
                  filler(1).th62  =    th
                  filler(1).th72  =    th
                  filler(1).th82  =    th
                  filler(1).th43  =    th
                  filler(1).th53  =    th
                  filler(1).th63  =    th
                  filler(1).th73  =    th
                  filler(1).th83  =    th
                  filler(1).th54  =    th
                  filler(1).th64  =    th
                  filler(1).th74  =    th
                  filler(1).th84  =    th
                  filler(1).th65  =    th
                  filler(1).th75  =    th
                  filler(1).th85  =    th
                  filler(1).th76  =    th
                  filler(1).th86  =    th
                  filler(1).th87  =    th
               else
                  filler(1).th21  =    th*stkpar(1).sec_factor
                  filler(1).th31  =    th*stkpar(1).sec_factor
                  filler(1).th41  =    th*stkpar(1).sec_factor
                  filler(1).th51  =    th*stkpar(1).sec_factor
                  filler(1).th61  =    th*stkpar(1).sec_factor
                  filler(1).th71  =    th*stkpar(1).sec_factor
                  filler(1).th81  =    th*stkpar(1).sec_factor
                  filler(1).th32  =    th*stkpar(1).sec_factor
                  filler(1).th42  =    th*stkpar(1).sec_factor
                  filler(1).th52  =    th*stkpar(1).sec_factor
                  filler(1).th62  =    th*stkpar(1).sec_factor
                  filler(1).th72  =    th*stkpar(1).sec_factor
                  filler(1).th82  =    th*stkpar(1).sec_factor
                  filler(1).th43  =    th
                  filler(1).th53  =    th
                  filler(1).th63  =    th
                  filler(1).th73  =    th
                  filler(1).th83  =    th
                  filler(1).th54  =    th
                  filler(1).th64  =    th
                  filler(1).th74  =    th
                  filler(1).th84  =    th
                  filler(1).th65  =    th
                  filler(1).th75  =    th
                  filler(1).th85  =    th
                  filler(1).th76  =    th
                  filler(1).th86  =    th
                  filler(1).th87  =    th
               endif
            
               if (stkpar(1).nlayers.eq.3) then
                  status = STK_AM_FILL3(
     1               spt_h,               spt ,
     2               vtx_h,               vtx ,
     3            groups_h,            groups ,
     4             track_h,             track ,
     5            filler_h,            filler ,
     6            config_h,            config ,
     7             geom_h,              geom )
                  if (status.ne.stafcv_ok) then
                     write(mess,1001) 'stf','stk_am_fill3'
                     call message(mess,1,mess_id(6))
                  end if
               else ! 4 layers
                  status = STK_AM_FILL4(
     1               spt_h,               spt ,
     2               vtx_h,               vtx ,
     3           groups_h,            groups ,
     4            track_h,             track ,
     5           filler_h,            filler ,
     6           config_h,            config ,
     7             geom_h,              geom )
                  if (status.ne.stafcv_ok) then
                     write(mess,1001) 'stf','stk_am_fill4'
                     call message(mess,1,mess_id(6))
                  end if
               end if
            
               status = STK_AM_FIT(
     1            stkpar_h,            stkpar ,
     2               spt_h,               spt ,
     3               vtx_h,               vtx ,
     4            groups_h,            groups ,    
     5             track_h,             track )
               if (status.ne.stafcv_ok) then
                  write(mess,1001) 'stfit','stk_am_fit'
                  call message(mess,1,mess_id(7))
               end if
            
               status = STK_AM_SELECT(
     1            stkpar_h,            stkpar ,
     2               spt_h,               spt ,
     3            groups_h,            groups ,
     4             track_h,             track )
               if (status.ne.stafcv_ok) then
                  write(mess,1001) 'sts','stk_am_select'
                  call message(mess,1,mess_id(8))
               end if
               print*,'Found ',track_h.nok-oldcandidate
               if (iteration.eq.stkpar(1).nitermax) done = .true.
               if (oldcandidate.eq.track_h.nok) then
                  iternull = iternull + 1
                  if (iternull.eq.stkpar(1).niternull) then
                     done = .true.
                  end if
               else
                  iternull = 0
                  oldcandidate = track_h.nok
               end if

            end do 


         else if (stkpar(1).mode.eq.3) then

            iteration=0
            done = .false.
            do while (.not.done)
                     
               iteration = iteration + 1
! loop over opening up for secondaries
               do isecond=1,4 

                  print 401,iteration
                  factor=(float(iteration)-1.0)/(float(nitermax)-1.0) ! wkw
c     factor=1 when iteration=nitermax !
                  th=th_init+(factor**2.0)*(th_max-th_init) ! wkw
c     th_max = th_max + float(iteration)*th_step ! wkw
                  write(mess,1002) iteration, th
                     
c     filler(1).th21  =    th
c     filler(1).th31  =    th
c     filler(1).th41  =  1.2 * th
c     filler(1).th51  =  1.2 * th
c     filler(1).th61  =  1.2 * th
c     filler(1).th32  =    th
c     filler(1).th42  =    th
c     filler(1).th52  =  1.2 * th
c     filler(1).th62  =  1.2 * th
c     filler(1).th43  =    th
c     filler(1).th53  =    th
c     filler(1).th63  =  1.2 * th
c     filler(1).th54  =    th
c     filler(1).th64  =    th
c     filler(1).th65  =    th
                     
                     
                     filler(1).th21  =    th*sfactor(isecond)
                     filler(1).th31  =    th*sfactor(isecond)
                     filler(1).th41  =    th*sfactor(isecond)
                     filler(1).th51  =    th*sfactor(isecond)
                     filler(1).th61  =    th*sfactor(isecond)
                     filler(1).th71  =    th*sfactor(isecond)
                     filler(1).th81  =    th*sfactor(isecond)
                     filler(1).th32  =    th*sfactor(isecond)
                     filler(1).th42  =    th*sfactor(isecond)
                     filler(1).th52  =    th*sfactor(isecond)
                     filler(1).th62  =    th*sfactor(isecond)
                     filler(1).th72  =    th*sfactor(isecond)
                     filler(1).th82  =    th*sfactor(isecond)
                     filler(1).th43  =    th
                     filler(1).th53  =    th
                     filler(1).th63  =    th
                     filler(1).th73  =    th
                     filler(1).th83  =    th
                     filler(1).th54  =    th
                     filler(1).th64  =    th
                     filler(1).th74  =    th
                     filler(1).th84  =    th
                     filler(1).th65  =    th
                     filler(1).th75  =    th
                     filler(1).th85  =    th
                     filler(1).th76  =    th
                     filler(1).th86  =    th
                     filler(1).th87  =    th
                     
                     
                     if(stkpar(1).nlayers.eq.3) then
                        status = STK_AM_FILL3(
     1               spt_h,               spt ,
     2               vtx_h,               vtx ,
     3            groups_h,            groups ,
     4             track_h,             track ,
     5            filler_h,            filler ,
     6            config_h,            config ,
     7              geom_h,              geom )
                        if (status.ne.stafcv_ok) then
                           write(mess,1001) 'stf','stk_am_fill3'
                           call message(mess,1,mess_id(6))
                        end if
                     else       ! 4 layers
                        status = STK_AM_FILL4(
     1               spt_h,               spt ,
     2               vtx_h,               vtx ,
     3            groups_h,            groups ,
     4             track_h,             track ,
     5            filler_h,            filler ,
     6            config_h,            config ,
     7              geom_h,              geom )
                        if (status.ne.stafcv_ok) then
                           write(mess,1001) 'stf','stk_am_fill4'
                           call message(mess,1,mess_id(6))
                        end if
                     end if
                     
                     status = STK_AM_FIT(
     1            stkpar_h,            stkpar ,
     2               spt_h,               spt ,
     3               vtx_h,               vtx ,
     4            groups_h,            groups ,
     5             track_h,             track )
                     if (status.ne.stafcv_ok) then
                        write(mess,1001) 'stfit','stk_am_fit'
                        call message(mess,1,mess_id(7))
                     end if
                     
                     status = STK_AM_SELECT(
     1            stkpar_h,            stkpar ,
     2               spt_h,               spt ,
     3            groups_h,            groups ,
     4             track_h,             track )
                     if (status.ne.stafcv_ok) then
                        write(mess,1001) 'sts','stk_am_select'
                        call message(mess,1,mess_id(8))
                     end if
                     print*,'Found ',track_h.nok-oldcandidate
                     if (iteration.eq.stkpar(1).nitermax) done=.true.
                     if (oldcandidate.eq.track_h.nok) then
                        iternull = iternull + 1
                        if (iternull.eq.stkpar(1).niternull) then
                           done = .true.
                        end if
                     else
                        iternull = 0
                        oldcandidate = track_h.nok
                     end if
                     
                  end do        ! loop over secondary opening of th
                  
               end do           ! loop over th
               
            end if              ! mode=3
            
            
         end if

c     Note: need to call kine even if not filling since
c     sgr may have been called
      if (track_h.nok.gt.0) then
         status = STK_AM_KINE(
     1               vtx_h,               vtx ,
     2            groups_h,            groups ,
     3             track_h,             track ,
     4              kine_h,              kine )
         if (status.ne.stafcv_ok) then
            stk_am = status
            write(mess,1001) 'stkin','stk_am_kine'
            call message(mess,1,mess_id(9))
            return
         end if
      end if



      WRITE(*,*) 'STK_AM: Stopping #################################'
      STK_AM=STAFCV_OK
      RETURN
      END

























