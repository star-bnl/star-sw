C**************************************************************************
C
C          Module  svm_svt_eval:  evaluation module for the vector-vector
C                             matching module svm for svt-to-tpc tracks
C                             with emphasis on the unmatched svt tracks.
C     Description:
C          The module reads the matching information in table evt_match,
C     the monte carlo tracking and matching information from tables svt_spt,
C     and tpeval, and the tracking parameters from tables svt_ctrack, tptrack
C     and svt_track
C     and provides the following information in table svm_eval_strk
C
C     For all unmatched svt tracks:
C
C     1.  svt track id  == svt_track.id
C     2.  status        =  1 for unmatched "bad" svt track
C                       =  2 for unmatched single good svt track
C                       =  3 for unmatched "good" matchable svt track
C     3.  track flag    == svt_track.flag
C     4.  fitting chisq == svt_track.chisq(2) for circle/line fit chi-sq
C     5.  inverse pt    == svt_track.invpt
C
C
C     Author:     Lanny Ray
C                 Dept. of Physics
C                 Univ. of Texas
C                 Austin, Texas 78712
C                 (512) 471-6107
C                 Internet address:  "ray@utaphy.ph.utexas.edu"
C
C     History:
C
C           June 8, 1994 - Initially written
C
C           June 9, 1994 - Installed in TAS under package svm
C
C           Dec 1996 - Installed into STAF
C           Sept 1997 - Updated to SL97a, updated svt table inputs.
C
C     Arguments:
C           TAS tables:  svt_spt, svt_groups, svt_track,       :  READ
C                        tptrack, tpeval, evt_match,           :  READ
C                        svm_eval_par                          :  READ
C                        svm_eval_strk                         :  WRITE 
C
C****************************************************************************

      integer function svm_svt_eval(
     &   svt_spt_h,    svt_spt,
     &   svt_groups_h, svt_groups,
     &   svt_track_h,  svt_track,
     &   tptrack_h,    tptrack,
     &   tpeval_h,     tpeval,
     &   evt_match_h,  evt_match,
     &   svm_eval_par_h,  svm_eval_par,
     &   svm_eval_strk_h, svm_eval_strk  )

      implicit none

C   Argument Declarations, STAF include and record statements:
C

#include "svm_svt_eval.inc"
C     Local Variable Type Declarations:
C

      integer     i, index, j, tpcmat, mctpc, svtmat, mcsvt, nmax, row
      integer     nmax2, tab_count
      integer     tpt_rows(10000), tpe_rows(10000), svtspt_rows(15000)
      integer     svt_trk(10000,4), tpc_trk(10000,4)
      integer     evt_svt(10000), evt_tpc(10000)
      parameter (nmax = 10000)
      parameter (nmax2 = 15000)

CC   Message Formats:
CC

100   format(5x,'Build Pointer Arrays')
101   format(5x,'SVT track id ',i10,' gt 10000 -- skip and go on')
102   format(5x,'TPC track id ',i10,' gt 10000 -- skip and go on')
1020  format(5x,'tpt track id ',i10,' gt 10000 -- skip and go on')
103   format(5x,'Begin building MC matching cross ref. pointers')
104   format(5x,'MC matching cross ref. pointers done')
108   format(5x,'svt-spt.id = ',i10,' gt 15000 -- skip and go on')
109   format(5x,'svt_track_h.nok .gt. svm_eval_strk_h.maxlen -STOP')

CC   Executable Code:
CC
CC   Set big arrays to zero:
CC
      do i = 1,nmax
      tpt_rows(i) = 0
      tpe_rows(i) = 0
      evt_svt(i)  = 0
      evt_tpc(i)  = 0
           do j = 1,4
           svt_trk(i,j) = 0
           tpc_trk(i,j) = 0
           end do
      end do
      do i = 1,nmax2
      svtspt_rows(i) = 0
      end do
CC  
CC  Set all rows in table svm_eval_strk to zero and nok to 0
CCC
      do i = 1,svm_eval_strk_h.maxlen
      svm_eval_strk(i).id      = 0
      svm_eval_strk(i).status  = 0
      svm_eval_strk(i).flag    = 0
      svm_eval_strk(i).chisq(1) = 0.0
      svm_eval_strk(i).chisq(2) = 0.0
      svm_eval_strk(i).invpt    = 0.0
      end do
      svm_eval_strk_h.nok      = 0
CC
CC  Build svt_spt primary key row array:
CC
      do 40 i = 1,svt_spt_h.nok
      if(svt_spt(i).id .le. 0) go to 40
      if(svt_spt(i).id .gt. nmax2) then
           write(6,108) svt_spt(i).id
           go to 40
      end if
      svtspt_rows(svt_spt(i).id) = i 
40    continue

CC  Build local arrays for TAS tables svt_spt, tpeval, evt_match, and tptrack
CC
CC  svt_trk(10000,4): cols1-4= no. pts on track,mc check, mc trk id, if true
CC                    mc match then this is the row index of tpc_trk
CC  tpc_trk(10000,4): cols1-4= no. pts on track,mc check, mc trk id, if true
CC                    mc match then this is the row index of svt_trk
CC  evt_svt(10000),  col1 = matched tpc track id #
CC  evt_tpc(10000),  col1 = matched svt track id #
CC
CC  Use nmax = 10000 for do-loops and dimension checks on arrays.
CC  Count number of svt space points on each reconstructed track, verify
CC  monte carlo track assignment, build local array svt_trk
CC
      write(6,100)
      do 10 i = 1,svt_track_h.nok
      if(svt_track(i).nspt .lt. 3) go to 10
      if(svt_track(i).id .le. 0) go to 10
      if(svt_track(i).id .gt. nmax) then
           write(6,101) svt_track(i).id
           go to 10
      end if
      svt_trk(svt_track(i).id , 1) = svt_track(i).nspt
      svt_trk(svt_track(i).id,2) = 1
           do 12 j = 1,svt_groups_h.nok
              if( svt_groups(j).id1 .eq. svt_track(i).id) then
                 index = svt_groups(j).id2
                 if(index .le. 0) go to 12
                 if(index .gt. nmax2) go to 12
                 row = svtspt_rows(index)
                 svt_trk(svt_track(i).id,3) = svt_spt(row).id_mctrack
                 go to 13
              endif
 12        continue
 13        continue
           do 14 j = 1,svt_groups_h.nok
              if( svt_groups(j).id1 .eq. svt_track(i).id) then
                 index = svt_groups(i).id2
                 if(index .le. 0) go to 14
                 if(index .gt. nmax2) go to 14
                 row = svtspt_rows(index)
                 if(svt_trk(svt_track(i).id,3) .ne. 
     &                svt_spt(row).id_mctrack) then
                    svt_trk(svt_track(i).id,2) = 0
                    svt_trk(svt_track(i).id,3) = 0
                    go to 15
                 end if
              endif
 14        continue
 15        continue
 10     continue

CC   Build tpc_trk and tpeval arrays:
CC
      do 20 i = 1,tpeval_h.nok
      if(tpeval(i).rtrk .le. 0) go to 20
      if(tpeval(i).rtrk .gt. nmax) then
           write(6,102) tpeval(i).rtrk
           go to 20
      end if
      tpe_rows(tpeval(i).rtrk) = i
      tpc_trk(tpeval(i).rtrk,1) = tpeval(i).nrec
      if(tpeval(i).qfact .ge. svm_eval_par(1).qmin) then
           tpc_trk(tpeval(i).rtrk,2) = 1
           tpc_trk(tpeval(i).rtrk,3) = tpeval(i).mtrk
      end if
20    continue

CC  Build evt_match arrays:
CC
      do 30 i = 1,evt_match_h.nok
      if(evt_match(i).id .eq. 0) go to 30
      if(evt_match(i).idsvt .le. 0) go to 30
      if(evt_match(i).idtpc .le. 0) go to 30
      if(evt_match(i).idsvt .gt. nmax) go to 30
      if(evt_match(i).idtpc .gt. nmax) go to 30
      evt_svt(evt_match(i).idsvt) = evt_match(i).idtpc
      evt_tpc(evt_match(i).idtpc) = evt_match(i).idsvt
30    continue

CC  Build tptrack array:
CC
CC  tpt_rows(10000)
CC  tpe_rows(10000)
CC
      do 32 i = 1,tptrack_h.nok
      if(tptrack(i).id .le. 0) go to 32
      if(tptrack(i).id .gt. nmax) then
           write(6,1020) tptrack(i).id
           go to 32
      end if
      tpt_rows(tptrack(i).id) = i
32    continue

CC  Add monte carlo matching information to column 4 of local arrays
CC  svt_trk and tpc_trk:
CC
      write(6,103)
      do 35 i = 1,nmax
      if(svt_trk(i,2) .eq. 0) go to 35

      do 36 j = 1,nmax
      if(tpc_trk(j,2) .eq. 0) go to 36
           if(svt_trk(i,3) .eq. tpc_trk(j,3))  then
           svt_trk(i,4) = j
           tpc_trk(j,4) = i
           go to 37
           end if
36    continue
37    continue
35    continue
      write(6,104)

CCC
CCC   Fill output table svm_eval_strk:
CCC

      if(svt_track_h.nok .gt. svm_eval_strk_h.maxlen) then
           write(6,109)
           svm_eval_strk_h.nok = 0
           go to 80
      end if

      tab_count = 0

      do 70 i = 1,svt_track_h.nok
      if(svt_track(i).nspt .lt. 3) go to 70
      if(svt_track(i).id   .le. 0) go to 70

      if(svt_track(i).id   .gt. nmax)  then
           write(6,101) svt_track(i).id
           go to 70
      end if

      index = svt_track(i).id
      if(svt_trk(index,1) .eq. 0) go to 70

      if(svt_trk(index,2) .eq. 0) then

           if(evt_svt(index) .eq. 0) then 
              tab_count = tab_count + 1
              svm_eval_strk(tab_count).id       = index       
              svm_eval_strk(tab_count).status   = 1
              svm_eval_strk(tab_count).flag     = svt_track(i).flag
              svm_eval_strk(tab_count).chisq(1) = svt_track(i).chisq(1)
              svm_eval_strk(tab_count).chisq(2) = svt_track(i).chisq(2)
              svm_eval_strk(tab_count).invpt   = abs(svt_track(i).invpt)
           end if

      else if(svt_trk(index,2) .eq. 1) then

           if(svt_trk(index,4) .eq. 0) then

              if(evt_svt(index) .eq. 0) then
              tab_count = tab_count + 1
              svm_eval_strk(tab_count).id       = index
              svm_eval_strk(tab_count).status   = 2
              svm_eval_strk(tab_count).flag     = svt_track(i).flag
              svm_eval_strk(tab_count).chisq(1) = svt_track(i).chisq(1)
              svm_eval_strk(tab_count).chisq(2) = svt_track(i).chisq(2)
              svm_eval_strk(tab_count).invpt   = abs(svt_track(i).invpt)
              end if

           else if(svt_trk(index,4) .ne. 0) then
                tpcmat = evt_svt(index)
                mctpc  = svt_trk(index,4)

              if(tpcmat .ne. mctpc .and. tpcmat .eq. 0) then
              tab_count = tab_count + 1
              svm_eval_strk(tab_count).id       = index
              svm_eval_strk(tab_count).status   = 3
              svm_eval_strk(tab_count).flag     = svt_track(i).flag
              svm_eval_strk(tab_count).chisq(1) = svt_track(i).chisq(1)
              svm_eval_strk(tab_count).chisq(2) = svt_track(i).chisq(2)
              svm_eval_strk(tab_count).invpt   = abs(svt_track(i).invpt)
              end if

           end if

      end if

70    continue

      svm_eval_strk_h.nok = tab_count

80    continue

      svm_svt_eval = STAFCV_OK

      return
      end

