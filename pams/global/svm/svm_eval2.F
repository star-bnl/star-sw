C**************************************************************************
C
C          Module  svm_eval2:  evaluation module for the vector-vector
C                             matching module svm for svt-to-tpc tracks
C                  Expanded to also fill 2 extra evaluation tables, 
C                      svm_svt_eval and svm_tpc_eval which report the
C                      tracking and matching status and kinematic values
C                      associated with the track.
C
C     Description:
C          The module reads the matching information in table evt_match,
C     the monte carlo tracking and matching information from tables svt_spt,
C     and tpeval, and the tracking parameters from tables svt_ctrack and tptrack
C     and provides the following information in table svm_eval_par
C
C     1.  The number of svt tracks which are not pure segments of a monte
C         carlo track.
C     2.  The number of the tracks in (1) that svm matched.
C     3.  The number of svt tracks which are pure segments of a monte carlo
C         track but that does not match to a tpc track.
C     4.  The number of the tracks in (3) that svm matched.
C     5.  The number of svt tracks which are pure segments of a monte carlo
C         track and which match to a tpc track.
C     6.  The number of svt tracks in (5) that svm matched incorrectly.
C     7.  The number of svt tracks in (5) that svm matched correctly.
C     8.  The number of svt tracks in (5) that svm left unmatched.
C
C     1.  The number of tpc tracks which are not "pure" segments of a monte
C         carlo track ("pure" means fraction of good mc points .gt. min.value)
C     2.  The number of the tracks in (1) that svm matched.
C     3.  The number of tpc tracks which are "pure" segments of a monte carlo
C         track but that does not match to a svt track.
C     4.  The number of the tracks in (3) that svm matched.
C     5.  The number of tpc tracks which are pure segments of a monte carlo
C         track and which match to a svt track.
C     6.  The number of tpc tracks in (5) that svm matched incorrectly.
C     7.  The number of tpc tracks in (5) that svm matched correctly.
C     8.  The number of tpc tracks in (5) that svm left unmatched.
C
C
C     In addition the module updates the matching table evt_match with the
C  following information for each matched pair of svt and tpc tracks:
C
C     1. monte carlo check parameter, mc_check = 1 if match is correct,
C        mc_check = -1 if match is incorrect.
C     2. The inverse p-transverse of the tpc segment of the matched tracks.
C     3. The pseudorapidity of the matched tracks using the tpc segment
C        at the given space point from table tptrack.
C     4. The Geant PID code number of the tpc segment of the matched tracks.
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
C         April 29, 1994  -  Initially written
C
C         June 6, 1994    -  Installed in TAS library, under pkg svm 
C
C         Jan, 1997       -  Converted to STAF Helen Caines
C         Sept, 1997      - Converted to SL97a, updated svt track-point table
C searching, removed stk_ctrack table, added sgr_groups table - H. Caines
C
C     Arguments:
C           TAS tables:  g2t_gepart                            :  READ
C                        svt_spt, sgr_groups, svt_track        :  READ
C                        tptrack, tpeval                       :  READ
C                        evt_match, svm_eval_par               :  UPDATE
C                        svm_svt_eval, svm_tpc_eval            :  WRITE
C
C     New evaluation Tables:
C
C----------------------------------------------------------------------------
C
C     Table:   svm_svt_eval
C
C     Variable      Type                Description
C
C        id          I          svt track id. = svt_ctrack.id = svt_track.id
C      status        I          = 1, bad MC track but not matched
C                                 2, bad MC track and matched
C                                 3, good MC track, no tpc partner,not matched
C                                 4, good MC track, no tpc partner and matched
C                                 5, good MC track, should match but not matched
C                                 6, good MC track, should match, matched corr.
C                                 7, good MC track, should match, matched incorr
C        pid         I          Geant pid from gepart
C        pt          R          transverse momentum in GeV/c from svt_track
C        eta         R          pseudorapidity computed from svt_track
C       ptot         R          total momentum in GeV/c from svt_track
C         y          R          rapidity computed from svt_track
C         e          R          total energy in GeV from svt_track
C        et          R          transverse energy in GeV from svt_track
C         q          I          charge from svt_track
C----------------------------------------------------------------------------
C
C     Table:   tpc_eval
C
C     Variable      Type                Description
C
C        id          I          tpc track id = tpeval.rtrk = tptrack.id
C      status        I          = 1, bad MC track but not matched (bad determin-
C                                    ed by parameter svm_eval_par(1).qmin).
C                                 2, bad MC track and matched
C                                 3, good MC track, no svt partner,not matched
C                                 4, good MC track, no svt partner and matched
C                                 5, good MC track, should match but not matched
C                                 6, good MC track, should match, matched corr.
C                                 7, good MC track, should match, matched incorr
C        pid         I          Geant pid from gepart
C        pt          R          transverse momentum in GeV/c from tptrack
C        eta         R          pseudorapidity computed from tptrack
C       ptot         R          total momentum in GeV/c from tptrack
C         y          R          rapidity computed from tptrack
C         e          R          total energy in GeV from tptrack
C        et          R          transverse energy in GeV from tptrack
C         q          I          charge from tptrack
C----------------------------------------------------------------------------
C****************************************************************************

      integer function svm_eval2(
     &   gepart_h,     gepart,
     &   svt_spt_h,    svt_spt,
     &   svt_groups_h, svt_groups,
     &   svt_track_h,  svt_track,
     &   tptrack_h,    tptrack,
     &   tpeval_h,     tpeval,
     &   evt_match_h,  evt_match,
     &   eval_par_h,  eval_par, 
     &   svt_eval_h,  svt_eval,
     &   tpc_eval_h,  tpc_eval ) 

      implicit none

C   Argument Declarations, TAS include and record statements:
C
#include "svm_eval2.inc"

C     Local Variable Type Declarations:
C

      integer     i, index, j, tpcmat, mctpc, svtmat, mcsvt, nmax, row
      integer     nmax2, k
      integer     tpt_rows(10000), tpe_rows(10000), svtspt_rows(15000)
      integer     svt_trk(10000,4), tpc_trk(10000,4)
      integer     evt_svt(10000), evt_tpc(10000)
      integer     nstrk_bad     ,   nttrk_bad
      integer     nstrk_badm    ,   nttrk_badm
      integer     nstrk_sgl    ,   nttrk_sgl
      integer     nstrk_sglm    ,   nttrk_sglm
      integer     nstrk_good    ,   nttrk_good
      integer     nstrk_good_inc    ,   nttrk_good_inc
      integer     nstrk_good_cor    ,   nttrk_good_cor
      integer     nstrk_good_un     ,   nttrk_good_un
      integer     count
      integer     nttrk_goodp       ,   nttrk_goodp_cor
      real tan_ang, ang, theta, pi
      real tanl, sinl, costh, th, mass, pz, e
      parameter (pi=3.141592654)
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
105   format(5x,'SVT track evaluation done')
106   format(5x,'TPC track evaluation done')
107   format(5x,'Table evt_match updated')
108   format(5x,'svt-spt.id = ',i10,' gt 15000 -- skip and go on')
301   format(5x,i10)
302   format(5x,'nspt .lt. 3 for row ',2i8)
303   format(5x,'nspt count is 0 for row ',i8)
304   format(5x,'no. svt tracks counted = ',i8)
305   format(5x,'svt_trk(i,2) .ne. 0,1 for row ',i8)
306   format(5x,'svt_track.id .le. 0 for ',2i8)
307   format(5x,'svt_track_h.nok = ',i8)

CC   Executable Code:
cc
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

CC  Set all rows in table svt_eval to zero and set nok = 0
CC
      do i = 1,svt_eval_h.maxlen
      svt_eval(i).id     = 0
      svt_eval(i).status = 0
      svt_eval(i).pid    = 0
      svt_eval(i).pt     = 0.0
      svt_eval(i).eta    = 0.0
      svt_eval(i).ptot   = 0.0
      svt_eval(i).y      = 0.0
      svt_eval(i).e      = 0.0
      svt_eval(i).et     = 0.0
      svt_eval(i).q      = 0
      end do
      svt_eval_h.nok = 0   

CC  Set all rows in table tpc_eval to zero and set nok = 0
CC
      do i = 1,tpc_eval_h.maxlen
      tpc_eval(i).id     = 0
      tpc_eval(i).status = 0
      tpc_eval(i).pid    = 0
      tpc_eval(i).pt     = 0.0
      tpc_eval(i).eta    = 0.0
      tpc_eval(i).ptot   = 0.0
      tpc_eval(i).y      = 0.0
      tpc_eval(i).e      = 0.0
      tpc_eval(i).et     = 0.0
      tpc_eval(i).q      = 0
      end do
      tpc_eval_h.nok = 0   

CC
CC  Build svt_spt primary key row array:
CC
      do 70 i = 1,svt_spt_h.nok
      if(svt_spt(i).id .le. 0) go to 70
      if(svt_spt(i).id .gt. nmax2) then
           write(6,108) svt_spt(i).id
           go to 70
      end if
      svtspt_rows(svt_spt(i).id) = i 
70    continue

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
      write(6,307) svt_track_h.nok
      write(6,100)
      do 10 i = 1,svt_track_h.nok
      if(svt_track(i).nspt .lt. 3) then
           write(6,302) i,svt_track(i).nspt
           go to 10
      end if
      if(svt_track(i).id .le. 0)  then
           write(6,306) i, svt_track(i).id
           go to 10
      end if
      if(svt_track(i).id .gt. nmax) then
           write(6,101) svt_track(i).id
           go to 10
      end if
      svt_trk(svt_track(i).id , 1) = svt_track(i).nspt
      if(svt_trk(svt_track(i).id , 1) .eq. 0) then
         write(6,303) i,svt_track(i).nspt
      elseif(svt_trk(svt_track(i).id , 1) .gt. 8) then
         write(6,303) i,svt_track(i).nspt
      endif
      svt_trk(svt_track(i).id,2) = 1
           k = 0
           do 12 j = 1,svt_groups_h.nok
              
              if( svt_groups(j).id1 .eq. svt_track(i).id) then
                 k = k +1
 1               if( k .gt. svt_track(i).nspt) goto 13
                 index = svt_groups(j).id2
                 if(index .le. 0) go to 12
                 if(index .gt. nmax2) go to 12
                 row = svtspt_rows(index)
                 svt_trk(svt_track(i).id,3) = svt_spt(row).id_mctrack
                 go to 13
              endif
 12        continue

 13        continue
           k = 0
           do 14 j = 1,svt_groups_h.nok
              if( svt_groups(j).id1 .eq. svt_track(i).id) then
                 k = k + 1
                 if( k .gt. svt_track(i).nspt) goto 15
                 index = svt_groups(j).id2
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
CCC
CCC   Load svt track information into evaluation table:
CCC
      if(i .le. svt_eval_h.maxlen) then
      svt_eval(i).id = svt_track(i).id
      if(svt_track(i).invpt .ne. 0.0) then
           svt_eval(i).pt = 1.0/abs(svt_track(i).invpt)
           tanl = svt_track(i).tanl
           sinl = tanl/sqrt(1.0 + tanl**2)
           costh = sinl
           th   = acos(costh)
           svt_eval(i).ptot = svt_eval(i).pt * sqrt(1.0+tanl**2)
           svt_eval(i).eta  = -log(tan(th/2.0))
           svt_eval(i).q= svt_track(i).invpt/abs(svt_track(i).invpt)

           if(svt_trk(svt_track(i).id , 2) .eq. 1 .and.
     >        svt_track(i).pid .gt.0 .and.
     >        svt_track(i).pid .le.gepart_h.nok  ) then
c               svt_eval(i).pid = gepart(svt_trk(svt_track(i).id,3)).pid
              svt_eval(i).pid = svt_track(i).pid
              mass = gepart(svt_eval(i).pid).amass 
              e = sqrt(mass**2 + svt_eval(i).ptot**2)
              svt_eval(i).e   = e
              svt_eval(i).et = sqrt(mass**2+svt_eval(i).pt**2)
              pz = svt_eval(i).pt * tanl
              svt_eval(i).y = 0.5*log((e + pz)/(e - pz))
           else
              svt_eval(i).pid = 0
              svt_eval(i).e   = 0.0
              svt_eval(i).et  = 0.0
              svt_eval(i).y   = 0.0
           end if
        else
           svt_eval(i).pt   = 0.0
           svt_eval(i).ptot = 0.0
           svt_eval(i).eta  = 0.0
           svt_eval(i).q    = 0
           svt_eval(i).pid  = 0
           svt_eval(i).e    = 0.0
           svt_eval(i).et   = 0.0
           svt_eval(i).y    = 0.0
        end if 
      end if
 10   continue
      svt_eval_h.nok = svt_track_h.nok
      if(svt_eval_h.nok .gt. svt_eval_h.maxlen)
     &     svt_eval_h.nok = svt_eval_h.maxlen
      
      count = 0    
      do i = 1,nmax
      if(svt_trk(i,1) .ne. 0) count = count + 1
      end do
      write(6,304) count


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
      if(tpeval(i).qfact .ge. eval_par(1).qmin) then
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

CCC
CCC   Load tpc track information into evaluation table:
CCC
      do 33 i = 1,tpeval_h.nok
      if(tpeval(i).rtrk .le. 0 .or. tpeval(i).rtrk .gt. nmax) go to 33
      if(i .le. tpc_eval_h.maxlen) then
      tpc_eval(i).id = tpeval(i).rtrk
      row = tpt_rows(tpeval(i).rtrk)
CCC   'row' is the row # in tptrack
      if(tptrack(row).invp .ne. 0.0) then
*           tpc_eval(i).pt = 1.0/abs(tptrack(row).invp)
            tpc_eval(i).pt = tpeval(i).ptg
           tanl = tptrack(row).tanl
           sinl = tanl/sqrt(1.0 + tanl**2)
           costh = sinl
           th   = acos(costh)
           tpc_eval(i).ptot = tpc_eval(i).pt*sqrt(1.0 + tanl**2)
           tpc_eval(i).eta  = -log(tan(th/2.0))
           tpc_eval(i).q    = tptrack(row).q

           if(tpc_trk(tpeval(i).rtrk , 2) .eq. 1 .and.
     >          tpeval(i).pid .le. gepart_h.nok .and. 
     >          tpeval(i).pid .gt. 0) then
              tpc_eval(i).pid = tpeval(i).pid
              mass = gepart(tpc_eval(i).pid).amass
              e = sqrt(mass**2 + tpc_eval(i).ptot**2)
              tpc_eval(i).e   = e
              tpc_eval(i).et = sqrt(mass**2+tpc_eval(i).pt**2)
              pz = tpc_eval(i).pt * tanl
              tpc_eval(i).y = 0.5*log((e + pz)/(e - pz))
           else
              tpc_eval(i).pid = 0
              tpc_eval(i).e   = 0.0
              tpc_eval(i).et  = 0.0
              tpc_eval(i).y   = 0.0
           end if
      else
           tpc_eval(i).pt   = 0.0
           tpc_eval(i).ptot = 0.0
           tpc_eval(i).eta  = 0.0
           tpc_eval(i).q    = 0
           tpc_eval(i).pid  = 0
           tpc_eval(i).e    = 0.0
           tpc_eval(i).et   = 0.0
           tpc_eval(i).y    = 0.0
      end if 
      end if
33    continue
      tpc_eval_h.nok = tpeval_h.nok
      if(tpc_eval_h.nok .gt. tpc_eval_h.maxlen)
     &   tpc_eval_h.nok = tpc_eval_h.maxlen

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

CC  Inventory the svt tracks and matches, counting the following:
CC
CC (1) nstrk_bad:
CC        The number of svt tracks which are not pure segments of a monte
CC        carlo track.
CC (2) nstrk_badm:
C         The number of the tracks in (1) that svm matched.
CC (3) nstrk_sgl:
C         The number of svt tracks which are pure segments of a monte carlo
C         track but that do not match to tpc tracks.
CC (4) nstrk_sglm:
C         The number of the tracks in (3) that svm matched.
CC (5) nstrk_good:
C         The number of svt tracks which are pure segments of a monte carlo
C         track and which match to a tpc track.
CC (6) nstrk_good_inc:
C         The number of svt tracks in (5) that svm matched incorrectly.
CC (7) nstrk_good_cor:
C         The number of svt tracks in (5) that svm matched correctly.
CC (8) nstrk_good_un:
C         The number of svt tracks in (5) that svm did not match.

      nstrk_bad        = 0
      nstrk_badm       = 0
      nstrk_sgl        = 0
      nstrk_sglm       = 0
      nstrk_good       = 0
      nstrk_good_inc   = 0
      nstrk_good_cor   = 0
      nstrk_good_un    = 0

      count = 0
      do 40 i = 1,nmax
      if(svt_trk(i,1) .eq. 0) go to 40
      count = count + 1
      if(svt_trk(i,2) .ne. 0 .and. svt_trk(i,2) .ne. 1) write(6,305) i
      if(svt_trk(i,2) .eq. 0) then
           nstrk_bad = nstrk_bad + 1
           if(evt_svt(i) .ne. 0) nstrk_badm = nstrk_badm + 1
      else if(svt_trk(i,2) .eq. 1) then
           if(svt_trk(i,4) .eq. 0) then
                nstrk_sgl = nstrk_sgl + 1
                if(evt_svt(i) .ne. 0) nstrk_sglm = nstrk_sglm + 1
           else if(svt_trk(i,4) .ne. 0) then
                nstrk_good = nstrk_good + 1
                tpcmat = evt_svt(i)
                mctpc  = svt_trk(i,4)
                if(tpcmat .eq. mctpc) nstrk_good_cor = nstrk_good_cor +1
                if(tpcmat .ne. mctpc .and. tpcmat .ne. 0)
     &                nstrk_good_inc = nstrk_good_inc +1
                if(tpcmat .ne. mctpc .and. tpcmat .eq. 0)
     &                nstrk_good_un  = nstrk_good_un  + 1
           end if
      end if
40    continue
      write(6,105)
      write(6,304) count
CCC
CCC   Fill status variable in table svt_eval:
CCC
      do 41 j = 1,svt_eval_h.nok
      i = svt_eval(j).id
CCC  'i' is the row index for track info. in svt_trk array
      if(i .le. 0) go to 41
      if(svt_trk(i,1) .eq. 0) go to 41
      if(svt_trk(i,2) .eq. 0) then
           if(evt_svt(i) .ne. 0) then
                svt_eval(j).status = 2
           else
                svt_eval(j).status = 1
           end if
      else if(svt_trk(i,2) .eq. 1) then
           if(svt_trk(i,4) .eq. 0) then
                if(evt_svt(i) .ne. 0) then
                     svt_eval(j).status = 4
                else
                     svt_eval(j).status = 3
                end if
           else if(svt_trk(i,4) .ne. 0) then
                tpcmat = evt_svt(i)
                mctpc  = svt_trk(i,4)
                if(tpcmat .eq. mctpc) svt_eval(j).status = 6
                if(tpcmat .ne. mctpc .and. tpcmat .ne. 0)
     &               svt_eval(j).status = 7
                if(tpcmat .ne. mctpc .and. tpcmat .eq. 0)
     &               svt_eval(j).status = 5
           end if
      end if
41    continue


CC  Inventory the tpc tracks and matches, counting the following:
CC
CC (1) nttrk_bad:
CC        The number of tpc tracks which are not "pure" segments of a monte
C         carlo track ("pure" means fraction of good mc points .gt. min.value)
CC (2) nttrk_badm:
C         The number of the tracks in (1) that svm matched.
CC (3) nttrk_sgl:
C         The number of tpc tracks which are "pure" segments of a monte carlo
C         track but that do not match to svt tracks.
CC (4) nttrk_sglm:
C         The number of the tracks in (3) that svm matched.
CC (5) nttrk_good:
C         The number of tpc tracks which are "pure" segments of a monte carlo
C         track and which match to a svt track.
CC (6) nttrk_good_inc:
C         The number of tpc tracks in (5) that svm matched incorrectly.
CC (7) nttrk_good_cor:
C         The number of tpc tracks in (5) that svm matched correctly.
CC (8) nttrk_good_un:
C         The number of tpc tracks in (5) that svm did not match.
C

      nttrk_bad        = 0
      nttrk_badm       = 0
      nttrk_sgl        = 0
      nttrk_sglm       = 0
      nttrk_good       = 0
      nttrk_good_inc   = 0
      nttrk_good_cor   = 0
      nttrk_good_un    = 0
      nttrk_goodp      = 0
      nttrk_goodp_cor  = 0

      do 50 i = 1,nmax
      if(tpc_trk(i,1) .eq. 0) go to 50
      if(tpc_trk(i,2) .eq. 0) then
           nttrk_bad = nttrk_bad + 1
           if(evt_tpc(i) .ne. 0) nttrk_badm = nttrk_badm + 1
      else if(tpc_trk(i,2) .eq. 1) then
           if(tpc_trk(i,4) .eq. 0) then
                nttrk_sgl = nttrk_sgl + 1
                if(evt_tpc(i) .ne. 0) nttrk_sglm = nttrk_sglm + 1
           else if(tpc_trk(i,4) .ne. 0) then
                nttrk_good = nttrk_good + 1
                if(tptrack(tpt_rows(i)).invp .le. 5.0)
     &               nttrk_goodp = nttrk_goodp + 1
                svtmat = evt_tpc(i)
                mcsvt  = tpc_trk(i,4)
                if(svtmat .eq. mcsvt) then
                     nttrk_good_cor = nttrk_good_cor +1
                     if(tptrack(tpt_rows(i)).invp .le. 5.0)
     &                    nttrk_goodp_cor = nttrk_goodp_cor + 1
                end if
                if(svtmat .ne. mcsvt .and. svtmat .ne. 0)
     &                nttrk_good_inc = nttrk_good_inc +1
                if(svtmat .ne. mcsvt .and. svtmat .eq. 0)
     &                nttrk_good_un  = nttrk_good_un  + 1
           end if
      end if
50    continue

CCC
CCC   Fill status variable in table tpc_eval:
CCC
      do 51 j = 1,tpc_eval_h.nok
      i = tpc_eval(j).id
CCC  'i' is the row index for track info. in tpc_trk array
      if(i .le. 0) go to 51
      if(tpc_trk(i,1) .eq. 0) go to 51
      if(tpc_trk(i,2) .eq. 0) then
           if(evt_tpc(i) .ne. 0) then
                tpc_eval(j).status = 2
           else
                tpc_eval(j).status = 1
           end if
      else if(tpc_trk(i,2) .eq. 1) then
           if(tpc_trk(i,4) .eq. 0) then
                if(evt_tpc(i) .ne. 0) then
                     tpc_eval(j).status = 4
                else
                     tpc_eval(j).status = 3
                end if
           else if(tpc_trk(i,4) .ne. 0) then
                svtmat = evt_tpc(i)
                mcsvt  = tpc_trk(i,4)
                if(svtmat .eq. mcsvt) tpc_eval(j).status = 6
                if(svtmat .ne. mcsvt .and. svtmat .ne. 0)
     &               tpc_eval(j).status = 7
                if(svtmat .ne. mcsvt .and. svtmat .eq. 0)
     &               tpc_eval(j).status = 5
           end if
      end if
51    continue

      write(6,106)

      write(6,201) nstrk_bad      ,  nttrk_bad
      write(6,202) nstrk_badm     ,  nttrk_badm
      write(6,203) nstrk_sgl      ,  nttrk_sgl
      write(6,204) nstrk_sglm     ,  nttrk_sglm
      write(6,205) nstrk_good     ,  nttrk_good
      write(6,206) nstrk_good_inc ,  nttrk_good_inc
      write(6,207) nstrk_good_cor ,  nttrk_good_cor
      write(6,208) nstrk_good_un  ,  nttrk_good_un
      write(6,209) nttrk_goodp
      write(6,210) nttrk_goodp_cor

201   format(5x,'nstrk_bad      ,  nttrk_bad       = ',2i8)
202   format(5x,'nstrk_badm     ,  nttrk_badm      = ',2i8)
203   format(5x,'nstrk_sgl      ,  nttrk_sgl       = ',2i8)
204   format(5x,'nstrk_sglm     ,  nttrk_sglm      = ',2i8)
205   format(5x,'nstrk_good     ,  nttrk_good      = ',2i8)
206   format(5x,'nstrk_good_inc ,  nttrk_good_inc  = ',2i8)
207   format(5x,'nstrk_good_cor ,  nttrk_good_cor  = ',2i8)
208   format(5x,'nstrk_good_un  ,  nttrk_good_un   = ',2i8)
209   format(5x,'                  nttrk_goodp     = ', i8)
210   format(5x,'                  nttrk_goodp_cor = ', i8)
   

CC  Put Summary results in table svm_eval_par
CC

      eval_par(1).nstrk_bad         =  nstrk_bad
      eval_par(1).nstrk_badm        =  nstrk_badm
      eval_par(1).nstrk_sgl         =  nstrk_sgl
      eval_par(1).nstrk_sglm        =  nstrk_sglm
      eval_par(1).nstrk_good        =  nstrk_good
      eval_par(1).nstrk_good_inc    =  nstrk_good_inc
      eval_par(1).nstrk_good_cor    =  nstrk_good_cor
      eval_par(1).nstrk_good_un     =  nstrk_good_un

      eval_par(1).nttrk_bad         =  nttrk_bad
      eval_par(1).nttrk_badm        =  nttrk_badm
      eval_par(1).nttrk_sgl         =  nttrk_sgl
      eval_par(1).nttrk_sglm        =  nttrk_sglm
      eval_par(1).nttrk_good        =  nttrk_good
      eval_par(1).nttrk_good_inc    =  nttrk_good_inc
      eval_par(1).nttrk_good_cor    =  nttrk_good_cor
      eval_par(1).nttrk_good_un     =  nttrk_good_un
CC  Fill matching evaluation summary information into table evt_match:
CC

      do 60 i = 1,evt_match_h.nok
      if(evt_match(i).id .eq. 0) go to 60
      mcsvt   = tpc_trk(evt_match(i).idtpc,4)
      svtmat  = evt_match(i).idsvt
      if(mcsvt .eq. svtmat) evt_match(i).mc_check = 1
      if(mcsvt .ne. svtmat) evt_match(i).mc_check = -1
      evt_match(i).invpt = tptrack(tpt_rows(evt_match(i).idtpc)).invp
      evt_match(i).pid   = tpeval(tpe_rows(evt_match(i).idtpc)).pid
      tan_ang = tptrack(tpt_rows(evt_match(i).idtpc)).tanl
      ang = atan(tan_ang)
      theta = pi/2.0 - ang
      evt_match(i).eta = -log(tan(theta/2.0))
60    continue
      write(6,107)
      svm_eval2 = STAFCV_OK

      return
      end

