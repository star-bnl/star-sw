
      INTEGER*4 FUNCTION EPI_EVAL(
     1         g2t_track_h,         g2t_track ,
     2           svt_spt_h,           svt_spt ,
     3        svt_ctrack_h,        svt_ctrack ,
     4         svt_track_h,         svt_track ,
     5           tptrack_h,           tptrack ,
     6            tpeval_h,            tpeval ,
     7            epipar_h,            epipar ,
     8           globtrk_h,           globtrk ,
     9          hypo_pid_h,          hypo_pid ,
     9        global_pid_h,        global_pid ,
     9     glob_pid_eval_h,     glob_pid_eval ) 

C>**************************************************************************
C
C          Module  epi_eval:  Event level global Particle Identification
C                             Evaluation Module
C
C     Description:
C
C          This module evaluates the performance of the global PID
C     module with respect to its accuracy for good monte carlo tracks.
C     It also provides some further evaluation of the tracking and matching
C     performance.
C          One row in the output table, glob_pid_eval is filled for
C     every valid row in table global_pid.
C
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
C         June 29, 1994  -  Initially written
C
C         July 1, 1994    -  Installed in TAS library, under pkg epi
C
C
C     Arguments:
C           TAS tables:  g2t_track                      :  READ
C                        svt_spt                    :  READ
C                        svt_ctrack                 :  READ
C                        svt_track                  :  READ
C                        tptrack                    :  READ
C                        tpeval                     :  READ
C                        epipar                     :  READ
C                        globtrk                    :  READ
C                        hypo_pid                   :  READ
C                        global_pid                 :  READ
C                        glob_pid_eval              :  WRITE
C
C ------------------------------------------------------------------------
C
C     Table:  epipar
C     Lib:    epi
C     Description:  Control parameters for global PID module epiam
C
C     Variable         Type Dimension  Comment
C     --------         ---- ---------  -------
C     ctrl_sharp         I             =0 no filtering, use all detectors
C                                      =1 throw out least sharp detector
C                                         prob. distribution, use rest
C                                      =2 use only the sharpest detector
C                                         prob. dist. for PID.
C
C     ctrl_wt            I             =0 use equal detector weight factors
C                                      =1 use input detec. weights from table
C                                         epipar
C                                      =2 use computed detec. weights input
C                                         from table hypo_pid
C                                      =3 use 1.0/sigma of probability dist.
C                                         of each detector
C                                      =4 use sharpness of prob. dist. of
C                                         each detector.
C                                      =5 use no weights, compute total prob.
C                                         dist. from the product of each detec.
C                                         prob. dist.
C
C     ctrl_conflict      I             =0 ignore conflicts, use total prob.
C                                         distribution maximum for PID.
C                                      =1 if conflict exists between sharp and
C                                         sufficiently weighted detec. prob.
C                                         dist. and the total prob. dist. then
C                                         use the detec. PID, the order of
C                                         preference being svt, tof, tpc.
C
C     w_svt              R             Relative svt  detec. weight (optional)
C     w_tpc              R             Relative tpc  detec. weight (optional)
C     w_tof              R             Relative tof  detec. weight (optional)
C     w_emc              R             Relative emc  detec. weight (optional)
C     w_xtpc             R             Relative xtpc  detec. weight (optional)
C
C     nsig_max           R             #sigma cut-off limit for each detec.
C                                       specific PID; i.e., for a given detector
C                                       PID information to be used the #sigma
C                                       for at least one mass hypothesis must
C                                       be .le. this quantity.
C
C     weight_min         R             used in conflict resolution; for
C                                       specific detector to override the total
C                                       PID assignment its (relative weight/
C                                       average detector weight) must be .gt.
C                                       this quantity.
C
C     sharp_min          R             minimum required sharpness for individ-
C                                       ual detector in conflict resolution.
C                                       Also used as a minimum sharpness cut-off
C                                       for the total probability distribution.
C
C     qfact_min          R              minimum fraction of tpc track from
C                                       same MC track to be considered "good";
C                                       same usage as in svm_eval module and
C                                       table svm_eval_par.qmin
C
C
C-----------------------------------------------------------------------------
C
C     Table:   glob_pid_eval (10000) ; row-by-row correspondence with global_pid
C     Library: epi
C
C     Variables          Type                       Description
C
C        id               I            primary key = global_pid.id
C      svt_seg            I            indicates if global track has svt segment
C                                      = 0, no svt segment
C                                      = 1, has svt segment and MC track ok.
C                                      = -1, has svt segment but track not MC ok
C      tpc_seg            I            indicates if global track has tpc segment
C                                      = 0, no tpc segment
C                                      = 1, has tpc segment and MC track ok
C                                      = -1, has tpc segment but track not MC ok
C      tof_seg            I            indicates if global track has tof hit
C                                      = 0, no tof hit
C                                      = 1, has tof hit
C      mc_check           I            = 0, global track is not a good MC track
C                                      = 1, global track is a good MC track
C                                        (i.e. each svt and/or tpc segment is
C                                         good Monte Carlo piece and if svt &
C                                         tpc segments are matched this is done
C                                         correctly.)
C      mc_pid             I            if mc_check = 1 this is the Geant pid
C                                      code #, otherwise it is set to 0
C       invpt             R            copied from globtrk.invpt
C        pid              I            copied from global_pid.most_prob_id
C        eta              R            pseudorapidity computed
C                                      at point on track where helix parameters
C                                      are given.
C
C------------------------------------------------------------------------------
C
C          Conditions for the Monte Carlo check parameter:
C
C      svt_seg          tpc_seg           resulting mc_check
C
C         0                1                          1
C         0                0                          0
C         0               -1                          0
C         1                1              if match is ok  -->  1
C         1                1              if match not ok -->  0
C         1                0                          1
C         1               -1                          0
C        -1                1                          0
C        -1                0                          0
C        -1               -1                          0
C
C------------------------------------------------------------------------------
C
C          Summary values for global tracking,matching, and PID:
C             These are printed out at the end of module execution
C
C       variable                 description
C
C         nct(1)        # valid rows in global_pid
C         nct(2)        # valid global tracks
C         nct(3)        # valid global tracks with hypo_pid information
C         nct(4)        # valid global tracks with TOF information
C
C         nct(5)        # valid global tracks with no svt segment
C         nct(6)        # valid global tracks with svt segment and MC ok
C         nct(7)        # valid global tracks with svt segment but MC not ok
C
C         nct(8)        # valid global tracks with no tpc segment
C         nct(9)        # valid global tracks with tpc segment and MC ok
C         nct(10)       # valid global tracks with tpc segment but MC not ok
C
C         nct(11)       # valid global tracks with good svt & tpc, matched
C         nct(12)       # valid global tracks with good svt & tpc, matched cor.
C         nct(13)       # valid global tracks with good svt & tpc, mat. incor
C
C         nct(14)       # bad global tracks -- bad MC segments or bad match
C
C         nct(15)       # valid global tracks with final MC check ok
C         nct(16)       # valid global tracks with final MC check not ok
C
C         nct(17)       # global tracks with MC ok, pid assignment correct
C         nct(18)       # global tracks with MC ok, pid assignment incorrect
C         nct(19)       # global tracks with MC ok, pid not assigned
C         nct(20)       # global tracks with MC not ok, pid assigned
C         nct(21)       # global tracks with MC not ok, pid not assigned
C
C         nct(22-26)    repeat preceding 5 values for global tracks with TOF hit
C
C         nct(27-31)    repeat prec. 5 values for global tracks with no TOF hit
C
C         nct(32-36)    repeat prec. 5 values for global tracks with svt segment
C
C         nct(37-41)    repeat prec. 5 values for global tracks with no svt seg
C
C         nct(42-46)    repeat prec. 5 values for glb. trk. with no svt, no TOF
C
C         nct(47-51)    repeat prec. 5 values for glb. trk. with svt & TOF
C
C>****************************************************************************

      implicit none

C   Argument Declarations, STAF include :
C

#include "epi_eval.inc"

C     Local Variable Type Declarations:
C

      integer  hypo_last(10000), hypo_prev(100000), nmax, nmax2, nmax3
      integer  globtrk_row(10000),  svttrk_row(10000)
      integer  svtctrk_row(10000), svtspt_row(15000)
      integer  tpctrk_row(10000),   tpeval_row(10000)

      integer  i,j,k,irow,i1,i2,i3,i4,i5,jctr,jspt(6),jsave
      integer  row1,row2,row3,row4,row5,row6,row7,row8
      integer  svt_mc_save, nct(60), mck, mcp, pid
      real     tan_ang, ang, theta, pi

      parameter (nmax = 10000)
      parameter (nmax2 = 100000)
      parameter (nmax3 = 15000)
      parameter (pi = 3.141592654)

CC
CC   Message Formats:
CC

100   format(5x,'Begin global PID evaluation - module epi_eval')
101   format(5x,'Primary key in row # ',i8,' of hypo_pid is ',
     1       i10,' which is .gt. 10000 - skip and go on')
102   format(5x,'Foreign key in row # ',i8,' of globtrk is ',
     1       i10,' which is .gt. 10000 - skip and go on')
103   format(5x,'Foreign key in row # ',i8,' of svt_track is ',
     1       i10,' which is .gt. 10000 - skip and go on')
104   format(5x,'Primary key in row # ',i8,' of svt_ctrack is ',
     1       i10,' which is .gt. 10000 - skip and go on')
105   format(5x,'Primary key in row # ',i8,' of svt_spt is ',
     1       i10,' which is .gt. 15000 - skip and go on')
106   format(5x,'Foreign key in row # ',i8,' of tptrack is ',
     1       i10,' which is .gt. 10000 - skip and go on')
107   format(5x,'Foreign key in row # ',i8,' of tpeval is ',
     1       i10,' which is .gt. 10000 - skip and go on')
108   format(5x,'Primary key in row # ',i8,' of global_pid is ',
     1       i10,' which is out of range-skip and go on')
109   format(5x,'Foreign key in row # ',i8,' of globtrk is ',
     1       i10,' which is out of range-skip and go on')
110   format(5x,'Primary key in row # ',i8,' of globtrk is ',
     1       i10,' which is out of range-skip and go on')
111   format(5x,'Primary key in row # ',i8,' of svt_track is ',
     1       i10,' which is out of range-skip and go on')
112   format(5x,'No valid sp.pt. keys found for svt_track row # ',i8,
     1       ' and svt_ctrack row # ',i8)
113   format(5x,'1 valid sp.pt. key found for svt_track row # ',i8,
     1       ' and svt_ctrack row # ',i8)
114   format(5x,'Primary key in row # ',i8,' of tptrack is ',
     1       i10,' which is out of range-skip and go on')
ccc
ccc    Formats for accumulated sums for overall PID performance evaluation
ccc

120   format(5x,'# global tracks: counters 1-4                = ',4i6)
121   format(5x,'# global tracks - with svt segment           = ',3i6)
122   format(5x,'# global tracks - with tpc segment           = ',3i6)
123   format(5x,'# global tracks - matched                    = ',3i6)
124   format(5x,'# bad global tracks                          = ', i6)
125   format(5x,'# glb trks: MC OK, and not OK                = ',2i6)
126   format(5x,'# glb trks: MC and PID checks -   Full       = ',5i6)
127   format(5x,'# glb trks: MC and PID checks -   with TOF   = ',5i6)
128   format(5x,'# glb trks: MC and PID checks -   no TOF     = ',5i6)
129   format(5x,'# glb trks: MC and PID checks -   with svt   = ',5i6)
130   format(5x,'# glb trks: MC and PID checks -   no svt     = ',5i6)
131   format(5x,'# glb trks: MC and PID checks -   no svt,tof = ',5i6)
132   format(5x,'# glb trks: MC and PID checks - with svt,tof = ',5i6)

CCC
CCC   Executable code:
CCC

CCC   Set Arrays to zero
CCC

      do i = 1,nmax
      hypo_last(i) = 0
      globtrk_row(i) = 0
      svttrk_row(i) = 0
      svtctrk_row(i) = 0
      tpctrk_row(i) = 0
      tpeval_row(i) = 0
      end do
      do i = 1,nmax2
      hypo_prev(i) = 0
      end do
      do i = 1,nmax3
      svtspt_row(i) = 0
      end do
      do i = 1,60
      nct(i) = 0
      end do

cc
cc
      write(6,100)
cc
cc   Build pointer arrays:
cc
cc   Build local arrays containing the last row value in hypo_pid for a
cc   given value of the primary key hypo_pid.id and the previous row
cc   using sequential local pointers.
cc
cc   Define local arrays:
cc
cc         hypo_last(10000)
cc         hypo_prev(100000)
cc

      do 10 i = 1,hypo_pid_h.nok
      if(hypo_pid(i).det .eq. 0) go to 10
      if(hypo_pid(i).id  .le. 0) go to 10
      if(hypo_pid(i).id  .gt. nmax) then
           write(6,101) i, hypo_pid(i).id
           go to 10
      end if
      irow = hypo_pid(i).id
      hypo_prev(i) = hypo_last(irow)
      hypo_last(irow) = i
10    continue

ccc   Build pointer array for globtrk:
ccc
      do 20 i = 1,globtrk_h.nok
      if(globtrk(i).invpt .eq. 0.0) go to 20
      if(globtrk(i).id    .eq.  0) go to 20
      if(globtrk(i).id_global_pid .le. 0)  go to 20
      if(globtrk(i).id_global_pid .gt. nmax) then
           write(6,102) i,globtrk(i).id_global_pid
           go to 20
      end if
      globtrk_row(globtrk(i).id_global_pid) = i
20    continue

ccc   Build pointer array for svt_track:
ccc
      do 30 i = 1,svt_track_h.nok
      if(svt_track(i).id_globtrk .le. 0)  go to 30
      if(svt_track(i).id_globtrk .gt. nmax) then
           write(6,103) i,svt_track(i).id_globtrk
           go to 30
      end if
      svttrk_row(svt_track(i).id_globtrk) = i
30    continue

ccc   Build pointer array for svt_ctrack:
ccc
      do 40 i = 1,svt_ctrack_h.nok
      if(svt_ctrack(i).id .le. 0)  go to 40
      if(svt_ctrack(i).id .gt. nmax) then
           write(6,104) i,svt_ctrack(i).id
           go to 40
      end if
      svtctrk_row(svt_ctrack(i).id) = i
40    continue

ccc   Build pointer array for svt_spt:
ccc
      do 50 i = 1,svt_spt_h.nok
      if(svt_spt(i).id .le. 0)  go to 50
      if(svt_spt(i).id .gt. nmax3) then
           write(6,105) i,svt_spt(i).id
           go to 50
      end if
      svtspt_row(svt_spt(i).id) = i
50    continue

ccc   Build pointer array for tptrack:
ccc
      do 60 i = 1,tptrack_h.nok
      if(tptrack(i).id_globtrk .le. 0)  go to 60
      if(tptrack(i).id_globtrk .gt. nmax) then
           write(6,106) i,tptrack(i).id_globtrk
           go to 60
      end if
      tpctrk_row(tptrack(i).id_globtrk) = i
60    continue

ccc   Build pointer array for tpeval:
ccc
      do 70 i = 1,tpeval_h.nok
      if(tpeval(i).rtrk .le. 0)  go to 70
      if(tpeval(i).rtrk .gt. nmax) then
           write(6,107) i,tpeval(i).rtrk
           go to 70
      end if
      tpeval_row(tpeval(i).rtrk) = i
70    continue

ccc   For table g2t_track I take advantage of the fact that g2t_track().id = row #
ccc   and avoid having to construct a HUGE pointer array
ccc

CCC
CCC ***********************************************************************
CCC *                                                                     *
CCC *     Begin Loop Thru global_pid table                                *
CCC *                                                                     *
CCC ***********************************************************************

      do 200 i = 1,global_pid_h.nok
ccc
ccc   Initialize the i-th row in glob_pid_eval to zero
ccc
      glob_pid_eval(i).id       = 0
      glob_pid_eval(i).svt_seg  = 0
      glob_pid_eval(i).tpc_seg  = 0
      glob_pid_eval(i).tof_seg  = 0
      glob_pid_eval(i).mc_check = 0
      glob_pid_eval(i).mc_pid   = 0
      glob_pid_eval(i).invpt    = 0.0
      glob_pid_eval(i).pid      = 0
      glob_pid_eval(i).eta      = 0.0

      if(global_pid(i).id .le. 0 .or. global_pid(i).id .gt. nmax) then
           write(6,108) i,global_pid(i).id
           go to 200
      end if

      glob_pid_eval(i).id = global_pid(i).id
      nct(1) = nct(1) + 1
ccc
ccc   Find row in globtrk  -- row1
ccc
      row1 = globtrk_row(global_pid(i).id)
      if(row1 .eq. 0) go to 200
      if(globtrk(row1).id .le. 0 .or. globtrk(row1).id .gt. nmax) then
           write(6,110) row1, globtrk(row1).id
           go to 200
      end if
ccc
ccc   Determine if this global track has TOF PID information:
ccc
      nct(2) = nct(2) + 1
      i1 = globtrk(row1).id_hypo_pid
      if(i1 .le. 0 .or. i1 .gt. nmax) then
c          write(6,109) row1, i1
           go to 200
      end if
      i2 = hypo_last(i1)
      if(i2 .eq. 0) go to 200
201   continue
      if(hypo_pid(i2).det .eq. 3) glob_pid_eval(i).tof_seg = 1
      i3 = hypo_prev(i2)
      if(i3 .eq. 0) go to 202
      i2 = i3
      go to 201
202   continue
      nct(3) = nct(3) + 1
      if(glob_pid_eval(i).tof_seg .eq. 1) nct(4) = nct(4) + 1
ccc
ccc   Find svt track segment if any:
ccc
      row2 = svttrk_row(globtrk(row1).id)
      if(row2 .eq. 0) then
           glob_pid_eval(i).svt_seg = 0
      else
           i4 = svt_track(row2).id
           if(i4 .le. 0 .or. i4 .gt. nmax) then
                write(6,111) row2, i4
                go to 200
           end if
           row3 = svtctrk_row(i4)
ccc
ccc  Collect space point keys, put in local array jspt:
ccc
           do j = 1,6
           jspt(j) = 0
           end do
           jctr = 0
           do j = 1,6
           jsave = svt_ctrack(row3).spt(j)
           if(jsave .gt. 0 .and. jsave .le. nmax3) then
                jctr = jctr + 1
                jspt(jctr) = jsave
           end if
           end do

           if(jctr .eq. 0) then
                write(6,112) row2, row3
                glob_pid_eval(i).svt_seg = -1
           else if(jctr .eq. 1) then
                write(6,113) row2, row3
                glob_pid_eval(i).svt_seg = -1
           else if(jctr .ge. 2) then
                svt_mc_save = 1
                row4 = svtspt_row(jspt(1))
                if(row4 .eq. 0) svt_mc_save = 0
                do j = 2,jctr
                row5 = svtspt_row(jspt(j))
                if(row5 .eq. 0) svt_mc_save = 0
                if(svt_spt(row4).id_mctrack .ne. 
     &             svt_spt(row5).id_mctrack)  svt_mc_save = 0
                end do
                if(svt_mc_save .eq. 1) glob_pid_eval(i).svt_seg = 1
                if(svt_mc_save .eq. 0) glob_pid_eval(i).svt_seg = -1
           end if
      end if

      if(glob_pid_eval(i).svt_seg .eq. 0) nct(5) = nct(5) + 1
      if(glob_pid_eval(i).svt_seg .eq. 1) nct(6) = nct(6) + 1
      if(glob_pid_eval(i).svt_seg .eq.-1) nct(7) = nct(7) + 1

ccc
ccc  Find tpc track segment if any:
ccc
ccc Note: The value of globtrk(row1).id has already been checked by this point
ccc
CCC  Change the following line in order to accommodate the
CCC  tptrack.id_globtrk pointer as is currently used (this points to
CCC  the row number in globtrk) -- L. Ray, Oct. 1994
CCC
CCC   row6 = tpctrk_row(globtrk(row1).id)
      row6 = tpctrk_row(row1)
      if(row6 .eq. 0) then
           glob_pid_eval(i).tpc_seg = 0
      else
           i5 = tptrack(row6).id
           if(i5 .le. 0 .or. i5 .gt. nmax) then
                write(6,114) row6, i5
                go to 200
           end if
           row7 = tpeval_row(i5)
           if(row7 .eq. 0) then
                glob_pid_eval(i).tpc_seg = -1
           else
                if(tpeval(row7).qfact .ge. epipar(1).qfact_min) then
                     glob_pid_eval(i).tpc_seg = 1
                else
                     glob_pid_eval(i).tpc_seg = -1
                end if
           end if
      end if

      if(glob_pid_eval(i).tpc_seg .eq. 0) nct(8) = nct(8) + 1
      if(glob_pid_eval(i).tpc_seg .eq. 1) nct(9) = nct(9) + 1
      if(glob_pid_eval(i).tpc_seg .eq.-1) nct(10)= nct(10)+ 1

ccc
ccc  Evaluate matched, good MC svt and tpc tracks:
ccc

      if(glob_pid_eval(i).svt_seg .eq. 1 .and.
     &   glob_pid_eval(i).tpc_seg .eq. 1) then
           nct(11) = nct(11) + 1
           if(svt_spt(row4).id_mctrack .eq. tpeval(row7).mtrk) then
                nct(12) = nct(12) + 1
                glob_pid_eval(i).mc_check = 1
                row8 = svt_spt(row4).id_mctrack
                glob_pid_eval(i).mc_pid = g2t_track(row8).ge_pid
           else
                nct(13) = nct(13) + 1
                glob_pid_eval(i).mc_check = 0
                glob_pid_eval(i).mc_pid   = 0
           end if
      else if(glob_pid_eval(i).svt_seg .eq. 0 .and.
     &         glob_pid_eval(i).tpc_seg .eq. 1) then
           glob_pid_eval(i).mc_check = 1
           row8 = tpeval(row7).mtrk
           glob_pid_eval(i).mc_pid = g2t_track(row8).ge_pid
      else if(glob_pid_eval(i).svt_seg .eq. 1 .and.
     &         glob_pid_eval(i).tpc_seg .eq. 0) then
           glob_pid_eval(i).mc_check = 1
           row8 = svt_spt(row4).id_mctrack
           glob_pid_eval(i).mc_pid = g2t_track(row8).ge_pid
      else
           glob_pid_eval(i).mc_check = 0
           glob_pid_eval(i).mc_pid   = 0
           nct(14) = nct(14) + 1
      end if
      if(glob_pid_eval(i).mc_check .eq. 1) nct(15) = nct(15) + 1
      if(glob_pid_eval(i).mc_check .eq. 0) nct(16) = nct(16) + 1

ccc
ccc   Fill remainder of variables in table glob_pid_eval:
ccc

      glob_pid_eval(i).invpt = globtrk(row1).invpt
      glob_pid_eval(i).pid   = global_pid(i).most_prob_id
      tan_ang = globtrk(row1).tanl
      ang     = atan(tan_ang)
      theta   = pi/2.0 - ang
      glob_pid_eval(i).eta = -log(tan(theta/2.0))

ccc
ccc   Total sums for performance evaluation:
ccc
      mck = glob_pid_eval(i).mc_check
      mcp = glob_pid_eval(i).mc_pid
      pid = glob_pid_eval(i).pid

      k = 16
           if(mck .eq. 1) then
                if(pid .eq. 0) then
                     nct(k+3) = nct(k+3) + 1
                else if(pid .ne. 0) then
                     if(pid .eq. mcp) nct(k+1) = nct(k+1) + 1
                     if(pid .ne. mcp) nct(k+2) = nct(k+2) + 1
                end if
           else if(mck .eq. 0) then
                if(pid .ne. 0) nct(k+4) = nct(k+4) + 1
                if(pid .eq. 0) nct(k+5) = nct(k+5) + 1
           end if

      if(glob_pid_eval(i).tof_seg .eq. 1) then
           k = 21
           if(mck .eq. 1) then
                if(pid .eq. 0) then
                     nct(k+3) = nct(k+3) + 1
                else if(pid .ne. 0) then
                     if(pid .eq. mcp) nct(k+1) = nct(k+1) + 1
                     if(pid .ne. mcp) nct(k+2) = nct(k+2) + 1
                end if
           else if(mck .eq. 0) then
                if(pid .ne. 0) nct(k+4) = nct(k+4) + 1
                if(pid .eq. 0) nct(k+5) = nct(k+5) + 1
           end if
      end if

      if(glob_pid_eval(i).tof_seg .eq. 0) then
           k = 26
           if(mck .eq. 1) then
                if(pid .eq. 0) then
                     nct(k+3) = nct(k+3) + 1
                else if(pid .ne. 0) then
                     if(pid .eq. mcp) nct(k+1) = nct(k+1) + 1
                     if(pid .ne. mcp) nct(k+2) = nct(k+2) + 1
                end if
           else if(mck .eq. 0) then
                if(pid .ne. 0) nct(k+4) = nct(k+4) + 1
                if(pid .eq. 0) nct(k+5) = nct(k+5) + 1
           end if
      end if

      if(glob_pid_eval(i).svt_seg .ne. 0) then
           k = 31
           if(mck .eq. 1) then
                if(pid .eq. 0) then
                     nct(k+3) = nct(k+3) + 1
                else if(pid .ne. 0) then
                     if(pid .eq. mcp) nct(k+1) = nct(k+1) + 1
                     if(pid .ne. mcp) nct(k+2) = nct(k+2) + 1
                end if
           else if(mck .eq. 0) then
                if(pid .ne. 0) nct(k+4) = nct(k+4) + 1
                if(pid .eq. 0) nct(k+5) = nct(k+5) + 1
           end if
      end if

      if(glob_pid_eval(i).svt_seg .eq. 0) then
           k = 36
           if(mck .eq. 1) then
                if(pid .eq. 0) then
                     nct(k+3) = nct(k+3) + 1
                else if(pid .ne. 0) then
                     if(pid .eq. mcp) nct(k+1) = nct(k+1) + 1
                     if(pid .ne. mcp) nct(k+2) = nct(k+2) + 1
                end if
           else if(mck .eq. 0) then
                if(pid .ne. 0) nct(k+4) = nct(k+4) + 1
                if(pid .eq. 0) nct(k+5) = nct(k+5) + 1
           end if
      end if

      if(glob_pid_eval(i).svt_seg .eq. 0 .and.
     &   glob_pid_eval(i).tof_seg .eq. 0) then
           k = 41
           if(mck .eq. 1) then
                if(pid .eq. 0) then
                     nct(k+3) = nct(k+3) + 1
                else if(pid .ne. 0) then
                     if(pid .eq. mcp) nct(k+1) = nct(k+1) + 1
                     if(pid .ne. mcp) nct(k+2) = nct(k+2) + 1
                end if
           else if(mck .eq. 0) then
                if(pid .ne. 0) nct(k+4) = nct(k+4) + 1
                if(pid .eq. 0) nct(k+5) = nct(k+5) + 1
           end if
      end if

      if(glob_pid_eval(i).svt_seg .ne. 0 .and.
     &   glob_pid_eval(i).tof_seg .eq. 1) then
           k = 46
           if(mck .eq. 1) then
                if(pid .eq. 0) then
                     nct(k+3) = nct(k+3) + 1
                else if(pid .ne. 0) then
                     if(pid .eq. mcp) nct(k+1) = nct(k+1) + 1
                     if(pid .ne. mcp) nct(k+2) = nct(k+2) + 1
                end if
           else if(mck .eq. 0) then
                if(pid .ne. 0) nct(k+4) = nct(k+4) + 1
                if(pid .eq. 0) nct(k+5) = nct(k+5) + 1
           end if
      end if

200   continue
ccc
c ************************************************************************
C *                                                                      *
C *              End Loop thru global_pid table                          *
C *                                                                      *
C ************************************************************************

ccc
ccc   Set nok value and tas user code:
ccc
      glob_pid_eval_h.nok  =  global_pid_h.nok

      epi_eval = STAFCV_OK

ccc
ccc   Print out the accumulated sums which evaluate the PID performance:
ccc

      write(6,120) (nct(i),i=1 ,4 )
      write(6,121) (nct(i),i=5 ,7 )
      write(6,122) (nct(i),i=8 ,10)
      write(6,123) (nct(i),i=11,13)
      write(6,124)  nct(14)
      write(6,125) (nct(i),i=15,16)
      write(6,126) (nct(i),i=17,21)
      write(6,127) (nct(i),i=22,26)
      write(6,128) (nct(i),i=27,31)
      write(6,129) (nct(i),i=32,36)
      write(6,130) (nct(i),i=37,41)
      write(6,131) (nct(i),i=42,46)
      write(6,132) (nct(i),i=47,51)

      return
      end
