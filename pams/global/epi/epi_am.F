
      INTEGER*4 FUNCTION EPI_AM(
     1            epipar_h,            epipar ,
     2           globtrk_h,           globtrk ,
     3          hypo_pid_h,          hypo_pid ,
     4        global_pid_h,        global_pid ) 
C>**************************************************************************
C
C          Module  epi_am:  Event level global Particle Identification
C
C     Description:
C
C          This module reads the detector specific particle identification
C     information from table hypo_pid for each global track in table globtrk
C     and determines, if possible, the most probable particle mass.
C     The module can be used to filter the detector specific particle id
C     information and to resolve conflicts between the pid information
C     from different detectors, this capability is limited however.
C          One row in global_pid is filled for every valid track in globtrk
C     regardless of whether any useful pid information is available.
C     If no pid information exists or if what does exist in hypo_pid does
C     not pass the filters and cut-offs then the most probable Geant id
C     code # is set to 0.
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
C         June 11, 1994  -  Initially written
C
C         June 13, 1994    -  Installed in TAS library, under pkg epi
C
C
C     Arguments:
C           TAS tables:  epipar                     :  READ
C                        globtrk, hypo_pid          :  UPDATE
C                        global_pid                 :  WRITE
C
C ------------------------------------------------------------------------
C
C     Table:  epipar
C     Lib:    epi
C     Description:  Control parameters for global PID module epi_am
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
C
C-----------------------------------------------------------------------------
C
C   Table: hypo_pid(100000)   Zebra code: HPIZ  Type: Dynamic
C     Owner: ray  Created:   769466030 Fri May 20 16:33:50 1994
C     Description: Contains all mass hypotheses for each
C                  global track
C     Variable         Type Dimension  Comment
C     --------         ---- ---------  -------
C     det                I             detector:1-5 for svt,tpc,tof,emc,xtpc
C     gid                I             Geant pid code # for this hypothesis
C     id                 I             identifier for rows corres. to globtrk
C     id_global_pid      I             foreign key to global pid table
C     method             I             report options in detec. specific pid
C     nsigma             R             # std.dev. from mean;(+)higher;(-)lower
C     prob               R             Probability for this mass hypothesis
C     weight             R             calculated weight for this detector &trk
C
C     Last mod.   769466762 Fri May 20 16:46:02 1994
C
C   --------------
C     Last mod. for epi library:   769466762 Fri May 20 16:46:02 1994
C
C-----------------------------------------------------------------------------
C
C     Table:  global_pid
C     Lib:    epi
C     Description: Final PID assignment for global tracks; one row entry
C                  is made for each valid global track in table globtrk
C                  regardless of the quality of the PID information.
C
C     Variable         Type Dimension  Comment
C     --------         ---- ---------  -------
C     id                 I             Unique primary key for each row.
C
C     flag               I             Reports condition of global track's
C                                      PID information, results of filtering,
C                                      and conflict resolution results.
C                                      =0 this global track has id_hypo_pid
C                                         not in the range between 0 and
C                                         nmax = 10000
C                                      =1 this global track has valid
C                                         id_hypo_pid but no rows found in
C                                         table hypo_pid
C                                      =2 none of the detector PID information
C                                         passed the epipar(1).nsig_max filter.
C                                      =3 good PID information found, no
C                                         sharpness filtering done
C                                      =4 good PID information found,
C                                         sharpness filtering done
C                                      =5 invalid detector# encountered
C                                      =6 too many (.gt.10) mass hypotheses
C                                         from one of the detectors
C                                      =7 invalid Geant pid code # found in
C                                         hypo_pid
C                                      =13,14 same as 3,4 but pid conflict
C                                         occurred and was resolved.
C                                      =20 final, total weighted probability
C                                          distribution is not sharp enough;
C                                          pid code # = 0 recorded.
C                                      =21 final, total weighted probability
C                                          distribution is not sharp enough
C                                          but one detector was sharp; this
C                                          detec PID used.
C
C     method             I             Records value of control switches:
C                                      = 100*epipar(1).ctrl_sharp
C                                       + 10*epipar(1).ctrl_wt
C                                       +    epipar(1).ctrl_conflict
C
C     most_prob_id       I             Geant PID code #, only cases filled are
C                                      e+, e-, pi+, pi-, K+, K-, proton, anti-
C                                      proton, deuteron, triton, alpha; for all
C                                      others and for all cases where the module
C                                      is unable to make a good pid assignment
C                                      this is set to zero.  Note that muons
C                                      and pions are not distinguished.
C
C     nhypo              I             total # rows in table hypo_pid for this
C                                      global track whether or not all rows
C                                      used in the final PID assignment.
C
C     quality            R             sharpness of the final probability dist.
C                                      or if conflict resolution used this
C                                      is set equal to (-)sharpness of the
C                                      detector PID used to resolve the conflict
C
C<****************************************************************************

      implicit none

C   Argument Declarations, TAS include and record statements:
C
#include "epi_am.inc"

C     Local Variable Type Declarations:
C

      integer  hypo_last(10000), hypo_prev(100000), nmax, nmax2
      integer  i,j,k,irow,i1,i2,i3,flag
      integer  nhypo_det(5), gpid(5,10), idet
      integer  n_active, sharp_det, max_col(6), iconf, det_use
      integer  code, nhypo_tot, globpid_ktr, gflag, n_sharp, n_sharp_j

      real     prob(5,10), nsigma(5,10), weight(5,10), sharp(5)
      real     std_dev(5), mean(5), sum, nsmin, smax, smin, wt(5)
      real     wt_prob(9), wt_sum, pmax, pnext, sharp_tot, wt_avg

      parameter (nmax = 10000)
      parameter (nmax2 = 100000)

CC
CC   Message Formats:
CC

100   format(5x,'Begin global PID assignment - module epi')
101   format(5x,'Primary key in row # ',i8,' of hypo_pid is ',
     1       i10,' which is .gt. 10000 - skip and go on')
102   format(5x,'Foreign key in row # ',i8,' of globtrk is ',
     1       i10,' which is .le. 0 - skip and go on')
103   format(5x,'For global track row #, id = ',2i8,' non-standard',
     1       ' pid type in hypo_pid -- do not use, go on')
104   format(5x,'For global track row # ',i8,' and hypo_pid row#',
     1      i8,' Bad detector #')
105   format(5x,'For global track row # ',i8,' more than 10 mass ',
     1      'hypotheses for detector ',i3)
106   format(5x,'For global track row # ',i8,' and hypo_pid row#',
     1      i8,' Bad Geant id code #')
107   format(5x,'Foreign key in row # ',i8,' of globtrk is ',
     1       i10,' which is .gt. 10000 - skip and go on')

CCC
CCC   Executable code:
CCC

CCC   Set Arrays to zero
CCC

      do i = 1,nmax
      hypo_last(i) = 0
      end do
      do i = 1,nmax2
      hypo_prev(i) = 0
      end do
      do i = 1,5
      nhypo_det(i) = 0
      sharp(i) = 0.0
      mean(i)  = 0.0
      std_dev(i) = 0.0
      wt(i)    = 0.0
           do j = 1,10
           gpid(i,j) = 0
           prob(i,j) = 0.0
           nsigma(i,j) = 0.0
           weight(i,j) = 0.0
           end do
      end do
      do i = 1,6
      max_col(i) = 0
      end do
      do i = 1,9
      wt_prob(i) = 0.0
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

cc
cc   Begin loop over all global tracks in table globtrk:
cc

      globpid_ktr = 0
      do 20 i = 1,globtrk_h.nok
      if(globtrk(i).invpt .eq. 0.0) go to 20
      if(globtrk(i).id    .eq.   0) go to 20
      globpid_ktr = globpid_ktr + 1
      global_pid(globpid_ktr).id = globpid_ktr
      globtrk(i).id_global_pid = global_pid(globpid_ktr).id
      global_pid(globpid_ktr).method = 100*epipar(1).ctrl_sharp
     &     + 10*epipar(1).ctrl_wt + epipar(1).ctrl_conflict
      i1 = globtrk(i).id_hypo_pid
      if(i1 .le. 0) then
c          write(6,102) i, i1
           gflag = 0
           go to 94
      end if
      if(i1 .gt. nmax) then
           write(6,107) i, i1
           gflag = 0
           go to 94
      end if

ccc   Fill local variables for this global track:
ccc
ccc      nhypo_det(5) = # hypotheses for each detector 1,2,3,4,5
ccc      gpid(5,10)   = Geant pid code # for each detector 1-5, for up
ccc                     to 10 mass hypotheses
ccc      prob(5,10)   = probability for each detector and mass hypothesis
ccc      nsigma(5,10) = # sigmas (+/-) for each detector and mass hypothesis
ccc      weight(5,10) = computed relative weight for each detector and
ccc                     mass hypothesis
ccc

      do j = 1,5
      nhypo_det(j) = 0
           do k = 1,10
           gpid(j,k)    = 0
           prob(j,k)    = 0.0
           nsigma(j,k)  = 0.0
           weight(j,k)  = 0.0
           end do
      end do

      i2 = hypo_last(i1)
      if(i2 .eq. 0) then
           gflag = 1
           nhypo_tot = 0
           go to 94
      end if
21    continue
      idet = hypo_pid(i2).det
           if(idet .lt. 1 .or. idet .gt. 5) then
           write(6,104) i, i2
           gflag = 5
           nhypo_tot = 0
           go to 94
           end if
      nhypo_det(idet) = nhypo_det(idet) + 1
           if(nhypo_det(idet) .gt. 10) then
           write(6,105) i, idet
           gflag = 6
           nhypo_tot = 0
           hypo_pid(i2).id_global_pid = 0
           go to 94
           end if
      gpid(idet,nhypo_det(idet)) = hypo_pid(i2).gid
           if(hypo_pid(i2).gid .lt. 1 .or. hypo_pid(i2).gid .gt.48) then
           write(6,106) i, i2
           gflag = 7
           nhypo_tot = 0
           go to 94
           end if
      prob(idet,nhypo_det(idet)) = hypo_pid(i2).prob
      nsigma(idet,nhypo_det(idet)) = hypo_pid(i2).nsigma
      weight(idet,nhypo_det(idet)) = hypo_pid(i2).weight
ccc
ccc   Set foreign key in hypo_pid to the global_pid table:
ccc
      hypo_pid(i2).id_global_pid = global_pid(globpid_ktr).id
23    continue

      i3 = hypo_prev(i2)
      if(i3 .eq. 0) go to 22
      i2 = i3
      go to 21
22    continue
      nhypo_tot = nhypo_det(1) + nhypo_det(2) + nhypo_det(3)
     &          + nhypo_det(4) + nhypo_det(5)
      if(nhypo_tot .eq. 0) then
           gflag = 1
           go to 94
      end if

ccc
ccc   At this point all PID hypothesis information for this global track
ccc   has been loaded into local arrays; nhypo_det, gpid, prob, nsigma,
ccc   and weight.
ccc
ccc   Next rearrange these local arrays into standard order --
ccc       see Subroutine pid_order:
ccc

      call pid_order(nhypo_det,gpid,prob,nsigma,weight,flag)
      if(flag .eq. 1) write(6,103) i, globtrk(i).id

ccc   Normalize probabilities:
ccc

      do 30 j = 1,5
      sum = 0.0
           do 31 k = 1,9
           sum = sum + abs(prob(j,k))
31         continue
      if(nhypo_det(j) .gt. 0 .and. sum .gt. 0.0) then
           do 32 k = 1,9
           prob(j,k) = abs(prob(j,k))/sum
32         continue
      end if
30    continue

ccc  Filter out hypotheses for which the # sigmas are always .gt.
ccc       epipar(1).nsig_max
ccc  Use abs(nsigma) since negative values allowed
ccc

      do 40 j = 1,5
      if(nhypo_det(j) .eq. 0)  go to 40
ccc
ccc   Find smallest # sigma for each pid that has non-zero probability
ccc
      nsmin = 1000.0
           do 41 k = 1,9
           if(prob(j,k) .gt. 0.0) then
                if(abs(nsigma(j,k)).lt. nsmin)
     1            nsmin = abs(nsigma(j,k))
           end if
41         continue
      if(nsmin .gt. epipar(1).nsig_max) then
           nhypo_det(j) = 0
           do 42 k = 1,10
           prob(j,k) = 0.0
           nsigma(j,k) = 0.0
           weight(j,k) = 0.0
42         continue
      end if
40    continue

ccc   Count remaining # contributing detectors
ccc
      n_active = 0
      do 50 j = 1,5
      if(nhypo_det(j) .gt. 0) n_active = n_active + 1
50    continue
      if(n_active .eq. 0) then
           gflag = 2
           go to 94
      end if

ccc   Compute sharpness, mean and std. dev. for each remaining probability
ccc   distribution
ccc

      call stat(prob,sharp,mean,std_dev,nhypo_det)

ccc   Filter detector hypotheses based on sharpness of probability distri-
ccc   butions and control switch epipar(1).ctrl_sharp
ccc
      gflag = 3
      if(epipar(1).ctrl_sharp  .eq. 0) go to 51
      if(n_active           .le. 1) go to 51
      gflag = 4
      if(epipar(1).ctrl_sharp  .eq. 1) then
           smin = 1.0e+20
           do 52 j = 1,5
           if(sharp(j) .lt. smin) then
                sharp_det = j
                smin = sharp(j)
           end if
52         continue
      nhypo_det(sharp_det) = 0
      sharp(sharp_det)  = 0.0
      mean(sharp_det)   = 0.0
      std_dev(sharp_det) = 0.0
      do 53 j = 1,10
      prob(sharp_det,j) = 0.0
      nsigma(sharp_det,j) = 0.0
      weight(sharp_det,j) = 0.0
53    continue
      n_active = n_active - 1

      else if(epipar(1).ctrl_sharp .eq. 2) then
           smax = 0.0
           do 54 j = 1,5
           if(sharp(j) .gt. smax) then
                sharp_det = j
                smax = sharp(j)
           end if
54         continue
           do 55 j = 1,5
           if(j .eq. sharp_det) go to 55
           nhypo_det(j) = 0
           sharp(j)     = 0.0
           mean(j)      = 0.0
           std_dev(j)   = 0.0
                do 56 k = 1,10
                prob(j,k) = 0.0
                nsigma(j,k) = 0.0
                weight(j,k) = 0.0
56              continue
55         continue
           n_active = 1

      end if

51    continue

ccc   Load detector weights:
ccc

      if(epipar(1).ctrl_wt .eq. 0) then
           do 60 j = 1,5
           wt(j) = 1.0
60         continue
      else if(epipar(1).ctrl_wt  .eq. 1) then
           wt(1) = abs(epipar(1).w_svt )
           wt(2) = abs(epipar(1).w_tpc )
           wt(3) = abs(epipar(1).w_tof )
           wt(4) = abs(epipar(1).w_emc )
           wt(5) = abs(epipar(1).w_xtpc)
      else if(epipar(1).ctrl_wt .eq. 2) then
           do 61 j = 1,5
           wt(j) = 0.0
           if(nhypo_det(j) .eq. 0) go to 61
                do 62 k = 1,9
                wt(j) = wt(j) + abs(weight(j,k))
62              continue
           wt(j) = wt(j)/float(nhypo_det(j))
61         continue

      else if(epipar(1).ctrl_wt .eq. 3) then
           do 63 j = 1,5
           wt(j) = 0.0
           if(std_dev(j) .ne. 0.0) wt(j) = 1.0/std_dev(j)
63         continue

      else if(epipar(1).ctrl_wt .eq. 4) then
           do 64 j = 1,5
           wt(j) = 0.0
           if(sharp(j) .ne. 0.0) wt(j) = sharp(j)
64         continue

      else if(epipar(1).ctrl_wt .eq. 5) then
           do 65 j = 1,5
           wt(j) = 1.0
65         continue

      end if

ccc   store sum of detector weights
ccc
      wt_sum = wt(1) + wt(2) + wt(3) + wt(4) + wt(5)

ccc  Store average weight for all active detectors
ccc
      wt_avg = wt_sum/float(n_active)

ccc   Compute weighted/final total probabilities:
ccc

      if(epipar(1).ctrl_wt .eq. 5) then
           do 70 j = 1,9
           wt_prob(j) = 1.0
70         continue
           do 71 j = 1,5
           if(nhypo_det(j) .eq. 0) go to 71
                do 72 k = 1,9
                wt_prob(k) = wt_prob(k)*prob(j,k)
72              continue
71         continue

      else

           do 73 k = 1,9
           wt_prob(k) = 0.0
                do 74 j = 1,5
                if(nhypo_det(j) .eq. 0) go to 74
                wt_prob(k) = wt_prob(k) + wt(j)*prob(j,k)
74              continue
           wt_prob(k) = wt_prob(k)/wt_sum
73         continue

      end if

ccc   Normalize weighted probabilities:
ccc
      sum = 0.0
      do 75 k = 1,9
      sum = sum + wt_prob(k)
75    continue
      do 76 k = 1,9
      wt_prob(k) = wt_prob(k)/sum
76    continue

ccc   At this point the final weighted probabilities are in wt_prob(1-9)
ccc   Each detector probability is in prob(j=1-5, k=1-9) for each
ccc   contributing detector as indicated by nhypo_det(j=1-5) .gt. 0
ccc
ccc   Next, find the most probable pid for total and for each detector
ccc   and locate the corresponding column (1-9) in the local, ordered arrays.
ccc

      do 80 j = 1,6
      pmax = 0.0
      if(j .le. 5) then
           if(nhypo_det(j) .eq. 0) go to 80
           do 81 k = 1,9
           if(prob(j,k) .gt. pmax) then
                max_col(j) = k
                pmax = prob(j,k)
           end if
81         continue
      else if(j.eq.6) then
           do 82 k = 1,9
           if(wt_prob(k) .gt. pmax) then
                max_col(j) = k
                pmax = wt_prob(k)
           end if
82         continue
      end if
80    continue

ccc   Compute sharpness of final, weighted probability distributions.
ccc   Find the next-to-largest probability among wt_prob values.
ccc
      pnext = 0.0
      do 90 j = 1,9
      if(j .eq. max_col(6)) go to 90
      if(wt_prob(j) .gt. pnext) then
           pnext = wt_prob(j)
      end if
90    continue

      if(pnext .ne. 0.0) then
           sharp_tot = wt_prob(max_col(6))/pnext
      else
           sharp_tot = 100.0
      end if

ccc  Check for and resolve conflicts between total, weighted best
ccc  probability and the detector specific best probabilities.
ccc  For a conflict to exist the probability distribution of the disagreeing
ccc  detector must be sufficiently sharp and its relative weighting also
ccc  sufficient where these cuts are taken to be epipar(1).sharp_min and
ccc  epipar(1).weight_min , respectively.
ccc
      if(epipar(1).ctrl_conflict .eq. 0) go to 91
      if(n_active .eq. 1)             go to 91
      if(n_active .ge. 2) then
           call conf_check(nhypo_det,sharp,wt,wt_avg,epipar(1).sharp_min,
     1    epipar(1).weight_min,max_col,iconf)
           if(iconf .eq. 0) go to 91
           if(epipar(1).ctrl_conflict .eq. 1) then
                gflag = gflag + 10
                do 92 k = 1,5
                j = k
                if(k.eq.2) j=3
                if(k.eq.3) j=2
                if(nhypo_det(j) .eq. 0) go to 92
                if(sharp(j) .gt. epipar(1).sharp_min  .and.
     &             (wt(j)/wt_avg) .gt. epipar(1).weight_min) then
                     det_use = j
                     go to 93
                end if
92              continue
                go to 91
           end if
      end if

ccc  Fill global_pid for case where no PID information exists or where none
ccc  of the detector PID values pass filters.
ccc

94    continue
      global_pid(globpid_ktr).flag  =  gflag
      global_pid(globpid_ktr).most_prob_id  = 0
      if(gflag .eq. 0) then
           global_pid(globpid_ktr).nhypo = 0
      else
           global_pid(globpid_ktr).nhypo = nhypo_tot
      end if
      global_pid(globpid_ktr).quality  = 0.0
      go to 20

ccc   Fill global_pid for case where PID assignment is based on the total
ccc   weighted probabilities, but first check to see that final distri-
ccc   bution is sharp enough.
ccc

91    continue
      if(sharp_tot .gt. epipar(1).sharp_min) then
           global_pid(globpid_ktr).flag = gflag
           call gcode(max_col(6),globtrk(i).icharge,code)
           global_pid(globpid_ktr).most_prob_id = code
           global_pid(globpid_ktr).nhypo = nhypo_tot
           global_pid(globpid_ktr).quality = sharp_tot
      else
           n_sharp = 0
           do 95 j = 1,5
           if(sharp(j) .gt. epipar(1).sharp_min) then
                n_sharp = n_sharp + 1
                n_sharp_j = j
           end if
95         continue
           if(n_sharp .eq. 1) then
                call gcode(max_col(n_sharp_j),globtrk(i).icharge,code)
                global_pid(globpid_ktr).flag = 21
                global_pid(globpid_ktr).most_prob_id = code
                global_pid(globpid_ktr).nhypo = nhypo_tot
                global_pid(globpid_ktr).quality = sharp(n_sharp_j)
           else
                global_pid(globpid_ktr).flag = 20
                global_pid(globpid_ktr).most_prob_id = 0
                global_pid(globpid_ktr).nhypo = nhypo_tot
                global_pid(globpid_ktr).quality = sharp_tot
           end if
      end if

      go to 20

ccc   Fill global_pid for case where PID assignment is based on the
ccc   innermost applicable detector PID, as a result of the conflict
ccc   resolution algorithm.
ccc

93    continue
           global_pid(globpid_ktr).flag = gflag
           call gcode(max_col(det_use),globtrk(i).icharge,code)
           global_pid(globpid_ktr).most_prob_id = code
           global_pid(globpid_ktr).nhypo = nhypo_tot
           global_pid(globpid_ktr).quality = -sharp(det_use)

20    continue

      epi_am = STAFCV_OK

      global_pid_h.nok = globpid_ktr

      return
      end
