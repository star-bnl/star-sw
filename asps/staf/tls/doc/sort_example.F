<PRE>
C===============================================================================
      INTEGER FUNCTION example_module( hh, Hits, th, Tracks )
C
C  This example code is intended to show a typical use of the
C  tls_sort_routines package in finding the row given a key.
C
      IMPLICIT NONE
C
#include "tas_structures.inc"
#include "tas_user_codes.inc"
#include "scs_svt_spt_pars.inc"
#include "scs_svt_spt_st.inc"
#include "stk_svt_track_pars.inc"
#include "stk_svt_track_st.inc"
C
      RECORD/ table_head_st / hh
      RECORD/ svt_spt_row_st / Hits(*)
      RECORD/ table_head_st / th
      RECORD/ svt_track_row_st / Tracks(*)
C
      INTEGER tls_Index_Sort_i, tls_Search_Index_i
      LOGICAL SCV_STATE
C
      INTEGER MAXHITS, MAXTRACKS, MAXFIND
      PARAMETER (MAXHITS=30000, MAXTRACKS=10000, MAXFIND=10)
C
      INTEGER hit_index(MAXHITS), track_index(MAXTRACKS)
      INTEGER i_rows(MAXFIND), j_off(MAXFIND)
      INTEGER Ok, n_found
C--------------------------------------------------------------------------
C
C  Sort hits according to track they have been assigned to.
C  Notice we sort once (per key) in this routine, before entering
C  any loops.
      Ok = tls_Index_Sort_i( hh.nok, Hits(1).id_track, Hits(2).id_track,
     +                       hit_index, MAXHITS )
      IF (.NOT.SCV_STATE( Ok, 'I')) THEN
          CALL SCV_MESSAGES( Ok, mess )
C         Some sort of problem, may need to take corrective action here.
          END IF
C
C  Sort tracks according to their ID.
      Ok = tls_Index_Sort_i( hh.nok, Tracks(1).ID, Tracks(2).ID,
     +                       track_index, MAXTRACKS )
      IF (.NOT.SCV_STATE( Ok, 'I')) THEN
          CALL SCV_MESSAGES( Ok, mess )
C         Some sort of problem, may need to take corrective action here.
          END IF
C
      DO i = 1,hh.nok
C
C  Find track(s) that this hit belongs to.
C  Search will typically be done within a loop.
         n_found = MAXFIND    ! Cannot pass MAXFIND into Search_Index
                              ! because argument gets modified.
         Ok = tls_Search_Index_i( th.nok, Tracks(1).ID, Tracks(2).ID,
     +                            Hits(i).id_track,
     +                            i_rows, n_found, track_index, j_off  )
         IF (.NOT.SCV_STATE( Ok, 'I')) THEN
             CALL SCV_MESSAGES( Ok, mess )
C            Some sort of problem, perhaps hit was not assigned to any track?
C            In this example we could get as many as 10 tracks returned.
             END IF
C        Track parameters are in Tracks(i_rows(1:n_found))
C
C  Now find all other hits that belong to this same track.
         n_found = MAXFIND
         Ok = tls_Search_Index_i( hh.nok, Hits(1).id_track, Hits(2).id_track,
     +                            Hits(i).id_track,
     +                            i_rows, n_found, hit_index, j_off  )
         IF (.NOT.SCV_STATE( Ok, 'I')) THEN
             CALL SCV_MESSAGES( Ok, mess )
C            Some sort of problem, perhaps Hits(i) was not assigned to any track?
C            In this example we could get as many as 10 hits returned.
             END IF
C        Hits for this track are in Hits(i_rows(1:n_found))
C
         END DO
      example_module = tucod_ok
      RETURN
      END
</PRE>

         
