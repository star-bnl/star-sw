      INTEGER*4 FUNCTION STK_AM_INIT(
     1            stkpar_h,            stkpar ,
     2            groups_h,            groups ,
     3             track_h,             track ,
     4              kine_h,              kine ,
     5          mcgroups_h,          mcgroups ,
     6           mctrack_h,           mctrack ,
     7            mckine_h,            mckine ) 
      IMPLICIT NONE

C     SYNOPSIS: 
C     STK initialization
C     
C     DESCRIPTION:
C     Clears tables for next event
C
C     AUTHOR:
C     W. Ken Wilson,  Wayne State University
C     
C     HISTORY:
C     22-jun-1994  wkw written
C     Aug 20 1997  Removed ctrack table
c
C     ARGUMENTS:
#include "stk_am_init.inc"

C     Local Declarations: 
c     -------------------

C     EXECUTABLE CODE:
C     ================

      WRITE(*,*) 'STK_AM_INIT: Starting ###############################'

      mctrack_h.nok=0
      track_h.nok=0
      kine_h.nok=0
      mckine_h.nok=0
      mcgroups_h.nok = 0
      groups_h.nok = 0
      stkpar(1).ngood=0

      WRITE(*,*) 'STK_AM_INIT: Stopping ###############################'
      STK_AM_INIT=STAFCV_OK
      RETURN
      END
