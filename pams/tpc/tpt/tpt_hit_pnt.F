* $Id: tpt_hit_pnt.F,v 1.2 1998/01/27 00:35:48 fisyak Exp $
* $Log: tpt_hit_pnt.F,v $
* Revision 1.2  1998/01/27 00:35:48  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_HIT_PNT(hit_h,hit,list_hits)
C>---------------------------------------------------------------------
C
C TPT_HIT_PNT - addressing hits with the hit.id
C
C DESCRIPTION:
C Sets up a table that allows to find a hit in the hit table if the 
C hit.id is known.
C
C INPUT ARGUMENTS:
C  hit_h     - header for the tphit (tpc hits) table
C  hit      - rows of the tphit table
C
C OUTPUT ARGUMENTS:
C  list_hits - array that lists row numbers for hits list_hit(hit.id)
C              =row number
C<---------------------------------------------------------------------

      IMPLICIT NONE

#include "PAM.inc"

#include "tcl_tphit.inc"

C_____________________________________________________________________
      RECORD /table_head_st/    hit_h
      RECORD /tcl_tphit_st/     hit(*)
C_____________________________________________________________________
C
      INTEGER list_hits(*)
      INTEGER i, ient
C_____________________________________________________________________
C
      tpt_hit_pnt=STAFCV_OK
C
C     reset to 0 list_hits
      do i=1,hit_h.maxlen
         list_hits(i)=0
      end do
C
      i=0
      do while (i.lt.hit_h.nok.and.tpt_hit_pnt.eq.STAFCV_OK)
         i = i+1
         ient = hit(i).id
         if(ient.ge.1.and.ient.le.2*hit_h.maxlen) then
                 list_hits(ient) = i
         else
                 write(6,*) ient,hit_h.maxlen,hit_h.nok,
     >           'failer to establish reference array for tphit'
                 tpt_hit_pnt=STAFCV_BAD
         endif
      end do
      end
