      Subroutine  G2R_GET_SYS (Cset,Cdet,iprin,idigi)
      IMPLICIT    NONE
      character*4     Cset, Cdet, Cseta
      Integer         ISLFLAG,AgHITSET,Iprin,Idigi,Nhits
*     - - - - - - - - - - - - - - -    

      Cseta=Cset(1:3)//'H'
      Iprin = ISLFLAG (Cset,'PRIN')
      Idigi = ISLFLAG (Cset,'DIGI')
      Nhits = AgHITSET(Cseta,Cdet)
      If (Nhits .le. 0) Iprin = -1

      END
*
* --------------------------------------------------------------------
*
      Function   G2R_GET_HIT (Csys)
      IMPLICIT   NONE
#include "geant321/gcnum.inc"
#include "geant321/gcflag.inc"
#include "genhit.h"
#include "atlsim/gentit.inc"
      Integer         G2R_GET_HIT,AgHITGET,G2T_VOLUME_ID,i
      real            XYZTr(nmh)
      character*3     Csys
*
         i = AgHITGET (0,XYZTr)
         if (abs(trac).gt.NTRACK) then
           i       = -1
           IEOTRI  =  1
           if (Idebug.ne.0) then
              print *,' G2T_GET_HIT error in ',Csys,', event will be skipped'
           endif     
         endif     
         G2R_GET_HIT = i
         if (i.eq.0) volume = g2t_volume_id (Csys,NUMBV)
      end




