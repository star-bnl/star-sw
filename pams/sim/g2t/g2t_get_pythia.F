* $Id: g2t_get_pythia.F,v 1.1 2004/08/05 16:38:50 potekhin Exp $
*
* $Log: g2t_get_pythia.F,v $
* Revision 1.1  2004/08/05 16:38:50  potekhin
* Propagating Pythia event characterization data
*
*
**:>-----------------------------------------------------------------------
*  module    G2T_get pythia fills Pythia event header struct
*  author    Maxim Potekhin
*  created   30 July 04
**:<-----------------------------------------------------------------------

      integer Function G2T_GET_PYTHIA(pythia_h, pythia)
      implicit   none
#include "g2t_get_pythia.inc"

      common/PYTNFO/ PYSTAT,PYT_ID,MANDS,MANDT,MANDU,HARDP,COSTH,BJOR1,BJOR2
      integer        PYSTAT,PYT_ID
      real           MANDS,MANDT,MANDU,HARDP,COSTH,BJOR1,BJOR2


      G2T_GET_PYTHIA   = 1
      pythia_h.nok = 1

      if(pyStat.ge.0) then
         pythia(1).subprocess_id = PYT_ID
         pythia(1).mand_s        = MANDS
         pythia(1).mand_t        = MANDT
         pythia(1).mand_u        = MANDU
         pythia(1).hard_p        = HARDP
         pythia(1).cos_th        = COSTH
         pythia(1).bjor_1        = BJOR1
         pythia(1).bjor_2        = BJOR2
      endif

      END

