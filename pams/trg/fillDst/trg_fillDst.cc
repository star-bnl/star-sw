/*:>-------------------------------------------------------------------
**: FILE:       trg_fillDst.cc
**: HISTORY:
**:   3/18/99 ppy Bug zeroing CTB info fixed, found by Torre Weneaus 
**:  
**:<------------------------------------------------------------------*/
#include "trg_fillDst.h"
#include <assert.h>


extern "C" long type_of_call trg_fillDst_(
  TABLE_HEAD_ST     *ctu_cor_h,    CTU_COR_ST              *ctu_cor,   
  TABLE_HEAD_ST     *mwc_raw_h,    MWC_RAW_ST              *mwc_raw,   
  TABLE_HEAD_ST     *dst_h,        DST_TRGDET_ST           *dst )  
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    trg_fillDst_
**: DESCRIPTION: Prepares data and calls fast tracking   
**:
**: 
**: AUTHOR:     ppy - Pablo P. Yepes, yepes@rice.edu  
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
//
//    Fill DST CTB info  
//
   int i, index ;
   for ( i = 0 ; i < 240 ; i++ ) dst->nCtb[i] = 0 ; 
   if ( ctu_cor_h->nok > 0 ) {
      for ( i = 0 ; i < ctu_cor_h->nok ; i++ ) {
         index = 4 * ( ctu_cor[i].i_phi-1 ) + ctu_cor[i].i_eta- 1 ;
         assert(index>=  0); assert(index< 240);
         dst->nCtb[index]    = ctu_cor[i].n ;
         dst->timeCtb[index] = ctu_cor[i].time ;
      }
   }
//
//    Fill DST MWC info
//
   for ( i = 0 ; i < 96 ; i++ ) dst->nMwc[i] = 0 ;
   if ( mwc_raw_h->nok > 0 ) {
      for ( i = 0 ; i < mwc_raw_h->nok ; i++ ) {
         index = mwc_raw[i].sector - 1  ;
         dst->nMwc[index]    = mwc_raw[i].count ;
      }
   }
//
    dst_h->nok = 1 ;
//
   return STAFCV_OK ;
   
}
