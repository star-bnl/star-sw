/*:>--------------------------------------------------------------------
**: FILE:       ctc.cc         
**: HISTORY:
**:             04feb93-v000a-ppy- Created by Pablo Yepes
**:  
**:<------------------------------------------------------------------*/
#include "ctc.h"
//
//  Function definitions
//
extern "C" void    MessageOut( const char *msg );
//
extern "C" long type_of_call ctc_(
  TABLE_HEAD_ST   *ctrl_h,       CTC_CTRL_ST   *ctrl  ,
  TABLE_HEAD_ST   *extra_h,      CTE_EXTRA_ST  *extra ,
  TABLE_HEAD_ST   *raw_h,        CTU_RAW_ST    *raw ,
  TABLE_HEAD_ST   *histos_h,     CTC_HISTOS_ST *histos  ,
  TABLE_HEAD_ST   *slat_h,       CTG_SLAT_ST   *slat )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    ctc_
**: DESCRIPTION: Uses tracks extrapolated to CTB for calibration
**:  
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu       
**: ARGUMENTS:
**:       IN:
**:               ctrl    - Controls module
**:             ctrl_h    - header structure for ctrl   
**:              extra    - Tracks extraplated to CTB 
**:            extra_h    - header Structure for extra
**:                raw    - CTB raw information
**:              raw_h    - header Structure for raw
**:    INOUT:
**:      OUT:
**:             histos    - Stores calibration histos
**:           histos_h    - header structure for histos
**:               slat    - CTB slat table (includes calibration)
**:             slat_h    - header Structure for slat
**: 
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
  char  OutMessage[50] ;
//
//    Make sure ctrl table has something
//
  if ( ctrl_h->nok != 1 ) {
     sprintf ( OutMessage, " Ctrl maxlen = %d is not valid ", ctrl_h->nok ) ;
     MessageOut ( OutMessage ) ;
     return STAFCV_BAD ;
  }
//
//    Make sure ctrl table has something
//
  if ( ctrl_h->nok != 1 ) {
     sprintf ( OutMessage, " Ctrl maxlen = %d is not valid ", ctrl_h->nok ) ;
     MessageOut ( OutMessage ) ;
     return STAFCV_BAD ;
  }


   

   return STAFCV_OK;
}
